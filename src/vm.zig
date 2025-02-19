const std = @import("std");
const types = @import("types.zig");
const compiler = @import("compiler.zig");
const Chunk = types.Chunk;
const OpCode = types.OpCode;
const Value = types.Value;
const ValueType = types.ValueType;
const Instruction = types.Instruction;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;
const Table = @import("table.zig").Table;
const object = @import("object.zig");

const VM = @This();
const STACK_MAX = 256;

pub const InterpretError = error{
    CompileError,
    RuntimeError,
    InternalError,
};

out: std.io.AnyWriter,
err: std.io.AnyWriter,
allocator: Allocator,
chunk: *Chunk = undefined,
ip: [*]Instruction = undefined,
stack: [STACK_MAX]Value = undefined,
stackTop: [*]Value = undefined,
globals: Table,
strings: Table,
objects: ArrayList(*object.Obj),

pub fn init(allocator: Allocator, stdout: std.fs.File, stderr: std.fs.File) !VM {
    var vm = VM{
        .out = stdout.writer().any(),
        .err = stderr.writer().any(),
        .objects = ArrayList(*object.Obj).init(allocator),
        .globals = try Table.init(allocator),
        .strings = try Table.init(allocator),
        .allocator = allocator,
    };
    vm.resetStack();
    return vm;
}

pub fn deinit(self: *VM) void {
    self.strings.deinit();
    self.globals.deinit();
    for (self.objects.items) |obj| {
        obj.destroy();
    }
    self.objects.deinit();
}

pub fn interpret(self: *VM, source: []const u8) InterpretError!void {
    var chunk = Chunk.init(self.allocator);
    defer chunk.deinit();

    compiler.compile(self.allocator, self, source, &chunk, self.err) catch |err| {
        return switch (err) {
            InterpretError.InternalError => InterpretError.InternalError,
            else => InterpretError.CompileError,
        };
    };

    self.chunk = &chunk;
    self.ip = self.chunk.code.items.ptr;

    try self.run();
}

fn run(self: *VM) InterpretError!void {
    while (true) {
        const instruction = self.readByte();
        switch (instruction.byte) {
            @intFromEnum(OpCode.constant) => {
                const constant = self.readConstant();
                self.push(constant);
            },
            @intFromEnum(OpCode.nil) => self.push(.{ .nil = 0 }),
            @intFromEnum(OpCode.true_) => self.push(.{ .boolean = true }),
            @intFromEnum(OpCode.false_) => self.push(.{ .boolean = false }),
            @intFromEnum(OpCode.pop) => _ = self.pop(),
            @intFromEnum(OpCode.get_global) => {
                const name = self.readConstant().obj;
                if (self.globals.get(name)) |value| {
                    self.push(value.value);
                } else {
                    try self.runtimeError("Undefined variable '{s}'", .{name.string.chars});
                }
            },
            @intFromEnum(OpCode.define_global) => {
                //std.debug.print("In define global\n", .{});
                const name = self.readConstant().obj;
                _ = self.globals.set(name, self.peek(0)) catch return InterpretError.InternalError;
                _ = self.pop();
            },
            @intFromEnum(OpCode.set_global) => {
                //std.debug.print("In set global\n", .{});
                const name = self.readConstant().obj;
                if (self.globals.set(name, self.peek(0)) catch return InterpretError.InternalError) {
                    _ = self.globals.delete(name);
                    try self.runtimeError("Undefined variable '{s}'.", .{name.string.chars});
                }
            },
            @intFromEnum(OpCode.equal) => {
                const a = self.pop();
                const b = self.pop();
                self.push(.{ .boolean = Value.valuesEqual(a, b) });
            },
            @intFromEnum(OpCode.greater) => try self.binaryOp(.greater),
            @intFromEnum(OpCode.less) => try self.binaryOp(.less),
            @intFromEnum(OpCode.add) => {
                if (object.isObjType(self.peek(0), .string) and object.isObjType(self.peek(1), .string)) {
                    try self.concatenate();
                } else {
                    try self.binaryOp(.add);
                }
            },
            @intFromEnum(OpCode.subtract) => try self.binaryOp(.subtract),
            @intFromEnum(OpCode.multiply) => try self.binaryOp(.multiply),
            @intFromEnum(OpCode.divide) => try self.binaryOp(.divide),
            @intFromEnum(OpCode.not) => self.push(.{ .boolean = isFalsey(self.pop()) }),
            @intFromEnum(OpCode.negate) => {
                switch (self.peek(0)) {
                    .number => self.push(.{ .number = -1 * self.pop().number }),
                    else => {
                        try self.runtimeError("Operand must be a number.", .{});
                    },
                }
            },
            @intFromEnum(OpCode.print) => {
                types.printValue(self.pop(), self.out) catch return InterpretError.InternalError;
                self.out.print("\n", .{}) catch return InterpretError.InternalError;
            },
            @intFromEnum(OpCode.return_) => {
                return;
            },
            else => return InterpretError.RuntimeError,
        }
    }
}

fn readByte(self: *VM) Instruction {
    defer self.ip += 1;
    return self.ip[0];
}

fn readConstant(self: *VM) types.Value {
    return self.chunk.constants.items[self.readByte().byte];
}

fn resetStack(self: *VM) void {
    self.stackTop = &self.stack;
}

fn push(self: *VM, value: Value) void {
    self.stackTop[0] = value;
    self.stackTop += 1;
}

fn pop(self: *VM) Value {
    self.stackTop -= 1;
    return self.stackTop[0];
}

fn peek(self: *VM, distance: usize) Value {
    return (self.stackTop - 1 - distance)[0];
}

fn isFalsey(value: Value) bool {
    return Value.isNil(value) or (Value.isBool(value) and !value.boolean);
}

fn concatenate(self: *VM) !void {
    const a = self.pop().obj.string;
    const b = self.pop().obj.string;

    const len = a.length + b.length;
    var chars = self.allocator.alloc(u8, len) catch return InterpretError.InternalError;
    @memcpy(chars[0..b.length], b.chars);
    @memcpy(chars[b.length..], a.chars);
    self.push(.{ .obj = object.ObjString.takeString(self.allocator, chars, self) catch return InterpretError.InternalError });
}

fn binaryOp(self: *VM, comptime code: OpCode) !void {
    if (!Value.isNumber(self.peek(0)) or !Value.isNumber(self.peek(1))) {
        try self.runtimeError("Operands must be numbers.", .{});
        return InterpretError.RuntimeError;
    }
    const b = self.pop().number;
    const a = self.pop().number;
    switch (code) {
        .add => self.push(.{ .number = a + b }),
        .subtract => self.push(.{ .number = a - b }),
        .multiply => self.push(.{ .number = a * b }),
        .divide => self.push(.{ .number = a / b }),
        .greater => self.push(.{ .boolean = a > b }),
        .less => self.push(.{ .boolean = a < b }),
        else => unreachable,
    }
}

fn runtimeError(self: *VM, comptime format: []const u8, args: anytype) !void {
    self.err.print(format, args) catch return InterpretError.InternalError;
    self.err.print("\n", .{}) catch return InterpretError.InternalError;

    const instruction = if (self.ip[0].line < self.chunk.code.items.len - 1) self.ip[0] else (self.ip - @intFromPtr(self.chunk.code.items.ptr) - 1)[0];
    const line = instruction.line;
    self.err.print("[line {d}] in script\n", .{line}) catch return InterpretError.InternalError;
    self.resetStack();
    return InterpretError.RuntimeError;
}
