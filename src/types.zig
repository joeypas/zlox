const std = @import("std");
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const Object = @import("object.zig");

pub const OpCode = enum(u8) {
    constant,
    nil,
    true_,
    false_,
    pop,
    get_global,
    define_global,
    set_global,
    equal,
    greater,
    less,
    add,
    subtract,
    multiply,
    divide,
    not,
    negate,
    print,
    return_,
};

pub const Instruction = struct {
    byte: u8,
    line: usize,
};

pub const Chunk = struct {
    code: ArrayList(Instruction),
    constants: ArrayList(Value),

    pub fn init(allocator: Allocator) Chunk {
        return .{
            .code = ArrayList(Instruction).init(allocator),
            .constants = ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *Chunk) void {
        self.code.deinit();
        self.constants.deinit();
    }

    pub fn writeChunk(self: *Chunk, byte: u8, line: usize) !void {
        if (self.code.capacity < self.code.capacity + 1) {
            if (self.code.capacity != 0) {
                try self.code.ensureTotalCapacity(self.code.capacity * 2);
            } else {
                try self.code.ensureTotalCapacity(self.code.capacity + 2);
            }
        }

        self.code.appendAssumeCapacity(.{ .byte = byte, .line = line });
    }

    fn writeValue(self: *Chunk, value: Value) !void {
        if (self.constants.capacity < self.constants.capacity + 1) {
            if (self.constants.capacity != 0) try self.constants.ensureTotalCapacity(self.constants.capacity * 2) else try self.constants.ensureTotalCapacity(self.constants.capacity + 2);
        }
        self.constants.appendAssumeCapacity(value);
    }

    pub fn addConstant(self: *Chunk, value: Value) !usize {
        try self.writeValue(value);
        return self.constants.items.len - 1;
    }
};

pub const ValueType = enum {
    boolean,
    number,
    obj,
    nil,
};

pub const Value = union(ValueType) {
    boolean: bool,
    number: f32,
    obj: *Object.Obj,
    nil: u1,

    pub fn isNumber(value: Value) bool {
        return switch (value) {
            .number => true,
            else => false,
        };
    }

    pub fn isBool(value: Value) bool {
        return switch (value) {
            .boolean => true,
            else => false,
        };
    }

    pub fn isObj(value: Value) bool {
        return switch (value) {
            .obj => true,
            else => false,
        };
    }

    pub fn isNil(value: Value) bool {
        return switch (value) {
            .nil => true,
            else => false,
        };
    }

    pub fn valuesEqual(a: Value, b: Value) bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) return false;

        return switch (a) {
            .boolean => a.boolean == b.boolean,
            .nil => false,
            .number => a.number == b.number,
            .obj => @intFromPtr(a.obj) == @intFromPtr(b.obj),
        };
    }
};

pub fn printValue(value: Value, out: std.io.AnyWriter) !void {
    switch (value) {
        .number => |val| try out.print("{d}", .{val}),
        .obj => |val| try val.printObject(out),
        .boolean => |val| try out.print("{any}", .{val}),
        .nil => try out.print("nil", .{}),
    }
}
