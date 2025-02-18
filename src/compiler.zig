const std = @import("std");
const types = @import("types.zig");
const Allocator = std.mem.Allocator;
const InterpretError = @import("vm.zig").InterpretError;
const Chunk = types.Chunk;
const OpCode = types.OpCode;
const Value = types.Value;
const Scanner = @import("scanner.zig");
const Token = Scanner.Token;
const TokenType = Scanner.TokenType;
const object = @import("object.zig");
const VM = @import("vm.zig");

const Compiler = @This();

const Precedence = enum {
    none,
    assignment,
    or_,
    and_,
    equallity,
    comparison,
    term,
    factor,
    unary,
    call,
    primary,
};

const ParseFn = ?*const fn (*Compiler) InterpretError!void;

const ParseRule = struct {
    prefix: ParseFn,
    infix: ParseFn,
    precedence: Precedence,
};

const Parser = struct {
    current: Token = undefined,
    previous: Token = undefined,
    hadError: bool = false,
    panicMode: bool = false,
};

const rules = [_]ParseRule{
    // Assigning function pointers and precedence
    .{ .prefix = Compiler.grouping, .infix = null, .precedence = .none }, // left_paren
    .{ .prefix = null, .infix = null, .precedence = .none }, // right_paren
    .{ .prefix = null, .infix = null, .precedence = .none }, // left_brace
    .{ .prefix = null, .infix = null, .precedence = .none }, // right_brace
    .{ .prefix = null, .infix = null, .precedence = .none }, // comma
    .{ .prefix = null, .infix = null, .precedence = .none }, // dot
    .{ .prefix = Compiler.unary, .infix = Compiler.binary, .precedence = .term }, // minus
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .term }, // plus
    .{ .prefix = null, .infix = null, .precedence = .none }, // semicolon
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .factor }, // slash
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .factor }, // star
    .{ .prefix = Compiler.unary, .infix = null, .precedence = .none }, // TOKEN_BANG
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .equallity }, // TOKEN_BANG_EQUAL
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_EQUAL
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .equallity }, // TOKEN_EQUAL_EQUAL
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .comparison }, // TOKEN_GREATER
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .comparison }, // TOKEN_GREATER_EQUAL
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .comparison }, // TOKEN_LESS
    .{ .prefix = null, .infix = Compiler.binary, .precedence = .comparison }, // TOKEN_LESS_EQUAL
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_IDENTIFIER
    .{ .prefix = Compiler.string, .infix = null, .precedence = .none }, // TOKEN_STRING
    .{ .prefix = Compiler.number, .infix = null, .precedence = .none }, // TOKEN_NUMBER
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_AND
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_CLASS
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_ELSE
    .{ .prefix = Compiler.literal, .infix = null, .precedence = .none }, // TOKEN_FALSE
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_FOR
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_FUN
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_IF
    .{ .prefix = Compiler.literal, .infix = null, .precedence = .none }, // TOKEN_NIL
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_OR
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_PRINT
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_RETURN
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_SUPER
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_THIS
    .{ .prefix = Compiler.literal, .infix = null, .precedence = .none }, // TOKEN_TRUE
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_VAR
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_WHILE
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_error_
    .{ .prefix = null, .infix = null, .precedence = .none },
};

fn getRule(ttype: TokenType) *const ParseRule {
    return &rules[@intFromEnum(ttype)];
}

allocator: Allocator,
parser: Parser,
scanner: Scanner,
stderr: std.io.AnyWriter,
currentChunk: *Chunk,
vm: *VM,

pub fn compile(allocator: Allocator, vm: *VM, source: []const u8, chunk: *Chunk, stderr: std.io.AnyWriter) InterpretError!void {
    var compiler = Compiler{
        .vm = vm,
        .allocator = allocator,
        .scanner = Scanner.init(source),
        .stderr = stderr,
        .parser = .{},
        .currentChunk = chunk,
    };

    try compiler.advance();
    try compiler.expression();
    try compiler.consume(.eof, "Expect end of expression.");
    try compiler.end();
    if (compiler.parser.hadError) return InterpretError.CompileError;
}

fn end(self: *Compiler) !void {
    try self.emitByte(@intFromEnum(OpCode.return_));
}

fn emitByte(self: *Compiler, byte: u8) !void {
    self.currentChunk.writeChunk(byte, self.parser.previous.line) catch return InterpretError.InternalError;
}

fn emitBytes(self: *Compiler, byte1: u8, byte2: u8) !void {
    try self.emitByte(byte1);
    try self.emitByte(byte2);
}

fn emitConstant(self: *Compiler, value: Value) !void {
    try self.emitBytes(@intFromEnum(OpCode.constant), try self.makeConstant(value));
}

fn makeConstant(self: *Compiler, value: Value) !u8 {
    const constant = self.currentChunk.addConstant(value) catch return InterpretError.InternalError;
    if (constant > 255) {
        try self.err("Too many constants in one chunk.");
    }

    return @intCast(constant);
}

fn advance(self: *Compiler) InterpretError!void {
    self.parser.previous = self.parser.current;

    while (true) {
        self.parser.current = self.scanner.scanToken();

        if (self.parser.current.type != .error_) break;

        try self.errorAtCurrent(self.parser.current.start[0..self.parser.current.length]);
    }
}

fn expression(self: *Compiler) !void {
    try self.parsePrecedence(.assignment);
}

fn parsePrecedence(self: *Compiler, prec: Precedence) !void {
    try self.advance();
    const prefix_rule = getRule(self.parser.previous.type).prefix;

    if (prefix_rule) |f| {
        try f(self);
    } else {
        try self.err("Expect expression.");
        return;
    }

    while (@intFromEnum(prec) <= @intFromEnum(getRule(self.parser.current.type).precedence)) {
        try self.advance();
        const infix = getRule(self.parser.previous.type).infix;
        if (infix) |f| {
            try f(self);
        }
    }
}

fn number(self: *Compiler) !void {
    const value = std.fmt.parseFloat(f32, self.parser.previous.start[0..self.parser.previous.length]) catch return InterpretError.InternalError;

    try self.emitConstant(Value{ .number = value });
}

fn string(self: *Compiler) !void {
    try self.emitConstant(
        .{
            .obj = object.ObjString.copyString(
                self.allocator,
                self.parser.previous.start[1 .. self.parser.previous.length - 1],
                self.vm,
            ) catch |erro| {
                std.debug.print("{any}", .{erro});
                return InterpretError.InternalError;
            },
        },
    );
}

fn unary(self: *Compiler) !void {
    const operator_type = self.parser.previous.type;

    try self.parsePrecedence(.unary);

    switch (operator_type) {
        .bang => try self.emitByte(@intFromEnum(OpCode.not)),
        .minus => try self.emitByte(@intFromEnum(OpCode.negate)),
        else => unreachable,
    }
}

fn grouping(self: *Compiler) !void {
    try self.expression();
    try self.consume(.right_paren, "Expect ')' after expression.");
}

fn binary(self: *Compiler) !void {
    const operator_type = self.parser.previous.type;
    const rule = getRule(operator_type);
    try self.parsePrecedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

    switch (operator_type) {
        .bang_equal => try self.emitBytes(@intFromEnum(OpCode.equal), @intFromEnum(OpCode.not)),
        .equal_equal => try self.emitByte(@intFromEnum(OpCode.equal)),
        .greater => try self.emitByte(@intFromEnum(OpCode.greater)),
        .greater_equal => try self.emitBytes(@intFromEnum(OpCode.less), @intFromEnum(OpCode.not)),
        .less => try self.emitByte(@intFromEnum(OpCode.less)),
        .less_equal => try self.emitBytes(@intFromEnum(OpCode.greater), @intFromEnum(OpCode.not)),
        .plus => try self.emitByte(@intFromEnum(OpCode.add)),
        .minus => try self.emitByte(@intFromEnum(OpCode.subtract)),
        .star => try self.emitByte(@intFromEnum(OpCode.multiply)),
        .slash => try self.emitByte(@intFromEnum(OpCode.divide)),
        else => unreachable,
    }
}

fn literal(self: *Compiler) !void {
    switch (self.parser.previous.type) {
        .false_ => try self.emitByte(@intFromEnum(OpCode.false_)),
        .nil => try self.emitByte(@intFromEnum(OpCode.nil)),
        .true_ => try self.emitByte(@intFromEnum(OpCode.true_)),
        else => unreachable,
    }
}

fn consume(self: *Compiler, ttype: TokenType, message: []const u8) !void {
    if (self.parser.current.type == ttype) {
        try self.advance();
        return;
    }

    try self.errorAtCurrent(message);
}

fn errorAtCurrent(self: *Compiler, message: []const u8) !void {
    self.errorAt(&self.parser.current, message) catch return InterpretError.InternalError;
}

fn err(self: *Compiler, message: []const u8) !void {
    self.errorAt(&self.parser.previous, message) catch return InterpretError.InternalError;
}

fn errorAt(self: *Compiler, token: *Token, message: []const u8) !void {
    if (self.parser.panicMode) return;
    try self.stderr.print("[line {d}] Error", .{token.line});

    self.parser.panicMode = true;

    if (token.type == .eof) {
        try self.stderr.print(" at end", .{});
    } else if (token.type == .error_) {} else {
        try self.stderr.print(" at '{s}'", .{token.start[0..token.length]});
    }

    try self.stderr.print(": {s}\n", .{message});
    self.parser.hadError = true;
}

fn compileTest(source: []const u8, out: std.io.AnyWriter) InterpretError!void {
    var scanner = Scanner.init(source);
    defer scanner.deinit();
    var line: isize = -1;

    while (true) {
        const token = scanner.scanToken();
        if (token.line != line) {
            out.print("{d:0>4} ", .{token.line}) catch return InterpretError.InternalError;
            line = @intCast(token.line);
        } else {
            out.print("   | ", .{}) catch return InterpretError.InternalError;
        }
        out.print("{} '{s}'\n", .{ token.type, token.start[0..token.length] }) catch return InterpretError.InternalError;

        if (token.type == .eof) break;
    }
}

test "compile" {
    var stdout = std.io.getStdOut();
    defer stdout.close();
    try compileTest(" \"st\" + \"ri\" ", stdout.writer().any());
}
