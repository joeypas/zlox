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
const build_options = @import("build_options");
const debug = @import("debug.zig");
const errlog = std.log.scoped(.compiler).err;

const Compiler = @This();

const UINT8_MAX = std.math.maxInt(u8);
const UINT16_MAX = std.math.maxInt(u16);

const Precedence = enum(u8) {
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

const Local = struct {
    name: Token,
    depth: isize,
};

const ParseFn = ?*const fn (*Compiler, bool) InterpretError!void;

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
    .{ .prefix = null, .infix = null, .precedence = .none }, // colon
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
    .{ .prefix = Compiler.variable, .infix = null, .precedence = .none }, // TOKEN_IDENTIFIER
    .{ .prefix = Compiler.string, .infix = null, .precedence = .none }, // TOKEN_STRING
    .{ .prefix = Compiler.number, .infix = null, .precedence = .none }, // TOKEN_NUMBER
    .{ .prefix = null, .infix = Compiler.and_, .precedence = .and_ }, // TOKEN_AND
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_CASE
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_CLASS
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_DEFAULT
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_ELSE
    .{ .prefix = Compiler.literal, .infix = null, .precedence = .none }, // TOKEN_FALSE
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_FOR
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_FUN
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_IF
    .{ .prefix = Compiler.literal, .infix = null, .precedence = .none }, // TOKEN_NIL
    .{ .prefix = null, .infix = Compiler.or_, .precedence = .or_ }, // TOKEN_OR
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_PRINT
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_RETURN
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_SUPER
    .{ .prefix = null, .infix = null, .precedence = .none }, // TOKEN_SWITCH
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
stdout: std.io.AnyWriter,
currentChunk: *Chunk,
vm: *VM,
locals: [UINT8_MAX]Local = undefined,
localCount: isize = 0,
scopeDepth: isize = 0,

pub fn compile(
    allocator: Allocator,
    vm: *VM,
    source: []const u8,
    chunk: *Chunk,
    stdout: std.io.AnyWriter,
    stderr: std.io.AnyWriter,
) InterpretError!void {
    var compiler = Compiler{
        .vm = vm,
        .allocator = allocator,
        .scanner = Scanner.init(source),
        .stderr = stderr,
        .stdout = stdout,
        .parser = .{},
        .currentChunk = chunk,
    };

    compiler.advance();
    //try compiler.expression();
    //try compiler.consume(.eof, "Expect end of expression.");

    while (!compiler.match(.eof)) {
        try compiler.declaration();
    }
    try compiler.end();
    if (compiler.parser.hadError) return InterpretError.CompileError;
}

fn end(self: *Compiler) !void {
    try self.emitByte(@intFromEnum(OpCode.return_));
    if (build_options.dev) {
        if (!self.parser.hadError)
            debug.dissasembleChunk(self.currentChunk, "code", self.stdout) catch |erro| {
                errlog("Error: {any} in emitByte.\n", .{erro});
                return InterpretError.InternalError;
            };
    }
}

fn beginScope(self: *Compiler) void {
    self.scopeDepth += 1;
}

fn endScope(self: *Compiler) !void {
    self.scopeDepth -= 1;

    while (self.localCount > 0 and self.locals[@intCast(self.localCount - 1)].depth > self.scopeDepth) {
        try self.emitByte(@intFromEnum(OpCode.pop));
        self.localCount -= 1;
    }
}

fn emitByte(self: *Compiler, byte: u8) !void {
    self.currentChunk.writeChunk(byte, self.parser.previous.line) catch |erro| {
        errlog("Error: {any} in emitByte.\nCurrent byte: {c}\n", .{ erro, byte });
        return InterpretError.InternalError;
    };
}

fn emitBytes(self: *Compiler, byte1: u8, byte2: u8) !void {
    try self.emitByte(byte1);
    try self.emitByte(byte2);
}

fn emitLoop(self: *Compiler, loop_start: usize) !void {
    try self.emitByte(@intFromEnum(OpCode.loop));

    const offset = self.currentChunk.code.items.len - loop_start + 2;
    if (offset > UINT16_MAX) try self.err("Loop body too large.");

    try self.emitByte(@intCast(offset >> 8 & 0xff));
    try self.emitByte(@intCast(offset & 0xff));
}

fn emitJump(self: *Compiler, instruction: u8) !usize {
    try self.emitByte(instruction);
    try self.emitByte(0xff);
    try self.emitByte(0xff);
    return self.currentChunk.code.items.len - 2;
}

fn emitConstant(self: *Compiler, value: Value) !void {
    try self.emitBytes(@intFromEnum(OpCode.constant), try self.makeConstant(value));
}

fn patchJump(self: *Compiler, offset: usize) !void {
    const jump = self.currentChunk.code.items.len - offset - 2;

    if (jump > UINT16_MAX) try self.err("Too much code to jump over.");

    self.currentChunk.code.items[offset].byte = @intCast((jump >> 8) & 0xff);
    self.currentChunk.code.items[offset + 1].byte = @intCast(jump & 0xff);
}

fn makeConstant(self: *Compiler, value: Value) !u8 {
    const constant = self.currentChunk.addConstant(value) catch |erro| {
        errlog("Error: {any} in makeConstant.\nCurrent value: {any}\n", .{ erro, value });
        return InterpretError.InternalError;
    };
    if (constant > 255) {
        try self.err("Too many constants in one chunk.");
    }

    return @intCast(constant);
}

fn printStatement(self: *Compiler) !void {
    try self.expression();
    try self.consume(.semicolon, "Expect ';' after value.");
    try self.emitByte(@intFromEnum(OpCode.print));
}

fn forStatement(self: *Compiler) InterpretError!void {
    self.beginScope();
    try self.consume(.left_paren, "Expect '(' after 'for'.");
    if (self.match(.semicolon)) {
        // No init.
    } else if (self.match(.var_)) {
        try self.varDeclaration();
    } else {
        try self.expressionStatement();
    }

    var loop_start = self.currentChunk.code.items.len;
    var exit_jump: ?usize = null;
    if (!self.match(.semicolon)) {
        try self.expression();
        try self.consume(.semicolon, "Expect ';' after loop condition.");

        exit_jump = try self.emitJump(@intFromEnum(OpCode.jump_if_false));
        try self.emitByte(@intFromEnum(OpCode.pop));
    }

    if (!self.match(.right_paren)) {
        const body_jump = try self.emitJump(@intFromEnum(OpCode.jump));
        const increment_start = self.currentChunk.code.items.len;
        try self.expression();
        try self.emitByte(@intFromEnum(OpCode.pop));
        try self.consume(.right_paren, "Expect ')' after for clauses.");

        try self.emitLoop(loop_start);
        loop_start = increment_start;
        try self.patchJump(body_jump);
    }

    try self.statement();
    try self.emitLoop(loop_start);

    if (exit_jump) |exit| {
        try self.patchJump(exit);
        try self.emitByte(@intFromEnum(OpCode.pop));
    }

    try self.endScope();
}

fn whileStatement(self: *Compiler) InterpretError!void {
    const loop_start = self.currentChunk.code.items.len;
    try self.consume(.left_paren, "Expect '(' after 'while'.");
    try self.expression();
    try self.consume(.right_paren, "Expect ')' after condition.");

    const exit_jump = try self.emitJump(@intFromEnum(OpCode.jump_if_false));
    try self.emitByte(@intFromEnum(OpCode.pop));
    try self.statement();
    try self.emitLoop(loop_start);

    try self.patchJump(exit_jump);
    try self.emitByte(@intFromEnum(OpCode.pop));
}

fn synchronize(self: *Compiler) void {
    self.parser.panicMode = false;

    while (self.parser.current.type != .eof) {
        if (self.parser.previous.type == .semicolon) return;
        switch (self.parser.current.type) {
            .class, .fun, .var_, .for_, .if_, .while_, .print, .return_ => return,
            else => {},
        }

        self.advance();
    }
}

fn advance(self: *Compiler) void {
    self.parser.previous = self.parser.current;

    while (true) {
        self.parser.current = self.scanner.scanToken();

        if (self.parser.current.type != .error_) break;

        self.errorAtCurrent(self.parser.current.start[0..self.parser.current.length]) catch unreachable;
    }
}

fn expression(self: *Compiler) !void {
    try self.parsePrecedence(.assignment);
}

fn block(self: *Compiler) !void {
    while (!self.check(.right_brace) and !self.check(.eof)) {
        try self.declaration();
    }

    try self.consume(.right_brace, "Expect '}' after block.");
}

fn expressionStatement(self: *Compiler) !void {
    try self.expression();
    try self.consume(.semicolon, "Expect ';' after expression.");
    try self.emitByte(@intFromEnum(OpCode.pop));
}

fn ifStatement(self: *Compiler) InterpretError!void {
    try self.consume(.left_paren, "Expect '(' after 'if'.");
    try self.expression();
    try self.consume(.right_paren, "Expect ')' after confition.");

    const then_jump = try self.emitJump(@intFromEnum(OpCode.jump_if_false));
    try self.emitByte(@intFromEnum(OpCode.pop));
    try self.statement();

    const else_jump = try self.emitJump(@intFromEnum(OpCode.jump));

    try self.patchJump(then_jump);
    try self.emitByte(@intFromEnum(OpCode.pop));

    if (self.match(.else_)) try self.statement();
    try self.patchJump(else_jump);
}

fn switchStatement(self: *Compiler) InterpretError!void {
    try self.consume(.left_paren, "Expect '(' after 'switch'.");
    try self.expression();
    try self.consume(.right_paren, "Expect ')' after confition.");
    try self.consume(.left_brace, "Expect '{' before cases.");

    var jumps: [UINT8_MAX]usize = undefined;
    var case_jumps: [UINT8_MAX]usize = undefined;
    var count: usize = 0;
    var at_least_one = false;

    while (!self.check(.right_brace) and !self.check(.eof)) {
        if (count > 0) {
            try self.patchJump(case_jumps[count - 1]);
            try self.emitByte(@intFromEnum(OpCode.pop));
        }
        if (self.match(.case)) {
            try self.expression();
            try self.consume(.colon, "Expect ':' after 'expression'.");
            case_jumps[count] = try self.emitJump(@intFromEnum(OpCode.case));
            try self.emitByte(@intFromEnum(OpCode.pop));
            try self.statement();
            jumps[count] = try self.emitJump(@intFromEnum(OpCode.jump));
            at_least_one = true;
            count += 1;
        } else if (self.match(.default)) {
            try self.consume(.colon, "Expect ':' after 'default'.");
            try self.statement();
            at_least_one = true;
        } else {
            try self.err("Not switch or default");
        }
    }

    try self.consume(.right_brace, "Expect '}' after cases.");
    if (!at_least_one) {
        try self.err("At least one case or default reqired in switch statement");
        //return InterpretError.CompileError;
    }

    for (0..count) |i| {
        try self.patchJump(jumps[i]);
    }

    try self.emitByte(@intFromEnum(OpCode.pop));
}

fn varDeclaration(self: *Compiler) !void {
    const global = try self.parseVariable("Expect variable name.");

    if (self.match(.equal)) {
        try self.expression();
    } else {
        try self.emitByte(@intFromEnum(OpCode.nil));
    }

    try self.consume(.semicolon, "Expect ';' after variable declaration.");

    try self.defineVariable(global);
}

fn declaration(self: *Compiler) InterpretError!void {
    if (self.match(.var_)) {
        try self.varDeclaration();
    } else {
        try self.statement();
    }

    if (self.parser.panicMode) self.synchronize();
}

fn statement(self: *Compiler) !void {
    if (self.match(.print)) {
        try self.printStatement();
    } else if (self.match(.switch_)) {
        try self.switchStatement();
    } else if (self.match(.for_)) {
        try self.forStatement();
    } else if (self.match(.if_)) {
        try self.ifStatement();
    } else if (self.match(.while_)) {
        try self.whileStatement();
    } else if (self.match(.left_brace)) {
        self.beginScope();
        try self.block();
        try self.endScope();
    } else {
        try self.expressionStatement();
    }
}

fn parsePrecedence(self: *Compiler, prec: Precedence) !void {
    self.advance();
    const prefix_rule = getRule(self.parser.previous.type).prefix;
    const canAssign = @intFromEnum(prec) <= @intFromEnum(Precedence.assignment);

    if (prefix_rule) |f| {
        try f(self, canAssign);
    } else {
        try self.err("Expect expression.");
        return;
    }

    while (@intFromEnum(prec) <= @intFromEnum(getRule(self.parser.current.type).precedence)) {
        self.advance();
        const infix = getRule(self.parser.previous.type).infix;
        if (infix) |f| {
            try f(self, canAssign);
        }

        if (canAssign and self.match(.equal)) {
            try self.err("Invalid assignment target.");
        }
    }
}

fn parseVariable(self: *Compiler, errorMessage: []const u8) !u8 {
    try self.consume(.identifier, errorMessage);

    try self.declareVariable();
    if (self.scopeDepth > 0) return 0;

    return self.identifierConstant(&self.parser.previous);
}

fn markInitialized(self: *Compiler) !void {
    self.locals[@intCast(self.localCount - 1)].depth = self.scopeDepth;
    try self.emitBytes(@intFromEnum(OpCode.set_local), @intCast(self.localCount - 1));
}

fn defineVariable(self: *Compiler, global: u8) !void {
    if (self.scopeDepth > 0) {
        try self.markInitialized();
        return;
    }

    try self.emitBytes(@intFromEnum(OpCode.define_global), global);
}

fn identifierConstant(self: *Compiler, name: *Token) !u8 {
    const constant = try self.makeConstant(.{ .obj = .{ .string = object.ObjString.copyString(
        self.allocator,
        name.start[0..name.length],
        self.vm,
    ) catch |erro| {
        errlog("{any} in identifierConstant.\nCurrent name: {any}\n", .{ erro, name });
        return InterpretError.InternalError;
    } } });
    return constant;
}

fn identifiersEqual(a: *Token, b: *Token) bool {
    if (a.length != b.length) return false;
    return std.mem.eql(u8, a.start[0..a.length], b.start[0..a.length]);
}

fn resolveLocal(self: *Compiler, name: *Token) !isize {
    var i = self.localCount - 1;
    while (i >= 0) : (i -= 1) {
        const local = &self.locals[@intCast(i)];
        if (identifiersEqual(name, &local.name)) {
            if (local.depth == -1) try self.err("Can't read local variable in its own initializer.");
            return i;
        }
    }

    return -1;
}

fn addLocal(self: *Compiler, name: Token) !void {
    if (self.localCount == UINT8_MAX) {
        return self.err("Too many local variables in function.");
    }

    var local = &self.locals[@intCast(self.localCount)];
    self.localCount += 1;
    local.name = name;
    local.depth = -1;
}

fn declareVariable(self: *Compiler) !void {
    if (self.scopeDepth == 0) return;

    const name = &self.parser.previous;

    var i = self.localCount - 1;
    while (i >= 0) : (i -= 1) {
        const local = &self.locals[@intCast(i)];
        if (local.depth != -1 and local.depth < self.scopeDepth) {
            break;
        }

        if (identifiersEqual(name, &local.name)) {
            try self.err("Already a variable with this name in this scope.");
        }
    }

    try self.addLocal(name.*);
}

fn and_(self: *Compiler, _: bool) !void {
    const end_jump = try self.emitJump(@intFromEnum(OpCode.jump_if_false));

    try self.emitByte(@intFromEnum(OpCode.pop));
    try self.parsePrecedence(.and_);

    try self.patchJump(end_jump);
}

fn or_(self: *Compiler, _: bool) !void {
    const else_jump = try self.emitJump(@intFromEnum(OpCode.jump_if_false));
    const end_jump = try self.emitJump(@intFromEnum(OpCode.jump));

    try self.patchJump(else_jump);
    try self.emitByte(@intFromEnum(OpCode.pop));

    try self.parsePrecedence(.or_);
    try self.patchJump(end_jump);
}

fn number(self: *Compiler, _: bool) !void {
    const value = std.fmt.parseFloat(f32, self.parser.previous.start[0..self.parser.previous.length]) catch |erro| {
        errlog("{any} in number.\n", .{erro});
        return InterpretError.InternalError;
    };

    try self.emitConstant(Value{ .number = value });
}

fn string(self: *Compiler, _: bool) !void {
    try self.emitConstant(
        .{ .obj = .{
            .string = object.ObjString.copyString(
                self.allocator,
                self.parser.previous.start[1 .. self.parser.previous.length - 1],
                self.vm,
            ) catch |erro| {
                errlog("{any}", .{erro});
                return InterpretError.InternalError;
            },
        } },
    );
}

fn variable(self: *Compiler, canAssign: bool) !void {
    self.namedVariable(&self.parser.previous, canAssign) catch return InterpretError.CompileError;
}

fn namedVariable(self: *Compiler, name: *Token, canAssign: bool) !void {
    var get_op: u8 = undefined;
    var set_op: u8 = undefined;
    var arg = try self.resolveLocal(name);
    if (arg != -1) {
        get_op = @intFromEnum(OpCode.get_local);
        set_op = @intFromEnum(OpCode.set_local);
    } else {
        arg = @intCast(try self.identifierConstant(name));
        get_op = @intFromEnum(OpCode.get_global);
        set_op = @intFromEnum(OpCode.set_global);
    }

    if (canAssign and self.match(.equal)) {
        try self.expression();
        try self.emitBytes(set_op, @intCast(arg));
    } else {
        try self.emitBytes(get_op, @intCast(arg));
    }
}

fn unary(self: *Compiler, _: bool) !void {
    const operator_type = self.parser.previous.type;

    try self.parsePrecedence(.unary);

    switch (operator_type) {
        .bang => try self.emitByte(@intFromEnum(OpCode.not)),
        .minus => try self.emitByte(@intFromEnum(OpCode.negate)),
        else => unreachable,
    }
}

fn grouping(self: *Compiler, _: bool) !void {
    try self.expression();
    try self.consume(.right_paren, "Expect ')' after expression.");
}

fn binary(self: *Compiler, _: bool) !void {
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

fn literal(self: *Compiler, _: bool) !void {
    switch (self.parser.previous.type) {
        .false_ => try self.emitByte(@intFromEnum(OpCode.false_)),
        .nil => try self.emitByte(@intFromEnum(OpCode.nil)),
        .true_ => try self.emitByte(@intFromEnum(OpCode.true_)),
        else => unreachable,
    }
}

fn consume(self: *Compiler, comptime T: TokenType, message: []const u8) !void {
    if (self.parser.current.type == T) {
        self.advance();
        return;
    }

    try self.errorAtCurrent(message);
}

fn match(self: *Compiler, comptime T: TokenType) bool {
    if (!self.check(T)) return false;
    self.advance();
    return true;
}

fn check(self: *Compiler, comptime T: TokenType) bool {
    return self.parser.current.type == T;
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
