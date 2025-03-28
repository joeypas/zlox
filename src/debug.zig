const std = @import("std");
const types = @import("types.zig");
const Writer = std.io.AnyWriter;
const OpCode = types.OpCode;
const Chunk = types.Chunk;
const ArrayList = std.ArrayList;

const printValue = types.printValue;

pub fn dissasembleChunk(chunk: *Chunk, name: []const u8, out: Writer) !void {
    try out.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = try dissasembleInstruction(chunk, offset, out);
    }
}

fn dissasembleInstruction(chunk: *Chunk, offset: usize, out: Writer) !usize {
    try out.print("{d:0>4} ", .{offset});
    if (offset > 0 and chunk.code.items[offset].line == chunk.code.items[offset - 1].line) {
        try out.print("   | ", .{});
    } else {
        try out.print("{d:0>4} ", .{chunk.code.items[offset].line});
    }

    const instruction = chunk.code.items[offset].byte;
    switch (instruction) {
        @intFromEnum(OpCode.constant) => return constantInstruction("OP_CONSTANT", chunk, offset, out),
        @intFromEnum(OpCode.nil) => return simpleInstruction("OP_NIL", offset, out),
        @intFromEnum(OpCode.true_) => return simpleInstruction("OP_TRUE", offset, out),
        @intFromEnum(OpCode.false_) => return simpleInstruction("OP_FALSE", offset, out),
        @intFromEnum(OpCode.pop) => return simpleInstruction("OP_POP", offset, out),
        @intFromEnum(OpCode.get_local) => return byteInstruction("OP_GET_LOCAL", chunk, offset, out),
        @intFromEnum(OpCode.set_local) => return byteInstruction("OP_SET_LOCAL", chunk, offset, out),
        @intFromEnum(OpCode.get_global) => return constantInstruction("OP_GET_GLOBAL", chunk, offset, out),
        @intFromEnum(OpCode.define_global) => return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset, out),
        @intFromEnum(OpCode.set_global) => return constantInstruction("OP_SET_GLOBAL", chunk, offset, out),
        @intFromEnum(OpCode.equal) => return simpleInstruction("OP_EQUAL", offset, out),
        @intFromEnum(OpCode.greater) => return simpleInstruction("OP_GREATER", offset, out),
        @intFromEnum(OpCode.less) => return simpleInstruction("OP_LESS", offset, out),
        @intFromEnum(OpCode.add) => return simpleInstruction("OP_ADD", offset, out),
        @intFromEnum(OpCode.subtract) => return simpleInstruction("OP_SUBTRACT", offset, out),
        @intFromEnum(OpCode.multiply) => return simpleInstruction("OP_MULTIPLY", offset, out),
        @intFromEnum(OpCode.divide) => return simpleInstruction("OP_DIVIDE", offset, out),
        @intFromEnum(OpCode.not) => return simpleInstruction("OP_NOT", offset, out),
        @intFromEnum(OpCode.negate) => return simpleInstruction("OP_NEGATE", offset, out),
        @intFromEnum(OpCode.print) => return simpleInstruction("OP_PRINT", offset, out),
        @intFromEnum(OpCode.jump) => return jumpInstruction("OP_JUMP", 1, chunk, offset, out),
        @intFromEnum(OpCode.jump_if_false) => return jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset, out),
        @intFromEnum(OpCode.loop) => return jumpInstruction("OP_LOOP", -1, chunk, offset, out),
        @intFromEnum(OpCode.case) => return jumpInstruction("OP_CASE", 1, chunk, offset, out),
        @intFromEnum(OpCode.return_) => return simpleInstruction("OP_RETURN", offset, out),
        else => {
            try out.print("Unknown opcode {any}\n", .{instruction});
            return offset + 1;
        },
    }
}

fn simpleInstruction(name: []const u8, offset: usize, out: Writer) !usize {
    try out.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(name: []const u8, chunk: *Chunk, offset: usize, out: Writer) !usize {
    const constant = chunk.code.items[offset + 1].byte;
    try out.print("{s:<16} {d:0>4} '", .{ name, constant });
    try printValue(chunk.constants.items[constant], out);
    try out.print("'\n", .{});
    return offset + 2;
}

fn byteInstruction(name: []const u8, chunk: *Chunk, offset: usize, out: Writer) !usize {
    const slot = chunk.code.items[offset + 1].byte;
    try out.print("{s:<16} {d:0>4}\n", .{ name, slot });
    return offset + 2;
}

fn jumpInstruction(name: []const u8, sign: isize, chunk: *Chunk, offset: usize, out: Writer) !usize {
    var jump: u16 = @as(u16, @intCast(chunk.code.items[offset + 1].byte)) << 8;
    jump |= chunk.code.items[offset + 2].byte;

    try out.print("{s:<16} {d:0>4} -> {d}\n", .{ name, offset, @as(isize, @intCast(offset + 3)) + sign * @as(isize, @intCast(jump)) });
    return offset + 3;
}
