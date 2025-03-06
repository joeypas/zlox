const std = @import("std");
const types = @import("types.zig");
const debug = @import("debug.zig");
const VM = @import("vm.zig");
const io = std.io;
const proc = std.process;
const OpCode = types.OpCode;
const Chunk = types.Chunk;
const Allocator = std.mem.Allocator;

pub fn repl(vm: *VM, out: io.AnyWriter, err_out: io.AnyWriter) void {
    var stdin = io.getStdIn();
    defer stdin.close();
    var in = stdin.reader();
    var line_buf: [1024]u8 = undefined;

    while (true) {
        out.print("> ", .{}) catch unreachable;
        const line = in.readUntilDelimiter(&line_buf, '\n') catch {
            out.print("\n", .{}) catch unreachable;
            break;
        };
        if (std.mem.eql(u8, line, "exit")) break;
        vm.interpret(line) catch |err| {
            err_out.print("{any}\n", .{err}) catch unreachable;
        };
    }
}
pub fn runFile(vm: *VM, allocator: Allocator, path: []const u8, err_out: io.AnyWriter) !void {
    const file = std.fs.openFileAbsolute(path, .{}) catch |err| {
        try err_out.print("Could not open file \"{s}\".\n", .{path});
        return err;
    };
    defer file.close();
    const meta = file.stat() catch |err| {
        try err_out.print("Could not determine file size\n", .{});
        return err;
    };
    const source = file.readToEndAlloc(allocator, meta.size) catch |err| {
        try err_out.print("Not enough memory to read \"{s}\".\n", .{path});
        return err;
    };
    defer allocator.free(source);

    try vm.interpret(source[0..]);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const alloc = gpa.allocator();

    const args = try proc.argsAlloc(alloc);
    defer proc.argsFree(alloc, args);

    var stdout = io.getStdOut();
    defer stdout.close();
    var stderr = io.getStdErr();
    defer stderr.close();

    var vm = try VM.init(alloc, stdout, stderr);
    defer vm.deinit();

    if (args.len == 1) {
        repl(&vm, stdout.writer().any(), stderr.writer().any());
    } else if (args.len == 2) {
        try runFile(&vm, alloc, args[1], stderr.writer().any());
    } else {
        _ = try stderr.write("Usage: zlox [path]\n");
        proc.exit(64);
    }
}
