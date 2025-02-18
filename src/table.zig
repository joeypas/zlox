const std = @import("std");
const types = @import("types.zig");
const object = @import("object.zig");
const ObjString = object.ObjString;
const Value = types.Value;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const TABLE_MAX_LOAD = 0.75;

pub const Table = @This();

pub const Entry = struct {
    key: ?*ObjString = null,
    value: Value = .{ .nil = 0 },
};

allocator: Allocator,
count: usize,
capacity: usize,
entries: ArrayList(Entry),

pub fn init(allocator: Allocator) !Table {
    var list = try ArrayList(Entry).initCapacity(allocator, 1);
    list.appendAssumeCapacity(.{});
    return Table{
        .allocator = allocator,
        .count = 0,
        .capacity = 1,
        .entries = list,
    };
}

pub fn deinit(self: *Table) void {
    self.entries.deinit();
}

pub fn findEntry(entries: *ArrayList(Entry), capacity: usize, key: *ObjString) *Entry {
    var index: u32 = if (capacity == 0) 0 else @intCast(key.hash % capacity);
    var tombstone: ?*Entry = null;

    while (true) {
        const entry = &entries.items[index];
        if (entry.key == null) {
            if (Value.isNil(entry.value)) {
                return if (tombstone != null) tombstone.? else entry;
            } else {
                if (tombstone == null) tombstone = entry;
            }
        } else if (entry.key == key) {
            return entry;
        }

        index = (index + 1) % @as(u32, @intCast(capacity));
    }
}

pub fn get(self: *Table, key: *ObjString) ?*Entry {
    if (self.count == 0) return null;
    const ret = findEntry(&self.entries, self.capacity, key);
    return if (ret.key == null) null else ret;
}

pub fn set(self: *Table, key: *ObjString, value: Value) !bool {
    if (self.count + 1 > @as(usize, @intFromFloat(@ceil(@as(f32, @floatFromInt(self.capacity)) * TABLE_MAX_LOAD)))) {
        try self.growCapacity();
    }
    var entry = findEntry(&self.entries, self.capacity, key);
    const is_new_key = entry.key == null;
    if (is_new_key and Value.isNil(entry.value)) self.count += 1;

    entry.key = key;
    entry.value = value;
    return is_new_key;
}

pub fn delete(self: *Table, key: *ObjString) bool {
    if (self.count == 0) return false;

    const entry_maybe = self.get(key);
    if (entry_maybe) |entry| {
        entry.key = null;
        entry.value = .{ .boolean = true };
        return true;
    }
    return false;
}

pub fn addAll(from: *Table, to: *Table) !void {
    for (0..from.capacity) |i| {
        const entry = &from.entries.items[i];
        if (entry.key) |key| {
            to.set(key, entry.value);
        }
    }
}

pub fn findString(self: *Table, chars: []const u8, hash: u32) ?*ObjString {
    if (self.count == 0) return null;

    var index = hash % self.capacity;
    while (true) {
        const entry = &self.entries.items[index];
        if (entry.key) |key| {
            if (key.length == chars.len and key.hash == hash and std.mem.eql(u8, key.chars, chars)) {
                return key;
            }
        } else {
            if (Value.isNil(entry.value)) return null;
        }

        index = (index + 1) % @as(u32, @intCast(self.capacity));
    }
}

fn growCapacity(self: *Table) !void {
    var entries = try ArrayList(Entry).initCapacity(self.allocator, self.capacity * 2);

    for (0..self.capacity * 2) |_| {
        entries.appendAssumeCapacity(.{});
    }

    self.count = 0;
    for (0..self.capacity) |i| {
        const entry = &self.entries.items[i];
        if (entry.key) |key| {
            var dest = findEntry(&entries, self.capacity * 2, key);
            dest.key = key;
            dest.value = entry.value;
            self.count += 1;
        } else {
            continue;
        }
    }

    self.entries.deinit();
    self.entries = entries;
    self.capacity = self.capacity * 2;
}

test "table" {
    const VM = @import("vm.zig");
    const alloc = std.testing.allocator;

    var vm = VM.init(alloc, std.io.getStdOut(), std.io.getStdErr());
    defer vm.deinit();

    var table = try Table.init(alloc);
    defer table.deinit();

    var ins = try ObjString.copyString(alloc, "one", &vm);
    _ = try table.set(&ins.string, .{ .number = 1 });
    ins = try ObjString.copyString(alloc, "two", &vm);
    _ = try table.set(&ins.string, .{ .number = 2 });
    ins = try ObjString.copyString(alloc, "three", &vm);
    _ = try table.set(&ins.string, .{ .number = 3 });
    _ = table.delete(&ins.string);

    std.debug.print("{d} {d}\n", .{ table.capacity, table.count });
}
