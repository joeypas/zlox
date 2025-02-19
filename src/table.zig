const std = @import("std");
const types = @import("types.zig");
const object = @import("object.zig");
const ObjString = object.ObjString;
const Obj = object.Obj;
const Value = types.Value;
const ArrayList = std.ArrayList;
const Allocator = std.mem.Allocator;

const TABLE_MAX_LOAD = 0.75;

pub const Table = @This();

pub const Entry = struct {
    key: ?*Obj = null,
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

pub fn findEntry(entries: *ArrayList(Entry), capacity: usize, key: *Obj) *Entry {
    var index: usize = @intCast(key.string.hash % capacity);
    var tombstone: ?*Entry = null;

    while (true) {
        const entry = &entries.items[index];
        if (entry.key == null) {
            if (Value.isNil(entry.value)) {
                return if (tombstone != null) tombstone.? else entry;
            } else {
                if (tombstone == null) tombstone = entry;
            }
        } else if (entry.key.? == key) {
            return entry;
        }

        index = (index + 1) % capacity;
    }
}

pub fn get(self: *Table, key: *Obj) ?*Entry {
    if (self.count == 0) return null;
    const ret = findEntry(&self.entries, self.capacity, key);
    return if (ret.key == null) null else ret;
}

pub fn set(self: *Table, key: *Obj, value: Value) !bool {
    if (self.count + 1 > @as(usize, @intFromFloat(@floor(@as(f32, @floatFromInt(self.capacity)) * TABLE_MAX_LOAD)))) {
        try self.growCapacity();
    }
    var entry = findEntry(&self.entries, self.capacity, key);
    const is_new_key = entry.key == null;
    if (is_new_key and Value.isNil(entry.value)) self.count += 1;

    entry.key = key;
    entry.value = value;
    return is_new_key;
}

pub fn delete(self: *Table, key: *Obj) bool {
    if (self.count == 0) return false;

    var entry = self.get(key);
    if (entry != null) {
        entry.?.key = null;
        entry.?.value = .{ .boolean = true };
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

pub fn findString(self: *Table, chars: []const u8, hash: u32) ?*Obj {
    if (self.count == 0) return null;

    var index: usize = @intCast(hash % self.capacity);
    while (true) {
        const entry = &self.entries.items[index];
        if (entry.key == null) {
            if (Value.isNil(entry.value)) return null;
        } else if (entry.key.?.string.length == chars.len and entry.key.?.string.hash == hash and std.mem.eql(u8, entry.key.?.string.chars, chars)) {
            return entry.key.?;
        }

        if (index == 0) return null;
        index = (index + 1) % self.capacity;
    }
}

fn growCapacity(self: *Table) !void {
    const new_cap = if (self.capacity < 8) 8 else self.capacity * 2;
    var entries = try ArrayList(Entry).initCapacity(self.allocator, new_cap);

    for (0..new_cap) |_| {
        entries.appendAssumeCapacity(.{});
    }

    self.count = 0;
    for (0..self.capacity) |i| {
        const entry = &self.entries.items[i];
        if (entry.key) |key| {
            var dest = findEntry(&entries, new_cap, key);
            dest.key = key;
            dest.value = entry.value;
            self.count += 1;
        }
    }

    self.entries.deinit();
    self.entries = entries;
    self.capacity = new_cap;
}

test "table" {
    const VM = @import("vm.zig");
    const alloc = std.testing.allocator;

    var vm = try VM.init(alloc, std.io.getStdOut(), std.io.getStdErr());
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
