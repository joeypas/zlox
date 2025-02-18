const std = @import("std");
const types = @import("types.zig");
const VM = @import("vm.zig");
const Allocator = std.mem.Allocator;
const Value = types.Value;
const Table = @import("table.zig").Table;

pub const ObjType = enum {
    string,
};

pub const Obj = union(ObjType) {
    string: ObjString,

    pub fn getType(self: *Obj) ObjType {
        return std.meta.activeTag(self.*);
    }

    pub fn isType(self: *Obj, comptime T: ObjType) bool {
        return self.getType() == T;
    }

    pub fn create(allocator: Allocator, vm: *VM) !*Obj {
        const ret = try allocator.create(Obj);
        try vm.objects.append(ret);
        return ret;
    }

    pub fn destroy(self: *Obj) void {
        switch (self.getType()) {
            .string => ObjString.destroy(self),
        }
    }

    pub fn printObject(self: *Obj, out: std.io.AnyWriter) !void {
        switch (self.*) {
            .string => |str| {
                try out.print("{s}", .{str.asArray()});
            },
        }
    }
};

pub fn isObjType(value: Value, comptime T: ObjType) bool {
    return value.isObj() and value.obj.isType(T);
}

pub fn hashString(key: []const u8) u32 {
    return std.hash.Fnv1a_32.hash(key);
}

pub const ObjString = struct {
    obj: *Obj,
    allocator: Allocator,
    length: usize = 0,
    chars: []u8 = undefined,
    hash: u32,

    pub fn create(allocator: Allocator, chars: []u8, vm: *VM, hash: u32) !*Obj {
        var ret = try Obj.create(allocator, vm);
        ret.string = .{
            .obj = ret,
            .chars = chars,
            .length = chars.len,
            .allocator = allocator,
            .hash = hash,
        };
        _ = try vm.strings.set(&ret.string, .{ .nil = 0 });

        return ret;
    }

    pub fn copyString(allocator: Allocator, chars: []const u8, vm: *VM) !*Obj {
        const hash = hashString(chars);
        const interned = Table.findString(&vm.strings, chars, hash);
        if (interned) |ret| return ret.obj;

        const heapChars = try allocator.dupe(u8, chars);
        return ObjString.create(allocator, heapChars, vm, hash);
    }

    pub fn takeString(allocator: Allocator, chars: []u8, vm: *VM) !*Obj {
        const hash = hashString(chars);
        const interned = Table.findString(&vm.strings, chars, hash);

        if (interned) |ret| {
            allocator.free(chars);
            return ret.obj;
        }

        return ObjString.create(allocator, chars, vm, hash);
    }

    pub fn destroy(self: *Obj) void {
        self.string.allocator.free(self.string.chars);
        self.string.allocator.destroy(self);
    }

    pub fn asArray(self: ObjString) []u8 {
        return self.chars;
    }
};
