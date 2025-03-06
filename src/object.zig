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
    string: *ObjString,

    pub fn getType(self: Obj) ObjType {
        return std.meta.activeTag(self);
    }

    pub fn isType(self: Obj, comptime T: ObjType) bool {
        return self.getType() == T;
    }

    pub fn create(allocator: Allocator, vm: *VM) !*Obj {
        const ret = try allocator.create(Obj);
        try vm.objects.append(ret);
        return ret;
    }

    pub fn destroy(self: Obj) void {
        switch (self) {
            .string => |str| str.destroy(),
        }
    }

    pub fn printObject(self: Obj, out: std.io.AnyWriter) !void {
        switch (self) {
            .string => |str| {
                try out.print("{s}", .{str.asArray()});
            },
        }
    }
};

pub fn isObjType(value: Value, comptime T: ObjType) bool {
    return value.isObj() and value.obj.isType(T);
}

pub fn objEql(a: Value, b: Value) bool {
    if (Obj.getType(a.obj) != Obj.getType(b.obj)) return false;

    return switch (a.obj) {
        .string => a.obj.string == b.obj.string,
    };
}

pub fn hashString(key: []const u8) u32 {
    return std.hash.Fnv1a_32.hash(key);
}

pub const ObjString = struct {
    //obj: *Obj,
    allocator: Allocator,
    length: usize = 0,
    chars: []u8 = undefined,
    hash: u32,

    pub fn create(allocator: Allocator, chars: []u8, vm: *VM, hash: u32) !*ObjString {
        const ret = try allocator.create(ObjString);
        ret.* = ObjString{
            //.obj = ret,
            .chars = chars,
            .length = chars.len,
            .allocator = allocator,
            .hash = hash,
        };
        _ = try vm.strings.set(.{ .string = ret }, .{ .nil = 0 });

        return ret;
    }

    pub fn copyString(allocator: Allocator, chars: []const u8, vm: *VM) !*ObjString {
        const hash = hashString(chars);
        const interned = Table.findString(&vm.strings, chars, hash);
        if (interned) |ret| return ret.string;

        const heapChars = try allocator.dupe(u8, chars);
        return ObjString.create(allocator, heapChars, vm, hash);
    }

    pub fn takeString(allocator: Allocator, chars: []u8, vm: *VM) !*ObjString {
        const hash = hashString(chars);
        const interned = Table.findString(&vm.strings, chars, hash);

        if (interned) |ret| {
            allocator.free(chars);
            return ret.string;
        }

        return ObjString.create(allocator, chars, vm, hash);
    }

    pub fn destroy(self: *ObjString) void {
        self.allocator.free(self.chars);
        self.allocator.destroy(self);
    }

    pub fn asArray(self: *ObjString) []u8 {
        return self.chars;
    }
};
