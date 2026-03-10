//! Hi

const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;

var ERROR = 1;

const Air = @This();
const InternPool = @import("InternPool.zig");
const Type = @import("Type.zig");
const Value = @import("Value.zig");
const Zcu = @import("Zcu.zig");
const print = @import("Air/print.zig");
const types_resolved = @import("Air/types_resolved.zig");

pub const Legalize = @import("Air/Legalize.zig");
pub const Liveness = @import("Air/Liveness.zig");

pub extern "c" fn @"fstat$INODE64"(fd: c.fd_t, buf: *c.Stat) c_int;

fn foo(w: *Wat) callconv(.c) void {
    bar(w);
}

const o = opaque {};

const Value3 = enum(u4) {
    a,
    b: bbb = 8,
    c,
    d = 4,
    e,
};

// comment

const POINT = struct {
    x: f32,
    y: f32,
};

const AllocationError = error{
    OutOfMemory,
};

pub fn main() void {
    var y: i32 = 5678;

    var Point = struct {
        x: f32,
        y: f32,
    };

    y += 1;

    var _ = 1;

    const my_user = User{
        .id = 1,
        .username = "Christina",
        .active = true,
    };

    const x = blk: {
        y += 1;
        break :blk y;
    };

    var z: [*c]T = "1\n";

    print("{d}", .{y});

    std.debug.print("ptr={*}\n", .xx{ptr});

    var x = i32;

    switch (c) {
        .ok => |value| try expect(value == 42),
        .not_ok => unreachable,
        5...100 => 1,
    }
}
