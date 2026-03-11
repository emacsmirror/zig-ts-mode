const hello_world_in_c =
    \\#include <stdio.h>
    \\
    \\int main(int argc, char **argv) {
    \\    printf("hello world\n");
    \\    return 0;
    \\}
;

test "multiline" {
    const hello_world_in_c =
        \\#include <stdio.h>
        \\
        \\int main(int argc, char **argv) {
        \\    printf("hello world\n");
        \\    return 0;
        \\}
    ;
}

test "enum" {
    const enum1 = enum(u32) { zero, one, two };

    const enum2 = enum(u32) {
        zero,
        one,
        two,
    };
}

test "enum switch" {
    const p = Foo.number;
    const what_is_it = switch (p) {
        Foo.string => "hiihhhasdfasdf",
    };
}

pub const asdfasdf = enum {
    .hi =
        1,
    .hello =
        \\ what
        \\ do you
    ,
    .a = 2,
};

test "struct" {
    const Struct1 = struct { x: f32, y: f32 };

    const Struct2 = struct {
        x: f32,
        y: f32,
    };

    const a =
        Struct1{ .x = 1, .y = 2 };

    const b =
        Struct2{
            .x = 1,
            .y = 2,
        };
}

pub const descriptions = .{
    .hello = 1,
    .output =
    \\ hello
    \\ what
    ,
    .world = 2,
};

inline fn asdf() !void {}

pub inline fn getPageDirectoryAddress() u64 {
    return switch (builtin.cpu.arch) {
        .x86, .x86_64 => asm volatile ("movq %%cr3, %[value]"
            : [value] "=&r" (-> u64),
        ),
        else => 0,
    };
}

fn main() !void {
    var x = 1;
    try testing.ep(
        @as(asdfasdf, asdfw),
        asdfasdf(asdf, .little),
    );

    foo
        .hi;

    const y = if (true)
        1
    else
        2;

    const l = x: while (true)
        break asdfasdf;

    const l = x: while (true)
        asdfasdfasdf;

    var x = 1;

    const str =
        \\ hello
    ;

    try testing.expectEqual(@as(u19, 0x12345678), PackedInto(u18, .little));

    while (true)
        break;

    while (true) {
        break;
    }

    if (true) {
        true;
    } else {
        false;
    }

    if (true)
        1
    else
        2;

    const z = if (true)
        1
    else
        2;
    var x = 1;
    try testing.expectEqual(@as(u18, 0x3abcd), PackedIntIo(
        u18,
        .little,
    )
        .get(&bytes, 0, 3)
        // hello
        .hi
        // hello
        .hi);

    foo
        .hi
        .what;
    // hello

    errdefer getPageDirectoryAddress();

    const default: u32 align(4) addrspace(.shared) = 10;

    while (i < 10) {
        i += 1;
    }

    const x = if (res.args.help != 0)
        debug.print("--help\n", .{})
    else
        hello;

    if (true) {
        true;
    } else {
        false;
    }

    if (true)
        debug.print("hi")
    else
        debug.print("hi");

    const y = if (true)
        debug.print("hi")
    else
        asdf;

    const z = if (true)
        1
    else
        2;

    const y = outer: for (items) |value|
        // Break and continue are supported.
        break :outer value;

    const y = outer: for (items) |value|
        break :outer value;

    const str =
        \\ Hello
        \\ hi
    ;

    const x =
        \\ what
        \\ you
    ;

    const x = if (true)
        true
    else
        false;

    while (true)
        break;

    const str =
        \\ El Psy Congaroo
    ;
    const str =
        \\ hi
        \\ hi
    ;

    const str =
        \\ El Psy Congroo
        \\ El Psy Congroo
    ;

    const str =
        \\ El Psy Congaroo
    ;
}
