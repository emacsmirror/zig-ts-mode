const X = struct (){
}

fn main() !void {
    var x = 1;
    try testing.expectEqual(
        @as(u18, 0x3abcd),
        PackedIntIo(u18, .little)
            .get(&bytes,
                0, 3
            )
            // hello
            .hi
            // hello
            .hi
            // hello
    );

    foo
        .hi
        .what;
    // hello

    errdefer asm volatile (""
        : [_] "=r,m" (k)
        : [_] "r,m" (a)
        : ""
    );

    const default = addrspace(0, 1);

    while (i < 10) {
        i += 1;
    }

    const x = if (res.args.help != 0)
        debug.print("--help\n", .{})
    else
        hello;

    const y = outer: for (items) |value|
        // Break and continue are supported.
        break :outer value;

    while (true) 
        break;

    const str =
        \\ El Psy Congroo
        \\ El Psy Congroo
    ;
}

test "PackedIntIo" {
    const bytes = [_]u8{
        0b01101_000, 0b01011_110,
        0b00011_101
    };
}

