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
        //
            .hi
    );

    errdefer asm volatile (""
        : [_] "=r,m" (k)
        : [_] "r,m" (a)
        : ""
    );

    const default = addrspace(0, 1);

    while (i < 10) {
        i += 1;
    }
}

test "PackedIntIo" {
    const bytes = [_]u8{
        0b01101_000, 0b01011_110,
        0b00011_101
    };
}

