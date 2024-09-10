// AsmOutputItem + AsmInputItem
pub fn syscall1(number: usize, arg1: usize) usize {
    return asm volatile ("syscall"
        : [ret] "={rax}" (-> usize),
        : [number] "{rax}" (number),
          [arg1] "{rdi}" (arg1),
        : "rcx", "r11"
    );
}

fn main() !void {
    // PtrPayload
    while (eventuallyErrorSequence()) |value| {
    // Payload
    } else |err| {
    
    }

    // PtrListPayload, SuffixExpr
    for (items, 0..) |value, i| {
    }

    const b = switch (a) {
        // PtrIndex Payload
        Item.a, Item.e => |item| item,
    };

    // FieldOrFnCall
    x.y;
}

