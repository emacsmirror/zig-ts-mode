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
    // PtrPayload + Payload
    while (eventuallyErrorSequence()) |value| {
         sum1 += value;
    } else |err| {
         try expect(err == error.ReachedZero);
    }
}
