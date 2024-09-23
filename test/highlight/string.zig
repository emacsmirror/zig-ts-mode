fn main() {
  std.debug.print("Hello %s", .{'\u{1f4a9}'});
  // TODO FormatSequence

  const hello_world_in_c =
  \\#include <stdio.h>
  \\
  \\int main(int argc, char **argv) {
  \\    printf("hello world\n");
  \\    return 0;
  \\}
  ;
}
