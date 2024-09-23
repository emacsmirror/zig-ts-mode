// FIXME: ok and not_ok here should be of `font-lock-constant-face`
const Type = enum {
  ok: u8 = 1,
  not_ok,
};

const allName = 1;

// ErrorSetDecl
const FileOpenError = error{
  AccessDenied,
  OutOfMemory,
  FileNotFound,
};



fn main() {
   const CAPS = 1;
}
