" Basic syntax highlighting for Icarus in Vim.
" You can enable this by default for files with the `.ic` extension by putting
" the following in your .vimrc, and placing this file in a location that vim
" will find it on startup (such as the directory `~/.vim/syntax/`).
"
" ```
" au BufNewFile,BufRead *.ic set filetype=icarus
" ```

if exists("b:current_syntax")
  finish
endif

syntax match Operator ":*=*"
syntax match Operator "[,'$]"
syntax match Operator "[~`+*\-/%<>&|!]=\?"
syntax match Identifier "#[a-zA-Z_=0-9()"]\+"
syntax match Identifier "#{[a-zA-Z_=0-9 ()"]\+}"
syntax match Identifier "#\.[a-zA-Z_0-9]\+"
syntax region MultiLineComment start=/\/\*/ skip=/\\./ end=/\*\// contains=MultiLineComment
hi link MultiLineComment Comment

syntax keyword Keyword move copy import flags enum struct return jump scope as goto foreign bytes align opaque block init destroy and or xor not abort interface slice callable reserve_memory compilation_error
syntax keyword Boolean    true false
syntax keyword Constant   bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 type module null char byte
syntax match   Character  "![^\\]\|!\\[abfnrtv0]"
syntax match   Number     "\([_a-zA-Z]\)\@<!\d\+\.\?\d*"
syntax match   Comment    "\/\/.*$"
syntax region  String     start=/\v"/ skip=/\v\\./ end=/\v"/
syntax match   Function   "[a-zA-Z_0-9]\+\((\)\@="
syntax match   Function   "\('\)\@<=[a-zA-Z_0-9]\+"

let b:current_syntax = "icarus"
