"Syntax definition file for the Icarus programming language.
"Copy this file into `~/.vim/syntax/icarus.vim`, and add the following line to
"your .vimrc:
"au BufNewFile,BufRead *.ic set filetype=icarus

if exists("b:current_syntax")
  finish
endif

syntax keyword Keyword move copy import flags enum struct return jump scope as goto foreign bytes align opaque block init destroy and or xor
syntax match Operator ":*=*"
syntax match Operator "[,'$]"
syntax match Operator "[~`+*\-/%<>&|!]=\?"
syntax region String start=/\v"/ skip=/\v\\./ end=/\v"/
syntax keyword Constant true false bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 type module null
syntax match Constant "\<\d+\(\.\d*\)\?"
syntax match Identifier "#[a-zA-Z_=0-9()"]\+"
syntax match Identifier "#{[a-zA-Z_=0-9 ()"]\+}"
syntax match Identifier "#\.[a-zA-Z_0-9]\+"
syntax match Comment "\/\/.*$"
syntax region MultiLineComment start=/\/\*/ skip=/\\./ end=/\*\// contains=MultiLineComment
hi link MultiLineComment Comment
let b:current_syntax = "icarus"
