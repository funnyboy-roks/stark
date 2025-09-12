" Vim syntax file
" Language:     stark
" Maintainer:   funnyboy_roks
" Filenames:    *.st
" Version:      1.0

" Quit when a syntax file was already loaded.
if exists("b:current_syntax")
    finish
endif


hi def link starkNumber Number
hi def link starkFloat starkNumber
hi def link starkDecNumber starkNumber
hi def link starkHexNumber starkNumber
hi def link starkBinNumber starkNumber
hi def link starkKeyword Keyword
hi def link starkComment Comment
hi def link starkLineComment starkComment
hi def link starkBlockComment starkComment
hi def link starkString String
hi def link starkSpecialChar SpecialChar
hi def link starkLabel Label
hi def link starkVariadic Function
hi def link starkBoolean Boolean
hi def link starkConditional Conditional
hi def link starkRepeat Repeat
hi def link starkType Type

syntax keyword Todo contained TODO FIXME XXX NOTE
syntax region starkBlockComment start=/\/\*/ end=/\*\// contains=Todo
syntax region starkLineComment start=/\/\// end=/$/ contains=Todo

syntax match starkSpecialChar contained /\\\(0[0-9]\{1,3\}\|x[0-9a-fA-F]\{1,2\}\|.\)/
syntax region starkString start=/c\?"/ end=/"/ contains=starkSpecialChar

syntax match starkLabel /'[a-zA-Z0-9_]\+/
syntax match starkVariadic /\.\.\./

syntax keyword starkKeyword extern fn
syntax keyword starkKeyword dup dup2 drop extern fn swap cast
syntax keyword starkKeyword break

syntax keyword starkBoolean true false

syntax keyword starkConditional then else
syntax keyword starkRepeat while
syntax keyword starkLabel case default

syntax keyword starkType i64 i32 i16 i8
syntax keyword starkType u64 u32 u16 u8
syntax keyword starkType f32 f64
syntax keyword starkType fatptr bool

syntax match starkDecNumber display "\<[0-9][0-9_]*"
syntax match starkHexNumber display "\<0x[a-fA-F0-9_]\+"
syntax match starkBinNumber display "\<0b[01_]\+"
syntax match starkFloat     display "\<[0-9][0-9_]*\.[0-9][0-9_]*"

syntax region Block start=/{/ end=/}/ transparent fold

let b:current_syntax = "stark"

