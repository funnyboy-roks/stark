" Vim syntax file
" Language:     stark
" Maintainer:   funnyboy_roks
" Filenames:    *.st
" Version:      1.0

" Quit when a syntax file was already loaded.
if exists("b:current_syntax")
    finish
endif


syntax keyword Todo contained TODO FIXME XXX NOTE
syntax region Comment start=/\/\*/ end=/\*\// contains=Todo
syntax region Comment start=/\/\// end=/$/ contains=Todo

syntax match SpecialChar contained /\\./
syntax region String start=/c\?"/ end=/"/ contains=SpecialChar

syntax match Label /'[a-zA-Z0-9_]\+/
syntax match Function /\.\.\./

syntax keyword Keyword extern fn
syntax keyword Keyword dup dup2 drop extern fn swap

syntax keyword Boolean true false
syntax keyword Debug dump_stack

syntax keyword Conditional then else while
syntax keyword Repeat while
syntax keyword Label case default

syntax keyword Type i64 i32 i16 i8
syntax keyword Type u64 u32 u16 u8
syntax keyword Type ptr fatptr bool

syntax region Block start=/{/ end=/}/ transparent fold

syntax match Number display "\<[0-9][0-9_]*"

let b:current_syntax = "stark"
