(module
  (type (;0;) (func (param i64) (result i64)))
  (import "env" "__linear_memory" (memory (;0;) 0))
  (import "env" "print" (func $print (param i64)))
  (func $_start (type 0) (param i64) (result i64)
    i64.const 69
    call $print
    i64.const 32)
  (export "_start" (func $_start)))
