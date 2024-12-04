(module
  (type (;0;) (func (param i64) (result i32)))
  (type (;1;) (func))
  (type (;2;) (func (param i32)))
  (import "env" "malloc" (func $malloc (type 0)))
  (func $__wasm_call_ctors (type 1))
  (func $_start (type 2) (param i32)
    (local i32 i32 i32)
    i64.const 4
    call $malloc
    local.tee 1
    i32.const 2
    i32.store
    i64.const 4
    call $malloc
    local.tee 2
    i32.const 3
    i32.store
    i64.const 4
    call $malloc
    i32.const 5
    i32.store
    local.get 2
    i32.const 5
    i32.store
    i64.const 4
    call $malloc
    i32.const 2
    i32.store
    local.get 1
    i32.load
    local.set 3
    i64.const 1
    call $malloc
    local.get 3
    i32.const 2
    i32.eq
    i32.store8
    i32.const 45
    local.set 1
    block  ;; label = @1
      local.get 3
      i32.const 2
      i32.eq
      br_if 0 (;@1;)
      i32.const 67
      local.set 1
    end
    i64.const 4
    call $malloc
    local.get 1
    i32.store
    local.get 2
    local.get 1
    i32.store
    local.get 0
    i64.const 0
    i64.store offset=8
    local.get 0
    local.get 2
    i32.store offset=4
    local.get 0
    local.get 2
    i32.store)
  (memory (;0;) 2)
  (global $__stack_pointer (mut i32) (i32.const 66560))
  (global (;1;) i32 (i32.const 1024))
  (global (;2;) i32 (i32.const 1024))
  (global (;3;) i32 (i32.const 1024))
  (global (;4;) i32 (i32.const 66560))
  (global (;5;) i32 (i32.const 1024))
  (global (;6;) i32 (i32.const 66560))
  (global (;7;) i32 (i32.const 131072))
  (global (;8;) i32 (i32.const 0))
  (global (;9;) i32 (i32.const 1))
  (export "memory" (memory 0))
  (export "__wasm_call_ctors" (func $__wasm_call_ctors))
  (export "_start" (func $_start))
  (export "__dso_handle" (global 1))
  (export "__data_end" (global 2))
  (export "__stack_low" (global 3))
  (export "__stack_high" (global 4))
  (export "__global_base" (global 5))
  (export "__heap_base" (global 6))
  (export "__heap_end" (global 7))
  (export "__memory_base" (global 8))
  (export "__table_base" (global 9)))
