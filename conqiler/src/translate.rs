use std::{
    error::Error,
    fs::File,
    io::Write,
    process::{Command, Output},
};

use melior::{
    pass::{self, PassManager},
    Context,
};
use tempfile::NamedTempFile;

pub fn compile_program_text(
    context: &melior::Context,
    module: &mut melior::ir::Module,
) -> Result<File, Box<dyn Error>> {
    lower_to_llvm_dialect(&context, module)?;

    let llir = module.as_operation().to_string();
    let llvmir_file = convert_mlir_to_llvmir(&llir)?;
    let wasm_file = convert_llvmir_to_wasm_text(llvmir_file)?;
    Ok(wasm_file.into_file())
}

pub fn compile_program(
    context: &melior::Context,
    module: &mut melior::ir::Module,
) -> Result<File, Box<dyn Error>> {
    lower_to_llvm_dialect(&context, module)?;

    let llir = module.as_operation().to_string();
    let llvmir_file = convert_mlir_to_llvmir(&llir)?;
    let wasm_file = convert_llvmir_to_wasm(llvmir_file)?;
    Ok(wasm_file.into_file())
}

pub fn compile_and_run_program(
    context: &melior::Context,
    module: &mut melior::ir::Module,
) -> Result<Output, Box<dyn Error>> {
    lower_to_llvm_dialect(&context, module)?;

    let llir = module.as_operation().to_string();
    let llvmir_file = convert_mlir_to_llvmir(&llir)?;
    let wasm_file = convert_llvmir_to_wasm(llvmir_file)?;
    let linked_wasm_file = NamedTempFile::new()?;
    let path = linked_wasm_file.path();

    let output = Command::new("wasm-ld-17")
        .arg("--allow-undefined")
        .arg("--export-all")
        .arg(wasm_file.path())
        .arg("-o")
        .arg(path)
        .output()?;

    linked_wasm_file.persist("wat.wasm")?;
    let path = "wat.wasm";

    eprintln!("{:?}", output);
    let output = Command::new("wasmer")
        .arg("run")
        .arg(path)
        .output()?;
    Ok(output)
}

pub fn lower_to_llvm_dialect(
    context: &melior::Context,
    module: &mut melior::ir::Module,
) -> Result<(), Box<dyn Error>> {
    //let module = melior::ir::Module::new(self.unknown_loc());
    //let block = module.body();

    // for e in prog.externs {
    //     block.append_operation(self.compile_extern(e)?);
    // }
    // for f in prog.funcs {
    //     block.append_operation(self.compile_func(f)?);
    // }

    //module.as_operation().dump();
    //println!("--");
    //assert!(module.as_operation().verify());
    assert!(module.as_operation().verify());

    // Convert to LLVM Dialect
    let pass_manager = PassManager::new(context);
    // pass_manager.add_pass(pass::r#async::create_async_func_to_async_runtime());
    // pass_manager.add_pass(pass::r#async::create_async_to_async_runtime());
    // pass_manager.add_pass(pass::conversion::create_async_to_llvm());
    pass_manager
        .nested_under("func.func")
        .add_pass(pass::conversion::create_arith_to_llvm());
    pass_manager
        .nested_under("func.func")
        .add_pass(pass::conversion::create_index_to_llvm());
    pass_manager.enable_verifier(true);
    pass_manager.add_pass(pass::transform::create_canonicalizer());
    pass_manager.add_pass(pass::conversion::create_scf_to_control_flow());
    pass_manager.add_pass(pass::conversion::create_arith_to_llvm());
    pass_manager.add_pass(pass::conversion::create_control_flow_to_llvm());
    pass_manager.add_pass(pass::conversion::create_index_to_llvm());
    pass_manager.add_pass(pass::conversion::create_finalize_mem_ref_to_llvm());
    pass_manager.add_pass(pass::conversion::create_func_to_llvm());
    pass_manager.add_pass(pass::conversion::create_reconcile_unrealized_casts());
    pass_manager.run(module)?;

    //module.as_operation().dump();
    assert!(module.as_operation().verify());
    Ok(())
}

pub fn convert_mlir_to_llvmir(s: &str) -> Result<NamedTempFile, Box<dyn Error>> {
    let mut inp = NamedTempFile::new()?;
    let out = NamedTempFile::new()?;
    inp.write(s.as_bytes())?;
    let _output = Command::new("mlir-translate-17")
        .arg("--target-abi=wasm32")
        .arg("--mlir-to-llvmir")
        .arg(inp.path())
        .arg("-o")
        .arg(out.path())
        .output()?;
    Ok(out)
}

pub fn convert_llvmir_to_wasm_text(inp: NamedTempFile) -> Result<NamedTempFile, Box<dyn Error>> {
    let out = NamedTempFile::new()?;
    let _output = Command::new("llc-17")
        .arg("-filetype=asm")
        .arg("-march=wasm32")
        .arg(inp.path())
        .arg("-o")
        .arg(out.path())
        .output()?;
    Ok(out)
}

pub fn convert_llvmir_to_wasm(inp: NamedTempFile) -> Result<NamedTempFile, Box<dyn Error>> {
    let out = NamedTempFile::new()?;
    let _output = Command::new("llc-17")
        .arg("-filetype=obj")
        .arg("-march=wasm32")
        .arg(inp.path())
        .arg("-o")
        .arg(out.path())
        .output()?;
    Ok(out)
}
