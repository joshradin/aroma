use std::fs::File;
use std::io;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

use itertools::Itertools;

const MAX_INPUT_PARAMS: usize = 2;

fn main() {
    println!("cargo::rerun-if-changed=build.rs");
    let out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    #[cfg(feature = "jit")]
    {
        let path = out_dir.join("abi.rs");
        let file = File::options()
            .create(true)
            .truncate(true)
            .write(true)
            .open(path)
            .unwrap();
        let mut writer = BufWriter::new(file);
        generate_abi_rs(&mut writer).unwrap();
    }
}

fn generate_abi_rs<W: Write>(w: &mut W) -> io::Result<()> {
    writeln!(
        w,
        r#"
use std::mem;
use std::ptr::NonNull;
use cranelift::codegen::trace;
use log::debug;
use crate::types::{{Type, Value}};
use crate::types::function::ObjFunction;"#
    )?;

    for i in 0..=MAX_INPUT_PARAMS {
        create_apply(i, w)?;
        create_accept(i, w)?;
    }

    create_call_jit(w)?;

    Ok(())
}

static TYPES: &[(&'static str, &'static str)] = &[
    ("Type::Long", "i64"),
    ("Type::Int", "i32"),
    ("Type::Byte", "i8"),
    ("Type::Boolean", "i8"),
    ("Type::Float", "i32"),
    ("Type::Double", "i64"),
];

fn create_call_jit<W: Write>(w: &mut W) -> io::Result<()> {
    writeln!(w, "pub fn call_jit(params: &[Value], func: &ObjFunction, ptr: NonNull<u8>) -> Option<Value> {{")?;
    writeln!(w, "let func_ptr = ptr.as_ptr();")?;
    writeln!(w, "unsafe {{")?;
    writeln!(w, "  match (func.params_ty(), func.return_ty()) {{")?;

    for inputs in 0..=MAX_INPUT_PARAMS {
        create_match_arm(inputs, w)?;
    }

    writeln!(
        w,
        r#"(_, _) => panic!(
                "can not handle functions of arity {{}} or more with return type {{:?}}",
                params.len(),
                func.return_ty()
            ),"#
    )?;
    writeln!(w, "  }}")?;
    writeln!(w, "}}")?;
    writeln!(w, "}}")?;
    Ok(())
}

fn create_match_arm<W: Write>(inputs: usize, w: &mut W) -> io::Result<()> {
    let input_permutations = TYPES.iter().permutations(inputs).collect::<Vec<_>>();

    for inputs in &input_permutations {
        writeln!(
            w,
            "([{types}], None) => {{ accept_{arity}(func_ptr,{params}) ; None }},",
            types = inputs.iter().map(|(ty, _)| ty).join(","),
            arity = inputs.len(),
            params = inputs
                .iter()
                .enumerate()
                .map(|(idx, (_, abi_ty))| format!(
                    "{abi_ty}::try_from(params[{idx}].clone()).unwrap()"
                ))
                .join(",")
        )?;
        for (out_ty, out_abi) in TYPES {
            writeln!(
                w,
                "([{types}], Some({out_ty})) => Some(apply_{arity}::<{input_abis}{out_abi}>(func_ptr,{params}).into()),",
                types = inputs.iter().map(|(ty, _)| ty).join(","),
                arity = inputs.len(),
                input_abis = inputs
                    .iter().map(|(_, abi_ty)| abi_ty).fold(String::new(), |accum, next| format!("{accum}{next},")),
                params = inputs
                    .iter()
                    .enumerate()
                    .map(|(idx, (_, abi_ty))| format!("{abi_ty}::try_from(params[{idx}].clone()).unwrap()"))
                    .join(",")
            )?;
        }
    }

    Ok(())
}

fn create_apply<W: Write>(params: usize, w: &mut W) -> io::Result<()> {
    let generics = (0..params).map(|i| format!("I{i}")).collect::<Vec<_>>();
    writeln!(
        w,
        r#"
#[doc = "Calls a function with {arity} inputs and returns an output"]
unsafe fn apply_{arity}<{param_ty}O>(ptr: *mut u8{param_pats}) -> O {{
    let func = mem::transmute::<_, fn({param_ty}) -> O>(ptr);
    func({params})
}}"#,
        arity = params,
        param_ty = generics
            .iter()
            .fold(String::new(), |accum, next| format!("{accum}{next},")),
        param_pats = generics
            .iter()
            .enumerate()
            .fold(String::new(), |accum, (idx, p)| format!(
                "{accum},p{idx}: {p}"
            )),
        params = generics
            .iter()
            .enumerate()
            .fold(String::new(), |accum, (idx, _)| format!("{accum}p{idx},")),
    )?;
    Ok(())
}
fn create_accept<W: Write>(params: usize, w: &mut W) -> io::Result<()> {
    let generics = (0..params).map(|i| format!("I{i}")).collect::<Vec<_>>();
    writeln!(
        w,
        r#"
#[doc = "Calls a function with {arity} inputs"]
unsafe fn accept_{arity}<{param_ty}>(ptr: *mut u8{param_pats}) {{
    let func = mem::transmute::<_, fn({param_ty})>(ptr);
    func({params});
}}"#,
        arity = params,
        param_ty = generics
            .iter()
            .fold(String::new(), |accum, next| format!("{accum}{next},")),
        param_pats = generics
            .iter()
            .enumerate()
            .fold(String::new(), |accum, (idx, p)| format!(
                "{accum},p{idx}: {p}"
            )),
        params = generics
            .iter()
            .enumerate()
            .fold(String::new(), |accum, (idx, _)| format!("{accum}p{idx},")),
    )?;
    Ok(())
}
