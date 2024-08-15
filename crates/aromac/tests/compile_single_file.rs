use aromac::AromaC;
use log::info;
use std::path::PathBuf;
use test_log::test;

mod common;

#[test]
fn test_compile_single_file() -> eyre::Result<()> {
    let output_dir = common::target_dir();
    info!("compiling to {output_dir:?}");
    let mut aroma_c = AromaC::builder()
        .output_directory(&output_dir)
        .build()
        .expect("could not create aromac");

    let file = PathBuf::new().join("aroma_files").join("simple.aroma");

    let compiled = aroma_c.compile(file)?;

    Ok(())
}
