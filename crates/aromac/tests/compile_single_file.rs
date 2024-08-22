use aromac::{AromaC, AromaCError};
use log::{error, info};
use std::path::PathBuf;
use eyre::eyre;
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

    let file = PathBuf::from("tests").join("aroma_files").join("simple.aroma");

    let c= match aroma_c.compile(&*file) {
        Ok(ok) => {}
        Err(e) => {
            return Err(eyre!("{e}"))
        }
    };

    Ok(())
}
