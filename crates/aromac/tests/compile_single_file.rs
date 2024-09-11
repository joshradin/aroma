use aromac::error::AromaCError;
use aromac::AromaC;
use eyre::eyre;
use tracing::{error, info};
use std::path::PathBuf;
use test_log::test;

mod common;

#[test(tokio::test)]
async fn test_compile_single_file() -> eyre::Result<()> {
    let output_dir = common::target_dir();
    info!("compiling to {output_dir:?}");
    let mut aroma_c = AromaC::builder()
        .output_directory(&output_dir)
        .build()
        .expect("could not create aromac");

    let file = PathBuf::from("tests")
        .join("aroma_files")
        .join("simple.aroma");

    aroma_c.compile(&*file).await?;

    Ok(())
}
