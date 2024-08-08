use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

pub fn target_dir() -> PathBuf {
    let target = Path::new(env!("CARGO_TARGET_TMPDIR"));
    let buf = target.join(format!(
        "target-{:?}",
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_secs()
    ));
    std::fs::create_dir_all(&buf).expect("could not create directory");
    buf
}
