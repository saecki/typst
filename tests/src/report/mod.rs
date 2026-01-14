mod diff;
mod html;

pub use self::diff::{DiffKind, File, FileReport, Old, image_diff, text_diff};

use std::fmt::Write as _;
use std::path::Path;

use ecow::EcoString;
use indexmap::IndexMap;
use rustc_hash::FxBuildHasher;

use crate::output::{HASH_OUTPUTS, HashedRefs};
use crate::{ARGS, STORE_PATH, run};

pub struct TestReport {
    pub name: EcoString,
    pub files: Vec<FileReport>,
}

impl TestReport {
    pub fn new(name: EcoString) -> Self {
        Self { name, files: Vec::new() }
    }
}

pub fn write(reports: &mut [TestReport]) {
    let report_path = Path::new(STORE_PATH).join("report.html");
    let missing_path = Path::new(STORE_PATH).join("missing.txt");

    if !ARGS.gen_report() {
        _ = std::fs::remove_file(report_path);
        _ = std::fs::remove_file(missing_path);
        return;
    }

    reports.sort_by(|a, b| a.name.cmp(&b.name));
    let html = html::generate(reports);
    std::fs::write(report_path, html).unwrap();

    // Check if old live output is missing, and could be regenerated.
    let mut missing_live = (reports.iter())
        .flat_map(|report| std::iter::repeat(&report.name).zip(report.files.iter()))
        .filter_map(|(name, file)| {
            let hash_ref = file.diffs.iter().find_map(DiffKind::missing_old)?;
            Some(((name.as_str(), file.output), hash_ref))
        })
        .collect::<IndexMap<_, _, FxBuildHasher>>();
    if missing_live.is_empty() {
        _ = std::fs::remove_file(missing_path);
        return;
    }
    missing_live.sort_by(|a, _, b, _| a.cmp(b));

    // Find the git revisions that in which the hash references that are missing
    // live output were committed.
    let mut newest_update_rev = None;
    let mut missing_old_revs = Vec::new();
    for output in HASH_OUTPUTS {
        let blame_lines = run::git_blame_file(&output.hash_refs_path()).unwrap();
        for (rev_hash, timestamp, text) in blame_lines {
            let (name, _) = HashedRefs::parse_line(&text).unwrap();

            if newest_update_rev.as_ref().is_none_or(|(_, t)| *t < timestamp) {
                newest_update_rev = Some((rev_hash.clone(), timestamp));
            }

            if !missing_live.contains_key(&(name.as_str(), output)) {
                continue;
            }
            if missing_old_revs.iter().any(|(r, _)| r == &rev_hash) {
                continue;
            }

            missing_old_revs.push((rev_hash, timestamp));
        }
    }
    let Some((newest_update_rev, _)) = newest_update_rev else {
        eprintln!("couldn't find git revision at which hash references were updated");
        _ = std::fs::remove_file(missing_path);
        return;
    };
    if missing_old_revs.is_empty() {
        eprintln!("warning: couldn't find git revisions for missing live outputs");
    }
    missing_old_revs.sort_by_key(|(_, timestamp)| *timestamp);

    let mut text = String::new();
    writeln!(text, "newest-update-rev: {newest_update_rev}").unwrap();
    writeln!(text, "missing-old-revs:").unwrap();
    for (rev_hash, _) in missing_old_revs.iter().rev() {
        writeln!(text, "- {rev_hash}").unwrap();
    }
    writeln!(text, "missing-live:").unwrap();
    for ((name, output), hash_ref) in missing_live.iter() {
        let path = output.hash_path(*hash_ref, name);
        writeln!(text, "- {}", path.display()).unwrap();
    }

    std::fs::write(missing_path, text).unwrap();
}
