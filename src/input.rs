use crate::DynResult;
use std::ffi::OsStr;
use std::io::{BufRead, BufReader, Read, Write as _};
#[cfg(unix)]
use std::os::unix::ffi::OsStrExt as _;
use std::path::PathBuf;
use std::process::{Command, Stdio};

#[derive(Default)]
pub struct ProgramInput {
    pub file_input: Vec<[String; 2]>,
    pub file_names: Vec<[String; 2]>,
    pub file_status_text: Vec<String>,
}

// WTF
fn concat<'a, A: AsRef<str>, B: AsRef<str>>(a: &'a [A], b: &'a [B]) -> impl Iterator<Item = &'a str> {
    a.iter().map(AsRef::as_ref).chain(b.iter().map(AsRef::as_ref))
}

fn read_token(input: &mut impl BufRead) -> DynResult<Vec<u8>> {
    let mut buffer = vec![];
    input.read_until(0, &mut buffer)?;
    if buffer.is_empty() {
        return Err("unexpected EOF reading from git diff")?;
    }
    if buffer[buffer.len() - 1] != 0 {
        return match std::str::from_utf8(&buffer) {
            Ok(s) => Err(format!("unexpected output from git diff: {}", s.trim()))?,
            Err(_) => Err(format!("unexpected output from git diff: {:?}", buffer))?,
        };
    }
    buffer.pop();
    Ok(buffer)
}

fn read_exact(input: &mut impl Read, size: usize) -> DynResult<Vec<u8>> {
    let mut buffer = vec![0; size];
    input.read_exact(&mut buffer)?;
    Ok(buffer)
}

fn read_number(input: &mut impl BufRead) -> DynResult<usize> {
    Ok(std::str::from_utf8(&read_token(input)?)?.parse()?)
}

fn join_path(parent: &[u8], child: &[u8]) -> DynResult<PathBuf> {
    #[cfg(unix)]
    {
        return Ok([OsStr::from_bytes(parent), OsStr::from_bytes(child)].iter().collect());
    }
    #[cfg(windows)]
    {
        // Vxdiff was not tested on Windows. This code is a best effort guess.
        // Git for Windows always prints paths with UTF-8, right?
        // The result of `git rev-parse --show-toplevel` is a normal Windows path Rust will
        // understand, starting with a drive letter, right?
        let parent = String::from_utf8(parent)?;
        let child = String::from_utf8(child)?;
        return Ok([parent, child].iter().collect());
    }
    #[allow(unreachable_code)]
    {
        unimplemented!()
    }
}

pub fn run_git_diff(current_exe: &str, git_diff_args: &[String], pager_args: &[String]) -> DynResult<()> {
    let args = concat(&["diff"], git_diff_args);
    let git_external_diff = shell_words::join([current_exe, "--git-external-diff"]);
    let git_pager = shell_words::join(concat(&[current_exe, "--git-pager-hack"], pager_args));
    let exit_status = Command::new("git")
        .args(args)
        .env("GIT_EXTERNAL_DIFF", git_external_diff)
        .env("GIT_PAGER", git_pager)
        .status()?;
    // TODO: std::process::ExitStatus::exit_ok() is unstable
    if exit_status.success() {
        Ok(())
    } else {
        Err(format!("running git diff failed: {exit_status}"))?
    }
}

fn get_working_tree_root() -> DynResult<Vec<u8>> {
    let std::process::Output {
        status,
        mut stdout,
        stderr: _,
    } = Command::new("git")
        .args(["rev-parse", "--show-toplevel"])
        .stderr(Stdio::inherit())
        .output()?;
    // TODO: std::process::ExitStatus::exit_ok() is unstable
    if !status.success() {
        return Err(format!("running git rev-parse --show-toplevel failed: {status}"))?;
    }
    match stdout.pop() {
        Some(b'\n') => {}
        Some(_) => return Err("git rev-parse --show-toplevel output doesn't end with a newline")?,
        None => return Err("git rev-parse --show-toplevel printed nothing (bare Git repo?)")?,
    }
    Ok(stdout)
}

pub fn read_by_running_git_diff_raw(git_diff_args: &[String]) -> DynResult<ProgramInput> {
    let mut working_tree_root = None;

    let diff_args = concat(&["diff", "--raw", "--no-abbrev", "-z"], git_diff_args);
    let mut diff_child = Command::new("git").args(diff_args).stdout(Stdio::piped()).spawn()?;
    let mut diff_stdout = BufReader::new(diff_child.stdout.take().unwrap());

    let mut cat_child = Command::new("git")
        .args(["cat-file", "--batch"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()?;
    let mut cat_stdin = cat_child.stdin.take().unwrap();
    let mut cat_stdout = BufReader::new(cat_child.stdout.take().unwrap());

    let mut result = ProgramInput::default();

    while !diff_stdout.fill_buf()?.is_empty() {
        let metadata = read_token(&mut diff_stdout)?;
        let metadata = std::str::from_utf8(&metadata)?;
        if !metadata.starts_with(':') {
            return Err(format!("unexpected output from git diff: {metadata}"))?;
        }
        if metadata.starts_with("::") {
            return Err("combined diff formats ('-c' and '--cc') are not supported")?;
        }
        let parts: Vec<_> = metadata[1..].split(' ').collect();
        if parts.len() != 5 {
            return Err(format!("unexpected output from git diff: {metadata}"))?;
        }
        let [old_mode, new_mode, old_oid, new_oid, status] = [parts[0], parts[1], parts[2], parts[3], parts[4]];

        let old_name_raw = read_token(&mut diff_stdout)?;
        let old_name_lossy = String::from_utf8_lossy(&old_name_raw).into_owned();

        let new_name_raw;
        let new_name_lossy;
        if status.starts_with('R') || status.starts_with('C') {
            new_name_raw = read_token(&mut diff_stdout)?;
            new_name_lossy = String::from_utf8_lossy(&new_name_raw).into_owned();
        } else {
            new_name_raw = old_name_raw.clone();
            new_name_lossy = old_name_lossy.clone();
        }

        if status.starts_with('U') {
            eprintln!("'{old_name_lossy}' is unmerged");
            continue;
        }

        let mut read = |mode: &str, oid: &str, name_raw: &[u8]| -> DynResult<String> {
            // TODO: deal with uncommon modes (symlinks, submodules)
            if mode == "040000" {
                return Err("unexpected directory in git diff --raw output")?;
            }

            let content;
            if oid.chars().all(|c| c == '0') {
                let working_tree_root = match working_tree_root {
                    Some(ref working_tree_root) => working_tree_root,
                    None => working_tree_root.insert(get_working_tree_root()?),
                };
                let path = join_path(working_tree_root, name_raw)?;
                content = std::fs::read(&path)?;
            } else {
                cat_stdin.write_all(format!("{oid}\n").as_bytes())?;

                let mut line = String::new();
                cat_stdout.read_line(&mut line)?;
                let Some(line) = line.strip_suffix('\n') else {
                    return Err("unexpected EOF reading from git cat-file --batch")?;
                };

                let parts: Vec<_> = line.split(' ').collect();
                if parts.len() != 3 || parts[0] != oid || parts[1] != "blob" {
                    return Err(format!(
                        "unexpected output from git cat-file: for query '{oid}' got '{line}'"
                    ))?;
                }

                let size: usize = parts[2].parse()?;

                content = read_exact(&mut cat_stdout, size)?;
                assert!(read_exact(&mut cat_stdout, 1)? == vec![b'\n']);
            }

            let content = String::from_utf8_lossy(&content).into_owned();

            Ok(content)
        };
        let old_content = if status == "A" {
            assert!(old_oid.chars().all(|c| c == '0'));
            String::new()
        } else {
            read(old_mode, old_oid, &old_name_raw)?
        };
        let new_content = if status == "D" {
            assert!(new_oid.chars().all(|c| c == '0'));
            String::new()
        } else {
            read(new_mode, new_oid, &new_name_raw)?
        };

        let file_status_text = {
            let similarity = |num: &str| {
                if num.is_empty() {
                    "".to_owned()
                } else {
                    format!("{num}% similarity")
                }
            };
            let ad_mode = |mode: &str| {
                if mode == "100644" {
                    "".to_owned()
                } else {
                    format!("mode {mode}")
                }
            };
            let modes = || {
                if old_mode == new_mode {
                    "".to_owned()
                } else {
                    format!("mode {old_mode} → {new_mode}")
                }
            };

            match (status.get(..1).unwrap_or_default(), status.get(1..).unwrap_or_default()) {
                ("A", "") => vec!["added".to_owned(), ad_mode(new_mode)],
                ("C", s) => vec!["copied".to_owned(), similarity(s), modes()],
                ("D", "") => vec!["deleted".to_owned(), ad_mode(old_mode)],
                ("M", s) => vec![similarity(s), modes()],
                ("R", s) => vec!["renamed".to_owned(), similarity(s), modes()],
                ("T", "") => vec![modes()],
                // "U" is handled earlier
                _ => vec![format!("unexpected status: {status}")],
            }
            .iter()
            .filter(|s| !s.is_empty())
            .map(String::as_str) // ugh
            .collect::<Vec<_>>() // ugh
            .join(", ")
        };

        result.file_input.push([old_content, new_content]);
        result.file_names.push([old_name_lossy, new_name_lossy]);
        result.file_status_text.push(file_status_text);
    }

    let diff_status = diff_child.wait()?;
    // TODO: std::process::ExitStatus::exit_ok() is unstable
    if !diff_status.success() {
        return Err(format!("running git diff failed: {diff_status}"))?;
    }

    std::mem::drop(cat_stdin);
    let cat_status = cat_child.wait()?;
    // TODO: std::process::ExitStatus::exit_ok() is unstable
    if !cat_status.success() {
        Err(format!("running git cat-file failed: {cat_status}"))?
    }

    Ok(result)
}

pub fn run_external_helper_for_git_diff(args: &[String]) -> DynResult<()> {
    // TODO: git-diff runs GIT_EXTERNAL_DIFF with both stdout and stderr pointing
    // to the pager.'s stdin. We don't have a good error reporting channel. >:(

    let Ok(path_counter) = std::env::var("GIT_DIFF_PATH_COUNTER") else {
            return Err("Missing env var GIT_DIFF_PATH_COUNTER. This should be run by git diff, not directly.")?;
        };
    let path_counter: usize = path_counter.parse()?;

    let Ok(path_total) = std::env::var("GIT_DIFF_PATH_TOTAL") else {
            return Err("Missing env var GIT_DIFF_PATH_TOTAL. This should be run by git diff, not directly.")?;
        };
    let path_total: usize = path_total.parse()?;

    let n = args.len();
    if n != 1 && n != 7 && n != 9 {
        return Err(format!(
            "Unexpected number of GIT_EXTERNAL_DIFF arguments received from git: {n}"
        ))?;
    }

    if atty::is(atty::Stream::Stdout) {
        return Err("stdout is a tty (maybe git isn't properly configured to run 'vxdiff --git-pager')")?;
    }

    print!("{path_counter}\0{path_total}\0{n}\0");
    for (i, arg) in args.iter().enumerate() {
        if i == 1 || i == 4 {
            let size = std::fs::metadata(arg)?.len();
            let f = std::fs::File::open(arg)?;
            print!("{size}\0");
            let mut taken = f.take(size);
            let copied = std::io::copy(&mut taken, &mut std::io::stdout())?;
            if copied != size {
                return Err(format!(
                    "file '{arg}' is shorter than expected (got EOF after {copied} bytes out of {size})"
                ))?;
            }
            if taken.into_inner().read(&mut [0])? != 0 {
                return Err(format!(
                    "file '{arg}' is longer than expected (can still read after {size} bytes)"
                ))?;
            }
            print!("\0");
        } else {
            print!("{arg}\0");
        }
    }

    Ok(())
}

pub fn read_as_git_pager() -> DynResult<ProgramInput> {
    let stdin = std::io::stdin();
    let mut stdin = stdin.lock();

    if stdin.fill_buf()?.is_empty() {
        return Ok(ProgramInput::default());
    }

    let mut counter = 1;
    let mut total = 0;
    let mut result = ProgramInput::default();

    loop {
        let got_counter = read_number(&mut stdin)?;
        if counter != got_counter {
            return Err(format!("Expected counter {counter}, but got {got_counter}"))?;
        }

        let got_total = read_number(&mut stdin)?;
        if counter == 1 {
            total = got_total;
        } else if total != got_total {
            return Err(format!("Reported total changed from {total} to {got_total}"))?;
        }

        let n = read_number(&mut stdin)?;
        assert!(n == 1 || n == 7 || n == 9);

        let old_name = String::from_utf8_lossy(&read_token(&mut stdin)?).into_owned();
        if n == 1 {
            eprintln!("'{old_name}' is unmerged");
        } else {
            let old_size = read_number(&mut stdin)?;
            let old_content = String::from_utf8_lossy(&read_exact(&mut stdin, old_size)?).into_owned();
            assert!(read_exact(&mut stdin, 1)? == vec![0]);

            let _old_oid = read_token(&mut stdin)?; // TODO: use it
            let _old_mode = read_token(&mut stdin)?;

            let new_size = read_number(&mut stdin)?;
            let new_content = String::from_utf8_lossy(&read_exact(&mut stdin, new_size)?).into_owned();
            assert!(read_exact(&mut stdin, 1)? == vec![0]);

            let _new_oid = read_token(&mut stdin)?; // TODO: use it
            let _new_mode = read_token(&mut stdin)?;

            let new_name;
            let xfrm_msg;
            if n == 9 {
                new_name = String::from_utf8_lossy(&read_token(&mut stdin)?).into_owned();
                xfrm_msg = String::from_utf8_lossy(&read_token(&mut stdin)?).into_owned();
            } else {
                new_name = old_name.clone();
                xfrm_msg = Default::default();
            }

            let file_status_text = xfrm_msg.split('\n').next().unwrap().to_owned(); // TODO

            result.file_input.push([old_content, new_content]);
            result.file_names.push([old_name, new_name]);
            result.file_status_text.push(file_status_text);
        }

        counter += 1;

        if counter > total {
            break;
        }
    }

    Ok(result)
}

pub fn read_file_list(files: &[String]) -> DynResult<ProgramInput> {
    assert_eq!(files.len() % 2, 0);

    let mut result = ProgramInput::default();

    for chunk in files.chunks_exact(2) {
        let old_name = chunk[0].clone();
        let new_name = chunk[1].clone();
        let old_content = String::from_utf8_lossy(&std::fs::read(&old_name)?).into_owned();
        let new_content = String::from_utf8_lossy(&std::fs::read(&new_name)?).into_owned();
        result.file_input.push([old_content, new_content]);
        result.file_names.push([old_name, new_name]);
        result.file_status_text.push("".to_owned());
    }

    Ok(result)
}
