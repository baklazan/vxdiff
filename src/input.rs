use crate::DynResult;
use std::io::{BufRead as _, Read as _};

#[derive(Default)]
pub struct ProgramInput {
    pub file_input: Vec<[String; 2]>,
    pub file_names: Vec<[String; 2]>,
}

// WTF
fn concat<'a, A: AsRef<str>, B: AsRef<str>>(a: &'a [A], b: &'a [B]) -> impl Iterator<Item = &'a str> {
    a.iter().map(AsRef::as_ref).chain(b.iter().map(AsRef::as_ref))
}

pub fn run_git_diff(current_exe: &str, git_diff_args: &[String], pager_args: &[&str]) -> DynResult<()> {
    let args = concat(&["diff"], git_diff_args);
    let git_external_diff = shell_words::join([current_exe, "--git-external-diff"]);
    let git_pager = shell_words::join(concat(&[current_exe, "--git-pager"], pager_args));
    // TODO: std::process::ExitStatus::exit_ok() is unstable
    let exit_status = std::process::Command::new("git")
        .args(args)
        .env("GIT_EXTERNAL_DIFF", git_external_diff)
        .env("GIT_PAGER", git_pager)
        .spawn()?
        .wait()?;
    if exit_status.success() {
        Ok(())
    } else {
        Err(format!("running git diff failed: {exit_status}"))?
    }
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
    fn read_token(input: &mut impl std::io::BufRead) -> DynResult<Vec<u8>> {
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

    fn read_exact(input: &mut impl std::io::Read, size: usize) -> DynResult<Vec<u8>> {
        let mut buffer = vec![0; size];
        input.read_exact(&mut buffer)?;
        Ok(buffer)
    }

    fn read_number(input: &mut impl std::io::BufRead) -> DynResult<usize> {
        Ok(std::str::from_utf8(&read_token(input)?)?.parse()?)
    }

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
            if n == 9 {
                new_name = String::from_utf8_lossy(&read_token(&mut stdin)?).into_owned();
                let _xfrm_msg = read_token(&mut stdin)?; // TODO: use it
            } else {
                new_name = old_name.clone();
            }

            result.file_input.push([old_content, new_content]);
            result.file_names.push([old_name, new_name]);
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

    for i in (0..files.len()).step_by(2) {
        let old_name = files[i].clone();
        let new_name = files[i + 1].clone();
        let old_content = String::from_utf8_lossy(&std::fs::read(&old_name)?).into_owned();
        let new_content = String::from_utf8_lossy(&std::fs::read(&new_name)?).into_owned();
        result.file_input.push([old_content, new_content]);
        result.file_names.push([old_name, new_name]);
    }

    Ok(result)
}
