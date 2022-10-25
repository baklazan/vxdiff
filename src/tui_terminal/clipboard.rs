use std::error::Error;
use std::io::{self, Write as _};

pub(super) enum ClipboardMechanism {
    /// Copy to clipboard using the OSC 52 escape sequence.
    /// Needs support from the terminal.
    /// See also: https://github.com/zyedidia/micro/blob/master/runtime/help/copypaste.md
    Terminal,

    /// Copy to clipboard by running a helper such as xclip, xsel or pbcopy.
    ExternalHelper,

    /// Don't copy to system clipboard.
    None,
}

pub(super) fn copy_to_clipboard(mechanism: &ClipboardMechanism, text: &str) -> Result<(), Box<dyn Error>> {
    match mechanism {
        ClipboardMechanism::Terminal => {
            // https://terminalguide.namepad.de/seq/osc-52/
            io::stdout().write(format!("\x1b]52;c;{}\x1b\\", base64::encode(text)).as_bytes())?;
        }
        ClipboardMechanism::ExternalHelper => {
            let mut found_candidate = false;
            let mut try_candidate = |cmd: &str, args: &[&str]| -> Result<(), Box<dyn Error>> {
                if !found_candidate {
                    match std::process::Command::new(cmd)
                        .args(args)
                        .current_dir("/")
                        .stdin(std::process::Stdio::piped())
                        .spawn()
                    {
                        Ok(mut child) => {
                            found_candidate = true;
                            // TODO: We should use threads or select() or something.
                            child.stdin.take().unwrap().write(text.as_bytes())?;
                            let status = child.wait()?;
                            if !status.success() {
                                return Err(io::Error::new(
                                    std::io::ErrorKind::Other,
                                    &*format!("{cmd} exited with status {status}"),
                                )
                                .into());
                            }
                        }
                        Err(err) => {
                            if err.kind() != io::ErrorKind::NotFound {
                                return Err(err.into());
                            }
                        }
                    }
                }
                Ok(())
            };
            // https://github.com/neovim/neovim/blob/cd96fe06e188bcd6e64f78cb078a307fb45f31f0/runtime/autoload/provider/clipboard.vim
            #[cfg(target_os = "macos")]
            try_candidate("pbcopy", &[])?;
            #[cfg(not(target_os = "macos"))]
            {
                if std::env::var_os("WAYLAND_DISPLAY").is_some() {
                    try_candidate("wl-copy", &["--primary", "--type", "text/plain"])?;
                }
                if std::env::var_os("DISPLAY").is_some() {
                    try_candidate("xclip", &["-i", "-selection", "primary"])?;
                    try_candidate("xsel", &["-i", "-p"])?;
                }
                try_candidate("lemonade", &["copy"])?;
                try_candidate("doitclient", &["wclip"])?;
                try_candidate("termux-clipboard-set", &[])?;
                if std::env::var_os("TMUX").is_some() {
                    try_candidate("tmux", &["load-buffer", "-"])?;
                }
            }
        }
        ClipboardMechanism::None => {}
    }
    Ok(())
}
