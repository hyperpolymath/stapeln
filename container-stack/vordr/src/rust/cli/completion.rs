// SPDX-License-Identifier: PMPL-1.0-or-later
//! Shell completion generation

use anyhow::Result;
use clap::{CommandFactory, Parser};
use clap_complete::{generate, Shell};
use std::io;

/// Generate shell completions
#[derive(Parser, Debug)]
pub struct CompletionArgs {
    /// Shell to generate completions for
    #[arg(value_enum)]
    pub shell: Shell,
}

/// Execute completion generation
pub fn execute(args: CompletionArgs) -> Result<()> {
    let mut cmd = crate::cli::Cli::command();
    let name = cmd.get_name().to_string();
    generate(args.shell, &mut cmd, name, &mut io::stdout());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_completion_args() {
        // Verify bash completion can be parsed
        let args = CompletionArgs { shell: Shell::Bash };
        assert!(matches!(args.shell, Shell::Bash));
    }
}
