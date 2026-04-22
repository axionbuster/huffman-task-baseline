# Copilot instructions

You are implementing the task described in `AGENTS.md` / `CLAUDE.md`. Read
that file first. It defines the contract, the allowed dependencies, and the
success criteria.

Work through the task end-to-end:

1. Install the Haskell toolchain in your environment (GHC 9.14.1 preferred,
   9.12.2 fallback, cabal 3.12+). `ghcup` is the standard installer; see
   `https://www.haskell.org/ghcup/` for the one-line command and the
   non-interactive environment variables.
2. Write the `.cabal` file and source modules.
3. Verify locally by running each file in `samples/` through
   `huffman encode` piped into `huffman decode` and comparing to the
   original.
4. Open a pull request when the sanity CI is green.

Keep the change focused. Do not add CI beyond what the sanity workflow
requires. Do not commit built artifacts. Do not touch `samples/` or
`.github/workflows/`.

If you hit a blocker — for instance, GHC 9.14.1 won't install in your
sandbox — document it in the PR description and fall back to GHC 9.12.2
rather than getting stuck.
