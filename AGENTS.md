# Task: Huffman codec in Haskell

Build a command-line Huffman encoder/decoder. Performance matters;
correctness matters more.

## Contract

Your repo must build with `cabal build -O2` and produce an executable
component named `huffman`. The grader will invoke your binary exactly like
this:

```
huffman encode < input.bin > output.enc
huffman decode < output.enc > output.bin
```

Bytes in, bytes out. No flags. No filename arguments. stdin → stdout only.

Success criteria, in order of importance:

1. **Correctness.** For every input `X`, `decode(encode(X)) == X` byte-for-byte.
   This holds for empty input, single-byte input, ASCII, UTF-8, arbitrary
   binary, highly repetitive input, and highly skewed distributions.
2. **Builds cleanly** with `cabal build -O2` on GHC 9.14.1 (or 9.12.2 as a
   fallback). No warnings requested — don't fight `-Wall`, just make it work.
3. **Reasonable compression.** On typical text input, the encoded file should
   be smaller than the raw file. Don't worry about beating `gzip`.
4. **Throughput.** Encode and decode should each process at least 10 MB/s on
   a standard GitHub-hosted runner. This is a low bar — the grader measures
   exactly how fast you go.

## Environment

You are responsible for setting up GHC and cabal in your working environment.

- Target toolchain: **GHC 9.14.1** with a recent cabal (3.12+).
- Fallback if 9.14.1 is unavailable in your environment: **GHC 9.12.2**. Pin
  the version in `cabal.project` so CI can reproduce.
- Recommended installer: `ghcup`
  (`curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`).
  For a non-interactive install set `BOOTSTRAP_HASKELL_NONINTERACTIVE=1` and
  `BOOTSTRAP_HASKELL_GHC_VERSION=9.14.1`.

A `cabal.project` stub is included. You write everything else: the `.cabal`
file, `src/`, any modules you need, and any dependency pins.

## Allowed dependencies

- Anything in GHC's boot libraries (`base`, `bytestring`, `containers`,
  `array`, `deepseq`, ...).
- `vector` if you want SoA / primitive arrays.
- `primitive` if you need `ByteArray#` wrappers.

Please do **not** use `zlib`, `zstd`, `lzma`, any existing Huffman package,
or pull from Hackage archives of prior solutions. The point is that you
implement it.

## Deliverables

- A buildable Haskell project rooted at the repo root.
- At least two source modules under `src/`. One-module solutions fail the
  module-count check.
- Passes the sanity workflow (see below). Private grader tests run
  automatically after the sanity workflow.

## Sanity check before you open the PR

`samples/` contains three small inputs. The CI workflow in
`.github/workflows/sanity.yml` will:

1. Build your project.
2. For each sample, encode then decode, and fail if the roundtrip isn't
   byte-identical.

Run it locally if you can. Open the PR when it passes. The private grader
then runs the full hidden test suite, measures throughput, and posts a
comment.

## Style

Write idiomatic Haskell. The grader applies an automated style check (hlint,
ormolu, and a few pattern-based heuristics) — don't fight it, but don't
sweat individual flags either.

## Notes on scope

- Single-threaded. Don't reach for `async` or `par`.
- No FFI, no Template Haskell, no unsafe primitives unless you genuinely
  need them.
- Don't over-engineer: there's no need for a plugin system, no need for a
  library target, no need for custom Setup.hs.
- The encoded stream format is your choice. Just make sure your decoder
  can read what your encoder writes.

Good luck.
