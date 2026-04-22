# Samples

Tiny public inputs. The sanity workflow runs `encode | decode` over each and
requires the result to byte-match. These are not the grader's tests — those
are hidden — but if these don't pass, grading won't succeed either.

- `empty.bin` — zero bytes.
- `hello.bin` — "hello, world\n".
- `small.bin` — short ASCII paragraph.

Use these while iterating locally. The grader runs much larger hidden inputs.
