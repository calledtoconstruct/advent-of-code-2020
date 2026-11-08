# Guiding principles: calledtoconstruct/advent-of-code-2020

## Purpose and scope
Haskell solutions to the Advent of Code 2020 puzzles, one executable (`AdventOfCode2020`).

## Stack and layout
Haskell2010, Cabal package `AdventOfCode2020.cabal` (cabal-version >=1.10), sources in `src/` with `Main.hs`, puzzle input in `data/`. A `.devcontainer/` (GHC 8.6.5 + Haskell Language Server) and `hie.yaml` support editing.

## Commands
| Check | Command | Evidence |
|---|---|---|
| Setup | `cabal update` then `cabal new-install --lib sort text split Unique extra containers` (inside the dev container) | readme.md |
| Build | Not documented; the Cabal file defines one executable, so a Cabal build of that target is the natural check, but no command is written down | AdventOfCode2020.cabal |
| Type-check | Covered by the GHC build; no separate tool | - |
| Lint / architecture guard | None present | - |
| Tests | None: the Cabal file has no test-suite | AdventOfCode2020.cabal |
| CI | None: no workflow files | repository tree |

Commands marked as not written down come from the build files, not from project docs; confirm them before relying on them.

## Before every push
Before every push: run the build and type-check/lint that exist here, plus only the tests for the files or feature you changed (team rule, ideas #14 note 6797). Do not run the full suite locally as a routine gate; this repository has no CI that runs the full suite, so run the full suite only when the change is broad, and say in the PR that CI coverage is missing. Merge needs a positive review. Explain any new failure your change caused. Never weaken a valid test to make it pass, and never invent a command this file does not list.

## Project rules
- Keep each day's solution self-contained and readable; the repo is a puzzle archive, not a library.
- Do not edit or regenerate files in `data/` unless the change is about that day's input.
- Add new dependencies to `build-depends` in the Cabal file rather than relying on globally installed libraries.

## What to avoid
- Committing to the default branch; work on a branch and open a pull request.
- Deleting files, branches or history, or changing repository settings.
- Adding tooling, CI or dependencies that the change does not need.
- Rules copied from other projects (file-size limits, clocks, quotas) that this repository never adopted.

## Delivery and safety
- Nothing is deployed or released.

## Open questions
- Which build command (`cabal build` vs `cabal new-build`) should be the standard check is not written down.
- There is no automated test or CI; a change can only be checked by building and running the executable.

## Sources
Read from the `master` branch on 2026-11-08 ET: `readme.md`, `AdventOfCode2020.cabal`, `.devcontainer/devcontainer.json`, repository tree.

## Instruction updates
Check `guiding-principles.version` at the start of each turn and when you enter this project; reload this file when the version changes. Change this file, its version and date, and `PRINCIPLES_HISTORY.md` in the same commit. Team-wide role instructions are versioned separately in the team-process repository.
