# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository

Pascal J. Bourguignon's personal Emacs Lisp library — a long-running collection of `pjb-*.el` modules covering editing utilities, language modes (Common Lisp, Objective-C, AutoCAD Lisp, PL/I, etc.), mail/ERC/Gnus tweaks, finance helpers, and Common Lisp interop. A few `.lisp` files exist alongside; per `README`, modules with no `interactive` forms have sometimes been ported to Common Lisp.

## Common commands

Build (byte-compile all sources listed in `EMACS_SOURCES`):
```
make
```
Compiles via `emacs -batch -q -l .emacs -f batch-byte-compile`. The local `.emacs` sets the `load-path` used during compilation — keep using the Makefile rather than invoking `emacs` directly so that load path is correct.

Run the ERT test suite (batch):
```
make test
```
Test files are listed in `TEST_EL_FILES` in the Makefile (`autocad-test.el`, `pjb-constants-test.el`, `pjb-echo-keys-test.el`, `pjb-utilities-test.el`). Each is loaded with `-Q -L .` and run via `ert-run-tests-batch-and-exit`. To run just one:
```
emacs --batch -Q -L . -l pjb-utilities-test.el -f ert-run-tests-batch-and-exit
```

Clean compiled artifacts:
```
make clean
```

Install to `$(SHARE_LISP)/packages/com/informatimago/emacs`:
```
make install
```

## Supported Emacs version

**Floor: Emacs 27.1.** All code may assume `cl-lib`, `seq`, `map`, `subr-x`, `project.el`, native JSON, `text-property-search`, and warned-on `lexical-binding`. New files MUST start with a `;;; foo.el --- ... -*- lexical-binding: t; -*-` cookie. Drop any version shim guarding `< 27`.

## Architecture notes

- **Adding a new source file**: it must be added to `EMACS_SOURCES` in the `Makefile` to be byte-compiled and installed. New tests must be added to `TEST_EL_FILES`.
- **Naming collisions with modern Emacs**: this library predates many built-ins, so symbols have been renamed defensively (e.g. `flatten-tree` → `pjb-flatten-tree`, `aetoken` accessor `value` → `aetoken-value`). When introducing or touching helpers with generic names, prefer a `pjb-` prefix to avoid clashing with current Emacs.
- **CL-like helpers**: this library intentionally provides a Common-Lisp-flavoured layer on top of Emacs Lisp (pathnames, characters, time, etc.). Those helpers live under the `cl-` prefix (the same namespace `cl-lib` itself uses) — e.g. `cl-pathname-name`, `cl-pathname-directory`, `cl-probe-file`, `cl-file-write-date`. **Before adding any `cl-foo`, check that the symbol is not already defined by `cl-lib` or by Emacs core**; if it is, fall back to a `pjb-` prefix. Do **not** redefine, advise, or shadow built-ins.
- **`.~N~` files** are Emacs backup files, not sources — ignore them. `flycheck_*.elc` files are flycheck scratch artifacts.
- **Common Lisp side**: `compile.lisp`, `deps.lisp`, `slime-rpc.{el,lisp}`, etc. interoperate with a CL image (clisp is invoked by the `summary` target). Most day-to-day work is on the `.el` files.
- **Modernization roadmap**: `MODERNIZATION.md` holds the multi-phase plan to retire `defadvice`, the `cl` package, and core-function shadows. Consult it before any sweeping refactor.
