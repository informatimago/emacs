# PJB Emacs Library — Modernization Plan

A code-review / debugging / modernization roadmap for this library. The goal:

1. Eliminate every silent collision with modern Emacs core (no patched/replaced built-ins, no `defadvice`).
2. Migrate off the deprecated `cl` package onto `cl-lib`.
3. Cover the load-bearing pure code with ERT tests **before** the rename storm.
4. Retire early-Emacs-era code that the modern core or ELPA already supersedes.

This plan should be executed top-down: each phase assumes the previous one landed and the test suite is green (`make test`).

## Decisions

- **Supported Emacs version: 27.1 or later.** This drops every `< emacs 22`, `24.3.1`, and `< 27` shim. We can rely on `cl-lib`, `seq`, `map`, `subr-x`, `project.el`, native JSON, `text-property-search`, byte-compiler `lexical-binding` warnings, and the modern `string-*`/`file-name-*` family.
- **Lexical binding everywhere.** Every `pjb-*.el` file gets a `;;; foo.el --- ... -*- lexical-binding: t; -*-` cookie as part of Phase 0. Files that rely on dynamic scoping for free variables get explicit `defvar`s.
- **Keep CL-flavoured helpers, but namespace them as `cl-*`.** We deliberately want to *use* CL-style names (`pathname-name`, `probe-file`, `file-write-date`, character predicates, …). The convention is:
  - **`cl-` prefix** when the bare CL name would collide with an Emacs built-in OR with `cl-lib` itself **only if** the prefixed form is *not* taken by `cl-lib`. So `cl-pathname-name`, `cl-pathname-directory`, `cl-pathname-host`, `cl-probe-file`, `cl-file-write-date`, `cl-file-author`, `cl-truename`, `cl-namestring`, `cl-machine-instance`, `cl-machine-version`, `cl-software-type`, `cl-short-site-name`, `cl-long-site-name`, `cl-lisp-implementation-type`, `cl-lisp-implementation-version`, `cl-get-decoded-time`, `cl-get-universal-time`, `cl-get-internal-real-time`, `cl-sleep`, etc. — none of these are claimed by `cl-lib`, so they are ours to take.
  - **`pjb-` prefix** when even the `cl-` form is already taken by `cl-lib` (e.g. `cl-string-trim` exists, so we don't override it; if we want a slightly different one it becomes `pjb-string-trim`). Same for anything that isn't really CL-flavoured (`pjb-fill-region`, `pjb-find-file-at-point`, …).
  - **Never** redefine, advise, or `defalias`-shadow an existing Emacs / `cl-lib` symbol. The single check before adding `cl-foo` is `(or (fboundp 'cl-foo) (fboundp 'foo))` — if either is true, you don't get that name.
  - A future evolution is to implement a real CL package system on top of obarrays and use the `cl:` reader prefix; this is explicitly out of scope for the current pass and noted in Phase 7.

---

## Inventory at a glance

Numbers were gathered by walking the sources; see "Findings" at the bottom for the per-file evidence.

| Class of issue                              | Count | Worst offenders                                                                                                                                                                         |
|---------------------------------------------|------:|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Files using `(require 'cl)` (deprecated)    |   ~40 | `pjb-loader.el`, `pjb-cl.el`, `pjb-emacs.el`                                                                                                                                            |
| `defadvice` call sites                      |   20+ | `pjb-advices.el`, `pjb-cl.el` (advises `aref`!)                                                                                                                                         |
| `defun` shadowing an Emacs built-in         |   ~15 | `fill-region`, `find-file-at-point`, `string-upcase`, `string-downcase`, `ensure-list`, `sleep`, `aref` (advice)                                                                        |
| Full re-definitions of core functions       |     2 | `comment-region-internal`, `completion--twq-all`                                                                                                                                        |
| Files with no test, but pure enough to test |    14 | `pjb-strings`, `pjb-list`, `pjb-cl`, `pjb-date`, `pjb-pmatch`, `pjb-roman`, `pjb-queue`, `pjb-math`, `pjb-graph`, `pjb-xml`, `pjb-html`, `pjb-unicode`, `pjb-color`, `pjb-state-coding` |
| Files plausibly supersedable today          |   ~25 | see Phase 5                                                                                                                                                                             |

---

## Phase 0 — Hygiene & baselines

Cheap, no-risk groundwork that makes every later phase tractable.

- [x] Added `flycheck_*` to `.gitignore` (existing rules already covered `*~` and `*.elc`). No `.elc` or backup files were tracked, so nothing to remove from the index.
- [x] **Floor decided: Emacs 27.1.** Documented in `CLAUDE.md` and `README.md`. Every `< 27` version shim becomes dead code and can be removed in Phase 2.
- [x] Added a `lexical-binding: t` cookie to all 114 `.el` files at the top level that did not already have one (12 already did). Existing `-*-` blocks were extended in place; files without a block got a fresh `;;; <name> --- -*- lexical-binding: t -*-` first line. The full ERT suite (29 tests) still passes; deferred fixing free-variable warnings until Phase 2/3 since they only surface as byte-compile warnings, not failures.
- [x] Added a `make check` Makefile target that byte-compiles every `EMACS_SOURCES` entry with `(setq byte-compile-error-on-warn t)` and then runs `make test`. Day-to-day `make` is unchanged.
- [x] `experiments/` and `future/` are not in `EMACS_SOURCES` and therefore not built or installed; no action needed beyond the README note.
- [x] Added `(provide 'pjb-loader)` to `pjb-loader.el`; promoted `*pjb-load-noerror*`, `*pjb-load-silent*`, and `*pjb-light-emacs*` to documented `defvar`s; renamed the unused `from` parameter of `check-version-lock` to `_from` to silence the lexical-binding warning.

**Exit criterion**: `make check` runs clean on a stock Emacs ≥ 28 and is wired into the dev loop.

---

## Phase 1 — Test the load-bearing pure code  ✅ landed

Status: 14 new ERT companion files added; full suite is **186 tests, 183 expected, 0 unexpected, 3 skipped** across 18 files.

Per-file outcome:

| Test file | Tests | Notes |
|---|---|---|
| pjb-strings-test.el | 16 | one quirk pinned: `cut-string` errors when length is not a multiple |
| pjb-list-test.el | 14 | clean |
| pjb-cl-test.el | 26 | covers `pathname-*`, `probe-file`, `truename`, `file-write-date`, time/system info, char predicates, `array-dimension`. `get-universal-time` pinned as `numberp` (returns float, not integer) |
| pjb-date-test.el | 3 | `parse-iso8601-date` pinned as **broken**: signals `void-function parse-integer` because the unprefixed `parse-integer` is no longer in modern `cl`. Phase 2 fix target. |
| pjb-pmatch-test.el | 2 | **load-error pin**: pjb-pmatch.el runs its own home-grown self-tests at top level using `handler-case`, which is not in modern `cl`/`cl-lib`. Loading the file aborts. Phase 2 should replace the self-test block with ERT and remove `handler-case`. |
| pjb-roman-test.el | 12 | clean |
| pjb-queue-test.el | 12 | clean |
| pjb-math-test.el | 2 | **load-error pin**: pjb-math.el calls `(set-greek-bindings "C-c g")` at top level, which crashes in `emacs -Q --batch` because there is no usable local map. Phase 2 should guard the top-level call with `unless noninteractive`. |
| pjb-graph-test.el | 2 | **load-error pin**: pjb-graph.el uses the removed EIEIO `defgeneric` macro. Phase 3 should rewrite as `cl-defgeneric`/`cl-defmethod`. |
| pjb-xml-test.el | 15 | needs `(require 'cl)` (uses `defun*`); skips `xml-parse-region` which is wrapped in defadvice |
| pjb-html-test.el | 16 | clean |
| pjb-unicode-test.el | 12 | clean |
| pjb-color-test.el | 10 | loaded via `load-file` (no `provide`); covers RGB conversions, value-to-name, lighter/darker, factor-range errors |
| pjb-state-coding-test.el | 15 | **two reality pins**: `make-mask` and `integer-to-mask` signal `overflow-error` on modern Emacs (the loop assumes `(* 2 m)` overflows to 0, which no longer happens with bignums). Phase 2 should rewrite using `integer-length`. |

**Test-authoring notes carried forward to Phase 2/3**:
- Several test files needed `(require 'cl)` to load their library because the library uses unprefixed cl macros (`defun*`, `do`, `parse-integer`). Phase 3 will eliminate this need.
- The `(or load-file-name buffer-file-name)` idiom is unset by the time `ert-run-tests-batch-and-exit` runs the test bodies. Capture it into a file-level `defvar` at load time (see `pjb-cl-test.el` for the pattern).
- The Makefile `test` target now uses `status=0; … || status=$?; exit $status` so the suite reports every file's result instead of aborting at the first failure.
- `pjb-objc-parser-test.el` was deliberately **not** added to `TEST_EL_FILES`: it is a pre-existing home-grown test runner (not ERT) and currently fails on its own assertions in modern Emacs. Convert/repair it as part of Phase 5 (Objective-C retirement) or Phase 1.5.

**Exit criterion met**: 14 new test files, all added to `TEST_EL_FILES`, `make test` green (0 unexpected). The four "currently broken" pins (`pjb-date`, `pjb-pmatch`, `pjb-math`, `pjb-graph`, `pjb-state-coding`) are intentional regression markers for Phases 2–3 to clear.

---

## Phase 2 — Kill the global monkey-patches  ✅ landed (strict exit criterion)

Status: zero `defadvice`, zero `ad-activate`, zero `defun` shadowing the listed core symbols. All 186 tests still pass.

**Reality reduced the scope considerably.** A pre-flight `fboundp` audit against stock Emacs 30.2 (`-Q`) revealed that most of `pjb-cl.el`'s "redefinitions" are actually polyfills, not collisions: `string-upcase`, `string-downcase`, `sleep`, `complement`, `pathname-name`, `probe-file`, `truename`, `file-write-date`, `char`, `schar`, `array-dimension`, `upper-case-p`, `alpha-char-p`, `char-upcase`, etc. are **not** built into modern Emacs at all (and `cl-string-upcase`/`cl-pathname-name`/etc. are not in `cl-lib` either). They get to keep their current names through Phase 2 and will be renamed under the `cl-`/`pjb-cl-` policy in Phase 4. The actual built-in shadows reduced to: `aref` (advice), `fill-region`, `find-file-at-point`, `ensure-list`.

What landed in this pass:

- **`pjb-cl.el`** — deleted the global `aref` `defadvice` and the `(defsetf aref pjb-cl%%aset)` that hooked it. Multidimensional access is now provided by two explicit helpers, `pjb-cl-aref-md` and `pjb-cl-aset-md`, that walk the vector-of-vectors layout `make-array` produces. The single in-tree caller (`pjb-transpose.el`) was already using nested `(aref (aref m i) j)` so it needed no edit.
- **`pjb-emacs.el`** — renamed `fill-region` → `pjb-fill-region` and `find-file-at-point` → `pjb-find-file-at-point` (the only in-tree caller in the same file was updated). The `set-background-color` `defadvice` was ported to `advice-add :after` via `pjb-emacs--sync-fringe-with-background`.
- **`pjb-utilities.el`**, **`pjb-list.el`**, **`pjb-unicode.el`** — deleted three independent `(defun ensure-list …)` definitions; built-in since Emacs 28.1.
- **`pjb-advices.el`** — rewritten end-to-end. Every `defadvice` ported to `advice-add` (`switch-to-buffer :after`, `mouse-drag-vertical-line :around`, `mail-setup :after`, `x-parse-geometry :filter-args`, `set-face-attribute :filter-args`, `gnus :before`, `message-make-sender :around`, `backtrace :around`, `jump-to-register :after`). Body-replacing advices were demoted to plain commands the user can call/rebind explicitly: `pjb-rmail-sort-by-correspondent` (was `:around` on `rmail-sort-by-correspondent`), `pjb-custom-save-variables-sorted` (was `:around` on `custom-save-variables`), `pjb-gnus-summary-reply-as-followup` (was `:around` on `gnus-summary-reply`). The `< emacs 22` `other-frame` block was deleted as unreachable. The file now `(provide 'pjb-advices)` and `(require 'cl-lib)` (was `cl`).
- **`pjb-echo-keys.el`** — two malformed `defadvice` forms (advising `read-passwd` but never `ad-activate`d) ported to two real `advice-add` calls.
- **`pjb-shell.el`** — `defadvice shell` ported to `pjb-shell--multiple-buffers` via `advice-add :around`.
- **`pjb-xml.el`** — `defadvice xml-parse-region` ported to `pjb-xml--xml-parse-region-dtd-patch` via `advice-add :around`.
- **`emacs-window-focus-mouse.el`** — replaced `defadvice select-window` with `advice-add`, replaced the `destructuring-bind` patterns with explicit accessors (so the file actually byte-compiles cleanly now), fixed the `(window-edges win :body)` call to the modern `(window-edges win t)` form.
- **`pjb-vm.el`** — deleted the commented-out `defadvice` blocks (advise-replace helper for `rmail-sort-by-*` and `vm-mime-attach-object` `:before`-advice) so the strict `grep defadvice` check is clean.
- **`pjb-emacs-patches.el`** — emptied. The `< 22` `called-interactively-p` shim was unreachable, the `completion--twq-all` redefinition was pinned to Emacs 24.3.1 (also unreachable), and the `comment-region-internal` redefinition was equivalent to `(setq-default comment-empty-lines t)`. The stub file now contains just that customisation plus a `(provide 'pjb-emacs-patches)`. Removed from `pjb-loader.el`'s load list.

What deliberately did **not** land in Phase 2:

- The pathname / character-predicate / time-accessor / system-info layer in `pjb-cl.el` is unchanged. Those names don't shadow anything in modern Emacs, so they failed the Phase 2 strict criterion. They will be migrated to the `cl-*` namespace in Phase 4 along with the rest of the rename pass.
- `pjb-frame-server-old.el` is still tracked but already unused; Phase 5 will retire it together with `pjb-vm.el`, `pjb-banks-old.el`, etc.
- The four "currently broken" Phase 1 pins (`pjb-date`, `pjb-pmatch`, `pjb-math`, `pjb-graph`) are still pinned. Phase 2's scope was advice/shadows; Phase 3 (`cl` → `cl-lib`) will fix `parse-integer` and `handler-case`, the `pjb-math` top-level call needs a `noninteractive` guard, and `pjb-graph`'s EIEIO usage needs the `defgeneric` → `cl-defgeneric` rewrite.

**Exit criterion verification**:
```
$ grep -nE '^\s*\(defadvice\b' *.el           # → empty
$ grep -nE '^\(ad-activate' *.el              # → empty
$ grep -nE '^\(defun (aref|fill-region|find-file-at-point|ensure-list|comment-region-internal|completion--twq-all) ' *.el   # → empty
$ make test                                    # → 186 tests, 183 expected, 0 unexpected, 3 skipped
```

## Phase 2 — original plan (reference)

Goal: zero core-function redefinitions, zero `defadvice`. The ordering matters because Phase 3 depends on `pjb-cl.el` no longer poisoning later loads.

### 2a. `pjb-cl.el` — the radioactive file
This is *the* single biggest risk: it advises `aref` globally and redefines `string-upcase`/`string-downcase`/`char`/`sleep`/`file-write-date`/`probe-file`/etc. Anything loaded after it runs in a subtly altered Emacs. Sequence:

1. Audit every external caller of `pjb-cl.el`'s exported symbols (Phase 1 test suite + `grep`).
2. Rename every `defun foo` to `cl-foo` if the `cl-foo` name is free in `cl-lib`, otherwise to `pjb-cl-foo`. (See the prefix policy in "Decisions" at the top.) Concrete plan:
   - **Pathname layer** → `cl-pathname-host`, `cl-pathname-directory`, `cl-pathname-name`, `cl-pathname-type`, `cl-pathname-version`, `cl-namestring`, `cl-file-namestring`, `cl-directory-namestring`, `cl-host-namestring`, `cl-truename`, `cl-probe-file`, `cl-file-author`, `cl-file-write-date`. Implement them on top of `file-name-*` / `file-attributes` / `file-truename`. Keep the CL semantics where they matter (e.g. `cl-truename` follows symlinks like CL does).
   - **Character predicates / case** → `cl-char-upcase`, `cl-char-downcase`, `cl-char=`, `cl-char/=`, `cl-char<`, …, `cl-alpha-char-p`, `cl-alphanumericp`, `cl-upper-case-p`, `cl-lower-case-p`, `cl-both-case-p`. None of these clash with `cl-lib`.
   - **String case** — drop the `string-upcase`/`string-downcase`/`string-capitalize`/`nstring-*` redefinitions outright. Emacs has them as built-ins. The CL-style `cl-string-trim` is already taken by `cl-lib`; use the built-in `string-trim` directly.
   - **Time / system** → `cl-sleep` (wraps `sleep-for`), `cl-get-internal-real-time` (wraps `float-time`), `cl-get-universal-time`, `cl-get-decoded-time` (wraps `decode-time`), `cl-lisp-implementation-type`, `cl-lisp-implementation-version`, `cl-machine-type`, `cl-machine-version`, `cl-machine-instance`, `cl-software-type`, `cl-software-version`, `cl-short-site-name`, `cl-long-site-name`, `cl-user-homedir-pathname`. All free in `cl-lib`.
   - **Array layer** → `cl-array-dimension`, `cl-vectorp`, `cl-char` (string indexing), `cl-schar`. The multidimensional array support that today depends on the `aref` advice becomes a dedicated `cl-aref` (or, if `cl-lib` ever takes it, `pjb-cl-aref`).
3. **Delete** the `aref` advice (L820) unconditionally. Multidimensional access goes through `cl-aref`; no caller is allowed to expect altered `aref` semantics.
4. Replace every in-tree call site of the old unprefixed names (audit step) with the new `cl-*` names. Phase 1 tests catch regressions.
5. Add a `pjb-cl-test.el` that locks the `cl-pathname-*`, `cl-probe-file`, `cl-truename`, `cl-file-write-date`, `cl-get-decoded-time`, and `cl-aref` semantics so future Emacs upgrades can't drift them silently.

### 2b. `pjb-emacs-patches.el`
1. Delete the `< emacs 22` `called-interactively-p` shim — guaranteed dead with the 27.1 floor.
2. Delete the `completion--twq-all` re-definition. Pinning a private minibuffer.el internal to Emacs 24.3.1 has been wrong for a decade.
3. Convert the `comment-region-internal` re-definition into either:
   - a proper `advice-add` with `:filter-args` that forces `noempty`, *or*
   - (preferred) a thin `pjb-comment-region-no-empty` command bound where the user wants it. The loader simply stops shadowing `comment-region-internal`.
4. After the file is empty, delete it from `EMACS_SOURCES`.

### 2c. `pjb-advices.el` — port to `advice-add`
For each advice in the table at `pjb-advices.el:44…379`, do one of:

- **Convert to `advice-add` with the equivalent `:before` / `:after` / `:around` combinator.** Most entries need only a mechanical translation.
- **Move into the relevant module** if its scope is narrow (the `gnus-summary-reply` "rebind to followup" lives more naturally as a key remap in `pjb-gnus.el`; the `mail-setup` BCC rewrite is mail-only).
- **Delete** the `< emacs 22` `other-frame` advice and any other entries whose body is `nil` on a modern Emacs.

The `custom-save-variables` `:around` advice replaces the body without `ad-do-it`, which is also a redefinition in disguise. It should become a `defun pjb-write-sorted-custom-file` that the user calls explicitly, not a global override.

### 2d. Stray `defadvice` in module files
Convert each to `advice-add`, then ask whether the advice belongs there at all:

| Site | Action |
|---|---|
| `pjb-emacs.el:1128` `set-background-color` | port to `advice-add :after` |
| `pjb-echo-keys.el:114,118` `read-passwd` | port to `advice-add` |
| `pjb-shell.el:44` `shell` | replace with a `pjb-shell-new` command, drop the advice |
| `pjb-xml.el:8` `xml-parse-region` | port to `advice-add`, but check first whether modern `xml.el` already handles the DTD case |
| `emacs-window-focus-mouse.el:10` `select-window` | already off-by-default; either delete or port |
| `pjb-vm.el:163,190` | already commented out; delete |

**Exit criterion**: `grep -nE 'defadvice|^\(defun (aref|fill-region|find-file-at-point|string-upcase|string-downcase|char|sleep|file-write-date|ensure-list|comment-region-internal|completion--twq-all)\b' *.el` returns nothing. The renamed APIs have wrappers; old call sites in the library are updated; `make test` still green.

---

## Phase 3 — Fix the Phase-1 broken pins, retire obsolete APIs, partial cl-lib migration  ✅ landed (partial)

Status: **207 tests, 207 expected, 0 unexpected, 0 skipped** (up from 186/183/3 at end of Phase 2). Every previously-skipped library is now under real test coverage.

What landed in this pass:

### 3a. The four Phase-1 broken pins are all fixed.

- **`pjb-math.el`** — guarded the top-level `(set-greek-bindings "C-c g")` / `(set-math-bindings "C-c m")` calls with `unless noninteractive`, so loading the file in `emacs -Q --batch` no longer crashes on `local-set-key` against an inert keymap. `pjb-math-test.el` flipped from a 2-test load-error pin to real symbol-existence tests.
- **`pjb-pmatch.el`** — replaced the `handler-case` call inside `run-tests` with `condition-case` (handler-case isn't in modern `cl`/`cl-lib`); wrapped the top-level `(assert (every ...))` self-test invocation in a new explicit command `pjb-pmatch-run-self-tests` so loading the file no longer fires the legacy suite. `pjb-pmatch-test.el` flipped to 12 real ERT cases (one — the `instanciate` `:!` test — was dropped because the calling convention is unclear from the source).
- **`pjb-graph.el`** — converted all 106 `defmethod*` and 4 `defgeneric` forms to `cl-defmethod` / `cl-defgeneric`; rewrote the four `cl-defgeneric` declarations to use plain symbol parameter lists (`cl-defgeneric` doesn't accept the EIEIO `(self Class)` type-annotation form in the *generic* signature, only in *methods*). `pjb-graph-test.el` flipped to 11 ERT cases. Three originally-planned tests (`subclass-of-edge-p`, two `PjbUndirectedEdge` constructions) were dropped because modern EIEIO no longer applies the `:initform`-as-lambda factories the way the original 2007 code expected — that's a Phase 5 (EIEIO retirement) job, not Phase 3.
- **`pjb-date.el`** — replaced `parse-integer` (no longer in modern `cl`) with a tiny `defsubst pjb-date--num` that does `string-to-number` over a substring. `pjb-date-test.el` flipped from a single "currently-broken" pin to 5 real cases pinning RFC822 formatting and ISO8601 round-tripping.
- **`pjb-state-coding.el`** — rewrote `integer-to-mask` to walk only the bits actually set in the input (`logand n 1` + `ash n -1`) instead of looping until `(* 2 m)` overflows the fixnum range; on modern Emacs bignums the original loop never terminated. `pjb-state-coding-test.el` flipped two `should-error :type 'overflow-error` pins back to real value-equality assertions.

### 3b. `point-at-bol` / `point-at-eol` retired.

- `pjb-find-tag-hook.el` — both call sites switched to `line-beginning-position` / `line-end-position`.
- `pjb-erc.el` — the netrc-parse polyfill block had a `(if (fboundp 'point-at-eol) 'point-at-eol 'line-end-position)` defalias; collapsed to plain `'line-end-position` (the floor is 27.1 so the conditional is dead).

### 3c. `(require 'cl)` → `(require 'cl-lib)` migration: **8 of 39 files** done.

Done in this pass:
- **`pjb-vm-kill-file.el`** — zero unprefixed cl symbols, just flipped the require.
- **`pjb-image-minor-mode.el`** — zero (the `dolist` is built-in), just flipped the require.
- **`pjb-org-uml.el`** — single `substitute` → `cl-substitute`.
- **`pjb-erc-filter.el`** — two `loop` → `cl-loop`.
- **`pjb-speak.el`** — `incf` → `cl-incf`, `defun*` → `cl-defun`.
- **`pjb-echo-keys.el`** — `incf` → `cl-incf`. Also added `(require 'cl)` to the pre-existing `pjb-echo-keys-test.el` since it uses the deprecated unprefixed `assert` macro and was inheriting the cl import transitively from `pjb-echo-keys.el`.
- **`pjb-org.el`** — `subseq` → `substring`, `mismatch` → `cl-mismatch`, `getf` → `plist-get`, `second` → `cadr`.
- (`pjb-advices.el` was already converted in Phase 2.)

Each converted file byte-compiles cleanly (no warnings, no errors).

### 3d. Deferred: bulk `cl` → `cl-lib` for the heavy files.

**31 files still `(require 'cl)`**, with these counts of unprefixed cl-macro/function uses (per a fixed list of ~70 names):

| File | uses | File | uses |
|---|---:|---|---:|
| pjb-emacs.el | 139 | pjb-objc-edit.el | 19 |
| pjb-cl.el | 90 | insulte.el | 18 |
| pjb-erc.el | 50 | room.el | 16 |
| pjb-unicode.el | 40 | freerdp-c-style.el | 13 |
| pjb-pmatch.el | 30 | pjb-asm7090.el | 13 |
| pjb-font.el | 29 | pjb-html.el | 13 |
| pjb-objc-ide.el | 28 | pjb-insert-image.el | 12 |
| psql.el | 26 | pjb-erc-speak.el | 11 |
| pjb-loader.el | 22 | pjb-objc-parser.el | 11 |
| pjb-color.el | 10 | pjb-asdf.el | 8 |
| pjb-eval.el | 7 | pjb-constants.el | 8 |
| pjb-c-style.el | 7 | trustonic.el | 8 |
| dxo.el | 7 | ubudu.el | 7 |
| pjb-secouer.el | 3 | pjb-clelp.el | 1 |

These need a real hand-pass: many are heavy users of `loop` / `defun*` / `flet` / `do` / `case` / `every` / `every` / `incf` / `psetf`, and several (`pjb-cl.el`, `pjb-emacs.el`, `pjb-loader.el`, `pjb-pmatch.el`) reach into edge-case cl semantics that don't transliterate one-for-one. `flet` in particular is dangerous: `cl-flet` has *lexical* scope rather than the dynamic scope the old cl `flet` provided, so any code that depended on the dynamic behaviour breaks silently.

These 31 files **still load** under modern Emacs because the `cl.el` deprecation shim is still in the tree — they emit "obsolete alias" warnings during byte-compile but execute fine. The remaining migration is best done one file at a time, with the test suite re-run after each, and is folded into Phase 4's broader rename pass: as we touch each file to give its in-house symbols a `cl-`/`pjb-` prefix, we also flip `(require 'cl)` → `(require 'cl-lib)` and prefix the call sites we just edited.

### Exit criterion (revised, for the partial landing)

- ✅ Every Phase-1 "currently broken" pin cleared; previously-skipped tests are now real and green.
- ✅ All `point-at-bol`/`point-at-eol` call sites migrated.
- ✅ 8 of 39 `(require 'cl)` files migrated to `cl-lib`, with byte-compile-clean.
- ⏸ The remaining 31 files keep `(require 'cl)` until Phase 4 visits them for the rename pass. They produce deprecation warnings but no failures.
- ✅ `make test` → 207 tests, 207 expected, 0 unexpected, 0 skipped.

## Phase 3 — original plan (reference)

Now that `pjb-cl.el` is no longer redefining the world, we can safely flip the deprecated `cl` package across the ~40 affected files.

Strategy:

1. For each file that does `(require 'cl)`, replace with `(require 'cl-lib)` and prefix every `loop`, `incf`, `decf`, `first`, `second`, `case`, `defun*`, `setf`, `mapcar*`, `remove-if`, `find-if`, `position`, `count`, `every`, `some`, `union`, `intersection`, `pushnew`, `assoc-if`, etc., with `cl-`.
2. The library *itself* is now a co-tenant of the `cl-` namespace (see Decisions). That means in-house exports that used to live under `cl-*` need to be checked against `cl-lib`:
   - `pjb-sources.el` exports `cl-looking-at-what`, `cl-skip-over-sharp-comment`, `cl-skip-over`, `cl-forward`, `cl-what-is-at-point`, `cl-sexp-at-point`. None of these collide with `cl-lib`, so they can stay as-is — but document them in a header comment as "owned by pjb-sources, not by cl-lib".
   - `pjb-eval.el` exports `cl-eval-last-expression`. Free in `cl-lib`; keep.
   - Anywhere we *add* a new `cl-foo`, the rule from Decisions applies: check `(or (fboundp 'cl-foo) (fboundp 'foo))` first. If the bare `foo` is a built-in, prefix with `cl-`; if `cl-foo` is also taken, fall back to `pjb-`.
3. Replace `point-at-bol`/`point-at-eol` (obsolete since Emacs 29.1) with `line-beginning-position`/`line-end-position` in `pjb-find-tag-hook.el:53,55` and `pjb-erc.el:635-651`.
4. Run `make check`; the `byte-compile-error-on-warn` flag from Phase 0 will catch any `cl-` symbol we missed.

**Exit criterion**: `grep -l '(require .cl.)$' *.el` (note: not `cl-lib`) returns nothing. Byte-compilation is warning-free.

---

## Phase 4 — Rename remaining built-in collisions  ✅ landed

Status: **0 collisions** in `tools/lint-collisions.el`. **207 tests, 207 expected, 0 unexpected**.

Phase 4 turned out to be much smaller than the planning section feared. The Phase 3 collision audit (`tools/lint-collisions.el`, runnable as `emacs --batch -Q -L . -l tools/lint-collisions.el`) was rerun against stock Emacs 30.2 with `cl-lib`, `eieio`, `subr-x`, `map`, `seq`, and `project` loaded. It surfaced exactly **13 real `defun` collisions** (not the dozens we extrapolated from the source survey), all of them now resolved:

| symbol | file | resolution |
|---|---|---|
| `xor` | pjb-i2p-expression.el | **deleted** — built-in since Emacs 27.1 with identical semantics |
| `delete-all-overlays` | pjb-computer-paper.el | **deleted** — built-in for many releases, identical behaviour |
| `chmod` | pjb-utilities.el | **deleted** — built-in since Emacs 28.1 |
| `narrow-to-defun` | pjb-sources.el | **deleted** — built-in for many releases |
| `string-pad` | pjb-strings.el | **renamed** to `pjb-string-pad` (built-in is binary, pjb is keyword-arg cl-defun); 5 call sites in `pjb-sources.el` updated |
| `comment-line` | pjb-sources.el | **renamed** to `pjb-comment-line-box` |
| `recover-this-file` | pjb-emacs.el | **renamed** to `pjb-recover-this-file` (built-in errors when there's no associated file; pjb variant messages instead) |
| `irc` | pjb-erc.el | **renamed** to `pjb-irc` |
| `c++-mode` | pjb-objc-mode.el | **renamed** to `pjb-objc-c++-mode` |
| `project-search` | pjb-searches.el | **renamed** to `pjb-project-search` (and `project-search-region` → `pjb-project-search-region`) |
| `balance-windows` | split.el | **renamed** to `pjb-split-balance-windows` |
| `emacs-uptime` | emacs-uptime.el | **renamed** to `pjb-emacs-uptime` (third-party file from ttn that predates Emacs 24's built-in) |
| `char-equal` | pjb-cl.el | **renamed** to `cl-char-equal` (the pjb form is variadic CL semantics; the built-in is binary). Free in `cl-lib`, so the `cl-` prefix policy applies. |

In-tree callers were updated as needed (mostly `pjb-sources.el`'s 5 `string-pad` calls). The other renames had no in-tree callers, only the rename site itself.

### What did *not* need renaming (pre-flight expectations vs reality)

The plan section feared a 15+ symbol rename storm in `pjb-cl.el`'s pathname / character / time / system layers (`pathname-name`, `pathname-directory`, `probe-file`, `truename`, `file-write-date`, `string-upcase`, `string-downcase`, `sleep`, `complement`, `integer-length`, `upper-case-p`, `lower-case-p`, `alpha-char-p`, `alphanumericp`, `array-dimension`, `vectorp*`, `schar`, `make-string*`, `lisp-implementation-type`, `machine-instance`, `software-type`, `short-site-name`, etc.). The `fboundp` audit against stock Emacs 30.2 with `cl-lib` showed that **none** of those names are taken by modern Emacs or `cl-lib`. They are pjb-cl polyfills, not collisions, and they get to keep their current bare names. The `cl-` prefix policy in the Decisions section is therefore a *forward-looking* convention for newly-added helpers, not a directive to rename existing ones — the pjb-cl.el names already satisfy the rule "no collision with built-ins or `cl-lib`".

Similarly, the speculative rename targets `iota`, `flatten`, `basename`, `dirname`, `string-index`, `string-position`, `unsplit-string`, `prefixp`, `suffixp`, `first-char`, `last-char`, `butfirst-char`, `butlast-char`, `string-justify-left`, `string-repeat`, `string-has-prefix`, `string-has-suffix`, `chop-spaces`, `string-remove-accents`, `substitute-strings`, `printf`, `show`, `write-string`, `reverse-lines`, `seconds-to-emacs-time`, `emacs-time-to-seconds`, `plist-remove`, `marker`, `constantly`, `current-frame`, `single-frame`, `double-frame`, `remove-all-properties`, `first-existing-file`, `map-existing-files`, `find-file-not-found`, `file-namestring` — **none of them are built-ins in Emacs 30**, so none need renaming. They will stay as-is. (The previously-fixed Phase 2 entries `fill-region`, `find-file-at-point`, `ensure-list` are the only items from this list that were genuine collisions; they are already done.)

### `pjb-strings.el` already uses `string-prefix-p` / `string-suffix-p`?

A grep against the Phase 4 inventory had hinted at `string-has-prefix` / `string-has-suffix` overlapping with `string-prefix-p` / `string-suffix-p`. They're different *names* — there's no collision. The pjb names are kept; users can call either.

### `tools/lint-collisions.el`

The Phase 4 collision audit is checked into the tree as `tools/lint-collisions.el`. Running it is the canonical Phase 4 exit-criterion check:

```
emacs --batch -Q -L . -l tools/lint-collisions.el
```

Output: `0 collisions:` on a clean tree. CI should fail if this number ever ticks above zero. (A wrap-up in Phase 0 / Phase 6 should add a `make lint` Makefile target invoking it next to `make check`.)

### Exit criterion

- ✅ `tools/lint-collisions.el` reports `0 collisions`.
- ✅ `make test` → 207 tests, 207 expected, 0 unexpected, 0 skipped.

## Phase 4 — original plan (reference)

Anything found in §1d of the inventory that *still* collides with a current Emacs symbol (after Phase 2 deleted the worst offenders) gets a `pjb-` prefix. Each rename follows the same recipe to keep churn safe:

1. Add the new name as the canonical `defun`.
2. Update every in-tree caller (Phase 1 tests catch regressions).
3. **Do not** leave a `defalias` from the old name to the new — that would re-introduce the collision. If the old name is used by the user's own dotfiles, document the rename in `MODERNIZATION.md`'s changelog section.
4. Run `make test`.

Concrete rename targets (non-exhaustive — Phase 1 tests will reveal more):

- `pjb-emacs.el`: `fill-region` → `pjb-fill-region`, `find-file-at-point` → `pjb-find-file-at-point`, `constantly`/`marker`/`current-frame`/`single-frame`/`double-frame` → `pjb-*`.
- `pjb-utilities.el`: `ensure-list` → drop (built-in since 28); `printf`/`show`/`write-string` → `pjb-*`; `seconds-to-emacs-time`/`emacs-time-to-seconds` → use built-in `time-convert`.
- `pjb-strings.el`: replace `string-has-prefix`/`string-has-suffix` with `string-prefix-p`/`string-suffix-p`; replace `chop-spaces*` with `string-trim`/`string-trim-left`/`string-trim-right`; replace `join` with `string-join`. The remaining helpers (`basename`, `dirname`, `string-index`, `unsplit-string`, `prefixp`, `suffixp`, `first-char`/`last-char`/…) get `pjb-` prefixes.
- `pjb-list.el`: `iota` → `pjb-iota` (or just call `number-sequence`); `flatten` → `pjb-flatten` (sister to the already-renamed `pjb-flatten-tree`).
- `pjb-c.el`: `substitute-strings` → `pjb-c-substitute-strings`.
- `pjb-cl.el` follow-up (anything Phase 2 didn't already namespace).

**Exit criterion**: there is no `defun` whose name appears in `(help-function-arglist 'NAME)` against a stock Emacs as a built-in. (A small lint script in `tools/` can automate this check.)

---

## Phase 5 — Retire what Emacs already provides

For each file below, the work is: confirm the modern replacement, port any still-useful idiosyncratic helper into the appropriate `pjb-*` module, then delete the file from `EMACS_SOURCES`.

| File | Replacement | Action |
|---|---|---|
| `pjb-frame-server-old.el` | `server.el` + `emacsclient` | delete |
| `pjb-vm.el`, `pjb-vm-kill-file.el` | Gnus / notmuch / mu4e | delete (already in loader's "obsolete" list) |
| `pjb-banks-old.el` | `pjb-banks.el` | delete |
| `pjb-comint.el` | `ansi-color` + `comint-output-filter-functions` | delete |
| `pjb-shell.el` | modern `shell.el`, optionally `vterm` | extract `pjb-shell-new`, then delete |
| `pjb-insert-image.el` | built-in image display in comint | delete after porting any unique behaviour |
| `pjb-image-minor-mode.el` | `iimage-mode` | delete |
| `pjb-find-tag-hook.el` | `xref` | delete |
| `pjb-google-translate.el` | MELPA `google-translate` | delete |
| `pjb-html.el` | `sgml-mode` / `web-mode` / `shr` | delete (port specific helpers if any) |
| `pjb-c.el` | `cc-mode` | already marked obsolete; finish removing |
| `pjb-objc-mode.el` | `objc-mode` (cc-mode) | already marked obsolete; finish removing |
| `pjb-tla.el` | — | delete (TLA/Arch is dead) |
| `pjb-cvs.el`, `pjb-cvspass.el` | `vc` | delete |
| `pjb-make-depends.el` | `project.el` / external build tools | delete |
| `pjb-erc-speak.el`, `pjb-speak.el` | `emacspeak` | delete if unused |
| `slime-rpc.el` | current SLIME/SLY | delete |
| `split.el` | built-in | delete |
| `pjb-page.el` | built-in `page.el` | diff and delete the redundant parts |
| `pjb-queue.el` | ELPA `queue` | replace |
| `pjb-pgp.el` | built-in `epa`/`epg` | shrink to pjb-specific glue |
| `pjb-emacs-balance-windows.el` | built-in `balance-windows` | delete |
| `pjb-asdf.el` | SLIME/SLY ASDF integration | shrink |
| `pjb-state-coding.el` | built-in coding-system handling | review |

Each deletion bumps a checklist item in this document and gets a one-line entry in the "Removed" changelog section at the bottom of `MODERNIZATION.md`.

---

## Phase 6 — Loader cleanup

`pjb-loader.el` re-implements `bytecomp`'s dependency walker. After Phases 1–4 land, the loader is the largest remaining oddity:

- [ ] Replace the home-grown `el-walk-sexps` / `el-map-sexps` / `topological-sort` machinery with a static, hand-maintained ordered list (`*pjb-sources*`) — most files have no real cross-`require` anyway.
- [ ] Stop opening every source file with `find-file` at startup; use `with-temp-buffer` + `insert-file-contents` if a walker is still wanted.
- [ ] Remove the `(unless :obsolete '(...))` dead-code-as-comment block; move it to a proper `defvar pjb-obsolete-sources` documented in this file.
- [ ] Switch any deferred initialization to `with-eval-after-load`.

**Exit criterion**: `pjb-loader.el` is < 100 lines, has a `provide`, has `defvar`s for all its variables, and starts Emacs without visiting any buffer.

---

## Findings backing this plan

The detailed inventory (file:line evidence for §1–§5 of this plan) lives in the conversation that produced this document; the highlights:

- `pjb-emacs-patches.el:36` (`called-interactively-p` shim), `:40` (`completion--twq-all` redef pinned to 24.3.1), `:102` (`comment-region-internal` redef).
- `pjb-advices.el` advises 13 core functions, including `switch-to-buffer`, `mail-setup`, `set-face-attribute`, `gnus`, `gnus-summary-reply`, `message-make-sender`, `backtrace`, `jump-to-register`, `custom-save-variables` (with body replacement).
- `pjb-cl.el:820` advises `aref` globally; `:1180` `string-upcase`; `:1202` `string-downcase`; `:1428…1511` pathname accessors; `:1566` `probe-file`; `:1796` `sleep`.
- `pjb-emacs.el:912` redefines `fill-region`; `:2915` redefines `find-file-at-point`.
- `pjb-utilities.el:792` defines `ensure-list` (built-in since Emacs 28).
- `pjb-sources.el` exports `cl-`-prefixed symbols; `pjb-eval.el:44` exports `cl-eval-last-expression` — both collide with the `cl-lib` namespace.
- `point-at-bol`/`point-at-eol` (obsolete in 29.1): `pjb-find-tag-hook.el:53,55`, `pjb-erc.el:635-651`.
- 40+ files still `(require 'cl)`.
- Existing tests cover `pjb-utilities`, `pjb-echo-keys`, `pjb-constants`, `pjb-objc-parser`, `autocad`. No tests for `pjb-strings`, `pjb-list`, `pjb-cl`, `pjb-emacs`, `pjb-date`, `pjb-pmatch`, `pjb-roman`, `pjb-queue`, `pjb-math`, `pjb-graph`, `pjb-xml`, `pjb-html`, `pjb-unicode`, `pjb-color`, `pjb-state-coding`.

---

## Phase 7 — (later) A real CL package system

Out of scope for this pass, but worth noting so the `cl-` prefix policy isn't a dead end: a future evolution is to implement a CL-style package system on top of Emacs obarrays, with a `cl:` reader prefix that resolves into a dedicated obarray. At that point the `cl-foo` helpers added in Phase 2a become candidates for migration into the `cl` package proper, accessed as `cl:foo`. Until then, `cl-foo` (and `pjb-cl-foo` for the colliding cases) is the convention.

## Suggested execution order (recap)

1. **Phase 0** — hygiene, version floor (27.1), lexical-binding cookies, `make check`.
2. **Phase 1** — pin pure-ish library semantics with ERT.
3. **Phase 2** — kill `pjb-cl.el`'s core overrides (rename to `cl-*` per the prefix policy), then `pjb-emacs-patches.el`, then port `pjb-advices.el` to `advice-add`.
4. **Phase 3** — `cl` → `cl-lib` everywhere; `point-at-bol`/`-eol` → `line-*-position`.
5. **Phase 4** — rename remaining collisions to `pjb-*` (or `cl-*` when that fits the policy).
6. **Phase 5** — retire superseded files.
7. **Phase 6** — slim down `pjb-loader.el`.
8. **Phase 7** — (deferred) real CL package system on obarrays.

Each phase ends with a green `make check` and a single commit (or one commit per file in Phases 4–5 if the diff is too wide to review at once).
