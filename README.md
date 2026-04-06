# PJB Emacs Library

Pascal J. Bourguignon's personal Emacs Lisp library — a long-running collection of `pjb-*.el` modules and a handful of standalone files. Some modules with no `interactive` forms have been ported to Common Lisp (e.g. `pjb-list`, `pjb-strings`, `pjb-graph`).

See `CLAUDE.md` for build/test instructions and `MODERNIZATION.md` for the ongoing cleanup roadmap.

## Supported Emacs version

**Emacs 27.1 or later.** The library assumes `cl-lib`, `seq`, `map`, `subr-x`, `project.el`, and `lexical-binding`. The CL-flavoured helpers (pathnames, characters, time accessors, etc.) live under the `cl-` prefix — the same namespace `cl-lib` itself uses — provided there is no collision; otherwise they fall back to a `pjb-` prefix.

## Status of interactive libraries

Maturity assessment of the user-facing libraries (those containing `interactive` forms). Pure utility / data-structure libraries (`pjb-list`, `pjb-strings`, `pjb-graph`, `pjb-queue`, `pjb-class`, `pjb-object`, `pjb-pmatch`, `pjb-make-depends`, `pjb-state-coding`, `pjb-worldfact`, `pjb-bourse`, `astruct`, `inputrc`, `room`, `inspect`, `gpx`, `sound`, `slime-rpc`, `site-start`, …) are not listed here — they are libraries, not commands.

Legend: **stable** = finished, used regularly · **working** = usable, rough edges · **POC** = sketch / experimental · **obsolete** = superseded.

| File                         | Status   | Notes                              |
|------------------------------|----------|------------------------------------|
| pjb-emacs.el                 | stable   | large mature interactive helpers   |
| pjb-utilities.el             | stable   | mature utilities                   |
| pjb-advices.el               | stable   | clean advices collection           |
| pjb-loader.el                | stable   | mature loader                      |
| pjb-test-driver.el           | stable   | clean test runner                  |
| pjb-eval.el                  | stable   | small focused eval-last-expression |
| pjb-describe.el              | stable   | small describe helper              |
| pjb-emacs-patches.el         | working  | patches with TODOs                 |
| pjb-emacs-balance-windows.el | stable   | minor TODO                         |
| pjb-milliways.el             | working  | small fun module                   |
| pjb-sources.el               | stable   | large sources tooling              |
| pjb-constants.el             | stable   | data + interactive helpers         |
| pjb-cl-faces.el              | stable   | mature CL faces                    |
| pjb-cl-magic.el              | stable   | mature CL auto-paren               |
| pjb-asdf.el                  | stable   | clean ASDF helpers                 |
| pjb-i2p-expression.el        | stable   | minor TODOs                        |
| pjb-s2p-expression.el        | stable   | scheme→prefix converter            |
| pjb-cvspass.el               | stable   | small cvs pass helper              |
| pjb-page.el                  | stable   | page-by-page viewer                |
| pjb-layers.el                | stable   | mature layers mode                 |
| pjb-transpose.el             | stable   | one TODO                           |
| pjb-secouer.el               | working  | small fun module                   |
| pjb-caps-mode.el             | stable   | small minor mode                   |
| pjb-oldies.el                | stable   | mature legacy combo                |
| pjb-searches.el              | stable   | clean searches                     |
| pjb-find-tag-hook.el         | stable   | small hook                         |
| pjb-computer-paper.el        | working  | one TODO                           |
| edit.el                      | stable   | tiny edit helper                   |
| split.el                     | working  | usable, one TODO                   |
| pjb-blink.el                 | stable   | small mature                       |
| pjb-server.el                | stable   | TCP server helper                  |
| pjb-frame-server-old.el      | obsolete | superseded by emacsclient          |
| pjb-shell.el                 | stable   | small shell helpers                |
| pjb-comint.el                | stable   | small comint filter                |
| pjb-echo-keys.el             | stable   | clean small mode                   |
| emacs-window-focus-mouse.el  | stable   | tiny utility                       |
| emacs-uptime.el              | stable   | tiny utility                       |
| pjb-c.el                     | working  | several TODOs                      |
| pjb-c-style.el               | stable   | C style definition                 |
| freerdp-c-style.el           | stable   | C style definition                 |
| pjb-thi.el                   | stable   | header/impl toggle                 |
| pjb-objc-mode.el             | working  | some TODOs                         |
| pjb-objc-edit.el             | stable   | ObjC edit helpers                  |
| pjb-objc-gen.el              | working  | code generation                    |
| pjb-objc-ide.el              | working  | rough IDE bits                     |
| pjb-objc-parser.el           | working  | partial parser                     |
| pjb-java.el                  | stable   | mature Java support                |
| pjb-java-generate.el         | working  | DTO generator                      |
| android-classes.el           | stable   | data + lookups                     |
| pjb-ruby.el                  | stable   | small Ruby helpers                 |
| pjb-pl1.el                   | working  | PL/1 mode, one TODO                |
| pjb-asm7090.el               | stable   | mature asm mode                    |
| autocad.el                   | stable   | new mode with ERT tests            |
| pjb-mail.el                  | stable   | mature, some TODOs                 |
| pjb-pgp.el                   | stable   | mature PGP integration             |
| pjb-gnus.el                  | stable   | small gnus helpers                 |
| pjb-vm.el                    | working  | VM is upstream-obsolete            |
| pjb-vm-kill-file.el          | working  | depends on obsolete VM             |
| pjb-erc.el                   | stable   | mature ERC config                  |
| pjb-erc-filter.el            | stable   | small filter                       |
| pjb-erc-speak.el             | stable   | ERC speak integration              |
| pjb-html.el                  | stable   | mature html helpers                |
| pjb-http.el                  | working  | small http helpers                 |
| pjb-xml.el                   | working  | one TODO                           |
| pjb-browser.el               | working  | TODOs                              |
| pjb-google-translate.el      | working  | small wrapper                      |
| pjb-termbin.el               | stable   | small termbin client               |
| pjb-org.el                   | stable   | clean org helpers                  |
| pjb-org-uml.el               | POC      | ~24 lines, sketch                  |
| pjb-banks.el                 | stable   | mature banks parsing               |
| pjb-banks-old.el             | obsolete | superseded by pjb-banks            |
| pjb-selftrade.el             | working  | scraper                            |
| pjb-euro.el                  | stable   | euro currency helpers              |
| pjb-roman.el                 | stable   | minor TODOs                        |
| pjb-date.el                  | stable   | small date helpers                 |
| pjb-unicode.el               | stable   | mature unicode helpers             |
| pjb-math.el                  | stable   | small math helpers                 |
| pjb-tla.el                   | stable   | TLA helpers                        |
| insulte.el                   | stable   | insult generator                   |
| pjb-color.el                 | stable   | mature color tools                 |
| pjb-ansi-color.el            | stable   | clean ANSI handling                |
| pjb-font.el                  | stable   | mature font helpers                |
| pjb-image-minor-mode.el      | stable   | small minor mode                   |
| pjb-insert-image.el          | working  | comint patch                       |
| pjb-animate.el               | POC      | ~17 lines, sketch                  |
| pjb-speak.el                 | stable   | small speech helper                |
| polygon.el                   | POC      | ~20 lines, sketch                  |
| pjb-cvs.el                   | working  | analyser, no `interactive`         |
| pjb-work.el                  | stable   | personal work helpers              |
| pjb-dodo.el                  | working  | small fun module                   |
| pjb-dot.el                   | stable   | dot file generator                 |
| pjb-zone.el                  | stable   | small zone helper                  |
| psql.el                      | working  | several TODOs                      |
| pjb-xcode.el                 | stable   | theme loader                       |
| pjb-xresources.el            | stable   | Xresources helpers                 |
| dxo.el                       | stable   | site config                        |
| trustonic.el                 | stable   | site config                        |
| ubudu.el                     | stable   | site config                        |

## Library classification

### Core utilities
- **pjb-utilities.el** — Various utility functions.
- **pjb-utilities-test.el** — Unit tests for `pjb-utilities.el`.
- **pjb-advices.el** — Patches various Emacs functions via `defadvice`.
- **pjb-emacs.el** — Functions useful only in interactive Emacs sessions.
- **pjb-emacs-patches.el** — Random patches to Emacs.
- **pjb-loader.el** — Loads pjb Emacs sources.
- **pjb-make-depends.el** — Generates dependencies for Lisp sources from `(require)` sexps.
- **pjb-test-driver.el** — Test driver that loads and runs all `pjb-*` tests.
- **pjb-milliways.el** — Schedules functions to run at the end of `~/.emacs`.
- **pjb-describe.el** — `describe` helpers.
- **inspect.el** — Object inspector.
- **room.el** — Implements a `room` function for Emacs Lisp.
- **emacs-uptime.el** — Reports Emacs process uptime (ttn).
- **site-start.el** — `site-start` file needed for `~/.emacs`.

### Common Lisp support
- **pjb-cl.el** — Exports a few Common Lisp operators missing from `cl`.
- **pjb-cl-faces.el** — Defines font-lock faces for COMMON-LISP symbols.
- **pjb-cl-magic.el** — Automagically inserts parentheses while typing Common Lisp code.
- **pjb-cl-magic-lambda-lists.el** — Hash table of CL lambda lists for the cl-magic auto-paren engine.
- **pjb-clelp.el** — Helpers for Common Lisp development.
- **pjb-asdf.el** — ASDF utilities.
- **pjb-eval.el** — `eval-last-expression` working for Common Lisp, Emacs Lisp and Scheme.
- **slime-rpc.el** — Emacs/Common Lisp RPC over SLIME/Swank.
- **pjb-pmatch.el** — Sexp pattern matcher.
- **pjb-class.el** — Helpers for classes.
- **pjb-object.el** — Root class providing compatibility utilities.
- **pjb-cvspass.el** — Unscrambles CVS passwords stored in `~/.cvspass`.
- **scratch.el** — Sketches/syntactic sugar for `defclass`.

### Data structures
- **pjb-list.el** — List utility functions.
- **pjb-queue.el** — FIFO queue type with head/tail pointers.
- **pjb-graph.el** — Graph class.
- **astruct.el** — Structures-as-alists prefixed by the structure type name.
- **inputrc.el** — Reader for readline `~/.inputrc` files.

### Strings, text, editing
- **pjb-strings.el** — String utility functions.
- **pjb-transpose.el** — Transposes or rotates the characters of a region.
- **pjb-secouer.el** — Randomizes the internal letters of words in the selected region.
- **pjb-caps-mode.el** — `caps-mode` for capitalization handling.
- **pjb-oldies.el** — Combines paper-mode, caps-mode, upcase/downcase-lisp via file hooks.
- **pjb-page.el** — Views a buffer page by page.
- **pjb-computer-paper.el** — Computer-paper mode helper.
- **pjb-layers.el** — Major mode for editing layered texts (pages separated by `^L`) with merging.
- **pjb-find-tag-hook.el** — `find-tag` hook.
- **pjb-searches.el** — Web and grep searchers.
- **edit.el** — Minimal edit-then-set-value buffer helper.
- **split.el** — Window splitting utilities for Emacs ≥ 24.

### Buffer / window / frame / shell
- **pjb-emacs-balance-windows.el** — Set the same size for all windows.
- **pjb-blink.el** — Alternate blinking parens.
- **pjb-server.el** — Manage a TCP server process inside Emacs (using netcat to listen).
- **pjb-frame-server-old.el** — Obsolete frame server (superseded by `mfod` / `emacsclient`).
- **emacs-window-focus-mouse.el** — Selects the window under the mouse pointer.
- **pjb-shell.el** — `shell`/`nshell` defined as functions instead of advices.
- **pjb-comint.el** — Adds output filters to handle more emacs-048 codes in comint.
- **pjb-echo-keys.el** — Echoes Emacs key chords as they are typed.
- **pjb-echo-keys-test.el** — Unit tests for `pjb-echo-keys.el`.

### Language modes — C family
- **pjb-c.el** — C indenting functions.
- **pjb-c-style.el** — Defines PJB's own C style.
- **freerdp-c-style.el** — C style used for the FreeRDP project.
- **pjb-thi.el** — Toggle header/implementation buffers for C-like languages.

### Language modes — Objective-C
- **pjb-objc-mode.el** — Objective-C major mode.
- **pjb-objc-edit.el** — Editing utilities for Objective-C++ with strange style rules.
- **pjb-objc-gen.el** — Generates Objective-C code.
- **pjb-objc-ide.el** — Objective-C refactoring tools.
- **pjb-objc-parser.el** — Partial parsers for Objective-C code.
- **pjb-objc-parser-test.el** — Tests for `pjb-objc-parser`.

### Language modes — Java, Ruby, PL/I, ASM, AutoCAD
- **pjb-java.el** — Commands to help editing Java code.
- **pjb-java-generate.el** — Generates Java DTO classes from descriptions.
- **android-classes.el** — Scrapes Android reference HTML to build package/class info.
- **pjb-ruby.el** — Help for Ruby editing.
- **pjb-pl1.el** — Major mode for editing PL/1 source.
- **pjb-asm7090.el** — Support for IBM 7090 assembler.
- **autocad.el** — Major modes for AutoCAD AutoLISP/Visual LISP, DCL, DWG.
- **autocad-test.el** — ERT tests for `autocad.el` font-lock and indentation.

### Mail / news / IRC
- **pjb-mail.el** — Various functions related to mail handling.
- **pjb-pgp.el** — Integrates PGP with Emacs.
- **pjb-gnus.el** — Defines Gnus commands.
- **pjb-vm.el** — VM mail reader stuff (obsolete).
- **pjb-vm-kill-file.el** — Kill-file feature for VM.
- **pjb-erc.el** — ERC stuff (anti-flood, lisppaste yank, etc.).
- **pjb-erc-filter.el** — Filters IRC nicks.
- **pjb-erc-speak.el** — ERC speak (text-to-speech) integration.

### Web / HTML / HTTP / XML
- **pjb-html.el** — HTML tag navigation (skip/up/down tag, etc.).
- **pjb-http.el** — HTTP and related helpers.
- **pjb-xml.el** — XML parsing patches/utilities.
- **pjb-browser.el** — A column-based browser.
- **pjb-google-translate.el** — Attempts to query Google Translate.
- **pjb-termbin.el** — Save buffer or region to termbin.

### Org-mode
- **pjb-org.el** — `org-mode` utilities including splitting big blocks into included files.
- **pjb-org-uml.el** — `org-mode` helpers for UML use-cases.

### Finance / banking
- **pjb-banks.el** — Formats bank movement listings fetched from the web.
- **pjb-banks-old.el** — Older bank listing formatting code.
- **pjb-bourse.el** — Stock-market helpers.
- **pjb-selftrade.el** — Selftrade brokerage helpers.
- **pjb-euro.el** — Euro currency utilities.

### Internationalization, dates, numbers, math
- **pjb-roman.el** — Roman numeral utilities.
- **pjb-date.el** — Date formats.
- **pjb-unicode.el** — Functions binding keys locally to Unicode alphabets.
- **pjb-math.el** — Bindings for mathematical symbols.
- **pjb-constants.el** — Constants of physics.
- **pjb-constants-test.el** — Unit tests for `pjb-constants.el`.
- **pjb-i2p-expression.el** — Converts infix expressions to prefix s-expressions and evaluates them.
- **pjb-s2p-expression.el** — Converts s-expressions to other forms (companion to i2p).
- **pjb-worldfact.el** — Functions to process the CIA World Factbook.
- **pjb-tla.el** — Three-letter-acronym dictionary.
- **insulte.el** — Captain Haddock's insults generator.

### Multimedia: sound, color, image, graphics
- **pjb-color.el** — Setting RGB colors with sliders.
- **pjb-ansi-color.el** — Colorize buffers containing ANSI color escape sequences.
- **pjb-font.el** — Font manipulation helpers.
- **pjb-image-minor-mode.el** — Minor mode replacing image links by inline images.
- **pjb-insert-image.el** — Patch to insert images in comint buffers (e.g. `inferior-lisp`).
- **pjb-animate.el** — Animates buffer contents at a chosen speed.
- **pjb-speak.el** — Calls an external `speak` script to say text.
- **sound.el** — Plays a generated sine wave through `play-sound`.
- **polygon.el** — Collects mouse-click points into a polygon.

### Source-code management & work tracking
- **pjb-cvs.el** — Application that analyses CVS revision graphs.
- **pjb-sources.el** — Functions helpful when writing programs (see also `pjb-state-coding.el`).
- **pjb-state-coding.el** — Generates a structure encoding hierarchical states into a bit field.
- **pjb-work.el** — Inserts start/stop timestamps and updates total line counts.
- **pjb-dodo.el** — Traces daily start/stop times and plots them.

### External tools / integrations
- **pjb-dot.el** — Generates Graphviz dot files from `pjb-graph` graphs.
- **gpx.el** — GPX (GPS exchange) data structures via `java-defstruct`.
- **pjb-zone.el** — Formats DNS zone files.
- **psql.el** — Interactive wrapper around `pg.el` for PostgreSQL.
- **pjb-xcode.el** — Loads an Xcode DVT theme plist and applies its colors to font-lock.
- **pjb-xresources.el** — Produces `~/.Xresources` from current Emacs settings.

### Site-specific configurations
- **dxo.el** — Emacs configuration used at DxO Labs / Optics Pro Mac team.
- **trustonic.el** — Emacs configuration used at Trustonic Ltd.
- **ubudu.el** — Emacs configuration used at Ubudu SAS.

