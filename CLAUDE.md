# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

org-drafts is a single-file Emacs Lisp package that intercepts org-capture finalization to present a hydra menu for dispatching draft content. When a user captures a DRAFT entry and presses `C-c C-c`, instead of immediately filing it, org-drafts offers choices: convert to TODO/NOTE, copy to clipboard, or send to external services (GPTel, Kagi, Claude.ai, Perplexity, email).

## Build and Compilation

Uses a Makefile, Nix flake, and lefthook for pre-commit checks.

```bash
# Build targets
make compile        # Byte-compile with warnings-as-errors
make lint           # Run package-lint
make checkdoc       # Validate docstrings
make format-check   # Check Emacs indentation standards
make format         # Auto-format to Emacs indentation standards
make test           # Run ERT test suite
make coverage       # Generate lcov coverage report
make coverage-check # Verify coverage meets threshold
make benchmark      # Run performance benchmarks
make benchmark-save # Save benchmark baseline
make benchmark-check # Check for >5% regression
make all            # compile + lint + checkdoc + format-check + test

# Nix
nix flake check     # Run all 5 checks (compile, lint, checkdoc, format-check, test)
nix build           # Build the package
nix develop         # Enter dev shell with all dependencies
```

Tests are in `org-drafts-test.el` (39 ERT tests). The `.elc` file is tracked in git.

## Dependencies

Required at load time: `cl-lib`, `org-macs`, `org-capture`, `copy-as-format`, `pretty-hydra`.

Optional (loaded on demand): `gptel`, `ox-slack` (commented out but referenced), `gnus` (via `compose-mail`).

## Architecture

### Core execution framework

All draft actions flow through a two-layer composition:

1. **`org-drafts-with`** — The foundation. Takes three callbacks:
   - `at-heading-func` — called at the heading position (e.g., to replace the keyword)
   - `at-capture-end-func` — called after body processing (e.g., to finalize capture)
   - `body-func` — receives three markers: `heading-pos`, `beg`, `end` covering the body text

   It handles navigation (back to heading, skip PROPERTIES drawer), creates markers, and detects whether we're in a capture buffer or an existing entry.

2. **`org-drafts-with-change-to`** — Wraps `org-drafts-with` with a keyword-replacement `at-heading-func` and capture-finalization `at-capture-end-func`. Every action function (`org-drafts-copy-to-clipboard`, `org-drafts-gptel`, `org-drafts-kagi`, etc.) calls this with `"SCRAP"` and a lambda that processes the body text.

**`org-drafts-change`** is a `defsubst` shorthand that calls `org-drafts-with-change-to` using the customizable `org-drafts-task-body-function` (default: `org-drafts-default-body-function`, which extracts the first line of body text as the heading title).

### Entry points

- **`org-drafts-action`** — Bound to `C-c C-c` in `org-capture-mode-map`. If the entry is a DRAFT, shows the hydra; otherwise delegates to `org-capture-finalize`.
- **`org-drafts-act-on-existing`** — Added to `org-ctrl-c-ctrl-c-hook`. Activates the hydra when point is on an existing DRAFT or SCRAP entry outside of capture mode.
- **`org-drafts-install`** — Sets up both of the above bindings/hooks.

### Hydra menu (`org-drafts/body`)

Defined via `pretty-hydra-define`. Three columns: Org (NOTE/TODO/DRAFT/SCRAP), Utils (clipboard/markdown/slack), Other (Kagi/GPTel/Perplexity/Claude/Email).

### Buffer manipulation pattern

Body functions receive marker arguments and use `with-restriction` to narrow the buffer to just the body region before operating. This is the pattern to follow when adding new action functions.

## Adding a New Action

Follow the existing pattern — create an interactive function that calls `org-drafts-with-change-to`:

```elisp
(defun org-drafts-my-action ()
  "Description."
  (interactive)
  (org-drafts-with-change-to "SCRAP"
    (lambda (_heading-pos beg end)
      (with-restriction beg end
        (let ((str (string-trim (buffer-string))))
          ;; Do something with str
          )))))
```

Then add a keybinding entry in the `pretty-hydra-define` form.

## Customization

`org-drafts-task-body-function` — controls how body text is processed when converting to TODO/NOTE. The default (`org-drafts-default-body-function`) moves the first body line into the heading title. Replace this to change that behavior.

## MCP Integration

The `.mcp.json` configures an `elisp-dev` MCP server for live introspection of the running Emacs environment. Use the elisp-dev tools (`elisp-describe-function`, `elisp-get-function-definition`, `elisp-describe-variable`, `elisp-info-lookup-symbol`) to inspect definitions at runtime.

## Issue Tracking

This project uses beads (`bd`) for issue tracking. See `AGENTS.md` for the workflow.
