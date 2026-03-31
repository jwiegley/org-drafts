# org-drafts

A small package that intercepts `org-capture` finalization to present a hydra
menu for dispatching draft content. When you capture a DRAFT entry and press
`C-c C-c`, instead of immediately filing it, org-drafts offers a set of
choices: convert to TODO or NOTE, copy to the clipboard, or send to an
external service (GPTel, Kagi, Claude.ai, Perplexity, email). It also works
on existing DRAFT and SCRAP entries outside of capture mode.

## Installation

```elisp
(use-package org-drafts
  :after (org)
  :bind* ("M-M" . (lambda () (interactive) (org-capture nil "d")))
  :config
  (org-drafts-install))
```

With the following org-capture template:

```elisp
("d" "DRAFT" entry
  "path-to-inbox-file.org"
  "* DRAFT %U\n%?"
  :prepend t)
```

## How it works

When you finalize a DRAFT capture, a hydra menu appears:

| Key   | Action     | Description                        |
|-------|------------|------------------------------------|
| `n`   | NOTE       | Convert to a NOTE entry            |
| `t`   | TODO       | Convert to a TODO entry            |
| `d`   | DRAFT      | Keep as DRAFT (finalize normally)  |
| `S`   | SCRAP      | Mark as SCRAP                      |
| `c`   | Copy       | Copy body to clipboard             |
| `M`   | Markdown   | Copy as markdown code block        |
| `s`   | Slack      | Copy formatted for Slack           |
| `k`   | Kagi       | Search with Kagi                   |
| `g`   | GPTel      | Send to GPTel chat buffer          |
| `p`   | Perplexity | Search with Perplexity             |
| `C`   | Claude     | Send to Claude.ai                  |
| `m`   | Email      | Create email with draft content    |
| `r`   | Rewrite    | Rewrite body with LLM via gptel    |

The hydra also activates on existing DRAFT and SCRAP entries when you press
`C-c C-c` (via `org-ctrl-c-ctrl-c-hook`).

## Dependencies

Required: `org`, `cl-lib` (both built-in), `copy-as-format`, `pretty-hydra`.

Optional: `gptel` (for GPTel and Rewrite actions), `ox-slack` (for Slack
formatting).

## Customization

`org-drafts-task-body-function` controls how the body text is processed when
converting to TODO or NOTE. The default moves the first line of body text
into the heading title. Replace it to change that behavior.

## Development

Enter the development shell:

```bash
nix develop
```

Run all checks:

```bash
make all         # compile + lint + checkdoc + format-check + test
nix flake check  # same checks via Nix
```

Individual targets:

```bash
make compile       # byte-compile with warnings as errors
make lint          # package-lint
make checkdoc      # documentation string validation
make format-check  # verify indentation matches Emacs standards
make format        # auto-format to Emacs standards
make test          # ERT tests
make coverage      # tests with coverage report
make benchmark     # performance benchmarks
```

Pre-commit hooks (via lefthook) run all checks in parallel.

## License

Copyright (C) 2025-2026, John Wiegley. All rights reserved.
See [LICENSE.md](LICENSE.md) for details.
