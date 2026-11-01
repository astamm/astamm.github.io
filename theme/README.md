# theme/: shared building blocks for the course decks

This folder holds the pieces shared across the six course decks (three "The R
Statistical Language" / base R, three Reproducible Workflows): the brand SCSS
and the CNRS logo.

```
theme/
├── README.md                 # this file
├── scss/ensai-theme.scss     # the ONE shared theme: brand palette + all layout/class rules for all six decks
└── logo/logo-cnrs.png        # CNRS logo used by every deck
```

## One shared theme for all six decks

All six decks (base R and RR alike) render from a **single shared theme file**,
`scss/ensai-theme.scss`. It is referenced once in the root `_quarto.yml`:

```yaml
format:
  revealjs:
    theme: [default, resources/slidecrafting/styles_template.scss, theme/scss/ensai-theme.scss]
```

`resources/slidecrafting/styles_template.scss` is the reusable dark-canvas
layout layer (its brand variables carry `!default`); `ensai-theme.scss`
declares the brand after it, so its values win. The brand (palette, layout
classes, the accent engine, `.term` terminal blocks) is course-agnostic: there
is no per-course or per-deck SCSS.

## How to edit it

Edit `scss/ensai-theme.scss`; all six decks pick up the change on the next
render. Do not fork it per deck. Layout-affecting rules (the `0.70em` base
size, `.paper-card` sizing, `.inv` handling, the `.stack`/`.code-pop` pins,
the accent engine) intentionally apply identically to all six sessions. See
`AGENTS.md` for the universal accent naming (`draft / review / release` etc.)
and for the gotcha that some selectors there are dormant on purpose and must
not be "cleaned up".

The `theme:` list and every other deck-common revealjs option live **once in
the root `_quarto.yml`**, applied to all six decks by project metadata
merging. Paths there resolve project-root-relative. Each deck's front matter
keeps only its varying keys (`title`, `subtitle`, footer, and on base-R 1/3
`knitr.opts_chunk.echo: true`).

## Logo

`logo/logo-cnrs.png` (also mirrored at `resources/logo/logo-cnrs.png`) is
shared once. Decks no longer set a per-deck `logo:`; it is centralized as a
base64 data URI in `_quarto.yml` under `format.revealjs.logo` (a relative path
is blocked under `file://`). The standalone `template/` deck keeps its own
local `logo:`.

## Setup guides

The setup guide is a single root `setup.qmd` (install R / Positron / Quarto /
packages, plus Python via uv), not a `theme/` file.
