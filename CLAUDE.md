# Grammar Space

Visual grammar learning app for English learners — pattern recognition through the brain's visual capabilities.

## Tech Stack

- **Elm 0.19.1** — Frontend language
- **Vite 5+** — Bundler / dev server (`vite` + `vite-plugin-elm`)
- **Tailwind CSS v4** — Styling (`tailwindcss` + `@tailwindcss/vite`)
- **elm-animator** — Animations (`mdgriffith/elm-animator`)
- **go-task** — Task runner (`Taskfile.yml`)
- **Nix flakes** — Dev environment (`flake.nix`)

## Quick Start

```sh
nix develop          # Enter dev shell (elm, node, task)
task install         # Install npm + Elm dependencies
task dev             # Start dev server at http://localhost:5173
task build           # Production build → dist/
```

## Commands

| Command        | Description                              |
|----------------|------------------------------------------|
| `task install` | Install all deps (npm + elm)             |
| `task dev`     | Start Vite dev server                    |
| `task build`   | Production build                         |
| `task test`    | Run elm-test                             |
| `task format`  | Format Elm files with elm-format         |
| `task clean`   | Remove node_modules, elm-stuff, dist     |

## Project Structure

```
src/
  Main.elm       — App entry point (Browser.element)
  style.css      — Tailwind v4 import
  main.js        — Elm app mount
index.html       — HTML shell (dark theme)
elm.json         — Elm dependencies
package.json     — Node devDependencies
vite.config.js   — Vite + Elm + Tailwind plugins
Taskfile.yml     — Task runner config
flake.nix        — Nix dev environment
```

## Architecture Patterns

### elm-animator

- Wrap animated state in `Animator.Timeline`: `active : Animator.Timeline Bool`
- **Always use `Animator.watchingWith`**, never `Animator.watching` — the latter uses `(always True)` and fires `onAnimationFrame` ~60fps forever, even when idle. Use `(always False)` as the resting predicate unless a state needs continuous animation (e.g. a looping spinner).
- Wire `Animator.toSubscription Tick model animator` into subscriptions
- Handle `Tick Time.Posix` in update with `Animator.update newTime animator`
- Use `Animator.go duration newState` to trigger animations
- Use `Animator.Inline.opacity`, `Animator.Inline.backgroundColor`, `Animator.Inline.scale` etc. in view

### Tailwind in Elm

- Apply Tailwind classes via `Html.Attributes.class "..."` in Elm views
- Tailwind v4 auto-detects classes in `src/` via `@source "../src"` in style.css
- Use `Animator.Inline.*` for animated CSS properties; Tailwind for static layout

## Coding Conventions

- Elm dependencies are always installed via `elm install` (never edit elm.json by hand)
- Use `Browser.element` for apps mounted into a DOM node
- Keep Elm modules focused — one module per logical concern
- Format Elm code with `elm-format` before committing

## Git

- **Always sign commits** with GPG. Never use `--no-gpg-sign`. If signing fails (e.g. GPG timeout), retry the same commit command — do not skip signing.
