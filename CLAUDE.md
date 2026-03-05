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
  Main.elm             — App entry point (Browser.element), ports, Model, update, view
  Api.elm              — OpenRouter HTTP calls, system prompts, response schema, error types
  Story.elm            — SavedStory type, StoryMode, JSON encode/decode, builder
  Exercise/
    Types.elm          — ExerciseState, ExerciseItem, ExerciseInputMode, phase logic
    View.elm           — Exercise UI: input tabs, active exercise, saved stories list
  Grammar/
    Types.elm          — Core grammar types (VerbTense, Tense, FiniteSpec, etc.)
    Engine.elm         — Sentence generation engine
    Lexicon.elm        — Subjects, verbs, default arguments
    Render.elm         — Render grammar structures to SentenceWord lists
  Timeline.elm         — SVG verb tense timeline diagram (interactive)
  style.css            — Tailwind v4 import
  main.js              — Minimal JS: PKCE/OAuth helpers, storage ports (~120 lines)
index.html             — HTML shell (dark theme)
elm.json               — Elm dependencies
package.json           — Node devDependencies
vite.config.js         — Vite + Elm + Tailwind plugins
Taskfile.yml           — Task runner config
flake.nix              — Nix dev environment
```

## Architecture Patterns

### Elm ↔ JS boundary

- **All business logic lives in Elm** — HTTP calls, prompts, schemas, language data, state management
- **JS is minimal** (~120 lines in `main.js`) — only PKCE/OAuth (Web Crypto API), storage ports
- Ports:
  - **Outgoing** (Elm → JS): `startOAuthLogin`, `saveApiKey`, `clearApiKey`, `saveStories`
  - **Incoming** (JS → Elm): `oauthKeyReceived`
- Flags: `{ apiKey : Maybe String, savedStories : List SavedStory }` — loaded from sessionStorage/localStorage

### API layer (`Api.elm`)

- All OpenRouter HTTP calls use `Http.request` with `Http.expectStringResponse` for status-code-aware error handling
- `ApiError` type: `Unauthorized | InsufficientCredits | RateLimited | NetworkError | BadResponse`
- On `Unauthorized`: clear API key via port, reset model state
- Response decoding: parse outer `choices[0].message.content` (string), then decode inner JSON with `decodeLLMResponse`

### Story persistence (`Story.elm`)

- Stories auto-save to localStorage via `saveStories` port after successful LLM response
- `SavedStory` contains all exercise items — replaying skips the LLM call entirely
- `StoryMode`: `UserWritten String` (stores original narrative) | `AIGenerated`
- Stories loaded from flags on init, displayed in "Saved Stories" tab

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
