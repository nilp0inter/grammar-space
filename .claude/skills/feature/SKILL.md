---
name: feature
description: Implement a new feature in Grammar Space following project conventions and elm-animator patterns.
argument-hint: [feature description]
allowed-tools: Read, Grep, Glob, Write, Edit, Bash
---

Implement a new feature: $ARGUMENTS

## Workflow

1. Read CLAUDE.md for project context, stack, and conventions
2. Plan the implementation:
   - Identify which Elm types, messages, and modules are needed
   - If the feature involves API calls, add functions to `src/Api.elm`
   - If the feature involves persistence, extend `src/Story.elm` and wire the `saveStories` port
   - If the feature needs new exercise UI, extend `src/Exercise/Types.elm` and `src/Exercise/View.elm`
   - Determine if animations are involved (elm-animator patterns)
   - Plan Tailwind classes for styling
   - Keep JS minimal — only add to `src/main.js` for things Elm cannot do (Web Crypto, OAuth redirects, browser storage)
3. Implement following existing patterns:
   - `src/Main.elm` — Model fields, Msg variants, update wiring, ports, view
   - `src/Api.elm` — HTTP calls with `Http.expectStringResponse`, `ApiError` handling
   - `src/Story.elm` — SavedStory types, JSON codecs
   - `src/Exercise/View.elm` — ExerciseConfig record pattern for view parameters
4. Verify the implementation:
   - Run `task build` to check for compilation errors
   - Run `task format` to format Elm files
   - Run `task dev` and confirm it works in the browser
