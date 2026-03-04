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
   - Determine if animations are involved (elm-animator patterns)
   - Plan Tailwind classes for styling
3. Implement following existing patterns in src/Main.elm and other modules
4. Verify the implementation:
   - Run `task build` to check for compilation errors
   - Run `task format` to format Elm files
   - Run `task dev` and confirm it works in the browser
