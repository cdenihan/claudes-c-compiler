# Frontend

Transforms C source text into a typed AST through four phases:

```
Source text -> Preprocessor -> Lexer -> Parser -> Sema -> Typed AST
```

## Modules

- **preprocessor/** - Text-to-text pass handling `#include`, `#define`, `#if`/`#ifdef`,
  `#pragma once`, `#pragma pack`, macro expansion (function-like and object-like),
  `#`/`##` operators, `__VA_ARGS__`, and line splicing. See `preprocessor/README.md`.
- **lexer/** - Tokenizes preprocessed source into `Token`s with source locations.
  Handles keywords, identifiers, numeric/string/character literals, operators,
  and punctuation. See `lexer/README.md`.
- **parser/** - Recursive descent parser producing a spanned AST. Split into focused
  sub-modules (expressions, types, statements, declarations, declarators).
  See `parser/README.md`.
- **sema/** - Semantic analysis: type checking, symbol table construction,
  `__builtin_*` signature mapping, expression type inference (`ExprTypeChecker`),
  and compile-time constant evaluation (`SemaConstEval`). See `sema/README.md`.

## Key Design Decisions

- The preprocessor runs as a text-to-text pass before lexing (not integrated into the
  lexer). This simplifies the architecture but means original source locations within
  macros are approximated via `# line "file"` markers.
- The parser uses recursive descent (no parser generator), making error recovery and
  GCC extension support straightforward.
- Sema produces both warnings and errors (e.g., non-integer switch expressions,
  implicit function declarations with `-Werror`). Type inference is split between
  sema (via `ExprTypeChecker` and `SemaConstEval`) and IR lowering.

## Known Limitations

- `_Atomic(type)` is parsed but treated as the underlying type (qualifier not tracked)
- `_Generic` has limited const-qualifier tracking for type matching
