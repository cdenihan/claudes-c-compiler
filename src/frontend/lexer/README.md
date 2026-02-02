# Lexer

Tokenizes C source code into a stream of tokens with source locations for the parser.

## Files

| File | Responsibility |
|------|---------------|
| `lexer.rs` | Main lexer implementation. Handles keywords, identifiers, numeric literals (int, float, hex, octal, binary), string/character literals (including wide, u8, UTF-16/32), operators, and punctuation. Tracks source positions via byte offsets |
| `token.rs` | Token type definitions (`Token` enum with span, `TokenKind` variants) |

## Design Notes

- The lexer operates on preprocessed text (after comment stripping and macro expansion)
- Source locations are byte offsets into the preprocessed text, mapped back to
  original file/line/column via `# line "file"` markers from the preprocessor
- Non-UTF-8 source bytes are handled via PUA encoding (see `common/encoding.rs`)
