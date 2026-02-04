//! Shared GAS assembly preprocessing utilities.
//!
//! Functions used by multiple assembler parsers (ARM, RISC-V, x86) for
//! text-level preprocessing before architecture-specific parsing:
//!
//! - C-style comment stripping (`/* ... */`)
//! - Line comment stripping (`#`, `//`, `@`)
//! - Semicolon splitting (GAS statement separator)
//! - `.rept`/`.irp`/`.endr` block expansion
//! - `.macro`/`.endm` definition and expansion
//! - `.if`/`.else`/`.endif` conditional assembly evaluation

use crate::backend::asm_expr;

// ── Comment handling ───────────────────────────────────────────────────

/// Characters that start a line comment for each architecture.
///
/// Used by `strip_comment` to determine where comments begin.
/// Each architecture may have multiple comment styles.
pub enum CommentStyle {
    /// `#` only (x86/x86-64 AT&T syntax)
    Hash,
    /// `#` and `//` (RISC-V GAS)
    HashAndSlashSlash,
    /// `//` and `@` (ARM GAS — but `@` is not a comment before type specifiers
    /// like `@function`, `@object`, `@progbits`, `@nobits`, `@tls_object`, `@note`)
    SlashSlashAndAt,
}

/// Strip C-style `/* ... */` comments from assembly text, handling multi-line spans.
/// Preserves newlines inside comments so line numbers remain correct for error messages.
pub fn strip_c_comments(text: &str) -> String {
    let mut result = String::with_capacity(text.len());
    let bytes = text.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'*' {
            i += 2;
            while i + 1 < bytes.len() {
                if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                    i += 2;
                    break;
                }
                if bytes[i] == b'\n' {
                    result.push('\n');
                }
                i += 1;
            }
        } else {
            result.push(bytes[i] as char);
            i += 1;
        }
    }
    result
}

/// Strip trailing line comment from a single line, respecting string literals.
///
/// Scans character by character tracking quote state so that comment characters
/// inside `"..."` strings are not treated as comment starts. Handles escaped
/// quotes (`\"`) correctly.
pub fn strip_comment<'a>(line: &'a str, style: &CommentStyle) -> &'a str {
    let bytes = line.as_bytes();
    let mut in_string = false;
    let mut i = 0;
    while i < bytes.len() {
        if in_string {
            if bytes[i] == b'\\' {
                i += 2; // skip escaped character
                continue;
            }
            if bytes[i] == b'"' {
                in_string = false;
            }
            i += 1;
            continue;
        }
        // Not in string
        if bytes[i] == b'"' {
            in_string = true;
            i += 1;
            continue;
        }
        match style {
            CommentStyle::Hash => {
                if bytes[i] == b'#' {
                    return &line[..i];
                }
            }
            CommentStyle::HashAndSlashSlash => {
                if bytes[i] == b'#' {
                    return &line[..i];
                }
                if bytes[i] == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                    return &line[..i];
                }
            }
            CommentStyle::SlashSlashAndAt => {
                if bytes[i] == b'/' && i + 1 < bytes.len() && bytes[i + 1] == b'/' {
                    return &line[..i];
                }
                if bytes[i] == b'@' {
                    // `@` is NOT a comment before type specifiers used in GAS directives
                    let after = &line[i + 1..];
                    if !after.starts_with("object")
                        && !after.starts_with("function")
                        && !after.starts_with("progbits")
                        && !after.starts_with("nobits")
                        && !after.starts_with("tls_object")
                        && !after.starts_with("note")
                    {
                        return &line[..i];
                    }
                }
            }
        }
        i += 1;
    }
    line
}

/// Split a line on `;` characters, respecting string literals.
/// In GAS syntax, `;` separates multiple statements on the same line.
pub fn split_on_semicolons(line: &str) -> Vec<&str> {
    let mut parts = Vec::new();
    let mut in_string = false;
    let mut escape = false;
    let mut start = 0;
    for (i, c) in line.char_indices() {
        if escape {
            escape = false;
            continue;
        }
        if c == '\\' && in_string {
            escape = true;
            continue;
        }
        if c == '"' {
            in_string = !in_string;
            continue;
        }
        if c == ';' && !in_string {
            parts.push(&line[start..i]);
            start = i + 1;
        }
    }
    parts.push(&line[start..]);
    parts
}

// ── .rept / .irp block expansion ───────────────────────────────────────

fn is_rept_start(trimmed: &str) -> bool {
    trimmed.starts_with(".rept ") || trimmed.starts_with(".rept\t")
}

fn is_irp_start(trimmed: &str) -> bool {
    trimmed.starts_with(".irp ") || trimmed.starts_with(".irp\t")
}

fn is_block_start(trimmed: &str) -> bool {
    is_rept_start(trimmed) || is_irp_start(trimmed)
}

/// Collect the body lines of a `.rept`/`.irp` block, handling nesting.
/// Returns the body lines and advances `i` past the closing `.endr`.
fn collect_block_body<'a>(
    lines: &[&'a str],
    i: &mut usize,
    comment_style: &CommentStyle,
) -> Result<Vec<&'a str>, String> {
    let mut depth = 1;
    let mut body = Vec::new();
    *i += 1;
    while *i < lines.len() {
        let inner = strip_comment(lines[*i], comment_style).trim().to_string();
        if is_block_start(&inner) {
            depth += 1;
        } else if inner == ".endr" {
            depth -= 1;
            if depth == 0 {
                break;
            }
        }
        body.push(lines[*i]);
        *i += 1;
    }
    if depth != 0 {
        return Err(".rept/.irp without matching .endr".to_string());
    }
    Ok(body)
}

/// Expand `.rept`/`.endr` and `.irp`/`.endr` blocks by repeating or
/// substituting contained lines.
///
/// Handles nested blocks and recursive expansion. Uses `parse_int` to
/// evaluate the `.rept` count expression.
pub fn expand_rept_blocks(
    lines: &[&str],
    comment_style: &CommentStyle,
    parse_int: fn(&str) -> Result<i64, String>,
) -> Result<Vec<String>, String> {
    let mut result = Vec::new();
    let mut i = 0;
    while i < lines.len() {
        let trimmed = strip_comment(lines[i], comment_style).trim().to_string();
        if is_rept_start(&trimmed) {
            let count_str = trimmed[".rept".len()..].trim();
            let count_val = parse_int(count_str)
                .map_err(|e| format!(".rept: bad count '{}': {}", count_str, e))?;
            // Treat negative counts as 0 (matches GNU as behavior)
            let count = if count_val < 0 { 0usize } else { count_val as usize };
            let body = collect_block_body(lines, &mut i, comment_style)?;
            let expanded_body = expand_rept_blocks(&body, comment_style, parse_int)?;
            for _ in 0..count {
                result.extend(expanded_body.iter().cloned());
            }
        } else if is_irp_start(&trimmed) {
            // .irp var, val1, val2, ...
            let args_str = trimmed[".irp".len()..].trim();
            let (var, values_str) = match args_str.find(',') {
                Some(pos) => (args_str[..pos].trim(), args_str[pos + 1..].trim()),
                None => (args_str, ""),
            };
            let values: Vec<&str> = values_str.split(',').map(|s| s.trim()).collect();
            let body = collect_block_body(lines, &mut i, comment_style)?;
            for val in &values {
                let subst_body: Vec<String> = body.iter().map(|line| {
                    let pattern = format!("\\{}", var);
                    line.replace(&pattern, val)
                }).collect();
                let subst_refs: Vec<&str> = subst_body.iter().map(|s| s.as_str()).collect();
                let expanded = expand_rept_blocks(&subst_refs, comment_style, parse_int)?;
                result.extend(expanded);
            }
        } else if trimmed == ".endr" {
            // stray .endr without .rept — skip
        } else {
            result.push(lines[i].to_string());
        }
        i += 1;
    }
    Ok(result)
}

// ── .macro / .endm expansion ───────────────────────────────────────────

/// Macro definition: name, parameter list, and body lines.
pub(crate) struct MacroDef {
    params: Vec<String>,
    body: Vec<String>,
}

/// Split macro invocation arguments, separating on commas and whitespace.
///
/// GAS allows both commas and spaces as macro argument separators.
/// Quoted strings are kept as a single argument with quotes stripped.
/// Parenthesized groups like `0(a1)` are kept together.
pub fn split_macro_args(s: &str) -> Vec<String> {
    if s.is_empty() {
        return Vec::new();
    }
    let mut args = Vec::new();
    let mut current = String::new();
    let bytes = s.as_bytes();
    let mut i = 0;
    let mut paren_depth = 0i32;
    while i < bytes.len() {
        match bytes[i] {
            b'(' => {
                paren_depth += 1;
                current.push('(');
            }
            b')' => {
                paren_depth -= 1;
                current.push(')');
            }
            b',' if paren_depth == 0 => {
                let trimmed = current.trim().to_string();
                if !trimmed.is_empty() {
                    args.push(trimmed);
                }
                current.clear();
            }
            b' ' | b'\t' if paren_depth == 0 => {
                let trimmed = current.trim().to_string();
                if !trimmed.is_empty() {
                    args.push(trimmed);
                    current.clear();
                }
                // Skip remaining whitespace
                while i + 1 < bytes.len() && (bytes[i + 1] == b' ' || bytes[i + 1] == b'\t') {
                    i += 1;
                }
                // If next char is comma, let the comma handle the split
                if i + 1 < bytes.len() && bytes[i + 1] == b',' {
                    // skip
                }
            }
            b'"' => {
                // Consume quoted string, stripping the outer quotes
                i += 1;
                while i < bytes.len() && bytes[i] != b'"' {
                    if bytes[i] == b'\\' && i + 1 < bytes.len() {
                        current.push(bytes[i + 1] as char);
                        i += 2;
                        continue;
                    }
                    current.push(bytes[i] as char);
                    i += 1;
                }
                // Skip closing quote
            }
            _ => {
                current.push(bytes[i] as char);
            }
        }
        i += 1;
    }
    let trimmed = current.trim().to_string();
    if !trimmed.is_empty() {
        args.push(trimmed);
    }
    args
}

/// Expand `.macro`/`.endm` definitions and macro invocations.
///
/// Two-pass approach:
/// 1. Collect macro definitions (`.macro name [params]` ... `.endm`)
/// 2. Expand macro invocations: lines where the first word matches a defined macro
///
/// Handles nested macro definitions and recursive expansion.
pub fn expand_macros(
    lines: &[&str],
    comment_style: &CommentStyle,
) -> Result<Vec<String>, String> {
    use std::collections::HashMap;
    let mut macros: HashMap<String, MacroDef> = HashMap::new();
    let mut result = Vec::new();
    let mut i = 0;

    while i < lines.len() {
        let trimmed = strip_comment(lines[i], comment_style).trim().to_string();
        if trimmed.starts_with(".macro ") || trimmed.starts_with(".macro\t") {
            // Parse: .macro name [param1[, param2, ...]]
            let rest = trimmed[".macro".len()..].trim();
            let (name, params_str) = match rest.find(|c: char| c == ' ' || c == '\t' || c == ',') {
                Some(pos) => (rest[..pos].trim(), rest[pos..].trim().trim_start_matches(',')),
                None => (rest, ""),
            };
            let params: Vec<String> = if params_str.is_empty() {
                Vec::new()
            } else {
                params_str.split(|c: char| c == ',' || c == ' ' || c == '\t')
                    .map(|s| s.trim().to_string())
                    .filter(|s| !s.is_empty())
                    .collect()
            };
            let mut body = Vec::new();
            let mut depth = 1;
            i += 1;
            while i < lines.len() {
                let inner = strip_comment(lines[i], comment_style).trim().to_string();
                if inner.starts_with(".macro ") || inner.starts_with(".macro\t") {
                    depth += 1;
                } else if inner == ".endm" || inner.starts_with(".endm ") || inner.starts_with(".endm\t") {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                }
                body.push(lines[i].to_string());
                i += 1;
            }
            macros.insert(name.to_string(), MacroDef { params, body });
        } else if trimmed == ".endm" || trimmed.starts_with(".endm ") || trimmed.starts_with(".endm\t") {
            // stray .endm — skip
        } else if !trimmed.is_empty() && !trimmed.starts_with('.') && !trimmed.starts_with('#') {
            let first_word = trimmed.split(|c: char| c == ' ' || c == '\t').next().unwrap_or("");
            let potential_name = first_word.trim_end_matches(':');
            if potential_name != first_word {
                // It's a label, not a macro invocation
                result.push(lines[i].to_string());
            } else if let Some(mac) = macros.get(potential_name) {
                let args_str = trimmed[first_word.len()..].trim();
                let args = split_macro_args(args_str);
                let mut expanded_lines = Vec::new();
                for body_line in &mac.body {
                    let mut expanded = body_line.clone();
                    for (pi, param) in mac.params.iter().enumerate() {
                        let pattern = format!("\\{}", param);
                        let replacement = args.get(pi).map(|s| s.as_str()).unwrap_or("0");
                        expanded = expanded.replace(&pattern, replacement);
                    }
                    expanded_lines.push(expanded);
                }
                let refs: Vec<&str> = expanded_lines.iter().map(|s| s.as_str()).collect();
                let re_expanded = expand_macros_with(&refs, &macros, comment_style)?;
                result.extend(re_expanded);
            } else {
                result.push(lines[i].to_string());
            }
        } else {
            result.push(lines[i].to_string());
        }
        i += 1;
    }
    Ok(result)
}

/// Re-expand macro invocations using already-collected macro definitions.
fn expand_macros_with(
    lines: &[&str],
    macros: &std::collections::HashMap<String, MacroDef>,
    comment_style: &CommentStyle,
) -> Result<Vec<String>, String> {
    let mut result = Vec::new();
    for line in lines {
        let trimmed = strip_comment(line, comment_style).trim().to_string();
        if trimmed.is_empty() || trimmed.starts_with('.') || trimmed.starts_with('#') {
            result.push(line.to_string());
            continue;
        }
        let first_word = trimmed.split(|c: char| c == ' ' || c == '\t').next().unwrap_or("");
        let potential_name = first_word.trim_end_matches(':');
        if potential_name != first_word {
            result.push(line.to_string());
        } else if let Some(mac) = macros.get(potential_name) {
            let args_str = trimmed[first_word.len()..].trim();
            let args = split_macro_args(args_str);
            let mut expanded_lines = Vec::new();
            for body_line in &mac.body {
                let mut expanded = body_line.clone();
                for (pi, param) in mac.params.iter().enumerate() {
                    let pattern = format!("\\{}", param);
                    let replacement = args.get(pi).map(|s| s.as_str()).unwrap_or("0");
                    expanded = expanded.replace(&pattern, replacement);
                }
                expanded_lines.push(expanded);
            }
            let refs: Vec<&str> = expanded_lines.iter().map(|s| s.as_str()).collect();
            let re_expanded = expand_macros_with(&refs, macros, comment_style)?;
            result.extend(re_expanded);
        } else {
            result.push(line.to_string());
        }
    }
    Ok(result)
}

// ── .if / .else / .endif conditional assembly ──────────────────────────

/// Evaluate a simple `.if` condition expression.
///
/// Supports: integer literals, `==`, `!=`, and simple arithmetic via
/// `asm_expr::parse_integer_expr`. Non-zero result is true.
pub fn eval_if_condition(cond: &str) -> bool {
    let cond = cond.trim();
    // Try "A == B"
    if let Some(pos) = cond.find("==") {
        let lhs = cond[..pos].trim();
        let rhs = cond[pos + 2..].trim();
        let l = asm_expr::parse_integer_expr(lhs).unwrap_or(i64::MIN);
        let r = asm_expr::parse_integer_expr(rhs).unwrap_or(i64::MAX);
        return l == r;
    }
    // Try "A != B"
    if let Some(pos) = cond.find("!=") {
        let lhs = cond[..pos].trim();
        let rhs = cond[pos + 2..].trim();
        let l = asm_expr::parse_integer_expr(lhs).unwrap_or(i64::MIN);
        let r = asm_expr::parse_integer_expr(rhs).unwrap_or(i64::MAX);
        return l != r;
    }
    // Simple integer expression: non-zero is true
    asm_expr::parse_integer_expr(cond).unwrap_or(0) != 0
}

// ── Shared data-value helpers ──────────────────────────────────────────

/// Check if a string looks like a GNU numeric label reference (e.g. "2f", "1b", "42f").
pub fn is_numeric_label_ref(s: &str) -> bool {
    if s.len() < 2 {
        return false;
    }
    let last = s.as_bytes()[s.len() - 1];
    if last != b'f' && last != b'F' && last != b'b' && last != b'B' {
        return false;
    }
    s[..s.len() - 1].bytes().all(|b| b.is_ascii_digit())
}

/// Find the position of the `-` operator in a symbol difference expression
/// like `sym_a - sym_b`. Skips position 0 to avoid matching a leading negation.
///
/// Returns `None` if no valid symbol difference operator is found.
pub fn find_symbol_diff_minus(expr: &str) -> Option<usize> {
    let bytes = expr.as_bytes();
    let len = bytes.len();
    let mut i = 1;
    while i < len {
        if bytes[i] == b'-' {
            let left_char = bytes[i - 1];
            let left_ok = left_char.is_ascii_alphanumeric()
                || left_char == b'_'
                || left_char == b'.'
                || left_char == b' '
                || left_char == b')';
            let right_start = expr[i + 1..].trim_start();
            if !right_start.is_empty() {
                let right_char = right_start.as_bytes()[0];
                let right_ok = right_char.is_ascii_alphabetic()
                    || right_char == b'_'
                    || right_char == b'.'
                    || right_char.is_ascii_digit()
                    || right_char == b'(';
                if left_ok && right_ok {
                    return Some(i);
                }
            }
        }
        i += 1;
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_strip_c_comments() {
        assert_eq!(strip_c_comments("a /* b */ c"), "a  c");
        assert_eq!(strip_c_comments("a /* b\nc */ d"), "a \nd");
    }

    #[test]
    fn test_strip_comment_hash() {
        let style = CommentStyle::Hash;
        assert_eq!(strip_comment("movq %rax, %rbx # comment", &style), "movq %rax, %rbx ");
        assert_eq!(strip_comment(".asciz \"a#b\"", &style), ".asciz \"a#b\"");
    }

    #[test]
    fn test_strip_comment_slash_slash_and_at() {
        let style = CommentStyle::SlashSlashAndAt;
        assert_eq!(strip_comment("mov x0, x1 // comment", &style), "mov x0, x1 ");
        assert_eq!(strip_comment("mov x0, x1 @ comment", &style), "mov x0, x1 ");
        assert_eq!(strip_comment(".type foo, @function", &style), ".type foo, @function");
    }

    #[test]
    fn test_split_on_semicolons() {
        let parts = split_on_semicolons("a; b; c");
        assert_eq!(parts, vec!["a", " b", " c"]);
        let parts = split_on_semicolons(".asciz \"a;b\"; nop");
        assert_eq!(parts, vec![".asciz \"a;b\"", " nop"]);
    }

    #[test]
    fn test_eval_if_condition() {
        assert!(eval_if_condition("1"));
        assert!(!eval_if_condition("0"));
        assert!(eval_if_condition("1 == 1"));
        assert!(!eval_if_condition("1 == 2"));
        assert!(eval_if_condition("1 != 2"));
    }

    #[test]
    fn test_is_numeric_label_ref() {
        assert!(is_numeric_label_ref("1f"));
        assert!(is_numeric_label_ref("42b"));
        assert!(!is_numeric_label_ref("f"));
        assert!(!is_numeric_label_ref("abc"));
    }

    #[test]
    fn test_find_symbol_diff_minus() {
        assert_eq!(find_symbol_diff_minus("a - b"), Some(2));
        assert_eq!(find_symbol_diff_minus("-5"), None);
        assert_eq!(find_symbol_diff_minus(".Lfoo-.Lbar"), Some(5));
    }

    #[test]
    fn test_split_macro_args() {
        assert_eq!(split_macro_args("a, b, c"), vec!["a", "b", "c"]);
        assert_eq!(split_macro_args("0(a1), x, y"), vec!["0(a1)", "x", "y"]);
        assert_eq!(split_macro_args(""), Vec::<String>::new());
    }
}
