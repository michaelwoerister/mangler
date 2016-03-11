// The MIT License (MIT)
//
// Copyright (c) 2016 Michael Woerister
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
// THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#![allow(unused)]

use std::str;
use std::char;

type ParseResult = Result<(), String>;

macro_rules! check_eof {
    ($label:expr, $text:expr, $pos:expr) => {
        if *$pos >= $text.len() {
            return Err(format!("{}: unexpected EOF", $label));
        }
    };

    ($label:expr, $text:expr, $pos:expr, $len:expr) => {
        if *$pos + $len > $text.len() {
            return Err(format!("{}: unexpected EOF", $label));
        }
    };
}

fn parse_name(text: &[u8], pos: &mut usize, output: &mut String) -> ParseResult {
    check_eof!("parse_name", text, pos);

    if text[*pos] == b'N' {
        parse_nested_name(text, pos, output)
    } else {
        try!(parse_unqualified_name(text, pos, output));
        if *pos < text.len() && text[*pos] == b'I' {
            parse_template_args(text, pos, output)
        } else {
            Ok(())
        }
    }
}

fn parse_nested_name(text: &[u8], pos: &mut usize, output: &mut String) -> ParseResult {
    check_eof!("parse_nested_name", text, pos);

    if text[*pos] != b'N' {
        return Err(format!("parse_nested_name: Expected 'N', found '{}'",
                           text[*pos] as char));
    } else {
        *pos += 1;
        check_eof!("parse_nested_name", text, pos);
    }

    while text[*pos] != b'E' {
        check_eof!("parse_nested_name", text, pos);

        try!(parse_unqualified_name(text, pos, output));

        if text[*pos] == b'I' {
            try!(parse_template_args(text, pos, output));
        }

        if text[*pos] != b'E' {
            output.push_str("::");
        }
    }

    // Eat the 'E'
    *pos += 1;

    Ok(())
}

fn parse_template_args(text: &[u8], pos: &mut usize, output: &mut String) -> ParseResult {
    check_eof!("parse_template_args", text, pos);

    if text[*pos] != b'I' {
        return Err(format!("parse_template_args: Expected 'I', found '{}'",
                           text[*pos] as char));
    }

    *pos += 1;
    check_eof!("parse_template_args", text, pos);
    output.push_str("<");

    while text[*pos] != b'E' {
        try!(parse_template_arg(text, pos, output));

        check_eof!("parse_template_args", text, pos);

        if text[*pos] != b'E' {
            output.push_str(",");
        }
    }

    // Eat the 'E'
    *pos += 1;
    output.push_str(">");

    Ok(())
}

fn parse_template_arg(text: &[u8], pos: &mut usize, output: &mut String) -> ParseResult {
    check_eof!("parse_template_arg", text, pos);

    match text[*pos] {
        b'X' => parse_expression(text, pos, output),
        b'L' => parse_expr_primary(text, pos, output),
        _ => parse_type(text, pos, output),
    }
}

fn parse_type(text: &[u8], pos: &mut usize, output: &mut String) -> ParseResult {
    check_eof!("parse_type", text, pos);

    if text[*pos] == b'N' || is_digit(text[*pos]) {
        parse_name(text, pos, output)
    } else {
        parse_builtin_type(text, pos, output)
    }
}

fn parse_builtin_type(text: &[u8], pos: &mut usize, output: &mut String) -> ParseResult {
    Ok(())
}

fn parse_expression(text: &[u8], pos: &mut usize, output: &mut String) -> ParseResult {
    check_eof!("parse_expression", text, pos);

    if text[*pos] != b'X' {
        return Err(format!("parse_expression: Expected 'X', found '{}'",
                           text[*pos] as char));
    } else {
        *pos += 1;
        check_eof!("parse_expression", text, pos);
    }

    check_eof!("parse_expression", text, pos);

    if text[*pos] == b'L' {
        parse_expr_primary(text, pos, output)
    } else {
        if let Some((label, op_kind)) = try_parse_operator_name(text, pos) {
            match op_kind {
                OpKind::Prefix => {
                    output.push_str(label);
                    parse_expression(text, pos, output)
                }
                OpKind::Infix => {
                    try!(parse_expression(text, pos, output));
                    output.push_str(label);
                    parse_expression(text, pos, output)
                }
                OpKind::Postfix => {
                    try!(parse_expression(text, pos, output));
                    output.push_str(label);
                    Ok(())
                }
            }
        } else {
            Err("parse_expression: Not an expression!".to_owned())
        }
    }
}

fn parse_expr_primary(text: &[u8], pos: &mut usize, output: &mut String) -> ParseResult {
    Ok(())
}

fn parse_unqualified_name(text: &[u8], pos: &mut usize, output: &mut String) -> ParseResult {
    check_eof!("parse_unqualified_name", text, pos);

    if is_digit(text[*pos]) {
        return parse_source_name(text, pos, output);
    }

    Ok(())
}

fn parse_source_name(text: &[u8], pos: &mut usize, output: &mut String) -> ParseResult {
    let name_len = try!(parse_int(text, pos));

    if *pos + name_len > text.len() {
        return Err(format!("parse_source_name: unexpected EOF"));
    }

    let source_name = &text[*pos..(*pos + name_len)];
    output.push_str(str::from_utf8(source_name).unwrap());
    *pos += name_len;
    Ok(())
}

fn parse_int(text: &[u8], pos: &mut usize) -> Result<usize, String> {
    check_eof!("parse_int", text, pos);

    if !is_digit(text[*pos]) {
        return Err(format!("parse_int: unexpected character '{}'",
                           char::from_u32(text[*pos] as u32).unwrap()));
    }

    let mut value = 0;
    while *pos < text.len() && is_digit(text[*pos]) {
        value = value * 10 + (text[*pos] - b'0') as usize;
        *pos += 1;
    }

    Ok(value)
}

enum OpKind {
    Infix,
    Prefix,
    Postfix,
}

fn try_parse_operator_name(text: &[u8], pos: &mut usize) -> Option<(&'static str, OpKind)> {
    if *pos >= text.len() {
        return None;
    }

    let (label, op_kind) = match &text[*pos..*pos + 2] {
        b"ps" => ("+", OpKind::Prefix), // + (unary)
        b"ng" => ("-", OpKind::Prefix), // - (unary)
        b"ad" => ("&", OpKind::Prefix), // & (unary)
        b"de" => ("*", OpKind::Prefix), // * (unary)
        b"co" => ("~", OpKind::Prefix), // ~
        b"pl" => ("+", OpKind::Infix), // +
        b"mi" => ("-", OpKind::Infix), // -
        b"ml" => ("*", OpKind::Infix), // *
        b"dv" => ("/", OpKind::Infix), // /
        b"rm" => ("%", OpKind::Infix), // %
        b"an" => ("&", OpKind::Infix), // &
        b"or" => ("|", OpKind::Infix), // |
        b"eo" => ("^", OpKind::Infix), // ^
        b"aS" => ("=", OpKind::Infix), // =
        b"pL" => ("+=", OpKind::Infix), // +=
        b"mI" => ("-=", OpKind::Infix), // -=
        b"mL" => ("*=", OpKind::Infix), // *=
        b"dV" => ("/=", OpKind::Infix), // /=
        b"rM" => ("%=", OpKind::Infix), // %=
        b"aN" => ("&=", OpKind::Infix), // &=
        b"oR" => ("|=", OpKind::Infix), // |=
        b"eO" => ("^=", OpKind::Infix), // ^=
        b"ls" => ("<<", OpKind::Infix), // <<
        b"rs" => (">>", OpKind::Infix), // >>
        b"lS" => ("<<=", OpKind::Infix), // <<=
        b"rS" => (">>=", OpKind::Infix), // >>=
        b"eq" => ("==", OpKind::Infix), // ==
        b"ne" => ("!=", OpKind::Infix), // !=
        b"lt" => ("<", OpKind::Infix), // <
        b"gt" => (">", OpKind::Infix), // >
        b"le" => ("<=", OpKind::Infix), // <=
        b"ge" => (">=", OpKind::Infix), // >=
        b"nt" => ("!", OpKind::Prefix), // !
        b"aa" => ("&&", OpKind::Infix), // &&
        b"oo" => ("&&", OpKind::Infix), // ||
        b"pp" => ("++", OpKind::Postfix), // ++ (postfix in <expression> context)
        b"mm" => ("--", OpKind::Postfix), // -- (postfix in <expression> context)
        b"cm" => (",", OpKind::Infix), // ,
        b"pm" => ("->*", OpKind::Infix), // ->*
        b"pt" => ("->", OpKind::Infix), // ->
        _ => return None,
    };

    *pos += 2;
    Some((label, op_kind))
}

fn is_digit(c: u8) -> bool {
    c >= b'0' && c <= b'9'
}


#[cfg(test)]
mod test {
    use super::{parse_int, parse_source_name, parse_nested_name};

    #[test]
    fn test_parse_int() {

        macro_rules! assert_parse_int {
            ($text:expr, $result:expr) => ({
                let mut pos = 0;
                assert_eq!(parse_int($text, &mut pos), $result);
            })
        }

        assert_parse_int!(b"", Err("parse_int: unexpected EOF".to_owned()));
        assert_parse_int!(b" ", Err("parse_int: unexpected character ' '".to_owned()));
        assert_parse_int!(b"a", Err("parse_int: unexpected character 'a'".to_owned()));
        assert_parse_int!(b".", Err("parse_int: unexpected character '.'".to_owned()));
        assert_parse_int!(b"-", Err("parse_int: unexpected character '-'".to_owned()));

        assert_parse_int!(b"1", Ok(1));
        assert_parse_int!(b"2", Ok(2));
        assert_parse_int!(b"3", Ok(3));
        assert_parse_int!(b"4", Ok(4));
        assert_parse_int!(b"5", Ok(5));
        assert_parse_int!(b"6", Ok(6));
        assert_parse_int!(b"7", Ok(7));
        assert_parse_int!(b"8", Ok(8));
        assert_parse_int!(b"9", Ok(9));

        assert_parse_int!(b"1a", Ok(1));
        assert_parse_int!(b"2 ", Ok(2));
        assert_parse_int!(b"3@", Ok(3));
        assert_parse_int!(b"4q", Ok(4));
        assert_parse_int!(b"5w", Ok(5));
        assert_parse_int!(b"6e", Ok(6));
        assert_parse_int!(b"7!", Ok(7));
        assert_parse_int!(b"8_", Ok(8));
        assert_parse_int!(b"9+", Ok(9));

        assert_parse_int!(b"12", Ok(12));
        assert_parse_int!(b"123", Ok(123));
        assert_parse_int!(b"1234", Ok(1234));
        assert_parse_int!(b"12345", Ok(12345));
        assert_parse_int!(b"123456", Ok(123456));

        assert_parse_int!(b"12 ", Ok(12));
        assert_parse_int!(b"123 ", Ok(123));
        assert_parse_int!(b"1234 ", Ok(1234));
        assert_parse_int!(b"12345 ", Ok(12345));
        assert_parse_int!(b"123456 ", Ok(123456));
    }

    macro_rules! assert_parse {
        ($text:expr, $parse_fn:ident, $expected_output:expr, $expected_result:expr) => ({
            let mut pos = 0;
            let mut output = String::new();
            let result = $parse_fn($text, &mut pos, &mut output);
            assert_eq!(&output[..], $expected_output);
            assert_eq!(result, $expected_result);
        })
    }

    #[test]
    fn test_parse_source_name() {

        assert_parse!(b"0", parse_source_name, "", Ok(()));
        assert_parse!(b"1a", parse_source_name, "a", Ok(()));
        assert_parse!(b"2ab", parse_source_name, "ab", Ok(()));
        assert_parse!(b"3abc", parse_source_name, "abc", Ok(()));
        assert_parse!(b"4abcd", parse_source_name, "abcd", Ok(()));
        assert_parse!(b"5abcde", parse_source_name, "abcde", Ok(()));
        assert_parse!(b"6abcdef", parse_source_name, "abcdef", Ok(()));

        assert_parse!(b"5    a", parse_source_name, "    a", Ok(()));

        assert_parse!(b"11abcde abcde", parse_source_name, "abcde abcde", Ok(()));

        assert_parse!(b"1",
                      parse_source_name,
                      "",
                      Err(format!("parse_source_name: unexpected EOF")));
        assert_parse!(b"2a",
                      parse_source_name,
                      "",
                      Err(format!("parse_source_name: unexpected EOF")));
        assert_parse!(b"0a", parse_source_name, "", Ok(()));
    }

    #[test]
    fn test_parse_nested_name() {
        assert_parse!(b"NE", parse_nested_name, "", Ok(()));
        assert_parse!(b"N3abcE", parse_nested_name, "abc", Ok(()));
        assert_parse!(b"N3abc4defgE", parse_nested_name, "abc::defg", Ok(()));
        assert_parse!(b"N3abcI3xyzE4defgE",
                      parse_nested_name,
                      "abc<xyz>::defg",
                      Ok(()));
    }
}
