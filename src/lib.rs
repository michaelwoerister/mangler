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

    if *pos >= text.len() {
        return Err("parse_int: unexpected EOF".to_owned());
    }

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

fn is_digit(c: u8) -> bool {
    c >= b'0' && c <= b'9'
}

#[cfg(test)]
mod test {
    use super::{parse_int, parse_source_name};

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
}
