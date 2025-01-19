//! Based on https://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
use std::collections::BTreeMap;

use range_map::{Range, RangeSet};

// TODO: I would like to make it configurable like vim's listchars...
// but I need to check that it has the same display width.
const TAB_LISTCHAR: char = ' ';

thread_local! {
	static NONSTANDARD_WIDTH:RangeSet<u32> =vec![
		(0, 32),
		(0x7f, 0xa0),

		// Combining
		(0x0300, 0x036F),
		(0x0483, 0x0486),
		(0x0488, 0x0489),
		(0x0591, 0x05BD),
		(0x05BF, 0x05BF),
		(0x05C1, 0x05C2),
		(0x05C4, 0x05C5),
		(0x05C7, 0x05C7),
		(0x0600, 0x0603),
		(0x0610, 0x0615),
		(0x064B, 0x065E),
		(0x0670, 0x0670),
		(0x06D6, 0x06E4),
		(0x06E7, 0x06E8),
		(0x06EA, 0x06ED),
		(0x070F, 0x070F),
		(0x0711, 0x0711),
		(0x0730, 0x074A),
		(0x07A6, 0x07B0),
		(0x07EB, 0x07F3),
		(0x0901, 0x0902),
		(0x093C, 0x093C),
		(0x0941, 0x0948),
		(0x094D, 0x094D),
		(0x0951, 0x0954),
		(0x0962, 0x0963),
		(0x0981, 0x0981),
		(0x09BC, 0x09BC),
		(0x09C1, 0x09C4),
		(0x09CD, 0x09CD),
		(0x09E2, 0x09E3),
		(0x0A01, 0x0A02),
		(0x0A3C, 0x0A3C),
		(0x0A41, 0x0A42),
		(0x0A47, 0x0A48),
		(0x0A4B, 0x0A4D),
		(0x0A70, 0x0A71),
		(0x0A81, 0x0A82),
		(0x0ABC, 0x0ABC),
		(0x0AC1, 0x0AC5),
		(0x0AC7, 0x0AC8),
		(0x0ACD, 0x0ACD),
		(0x0AE2, 0x0AE3),
		(0x0B01, 0x0B01),
		(0x0B3C, 0x0B3C),
		(0x0B3F, 0x0B3F),
		(0x0B41, 0x0B43),
		(0x0B4D, 0x0B4D),
		(0x0B56, 0x0B56),
		(0x0B82, 0x0B82),
		(0x0BC0, 0x0BC0),
		(0x0BCD, 0x0BCD),
		(0x0C3E, 0x0C40),
		(0x0C46, 0x0C48),
		(0x0C4A, 0x0C4D),
		(0x0C55, 0x0C56),
		(0x0CBC, 0x0CBC),
		(0x0CBF, 0x0CBF),
		(0x0CC6, 0x0CC6),
		(0x0CCC, 0x0CCD),
		(0x0CE2, 0x0CE3),
		(0x0D41, 0x0D43),
		(0x0D4D, 0x0D4D),
		(0x0DCA, 0x0DCA),
		(0x0DD2, 0x0DD4),
		(0x0DD6, 0x0DD6),
		(0x0E31, 0x0E31),
		(0x0E34, 0x0E3A),
		(0x0E47, 0x0E4E),
		(0x0EB1, 0x0EB1),
		(0x0EB4, 0x0EB9),
		(0x0EBB, 0x0EBC),
		(0x0EC8, 0x0ECD),
		(0x0F18, 0x0F19),
		(0x0F35, 0x0F35),
		(0x0F37, 0x0F37),
		(0x0F39, 0x0F39),
		(0x0F71, 0x0F7E),
		(0x0F80, 0x0F84),
		(0x0F86, 0x0F87),
		(0x0F90, 0x0F97),
		(0x0F99, 0x0FBC),
		(0x0FC6, 0x0FC6),
		(0x102D, 0x1030),
		(0x1032, 0x1032),
		(0x1036, 0x1037),
		(0x1039, 0x1039),
		(0x1058, 0x1059),
		(0x1160, 0x11FF),
		(0x135F, 0x135F),
		(0x1712, 0x1714),
		(0x1732, 0x1734),
		(0x1752, 0x1753),
		(0x1772, 0x1773),
		(0x17B4, 0x17B5),
		(0x17B7, 0x17BD),
		(0x17C6, 0x17C6),
		(0x17C9, 0x17D3),
		(0x17DD, 0x17DD),
		(0x180B, 0x180D),
		(0x18A9, 0x18A9),
		(0x1920, 0x1922),
		(0x1927, 0x1928),
		(0x1932, 0x1932),
		(0x1939, 0x193B),
		(0x1A17, 0x1A18),
		(0x1B00, 0x1B03),
		(0x1B34, 0x1B34),
		(0x1B36, 0x1B3A),
		(0x1B3C, 0x1B3C),
		(0x1B42, 0x1B42),
		(0x1B6B, 0x1B73),
		(0x1DC0, 0x1DCA),
		(0x1DFE, 0x1DFF),
		(0x200B, 0x200F),
		(0x202A, 0x202E),
		(0x2060, 0x2063),
		(0x206A, 0x206F),
		(0x20D0, 0x20EF),
		(0x302A, 0x302F),
		(0x3099, 0x309A),
		(0xA806, 0xA806),
		(0xA80B, 0xA80B),
		(0xA825, 0xA826),
		(0xFB1E, 0xFB1E),
		(0xFE00, 0xFE0F),
		(0xFE20, 0xFE23),
		(0xFEFF, 0xFEFF),
		(0xFFF9, 0xFFFB),
		(0x10A01, 0x10A03),
		(0x10A05, 0x10A06),
		(0x10A0C, 0x10A0F),
		(0x10A38, 0x10A3A),
		(0x10A3F, 0x10A3F),
		(0x1D167, 0x1D169),
		(0x1D173, 0x1D182),
		(0x1D185, 0x1D18B),
		(0x1D1AA, 0x1D1AD),
		(0x1D242, 0x1D244),
		(0xE0001, 0xE0001),
		(0xE0020, 0xE007F),
		(0xE0100, 0xE01EF),
	]
	.into_iter()
	.map(|(a, b)| Range::new(a, b))
	.collect::<RangeSet<u32>>();
}

/// Process non-1-width characters, which require replacing fragments of source text with 1-width characters, to not break source formatting
/// I.e replace tabs with specified amount of spaces, and special characters with their visual equivalents
///
/// Characters, which should be displayed as-is, but whose occupy more that one column, will be kept as is, and offsets will be fixed later
///
/// Returns fixups to convert byte offsets to char offsets using [`apply_fixups`]
pub fn fixup_byte_to_char(
	mut text: &str,
	tab_width: usize,
) -> (String, BTreeMap<usize, isize>, Vec<usize>) {
	let mut fixups = BTreeMap::new();
	let mut decorations = Vec::new();
	let mut out = String::new();
	let mut fixup = |byte_offset: usize, source_bytes: usize, output_chars: usize| {
		let entry = fixups.entry(byte_offset).or_default();
		*entry -= source_bytes as isize;
		*entry += output_chars as isize;
	};

	let mut total_byte_offset = 0;
	let mut total_char_offset = 0;
	let mut display_offset_since_newline = 0;
	loop {
		let mut current_segment_offset = 0;
		for char in text.chars().take_while(|char| {
			*char == '\n' || *char == ' ' || !NONSTANDARD_WIDTH.with(|r| r.contains(*char as u32))
		}) {
			let char_bytes = char.len_utf8();
			fixup(total_byte_offset, char_bytes, 1);
			current_segment_offset += char_bytes;
			total_byte_offset += char_bytes;
			total_char_offset += 1;
			if char == '\n' {
				display_offset_since_newline = 0;
			} else {
				display_offset_since_newline += if is_fullwidth(char) { 2 } else { 1 };
			}
		}
		out.push_str(&text[..current_segment_offset]);
		text = &text[current_segment_offset..];

		if text.is_empty() {
			break;
		}

		let char = text.chars().next().expect("not empty");
		let bytes = text.as_bytes();
		match char {
			// Tab character aligns next symbol to next multiply of tab_width display characters
			'\t' => {
				let mut size = 1;
				display_offset_since_newline += 1;
				while display_offset_since_newline % tab_width != 0 {
					display_offset_since_newline += 1;
					size += 1;
				}
				for i in 0..size {
					if i == 0 {
						decorations.push(total_char_offset);
						out.push(TAB_LISTCHAR);
					} else {
						out.push(' ');
					}
				}
				text = &text[1..];
				fixup(total_byte_offset, 1, size);
				total_byte_offset += 1;
				total_char_offset += size;
			}
			// TODO: Would be cool to display multiple spaces at the start of the line
			// as tab
			'\r' if bytes[1] == b'\n' => {
				out.push('\n');
				text = &text[2..];
				fixup(total_byte_offset, 2, 1);
				total_byte_offset += 2;
				total_char_offset += 1;

				display_offset_since_newline = 0;
			}
			'\r' => {
				out.push_str("<CR>");
				text = &text[1..];
				fixup(total_byte_offset, 1, 4);
				display_offset_since_newline += 4;
				total_byte_offset += 1;
				total_char_offset += 4;
			}
			'\0' => {
				out.push_str("<NUL>");
				text = &text[1..];
				fixup(total_byte_offset, 1, 5);
				display_offset_since_newline += 5;
				total_byte_offset += 1;
				total_char_offset += 5;
			}
			c => {
				let str = format!("<U+{:0>4X}>", c as u32);
				out.push_str(&str);
				let size = c.len_utf8();
				fixup(total_byte_offset, size, str.len());
				display_offset_since_newline += str.len();
				text = &text[size..];
				total_byte_offset += size;
			}
		}
	}

	(out, fixups, decorations)
}

fn is_fullwidth(c: char) -> bool {
	let ucs = c as u32;
	ucs >= 0x1100
		&& (ucs <= 0x115f ||
			// Hangul Jamo init. consonants
			ucs == 0x2329 || ucs == 0x232a ||
			// CJK ... Yi
			((0x2e80..=0xa4cf).contains(&ucs) && ucs != 0x303f) ||
			// Hangul Syllables
			(0xac00..=0xd7a3).contains(&ucs) ||
			// CJK Compatibility Ideographs
			(0xf900..=0xfaff).contains(&ucs) ||
			// Vertical forms
			(0xfe10..=0xfe19).contains(&ucs) ||
			// CJK Compatibility Forms
			(0xfe30..=0xfe6f).contains(&ucs) ||
			(0xff00..=0xff60).contains(&ucs) ||
			// Fullwidth Forms
			(0xffe0..=0xffe6).contains(&ucs) ||
			(0x20000..=0x2fffd).contains(&ucs) ||
			(0x30000..=0x3fffd).contains(&ucs))
}

/// Some of the unicode codepoints require two columns to display, this function generates fixup to adjust
/// char position to display
pub fn fixup_char_to_display(text: impl Iterator<Item = char>) -> BTreeMap<usize, isize> {
	let mut fixups = BTreeMap::new();
	let mut fixup = |byte_offset: usize, chars: usize, display_chars: usize| {
		let entry = fixups.entry(byte_offset).or_default();
		*entry -= chars as isize;
		*entry += display_chars as isize;
	};
	for (char_index, char) in text.enumerate() {
		if is_fullwidth(char) {
			fixup(char_index, 1, 2)
		}
	}
	fixups
}

pub fn apply_fixup(offset: &mut usize, fixups: &BTreeMap<usize, isize>) {
	for (_, v) in fixups.range(..*offset) {
		if *v >= 0 {
			*offset += *v as usize;
		} else {
			*offset -= (-*v) as usize;
		}
	}
}
#[cfg(test)]
pub fn apply_fixups(offsets: &mut [usize], fixups: &BTreeMap<usize, isize>) {
	for offset in offsets.iter_mut() {
		apply_fixup(offset, fixups)
	}
}

#[cfg(test)]
mod replacements {
	use super::*;

	#[test]
	fn cr() {
		let (out, map, _) = fixup_byte_to_char("\rhello", 4);
		let mut offsets = [0, 1];
		apply_fixups(&mut offsets, &map);
		assert_eq!(out, "<CR>hello");
		assert_eq!(offsets, [0, 4]);
	}

	#[test]
	fn tab() {
		let (out, map, dec) = fixup_byte_to_char("\t\thello", 2);
		let mut offsets = [0, 1, 2];
		apply_fixups(&mut offsets, &map);
		assert_eq!(out, "    hello");
		assert_eq!(offsets, [0, 2, 4]);
		assert_eq!(dec, [0, 2]);
	}

	#[test]
	fn combining() {
		let (out, map, _) = fixup_byte_to_char("\u{0610}", 4);
		let mut offsets = [0, 2];
		apply_fixups(&mut offsets, &map);
		assert_eq!(out, "<U+0610>");
		assert_eq!(offsets, [0, 8]);
	}

	#[test]
	fn combining_emoji() {
		let (out, map, _) = fixup_byte_to_char("👨‍👨‍👧‍👧", 4);
		let mut offsets = [0, 4, 7, 11, 14];
		apply_fixups(&mut offsets, &map);
		assert_eq!(out, "👨<U+200D>👨<U+200D>👧<U+200D>👧");
		assert_eq!(offsets, [0, 1, 9, 10, 18])
	}
}
