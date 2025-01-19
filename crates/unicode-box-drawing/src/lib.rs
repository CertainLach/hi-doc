#![no_std]

use core::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Width {
	None,
	Single,
	Double,
}
impl Width {
	fn inc(self) -> Self {
		match self {
			Width::None => Self::Single,
			Width::Single => Self::Double,
			Width::Double => Self::Double,
		}
	}
	pub fn is_none(&self) -> bool {
		matches!(self, Self::None)
	}
	const fn bits(&self) -> u8 {
		match self {
			Width::None => 0,
			Width::Single => 1,
			Width::Double => 2,
		}
	}
	const fn from_bits(v: u8) -> Option<Self> {
		Some(match v {
			0 => Self::None,
			1 => Self::Single,
			2 => Self::Double,
			_ => return None,
		})
	}
}

const BOX_CHAR_BYTES: usize = 3;
const BOX_CHARS_STR: &str = {
	let c = "╴╸╷┐┑╻┒┓╶─╾┌┬┭┎┰┱╺╼━┍┮┯┏┲┳╵┘┙│┤┥╽┧┪└┴┵├┼┽┟╁╅┕┶┷┝┾┿┢╆╈╹┚┛╿┦┩┃┨┫┖┸┹┞╀╃┠╂╉┗┺┻┡╄╇┣╊╋";
	assert!(c.len() == 80 * BOX_CHAR_BYTES);
	c
};
const BOX_CHARS: &[u8] = BOX_CHARS_STR.as_bytes();
const fn corner_round(c: char) -> char {
	match c {
		'┌' => '╭',
		'└' => '╰',
		'┐' => '╮',
		'┘' => '╯',
		_ => c,
	}
}
fn unround_corner(c: char) -> char {
	match c {
		'╭' => '┌',
		'╰' => '└',
		'╮' => '┐',
		'╯' => '┘',
		_ => c,
	}
}

const fn div_rem(a: u8, b: u8) -> (u8, u8) {
	(a / b, a % b)
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
struct Raw {
	top: Width,
	right: Width,
	bottom: Width,
	left: Width,
}
impl Raw {
	const fn encode(&self) -> u8 {
		self.top.bits() * 27 + self.right.bits() * 9 + self.bottom.bits() * 3 + self.left.bits()
	}
	const fn decode(v: u8) -> Option<Self> {
		let (v, left) = div_rem(v, 3);
		let (v, bottom) = div_rem(v, 3);
		let (v, right) = div_rem(v, 3);
		let (v, top) = div_rem(v, 3);
		if v != 0 {
			return None;
		}
		Some(Self {
			top: Width::from_bits(top).expect("valid"),
			right: Width::from_bits(right).expect("valid"),
			bottom: Width::from_bits(bottom).expect("valid"),
			left: Width::from_bits(left).expect("valid"),
		})
	}
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct BoxCharacter(u8);
impl fmt::Debug for BoxCharacter {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		struct Inner(Raw);
		impl fmt::Debug for Inner {
			fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
				fn fmt(f: &mut fmt::Formatter<'_>, width: Width, c: char) -> fmt::Result {
					match width {
						Width::None => Ok(()),
						Width::Single => write!(f, "{c}"),
						Width::Double => write!(f, "{c}{c}"),
					}
				}
				fmt(f, self.0.top, 't')?;
				fmt(f, self.0.right, 'r')?;
				fmt(f, self.0.bottom, 'b')?;
				fmt(f, self.0.left, 'l')
			}
		}
		let mut d = f.debug_tuple("BoxCharacter");
		d.field(&Inner(self.raw()));
		d.finish()
	}
}
impl fmt::Display for BoxCharacter {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.char())
	}
}

impl BoxCharacter {
	pub fn top(self) -> Width {
		self.raw().top
	}
	pub fn right(self) -> Width {
		self.raw().right
	}
	pub fn bottom(self) -> Width {
		self.raw().bottom
	}
	pub fn left(self) -> Width {
		self.raw().left
	}
	pub fn new(top: Width, right: Width, bottom: Width, left: Width) -> Self {
		Self::from_raw(Raw {
			top,
			right,
			bottom,
			left,
		})
	}
	const fn raw(self) -> Raw {
		Raw::decode(self.0).expect("BoxData items are properly encoded")
	}
	const fn from_raw(r: Raw) -> Self {
		Self(r.encode())
	}
	pub fn with_top(self) -> Self {
		let r = self.raw();
		Self::from_raw(Raw {
			top: r.top.inc(),
			..r
		})
	}
	pub fn with_right(self) -> Self {
		let r = self.raw();
		Self::from_raw(Raw {
			right: r.right.inc(),
			..r
		})
	}
	pub fn with_bottom(self) -> Self {
		let r = self.raw();
		Self::from_raw(Raw {
			bottom: r.bottom.inc(),
			..r
		})
	}
	pub fn with_left(self) -> Self {
		let r = self.raw();
		Self::from_raw(Raw {
			left: r.left.inc(),
			..r
		})
	}
	pub const fn mirror_vertical(self) -> Self {
		let r = self.raw();
		Self::from_raw(Raw {
			top: r.bottom,
			bottom: r.top,
			..r
		})
	}
	pub const fn mirror_horizontal(self) -> Self {
		let r = self.raw();
		Self::from_raw(Raw {
			left: r.right,
			right: r.left,
			..r
		})
	}
	pub const fn char(&self) -> char {
		let Some(i) = self.0.checked_sub(1) else {
			return ' ';
		};
		let i = i as usize;

		let a = BOX_CHARS[i * BOX_CHAR_BYTES] as u32;
		let b = BOX_CHARS[i * BOX_CHAR_BYTES + 1] as u32;
		let c = BOX_CHARS[i * BOX_CHAR_BYTES + 2] as u32;

		// .chars() iterator is not const, and for some reason there is no longer char::decode_utf8
		// function in stdlib, thus here I inlined utf-8 decoding of 3 bytes...
		let c = ((a & 0x0f) << 12) | ((b & 0x3f) << 6) | (c & 0x3f);
		char::from_u32(c).expect("char")
	}
	pub const fn char_round(&self) -> char {
		corner_round(self.char())
	}
	pub fn decode_char(v: char) -> Option<Self> {
		if v == ' ' {
			return Some(Self(0));
		};
		let v = unround_corner(v);
		let id = BOX_CHARS_STR.find(v)? / 3;
		Some(Self::from_raw(
			Raw::decode(id as u8 + 1).expect("valid idx"),
		))
	}
	pub const fn from_str(v: &[u8]) -> Self {
		const fn c(v: &[u8], f: usize, b: u8) -> (u8, usize) {
			let mut o = 0;
			if v.len() - f >= 2 {
				if v[f] == b {
					o += 1;
				}
				if v[f + 1] == b {
					o += 1;
				}
			} else if v.len() - f >= 1 && v[f] == b {
				o += 1;
			}
			(o, f + o as usize)
		}
		let (top, f) = c(v, 0, b't');
		let (right, f) = c(v, f, b'r');
		let (bottom, f) = c(v, f, b'b');
		let (left, f) = c(v, f, b'l');
		assert!(f == v.len(), "invalid box def");
		Self::from_raw(Raw {
			top: Width::from_bits(top).expect("v"),
			right: Width::from_bits(right).expect("v"),
			bottom: Width::from_bits(bottom).expect("v"),
			left: Width::from_bits(left).expect("v"),
		})
	}
}

#[macro_export]
macro_rules! bc {
	($i:ident) => {
		const { $crate::BoxCharacter::from_str(stringify!($i).as_bytes()) }
	};
}

#[cfg(test)]
mod tests {
	use crate::{BoxCharacter, Raw, Width};

	#[test]
	fn box_encoding() {
		let w = [Width::None, Width::Single, Width::Double];
		for top in w {
			for right in w {
				for bottom in w {
					for left in w {
						let e = BoxCharacter::from_raw(Raw {
							top,
							right,
							bottom,
							left,
						});
						let c = e.char();
						let c = BoxCharacter::decode_char(c).expect("from encoded");
						assert_eq!(e, c);
					}
				}
			}
		}
	}

	#[test]
	fn smoke() {
		let c = bc!(ttrb);
		assert_eq!(c.char(), '┞')
	}
}
