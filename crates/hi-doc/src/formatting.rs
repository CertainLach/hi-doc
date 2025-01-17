use crate::segment::{Meta, MetaApply, SegmentBuffer};

pub type Text = SegmentBuffer<Formatting>;

#[derive(Default, Clone, PartialEq, Debug)]
pub struct Formatting {
	pub color: Option<u32>,
	pub bg_color: Option<u32>,
	pub bold: bool,
	pub underline: bool,
	pub decoration: bool,
}
impl Meta for Formatting {
	fn try_merge(&mut self, other: &Self) -> bool {
		self == other
	}
}

impl MetaApply<Formatting> for Formatting {
	fn apply(&mut self, change: &Formatting) {
		if let Some(color) = change.color {
			self.color = Some(color);
		}
		if let Some(bg_color) = change.bg_color {
			self.bg_color = Some(bg_color);
		}
		if change.bold {
			self.bold = true;
		}
		if change.underline {
			self.underline = true;
		}
	}
}

pub struct AddColorToUncolored(pub u32);
impl MetaApply<AddColorToUncolored> for Formatting {
	fn apply(&mut self, change: &AddColorToUncolored) {
		if self.color.is_some() {
			return;
		}
		self.color = Some(change.0);
	}
}

impl Formatting {
	pub fn line_number() -> Self {
		Self {
			color: Some(0x92837400),
			bg_color: Some(0x28282800),
			..Default::default()
		}
	}
	pub fn color(color: u32) -> Self {
		Self {
			color: Some(color),
			..Default::default()
		}
	}
	pub fn rgb([r, g, b]: [u8; 3]) -> Self {
		Self::color(u32::from_be_bytes([r, g, b, 0]))
	}

	pub fn underline(mut self) -> Self {
		self.underline = true;
		self
	}

	pub fn bold(mut self) -> Self {
		self.bold = true;
		self
	}

	pub fn decoration(mut self) -> Self {
		self.decoration = true;
		self
	}
}

const E: &str = "\x1b[";

pub fn text_to_ansi(buf: &Text, out: &mut String) {
	use std::fmt::Write;

	for (text, meta) in buf.iter() {
		if meta.bold {
			write!(out, "{E}1m").expect("no fmt error");
		}
		if meta.underline {
			write!(out, "{E}4m").expect("no fmt error");
		}
		if let Some(color) = meta.color {
			let [r, g, b, _a] = u32::to_be_bytes(color);
			write!(out, "{E}38;2;{r};{g};{b}m").expect("no fmt error");
		}
		if let Some(bg_color) = meta.bg_color {
			let [r, g, b, _a] = u32::to_be_bytes(bg_color);
			write!(out, "{E}48;2;{r};{g};{b}m").expect("no fmt error")
		}
		for chunk in text {
			write!(out, "{chunk}").expect("no fmt error");
		}
		if meta.color.is_some() || meta.bg_color.is_some() || meta.underline || meta.bold {
			write!(out, "{E}0m").expect("no fmt error")
		}
	}
}
