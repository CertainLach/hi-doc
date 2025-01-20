use core::fmt;

use crate::segment::{Meta, MetaApply, SegmentBuffer};

pub type Text = SegmentBuffer<Formatting>;

#[derive(Default, Clone, PartialEq, Debug)]
pub struct Formatting {
	pub color: Option<u32>,
	pub bg_color: Option<u32>,
	pub bold: bool,
	pub underline: bool,
	pub decoration: bool,
	pub url: Option<String>,
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
		if change.url.is_some() {
			self.url = change.url.clone();
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
	pub fn listchar() -> Self {
		Self {
			color: Some(0x92837400),
			decoration: true,
			..Default::default()
		}
	}
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

	// TODO: Use url crate for sanitization purposes?
	pub fn url(mut self, url: String) -> Self {
		self.url = Some(url);
		self
	}
}

const CSI: &str = "\x1b[";
const OSC: &str = "\x1b]";
const ST: &str = "\x1b\\";

pub fn text_to_ansi(buf: &Text, out: &mut String) {
	text_to_ansi_res(buf, out).expect("no fmt error")
}
pub fn text_to_ansi_res(buf: &Text, out: &mut String) -> fmt::Result {
	use std::fmt::Write;

	for (text, meta) in buf.iter() {
		if meta.bold {
			write!(out, "{CSI}1m")?;
		}
		if meta.underline {
			write!(out, "{CSI}4m")?;
		}
		if let Some(color) = meta.color {
			let [r, g, b, _a] = u32::to_be_bytes(color);
			write!(out, "{CSI}38;2;{r};{g};{b}m")?;
		}
		if let Some(bg_color) = meta.bg_color {
			let [r, g, b, _a] = u32::to_be_bytes(bg_color);
			write!(out, "{CSI}48;2;{r};{g};{b}m")?;
		}
		// We might want to add `id=` parameter to make terminals highlight split `Text`?
		if let Some(url) = &meta.url {
			write!(out, "{OSC}8;;{url}{ST}")?;
		}
		for chunk in text {
			write!(out, "{chunk}")?;
		}
		if meta.url.is_some() {
			write!(out, "{OSC}8;;{ST}")?;
		}
		if meta.color.is_some() || meta.bg_color.is_some() || meta.underline || meta.bold {
			write!(out, "{CSI}0m")?;
		}
	}
	Ok(())
}
