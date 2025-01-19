//! Definitions of used formatting characters and how to act when they are overlapped during formatting

use extension_trait::extension_trait;
use unicode_box_drawing::BoxCharacter;

pub enum PreserveStyle {
	Keep,
	Replace,
}

#[extension_trait]
pub(crate) impl BoxCharacterExt for BoxCharacter {
	fn cross_vertical(self) -> (PreserveStyle, Self) {
		let v = self.with_top().with_bottom();
		(
			if self.top().is_none() || self.bottom().is_none() {
				PreserveStyle::Replace
			} else {
				PreserveStyle::Keep
			},
			v,
		)
	}
	fn mirror_vertical_if(self, cond: bool) -> Self {
		if cond {
			self.mirror_vertical()
		} else {
			self
		}
	}
	fn left_if(self, cond: bool) -> Self {
		if cond {
			self.with_left()
		} else {
			self
		}
	}
	fn cross_horizontal(self) -> (PreserveStyle, Self) {
		let v = self.with_left().with_right();
		(
			if self.left().is_none() || self.right().is_none() {
				PreserveStyle::Replace
			} else {
				PreserveStyle::Keep
			},
			v,
		)
	}
}

pub(crate) fn cross_vertical(c: char) -> Option<(PreserveStyle, BoxCharacter)> {
	let c = BoxCharacter::decode_char(c)?;
	Some(c.cross_vertical())
}
pub(crate) fn cross_horizontal(c: char) -> Option<(PreserveStyle, BoxCharacter)> {
	let c = BoxCharacter::decode_char(c)?;
	Some(c.cross_horizontal())
}
