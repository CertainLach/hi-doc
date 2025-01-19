//! Definitions of used formatting characters and how to act when they are overlapped during formatting

use extension_trait::extension_trait;
use unicode_box_drawing::BoxCharacter;

/// Line connections
pub(crate) mod line {
	pub const CONTINUE: char = '─';
	pub const CONTINUE_CROSS: char = '━';

	pub const RANGE_EMPTY: char = '╶';
	pub const RANGE_EMPTY_CROSS: char = '╼';
	pub const RANGE_EMPTY_CROSS_CROSS: char = '━';

	pub const RANGE_START: char = '╭';
	pub const RANGE_START_CROSS: char = '┮';
	pub const RANGE_START_CROSS_CROSS: char = '┯';

	pub const RANGE_END: char = '╰';
	pub const RANGE_END_CROSS: char = '┶';
	pub const RANGE_END_CROSS_CROSS: char = '┷';

	pub const RANGE_CONNECTION: char = '├';
	pub const RANGE_CONNECTION_CROSS: char = '┾';
	pub const RANGE_CONNECTION_CROSS_CROSS: char = '┿';

	pub const RANGE_CONTINUE: char = '│';
	pub const RANGE_CONTINUE_CROSS: char = '┼';
	pub const RANGE_CONTINUE_CROSS_CROSS: char = '┿';

	pub fn cross(char: char) -> Option<(bool, char)> {
		match char {
			CONTINUE => Some((true, CONTINUE_CROSS)),
			#[allow(unreachable_patterns)]
			CONTINUE_CROSS | RANGE_EMPTY_CROSS_CROSS => None,

			RANGE_EMPTY => Some((true, RANGE_EMPTY_CROSS)),
			RANGE_EMPTY_CROSS => Some((true, RANGE_EMPTY_CROSS_CROSS)),

			RANGE_START => Some((true, RANGE_START_CROSS)),
			RANGE_START_CROSS => Some((true, RANGE_START_CROSS_CROSS)),
			RANGE_START_CROSS_CROSS => None,

			RANGE_END => Some((true, RANGE_END_CROSS)),
			RANGE_END_CROSS => Some((true, RANGE_END_CROSS_CROSS)),
			RANGE_END_CROSS_CROSS => None,

			RANGE_CONNECTION => Some((true, RANGE_CONNECTION_CROSS)),
			RANGE_CONNECTION_CROSS => Some((true, RANGE_CONNECTION_CROSS_CROSS)),
			#[allow(unreachable_patterns)]
			RANGE_CONNECTION_CROSS_CROSS | RANGE_CONTINUE_CROSS_CROSS => None,

			RANGE_CONTINUE => Some((false, RANGE_CONTINUE_CROSS)),
			RANGE_CONTINUE_CROSS => Some((true, RANGE_CONTINUE_CROSS_CROSS)),

			' ' => Some((false, CONTINUE)),

			c => unreachable!("{c:?}"),
		}
	}
}

pub enum PreserveStyle {
	Keep,
	Replace,
}

#[extension_trait]
pub(crate) impl BoxCharacterExt for BoxCharacter {
	fn cross_vert(self) -> (PreserveStyle, Self) {
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
	fn cross_hor(self) -> (PreserveStyle, Self) {
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

// fn brc(s: &str, bottom: bool) -> char {
// 	corner_round(
// 		BoxData::from_str(s.as_bytes())
// 			.mirror_vert_if(!bottom)
// 			.char(),
// 	)
// }

pub(crate) fn cross_vertical(c: char) -> Option<(PreserveStyle, BoxCharacter)> {
	let c = BoxCharacter::decode_char(c)?;
	Some(c.cross_vert())
}
