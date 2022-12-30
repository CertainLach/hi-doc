//! Definitions of used formatting characters and how to act when they are overlapped during formatting

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

/// Lines from range to annotation text/line connection
pub mod arrow {
	pub struct Chars {
		pub cont: char,
		pub cont_x: char,
		pub arrow_rl: char,
		pub arrow_rl_x: char,
		pub arrow_rl_x_x: char,
		pub arrow_l: char,
		pub arrow_l_x: char,
		pub arrow_l_x_x: char,
		pub arrow_r: char,
		pub arrow_r_x: char,
		pub arrow_r_x_x: char,
		pub arrow_cont: char,
		pub arrow_cont_x: char,
		pub arrow_cont_x_x: char,
		pub arrow_inline: char,
	}
	pub static BOTTOM: Chars = Chars {
		cont: '│',
		cont_x: '┃',
		arrow_rl: '┴',
		arrow_rl_x: '╀',
		arrow_rl_x_x: '╂',
		arrow_l: '╯',
		arrow_l_x: '┦',
		arrow_l_x_x: '┨',
		arrow_r: '╰',
		arrow_r_x: '┞',
		arrow_r_x_x: '┠',
		arrow_cont: '─',
		arrow_cont_x: '┼',
		arrow_cont_x_x: '╂',
		arrow_inline: '🢒',
	};
	pub static TOP: Chars = Chars {
		arrow_rl: '┬',
		arrow_rl_x: '╁',
		arrow_l: '╮',
		arrow_l_x: '┧',
		arrow_r: '╭',
		arrow_r_x: '┟',
		..BOTTOM
	};

	pub fn cross(chars: &Chars, char: char) -> Option<(bool, char)> {
		match char {
			x if x == chars.cont => Some((true, chars.cont_x)),
			x if x == chars.cont_x => None,

			x if x == chars.arrow_cont => Some((false, chars.arrow_cont_x)),
			x if x == chars.arrow_cont_x => Some((true, chars.arrow_cont_x_x)),
			#[allow(unreachable_patterns)]
			x if x == chars.arrow_cont_x_x || x == chars.arrow_rl_x_x => None,

			x if x == chars.arrow_r => Some((false, chars.arrow_r_x)),
			x if x == chars.arrow_r_x => Some((true, chars.arrow_r_x_x)),
			x if x == chars.arrow_r_x_x => None,

			x if x == chars.arrow_l => Some((false, chars.arrow_l_x)),
			x if x == chars.arrow_l_x => Some((true, chars.arrow_l_x_x)),
			x if x == chars.arrow_l_x_x => None,

			x if x == chars.arrow_rl => Some((false, chars.arrow_rl_x)),
			x if x == chars.arrow_rl_x => Some((true, chars.arrow_rl_x_x)),

			' ' => Some((false, chars.cont)),

			c => unreachable!("{c:?}"),
		}
	}
}

/// Ranges
pub(crate) mod single {
	pub struct Chars {
		pub cont: char,
		pub cont_x: char,
		pub range_start: char,
		pub range_start_x: char,
		pub range_cont: char,
		pub range_cont_x: char,
		pub range_cont_x_x: char,
		pub range_end: char,
		pub range_end_x: char,
		pub range_end_x_x: char,
	}
	pub static BOTTOM: Chars = Chars {
		cont: '│',
		cont_x: '┃',

		range_start: '├',
		range_start_x: '┠',

		range_cont: '─',
		range_cont_x: '┼',
		range_cont_x_x: '╂',

		range_end: '╯',
		range_end_x: '┦',
		range_end_x_x: '┨',
	};
	pub static TOP: Chars = Chars {
		range_end: '╮',
		range_end_x: '┧',
		..BOTTOM
	};

	pub fn cross(chars: &Chars, char: char) -> Option<(bool, char)> {
		match char {
			x if x == chars.cont => Some((true, chars.cont_x)),
			x if x == chars.cont_x => None,

			x if x == chars.range_start => Some((true, chars.range_start_x)),
			x if x == chars.range_start_x => None,

			x if x == chars.range_cont => Some((false, chars.range_cont_x)),
			x if x == chars.range_cont_x => Some((true, chars.range_cont_x_x)),
			x if x == chars.range_cont_x_x => None,

			x if x == chars.range_end => Some((true, chars.range_end_x)),
			x if x == chars.range_end_x => Some((true, chars.range_end_x_x)),
			x if x == chars.range_end_x_x => None,

			' ' => Some((false, chars.cont)),

			c => unreachable!("{c:?}"),
		}
	}
}
