use range_map::RangeSet;

use crate::formatting::{Formatting, Text};

/// Used to identify interline connections
#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub(crate) struct AnnotationId(pub usize);

#[derive(Clone)]
pub struct Opts {
	/// For primary ranges, instead of creating line with range annotaions,
	/// apply range colors directly to source string. Only useable with colors, may look ugly
	/// with applied syntax highlight.
	pub colored_range_display: bool,
	/// Allow hiding source lines containing no annotations
	pub fold: bool,
	/// Max width of tab in spaces.
	pub tab_width: usize,

	/// Minimum lines of code above and below annotated line
	pub context_lines: usize,

	/// Allow formatting which is only recognizable by color.
	pub colorblind_output: bool,
}

#[derive(Clone, Debug)]
pub struct Annotation {
	pub priority: usize,
	pub formatting: Formatting,
	/// Byte ranges of the annotated regions
	/// Should not be empty
	pub ranges: RangeSet<usize>,
	pub text: Text,
	pub location: AnnotationLocation,
}

#[derive(Clone, Copy, Default, Debug)]
pub enum AnnotationLocation {
	Any,
	#[default]
	AnyNotInline,
	Above,
	Below,
	AboveOrInline,
	BelowOrInline,
}
impl AnnotationLocation {
	pub fn is_any(&self) -> bool {
		matches!(self, Self::Any)
	}
	pub fn is_inline(&self) -> bool {
		matches!(self, Self::Any | Self::AboveOrInline | Self::BelowOrInline)
	}
	pub fn is_above(&self) -> bool {
		matches!(self, Self::Any | Self::AnyNotInline | Self::Above | Self::AboveOrInline)
	}
	pub fn is_below(&self) -> bool {
		matches!(self, Self::Any | Self::AnyNotInline | Self::Below | Self::BelowOrInline)
	}
}
