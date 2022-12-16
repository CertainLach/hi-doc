use range_map::RangeSet;

use crate::formatting::{Formatting, Text};

/// Used to identify interline connections
#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub(crate) struct AnnotationId(pub usize);

#[derive(Default, Clone)]
pub struct Opts {
	/// For primary ranges, instead of creating line with range annotaions,
	/// apply range colors directly to source string. Only useable with colors
	pub apply_to_orig: bool,
}

#[derive(Clone)]
pub struct Annotation {
	pub priority: usize,
	pub formatting: Formatting,
	/// Byte ranges of the annotated regions
	/// Should not be empty
	pub ranges: RangeSet<usize>,
	pub text: Text,
}
