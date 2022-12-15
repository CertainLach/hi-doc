use range_map::RangeSet;

use crate::formatting::{Formatting, Text};

/// Used to identify interline connections
#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub(crate) struct AnnotationId(pub usize);

#[derive(Default, Clone)]
pub struct Opts {
	/// Debug option, disables prettying line sorting
	pub ratnest_sort: bool,
	/// Debug option, randomly disables range merging
	pub ratnest_merge: bool,
	/// For primary ranges, instead of creating line with range annotaions,
	/// apply range colors directly to source string. Only useable with colors
	pub first_layer_reformats_orig: bool,
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
