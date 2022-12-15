use std::ops::RangeInclusive;

use range_map::RangeSet;

use crate::formatting::{Formatting, Text};

#[derive(Hash, PartialEq, Eq, Debug, Clone, Copy)]
pub struct AnnotationId(u32);
pub struct AnnotationIdAllocator(u32);
impl AnnotationIdAllocator {
	pub fn new() -> Self {
		Self(0)
	}
	pub fn next(&mut self) -> AnnotationId {
		let id = self.0;
		self.0 += 1;
		AnnotationId(id)
	}
}

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
	pub id: AnnotationId,
	pub priority: usize,
	pub formatting: Formatting,
	/// Byte ranges of the annotated regions
	/// Should not be empty
	pub ranges: RangeSet<usize>,
	pub text: Text,
}
