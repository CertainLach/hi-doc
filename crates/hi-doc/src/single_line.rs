//! single_line module tries to draw optimal annotation lines for a single code line,
//! e.g its function is to draw those arrows:
//!            ╭────── Local name
//!            │    ^^- LABEL_PADDING
//!    ╭─┬───╮ ├─╮ ╭── Equals
//!    │ local std = self,
//!     ^- But note that everything to the left is line interconnections, which aren't drawn
//!        by this module!

use core::fmt;
use std::{
	cmp::Reverse,
	collections::{BTreeMap, HashMap, HashSet},
};

use annotated_string::AnnotatedRope;
use itertools::Itertools;
use num_traits::PrimInt;
use range_map::RangeSet;
use unicode_box_drawing::bc;

use crate::{
	annotation::{AnnotationId, AnnotationLocation},
	anomaly_fixer::apply_fixup,
	chars::{cross_vertical, BoxCharacterExt, PreserveStyle},
	Formatting, Text,
};

const LABEL_PADDING: usize = 2;

/// This kind of annotations is not used directly, instead library creates those
#[derive(Debug, Clone)]
pub(crate) struct LineAnnotation {
	pub id: AnnotationId,
	pub priority: usize,
	pub ranges: RangeSet<usize>,
	pub formatting: Formatting,
	/// Should this annotation have a line pointing to the left
	/// This option is used to make places for interconnecting lines
	pub left: bool,
	/// What text to display to the right of annotated line
	pub right: Text,

	pub location: AnnotationLocation,
}

impl LineAnnotation {
	fn can_inline(&self) -> bool {
		!self.right.chars().contains(&'\n')
			&& (!self.ranges.ranges().any(|r| r.start == r.end) || self.ranges.num_ranges() == 1)
	}
	fn can_inline_left(&self, has_annotations_to_right: bool) -> bool {
		self.can_inline() && self.left && (self.right.is_empty() || !has_annotations_to_right)
	}
	fn can_inline_right(&self, has_annotations_to_left: bool) -> bool {
		self.can_inline() && !self.right.is_empty() && (!self.left || !has_annotations_to_left)
	}
}

/// Distribute annotations per layers
/// In single layer, no annotation range conflicts will occur
///
/// Sorting order is not determined, and depends on sorting of original array
///
/// TODO: Should this grouping also consider trying to make most annotations inline?
pub(crate) fn group_nonconflicting<T: PrimInt + fmt::Debug>(
	annotations: &[(AnnotationId, RangeSet<T>)],
	exclude: &HashSet<AnnotationId>,
) -> Vec<Vec<AnnotationId>> {
	let mut layers = vec![];
	let mut processed = exclude.clone();

	for (i, annotation) in annotations.iter().enumerate() {
		if !processed.insert(annotation.0) {
			continue;
		}
		let mut occupied = annotation.1.clone();
		let mut layer = vec![annotation.0];

		for other in annotations[i + 1..].iter() {
			if !occupied.intersection(&other.1).is_empty() {
				continue;
			}
			if !processed.insert(other.0) {
				continue;
			}
			occupied = occupied.union(&other.1);
			layer.push(other.0);
		}

		layers.push(layer);
	}

	layers
}

#[allow(clippy::too_many_arguments)]
fn draw_layer_single_annotation(
	layer: &mut Text,
	annotation: &LineAnnotation,
	bottom: bool,
	char_to_display: &impl Fn(usize) -> usize,
	min_pos: usize,
	max_range_display: usize,
	extralayers: &mut Vec<(Option<AnnotationId>, Text)>,
	is_inline: bool,
) -> Option<AnnotationId> {
	let starts = annotation
		.ranges
		.ranges()
		.map(|r| r.start)
		.collect::<Vec<_>>();

	let mut min = usize::MAX;
	let mut max = 0;
	let left = if !is_inline {
		for (i, start) in starts.iter().enumerate() {
			let first = i == 0;
			let last = i == starts.len() - 1;
			min = min.min(*start);
			max = max.max(*start);
			let c = if annotation.left && !annotation.right.is_empty() {
				bc!(rbl)
			} else if annotation.left && last {
				bc!(bl)
			} else if !annotation.right.is_empty() && first {
				bc!(rb)
			} else {
				assert!(
					annotation.left || !annotation.right.is_empty(),
					"no right or left text: {annotation:?}"
				);
				bc!(rbl)
			}
			.mirror_vertical_if(bottom)
			.char_round();
			layer.splice(
				char_to_display(*start)..=char_to_display(*start),
				Some(AnnotatedRope::fragment(
					c.to_string(),
					annotation.formatting.clone(),
				)),
			);
		}

		if annotation.left {
			let size = char_to_display(min) - char_to_display(min_pos);
			layer.splice(
				char_to_display(min_pos)..char_to_display(min),
				Some(AnnotatedRope::fragment(
					bc!(rl).char_round().to_string().repeat(size),
					annotation.formatting.clone(),
				)),
			);
			Some(annotation.id)
		} else {
			None
		}
	} else {
		min = annotation.ranges.ranges().next().expect("not empty").start;
		max = annotation.ranges.ranges().last().expect("not empty").end;
		None
	};
	if !annotation.right.is_empty() {
		let right = &annotation.right;

		let size = max_range_display - char_to_display(max) + LABEL_PADDING;
		layer.splice(
			char_to_display(max) + 1..max_range_display + 1,
			Some(AnnotatedRope::fragment(
				bc!(rl).char_round().to_string().repeat(size),
				annotation.formatting.clone(),
			)),
		);
		layer.extend([Text::fragment(" ", Default::default())]);
		let lines = right.split('\n');
		layer.append(lines[0].clone());
		for right in lines.iter().skip(1) {
			let mut fmtlayer =
				AnnotatedRope::fragment(" ".repeat(max_range_display), Formatting::default());
			fmtlayer.append(right.clone());
			extralayers.push((None, fmtlayer));
		}
	}
	for i in char_to_display(min)..=char_to_display(max) {
		if layer.get(i).unwrap().0 != ' ' {
			continue;
		}
		layer.splice(
			i..=i,
			Some(AnnotatedRope::fragment(
				bc!(rl).char_round().to_string(),
				annotation.formatting.clone(),
			)),
		);
	}
	left
}

#[allow(dead_code)]
pub(crate) fn generate_range_annotations(
	mut annotations: Vec<LineAnnotation>,
	char_to_display_fixup: &BTreeMap<usize, isize>,
	hide_ranges_for: &HashSet<AnnotationId>,
	bottom: bool,
) -> Vec<(Option<AnnotationId>, Text)> {
	if annotations.is_empty() {
		return Vec::new();
	}

	let char_to_display = move |mut offset: usize| {
		apply_fixup(&mut offset, char_to_display_fixup);
		offset
	};

	annotations.sort_by_key(|ann| (Reverse(ann.priority), Reverse(ann.ranges.num_elements())));

	let mut per_line_ranges = group_nonconflicting(
		&annotations
			.iter()
			.map(|a| (a.id, a.ranges.clone()))
			.collect::<Vec<_>>(),
		hide_ranges_for,
	);

	let annotations_by_id = annotations
		.iter()
		.map(|a| (a.id, a))
		.collect::<HashMap<_, _>>();

	let mut range_fmt_layers = Vec::new();
	// Useless range - which contains only single-char pointers
	let mut useless_range_fmt_layers = Vec::new();

	let max_range_display = char_to_display(
		per_line_ranges
			.iter()
			.flat_map(|l| l.iter())
			.map(|i| annotations_by_id.get(i).expect("exists"))
			.map(|a| {
				a.ranges
					.ranges()
					.last()
					.expect("no range in annotation")
					.end
			})
			.max()
			.unwrap_or(0),
	);

	let mut inlined_annotations = HashSet::new();
	{
		// use crate::chars::single::*;
		// let chars = if bottom { &BOTTOM } else { &TOP };
		for layer in per_line_ranges.iter_mut() {
			let mut fmtlayer = AnnotatedRope::fragment(
				// TODO: Avoid allocation?
				" ".repeat(max_range_display + 1),
				Formatting::default(),
			);

			let mut useless = true;

			let mut inline_right = None;
			let mut inline_left = None;
			{
				layer.sort_by_key(|l| {
					annotations_by_id[l]
						.ranges
						.ranges()
						.last()
						.expect("has range")
						.end
				});
				let rightmost_annotation = annotations_by_id[layer.last().expect("not empty")];
				if rightmost_annotation.can_inline_right(layer.len() != 1) {
					inline_right = Some(rightmost_annotation.id);
					inlined_annotations.insert(rightmost_annotation.id);
					useless = false;
				}

				layer.sort_by_key(|l| {
					annotations_by_id[l]
						.ranges
						.ranges()
						.next()
						.expect("has range")
						.start
				});
				let leftmost_annotation = annotations_by_id[layer.first().expect("not empty")];
				if leftmost_annotation.can_inline_left(layer.len() != 1) {
					inline_left = Some(leftmost_annotation.id);
					inlined_annotations.insert(leftmost_annotation.id);
					useless = false;
				}
			};

			// let mut inline_right = None;
			for annotation in layer
				.iter()
				.map(|i| annotations_by_id.get(i).expect("exists"))
			{
				let is_inline_left = inline_left == Some(annotation.id);
				let is_inline_right = inline_right == Some(annotation.id);
				for (i, range) in annotation.ranges.ranges().enumerate() {
					let first_range = i == 0;
					let data = if range.start == range.end {
						vec![if is_inline_right { bc!(rb) } else { bc!(tb) }
							.mirror_vertical_if(bottom)
							.char_round()]
					} else {
						let mut out = vec![if is_inline_left {
							bc!(rbl)
						} else if is_inline_right {
							bc!(rb).left_if(!first_range)
						} else {
							bc!(trb)
						}
						.mirror_vertical_if(bottom)
						.char_round()];
						out.resize(
							char_to_display(range.end) - char_to_display(range.start),
							bc!(rl).char_round(),
						);
						out.push(
							if is_inline_right { bc!(rbl) } else { bc!(bl) }
								.mirror_vertical_if(bottom)
								.char_round(),
						);
						useless = false;
						out
					};
					fmtlayer.splice(
						char_to_display(range.start)..=char_to_display(range.end),
						Some(AnnotatedRope::fragment_chars(
							data,
							annotation.formatting.clone(),
						)),
					);
				}
			}

			if inline_left.is_some() {
				assert!(!useless)
			}

			let idx = range_fmt_layers.len();
			range_fmt_layers.push((inline_left, fmtlayer, inline_right));
			if useless {
				useless_range_fmt_layers.push(idx);
			}
		}

		// Draw crosses over other layers
		for (i, layer) in per_line_ranges.iter().enumerate() {
			for other in range_fmt_layers[i + 1..].iter_mut() {
				for annotation in layer
					.iter()
					.map(|i| annotations_by_id.get(i).expect("exists"))
				{
					if inlined_annotations.contains(&annotation.id) {
						continue;
					}
					for start in annotation.ranges.ranges().map(|r| r.start) {
						let (c, orig_fmt) = other
							.1
							.get(char_to_display(start))
							.expect("extended to max");
						if let Some((keep_style, replacement)) = cross_vertical(c) {
							other.1.splice(
								char_to_display(start)..=char_to_display(start),
								Some(AnnotatedRope::fragment(
									replacement.to_string(),
									match keep_style {
										PreserveStyle::Keep => orig_fmt.clone(),
										PreserveStyle::Replace => annotation.formatting.clone(),
									},
								)),
							)
						}
					}
				}
			}
		}
	}

	annotations.sort_by_key(|a| {
		(
			a.right.is_empty(),
			!a.left,
			Reverse(a.ranges.ranges().next().expect("not empty").start),
		)
	});

	let max_range_display = char_to_display(
		annotations
			.iter()
			.map(|a| {
				a.ranges
					.ranges()
					.last()
					.expect("no range in annotation")
					.end
			})
			.max()
			.expect("has annotation"),
	);

	let min_pos = annotations
		.iter()
		.map(|a| {
			a.ranges
				.ranges()
				.next()
				.expect("no range in annotation")
				.start
		})
		.min()
		.expect("has annotation");

	// Draw text to right for inlined to-right annotations
	for (_, text, right) in range_fmt_layers.iter_mut() {
		let Some(right) = right else {
			continue;
		};
		let annotation = annotations.iter().find(|a| a.id == *right).expect("exists");
		let mut extralayers = vec![];
		draw_layer_single_annotation(
			text,
			annotation,
			bottom,
			&char_to_display,
			min_pos,
			max_range_display,
			&mut extralayers,
			true,
		);
		assert!(
			extralayers.is_empty(),
			"inlined annotations are single-line"
		);

		// text.extend([
		// 	Text::repeat_char(
		// 		chars.arrow_cont,
		// 		max_range_display + 1 + LABEL_PADDING - text.chars().count(),
		// 		annotation.formatting.clone(),
		// 	),
		// 	Text::segment(" ", Default::default()),
		// 	annotation.right.clone(),
		// ]);
	}

	// Remove inlined annotations.
	annotations.retain(|a| !inlined_annotations.contains(&a.id));

	let mut layers = Vec::new();
	{
		for annotation in &annotations {
			let mut fmtlayer =
				AnnotatedRope::fragment(" ".repeat(max_range_display + 1), Formatting::default());
			let mut extralayers = Vec::new();

			let left = draw_layer_single_annotation(
				&mut fmtlayer,
				annotation,
				bottom,
				&char_to_display,
				min_pos,
				max_range_display,
				&mut extralayers,
				false,
			);

			extralayers.insert(0, (left, fmtlayer));
			// layers.extend(extralayers);
			layers.push(extralayers);
		}
		// Cross lines for earlier displayed annotations
		for (i, annotation) in annotations.iter().enumerate() {
			if inlined_annotations.contains(&annotation.id) {
				continue;
			}
			for affected in layers[..i].iter_mut().flatten() {
				for start in annotation.ranges.ranges().map(|r| r.start) {
					let (c, orig_fmt) = affected.1.get(start).expect("extended to max");
					if let Some((keep_style, replacement)) = cross_vertical(c) {
						affected.1.splice(
							char_to_display(start)..=char_to_display(start),
							Some(AnnotatedRope::fragment(
								replacement.to_string(),
								match keep_style {
									PreserveStyle::Keep => orig_fmt.clone(),
									PreserveStyle::Replace => annotation.formatting.clone(),
								},
							)),
						)
					}
				}
			}
		}
	}

	let mut out = Vec::new();
	for (idx, layer) in range_fmt_layers.iter().enumerate() {
		if useless_range_fmt_layers.contains(&idx) {
			continue;
		}
		let (left, data, _) = layer.clone();
		out.push((left, data));
	}
	for layer in layers.iter().flatten() {
		out.push(layer.clone())
	}

	out
}
