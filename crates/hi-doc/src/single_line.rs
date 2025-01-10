use core::fmt;
use std::{
	cmp::Reverse,
	collections::{BTreeMap, HashMap, HashSet},
};

use num_traits::PrimInt;
use range_map::RangeSet;

use crate::{
	annotation::AnnotationId, anomaly_fixer::apply_fixup, segment::SegmentBuffer, Formatting, Text,
};

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
}

/// Distribute annotations per layers
/// In single layer, no annotation range conflicts will occur
///
/// Sorting order is not determined, and depends on sorting of original array
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

	let per_line_ranges = group_nonconflicting(
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

	{
		use crate::chars::single::*;
		let chars = if bottom { &BOTTOM } else { &TOP };
		for layer in per_line_ranges.iter() {
			let mut fmtlayer = SegmentBuffer::segment(
				// TODO: Avoid allocation?
				" ".repeat(max_range_display + 1),
				Formatting::default(),
			);

			let mut useless = true;
			for annotation in layer
				.iter()
				.map(|i| annotations_by_id.get(i).expect("exists"))
			{
				for range in annotation.ranges.ranges() {
					let data = if range.start == range.end {
						vec![chars.cont]
					} else {
						let mut out = vec![chars.range_start];
						out.resize(
							char_to_display(range.end) - char_to_display(range.start),
							chars.range_cont,
						);
						out.push(chars.range_end);
						useless = false;
						out
					};
					fmtlayer.splice(
						char_to_display(range.start)..=char_to_display(range.end),
						Some(SegmentBuffer::segment_chars(
							data,
							annotation.formatting.clone(),
						)),
					);
				}
			}

			let idx = range_fmt_layers.len();
			range_fmt_layers.push(fmtlayer);
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
					for start in annotation.ranges.ranges().map(|r| r.start) {
						let (c, orig_fmt) =
							other.get(char_to_display(start)).expect("extended to max");
						if let Some((keep_style, replacement)) = cross(chars, c) {
							other.splice(
								char_to_display(start)..=char_to_display(start),
								Some(SegmentBuffer::segment(
									replacement.to_string(),
									if keep_style {
										orig_fmt.clone()
									} else {
										annotation.formatting.clone()
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

	let mut layers = Vec::new();
	{
		use crate::chars::arrow::*;
		let chars = if bottom { &BOTTOM } else { &TOP };
		for annotation in &annotations {
			let mut fmtlayer =
				SegmentBuffer::segment(" ".repeat(max_range_display + 1), Formatting::default());
			let mut extralayers = Vec::new();

			let starts = annotation
				.ranges
				.ranges()
				.map(|r| r.start)
				.collect::<Vec<_>>();

			let mut min = usize::MAX;
			let mut max = 0;
			for (i, start) in starts.iter().enumerate() {
				let first = i == 0;
				let last = i == starts.len() - 1;
				min = min.min(*start);
				max = max.max(*start);
				let c = if annotation.left && !annotation.right.is_empty() {
					chars.arrow_rl
				} else if annotation.left && last {
					chars.arrow_l
				} else if !annotation.right.is_empty() && first {
					chars.arrow_r
				} else {
					assert!(
						annotation.left || !annotation.right.is_empty(),
						"no right or left text: {annotation:?}"
					);
					chars.arrow_rl
				};
				fmtlayer.splice(
					char_to_display(*start)..=char_to_display(*start),
					Some(SegmentBuffer::segment(
						c.to_string(),
						annotation.formatting.clone(),
					)),
				);
			}
			let annotation_id = if annotation.left {
				let size = char_to_display(min) - char_to_display(min_pos);
				fmtlayer.splice(
					char_to_display(min_pos)..char_to_display(min),
					Some(SegmentBuffer::segment(
						chars.arrow_cont.to_string().repeat(size),
						annotation.formatting.clone(),
					)),
				);
				Some(annotation.id)
			} else {
				None
			};
			if !annotation.right.is_empty() {
				let right = &annotation.right;

				let size = max_range_display - char_to_display(max) + 2;
				fmtlayer.splice(
					char_to_display(max) + 1..max_range_display + 1,
					Some(SegmentBuffer::segment(
						chars.arrow_cont.to_string().repeat(size),
						annotation.formatting.clone(),
					)),
				);
				fmtlayer.extend([Text::segment(" ", Default::default())]);
				let lines = right.split('\n');
				fmtlayer.append(lines[0].clone());
				for right in lines.iter().skip(1) {
					let mut fmtlayer = SegmentBuffer::segment(
						" ".repeat(max_range_display),
						Formatting::default(),
					);
					fmtlayer.append(right.clone());
					extralayers.push((None, fmtlayer));
				}
			}
			for i in char_to_display(min)..=char_to_display(max) {
				if fmtlayer.get(i).unwrap().0 != ' ' {
					continue;
				}
				fmtlayer.splice(
					i..=i,
					Some(SegmentBuffer::segment(
						chars.arrow_cont.to_string(),
						annotation.formatting.clone(),
					)),
				);
			}

			extralayers.insert(0, (annotation_id, fmtlayer));
			// layers.extend(extralayers);
			layers.push(extralayers);
		}
		// Cross lines for earlier displayed annotations
		for (i, annotation) in annotations.iter().enumerate() {
			for affected in layers[..i].iter_mut().flatten() {
				for start in annotation.ranges.ranges().map(|r| r.start) {
					let (c, orig_fmt) = affected.1.get(start).expect("extended to max");
					if let Some((keep_style, replacement)) = cross(chars, c) {
						affected.1.splice(
							char_to_display(start)..=char_to_display(start),
							Some(SegmentBuffer::segment(
								replacement.to_string(),
								if keep_style {
									orig_fmt.clone()
								} else {
									annotation.formatting.clone()
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
		out.push((None, layer.clone()));
	}
	for layer in layers.iter().flatten() {
		out.push(layer.clone())
	}

	out
}
