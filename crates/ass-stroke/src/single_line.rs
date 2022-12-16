use core::fmt;
use std::{
	cmp::Reverse,
	collections::{BTreeMap, HashMap, HashSet},
};

use num_traits::PrimInt;
use range_map::RangeSet;

use crate::{
	annotation::AnnotationId,
	anomaly_fixer::apply_fixup,
	segment::{Segment, SegmentBuffer},
	Formatting, Text,
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

#[derive(Debug)]
pub(crate) struct InlineAnnotation {
	ranges: RangeSet<usize>,
	formatting: Formatting,
}

#[derive(Debug)]
pub(crate) struct SingleLine {
	pub annotation: Option<AnnotationId>,
	pub right: Option<(Formatting, Text)>,
	pub inline: Vec<InlineAnnotation>,
	pub hide_ranges_for: HashSet<AnnotationId>,
	pub processed: HashSet<AnnotationId>,
}

fn can_use(occupied: &RangeSet<usize>, ranges: &RangeSet<usize>) -> bool {
	if !occupied.intersection(ranges).is_empty() {
		for range in ranges.ranges() {
			if range.start == 0 {
				return false;
			}
			for i in range.start - 1..=range.end + 1 {
				if !occupied.contains(i) {
					return false;
				}
			}
		}
	}
	true
}

pub(crate) fn group_singleline(annotations: &[LineAnnotation]) -> SingleLine {
	let mut processed = HashSet::new();
	let mut occupied = RangeSet::new();

	let mut inline = Vec::new();

	let mut annotation = if let Some(leftmost) = annotations
		.iter()
		.filter(|a| !processed.contains(&a.id))
		.filter(|a| a.left && a.right.is_empty())
		.filter(|a| can_use(&occupied, &a.ranges))
		.min_by_key(|a| {
			(
				Reverse(a.priority),
				a.ranges.elements().next().unwrap_or(usize::MAX),
				Reverse(a.ranges.num_elements()),
			)
		}) {
		processed.insert(leftmost.id);
		occupied = occupied.union(&leftmost.ranges);

		inline.push(InlineAnnotation {
			ranges: leftmost.ranges.clone(),
			formatting: leftmost.formatting.clone(),
		});
		Some(leftmost.id)
	} else {
		None
	};

	let mut right = if let Some(rightmost) = annotations
		.iter()
		.filter(|a| !processed.contains(&a.id))
		.filter(|a| !a.left && !a.right.is_empty() && a.right.data().all(|c| *c != '\n'))
		.filter(|a| can_use(&occupied, &a.ranges))
		.max_by_key(|a| {
			(
				a.priority,
				a.ranges.elements().last().unwrap_or(0),
				a.ranges.num_elements(),
			)
		}) {
		processed.insert(rightmost.id);
		occupied = occupied.union(&rightmost.ranges);

		inline.push(InlineAnnotation {
			ranges: rightmost.ranges.clone(),
			formatting: rightmost.formatting.clone(),
		});

		Some((rightmost.formatting.clone(), rightmost.right.clone()))
	} else {
		None
	};

	if annotation.is_none() && right.is_none() {
		if let Some(most) = annotations
			.iter()
			.filter(|a| !processed.contains(&a.id))
			.filter(|a| a.left && a.right.data().all(|c| *c != '\n'))
			.filter(|a| can_use(&occupied, &a.ranges))
			.min_by_key(|a| {
				(
					Reverse(a.priority),
					a.ranges.elements().next().unwrap_or(usize::MAX),
					Reverse(a.ranges.num_elements()),
				)
			}) {
			processed.insert(most.id);
			occupied = occupied.union(&most.ranges);

			inline.push(InlineAnnotation {
				ranges: most.ranges.clone(),
				formatting: most.formatting.clone(),
			});
			annotation = Some(most.id);
			right = Some((most.formatting.clone(), most.right.clone()));
		}
	}

	let mut hide_ranges_for = HashSet::new();

	// Ensure inlined annotations are not shadowing leftmost/rightmost item
	// New annotation should be either not intersect with others, or
	for a in annotations.iter().filter(|a| !processed.contains(&a.id)) {
		if !can_use(&occupied, &a.ranges) {
			continue;
		}
		inline.push(InlineAnnotation {
			ranges: a.ranges.clone(),
			formatting: a.formatting.clone(),
		});
		hide_ranges_for.insert(a.id);
	}

	SingleLine {
		annotation,
		right,
		inline,
		hide_ranges_for,
		processed,
	}
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

pub(crate) fn apply_inline_annotations(
	text: &mut Text,
	annotations: &[InlineAnnotation],
	right: Option<(Formatting, Text)>,
) {
	for annotation in annotations {
		for range in annotation.ranges.ranges() {
			text.apply_meta(range.start..=range.end, &annotation.formatting)
		}
	}
	if let Some((formatting, right)) = right {
		text.extend(Text::single(
			[crate::chars::arrow::ARROW_INLINE, ' '],
			formatting,
		));
		text.extend(right);
	}
}

#[allow(dead_code)]
pub(crate) fn generate_range_annotations(
	mut annotations: Vec<LineAnnotation>,
	char_to_display_fixup: &BTreeMap<usize, isize>,
	hide_ranges_for: &HashSet<AnnotationId>,
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
		for layer in per_line_ranges.iter() {
			let mut fmtlayer = SegmentBuffer::new([Segment::new(
				vec![' '; max_range_display + 1],
				Formatting::default(),
			)]);

			let mut useless = true;
			for annotation in layer
				.iter()
				.map(|i| annotations_by_id.get(i).expect("exists"))
			{
				for range in annotation.ranges.ranges() {
					let data = if range.start == range.end {
						vec![CONTINUE]
					} else {
						let mut out = vec![RANGE_START];
						out.resize(
							char_to_display(range.end) - char_to_display(range.start),
							RANGE_CONTINUE,
						);
						out.push(RANGE_END);
						useless = false;
						out
					};
					fmtlayer.splice(
						char_to_display(range.start)..=char_to_display(range.end),
						Some(SegmentBuffer::new([Segment::new(
							data,
							annotation.formatting.clone(),
						)])),
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
						if let Some((keep_style, replacement)) = cross(c) {
							other.splice(
								char_to_display(start)..=char_to_display(start),
								Some(SegmentBuffer::new([Segment::new(
									[replacement],
									if keep_style {
										orig_fmt
									} else {
										annotation.formatting.clone()
									},
								)])),
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
		for annotation in &annotations {
			let mut fmtlayer = SegmentBuffer::new([Segment::new(
				vec![' '; max_range_display + 1],
				Formatting::default(),
			)]);
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
					ARROW_RL
				} else if annotation.left && last {
					ARROW_L
				} else if !annotation.right.is_empty() && first {
					ARROW_R
				} else {
					assert!(
						annotation.left || !annotation.right.is_empty(),
						"no right or left text: {annotation:?}"
					);
					ARROW_RL
				};
				fmtlayer.splice(
					char_to_display(*start)..=char_to_display(*start),
					Some(SegmentBuffer::new([Segment::new(
						[c],
						annotation.formatting.clone(),
					)])),
				);
			}
			let annotation_id = if annotation.left {
				let size = char_to_display(min) - char_to_display(min_pos);
				fmtlayer.splice(
					char_to_display(min_pos)..char_to_display(min),
					Some(SegmentBuffer::new([Segment::new(
						vec![ARROW_CONTINUE; size],
						annotation.formatting.clone(),
					)])),
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
					Some(SegmentBuffer::new([Segment::new(
						vec![ARROW_CONTINUE; size],
						annotation.formatting.clone(),
					)])),
				);
				fmtlayer.extend(Text::single([' '], Default::default()));
				let lines = right.split('\n');
				fmtlayer.extend(lines[0].clone());
				for right in lines.iter().skip(1) {
					let mut fmtlayer = SegmentBuffer::new([Segment::new(
						vec![' '; max_range_display],
						Formatting::default(),
					)]);
					fmtlayer.extend(right.clone());
					extralayers.push((None, fmtlayer));
				}
			}
			for i in char_to_display(min)..=char_to_display(max) {
				if fmtlayer.get(i).unwrap().0 != ' ' {
					continue;
				}
				fmtlayer.splice(
					i..=i,
					Some(SegmentBuffer::new([Segment::new(
						[ARROW_CONTINUE],
						annotation.formatting.clone(),
					)])),
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
					if let Some((keep_style, replacement)) = cross(c) {
						affected.1.splice(
							char_to_display(start)..=char_to_display(start),
							Some(SegmentBuffer::new([Segment::new(
								[replacement],
								if keep_style {
									orig_fmt
								} else {
									annotation.formatting.clone()
								},
							)])),
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

/*
TODO: optimize lines to left after multi-line to right texts:
|       ╰────┼────  Baz
|            │      Line2
|   ─────────╯
=>
|       ╰────┼────  Baz
|   ─────────╯      Line2
*/

#[cfg(test)]
mod tests {
	use crate::anomaly_fixer::fixup_char_to_display;

	use super::*;

	fn default<T: Default>() -> T {
		Default::default()
	}

	#[test]
	fn single_line() {
		use random_color::RandomColor;
		fn gen_color(seed: u32) -> Formatting {
			let [r, g, b] = RandomColor::new().seed(seed).to_rgb_array();
			Formatting::color(u32::from_be_bytes([r, g, b, 0]))
		}
		use range_map::Range;

		let mut last_aid = 0;
		let mut aid = || {
			last_aid += 1;
			AnnotationId(last_aid)
		};

		let out = generate_range_annotations(
			vec![
				LineAnnotation {
					id: aid(),
					priority: 0,
					ranges: vec![Range::new(3usize, 6), Range::new(8usize, 10)]
						.into_iter()
						.collect(),
					formatting: gen_color(0),
					left: true,
					right: Text::single("Foo".chars(), default()),
				},
				LineAnnotation {
					id: aid(),
					priority: 0,
					ranges: vec![Range::new(3usize, 10)].into_iter().collect(),
					formatting: gen_color(1),
					left: false,
					right: Text::single("Bar".chars(), default()),
				},
				LineAnnotation {
					id: aid(),
					priority: 1,
					ranges: vec![Range::new(7usize, 7)].into_iter().collect(),
					formatting: gen_color(2),
					left: false,
					right: Text::single("Baz\nLine2".chars(), default()),
				},
				LineAnnotation {
					id: aid(),
					priority: 0,
					ranges: vec![Range::new(12usize, 17)].into_iter().collect(),
					formatting: gen_color(3),
					left: true,
					right: Text::empty(),
				},
				LineAnnotation {
					id: aid(),
					priority: 0,
					ranges: vec![Range::new(11usize, 14)].into_iter().collect(),
					formatting: gen_color(4),
					left: false,
					right: Text::single("FooBar".chars(), default()),
				},
			],
			&fixup_char_to_display("012345678901234567890123456789012345".chars()),
			&HashSet::new(),
		);
	}
}
