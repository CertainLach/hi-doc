use core::fmt;
use std::{
	cmp::Reverse,
	collections::{HashMap, HashSet},
};

use num_traits::PrimInt;
use rand::seq::SliceRandom;
use range_map::RangeSet;

use crate::{
	annotation::{AnnotationId, Opts},
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

/// Distribute annotations per layers
/// In single layer, no annotation range conflicts will occur
///
/// Sorting order is not determined, and depends on sorting of original array
pub(crate) fn group_nonconflicting<T: PrimInt + fmt::Debug>(
	annotations: Vec<(AnnotationId, RangeSet<T>)>,
) -> Vec<Vec<AnnotationId>> {
	let mut layers = vec![];

	let mut pending = annotations.iter().map(|a| a.0).collect::<HashSet<_>>();
	for (i, annotation) in annotations.iter().enumerate() {
		if !pending.remove(&annotation.0) {
			continue;
		}
		let mut occupied = annotation.1.clone();
		let mut layer = vec![annotation.0];

		for other in annotations[i + 1..].iter() {
			if !occupied.intersection(&other.1).is_empty() {
				continue;
			}
			if !pending.remove(&other.0) {
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
pub(crate) fn generate_segment(
	mut annotations: Vec<LineAnnotation>,
	mut text: Text,
	opts: &Opts,
) -> (Text, Vec<(Option<AnnotationId>, Text)>) {
	if opts.ratnest_sort {
		annotations.shuffle(&mut rand::thread_rng());
	} else {
		annotations.sort_by_key(|ann| (Reverse(ann.priority), Reverse(ann.ranges.num_elements())));
	}

	let layers = group_nonconflicting(
		annotations
			.iter()
			.map(|a| (a.id, a.ranges.clone()))
			.collect(),
	);

	let annotations_by_id = annotations
		.iter()
		.map(|a| (a.id, a))
		.collect::<HashMap<_, _>>();

	let mut layers_pre = Vec::new();
	let min_pos = layers
		.iter()
		.flat_map(|l| l.iter())
		.map(|i| annotations_by_id.get(i).expect("exists"))
		.map(|a| {
			a.ranges
				.ranges()
				.next()
				.expect("no range in annotation")
				.start
		})
		.min()
		.expect("layer is not empty");
	let max_size = layers
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
		.expect("layer is not empty");
	{
		use crate::chars::single::*;
		if opts.first_layer_reformats_orig {
			{
				// Dummy
				let fmtlayer = SegmentBuffer::new([Segment::new(
					vec![' '; max_size + 1],
					Formatting::default(),
				)]);
				layers_pre.push(fmtlayer);
			}
			for annotation in layers[0]
				.iter()
				.map(|i| annotations_by_id.get(i).expect("exists"))
			{
				for range in annotation.ranges.ranges() {
					text.apply_meta(range.start..=range.end, &annotation.formatting);
				}
			}
		}
		for layer in layers.iter().skip(if opts.first_layer_reformats_orig {
			1
		} else {
			0
		}) {
			let mut fmtlayer =
				SegmentBuffer::new([Segment::new(vec![' '; max_size + 1], Formatting::default())]);

			for annotation in layer
				.iter()
				.map(|i| annotations_by_id.get(i).expect("exists"))
			{
				for range in annotation.ranges.ranges() {
					let data = if range.start == range.end {
						vec![CONTINUE]
					} else {
						let mut out = vec![RANGE_START];
						out.resize(range.end - range.start, RANGE_CONTINUE);
						out.push(RANGE_END);
						out
					};
					fmtlayer.splice(
						range.start..=range.end,
						Some(SegmentBuffer::new([Segment::new(
							data,
							annotation.formatting.clone(),
						)])),
					);
				}
			}

			layers_pre.push(fmtlayer);
		}

		for (i, layer) in layers.iter().enumerate() {
			for other in layers_pre[i + 1..].iter_mut() {
				for annotation in layer
					.iter()
					.map(|i| annotations_by_id.get(i).expect("exists"))
				{
					for start in annotation.ranges.ranges().map(|r| r.start) {
						let (c, orig_fmt) = other.get(start).expect("extended to max");
						if let Some((keep_style, replacement)) = cross(c) {
							other.splice(
								start..=start,
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

	if opts.ratnest_sort {
		annotations.shuffle(&mut rand::thread_rng());
	} else {
		annotations.sort_by_key(|a| {
			(
				a.right.is_empty(),
				!a.left,
				Reverse(a.ranges.ranges().next().expect("not empty").start),
			)
		});
	}

	let mut layers = Vec::new();
	{
		use crate::chars::arrow::*;
		for annotation in &annotations {
			let mut fmtlayer =
				SegmentBuffer::new([Segment::new(vec![' '; max_size + 1], Formatting::default())]);
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
					start..=start,
					Some(SegmentBuffer::new([Segment::new(
						[c],
						annotation.formatting.clone(),
					)])),
				);
			}
			let annotation_id = if annotation.left {
				let size = min - min_pos;
				fmtlayer.splice(
					min_pos..min,
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

				let size = max_size - max + 2;
				fmtlayer.splice(
					max + 1..max_size + 1,
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
						vec![' '; max_size],
						Formatting::default(),
					)]);
					fmtlayer.extend(right.clone());
					extralayers.push((None, fmtlayer));
				}
			}
			for i in min..=max {
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
		for (i, annotation) in annotations.iter().enumerate() {
			for affected in layers[..i].iter_mut().flatten() {
				for start in annotation.ranges.ranges().map(|r| r.start) {
					let (c, orig_fmt) = affected.1.get(start).expect("extended to max");
					if let Some((keep_style, replacement)) = cross(c) {
						affected.1.splice(
							start..=start,
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
	for layer in layers_pre.iter().skip(if opts.first_layer_reformats_orig {
		1
	} else {
		0
	}) {
		out.push((None, layer.clone()));
	}
	for layer in layers.iter().flatten() {
		out.push(layer.clone())
	}

	(text, out)
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

		let mut caid = 0;
		let mut aid = || {
			caid += 1;
			AnnotationId(caid)
		};

		generate_segment(
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
			Text::single("012345678901234567890123456789012345".chars(), default()),
			&Opts {
				ratnest_sort: true,
				ratnest_merge: true,
				first_layer_reformats_orig: true,
			},
		);
	}
}
