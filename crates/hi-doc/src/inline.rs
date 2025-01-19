use std::{cmp::Reverse, collections::HashSet};

use range_map::RangeSet;

use crate::{
	annotation::AnnotationId,
	formatting::{Formatting, Text},
	single_line::LineAnnotation,
};

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
		.filter(|a| a.location.is_inline())
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
		.filter(|a| a.location.is_inline())
		.filter(|a| !processed.contains(&a.id))
		.filter(|a| !a.left && !a.right.is_empty() && a.right.chars().all(|c| c != '\n'))
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
			.filter(|a| a.left && a.right.chars().all(|c| c != '\n'))
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
			if most.right.is_empty() {
				right = None
			} else {
				right = Some((most.formatting.clone(), most.right.clone()));
			}
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
		text.extend([
			Text::segment(
				format!("{} ", '🢒'),
				formatting,
			),
			right,
		]);
	}
}
