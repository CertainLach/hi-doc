extern crate hi_doc_jumprope as jumprope;

use std::{
	collections::{BTreeMap, BTreeSet, HashMap, HashSet},
	ops::RangeInclusive,
};

mod segment;
use annotation::{Annotation, AnnotationId, Opts};
use anomaly_fixer::{apply_fixup, fixup_byte_to_char, fixup_char_to_display};
pub use formatting::Text;
use rand::{rngs::SmallRng, Rng, SeedableRng};
use random_color::{
	options::{Gamut, Luminosity},
	RandomColor,
};
use range_map::{Range, RangeSet};
use segment::SegmentBuffer;
use single_line::LineAnnotation;

pub use crate::formatting::Formatting;

mod annotation;
mod anomaly_fixer;
pub(crate) mod associated_data;
mod chars;
mod formatting;
mod inline;
mod single_line;

#[derive(Clone, Debug)]
struct RawLine {
	data: Text,
}

#[derive(Debug)]
struct AnnotationLine {
	prefix: Text,
	line: Text,
	/// There will be lines drawn to connect lines with the same annotation id specified
	annotation: Option<AnnotationId>,
}

#[derive(Debug)]
struct GapLine {
	prefix: Text,
	line: Text,
}

#[derive(Debug)]
struct TextLine {
	prefix: Text,
	line_num: usize,
	line: Text,
	/// Is this line allowed to be hidden by fold?
	fold: bool,
	annotation: Option<AnnotationId>,
	annotations: Vec<LineAnnotation>,
	top_annotations: Vec<(Option<AnnotationId>, Text)>,
	bottom_annotations: Vec<(Option<AnnotationId>, Text)>,
}
impl TextLine {
	#[allow(dead_code)]
	fn add_prefix(&mut self, this: Text, annotations: Text) {
		self.prefix.extend([this]);
		for (_, ele) in self.bottom_annotations.iter_mut() {
			ele.splice(0..0, Some(annotations.clone()));
		}
	}
	fn len(&self) -> usize {
		self.line.len()
	}
	fn is_empty(&self) -> bool {
		self.line.is_empty()
	}
	// fn trim_end(&mut self) {
	// 	self.line.truncate(self.line.trim_end().len());
	// }
}

fn cons_slices<T>(mut slice: &mut [T], test: impl Fn(&T) -> bool) -> Vec<&mut [T]> {
	let mut out = Vec::new();

	while !slice.is_empty() {
		let mut skip = 0;
		while !slice.get(skip).map(&test).unwrap_or(true) {
			skip += 1;
		}
		let mut take = 0;
		while slice.get(skip + take).map(&test).unwrap_or(false) {
			take += 1;
		}
		let (_skipped, rest) = slice.split_at_mut(skip);
		let (taken, rest) = rest.split_at_mut(take);
		if !taken.is_empty() {
			out.push(taken);
		}
		slice = rest;
	}

	out
}

#[derive(Debug)]
enum Line {
	Text(TextLine),
	Annotation(AnnotationLine),
	Raw(RawLine),
	Nop,
	Gap(GapLine),
}
impl Line {
	fn text_mut(&mut self) -> Option<&mut Text> {
		Some(match self {
			Line::Text(t) => &mut t.line,
			Line::Gap(t) => &mut t.line,
			Line::Annotation(t) => &mut t.line,
			_ => return None,
		})
	}
	fn is_text(&self) -> bool {
		matches!(self, Self::Text(_))
	}
	fn is_annotation(&self) -> bool {
		matches!(self, Self::Annotation(_))
	}
	fn as_annotation(&self) -> Option<&AnnotationLine> {
		match self {
			Self::Annotation(a) => Some(a),
			_ => None,
		}
	}
	fn is_gap(&self) -> bool {
		matches!(self, Self::Gap(_))
	}
	fn as_text_mut(&mut self) -> Option<&mut TextLine> {
		match self {
			Line::Text(t) => Some(t),
			_ => None,
		}
	}
	#[allow(dead_code)]
	fn as_gap_mut(&mut self) -> Option<&mut GapLine> {
		match self {
			Line::Gap(t) => Some(t),
			_ => None,
		}
	}
	fn as_text(&self) -> Option<&TextLine> {
		match self {
			Line::Text(t) => Some(t),
			_ => None,
		}
	}
	fn as_raw(&self) -> Option<&RawLine> {
		match self {
			Line::Raw(r) => Some(r),
			_ => None,
		}
	}
	fn is_nop(&self) -> bool {
		matches!(self, Self::Nop)
	}
}

#[derive(Debug)]
pub struct Source {
	lines: Vec<Line>,
}

fn cleanup_nops(source: &mut Source) {
	let mut i = 0;
	while i < source.lines.len() {
		if source.lines[i].is_nop() {
			source.lines.remove(i);
		} else {
			i += 1;
		}
	}
}

/// Remove NOP/empty annotation lines
fn cleanup(source: &mut Source) {
	for slice in cons_slices(&mut source.lines, Line::is_text) {
		for line in slice
			.iter_mut()
			.take_while(|l| l.as_text().unwrap().is_empty())
		{
			*line = Line::Nop;
		}
		for line in slice
			.iter_mut()
			.rev()
			.take_while(|l| l.as_text().unwrap().is_empty())
		{
			*line = Line::Nop;
		}
	}
	cleanup_nops(source);
	for slice in cons_slices(&mut source.lines, Line::is_gap) {
		if slice.len() == 1 {
			continue;
		}
		for ele in slice.iter_mut().skip(1) {
			*ele = Line::Nop;
		}
	}
	cleanup_nops(source);
}

fn fold(source: &mut Source, opts: &Opts) {
	for slice in cons_slices(&mut source.lines, Line::is_text) {
		'line: for i in 0..slice.len() {
			for j in i.saturating_sub(opts.context_lines)..=(i + opts.context_lines) {
				let Some(ctx) = slice.get(j) else {
					continue;
				};
				let Line::Text(t) = ctx else {
					continue;
				};
				if t.fold {
					continue;
				}
				continue 'line;
			}
			slice[i] = Line::Gap(GapLine {
				prefix: Text::new(),
				line: Text::new(),
			});
		}
	}
	cleanup(source);
}

fn draw_line_numbers(source: &mut Source) {
	for lines in &mut cons_slices(&mut source.lines, |l| {
		l.is_annotation() || l.is_text() || l.is_gap()
	}) {
		let max_num = lines
			.iter()
			.filter_map(|l| match l {
				Line::Text(t) => Some(t.line_num),
				_ => None,
			})
			.max()
			.unwrap_or(0);
		let max_len = max_num.to_string().len();
		let prefix_segment =
			SegmentBuffer::segment(" ".repeat(max_len - 1), Formatting::line_number());
		for line in lines.iter_mut() {
			match line {
				Line::Text(t) => t.prefix.extend([SegmentBuffer::segment(
					format!("{:>width$} ", t.line_num, width = max_len),
					Formatting::line_number(),
				)]),
				Line::Annotation(a) => a.prefix.extend([
					prefix_segment.clone(),
					SegmentBuffer::segment("· ", Formatting::line_number()),
				]),
				Line::Gap(a) => a.prefix.extend([
					prefix_segment.clone(),
					SegmentBuffer::segment("⋮ ", Formatting::line_number()),
				]),
				_ => unreachable!(),
			}
		}
	}
}

fn draw_line_connections(
	source: &mut Source,
	annotation_formats: HashMap<AnnotationId, Formatting>,
) {
	for lines in &mut cons_slices(&mut source.lines, |l| {
		l.is_annotation() || l.is_text() || l.is_gap()
	}) {
		#[derive(Debug)]
		struct Connection {
			range: Range<usize>,
			connected: Vec<usize>,
		}

		let mut connected_annotations = HashMap::new();
		for (i, line) in lines.iter().enumerate() {
			let annotation = if let Some(annotation) = line.as_annotation() {
				annotation.annotation
			} else if let Some(text) = line.as_text() {
				text.annotation
			} else {
				None
			};
			if let Some(annotation) = annotation {
				let conn = connected_annotations
					.entry(annotation)
					.or_insert(Connection {
						range: Range::new(i, i),
						connected: Vec::new(),
					});
				conn.range.start = conn.range.start.min(i);
				conn.range.end = conn.range.end.max(i);
				conn.connected.push(i);
			}
		}
		let mut grouped = connected_annotations
			.iter()
			.map(|(k, v)| (*k, vec![v.range].into_iter().collect::<RangeSet<usize>>()))
			.collect::<Vec<_>>();

		grouped.sort_by_key(|a| a.1.num_elements());
		let grouped = single_line::group_nonconflicting(&grouped, &HashSet::new());

		for group in grouped {
			for annotation in group {
				let annotation_fmt = annotation_formats
					.get(&annotation)
					.expect("id is used in string but not defined")
					.clone()
					.decoration();
				let conn = connected_annotations.get(&annotation).expect("exists");
				let range = conn.range;
				let mut max_index = usize::MAX;
				for line in range.start..=range.end {
					match &lines[line] {
						Line::Text(t) if t.line.chars().all(|c| c.is_whitespace()) => {}
						Line::Text(t) => {
							let whitespaces =
								t.line.chars().take_while(|i| i.is_whitespace()).count();
							max_index = max_index.min(whitespaces)
						}
						Line::Annotation(t) if t.line.chars().all(|c| c.is_whitespace()) => {}
						Line::Annotation(t) => {
							let whitespaces =
								t.line.chars().take_while(|i| i.is_whitespace()).count();
							max_index = max_index.min(whitespaces)
						}
						Line::Gap(_) => {}
						_ => unreachable!(),
					}
				}
				while max_index < 2 {
					let seg = Some(SegmentBuffer::segment(
						" ".repeat(2 - max_index),
						annotation_fmt.clone(),
					));
					for line in lines.iter_mut() {
						match line {
							Line::Text(t) => t.line.splice(0..0, seg.clone()),
							Line::Annotation(t) => t.line.splice(0..0, seg.clone()),
							Line::Gap(t) => t.line.splice(0..0, seg.clone()),
							_ => unreachable!(),
						}
					}
					max_index = 2;
				}
				if max_index >= 2 {
					let offset = max_index - 2;

					for line in range.start..=range.end {
						use chars::line::*;
						let char = if range.start == range.end {
							RANGE_EMPTY
						} else if line == range.start {
							RANGE_START
						} else if line == range.end {
							RANGE_END
						} else if conn.connected.contains(&line) {
							RANGE_CONNECTION
						} else {
							RANGE_CONTINUE
						};
						let text = lines[line].text_mut().expect("only with text reachable");
						if text.len() <= offset {
							text.resize(offset + 1, ' ', annotation_fmt.clone());
						}
						text.splice(
							offset..offset + 1,
							Some(SegmentBuffer::segment(
								char.to_string(),
								annotation_fmt.clone(),
							)),
						);

						if conn.connected.contains(&line) {
							for i in offset + 1..text.len() {
								let (char, fmt) = text.get(i).expect("in bounds");
								if !text.get(i).expect("in bounds").0.is_whitespace()
									&& !fmt.decoration
								{
									break;
								}
								if let Some((keep_style, replacement)) = cross(char) {
									text.splice(
										i..=i,
										Some(SegmentBuffer::segment(
											replacement.to_string(),
											if keep_style {
												fmt.clone()
											} else {
												annotation_fmt.clone()
											},
										)),
									)
								}
							}
						}
					}
				}
			}
		}
	}
}

fn generate_annotations(source: &mut Source, opts: &Opts) {
	for line in source
		.lines
		.iter_mut()
		.flat_map(Line::as_text_mut)
		.filter(|t| !t.annotations.is_empty())
	{
		let hide_ranges_for = if opts.apply_to_orig {
			let parsed = inline::group_singleline(&line.annotations);
			assert!(line.annotation.is_none());
			line.annotation = parsed.annotation;
			inline::apply_inline_annotations(&mut line.line, &parsed.inline, parsed.right);

			line.annotations
				.retain(|a| !parsed.processed.contains(&a.id));
			line.fold = false;

			parsed.hide_ranges_for
		} else {
			HashSet::new()
		};

		let char_to_display_fixup = fixup_char_to_display(line.line.chars());
		let mut extra = single_line::generate_range_annotations(
			line.annotations.clone(),
			&char_to_display_fixup,
			&hide_ranges_for,
			false,
		);
		extra.reverse();
		// TODO: instead of writing generated annotations into lines, return them from this function, and apply later
		line.top_annotations = extra;
		line.annotations.truncate(0);
	}
}

fn apply_annotations(source: &mut Source) {
	// Top
	{
		let mut insertions = vec![];
		for (i, line) in source
			.lines
			.iter_mut()
			.enumerate()
			.flat_map(|(i, l)| l.as_text_mut().map(|t| (i, t)))
		{
			for buf in line.top_annotations.drain(..) {
				insertions.push((i + 1, buf))
			}
		}
		insertions.reverse();
		for (i, (annotation, line)) in insertions {
			source.lines.insert(
				i - 1,
				Line::Annotation(AnnotationLine {
					line,
					annotation,
					prefix: SegmentBuffer::new(),
				}),
			);
		}
	}
	// Bottom
	{
		let mut insertions = vec![];
		for (i, line) in source
			.lines
			.iter_mut()
			.enumerate()
			.flat_map(|(i, l)| l.as_text_mut().map(|t| (i, t)))
		{
			for buf in line.bottom_annotations.drain(..) {
				insertions.push((i + 1, buf))
			}
		}
		insertions.reverse();
		for (i, (annotation, line)) in insertions {
			source.lines.insert(
				i,
				Line::Annotation(AnnotationLine {
					line,
					annotation,
					prefix: SegmentBuffer::new(),
				}),
			);
		}
	}
}

fn process(
	source: &mut Source,
	annotation_formats: HashMap<AnnotationId, Formatting>,
	opts: &Opts,
) {
	cleanup(source);
	// Format inline annotations
	generate_annotations(source, opts);
	// Make gaps in files
	if opts.fold {
		fold(source, opts)
	}
	// Expand annotation buffers
	apply_annotations(source);
	// Connect annotation lines
	draw_line_connections(source, annotation_formats);
	// Apply line numbers
	draw_line_numbers(source);
	// To raw
	{
		for line in &mut source.lines {
			match line {
				Line::Text(t) => {
					let mut buf = SegmentBuffer::new();
					buf.extend([t.prefix.clone(), t.line.clone()]);
					*line = Line::Raw(RawLine { data: buf });
				}
				Line::Annotation(t) => {
					let mut buf = SegmentBuffer::new();
					buf.extend([t.prefix.clone(), t.line.clone()]);
					*line = Line::Raw(RawLine { data: buf })
				}
				Line::Gap(t) => {
					let mut buf = SegmentBuffer::new();
					buf.extend([t.prefix.clone(), t.line.clone()]);
					*line = Line::Raw(RawLine { data: buf })
				}
				Line::Raw(_) | Line::Nop => {}
			}
		}
	}
	cleanup(source);
}

fn linestarts(str: &str) -> BTreeSet<usize> {
	let mut linestarts = BTreeSet::new();
	for (i, c) in str.chars().enumerate() {
		if c == '\n' {
			linestarts.insert(i + 1);
		}
	}
	linestarts
}
struct LineCol {
	line: usize,
	column: usize,
}
fn offset_to_linecol(mut offset: usize, linestarts: &BTreeSet<usize>) -> LineCol {
	let mut line = 0;
	let last_offset = linestarts
		.range(..=offset)
		.inspect(|_| line += 1)
		.last()
		.copied()
		.unwrap_or(0);
	offset -= last_offset;
	LineCol {
		line,
		column: offset,
	}
}

fn parse(txt: &str, annotations: &[Annotation], opts: &Opts) -> Source {
	let (txt, byte_to_char_fixup) = fixup_byte_to_char(txt, opts.tab_width);
	let mut annotations = annotations.to_vec();

	// Convert byte offsets to char offsets
	for annotation in annotations.iter_mut() {
		let ranges: RangeSet<usize> = annotation
			.ranges
			.ranges()
			.map(|r| {
				let mut start = r.start;
				let mut end = r.end;
				apply_fixup(&mut start, &byte_to_char_fixup);
				apply_fixup(&mut end, &byte_to_char_fixup);
				Range::new(start, end)
			})
			.collect();
		annotation.ranges = ranges;
	}
	let linestarts = linestarts(&txt);

	let mut lines: Vec<Line> = txt
		.split('\n')
		.map(|s| s.to_string())
		.enumerate()
		.map(|(num, line)| TextLine {
			line_num: num + 1,
			line: SegmentBuffer::segment(
				// Reserve 1 char for the spans pointing to EOL
				line.chars().chain([' ']).collect::<String>(),
				Formatting::default(),
			),
			annotation: None,
			prefix: SegmentBuffer::new(),
			annotations: Vec::new(),
			bottom_annotations: Vec::new(),
			top_annotations: Vec::new(),
			fold: true,
		})
		.map(Line::Text)
		.collect();

	for (aid, annotation) in annotations.iter().enumerate() {
		let mut line_ranges: BTreeMap<usize, RangeSet<usize>> = BTreeMap::new();
		for range in annotation.ranges.ranges() {
			let start = offset_to_linecol(range.start, &linestarts);
			let end = offset_to_linecol(range.end, &linestarts);

			if start.line == end.line {
				let set = line_ranges.entry(start.line).or_insert_with(RangeSet::new);
				*set = set.union(&[Range::new(start.column, end.column)].into_iter().collect());
			} else {
				{
					let set = line_ranges.entry(start.line).or_insert_with(RangeSet::new);
					let line = lines[start.line].as_text().expect("annotation OOB");
					*set = set.union(
						&[Range::new(start.column, line.len() - 1)]
							.into_iter()
							.collect(),
					);
				}
				{
					let set = line_ranges.entry(end.line).or_insert_with(RangeSet::new);
					*set = set.union(&[Range::new(0, end.column)].into_iter().collect());
				}
			}
		}
		let left = line_ranges.len() > 1;
		let line_ranges_len = line_ranges.len();

		for (i, (line, ranges)) in line_ranges.into_iter().enumerate() {
			let last = i == line_ranges_len - 1;
			let line = lines[line].as_text_mut().expect("annotation OOB");
			line.annotations.push(LineAnnotation {
				id: AnnotationId(aid),
				priority: annotation.priority,
				ranges,
				formatting: annotation.formatting.clone(),
				left,
				right: if last {
					annotation.text.clone()
				} else {
					Text::new()
				},
			});
			line.fold = false;
		}
	}

	let mut source = Source { lines };

	let annotation_formats = annotations
		.iter()
		.enumerate()
		.map(|(aid, a)| (AnnotationId(aid), a.formatting.clone()))
		.collect();

	process(&mut source, annotation_formats, opts);

	source
}

pub fn source_to_ansi(source: &Source) -> String {
	let mut out = String::new();
	for line in &source.lines {
		let line = line
			.as_raw()
			.expect("after processing all lines should turn raw");
		let data = line.data.clone();
		formatting::text_to_ansi(&data, &mut out);
		out.push('\n');
	}
	out
}

pub struct FormattingGenerator {
	rand: SmallRng,
}
impl FormattingGenerator {
	pub fn new(src: &[u8]) -> Self {
		let mut rng_seed = [0; 8];
		// let seed = seed.to_value();
		for chunk in src.chunks(8) {
			for (s, c) in rng_seed.iter_mut().zip(chunk.iter()) {
				*s ^= *c;
			}
		}

		Self {
			rand: SmallRng::seed_from_u64(u64::from_be_bytes(rng_seed)),
		}
	}
	fn next(&mut self) -> RandomColor {
		let mut color = RandomColor::new();
		color.seed(self.rand.gen::<u64>());
		color.luminosity(Luminosity::Bright);
		color
	}
}

pub struct SnippetBuilder {
	src: String,
	generator: FormattingGenerator,
	annotations: Vec<Annotation>,
}
impl SnippetBuilder {
	pub fn new(src: impl AsRef<str>) -> Self {
		Self {
			src: src.as_ref().to_string(),
			generator: FormattingGenerator::new(src.as_ref().as_bytes()),
			annotations: Vec::new(),
		}
	}
	fn custom(&mut self, custom_color: Gamut, text: Text) -> AnnotationBuilder<'_> {
		let mut color = self.generator.next();
		color.hue(custom_color);
		let formatting = Formatting::rgb(color.to_rgb_array());
		// FIXME: apply_meta is not implemented
		// let [r, g, b] = color.luminosity(Luminosity::Light).to_rgb_array();
		// text.apply_meta(
		// 	0..text.len(),
		// 	&AddColorToUncolored(u32::from_be_bytes([r, g, b, 0])),
		// );
		AnnotationBuilder {
			snippet: self,
			priority: 0,
			formatting,
			ranges: Vec::new(),
			text,
		}
	}
	pub fn error(&mut self, text: Text) -> AnnotationBuilder<'_> {
		self.custom(Gamut::Red, text)
	}
	pub fn warning(&mut self, text: Text) -> AnnotationBuilder<'_> {
		self.custom(Gamut::Orange, text)
	}
	pub fn note(&mut self, text: Text) -> AnnotationBuilder<'_> {
		self.custom(Gamut::Green, text)
	}
	pub fn info(&mut self, text: Text) -> AnnotationBuilder<'_> {
		self.custom(Gamut::Blue, text)
	}
	pub fn build(self) -> Source {
		parse(
			&self.src,
			&self.annotations,
			&Opts {
				apply_to_orig: true,
				fold: true,
				tab_width: 4,
				context_lines: 2,
			},
		)
	}
}

#[must_use]
pub struct AnnotationBuilder<'s> {
	snippet: &'s mut SnippetBuilder,
	priority: usize,
	formatting: Formatting,
	ranges: Vec<Range<usize>>,
	text: Text,
}

impl AnnotationBuilder<'_> {
	pub fn range(mut self, range: RangeInclusive<usize>) -> Self {
		assert!(
			*range.end() < self.snippet.src.len(),
			"out of bounds annotation"
		);
		self.ranges.push(Range::new(*range.start(), *range.end()));
		self
	}
	pub fn ranges(mut self, ranges: impl IntoIterator<Item = RangeInclusive<usize>>) -> Self {
		for range in ranges {
			self = self.range(range);
		}
		self
	}
	pub fn build(self) {
		self.snippet.annotations.push(Annotation {
			priority: self.priority,
			formatting: self.formatting,
			ranges: self.ranges.into_iter().collect(),
			text: self.text,
		});
	}
}

#[cfg(test)]
mod tests {
	use super::*;

	fn default<T: Default>() -> T {
		Default::default()
	}

	#[test]
	fn readme() {
		let mut snippet = SnippetBuilder::new(include_str!("../../../fixtures/std.jsonnet"));
		snippet
			.error(Text::segment("Local defs", default()))
			.ranges([4..=8, 3142..=3146])
			.build();
		snippet
			.warning(Text::segment("Local name", default()))
			.range(10..=12)
			.build();
		snippet
			.info(Text::segment("Equals", default()))
			.range(14..=14)
			.build();
		snippet
			.note(Text::segment("Connected definition", default()))
			.ranges([3133..=3135, 6155..=6157])
			.build();
		snippet
			.note(Text::segment("Another connected definition", default()))
			.ranges([5909..=5913, 6062..=6066, 6242..=6244])
			.build();
		let s = snippet.build();
		println!("{}", source_to_ansi(&s))
	}

	#[test]
	fn test_fmt() {
		let mut snippet = SnippetBuilder::new(include_str!("../../../fixtures/std.jsonnet"));
		snippet
			.info(Text::segment("Hello world", default()))
			.range(2832..=3135)
			.build();
		snippet
			.warning(Text::segment("Conflict", default()))
			.range(2838..=2847)
			.build();
		snippet
			.error(Text::segment("Still has text", default()))
			.range(2839..=2846)
			.build();
		let s = snippet.build();
		println!("{}", source_to_ansi(&s))
	}

	#[test]
	fn fullwidth_marker() {
		let mut snippet = SnippetBuilder::new("ＡＢＣ");
		snippet
			.info(Text::segment("a", default()))
			.range(0..=2)
			.build();
		snippet
			.info(Text::segment("b", default()))
			.range(3..=5)
			.build();
		snippet
			.info(Text::segment("c", default()))
			.range(6..=8)
			.build();
		let s = snippet.build();
		println!("{}", source_to_ansi(&s))
	}

	#[test]
	fn fullwidth_marker_apply() {
		let s = parse(
			"ＡＢＣ",
			&[
				Annotation {
					priority: 0,
					formatting: Formatting::color(0xff000000),
					ranges: [Range::new(0, 2)].into_iter().collect(),
					text: Text::segment("a", default()),
				},
				Annotation {
					priority: 0,
					formatting: Formatting::color(0x00ff0000),
					ranges: [Range::new(3, 5)].into_iter().collect(),
					text: Text::segment("b", default()),
				},
				Annotation {
					priority: 0,
					formatting: Formatting::color(0x0000ff00),
					ranges: [Range::new(6, 8)].into_iter().collect(),
					text: Text::segment("c", default()),
				},
			],
			&Opts {
				apply_to_orig: true,
				fold: true,
				tab_width: 4,
				context_lines: 2,
			},
		);
		println!("{}", source_to_ansi(&s))
	}

	#[test]
	fn tab_in_normal_and_fullwidth() {
		let s = parse(
			"Ａ\tＢ\n\tＢ\na\tb\n\tb",
			&[
				Annotation {
					priority: 0,
					formatting: Formatting::color(0xff000000),
					ranges: [Range::new(17, 17)].into_iter().collect(),
					text: Text::segment("Line start", default()),
				},
				Annotation {
					priority: 0,
					formatting: Formatting::color(0x00ff0000),
					ranges: [Range::new(18, 18)].into_iter().collect(),
					text: Text::segment("Aligned", default()),
				},
			],
			&Opts {
				apply_to_orig: false,
				fold: false,
				tab_width: 4,
				context_lines: 2,
			},
		);
		dbg!(&s);
		println!("{}", source_to_ansi(&s))
	}

	#[test]
	fn example_from_annotate_snippets() {
		let src = r#") -> Option<String> {
	for ann in annotations {
		match (ann.range.0, ann.range.1) {
			(None, None) => continue,
			(Some(start), Some(end)) if start > end_index => continue,
			(Some(start), Some(end)) if start >= start_index => {
				let label = if let Some(ref label) = ann.label {
					format!(" {}", label)
				} else {
					String::from("")
				};
				return Some(format!(
					"{}{}{}",
					" ".repeat(start - start_index),
					"^".repeat(end - start),
					label
				));
			}
			_ => continue,
		}
	}"#;
		let mut snippet = SnippetBuilder::new(src);
		snippet
			.error(Text::segment(
				"expected `Option<String>` because of return type",
				default(),
			))
			.range(5..=18)
			.build();
		snippet
			.note(Text::segment(
				"expected enum `std::option::Option`",
				default(),
			))
			.range(22..=510)
			.build();
		let s = snippet.build();
		println!("{}", source_to_ansi(&s))
	}
}
