use std::{
	collections::{BTreeMap, BTreeSet, HashMap, HashSet},
	mem,
	ops::RangeInclusive,
};

use annotated_string::AnnotatedRope;
use annotation::{Annotation, AnnotationId, Opts};
use anomaly_fixer::{apply_fixup, fixup_byte_to_char, fixup_char_to_display};
pub use formatting::Text;
use rand::{rngs::SmallRng, Rng, SeedableRng};
use random_color::{
	options::{Gamut, Luminosity},
	RandomColor,
};
use range_map::{Range, RangeSet};
use single_line::LineAnnotation;
use unicode_box_drawing::bc;

pub use crate::formatting::Formatting;

use self::{
	annotation::AnnotationLocation,
	chars::{cross_horizontal, BoxCharacterExt, PreserveStyle},
};

mod annotation;
mod anomaly_fixer;
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
struct BorderLine {
	prefix: Text,
	line: Text,
	top_border: bool,
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
	Border(BorderLine),
}
impl Line {
	fn text_mut(&mut self) -> Option<&mut Text> {
		Some(match self {
			Line::Text(t) => &mut t.line,
			Line::Gap(t) => &mut t.line,
			Line::Annotation(t) => &mut t.line,
			Line::Border(t) => &mut t.line,
			_ => return None,
		})
	}
	fn is_text(&self) -> bool {
		matches!(self, Self::Text(_))
	}
	fn is_annotation(&self) -> bool {
		matches!(self, Self::Annotation(_))
	}
	fn is_border(&self) -> bool {
		matches!(self, Self::Border(_))
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
		l.is_annotation() || l.is_text() || l.is_gap() || l.is_border()
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
			AnnotatedRope::fragment(" ".repeat(max_len - 1), Formatting::line_number());
		for line in lines.iter_mut() {
			match line {
				Line::Text(t) => t.prefix.extend([
					AnnotatedRope::fragment(
						format!("{:>width$}  ", t.line_num, width = max_len),
						Formatting::line_number(),
					),
					AnnotatedRope::fragment("│ ", Formatting::border()),
				]),
				Line::Annotation(a) => a.prefix.extend([
					prefix_segment.clone(),
					AnnotatedRope::fragment("   · ", Formatting::border()),
				]),
				Line::Border(a) => a.prefix.extend([
					prefix_segment.clone(),
					AnnotatedRope::fragment(
						format!(
							"   {}{}{}",
							bc!(tr).mirror_vertical_if(a.top_border).char_round(),
							bc!(rl),
							bc!(rl),
						),
						Formatting::border(),
					),
				]),
				Line::Gap(a) => a.prefix.extend([
					prefix_segment.clone(),
					AnnotatedRope::fragment(
						format!("   {} ", bc!(tb).char_dotted_w4()),
						Formatting::border(),
					),
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
					let seg = Some(AnnotatedRope::fragment(
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
						let char = if range.start == range.end {
							bc!(r)
						} else if line == range.start {
							bc!(rb)
						} else if line == range.end {
							bc!(tr)
						} else if conn.connected.contains(&line) {
							bc!(trb)
						} else {
							bc!(tb)
						}
						.char_round();
						let text = lines[line].text_mut().expect("only with text reachable");
						if text.len() <= offset {
							text.resize(offset + 1, ' ', annotation_fmt.clone());
						}
						text.splice(
							offset..offset + 1,
							Some(AnnotatedRope::fragment(
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
								if let Some((keep_style, replacement)) = cross_horizontal(char) {
									text.splice(
										i..=i,
										Some(AnnotatedRope::fragment(
											replacement.to_string(),
											match keep_style {
												PreserveStyle::Keep => fmt.clone(),
												PreserveStyle::Replace => annotation_fmt.clone(),
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
		generate_annotations_line(line, opts);
	}
}

fn generate_annotations_line(line: &mut TextLine, opts: &Opts) {
	// We don't need ranges for those lines, because they are embedded into the code itself.
	let hide_ranges_for = if opts.colored_range_display && !opts.colorblind_output {
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

	let total = line.annotations.len();

	let (mut above, rest) = mem::take(&mut line.annotations)
		.into_iter()
		.partition::<Vec<LineAnnotation>, _>(|v| v.location.is_above() && !v.location.is_below());
	let (mut below, mut both) = rest
		.into_iter()
		.partition::<Vec<LineAnnotation>, _>(|v| v.location.is_below() && !v.location.is_above());

	let target_above = (total + above.len() - below.len()).div_ceil(2);
	let needed_above = target_above.saturating_sub(above.len());

	let below_both = both.split_off(needed_above.min(both.len()));
	let above_both = both;

	above.extend(above_both);
	below.extend(below_both);

	for (annotations, above) in [(above, true), (below, false)] {
		let mut extra = single_line::generate_range_annotations(
			annotations,
			&char_to_display_fixup,
			&hide_ranges_for,
			!above,
		);
		if above {
			extra.reverse();
			line.top_annotations = extra;
		} else {
			line.bottom_annotations = extra;
		}
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
					prefix: AnnotatedRope::new(),
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
					prefix: AnnotatedRope::new(),
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

	cleanup(source);
}

fn to_raw(source: &mut Source) {
	// To raw
	for line in &mut source.lines {
		match line {
			Line::Text(t) => {
				let mut buf = AnnotatedRope::new();
				buf.extend([t.prefix.clone(), t.line.clone()]);
				*line = Line::Raw(RawLine { data: buf });
			}
			Line::Annotation(t) => {
				let mut buf = AnnotatedRope::new();
				buf.extend([t.prefix.clone(), t.line.clone()]);
				*line = Line::Raw(RawLine { data: buf })
			}
			Line::Gap(t) => {
				let mut buf = AnnotatedRope::new();
				buf.extend([t.prefix.clone(), t.line.clone()]);
				*line = Line::Raw(RawLine { data: buf })
			}
			Line::Border(t) => {
				let mut buf = AnnotatedRope::new();
				buf.extend([t.prefix.clone(), t.line.clone()]);
				*line = Line::Raw(RawLine { data: buf })
			}
			Line::Raw(_) | Line::Nop => {}
		}
	}
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
#[derive(Debug, Clone, Copy)]
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

fn parse(
	txt: &str,
	annotations: &[Annotation],
	opts: &Opts,
	mut highlights: Vec<(RangeInclusive<usize>, Formatting)>,
) -> Source {
	let (txt, byte_to_char_fixup, decorations) = fixup_byte_to_char(txt, opts.tab_width);

	let mut annotations = annotations.to_vec();

	for (r, _) in highlights.iter_mut() {
		let (mut start, mut end_exclusive) = (*r.start(), *r.end() + 1);
		apply_fixup(&mut start, &byte_to_char_fixup);
		apply_fixup(&mut end_exclusive, &byte_to_char_fixup);
		*r = start..=end_exclusive - 1;
	}

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
		.map(|(num, line)| {
			let chars = line.chars().chain([' ']).collect::<String>();
			TextLine {
				line_num: num + 1,
				line: AnnotatedRope::fragment(
					// Reserve 1 char for the spans pointing to EOL
					chars,
					Formatting::default(),
				),
				annotation: None,
				prefix: AnnotatedRope::new(),
				annotations: Vec::new(),
				bottom_annotations: Vec::new(),
				top_annotations: Vec::new(),
				fold: true,
			}
		})
		.map(Line::Text)
		.collect();

	for (r, f) in highlights.iter() {
		let start = *r.start();
		let end = *r.end();
		let start = offset_to_linecol(start, &linestarts);
		let end = offset_to_linecol(end, &linestarts);

		for (relative_linenumber, line) in lines[start.line..=end.line].iter_mut().enumerate() {
			let i = relative_linenumber + start.line;
			if let Line::Text(text_line) = line {
				let start = if i == start.line { start.column } else { 0 };
				let end = if i == end.line {
					end.column
				} else {
					text_line.line.len() - 1
				};
				text_line.line.annotate_range(start..=end, f);
			}
		}
	}

	for pos in decorations.iter().copied() {
		let start = offset_to_linecol(pos, &linestarts);
		let line = &mut lines[start.line];
		if let Line::Text(text_line) = line {
			text_line
				.line
				.annotate_range(start.column..=start.column, &Formatting::listchar());
		}
	}

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
				location: annotation.location,
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

#[derive(Debug)]
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
		color.seed(self.rand.random::<u64>());
		color.luminosity(Luminosity::Bright);
		color
	}
}

#[derive(Debug)]
pub struct SnippetBuilder {
	src: String,
	generator: FormattingGenerator,
	annotations: Vec<Annotation>,
	highlights_before: Vec<(RangeInclusive<usize>, Formatting)>,

	file_name: Option<Text>,
}
impl SnippetBuilder {
	pub fn new(src: impl AsRef<str>) -> Self {
		Self {
			src: src.as_ref().to_string(),
			generator: FormattingGenerator::new(src.as_ref().as_bytes()),
			annotations: Vec::new(),
			highlights_before: Vec::new(),
			file_name: None,
		}
	}
	pub fn with_file_name(mut self, filename: impl AsRef<str>, url: Option<String>) -> Self {
		let mut formatting = Formatting::filename();
		if let Some(url) = url {
			formatting = formatting.url(url);
		}
		self.file_name = Some(Text::fragment(filename, formatting));
		self
	}
	#[cfg(feature = "tree-sitter")]
	pub fn highlight(
		&mut self,
		config: tree_sitter_highlight::HighlightConfiguration,
		fmt: impl Fn(usize, &str) -> Formatting,
	) {
		use tree_sitter_highlight::{Highlight, Highlighter};

		let mut highlighter = Highlighter::new();
		let iter = highlighter
			.highlight(&config, self.src.as_bytes(), None, |_| None)
			.expect("highlight");

		let mut highlights = Vec::new();
		let mut highlight: Option<Highlight> = None;
		for v in iter {
			let v = v.expect("e");
			match v {
				tree_sitter_highlight::HighlightEvent::Source { start, end } => {
					if let Some(hi) = &highlight {
						let f = fmt(hi.0, &self.src[start..end]);
						highlights.push((start..=end - 1, f));
					}
				}
				tree_sitter_highlight::HighlightEvent::HighlightStart(s) => {
					assert!(highlight.is_none());
					highlight = Some(s);
				}
				tree_sitter_highlight::HighlightEvent::HighlightEnd => {
					assert!(highlight.is_some());
					highlight = None;
				}
			}
		}
		self.highlights_before.extend(highlights);
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
			location: AnnotationLocation::AnyNotInline,
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
		let mut source = parse(
			&self.src,
			&self.annotations,
			&Opts {
				colored_range_display: true,
				fold: true,
				tab_width: 4,
				context_lines: 2,
				colorblind_output: false,
			},
			self.highlights_before,
		);

		if let Some(file_name) = self.file_name {
			draw_file_name(&mut source, file_name);
		}

		let line_numbers = true;
		if line_numbers {
			draw_line_numbers(&mut source);
		}

		to_raw(&mut source);
		source
	}
}

fn draw_file_name(source: &mut Source, file_name: Text) {
	source.lines.insert(
		0,
		Line::Border(BorderLine {
			prefix: AnnotatedRope::new(),
			line: [
				AnnotatedRope::fragment("[", Formatting::listchar()),
				file_name,
				AnnotatedRope::fragment("]", Formatting::listchar()),
			]
			.into_iter()
			.collect(),
			top_border: true,
		}),
	);
}

#[must_use]
#[derive(Debug)]
pub struct AnnotationBuilder<'s> {
	snippet: &'s mut SnippetBuilder,
	priority: usize,
	formatting: Formatting,
	ranges: Vec<Range<usize>>,
	text: Text,
	location: AnnotationLocation,
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
	fn location(mut self, location: AnnotationLocation) -> Self {
		assert!(
			matches!(self.location, AnnotationLocation::AnyNotInline),
			"location methods should only be called once"
		);
		self.location = location;
		self
	}
	pub fn any_inline(self) -> Self {
		self.location(AnnotationLocation::Any)
	}
	pub fn above(self) -> Self {
		self.location(AnnotationLocation::Above)
	}
	pub fn below(self) -> Self {
		self.location(AnnotationLocation::Below)
	}
	pub fn above_or_inline(self) -> Self {
		self.location(AnnotationLocation::AboveOrInline)
	}
	pub fn below_or_inline(self) -> Self {
		self.location(AnnotationLocation::BelowOrInline)
	}
	pub fn build(self) {
		self.snippet.annotations.push(Annotation {
			priority: self.priority,
			formatting: self.formatting,
			ranges: self.ranges.into_iter().collect(),
			text: self.text,
			location: self.location,
		});
	}
}

#[cfg(test)]
mod tests {
	// #[test]
	// fn fullwidth_marker_apply() {
	// 	let s = parse(
	// 		"ＡＢＣ",
	// 		&[
	// 			Annotation {
	// 				priority: 0,
	// 				formatting: Formatting::color(0xff000000),
	// 				ranges: [Range::new(0, 2)].into_iter().collect(),
	// 				text: Text::fragment("a", default()),
	// 				location: AnnotationLocation::BelowOrInline,
	// 			},
	// 			Annotation {
	// 				priority: 0,
	// 				formatting: Formatting::color(0x00ff0000),
	// 				ranges: [Range::new(3, 5)].into_iter().collect(),
	// 				text: Text::fragment("b", default()),
	// 				location: AnnotationLocation::BelowOrInline,
	// 			},
	// 			Annotation {
	// 				priority: 0,
	// 				formatting: Formatting::color(0x0000ff00),
	// 				ranges: [Range::new(6, 8)].into_iter().collect(),
	// 				text: Text::fragment("c", default()),
	// 				location: AnnotationLocation::BelowOrInline,
	// 			},
	// 		],
	// 		&Opts {
	// 			colored_range_display: true,
	// 			fold: true,
	// 			tab_width: 4,
	// 			context_lines: 2,
	// 			colorblind_output: true,
	// 		},
	// 		vec![],
	// 	);
	// 	println!("{}", source_to_ansi(&s))
	// }

	// #[test]
	// fn tab_in_normal_and_fullwidth() {
	// 	let s = parse(
	// 		"Ａ\tＢ\n\tＢ\na\tb\n\tb",
	// 		&[
	// 			Annotation {
	// 				priority: 0,
	// 				formatting: Formatting::color(0xff000000),
	// 				ranges: [Range::new(17, 17)].into_iter().collect(),
	// 				text: Text::fragment("Line start", default()),
	// 				location: AnnotationLocation::Below,
	// 			},
	// 			Annotation {
	// 				priority: 0,
	// 				formatting: Formatting::color(0x00ff0000),
	// 				ranges: [Range::new(18, 18)].into_iter().collect(),
	// 				text: Text::fragment("Aligned", default()),
	// 				location: AnnotationLocation::Below,
	// 			},
	// 		],
	// 		&Opts {
	// 			colored_range_display: true,
	// 			fold: false,
	// 			tab_width: 4,
	// 			context_lines: 2,
	// 			colorblind_output: true,
	// 		},
	// 		vec![],
	// 	);
	// 	println!("{}", source_to_ansi(&s))
	// }
}
