use hi_doc::{source_to_ansi, Formatting, SnippetBuilder, Text};
use tree_sitter_highlight::HighlightConfiguration;

fn main() {
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
	let mut snippet = SnippetBuilder::new(src)
		.with_file_name("source.rs", Some("file:/path/to/source.rs".to_owned()));
	let language = tree_sitter_rust::LANGUAGE;
	let mut config = HighlightConfiguration::new(
		language.into(),
		"rust",
		tree_sitter_rust::HIGHLIGHTS_QUERY,
		tree_sitter_rust::INJECTIONS_QUERY,
		"",
	)
	.expect("config");
	config.configure(&["punctuation.bracket", "keyword", "property"]);
	snippet.highlight(config, |name, _code| {
		if name == 1 {
			Formatting::rgb([255, 50, 50])
				.url("https://www.youtube.com/watch?v=dQw4w9WgXcQ".to_string())
		} else if name == 2 {
			Formatting::rgb([50, 150, 50])
		} else {
			Formatting::rgb([50, 255, 255])
		}
	});
	snippet
		.error(Text::fragment(
			"expected `Option<String>` because of return type",
			Formatting::default(),
		))
		.range(5..=18)
		.build();
	snippet
		.note(Text::fragment(
			"expected enum `std::option::Option`",
			Default::default(),
		))
		.range(22..=510)
		.build();
	let s = snippet.build();
	println!("{}", source_to_ansi(&s))
}
