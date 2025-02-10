use hi_doc::{source_to_ansi, SnippetBuilder, Text};

fn main() {
	let mut snippet = SnippetBuilder::new(include_str!("../../../fixtures/std.jsonnet"));
	snippet
		.info(Text::default_fragment("Hello world"))
		.range(2832..=3135)
		.build();
	snippet
		.warning(Text::default_fragment("Conflict"))
		.range(2838..=2847)
		.build();
	snippet
		.error(Text::default_fragment("Still has text"))
		.range(2839..=2846)
		.build();
	let s = snippet.build();
	println!("{}", source_to_ansi(&s))
}
