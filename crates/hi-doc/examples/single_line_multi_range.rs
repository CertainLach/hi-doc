use hi_doc::{source_to_ansi, SnippetBuilder, Text};

fn main() {
	let mut snippet = SnippetBuilder::new("012345678901234567890");

	snippet
		.error(Text::default_fragment("a"))
		.range(0..=3)
		.range(10..=13)
		.build();
	snippet
		.error(Text::default_fragment("b"))
		.range(2..=2)
		.range(10..=13)
		.build();
	snippet
		.error(Text::default_fragment("c"))
		.range(3..=3)
		.range(10..=13)
		.build();

	println!("{}", source_to_ansi(&snippet.build()))
}
