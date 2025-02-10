use hi_doc::{source_to_ansi, SnippetBuilder, Text};

fn main() {
	let mut snippet = SnippetBuilder::new("ＡＢＣ");
	snippet
		.info(Text::default_fragment("a"))
		.range(0..=2)
		.above()
		.build();
	snippet
		.info(Text::default_fragment("b"))
		.range(3..=5)
		.above()
		.build();
	snippet
		.info(Text::default_fragment("c"))
		.range(6..=8)
		.above()
		.build();
	let s = snippet.build();
	println!("{}", source_to_ansi(&s))
}
