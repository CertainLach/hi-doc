use hi_doc::{source_to_ansi, SnippetBuilder, Text};

fn main() {
	let mut snippet = SnippetBuilder::new(include_str!("../../../fixtures/std.jsonnet"))
		.with_file_name("readme.rs", None);
	snippet
		.error(Text::default_fragment("Local defs"))
		.ranges([4..=8, 3142..=3146])
		.build();
	snippet
		.warning(Text::default_fragment("Local name"))
		.range(10..=12)
		.build();
	snippet
		.info(Text::default_fragment("Equals"))
		.range(14..=14)
		.build();
	snippet
		.note(Text::default_fragment("Connected definition"))
		.ranges([3133..=3135, 6155..=6157])
		.build();
	snippet
		.note(Text::default_fragment("Another connected definition"))
		.ranges([5909..=5913, 6062..=6066, 6242..=6244])
		.build();
	let s = snippet.build();
	println!("{}", source_to_ansi(&s))
}
