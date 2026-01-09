use std::{
	cell,
	cmp::Ordering,
	fmt,
	iter::repeat_n,
	ops::{Bound, Range, RangeBounds},
};

use hi_doc_jumprope::{
	iter::{Chars as RopeChars, ContentIter, SliceIter, Substrings},
	JumpRope, JumpRopeBuf,
};

use crate::{AnnotatedRange, ApplyAnnotation};

#[derive(Clone, Debug)]
pub struct AnnotatedRope<M> {
	rope: JumpRopeBuf,
	annotations: AnnotatedRange<M>,
}

#[ouroboros::self_referencing]
pub struct Fragments<'r> {
	rope_buf: cell::Ref<'r, JumpRope>,
	#[borrows(rope_buf)]
	#[not_covariant]
	iter: Substrings<'this, SliceIter<'this>>,
}
impl<'f> Iterator for Fragments<'f> {
	type Item = &'f str;

	fn next(&mut self) -> Option<Self::Item> {
		let v = self.with_iter_mut(|s| s.next())?;

		// Safety: this str is located in rope_buf, yet it isn't
		// possible to explain this to jumprope
		//
		// This value will live as long as parent ropebuf, because
		// the iterator itself has a lock on jumprope (Ref), and
		// the string is borrowed from ouroboros.
		Some(unsafe {
			std::str::from_utf8_unchecked(std::slice::from_raw_parts(v.as_ptr(), v.len()))
		})
	}
}
#[ouroboros::self_referencing]
pub struct Chars<'r> {
	rope_buf: cell::Ref<'r, JumpRope>,
	#[borrows(rope_buf)]
	#[covariant]
	iter: RopeChars<'this, ContentIter<'this>>,
}
impl Iterator for Chars<'_> {
	type Item = char;

	fn next(&mut self) -> Option<Self::Item> {
		self.with_iter_mut(|c| c.next())
	}
}

fn bounds_to_exclusive(bounds: impl RangeBounds<usize>, len: usize) -> Range<usize> {
	let start = match bounds.start_bound() {
		Bound::Included(v) => *v,
		Bound::Excluded(_) => unreachable!("not creatable with standard syntax"),
		Bound::Unbounded => 0,
	};
	let end = match bounds.end_bound() {
		Bound::Included(i) => i + 1,
		Bound::Excluded(e) => *e,
		Bound::Unbounded => len,
	};
	start..end
}

impl<M: Clone + Default> AnnotatedRope<M> {
	pub fn default_fragment(v: impl AsRef<str>) -> Self {
		Self::fragment(v, M::default())
	}
}

impl<M: Clone> AnnotatedRope<M> {
	pub fn new() -> Self {
		Self {
			rope: JumpRopeBuf::new(),
			annotations: AnnotatedRange::new(),
		}
	}
	pub fn fragment(v: impl AsRef<str>, meta: M) -> Self {
		let v: String = v.as_ref().to_string();
		let rope: JumpRopeBuf = v.into();
		if rope.is_empty() {
			Self::new()
		} else {
			Self {
				annotations: AnnotatedRange::with_size(rope.len_chars(), meta),
				rope,
			}
		}
	}
	pub fn fragment_chars(v: impl IntoIterator<Item = char>, meta: M) -> Self {
		let v: String = v.into_iter().collect();
		let rope: JumpRopeBuf = v.into();
		if rope.is_empty() {
			Self::new()
		} else {
			Self {
				annotations: AnnotatedRange::with_size(rope.len_chars(), meta),
				rope,
			}
		}
	}
	#[deprecated = "use fragment_chars with repeated char"]
	pub fn repeated_char_fragment(char: char, count: usize, meta: M) -> Self {
		Self::fragment(char.to_string().repeat(count), meta)
	}
	pub fn insert(&mut self, position: usize, buf: Self) {
		let incoming = buf.rope.borrow();
		let mut offset = position;
		for (str, len) in incoming.substrings_with_len() {
			self.rope.insert(offset, str);
			offset += len;
		}
		self.annotations.insert(position, buf.annotations);
	}
	pub fn remove(&mut self, range: impl RangeBounds<usize>) {
		let range = bounds_to_exclusive(range, self.len());
		self.rope.remove(range.clone());
		self.annotations.remove(range);
	}
	pub fn splice(&mut self, range: impl RangeBounds<usize>, value: Option<Self>) {
		let range = bounds_to_exclusive(range, self.len());
		let start = range.start;
		self.remove(range);
		if let Some(value) = value {
			self.insert(start, value);
		}
	}
	pub fn extend(&mut self, buf: impl IntoIterator<Item = Self>) {
		for buf in buf {
			self.insert(self.annotations.len(), buf)
		}
	}
	pub fn append(&mut self, buf: Self) {
		self.extend([buf]);
	}
	pub fn fragments(&self) -> impl IntoIterator<Item = (Fragments<'_>, &'_ M)> {
		self.annotations.iter().map(|v| {
			(
				Fragments::new(self.rope.borrow(), |r| r.slice_substrings(v.1)),
				v.0,
			)
		})
	}
	pub fn len(&self) -> usize {
		self.rope.len_chars()
	}
	pub fn is_empty(&self) -> bool {
		self.rope.is_empty()
	}

	pub fn get(&self, pos: usize) -> Option<(char, &M)> {
		self.rope
			.borrow()
			.slice_chars(pos..pos + 1)
			.next()
			.map(|v| (v, self.annotations.get(pos).expect("meta is broken?")))
	}

	pub fn chars(&self) -> Chars<'_> {
		Chars::new(self.rope.borrow(), |v| v.chars())
	}
	pub fn resize(&mut self, size: usize, char: char, meta: M) {
		match size.cmp(&self.len()) {
			Ordering::Less => self.remove(size..),
			Ordering::Greater => self.extend([Self::fragment_chars(
				repeat_n(char, size - self.len()),
				meta,
			)]),
			Ordering::Equal => {}
		}
	}
	pub fn split_at(self, pos: usize) -> (Self, Self) {
		let rope = self.rope.into_inner();
		let mut left = rope.clone();
		left.remove(pos..left.len_chars());
		let mut right = rope;
		right.remove(0..pos);

		let meta = self.annotations.clone();
		let (meta_left, meta_right) = meta.split(pos);
		(
			AnnotatedRope {
				rope: left.into(),
				annotations: meta_left,
			},
			AnnotatedRope {
				rope: right.into(),
				annotations: meta_right,
			},
		)
	}
	pub fn index_of(&self, char: char) -> Option<usize> {
		self.chars().position(|v| v == char)
	}
	pub fn split(&self, char: char) -> Vec<Self> {
		let mut out = Vec::new();
		let mut v = self.clone();
		while let Some(pos) = v.index_of(char) {
			let (left, right) = v.split_at(pos);
			out.push(left);
			v = right;
		}
		out.push(v);
		out
	}
}
impl<M> AnnotatedRope<M> {}

impl<M: Clone + fmt::Debug> Default for AnnotatedRope<M> {
	fn default() -> Self {
		Self::new()
	}
}
impl<M: Clone + PartialEq + fmt::Debug> AnnotatedRope<M> {
	pub fn annotate_range<T>(&mut self, range: impl RangeBounds<usize>, value: &T)
	where
		M: ApplyAnnotation<T>,
	{
		self.annotations
			.apply_meta(bounds_to_exclusive(range, self.len()), value)
	}
}

impl<M: Clone> FromIterator<AnnotatedRope<M>> for AnnotatedRope<M> {
	fn from_iter<T: IntoIterator<Item = AnnotatedRope<M>>>(iter: T) -> Self {
		let mut rope = AnnotatedRope::new();
		rope.extend(iter);
		rope
	}
}
