//! Implementation becomes much fancier with cursors.

use itertools::Itertools;

use crate::segment::MetaApply;
use core::fmt;
use std::cell::Cell;
use std::collections::btree_map::Iter;
use std::collections::BTreeMap;
use std::iter::Peekable;
use std::ops::Range;

#[derive(PartialEq, Eq, PartialOrd, Ord, Default, Clone)]
struct MutableOffset(Cell<usize>);
impl MutableOffset {
	fn new(value: usize) -> Self {
		Self(Cell::new(value))
	}
	fn set(&self, value: usize) {
		self.0.set(value)
	}
	fn get(&self) -> usize {
		self.0.get()
	}
}
impl fmt::Debug for MutableOffset {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		fmt::Debug::fmt(&self.0.get(), f)
	}
}

#[derive(Debug, Clone)]
pub struct AssociatedData<D> {
	// Segment start => Data
	// As every segment needs to be annotated (Otherwise, use Option<D>),
	// no need to keep segment sizes here.
	//
	// TODO: Might be done without interior mutability, but BTreeMap provides
	// no mutable access to the keys.
	data: BTreeMap<MutableOffset, D>,
	len: usize,
}
fn assert_none<T>(v: Option<T>) {
	debug_assert!(v.is_none());
}
impl<D: Clone + fmt::Debug> AssociatedData<D> {
	pub fn new() -> Self {
		Self {
			data: BTreeMap::new(),
			len: 0,
		}
	}
	pub fn with_size(size: usize, data: D) -> Self {
		if size == 0 {
			Self::new()
		} else {
			Self {
				data: [(MutableOffset::new(0), data)].into_iter().collect(),
				len: size,
			}
		}
	}
	pub fn append(&mut self, size: usize, data: D) {
		if size == 0 {
			return;
		}
		self.data.insert(MutableOffset::new(self.len), data);
		self.len += size;
	}
	pub fn insert(&mut self, pos: usize, value: Self) {
		if value.len == 0 {
			return;
		}
		if pos == self.len {
			// Otherwise will fail prev_meta get check on empty buffer.
			self.data.extend(
				value
					.data
					.into_iter()
					.map(|(k, v)| (MutableOffset::new(k.get() + self.len), v)),
			);
			self.len += value.len;
			return;
		}
		assert!(pos < self.len);

		let (prev_meta, prev_pos) = self
			.data
			.range(..=MutableOffset::new(pos))
			.next_back()
			.map(|(k, v)| (v, k.get()))
			.expect("element at idx 0 always has associated meta");

		// .rev() for prevent self-overriding
		for (key, _) in self.data.range(MutableOffset::new(pos)..).rev() {
			key.set(key.get() + value.len);
		}

		if prev_pos < pos {
			// If not inserting before element, then splitting current meta
			// at insertion place.
			assert_none(
				self.data
					.insert(MutableOffset::new(pos + value.len), prev_meta.clone()),
			);
		}

		for (off, data) in value.data {
			assert_none(self.data.insert(MutableOffset::new(pos + off.get()), data));
		}
		self.len += value.len;
	}
	pub fn remove(&mut self, range: Range<usize>) {
		let pos = range.start;
		let size = range.len();
		if size == 0 {
			return;
		}
		assert!(pos <= self.len - size);

		let mut removed_keys = Vec::new();
		let mut removed_range = self
			.data
			.range(MutableOffset::new(pos)..MutableOffset::new(pos + size));
		let meta = removed_range.next_back().map(|(key, v)| {
			removed_keys.push(key.get());
			// TODO: take
			v.clone()
		});
		for (key, _) in removed_range {
			removed_keys.push(key.get());
		}
		for key in removed_keys {
			self.data.remove(&MutableOffset::new(key));
		}
		for (key, _) in self.data.range(MutableOffset::new(pos)..) {
			key.set(key.get() - size);
		}
		if let Some(meta) = meta {
			// If there is no range starting right after deleted - insert start of meta from what was deleted.
			self.data.entry(MutableOffset::new(pos)).or_insert(meta);
		}

		self.len -= size;
	}
	pub fn concat(&mut self, other: Self) {
		if other.is_empty() {
			return;
		}
		let offset = self.len;
		self.len += other.len;
		self.data.extend(other.data.into_iter().map(|(k, v)| {
			k.set(k.get() + offset);
			(k, v)
		}))
	}
	pub fn split(mut self, offset: usize) -> (Self, Self) {
		assert!(offset <= self.len);
		if offset == 0 {
			return (Self::new(), self);
		}
		if offset == self.len {
			return (self, Self::new());
		}

		#[allow(clippy::mutable_key_type, reason = "invariants are held")]
		let mut other = self.data.split_off(&MutableOffset(Cell::new(offset)));

		// Do we need to clone split element?
		let mut split_in_the_middle = true;

		for (i, (pos, _)) in other.iter_mut().enumerate() {
			pos.set(pos.get() - offset);
			if i == 0 && pos.get() == 0 {
				split_in_the_middle = false;
			}
		}

		if split_in_the_middle {
			let mid_data = self
				.data
				.range(..MutableOffset::new(offset))
				.next_back()
				.expect("not empty");
			assert_none(other.insert(MutableOffset::new(0), mid_data.1.clone()));
		}

		let other_len = self.len - offset;
		self.len = offset;
		(
			self,
			Self {
				data: other,
				len: other_len,
			},
		)
	}

	pub fn is_empty(&self) -> bool {
		self.len == 0
	}
	pub fn len(&self) -> usize {
		self.len
	}
	pub fn iter(&self) -> AssocIterator<'_, D> {
		AssocIterator {
			inner: self.data.iter().peekable(),
			total_size: self.len,
		}
	}
	pub fn get(&self, pos: usize) -> Option<&D> {
		self.data
			.range(..=MutableOffset::new(pos))
			.next_back()
			.map(|v| v.1)
	}
	// Make sure there is an element at pos
	fn cut(&mut self, pos: usize) {
		debug_assert!(pos <= self.len);
		let mut iter = self.data.range_mut(..=MutableOffset::new(pos));
		let (item_pos, item_at_pos) = iter.next_back().expect("we always have 0th element");
		if item_pos.get() == pos {
			// Already split
			return;
		}
		let data = item_at_pos.clone();
		self.data.insert(MutableOffset::new(pos), data);
	}
}
impl<D: Clone + PartialEq> AssociatedData<D> {
	// Compare meta at chunks on `(pos..)` and `(..pos-1)`
	// If it equals - merge those two chunks
	fn merge_hint(&mut self, pos: usize) {
		let Some((a, b)) = self
			.data
			.range_mut(..=MutableOffset::new(pos))
			.rev()
			.next_tuple()
		else {
			// pos it at first chunk
			return;
		};
		if a.1 != b.1 {
			// Inequal - do not merge
			return;
		}
		// Expand left chunk automatically
		self.data.remove(&MutableOffset::new(pos));
	}
}

impl<D: Clone + PartialEq + fmt::Debug> AssociatedData<D> {
	pub fn apply_meta<T>(&mut self, range: Range<usize>, change: &T)
	where
		D: MetaApply<T>,
	{
		self.cut(range.start);
		self.cut(range.end);
		for (_, meta) in self
			.data
			.range_mut(MutableOffset::new(range.start)..MutableOffset::new(range.end))
		{
			meta.apply(change);
		}
		self.merge_hint(range.start);
		self.merge_hint(range.end);
	}
}
pub struct AssocIterator<'a, D> {
	inner: Peekable<Iter<'a, MutableOffset, D>>,
	total_size: usize,
}
impl<'a, D> Iterator for AssocIterator<'a, D> {
	type Item = (&'a D, Range<usize>);

	fn next(&mut self) -> Option<Self::Item> {
		let this = self.inner.next()?;
		let offset_of_this = this.0.get();
		let offset_of_next = self
			.inner
			.peek()
			.map(|v| v.0.get())
			.unwrap_or(self.total_size);
		Some((this.1, offset_of_this..offset_of_next))
	}
}

#[test]
fn assoc_smoke() {
	let mut data = <AssociatedData<char>>::new();
	dbg!(&data);
	data.insert(0, AssociatedData::with_size(3, 'a'));
	data.insert(2, AssociatedData::with_size(1, 'c'));
	data.insert(0, AssociatedData::with_size(3, 'b'));
	data.remove(1..3);
	dbg!(&data.split(1));
}
