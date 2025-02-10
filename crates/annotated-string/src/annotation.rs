pub trait Annotation: Clone {
	fn try_merge(&mut self, other: &Self) -> bool;
}
impl Annotation for usize {
	fn try_merge(&mut self, other: &Self) -> bool {
		if *self != *other {
			return false;
		}
		true
	}
}

pub trait ApplyAnnotation<T> {
	fn apply(&mut self, change: &T);
}
