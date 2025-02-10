mod annotated_range;
mod annotation;
#[cfg(feature = "rope")]
mod rope;

pub use annotated_range::AnnotatedRange;
pub use annotation::{ApplyAnnotation, Annotation};

#[cfg(feature = "rope")]
pub use rope::AnnotatedRope;
