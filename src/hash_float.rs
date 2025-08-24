use std::{
    hash::Hash,
    ops::{Deref, DerefMut},
};

#[derive(Copy, Clone, PartialEq, Debug, Default)]
#[repr(transparent)]
pub struct FloatExt<F>(F);

macro_rules! hf_impl {
    ($f: ty) => {
        impl From<$f> for FloatExt<$f> {
            fn from(value: $f) -> Self {
                Self(value)
            }
        }

        // Technically wrong, but close enough
        impl Eq for FloatExt<$f> {}

        impl Hash for FloatExt<$f> {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                state.write(&self.0.to_le_bytes())
            }
        }

        impl Deref for FloatExt<$f> {
            type Target = $f;

            fn deref(&self) -> &Self::Target {
                &self.0
            }
        }

        impl DerefMut for FloatExt<$f> {
            fn deref_mut(&mut self) -> &mut Self::Target {
                &mut self.0
            }
        }
    };
}

hf_impl!(f32);
hf_impl!(f64);
