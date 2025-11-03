use std::{
    iter::Sum,
    ops::{Add, AddAssign, Bound, Range, RangeBounds},
};

use miette::SourceSpan;

pub trait Spanned {
    fn span(&self) -> Span;
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    const NONE: Self = Self {
        start: usize::MAX,
        end: usize::MIN,
    };

    pub fn new(range: impl RangeBounds<usize>) -> Self {
        let (start, end) = match (range.start_bound(), range.end_bound()) {
            (Bound::Included(&s), Bound::Included(&e)) => (s, e + 1),
            (Bound::Included(&s), Bound::Excluded(&e)) => (s, e),
            (Bound::Included(&s), Bound::Unbounded) => panic!("Invalid range for span: {s}.."),
            (Bound::Excluded(&s), Bound::Included(&e)) => (s + 1, e + 1),
            (Bound::Excluded(&s), Bound::Excluded(&e)) => (s + 1, e),
            (Bound::Excluded(&s), Bound::Unbounded) => panic!("Invalid range for span: ({s}.."),
            (Bound::Unbounded, Bound::Included(&e)) => panic!("Invalid range for span: ..={e}"),
            (Bound::Unbounded, Bound::Excluded(&e)) => panic!("Invalid range for span: ..{e}"),
            (Bound::Unbounded, Bound::Unbounded) => panic!("Invalid range for span: .."),
        };
        Self { start, end }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub const fn none() -> Self {
        Self::NONE
    }

    pub const fn empty() -> Self {
        Self { start: 0, end: 0 }
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl Sum for Span {
    fn sum<I: Iterator<Item = Self>>(iter: I) -> Self {
        let mut span = Self::NONE;
        for x in iter {
            span += x;
        }

        if span == Self::NONE {
            Self::empty()
        } else {
            span
        }
    }
}

impl Add for Span {
    type Output = Span;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            start: std::cmp::min(self.start, rhs.start),
            end: std::cmp::max(self.end, rhs.end),
        }
    }
}

impl AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        self.start = std::cmp::min(self.start, rhs.start);
        self.end = std::cmp::max(self.end, rhs.end);
    }
}

impl From<(usize, usize)> for Span {
    fn from((start, end): (usize, usize)) -> Self {
        Self { start, end }
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self::new(range)
    }
}

impl From<Span> for SourceSpan {
    fn from(value: Span) -> Self {
        (value.start..value.end).into()
    }
}
