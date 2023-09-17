#![no_std]
#![forbid(unsafe_code)]
#![doc = include_str!("../README.md")]

#[cfg(doc)]
extern crate std;

#[cfg(test)]
extern crate std;

pub trait KeyFunc<V> {
    type Key<'a>
    where
        V: 'a;

    fn key(value: &V) -> Self::Key<'_>;
}

/// A wrapper around `V` that implements comparisons and hashing in terms of the provided key.
pub struct ByKey<V, F> {
    value: V,
    _phantom_f: core::marker::PhantomData<F>,
}

impl<V, F> ByKey<V, F> {
    pub fn new(value: V) -> Self {
        Self {
            value,
            _phantom_f: core::marker::PhantomData,
        }
    }

    pub fn into_inner(self) -> V {
        self.value
    }

    pub fn inner(&self) -> &V {
        &self.value
    }
}

impl<V, F: KeyFunc<V>> ByKey<V, F> {
    pub fn key(&self) -> F::Key<'_> {
        F::key(&self.value)
    }
}

// Note that `ByKey` must not implement `std::borrow::Borrow` because that requires equality and
// comparison to agree with those of the borrowed type.

impl<V: Clone, F> Clone for ByKey<V, F> {
    fn clone(&self) -> Self {
        Self::new(self.value.clone())
    }
}

impl<V: Copy, F> Copy for ByKey<V, F> {}

impl<V: core::fmt::Debug, F> core::fmt::Debug for ByKey<V, F> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_tuple("ByKey").field(&self.value).finish()
    }
}

impl<V, F: KeyFunc<V>> PartialEq for ByKey<V, F>
where
    for<'a> F::Key<'a>: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key()
    }
}

impl<V, F: KeyFunc<V>> Eq for ByKey<V, F> where for<'a> F::Key<'a>: Eq {}

impl<V, F: KeyFunc<V>> core::hash::Hash for ByKey<V, F>
where
    for<'a> F::Key<'a>: core::hash::Hash,
{
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.key().hash(state)
    }
}

impl<V, F: KeyFunc<V>> PartialOrd for ByKey<V, F>
where
    for<'a> F::Key<'a>: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        self.key().partial_cmp(&other.key())
    }
}

impl<V, F: KeyFunc<V>> Ord for ByKey<V, F>
where
    for<'a> F::Key<'a>: Ord,
{
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.key().cmp(&other.key())
    }
}

impl<V, F> From<V> for ByKey<V, F> {
    fn from(value: V) -> Self {
        Self::new(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::*;
    use proptest::prelude::*;
    use std::format;
    use std::hash::{Hash, Hasher};

    type TestByKey<T> = ByKey<T, ()>;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    struct Unique(usize);

    static NEXT_UNIQUE: core::sync::atomic::AtomicUsize = core::sync::atomic::AtomicUsize::new(0);

    impl Unique {
        fn new() -> Self {
            Unique(NEXT_UNIQUE.fetch_add(1, core::sync::atomic::Ordering::SeqCst))
        }
    }

    #[test]
    fn into_inner() {
        let u = Unique::new();
        assert_eq!(TestByKey::new(u).into_inner(), u);
    }

    #[test]
    fn inner() {
        let u = Unique::new();
        assert_eq!(TestByKey::new(u).inner(), &u);
    }

    #[test]
    fn clone() {
        let u = Unique::new();
        assert_eq!(TestByKey::new(u).clone().into_inner(), u);
    }

    #[derive(Clone, Copy, Debug)]
    struct Incomparable<T>(T);

    struct IncomparableKeyFunc;

    impl<T: 'static> KeyFunc<Incomparable<T>> for IncomparableKeyFunc {
        type Key<'a> = &'a T;

        fn key(v: &Incomparable<T>) -> Self::Key<'_> {
            &v.0
        }
    }

    type IncomparableByKey<T> = ByKey<Incomparable<T>, IncomparableKeyFunc>;

    fn incomparable_by_key<V>(v: V) -> IncomparableByKey<V> {
        Incomparable(v).into()
    }

    // Define `MockEq`.
    mock! {
        Eq {
            fn identity(&self) -> Unique;
        }
        impl PartialEq for Eq {
            fn eq(&self, other: &Self) -> bool;
        }
        impl Eq for Eq {}
    }

    // Define `MockPartialOrd`.
    mock! {
        PartialOrd {
            fn identity(&self) -> Unique;
        }
        impl PartialOrd for PartialOrd {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering>;
        }
    }

    impl PartialEq for MockPartialOrd {
        fn eq(&self, other: &Self) -> bool {
            self.partial_cmp(other) == Some(std::cmp::Ordering::Equal)
        }
    }

    // Define `MockOrd`.
    mock! {
        Ord {
            fn identity(&self) -> Unique;
        }
        impl Ord for Ord {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering;
        }
    }

    impl PartialEq for MockOrd {
        fn eq(&self, other: &Self) -> bool {
            self.cmp(other) == std::cmp::Ordering::Equal
        }
    }

    impl Eq for MockOrd {}

    impl PartialOrd for MockOrd {
        fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    // Define `MockHash`.
    mock! {
        Hash {
            fn writes(&self) -> std::vec::Vec<std::vec::Vec<u8>>;
        }
    }

    impl Hash for MockHash {
        fn hash<H: Hasher>(&self, state: &mut H) {
            for bytes in self.writes() {
                state.write(bytes.as_ref())
            }
        }
    }

    // Define `MockHasher`.
    mock! {
        Hasher {}
        impl Hasher for Hasher {
            fn finish(&self) -> u64;
            fn write(&mut self, bytes: &[u8]);
        }
    }

    proptest! {
        #[test]
        fn eq(result: bool) {
            let rid = Unique::new();
            let mut r = MockEq::new();
            r.expect_identity()
                .return_const(rid);
            let mut l = MockEq::new();
            l.expect_eq()
                .with(predicate::function(move |r: &MockEq| r.identity() == rid))
                .return_const(result);
            prop_assert_eq!(incomparable_by_key(l) == incomparable_by_key(r), result);
        }

        #[test]
        fn partial_cmp(result: Option<std::cmp::Ordering>) {
            let rid = Unique::new();
            let mut r = MockPartialOrd::new();
            r.expect_identity()
                .return_const(rid);
            let mut l = MockPartialOrd::new();
            l.expect_partial_cmp()
                .with(predicate::function(move |r: &MockPartialOrd| r.identity() == rid))
                .return_const(result);
            prop_assert_eq!(incomparable_by_key(l).partial_cmp(&incomparable_by_key(r)), result);
        }

        #[test]
        fn cmp(result: std::cmp::Ordering) {
            let rid = Unique::new();
            let mut r = MockOrd::new();
            r.expect_identity()
                .return_const(rid);
            let mut l = MockOrd::new();
            l.expect_cmp()
                .with(predicate::function(move |r: &MockOrd| r.identity() == rid))
                .return_const(result);
            prop_assert_eq!(incomparable_by_key(l).cmp(&incomparable_by_key(r)), result);
        }

        #[test]
        fn hash(writes: std::vec::Vec<std::vec::Vec<u8>>) {
            let mut hasher = MockHasher::new();
            let mut seq = Sequence::new();

            // Strictly speaking, this tests the implementation rather than the interface.
            // However, there's relatively little reason to stray from the naive implementation,
            // and this test has the advantage of ensuring that the hash is as good as that of the
            // key type while a strict test of the interface would not.
            for bytes in writes.iter() {
                hasher.expect_write()
                    .with(predicate::eq(bytes.clone()))
                    .times(1)
                    .return_const(())
                    .in_sequence(&mut seq);
            }

            let mut k = MockHash::new();
            k.expect_writes().return_const(writes);

            incomparable_by_key(k).hash(&mut hasher);
        }
    }
}
