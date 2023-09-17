This crate provides a single type, [`ByKey`], that wraps a value and implements comparisons and hashing in terms of the provided key function.

# Example

```rust
// Generic function that returns pointer underlying a `rc::Weak`.
struct WeakAsPtr;
impl<T> bykey::KeyFunc<std::rc::Weak<T>> for WeakAsPtr {
    type Key<'a> = *const T where T: 'a;
    fn key(value: &std::rc::Weak<T>) -> *const T {
      value.as_ptr()
    }
}

// Wrapper around `std::rc::Weak` that compares by-pointer.
type WeakKey<T> = bykey::ByKey<std::rc::Weak<T>, WeakAsPtr>;

// `WeakKey`s of incomparable types can be used as keys.
struct Incomparable;
let mut s = std::collections::HashSet::new();
let r = std::rc::Rc::new(Incomparable);
s.insert(WeakKey::new(std::rc::Rc::downgrade(&r)));
```