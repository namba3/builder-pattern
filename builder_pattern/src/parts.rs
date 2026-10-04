/// Marker types that can safely yield their initialized field value.
pub trait Ready<T> {
    fn into_inner(self) -> T;
}

#[repr(transparent)]
pub struct Certain<T>(T);
impl<T> Certain<T> {
    #[inline]
    pub const fn new(t: T) -> Self {
        Self(t)
    }
}
impl<T> Ready<T> for Certain<T> {
    fn into_inner(self) -> T {
        self.0
    }
}

/// Type-state wrapper for possibly uninitialized T storage.
///
/// Dropping this wrapper never drops a stored T. Builder transitions must
/// move initialized storage into its initialized state so the value is dropped
/// exactly once.
///
/// # Exmple
/// ```
/// use builder_pattern::parts::Uninit;
///
/// fn main() {
///     let data = Uninit::<String>::uninit();
///     drop(data); // ok
///
///     let data = Uninit::new(String::from("test"));
///     // Transfer the initialized value before dropping the wrapper.
///     let data: String = unsafe { data.assume_init() };
///     drop(data); // ok, not leaks the string
/// }
/// ```
#[repr(transparent)]
pub struct Uninit<T>(core::mem::MaybeUninit<T>);
impl<T> Uninit<T> {
    #[inline]
    // Keep the explicit pair `uninit` / `new` for uninitialized / initialized storage.
    #[allow(clippy::self_named_constructors)]
    pub const fn uninit() -> Self {
        Self(core::mem::MaybeUninit::uninit())
    }
    #[inline]
    pub const fn new(t: T) -> Self {
        Self(core::mem::MaybeUninit::new(t))
    }

    /// Extracts the initialized value from this wrapper.
    ///
    /// # Safety
    ///
    /// The storage must have been initialized with a valid value of T.
    #[inline]
    pub unsafe fn assume_init(self) -> T {
        // SAFETY: upheld by the caller.
        unsafe { self.0.assume_init() }
    }
}
#[repr(transparent)]
pub struct False(bool);
impl False {
    #[inline]
    pub const fn new() -> False {
        Self(false)
    }
}
impl core::default::Default for False {
    fn default() -> Self {
        Self::new()
    }
}
impl Ready<bool> for False {
    fn into_inner(self) -> bool {
        self.0
    }
}

#[repr(transparent)]
pub struct True(bool);
impl True {
    #[inline]
    pub const fn new() -> True {
        Self(true)
    }
}
impl core::default::Default for True {
    fn default() -> Self {
        Self::new()
    }
}
impl Ready<bool> for True {
    fn into_inner(self) -> bool {
        self.0
    }
}

#[repr(transparent)]
pub struct None<T>(Option<T>);
impl<T> None<T> {
    #[inline]
    pub const fn new() -> None<T> {
        Self(Option::None)
    }
}
impl<T> core::default::Default for None<T> {
    fn default() -> Self {
        Self::new()
    }
}
impl<T> Ready<Option<T>> for None<T> {
    fn into_inner(self) -> Option<T> {
        self.0
    }
}

#[repr(transparent)]
pub struct Some<T>(Option<T>);
impl<T> Some<T> {
    #[inline]
    pub const fn new(t: T) -> Self {
        Self(Option::Some(t))
    }
}
impl<T> Ready<Option<T>> for Some<T> {
    fn into_inner(self) -> Option<T> {
        self.0
    }
}

#[repr(transparent)]
pub struct Vec<T>(std::vec::Vec<T>);
impl<T> Vec<T> {
    #[inline]
    pub const fn new() -> Self {
        Self(std::vec::Vec::new())
    }
    #[inline]
    pub fn push(&mut self, t: T) {
        self.0.push(t);
    }
    #[inline]
    pub fn extend<Iter: core::iter::IntoIterator<Item = T>>(&mut self, iter: Iter) {
        self.0.extend(iter)
    }
}
impl<T> core::default::Default for Vec<T> {
    fn default() -> Self {
        Self::new()
    }
}
impl<T> Ready<std::vec::Vec<T>> for Vec<T> {
    fn into_inner(self) -> std::vec::Vec<T> {
        self.0
    }
}

#[repr(transparent)]
pub struct Default<T>(T);
impl<T> Default<T> {
    #[inline]
    pub const fn new(t: T) -> Self {
        Self(t)
    }
}
impl<T> Ready<T> for Default<T> {
    fn into_inner(self) -> T {
        self.0
    }
}

#[repr(transparent)]
pub struct Fixed<T>(T);
impl<T> Fixed<T> {
    #[inline]
    pub const fn new(t: T) -> Self {
        Self(t)
    }
}
impl<T> Ready<T> for Fixed<T> {
    fn into_inner(self) -> T {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_ready<T, Value>()
    where
        T: Ready<Value>,
    {
    }

    #[test]
    fn value_markers_preserve_their_values() {
        assert_eq!(Certain::new(1).0, 1);
        assert_eq!(Default::new(2).0, 2);
        assert_eq!(Fixed::new(3).0, 3);
        assert_eq!(Certain::new(4).into_inner(), 4);
        assert_eq!(Default::new(5).into_inner(), 5);
        assert_eq!(Fixed::new(6).into_inner(), 6);
    }

    #[test]
    fn boolean_markers_represent_false_and_true() {
        assert!(!False::new().0);
        assert!(True::new().0);
        assert!(!False::new().into_inner());
        assert!(True::new().into_inner());
        assert!(!False::default().into_inner());
        assert!(True::default().into_inner());
    }

    #[test]
    fn option_markers_represent_none_and_some() {
        assert_eq!(None::<u8>::new().0, Option::None);
        assert_eq!(Some::new(7).0, Option::Some(7));
        assert_eq!(None::<u8>::new().into_inner(), Option::None);
        assert_eq!(Some::new(8).into_inner(), Option::Some(8));
        assert_eq!(None::<u8>::default().into_inner(), Option::None);
    }

    #[test]
    fn vector_marker_pushes_and_extends_values() {
        let mut values = Vec::new();
        values.push(1);
        values.extend([2, 3]);

        assert_eq!(values.0, std::vec![1, 2, 3]);
        assert_eq!(values.into_inner(), std::vec![1, 2, 3]);
        assert!(Vec::<u8>::default().into_inner().is_empty());
    }

    #[test]
    fn all_fully_initialized_markers_implement_ready() {
        assert_ready::<Certain<u8>, u8>();
        assert_ready::<Default<u8>, u8>();
        assert_ready::<Fixed<u8>, u8>();
        assert_ready::<False, bool>();
        assert_ready::<True, bool>();
        assert_ready::<None<u8>, Option<u8>>();
        assert_ready::<Some<u8>, Option<u8>>();
        assert_ready::<Vec<u8>, std::vec::Vec<u8>>();
    }

    #[test]
    fn uninitialized_string_storage_can_be_dropped() {
        let _storage = Uninit::<String>::uninit();
    }

    #[test]
    fn initialized_storage_yields_its_value() {
        let storage = Uninit::new(String::from("ready"));
        let value = unsafe { storage.assume_init() };

        assert_eq!(value, "ready");
    }
}
