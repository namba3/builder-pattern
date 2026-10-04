/// marker trait
pub trait Ready {}

#[repr(transparent)]
pub struct Certain<T>(T);
impl<T> Certain<T> {
    #[inline]
    pub const fn new(t: T) -> Self {
        Self(t)
    }
}
impl<T> Ready for Certain<T> {}

/// behaves like core::mem::MaybeUninit.
/// The inner data will be ignored with code::mem::forget when drops,
/// so you MUST use core::mem::transmute or something to safely drop the inner data.
///
/// # Exmple
/// ```
/// use builder_pattern::parts::Uninit;
///
/// fn main() {
///     let data = unsafe { Uninit::<String>::uninit() };
///     drop(data); // ok
///
///     let data = unsafe { Uninit::new(String::from("test")) };
///     // drop(data); // leaks inner string!!!
///     let data: String = unsafe{ core::mem::transmute(data) }; // transmute
///     drop(data); // ok, not leaks the string
/// }
/// ```
#[repr(transparent)]
pub struct Uninit<T>(core::mem::ManuallyDrop<T>);
impl<T> Uninit<T> {
    #[inline]
    pub unsafe fn uninit() -> Self {
        unsafe { core::mem::MaybeUninit::uninit().assume_init() }
    }
    #[inline]
    pub const unsafe fn new(t: T) -> Self {
        Self(core::mem::ManuallyDrop::new(t))
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
impl Ready for False {}

#[repr(transparent)]
pub struct True(bool);
impl True {
    #[inline]
    pub const fn new() -> True {
        Self(true)
    }
}
impl Ready for True {}

#[repr(transparent)]
pub struct None<T>(Option<T>);
impl<T> None<T> {
    #[inline]
    pub const fn new() -> None<T> {
        Self(Option::None)
    }
}
impl<T> Ready for None<T> {}

#[repr(transparent)]
pub struct Some<T>(Option<T>);
impl<T> Some<T> {
    #[inline]
    pub const fn new(t: T) -> Self {
        Self(Option::Some(t))
    }
}
impl<T> Ready for Some<T> {}

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
impl<T> Ready for Vec<T> {}

#[repr(transparent)]
pub struct Default<T>(T);
impl<T> Default<T> {
    #[inline]
    pub const fn new(t: T) -> Self {
        Self(t)
    }
}
impl<T> Ready for Default<T> {}

#[repr(transparent)]
pub struct Fixed<T>(T);
impl<T> Fixed<T> {
    #[inline]
    pub const fn new(t: T) -> Self {
        Self(t)
    }
}
impl<T> Ready for Fixed<T> {}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_ready<T: Ready>() {}

    #[test]
    fn value_markers_preserve_their_values() {
        assert_eq!(Certain::new(1).0, 1);
        assert_eq!(Default::new(2).0, 2);
        assert_eq!(Fixed::new(3).0, 3);
    }

    #[test]
    fn boolean_markers_represent_false_and_true() {
        assert!(!False::new().0);
        assert!(True::new().0);
    }

    #[test]
    fn option_markers_represent_none_and_some() {
        assert_eq!(None::<u8>::new().0, Option::None);
        assert_eq!(Some::new(7).0, Option::Some(7));
    }

    #[test]
    fn vector_marker_pushes_and_extends_values() {
        let mut values = Vec::new();
        values.push(1);
        values.extend([2, 3]);

        assert_eq!(values.0, std::vec![1, 2, 3]);
    }

    #[test]
    fn all_fully_initialized_markers_implement_ready() {
        assert_ready::<Certain<u8>>();
        assert_ready::<Default<u8>>();
        assert_ready::<Fixed<u8>>();
        assert_ready::<False>();
        assert_ready::<True>();
        assert_ready::<None<u8>>();
        assert_ready::<Some<u8>>();
        assert_ready::<Vec<u8>>();
    }
}
