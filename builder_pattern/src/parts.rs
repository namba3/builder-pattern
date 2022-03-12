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
/// so you MUST use core::mem::transmute or something to safely drop the inner data
///
/// # Exmple
/// ```
/// use crate::Uninit;
///
/// fn main() {
///     let data = unsafe { Uninit::<String>::uninit() };
///     drop(data); // ok
///
///     let data = Uninit::new(String::from("test"));
///     // drop(data); // leaks inner string!!!
///     let data: String = unsafe{ core::memm::transmute(data) }; // transmute
///     drop(data); // ok, not leaks the string
/// }
/// ```
#[repr(transparent)]
pub struct Uninit<T>(core::mem::ManuallyDrop<T>);
impl<T> Uninit<T> {
    #[inline]
    pub unsafe fn uninit() -> Self {
        core::mem::MaybeUninit::uninit().assume_init()
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
