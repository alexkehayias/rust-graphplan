#[macro_export]
/// Create a **HashSet** from a list of elements. Implementation
/// copied from the maplit library https://github.com/bluss/maplit
///
/// ## Example
///
/// ```
/// #[macro_use] extern crate graphplan;
/// # fn main() {
///
/// let set = hashset!{"a", "b"};
/// assert!(set.contains("a"));
/// assert!(set.contains("b"));
/// assert!(!set.contains("c"));
/// # }
/// ```
macro_rules! hashset {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(hashset!(@single $rest)),*]));

    ($($key:expr,)+) => { hashset!($($key),+) };
    ($($key:expr),*) => {
        {
            let _cap = hashset!(@count $($key),*);
            let mut _set = ::std::collections::HashSet::with_capacity(_cap);
            $(
                let _ = _set.insert($key);
            )*
                _set
        }
    };
}

#[macro_export]
/// Create a **BTreeSet** from a list of elements. Implementation
/// copied from the maplit library https://github.com/bluss/maplit
///
/// ## Example
///
/// ```
/// #[macro_use] extern crate graphplan;
/// # fn main() {
///
/// let set = btreeset!{"a", "b"};
/// assert!(set.contains("a"));
/// assert!(set.contains("b"));
/// assert!(!set.contains("c"));
/// # }
/// ```
macro_rules! btreeset {
    ($($key:expr,)+) => (btreeset!($($key),+));

    ( $($key:expr),* ) => {
        {
            let mut _set = ::std::collections::BTreeSet::new();
            $(
                _set.insert($key);
            )*
            _set
        }
    };
}

#[macro_export]
/// Create a **HashMap** from a list of key-value pairs. Implementation
/// copied from the maplit library https://github.com/bluss/maplit
///
/// ## Example
///
/// ```
/// #[macro_use] extern crate graphplan;
/// # fn main() {
///
/// let map = hashmap!{
///     "a" => 1,
///     "b" => 2,
/// };
/// assert_eq!(map["a"], 1);
/// assert_eq!(map["b"], 2);
/// assert_eq!(map.get("c"), None);
/// # }
/// ```
macro_rules! hashmap {
    (@single $($x:tt)*) => (());
    (@count $($rest:expr),*) => (<[()]>::len(&[$(hashmap!(@single $rest)),*]));

    ($($key:expr => $value:expr,)+) => { hashmap!($($key => $value),+) };
    ($($key:expr => $value:expr),*) => {
        {
            let _cap = hashmap!(@count $($key),*);
            let mut _map = ::std::collections::HashMap::with_capacity(_cap);
            $(
                let _ = _map.insert($key, $value);
            )*
            _map
        }
    };
}
