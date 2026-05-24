#[macro_export]
macro_rules! vec_in {
    (in: $alloc:expr $(,)?) => (
        ::std::vec::Vec::new_in($alloc)
    );
    (in: $alloc:expr, $elem:expr; $n:expr) => (
        ::std::vec::from_elem_in($elem, $n, $alloc)
    );
    (in: $alloc:expr, $($x:expr),+ $(,)?) => ({
        let alloc = $alloc;
        let mut v = ::std::vec::Vec::with_capacity_in(
            const { [$($crate::vec_in!(@count $x)),+].len() },
            alloc,
        );
        $(v.push($x);)+
        v
    });
    (@count $_x:expr) => { () };
}
