use super::*;

#[test]
fn test_check_length_surpasses() -> fmt::Result {
    assert!(!check_length_surpasses(format_args!("a short static str"), 20)?);
    assert!(check_length_surpasses(format_args!("a short static str"), 10)?);

    let str = fmt::from_fn(|f| f.write_str("NOT a static str!"));
    let bomb = fmt::from_fn(|f| panic!());
    // make sure `bomb` is not displayed
    assert!(check_length_surpasses(format_args!("{str}{bomb}"), 10)?);

    Ok(())
}
