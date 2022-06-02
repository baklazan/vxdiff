mod algorithm;

fn main() {
    let old = vec!["hello", "world", "!"];
    let new = vec!["hell", "world", "!"];
    let result = algorithm::diff(&old[..], &new[..]);
    println!("{:?}", result);

    println!("{:?}", algorithm::experiment(&old[..], &new[..]));
}
