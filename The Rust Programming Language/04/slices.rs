#[allow(unused_mut)]
fn main() {
    println!("{}", first_word(&String::from("hi there")));
    println!("{}", first_word(&String::from("hello world")));

    let mut s1 = String::from("hello, world");
    let hello = &s1[..5]; /* immutable borrow -- also same as [0..5] */
    let world = &s1[7..]; /* immutable borrow -- also same as [7..12] */

    // error[E0502]: cannot borrow `s1` as mutable because it is also borrowed as immutable
    //  --> slices.rs:8:5
    //   |
    // 6 |     let hello = &s1[0..5];
    //   |                  - immutable borrow occurs here
    // 7 |     let world = &s1[7..12];
    // 8 |     s1.clear();
    //   |     ^^^^^^^^^ mutable borrow occurs here
    // 9 |     println!("{}, {}", hello, world);
    //   |                        ----- immutable borrow later used here
    //
    //
    // s1.clear(); // mutable borrow after an immutable borrow
    println!("{}, {}", hello, world);

    let mut s2 = String::from("first hi there");
    let s3 = first_word(&s2);
    println!("{}", s3);
    println!("{}", first_word(&s2[..]));
    println!("{}", first_word(&String::from("first!")));

    // error[E0308]: mismatched types
    //   --> slices.rs:30:31
    //    |
    // 30 |     println!("{}", first_word(String::from("first!")));
    //    |                    ---------- ^^^^^^^^^^^^^^^^^^^^^^ expected `&str`, found `String`
    //    |                    |
    //    |                    arguments to this function are incorrect
    //    |
    // note: function defined here
    //   --> slices.rs:33:4
    //    |
    // 33 | fn first_word(s: &str) -> &str {
    //    |    ^^^^^^^^^^ -------
    // help: consider borrowing here
    //    |
    // 30 |     println!("{}", first_word(&String::from("first!")));
    //    |                               +
    //
    //
    // println!("{}", first_word(String::from("first!")));
}

fn first_word(s: &str) -> &str {
    let bytes = s.as_bytes();

    // iter is a method that returns each element in a collection and enumerate wraps the result of
    // iter and returns each element as part of a tuple.
    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return &s[..i];
        }
    }

    &s /* &s[..] */
}
