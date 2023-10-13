#[allow(unused_mut)]
fn main() {
    let mut s = String::from("hi");
    let l = change_then_calculate_len(&mut s);
    println!("{} - {}", s, l);

    let s1 = &mut s;
    println!("{}", s1);
    let s2 = &mut s;
    println!("{}", s2);
    // error[E0499]: cannot borrow `s` as mutable more than once at a time
    //  --> references.rs:7:14
    //   |
    // 6 |     let s1 = &mut s;
    //   |              ------ first mutable borrow occurs here
    // 7 |     let s2 = &mut s;
    //   |              ^^^^^^ second mutable borrow occurs here
    // 8 |     println!("{}, {}", s1, s2);
    //   |                        -- first borrow later used here
    //
    //
    // println!("{} / {}", s1, s2);
    //
    //
    // The restriction preventing multiple mutable references to the same data at the same time
    // allows for mutation but in a very controlled fashion.

    let mut ss = String::from("a");

    let r1 = &ss;
    let r2 = &ss;

    // error[E0502]: cannot borrow `ss` as mutable because it is also borrowed as immutable
    //   --> references.rs:33:18
    //    |
    // 30 |     let r1 = &ss;
    //    |              --- immutable borrow occurs here
    // ...
    // 33 |     let mut r3 = &mut ss;
    //    |                  ^^^^^^^ mutable borrow occurs here
    // 34 |
    // 35 |     println!("{}", r1);
    //    |                    -- immutable borrow later used here
    //
    //
    // let mut r3 = &mut ss;

    println!("{}", r1);
    println!("{}", r2);

    let mut r3 = &mut ss;
    println!("{}", r3);
}

// The scope in which `s` is valid is the same as any other function parameter. The value pointed
// to by the reference is not dropped when `s` stops being used since `s` doesn't have ownership.
//
// We call the action of creating a reference _borrowing_.
fn change_then_calculate_len(s: &mut String) -> usize {
    s.push_str(" there");
    s.len()
}
