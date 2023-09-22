fn main() {
    println!("Hi");
    bye();
    returns_nothing();
}

fn bye() -> u32 {
    println!("Bye");
    123
}

fn returns_nothing() -> () {
    1;
    2;
    3;
}
