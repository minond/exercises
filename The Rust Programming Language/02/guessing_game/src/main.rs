use std::io;
use std::cmp::Ordering;
use rand::Rng;

fn main() {
    let secret_number = rand::thread_rng().gen_range(1..=100);

    println!("Guess the number!");
    println!("The number is {secret_number}");

    loop {
        println!("Please input your guess.");

        let mut guess = String::new();

        // The & indicates that this argument is a reference, which gives you a way to let multiple
        // parts of your code access one piece of data without needing to copy that data into memory
        // multiple times. References are a complex feature, and one of Rust’s major advantages is how
        // safe and easy it is to use references. You don’t need to know a lot of those details to
        // finish this program. For now, all you need to know is that, like variables, references are
        // immutable by default. Hence, you need to write &mut guess rather than &guess to make it
        // mutable. (Chapter 4 will explain references more thoroughly.)
        //
        // - https://doc.rust-lang.org/book/ch02-00-guessing-game-tutorial.html
        io::stdin()
            .read_line(&mut guess)
            .expect("Failed to read input");

        // The type checker will infer the type of <parse>
        // (https://doc.rust-lang.org/std/primitive.str.html#method.parse) to be `parse<F>(&self) ->
        // Result<F, <F as FromStr>::Err>` where `F` is u32 because of the annotation. The type can
        // also be passed to the function explicitly:
        //
        // let guess = guess.trim().parse::<u32>()
        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => continue,
        };

        match guess.cmp(&secret_number) {
            Ordering::Less => println!("Too small!"),
            Ordering::Greater => println!("Too big!"),
            Ordering::Equal => {
                println!("You win!");
                break;
            }
        }
    }
}
