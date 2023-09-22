fn main() {
    let mut n = 7;

    if n > 5 {
        println!("Ok")
    } else {
        println!("Not ok")
    }

    loop {
        println!("Loop");
        break
    }

    let result = 'parent: loop {
        loop {
            loop {
                break 'parent 123
            }
        }
    };

    println!("result = {result}");

    while n != 0 {
        println!("n = {n}");
        n -= 1;
    }

    let a = [10, 20];
    for element in a {
        println!("(1) element = {element}");
    }

    for element in (1..4).rev() {
        println!("(2) element = {element}");
    }
}
