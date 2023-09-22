fn main() {
    let tup = (34, 45, 'a');
    let (x, y, z) = tup;
    println!("x = {x}");
    println!("y = {y}");
    println!("z = {z}");

    let x = tup.0;
    let y = tup.1;
    let z = tup.2;
    println!("x = {x}");
    println!("y = {y}");
    println!("z = {z}");
}
