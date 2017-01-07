
fn print_bool(a: bool) {
    if a {
        printf("true\n");
        // println!("true");
    } else {
        printf("false\n");
        // println!("false");
    }
}

fn main() {
    let a: bool = false;
    let b: bool = true;

    print_bool(a & b);
    print_bool(a | b);
    print_bool(b ^ true);
    print_bool(!a);
}
