
fn print_bool(a: bool) {
    if a {
        printf("true\n");
    } else {
        printf("false\n");
    }
}

fn main() {
    let a: bool = false;
    let b = true; //type inferencing

    print_bool(a & b);
    print_bool(a | b);
    print_bool(b ^ true);
    print_bool(!a);
}
