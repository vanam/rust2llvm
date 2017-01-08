
fn func() -> i32 {
    // if as a expression and as implicit return statement
    if true {
        if true {
            1;
        } else {
            2;
        }
    } else {
        3;
    }
}

fn main() {
    // let a: i32 = 0;
    let a: i32 = 1;
    // let a: i32 = 3;
    // let a: i32 = 4;

    if a == 0 {
        printf("Is zero\n");
    } else if a % 2 == 0 {
        printf("Is even\n");
    } else {
        if a == 1 {
            printf("Is one\n");
        }
        printf("Is odd\n");
    }

    // if as a expression in assignment
    a = if true {
        1;
    } else {
        2;
    };

    func();
}
