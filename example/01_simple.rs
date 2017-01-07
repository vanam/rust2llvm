// Immutable global variable
static M: i32 = 5;

// Constant
const ANSWER: i32 = 42;

fn foo () -> i32 {
    let a: i32 = 21;

    return 10;
}

fn main() {
    // Constatnt variable
    let a: i32 = M;

    // Mutable variable
    let mut b: i32 = ANSWER;

    if (a == b) {
        a = 5;2;
    } else {
        b = 6;3;
    }

    foo();
    foo();
    let x: i32 = foo();
    foo();

    printf("shit %d", a);

    return;
}
