
// global variable
static START: i32 = 10;

// constant
const DECREMENT: i32 = 1;

fn main() {
    // variables are mutable, mut keyword is optional
    let mut c: i32 = START;
    let d: i32 = 2;                // unused variable

    loop {
        printf("%d\n", c);

        c = c - DECREMENT;

        if c == 0 {
            return;
        }
    }
    // unreachable
}
