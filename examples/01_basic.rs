
// Global variable
static START: i32 = 10;

// Constant
const DECREMENT: i32 = 1;

fn main() {
    let mut c: i32 = START;
    let d: i32 = 2; // variables are mutable, mut keyword is optional

    loop {
        printf("%d\n", c);

        c = c - DECREMENT;

        if c == 0 {
            return;
        }
    }
    // unreachable
}
