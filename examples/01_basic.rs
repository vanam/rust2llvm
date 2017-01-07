
// Global variable
static START: i32 = 10;

// Constant
const DECREMENT: i32 = 1;

fn main() {
    let mut c: i32 = START;

    loop {
        // printf("%d\n", c);
        println!("{}", c);
        
        c = c - DECREMENT;

        if c == 0 {
            return;
        }
    }
    // unreachable
}
