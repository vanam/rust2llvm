// Immutable global variable
static M: i32 = 5;

// Constant
const ANSWER: i32 = 42;

fn foo (){
    let a: i32 = 21;
}

fn main() {
    // Constatnt variable
    let a: i32 = M;

    // Mutable variable
    let mut b: i32 = ANSWER;
}
