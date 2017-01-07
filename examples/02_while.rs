
fn main() {
    let mut number: i32 = 5;// 5! = 120
    let mut factorial: i32 = 1;

    // loop terminates when number is less than or equal to 0
    while number > 0 {
        factorial = factorial * number;
        number = number - 1;
    }

    printf("Factorial = %d\n", factorial);
}
