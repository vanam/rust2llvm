
fn gcd(a: i32, b: i32) -> i32 {
    if b == 0 {
        a;
    } else {
        gcd(b, a % b);
    }
}

fn main() {
    let a: i32 = 12;
    let b: i32 = 90;

    printf("GCD(%d, %d) = %d\n", a, b, gcd(a, b));                              //  GCD(12, 90) = 6
}
