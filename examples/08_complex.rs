
static RANDOM_SEED: i32 = 3;                              // random seed = 3, other values possible

const N: i32 = 44448853;                                   // we assume that n is not a large prime

fn abs_val(a: i32) -> i32 {
    //if as a expression and expression as a implicit return statement
    if a > 0 {
         a;
    } else {
        -a;
    }
}

fn mulmod(a: i32, b: i32, c: i32) -> i32 {            // returns (a * b) % c, and minimize overflow
    let mut tmp_b: i32 = b;
    let mut x: i32 = 0;
    let mut y: i32 = a % c;

    while tmp_b > 0 {
        if tmp_b % 2 == 1 {
            x = (x + y) % c;
        }
        y = (y * 2) % c;
        tmp_b = tmp_b / 2;
    }

    return x % c;
}

fn gcd(a: i32, b: i32) -> i32 {
    //support for direct recursion
    return if b == 0 { a; } else { gcd(b, a % b); };
}

fn pollard_rho(n: i32) -> i32 {
    let mut i: i32 = 0;
    let mut k: i32 = 2;

    let mut x:i32 = RANDOM_SEED;
    let mut y:i32 = RANDOM_SEED;

    let mut d: i32 = -1;

    loop {
        i = i + 1;
        x = (mulmod(x, x, n) + n - 1) % n;                                   // generating function

        d = gcd(abs_val(y - x), n);                                              // the key insight

        // if (d != 1 && d != n) {
        if (d != 1) & (d != n) {
            return d;
        }
         // found one non-trivial factor
        if i == k {
            y = x;
            k = k * 2;
        }
    }
    // unreachable
    return -1; //set mandatory return
}

fn main() {
    let mut ans: i32 = pollard_rho(N);                      // break n into two non trivial factors
    if ans > N / ans { ans = N / ans; }                              // make ans the smaller factor

    printf("%d %d\n", ans, N / ans);                                        // should be: 6661 6673
}
