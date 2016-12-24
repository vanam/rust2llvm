fn f1() {
    return;
}

fn f2(a: i32) {
    return;
}

fn f3(a: f32) {
    return;
}

fn f4(a: i32) -> i32 {
    return a;
}

fn f5(a: f32) -> f32 {
    return a;
}

fn f6(a: i32, b: f32) {
    return;
}


fn main() {
    let a:i32 = 1;
    let b = true;
    let f:f32 = 2;

    //legal
    f1();
    f2(a);
    f3(f);
    let c:i32 = f4(a);
    let d:f32 = f5(f);
    f6(a, f);

    //ilegal
    //f6(a, a);
    //f1(a);
    //f2(f);
    //f4();
}
