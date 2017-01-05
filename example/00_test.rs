static M: i32 = 5;

const ANSWER: i32 = 42;

fn foo (){
    let a: i32 = 21;
    return;
}

fn bar (b:i32) -> i32 {
    let a: i32 = 21;
    return (a+a);
    return 1 * 1;
    return b;
}


fn main() {
    1 + 2 * 4;
    let a: i32 = M;
    let b: i32 = 1 + 2 * 3;
    let mut b: i32 = ANSWER;
    foo();
    let c: i32 = bar(a);
    let d1: i32 = bar(a) * 2;
    let d2: i32 = 2 + bar(a);
    let f1 = true == true;
    let f2 = true != false;
    //c = bar(a) * bar(a);

    if a == b || a == c && c != b && 1 < 2 || 1 + 2 <= 3{
        let d = true;
    }

    if (1 + 2 <= 3) {
        let d = true || false;
    } else {
        let d = false;
    }

    //let bAssign = if 1 < 2 {true;} else {false;}
    let iAssign:i32 = if 1 < 2 {1;} else {2;}

    a = 30;
    a = b = c;

    f1 = false;
    let fl1: f32 = 2.3;
    fl1 = 4.3;
}
