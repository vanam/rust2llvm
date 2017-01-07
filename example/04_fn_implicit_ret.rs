
fn f1 () -> i32 {
    let a: i32 = 21; //ok from parsers POV, this is statement and we dont check statements
}

fn f2 () -> i32 {
    if true { //ok from parsers POV, this is statement and we dont check statements
        2.4;
    }
}

fn f3 () -> i32 {

    while true {  //ok from parsers POV, this is statement and we dont check statements
        -4.2;
    }
}

/*
fn f4 () -> i32 {
    return 1.2; //illegal, types does not match
}
*/

fn f5 () -> i32 {
    1; //ok, implicit return
}


fn f6 () -> f32 {
    if true { //this if is expression
        return 1.0; //ok
    } else {
        1.2; //ok
    }
}



/*
fn f7 () -> i32 {
    if true { //this if is expression
        return 1.0; //fail, types does not match
    } else {
        1.2; //fail, types does not match
    }
}
*/

/*
fn f8 () -> f32 {
    if true { //this if is expression
        return 1.0; //ok
    } else {
        //missing expression
    }
}
*/

fn main() {


}
