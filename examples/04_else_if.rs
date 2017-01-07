
fn main() {
    // let a: i32 = 0;
    let a: i32 = 1;
    // let a: i32 = 3;
    // let a: i32 = 4;

    if a == 0 {
        printf("Is zero\n");
        // println!("Is zero");
    } else if a % 2 == 0 {
        printf("Is even\n");
        // println!("Is even");
    } else {
        if a == 1 {
            printf("Is one\n");
            // println!("Is one");
        }
        printf("Is odd\n");
        // println!("Is odd");
    }
}
