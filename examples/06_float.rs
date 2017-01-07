
const PI: f32 = 3.141592654;

fn get_circumference(r: f32) -> f32 {
    return PI * 2.0 * r;
}

fn get_area(r: f32) -> f32 {
    return PI * r * r;
}

fn main() {
    let r: f32 = 5.0;
    let c: f32 = get_circumference(r);

    printf("Circumference (Perimeter) = %.2f\n", c);                                       // 31.42
    printf("Area of circle = %.2f\n", get_area(r));                          // get_area(5) = 78.54
    printf("Length of arc (central angle = 60 degrees) = %.2lf\n", 60.0 / 360.0 * c);       // 5.24
}
