use drv_example::{Hello, Example, Example2};


#[derive(Example)]
struct Hoi; //(i32, String);

/// Hey
/// Ho
#[derive(Example, Hello)]
#[hello]
struct Hey {
    /// This is a test
    #[hello = 2]
    #[hello(1)]
    a: i32,
    /// doc comment 1
    ///
    /// doc comment 3
    #[hello]
    b: String,
}

#[derive(Example2)]
enum Asdf {
    Hello,
    Pizza
}

fn main() {}
