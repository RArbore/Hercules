extern crate clap;

use std::path::Path;

fn main() {
    let module = hercules_rt::load_binary(Path::new("test.o"));

    let matmul = hercules_rt::lookup_function!(
        module,
        "matmul",
        *const f32,
        *const f32,
        u64,
        u64,
        u64,
        *mut f32,
        => *const f32
    );

    let a = [[1.0f32, 2.0f32], [3.0f32, 4.0f32]];
    let b = [[5.0f32, 6.0f32], [7.0f32, 8.0f32]];
    let mut c = [[0.0f32, 0.0f32], [0.0f32, 0.0f32]];
    unsafe {
        matmul(
            std::mem::transmute(a.as_ptr()),
            std::mem::transmute(b.as_ptr()),
            2,
            2,
            2,
            std::mem::transmute(c.as_mut_ptr()),
        )
    };
    println!("{} {}\n{} {}", c[0][0], c[0][1], c[1][0], c[1][1]);
}
