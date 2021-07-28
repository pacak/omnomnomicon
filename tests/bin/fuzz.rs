use honggfuzz::fuzz;
use omnomnomicon_tests::parse_ext;

fn main() {
    loop {
        fuzz!(|data: &str| {
            parse_ext(data);
        });
    }
}
