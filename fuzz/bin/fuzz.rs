use honggfuzz::fuzz;
use omnomnomicon::tutorial::parse_command;

fn main() {
    loop {
        fuzz!(|data: &str| {
            parse_command(data);
        });
    }
}
