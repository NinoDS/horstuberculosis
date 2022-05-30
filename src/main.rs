use std::env;

fn main() {
    // CLI
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }

    // If file has no extension, add .horst
    let mut file_name = args[1].clone();
    if !file_name.contains(".") {
        file_name.push_str(".horst");
    }

    // Read file
    let mut file = match std::fs::read_to_string(file_name) {
        Ok(file) => file,
        Err(e) => {
            println!("Error: {}", e);
            return;
        }
    };

}
