use cc_json_parser::parse;
use std::fs;

fn main() {
    run_tests("tests_john/step1/");

    run_tests("tests_john/step2/");

    run_tests("tests_john/step3/");

    run_tests("tests_john/step4/");
    run_tests("test/");
}

fn run_tests(folder: &str) {
    fs::read_dir(folder)
        .unwrap()
        .map(|entry| entry.unwrap().file_name())
        .for_each(|path| {
            let fullpath = folder.to_owned() + path.to_str().unwrap();
            if !fullpath.ends_with(".json"){
                return;
            };
            let json = fs::read_to_string(&fullpath).unwrap();
            println!("{fullpath}");
            match parse(&json){
                Ok(json_value) => println!("Value: {json_value:?}"),
                Err(err) => println!("Error: {err}"),
            }
        });
}
