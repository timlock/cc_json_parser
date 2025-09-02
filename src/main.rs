use cc_json_parser::parser::parse;
use std::error::Error;
use std::fs;

fn main() -> Result<(), Box<dyn Error>> {
    run_tests("tests_john/step1/")?;

    run_tests("tests_john/step2/")?;

    run_tests("tests_john/step3/")?;

    run_tests("tests_john/step4/")?;
    run_tests("test/");

    Ok(())
}

fn run_tests(folder: &str) -> Result<(), Box<dyn Error>> {
    let dirs = fs::read_dir(folder)
        .unwrap()
        .map(|entry| entry.unwrap().file_name());

    for path in dirs {
        let fullpath = folder.to_owned() + path.to_str().unwrap();
        if !fullpath.ends_with("pass1.json") {
            continue;
        };
        let json = fs::read_to_string(&fullpath).unwrap();
        println!("{fullpath}");
        let json_value = parse(&json).map_err(|err| err.to_string())?;
        println!("Value: {json_value:?}");
    }
    Ok(())
}
