use std::fs;

fn main() {
    run_tests("tests_john/step1/");

    run_tests("tests_john/step2/");

    run_tests("tests_john/step3/");

    run_tests("tests_john/step4/");
    run_tests("test/");
}

fn run_tests(folder: &str) {
    // fs::read_dir(folder)
    //     .unwrap()
    //     .map(|entry| entry.unwrap().file_name())
    //     .for_each(|path| {
    //         let fullpath = folder.to_owned() + path.to_str().unwrap();
    //         let json = fs::read_to_string(&fullpath).unwrap();
    //         println!("{fullpath}");
    //         let is_valid = validate(&json);
    //         println!("  is {is_valid}");
    //     });
}
