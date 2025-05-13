#[macro_export]
macro_rules! timed {
    ($expre:expr) => {
        {
            use std::time::SystemTime;
            let start = SystemTime::now();
            let result = $expre;
            let end = SystemTime::now();
            let duration = end.duration_since(start).unwrap();
            let mut msg = stringify!($expre).to_owned();
            msg.truncate(20);
            println!("[{}:{}] {}: {} ms", file!(), line!(), msg, duration.as_millis());
            result
        }
    };
}

