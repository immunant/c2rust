use std::fs::File;
use std::io::BufReader;
use c2rust_analysis_rt::events::Event;
use bincode;

pub fn read_event_log(path: String) -> Vec<Event> {
    let file = File::open(path).unwrap();
    let mut events = vec![];
    let mut reader = BufReader::new(file);
    loop {
        match bincode::deserialize_from(&mut reader) {
            Ok(e) => events.push(e),
            _ => break
        }
    }
    events
}