fn main() {
    'a: loop {
        println!("a");
    }

    'b: loop {
        println!("b");
        break 'b;
        println!("b");
    }


    'a: while true {
        println!("a");
    }

    'b: while true {
        println!("b");
        break 'b;
        println!("b");
    }


    'a: while let Some(_) = Some(()) {
        println!("a");
    }

    'b: while let Some(_) = Some(()) {
        println!("b");
        break 'b;
        println!("b");
    }


    'a: for i in 0 .. 10 {
        println!("a");
    }

    'b: for i in 0 .. 10 {
        println!("b");
        break 'b;
        println!("b");
    }
}
