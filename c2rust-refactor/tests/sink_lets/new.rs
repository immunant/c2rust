fn f() {
    {
        {
            let mut x;
            x = 5;
        }
    }

    {
        let mut x;
        {
            x = 1;
        }
        {
            x = 2;
        }
    }

    {
        let mut x;
        if true {
            x = 1;
        } else {
            x = 2;
        }
    }

    {
        if true {
            let mut x;
            x = 1;
        }
    }

    {

        {
            let mut x;
            {
                x = 1;
            }
            {
                x = 2;
            }
        }
    }

    {
        {
            let mut x;
            x = 1;
            x = 2;
        }
    }

    {
        let y = {
            let mut x;
            x = 1;
            2
        };
    }


    {
        let mut x;
        {
            {
                x = 1;
            }
            {
                x = 2;
            }
        }
        {
            {
                x = 1;
            }
            {
                x = 2;
            }
        }
    }

    {
        // Initialized `let`s can't sink, because the initializer might have side effects.
        let mut y = 0;
        {
            let mut x;
            {
                x = 1;
            }
            {
                x = 2;
            }
        }
        {
            {
                y = 1;
            }
            {
                y = 2;
            }
        }
    }
}

fn main() {}
