fn main() {
    {
        let x = (5);
    }

    {
        let x = (5);
        let y = (5);
    }

    {
        let y = (5);
        let x = (5);
    }

    {
        let mut y = 17;
        y = 17;
        let x = (5);
    }

    {
        let mut x;
        {
            x = 5;
        }
        x = 10;
    }

    {
        let x = (5);
        let y = (5);
        let mut x = (10);
    }

    {
        let mut x = ::std::mem::uninitialized();
        ::std::mem::drop(&mut x);
        x = 10;
    }
}
