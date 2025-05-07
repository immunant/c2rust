fn main() {
    {
        let x;
        x = 5;
    }

    {
        let x;
        let y;
        x = 5;
        y = 5;
    }

    {
        let x;
        let y;
        y = 5;
        x = 5;
    }

    {
        let x;
        let mut y = 17;
        y = 17;
        x = 5;
    }

    {
        let mut x;
        {
            x = 5;
        }
        x = 10;
    }

    {
        let x;
        x = 5;
        let y;
        y = 5;
        let mut x = ::std::mem::uninitialized();
        x = 10;
    }

    {
        let mut x = ::std::mem::uninitialized();
        ::std::mem::drop(&mut x);
        x = 10;
    }
}
