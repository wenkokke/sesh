# Rusty Variation

[![Build Status](https://travis-ci.org/wenkokke/rusty-variation.svg?branch=master)](https://travis-ci.org/wenkokke/rusty-variation)

A library for deadlock-free session-typed communication in Rust.

```rust
    type CalcSrv = Recv<CalcOp, End>;
    enum CalcOp {
        Done(End),
        Neg(Recv<i32, Send<i32, CalcSrv>>),
        Add(Recv<i32, Send<i32, CalcSrv>>)}

    fn calc_server(s: CalcSrv) -> Result<(), Box<Error>> {
        offer!(s, {
            CalcOp::Done(End) => {
            },
            CalcOp::Neg(s) => {
                let (x, s) = recv(s)?;
                let s = send(-x, s)?;
                close(s)
            },
            CalcOp::Add(s) => {
                let (x, s) = recv(s)?;
                let (y, s) = recv(s)?;
                let s = send(x.wrapping_add(y), s)?;
                close(s)
            },
        })
    }

    #[test]
    fn calc_works() {
        assert!(|| -> Result<(), Box<Error>> {

            // Pick some random numbers.
            let mut rng = thread_rng();

            // Test the negation function.
            {
                let s: <CalcSrv as Session>::Dual = fork!(calc_server);
                let x: i32 = rng.gen();
                let s = select!(CalcOp::Neg, s)?;
                let s = send(x, s)?;
                let (y, s) = recv(s)?;
                let End = select!(CalcOp::Done, s)?;
                assert_eq!(-x, y);
            }

            // Test the addition function.
            {
                let s: <CalcSrv as Session>::Dual = fork!(calc_server);
                let x: i32 = rng.gen();
                let y: i32 = rng.gen();
                let s = select!(CalcOp::Add, s)?;
                let s = send(x, s)?;
                let s = send(y, s)?;
                let (z, s) = recv(s)?;
                close(s)?;
                assert_eq!(x.wrapping_add(y), z);
            }

            Ok(())

        }().is_ok());
    }
```
