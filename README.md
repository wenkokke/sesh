# Rusty Variation

[![Build Status](https://travis-ci.org/wenkokke/rusty-variation.svg?branch=master)](https://travis-ci.org/wenkokke/rusty-variation)

A library for deadlock-free session-typed communication in Rust.

```rust
extern crate rand;
extern crate rusty_variation;

use rand::{Rng, thread_rng};
use rusty_variation::*;
use std::boxed::Box;
use std::error::Error;

type NegSrv = Recv<i64, Send<i64, End>>;
type AddSrv = Recv<i64, Recv<i64, Send<i64, End>>>;
enum CalcOp { Neg(NegSrv), Add(AddSrv) }

type CalcSrv = Recv<CalcOp, End>;
type CalcCli = <CalcSrv as Session>::Dual;

fn server(s: CalcSrv) -> Result<(), Box<dyn Error>> {
    offer!(s, {
        CalcOp::Neg(s) => {
            let (x, s) = recv(s)?;
            let s = send(-x, s);
            close(s)?;
            Ok(())
        },
        CalcOp::Add(s) => {
            let (x, s) = recv(s)?;
            let (y, s) = recv(s)?;
            let s = send(x.wrapping_add(y), s);
            close(s)?;
            Ok(())
        },
    })
}

#[test]
fn server_works() {
    assert!(|| -> Result<(), Box<dyn Error>> {

        // Pick some random numbers.
        let mut rng = thread_rng();

        // Test the negation function.
        {
            let s: CalcCli = fork(server);
            let x: i64 = rng.gen();
            let s = choose!(CalcOp::Neg, s);
            let s = send(x, s);
            let (y, s) = recv(s)?;
            close(s)?;
            assert_eq!(-x, y);
        }

        // Test the addition function.
        {
            let s: CalcCli = fork(server);
            let x: i64 = rng.gen();
            let y: i64 = rng.gen();
            let s = choose!(CalcOp::Add, s);
            let s = send(x, s);
            let s = send(y, s);
            let (z, s) = recv(s)?;
            close(s)?;
            assert_eq!(x.wrapping_add(y), z);
        }

        Ok(())

    }().is_ok());
}
```
