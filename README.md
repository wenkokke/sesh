# Rusty Variation

[![Build Status](https://travis-ci.org/wenkokke/rusty-variation.svg?branch=master)](https://travis-ci.org/wenkokke/rusty-variation)

A library for deadlock-free session-typed communication in Rust.

```rust
#[macro_use]
extern crate rusty_variation;

use std::boxed::Box;
use std::error::Error;
use rusty_variation::*;

type CalcSrv = Recv<CalcOp, End>;
enum CalcOp {
    Done(End),
    Neg(Recv<i32, Send<i32, CalcSrv>>),
    Add(Recv<i32, Recv<i32, Send<i32, CalcSrv>>>)}

fn calc_server(s: CalcSrv) -> Result<(), Box<Error>> {
    offer!(s, {
        CalcOp::Done(End) => {
            Ok(())
        },
        CalcOp::Neg(s) => {
            let (x, s) = recv(s)?;
            let s = send(-x, s)?;
            calc_server(s)
        },
        CalcOp::Add(s) => {
            let (x, s) = recv(s)?;
            let (y, s) = recv(s)?;
            let s = send(x.wrapping_add(y), s)?;
            calc_server(s)
        },
    })
}

fn main() {
    assert!(|| -> Result<(), Box<Error>> {

        // Test the negation function.
        {
            let s: <CalcSrv as Session>::Dual = fork!(calc_server);
            let s = select!(CalcOp::Neg, s)?;
            let s = send(6, s)?;
            let (y, s) = recv(s)?;
            let End = select!(CalcOp::Done, s)?;
            assert_eq!(-6, y);
        }

        // Test the addition function.
        {
            let s: <CalcSrv as Session>::Dual = fork!(calc_server);
            let s = select!(CalcOp::Add, s)?;
            let s = send(4, s)?;
            let s = send(5, s)?;
            let (z, s) = recv(s)?;
            let End = select!(CalcOp::Done, s)?;
            assert_eq!(9, z);
        }

        Ok(())

    }().is_ok());
}
```
