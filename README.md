# Rusty Variation

A library for deadlock-free session-typed channels in Rust.

```rust
use rv::*;

type NegServer<N> = Receive<N, Send<N, End>>;
type AddServer<N> = Receive<N, Receive<N, Send<N, End>>>;
enum Op<N: marker::Send> {
    Neg(NegServer<N>),
    Add(AddServer<N>),
}
type NiceCalcServer<N> = Receive<Op<N>, End>;
type NiceCalcClient<N> = <NiceCalcServer<N> as Session>::Dual;
  
#[test]
fn nice_calc_works() {  
    assert!(|| -> Result<i32, Box<Error>> {
  
        // Pick some random numbers.
        let mut rng = thread_rng();
        let x: i32 = rng.gen();
        let y: i32 = rng.gen();
      
        // Fork a calculator server.
        let s = fork!(move |s: NiceCalcServer<i32>| {
            offer!(s, {
                Op::Neg(s) => {
                    let (x, s) = receive(s)?;
                    let s = send(-x, s)?;
                    close(s)
                },
                Op::Add(s) => {
                    let (x, s) = receive(s)?;
                    let (y, s) = receive(s)?;
                    let s = send(x.wrapping_add(y), s)?;
                    close(s)
                },
            })
        });
         
        // Send it the numbers. 
        let s = select!(Op::Add, s)?;
        let s = send(x, s)?;
        let s = send(y, s)?;
        
        // Receive the answer and close the channel.
        let (z, s) = receive(s)?;
        close(s)?;
        
        // Check the answer.
        assert_eq!(x.wrapping_add(y), z);
        
        Ok(())

    }().is_ok());
}
```
