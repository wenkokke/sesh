extern crate either;

use std::boxed::Box;
use std::error::Error;
use std::marker;
use std::mem;
use std::sync::mpsc::{Sender, Receiver};
use std::sync::mpsc;
use std::thread::{JoinHandle,spawn};
use either::Either;

/// The session types supported.

#[derive(Debug)]
pub struct End;

#[must_use]
#[derive(Debug)]
pub struct Send<T, S: Session> {
    channel: Sender<(T, S::Dual)>,
}

#[must_use]
#[derive(Debug)]
pub struct Recv<T, S: Session> {
    channel: Receiver<(T, S)>,
}

pub trait Session: marker::Sized + marker::Send {
    type Dual: Session<Dual=Self>;

    #[doc(hidden)]
    fn new() -> (Self, Self::Dual);
}

impl Session for End {
    type Dual = End;

    fn new() -> (Self, Self::Dual) {
        return (End, End);
    }
}

impl<T: marker::Send, S: Session> Session for Send<T, S> {
    type Dual = Recv<T, S::Dual>;

    fn new() -> (Self, Self::Dual) {
        let (sender, receiver) = mpsc::channel::<(T, S::Dual)>();
        return (Send { channel: sender }, Recv { channel: receiver });
    }
}

impl<T: marker::Send, S: Session> Session for Recv<T, S> {
    type Dual = Send<T, S::Dual>;

    fn new() -> (Self, Self::Dual) {
        let (there, here) = Self::Dual::new();
        return (here, there);
    }
}


/// The communication primitives.

pub fn send<'a, T, S>(x: T, s: Send<T, S>) -> Result<S, Box<Error + 'a>>
where
    T: marker::Send + 'a,
    S: Session + 'a,
{
    let (here, there) = S::new();
    s.channel.send((x, there)).unwrap_or(());
    Ok(here)
}

pub fn recv<'a, T, S>(s: Recv<T, S>) -> Result<(T, S), Box<Error + 'a>>
where
    T: marker::Send,
    S: Session,
{
    let (v, s) = s.channel.recv()?;
    Ok((v, s))
}

pub fn close(s: End) -> Result<(), Box<Error>> {
    let End = s;
    Ok(())
}

pub fn cancel<T>(x: T) -> Result<(), Box<Error>> {
    mem::drop(x);
    Ok(())
}

pub(crate) fn fork_with_thread_id<S, P>(p: P) -> (JoinHandle<()>, S::Dual)
where
    S: Session + 'static,
    P: Fn(S) -> Result<(), Box<Error>> + marker::Send + 'static
{
    let (there, here) = Session::new();
    let other_thread = spawn(move || {
        match p(there) {
            Ok(()) => (),
            Err(e) => panic!("{}", e.description()),
        }
    });
    (other_thread, here)
}

pub fn fork<S, P>(p: P) -> S::Dual
where
    S: Session + 'static,
    P: Fn(S) -> Result<(), Box<Error>> + marker::Send + 'static
{
    fork_with_thread_id(p).1
}

pub type Offer<S1, S2> = Recv<Either<S1, S2>, End>;
pub type Choose<S1, S2> = Send<Either<<S1 as Session>::Dual, <S2 as Session>::Dual>, End>;

pub fn offer_either<'a, S1, S2, F, G, R>(s: Offer<S1, S2>, f: F, g: G) -> Result<R, Box<Error + 'a>>
where
    S1: Session,
    S2: Session,
    F: FnOnce(S1) -> Result<R, Box<Error + 'a>>,
    G: FnOnce(S2) -> Result<R, Box<Error + 'a>>,
{
    let (e, End) = recv(s)?;
    e.either(f, g)
}

pub fn choose_left<'a, S1, S2>(s: Choose<S1, S2>) -> Result<S1, Box<Error + 'a>>
where
    S1: Session + 'a,
    S2: Session + 'a,
{
    let (here, there) = S1::new();
    let End = send(Either::Left(there), s)?;
    Ok(here)
}

pub fn choose_right<'a, S1, S2>(s: Choose<S1, S2>) -> Result<S2, Box<Error + 'a>>
where
    S1: Session + 'a,
    S2: Session + 'a,
{
    let (here, there) = S2::new();
    let End = send(Either::Right(there), s)?;
    Ok(here)
}

#[macro_export]
macro_rules! offer {
    ($session:expr, { $($pat:pat => $result:expr,)* }) => {
        (move || -> Result<_, _> {
            let (l, End) = recv($session)?;
            match l {
                $(
                    $pat => $result,
                )*
            }
        })()
    };
}

#[macro_export]
macro_rules! choose {
    ($label:path, $session:expr) => {
        (move || -> Result<_, Box<Error>> {
            let (here, there) = <_ as Session>::new();
            let End = send($label(there), $session)?;
            Ok(here)
        })()
    };
}

#[cfg(test)]
mod tests {
    extern crate rand;

    use std::marker;
    use super::*;
    use self::rand::{Rng, thread_rng};

    // Test sending a ping across threads.

    #[test]
    fn ping_works() {
        assert!(|| -> Result<(), Box<Error>> {

            let s = fork(move |s: Send<(), End>| {
                let s = send((), s)?;
                close(s)
            });
            let ((), s) = recv(s)?;
            close(s)

        }().is_ok());
    }

    // Test a simple calculator server, implemented using binary choice.

    type NegServer<N> = Recv<N, Send<N, End>>;
    type NegClient<N> = <NegServer<N> as Session>::Dual;

    type AddServer<N> = Recv<N, Recv<N, Send<N, End>>>;
    type AddClient<N> = <AddServer<N> as Session>::Dual;

    type SimpleCalcServer<N> = Offer<NegServer<N>, AddServer<N>>;
    type SimpleCalcClient<N> = <SimpleCalcServer<N> as Session>::Dual;

    fn simple_calc_server(s: SimpleCalcServer<i32>) -> Result<(), Box<Error>> {
        offer_either(s,
                     |s: NegServer<i32>| {
                         let (x, s) = recv(s)?;
                         let s = send(-x, s)?;
                         close(s)
                     },
                     |s: AddServer<i32>| {
                         let (x, s) = recv(s)?;
                         let (y, s) = recv(s)?;
                         let s = send(x.wrapping_add(y), s)?;
                         close(s)
                     })
    }

    #[test]
    fn simple_calc_works() {
        assert!(|| -> Result<(), Box<Error>> {

            let mut rng = thread_rng();

            // Test the negation function.
            {
                let s: SimpleCalcClient<i32> = fork(simple_calc_server);
                let x: i32 = rng.gen();
                let s = choose_left::<_, AddClient<i32>>(s)?;
                let s = send(x, s)?;
                let (y, End) = recv(s)?;
                assert_eq!(-x, y);
            }

            // Test the addition function.
            {
                let s: SimpleCalcClient<i32> = fork(simple_calc_server);
                let x: i32 = rng.gen();
                let y: i32 = rng.gen();
                let s = choose_right::<NegClient<i32>, _>(s)?;
                let s = send(x, s)?;
                let s = send(y, s)?;
                let (z, End) = recv(s)?;
                assert_eq!(x.wrapping_add(y), z);
            }

            Ok(())

        }().is_ok());
    }

    // Test a nice calculator server, implemented using variant types.

    enum CalcOp<N: marker::Send> {
        Neg(NegServer<N>),
        Add(AddServer<N>),
    }
    type NiceCalcServer<N> = Recv<CalcOp<N>, End>;
    type NiceCalcClient<N> = <NiceCalcServer<N> as Session>::Dual;

    fn nice_calc_server(s: NiceCalcServer<i32>) -> Result<(), Box<Error>> {
        offer!(s, {
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
    fn nice_calc_works() {
        assert!(|| -> Result<(), Box<Error>> {

            // Pick some random numbers.
            let mut rng = thread_rng();

            // Test the negation function.
            {
                let s: NiceCalcClient<i32> = fork(nice_calc_server);
                let x: i32 = rng.gen();
                let s = choose!(CalcOp::Neg, s)?;
                let s = send(x, s)?;
                let (y, s) = recv(s)?;
                close(s)?;
                assert_eq!(-x, y);
            }

            // Test the addition function.
            {
                let s: NiceCalcClient<i32> = fork(nice_calc_server);
                let x: i32 = rng.gen();
                let y: i32 = rng.gen();
                let s = choose!(CalcOp::Add, s)?;
                let s = send(x, s)?;
                let s = send(y, s)?;
                let (z, s) = recv(s)?;
                close(s)?;
                assert_eq!(x.wrapping_add(y), z);
            }

            Ok(())

        }().is_ok());
    }

    #[test]
    fn cancel_recv_works() {

        let (other_thread, s) = fork_with_thread_id(nice_calc_server);

        assert!(|| -> Result<(), Box<Error>> {

            cancel(s)?;
            Ok(())

        }().is_ok());

        assert!(other_thread.join().is_err());
    }

    #[test]
    fn cancel_send_works() {

        let (other_thread, s) = fork_with_thread_id(
            move |s: Recv<(), End>| {cancel(s)});

        assert!(|| -> Result<(), Box<Error>> {

            send((), s)?;
            Ok(())

        }().is_ok());

        assert!(other_thread.join().is_ok());
    }

    #[test]
    fn delegation_works() {
        let (other_thread1, s) = fork_with_thread_id(nice_calc_server);
        let (other_thread2, u) = fork_with_thread_id(
            move |u: Recv<NiceCalcClient<i32>, End>| {cancel(u)});

        assert!(|| -> Result<(), Box<Error>> {

            let u = send(s, u)?;
            close(u)?;
            Ok(())

        }().is_ok());

        assert!(other_thread1.join().is_err());
        assert!(other_thread2.join().is_ok());
    }

    #[test]
    fn closure_works() {
        let (other_thread, s) = fork_with_thread_id(nice_calc_server);

        assert!(|| -> Result<i32, Box<Error>> {

            // Create a closure which uses the session.
            let f = move |x: i32| -> Result<i32, Box<Error>> {
                let s = choose!(CalcOp::Neg, s)?;
                let s = send(x, s)?;
                let (y, s) = recv(s)?;
                close(s)?;
                Ok(y)
            };

            // Let the closure go out of scope.
            Err(Box::new(mpsc::RecvError))?;
            f(5)

        }().is_err());

        assert!(other_thread.join().is_err());
    }

    enum SumOp<N: marker::Send> {
        More(Recv<N, NiceSumServer<N>>),
        Done(Send<N, End>),
    }
    type NiceSumServer<N> = Recv<SumOp<N>, End>;
    type NiceSumClient<N> = <NiceSumServer<N> as Session>::Dual;

    fn nice_sum_server(s: NiceSumServer<i32>) -> Result<(), Box<Error>> {
        nice_sum_server_accum(s, 0)
    }

    fn nice_sum_server_accum(s: NiceSumServer<i32>, x: i32) -> Result<(), Box<Error>> {
        offer!(s, {
            SumOp::More(s) => {
                let (y, s) = recv(s)?;
                nice_sum_server_accum(s, x.wrapping_add(y))
            },
            SumOp::Done(s) => {
                let s = send(x, s)?;
                close(s)
            },
        })
    }

    fn nice_sum_client_accum(s: NiceSumClient<i32>, mut xs: Vec<i32>)
                             -> Result<i32, Box<Error>> {
        match xs.pop() {
            Option::Some(x) => {
                let s = choose!(SumOp::More, s)?;
                let s = send(x, s)?;
                nice_sum_client_accum(s, xs)
            },
            Option::None => {
                let s = choose!(SumOp::Done, s)?;
                let (sum, s) = recv(s)?;
                close(s)?;
                Ok(sum)
            },
        }
    }

    #[test]
    fn recursion_works() {

        // Pick some random numbers.
        let mut rng = thread_rng();
        let xs: Vec<i32> = (1..100).map(|_| rng.gen()).collect();
        let sum1: i32 = xs.iter().fold(0, |sum, &x| sum.wrapping_add(x));

        let (other_thread, s) = fork_with_thread_id(nice_sum_server);

        assert!(|| -> Result<(), Box<Error>> {

            let sum2 = nice_sum_client_accum(s, xs)?;
            assert_eq!(sum1, sum2);
            Ok(())

        }().is_ok());

        assert!(other_thread.join().is_ok());
    }

    #[allow(dead_code)]
    fn deadlock_loop() {

        let s = fork(move |s: Send<(), End>| {
            loop {
                // Let's trick the reachability checker
                if false { break; }
            }
            let End = send((), s)?;
            Ok(())
        });

        || -> Result<(), Box<Error>> {
            let ((), End) = recv(s)?;
            Ok(())
        }().unwrap();
    }

    #[allow(dead_code)]
    fn deadlock_forget() {

        let s = fork(move |s: Send<(), End>| {
            mem::forget(s);
            Ok(())
        });

        || -> Result<(), Box<Error>> {
            let ((), End) = recv(s)?;
            Ok(())
        }().unwrap();
    }

//  #[allow(dead_code)]
//  fn deadlock_new() {
//
//      let (s1, r1) = <Send<Void, End>>::new();
//      let (s2, r2) = <Send<Void, End>>::new();
//      spawn(move || {
//          let (x, End) = recv(r1)?;
//          let End = send(x, s2)?;
//          Ok(())
//      });
//
//      || -> Result<(), Box<Error>> {
//          let (x, End) = recv(r2)?;
//          let End = send(x, s1)?;
//          Ok(())
//      }().unwrap();
//  }
}

// */
// */
// */
// */
// */
