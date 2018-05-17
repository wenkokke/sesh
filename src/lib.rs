#[macro_use]
extern crate either;

use either::Either;
use std::convert::From;
use std::error::Error;
use std::fmt;
use std::marker;
use std::sync::mpsc::{Sender, Receiver};
use std::sync::mpsc;

// Define custom error type for Rusty Variation

pub enum RvError<T> {
    Cancel,
    SendError(mpsc::SendError<T>),
    ReceiveError(mpsc::RecvError),
}

impl<T> fmt::Debug for RvError<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        "RvError(..)".fmt(f)
    }
}

impl<T> fmt::Display for RvError<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RvError::Cancel => write!(f, "acting on a cancelled channel"),
            RvError::SendError(e) => e.fmt(f),
            RvError::ReceiveError(e) => e.fmt(f),
        }
    }
}

impl<T: marker::Send> Error for RvError<T> {
    fn description<'a>(&'a self) -> &'a str {
        match self {
            RvError::Cancel => "acting on a cancelled channel",
            RvError::SendError(e) => e.description(),
            RvError::ReceiveError(e) => e.description(),
        }
    }

    fn cause(&self) -> Option<&Error> {
        match self {
            RvError::Cancel => None,
            RvError::SendError(e) => Some(e),
            RvError::ReceiveError(e) => Some(e),
        }
    }
}

impl<T: marker::Send> From<mpsc::SendError<T>> for RvError<T> {
    fn from(error: mpsc::SendError<T>) -> Self {
        RvError::SendError(error)
    }
}

impl<T> From<mpsc::RecvError> for RvError<T> {
    fn from(error: mpsc::RecvError) -> Self {
        RvError::ReceiveError(error)
    }
}

pub struct End;
pub struct Send<T, S: Session> {
    channel: Sender<(T, S::Dual)>,
}
pub struct Receive<T, S: Session> {
    channel: Receiver<(T, S)>,
}

pub trait Session: marker::Sized + marker::Send {
    type Dual: Session<Dual=Self>;

    fn new() -> (Self, Self::Dual);
}

impl Session for End {
    type Dual = End;

    fn new() -> (Self, Self::Dual) {
        return (End, End);
    }
}

impl<T: marker::Send, S: Session> Session for Send<T, S> {
    type Dual = Receive<T, S::Dual>;

    fn new() -> (Self, Self::Dual) {
        let (sender, receiver) = mpsc::channel::<(T, S::Dual)>();
        return (Send { channel: sender }, Receive { channel: receiver });
    }
}

impl<T: marker::Send, S: Session> Session for Receive<T, S> {
    type Dual = Send<T, S::Dual>;

    fn new() -> (Self, Self::Dual) {
        let (there, here) = Self::Dual::new();
        return (here, there);
    }
}

pub fn send<T: marker::Send, S: Session>(x: T, s: Send<T, S>) -> Option<S> {
    let (here, there) = S::new();
    s.channel.send((x, there)).ok()?;
    Some(here)
}

pub fn receive<T: marker::Send, S: Session>(s: Receive<T, S>) -> Option<(T, S)> {
    s.channel.recv().ok()
}

pub fn close(s: End) -> Option<()> {
    let End = s;
    Some(())
}

#[macro_export]
macro_rules! fork {
    (move | $session_name:ident : $session_type:ty | $forked_process:block ) => {{
        let ($session_name, here) = <$session_type as $crate::Session>::new();
        ::std::thread::spawn(move || {
            (move || -> Option<()> {
                $forked_process
            })();
        });
        here
    }};
}

pub type Offer<S1, S2> = Receive<Either<S1, S2>, End>;
pub type Select<S1, S2> = Send<Either<<S1 as Session>::Dual, <S2 as Session>::Dual>, End>;

pub fn offer_either<S1: Session, S2: Session, F, G, R>(s: Offer<S1, S2>, f: F, g: G) -> Option<R>
where
    F: FnOnce(S1) -> Option<R>,
    G: FnOnce(S2) -> Option<R>,
{
    let (e, End) = receive(s)?;
    e.either(f, g)
}

pub fn select_left<S1: Session, S2: Session>(s: Select<S1, S2>) -> Option<S1> {
    let (here, there) = S1::new();
    let End = send(Either::Left(there), s)?;
    Some(here)
}

pub fn select_right<S1: Session, S2: Session>(s: Select<S1, S2>) -> Option<S2> {
    let (here, there) = S2::new();
    let End = send(Either::Right(there), s)?;
    Some(here)
}

#[macro_export]
macro_rules! offer {
    ($session:expr, { $($pat:pat => $result:expr,)* }) => {
        (move || -> Option<()> {
            let (l, End) = receive($session)?;
            match l {
                $(
                    $pat => $result,
                )*
            }
        })();
    };
}

#[macro_export]
macro_rules! select {
    ($label:path, $session:expr) => {
        (move || -> Option<_> {
            let (here, there) = <_ as Session>::new();
            let End = send($label(there), $session)?;
            Some(here)
        })();
    };
}

#[cfg(test)]
#[allow(dead_code)]
mod tests {
    extern crate rand;

    use std::marker;
    use super::*;
    use self::rand::{Rng, thread_rng};

    // Test sending a ping across threads.

    #[test]
    fn ping_works() {
        assert!(|| -> Option<()> {

            let s = fork!(move |s: Send<(), End>| {
                let s = send((), s)?;
                close(s)
            });
            let ((), s) = receive(s)?;
            close(s)

        }().is_some());
    }

    // Test a simple calculator server, implemented using binary choice.

    type NegServer<N> = Receive<N, Send<N, End>>;
    type NegClient<N> = <NegServer<N> as Session>::Dual;

    type AddServer<N> = Receive<N, Receive<N, Send<N, End>>>;
    type AddClient<N> = <AddServer<N> as Session>::Dual;

    type SimpleCalcServer<N> = Offer<NegServer<N>, AddServer<N>>;
    type SimpleCalcClient<N> = <SimpleCalcServer<N> as Session>::Dual;

    #[test]
    fn simple_calc_works() {
        assert!(|| -> Option<()> {

            // Pick some random numbers.
            let mut rng = thread_rng();
            let x: i32 = rng.gen();
            let y: i32 = rng.gen();

            // Create a calculator server and send it the numbers.
            let s = fork!(move |s: SimpleCalcServer<i32>| {
                offer_either(s,
                      |s: NegServer<i32>| {
                          let (x, s) = receive(s)?;
                          let s = send(-x, s)?;
                          close(s)
                      },
                      |s: AddServer<i32>| {
                          let (x, s) = receive(s)?;
                          let (y, s) = receive(s)?;
                          let s = send(x.wrapping_add(y), s)?;
                          close(s)
                      })

            });

            let s = select_right::<NegClient<i32>, _>(s)?;
            let s = send(x, s)?;
            let s = send(y, s)?;
            let (z, End) = receive(s)?;

            // Check if the server worked.
            assert_eq!(x.wrapping_add(y), z);
            Some(())

        }().is_some());
    }

    // Test a nice calculator server, implemented using variant types.

    enum Op<N: marker::Send> {
        Neg(NegServer<N>),
        Add(AddServer<N>),
    }
    type NiceCalcServer<N> = Receive<Op<N>, End>;
    type NiceCalcClient<N> = <NiceCalcServer<N> as Session>::Dual;

    #[test]
    fn nice_server_works() {
        assert!(|| -> Option<()> {

            // Pick some random numbers.
            let mut rng = thread_rng();
            let x: i32 = rng.gen();
            let y: i32 = rng.gen();

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

            let s = select!(Op::Add, s)?;
            let s = send(x, s)?;
            let s = send(y, s)?;
            let (z, s) = receive(s)?;
            close(s);

            // Check if the server worked correctly.
            assert_eq!(x.wrapping_add(y), z);

            Some(())

        }().is_some());
    }
}

// */
// */
// */
// */
// */
