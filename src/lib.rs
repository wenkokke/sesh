//! # Sesh 🥂
//!
//! Sesh is an implementation of *deadlock-free binary session types with
//! failure* in Rust.
//!
//! What does that mean?
//!
//! - The *session types* bit means that it uses Rust types to describe
//! communication protocols, and ensure that they're implemented correctly.
//! - The *binary* bit means that for any protocol, there's two parties involved, no
//! more, no less.
//! - The *deadlock-free* bit means, well, that if you write your session typed
//! programs using Sesh, and you don't cheat by using other concurrency
//! constructs, your can be sure your programs won't deadlock!
//! - The *with failure* bit means Sesh is aware that sometimes programs may
//! fail, and that it takes failure into account. If one of the parties involved
//! in a protocol crashes, like if the thread panics, the Sesh lets the other
//! party know, to make sure it doesn't end up just waiting forever.
//!
//! The author of this package is an academic, so she is contractually obliged
//! to only come up with uninspiring examples. With that in mind, let's pretend
//! you want a server that does addition. You send it two numbers, and it sends
//! you a number back. There's two session types associated with that protocol.
//! One for the client and one for the server. The type for the server, using
//! Sesh, would be:
//!
//! ```
//! # extern crate sesh;
//! # use sesh::*;
//! #
//! type AddServer = Recv<i64, Recv<i64, Send<i64, End>>>;
//! ```
//!
//! Session types are always *dual*. If the client sends a number, the server
//! should be ready to receive a number, otherwise we'd be in trouble.
//! We can get the session type for the client using duality. This just replaces
//! all sends by receives, and vice versa:
//!
//! ```
//! # extern crate sesh;
//! # use sesh::*;
//! #
//! # type AddServer = Recv<i64, Recv<i64, Send<i64, End>>>;
//! type AddClient = <AddServer as Session>::Dual;
//! ```
//!
//! Once you've written down the protocol, the hard part is out of the way.
//! Rust will make sure our server follows the protocol:
//!
//! ```
//! # extern crate sesh;
//! # use sesh::*;
//! # use std::boxed::Box;
//! # use std::error::Error;
//! #
//! # type AddServer = Recv<i64, Recv<i64, Send<i64, End>>>;
//! #
//! fn add_server(s: AddServer) -> Result<(), Box<dyn Error>> {
//!   let (i, s) = recv(s)?;  // Receive the first number.
//!   let (j, s) = recv(s)?;  // Receive the second number.
//!   let s = send(i + j, s); // Send the sum of both numbers.
//!   close(s)                // Close the session.
//! }
//! ```
#![feature(never_type)]

extern crate crossbeam_channel;
extern crate either;

use std::boxed::Box;
use std::error::Error;
use std::fmt;
use std::marker;
use std::mem;
use std::thread::{JoinHandle, spawn};
use std::panic;
use crossbeam_channel::{Sender, Receiver, bounded, Select};
use either::Either;

/// Send `T`, then continue as `S`.
#[must_use]
#[derive(Debug)]
pub struct Send<T, S: Session> {
    channel: Sender<(T, S::Dual)>,
}

/// Receive `T`, then continue as `S`.
#[must_use]
#[derive(Debug)]
pub struct Recv<T, S: Session> {
    channel: Receiver<(T, S)>,
}

/// End of communication.
#[must_use]
#[derive(Debug)]
pub struct End {
    sender: Sender<()>,
    receiver: Receiver<()>,
}

/// Trait for session types. Provides duality.
pub trait Session: marker::Sized + marker::Send {

    /// The session type dual to `Self`.
    type Dual: Session<Dual=Self>;

    /// Creates two new *dual* channels.
    ///
    /// *Here be dragons!*
    ///
    /// The `new` function is used internally in this library to define
    /// functions such as `send` and `fork`. When combined with `thread::spawn`,
    /// it can be used to construct deadlocks.
    #[doc(hidden)]
    fn new() -> (Self, Self::Dual);
}

impl Session for End {
    type Dual = End;

    #[doc(hidden)]
    fn new() -> (Self, Self::Dual) {
        let (sender1, receiver1) = bounded::<()>(1);
        let (sender2, receiver2) = bounded::<()>(1);

        return (End { sender: sender1, receiver: receiver2 },
                End { sender: sender2, receiver: receiver1 });
    }
}

impl<T: marker::Send, S: Session> Session for Send<T, S> {
    type Dual = Recv<T, S::Dual>;

    #[doc(hidden)]
    fn new() -> (Self, Self::Dual) {
        let (sender, receiver) = bounded::<(T, S::Dual)>(1);
        return (Send { channel: sender }, Recv { channel: receiver });
    }
}

impl<T: marker::Send, S: Session> Session for Recv<T, S> {
    type Dual = Send<T, S::Dual>;

    #[doc(hidden)]
    fn new() -> (Self, Self::Dual) {
        let (there, here) = Self::Dual::new();
        return (here, there);
    }
}


/// Send a value of type `T`. Always succeeds. Returns the continuation of the
/// session `S`.
pub fn send<T, S>(x: T, s: Send<T, S>) -> S
where
    T: marker::Send,
    S: Session,
{
    let (here, there) = S::new();
    s.channel.send((x, there)).unwrap_or(());
    here
}

/// Receive a value of type `T`. Can fail. Returns either a pair of the received
/// value and the continuation of the session `S` or an error.
pub fn recv<T, S>(s: Recv<T, S>) -> Result<(T, S), Box<dyn Error>>
where
    T: marker::Send,
    S: Session,
{
    let (v, s) = s.channel.recv()?;
    Ok((v, s))
}


/// Cancels a session. Always succeeds. If the partner calls `recv` or `close`
/// after cancellation, those calls fail.
pub fn cancel<T>(x: T) -> () {
    mem::drop(x);
}

/// Closes a session. Synchronises with the partner, and fails if the partner
/// has crashed.
pub fn close(s: End) -> Result<(), Box<dyn Error>> {
    s.sender.send(()).unwrap_or(());
    s.receiver.recv()?;
    Ok(())
}


#[doc(hidden)]
pub fn fork_with_thread_id<S, P>(p: P) -> (JoinHandle<()>, S::Dual)
where
    S: Session + 'static,
    P: FnOnce(S) -> Result<(), Box<dyn Error>> + marker::Send + 'static
{
    let (there, here) = Session::new();
    let other_thread = spawn(move || {
        panic::set_hook(Box::new(|_info| {
            // do nothing
        }));
        match p(there) {
            Ok(()) => (),
            Err(e) => panic!("{}", e.description()),
        }
    });
    (other_thread, here)
}

/// Creates a child process, and a session with two dual endpoints of type `S`
/// and `S::Dual`. The first endpoint is given to the child process. Returns the
/// second endpoint.
pub fn fork<S, P>(p: P) -> S::Dual
where
    S: Session + 'static,
    P: FnOnce(S) -> Result<(), Box<dyn Error>> + marker::Send + 'static
{
    fork_with_thread_id(p).1
}


/// Offer a choice between two sessions `S1` and `S1`. Implemented using `Recv`
/// and `Either`.
pub type Offer<S1, S2> =
    Recv<Either<S1, S2>, End>;

/// Choose between two sessions `S1` and `S2`. Implemented using `Send` and
/// `Either`.
pub type Choose<S1, S2> =
    Send<Either<<S1 as Session>::Dual, <S2 as Session>::Dual>, End>;

/// Offer a choice between two sessions `S1` and `S2`.
pub fn offer_either<'a, S1, S2, F, G, R>(s: Offer<S1, S2>, f: F, g: G)
                                         -> Result<R, Box<dyn Error + 'a>>
where
    S1: Session,
    S2: Session,
    F: FnOnce(S1) -> Result<R, Box<dyn Error + 'a>>,
    G: FnOnce(S2) -> Result<R, Box<dyn Error + 'a>>,
{
    let (e, s) = recv(s)?;
    cancel(s);
    e.either(f, g)
}

/// Given a choice between sessions `S1` and `S1`, choose the first option.
pub fn choose_left<'a, S1, S2>(s: Choose<S1, S2>) -> S1
where
    S1: Session + 'a,
    S2: Session + 'a,
{
    let (here, there) = S1::new();
    let s = send(Either::Left(there), s);
    cancel(s);
    here
}

/// Given a choice between sessions `S1` and `S1`, choose the second option.
pub fn choose_right<'a, S1, S2>(s: Choose<S1, S2>) -> S2
where
    S1: Session + 'a,
    S2: Session + 'a,
{
    let (here, there) = S2::new();
    let s = send(Either::Right(there), s);
    cancel(s);
    here
}


/// Offer a choice between many different sessions wrapped in an `enum`
#[macro_export]
macro_rules! offer {
    ($session:expr, { $($pat:pat => $result:expr,)* }) => {
        (move || -> Result<_, _> {
            let (l, s) = recv($session)?;
            cancel(s);
            match l {
                $(
                    $pat => $result,
                )*
            }
        })()
    };
}

/// Choose between many different sessions wrapped in an `enum`
#[macro_export]
macro_rules! choose {
    ($label:path, $session:expr) => {{
        let (here, there) = <_ as Session>::new();
        let s = send($label(there), $session);
        cancel(s);
        here
    }};
}


/// Error returned when `select` or `select_mut` are called with an empty vector.
#[derive(Debug)]
enum SelectError {
    EmptyVec,
}

impl fmt::Display for SelectError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            SelectError::EmptyVec =>
                write!(f, "please use a vector with at least one element"),
        }
    }
}

impl Error for SelectError {
    fn description(&self) -> &str {
        match *self {
            SelectError::EmptyVec => "empty vectors not allowed",
        }
    }

    fn cause(&self) -> Option<&dyn Error> {
        match *self {
            SelectError::EmptyVec => None,
        }
    }
}

/// Selects the first active session. Receives from the selected session, and
/// removes the endpoint from the input vector. Returns the received value and
/// the continuation of the selected session.
pub fn select_mut<T, S>(rs: &mut Vec<Recv<T, S>>) -> Result<(T, S), Box<dyn Error>>
where
    T: marker::Send,
    S: Session,
{
    if rs.is_empty() {
        Err(Box::new(SelectError::EmptyVec))
    }
    else {
        let (index, res) = {
            let mut sel = Select::new();
            let iter = rs.iter();
            for r in iter {
                sel.recv(&r.channel);
            }
            loop {
                let index = sel.ready();
                let res = rs[index].channel.try_recv();

                if let Err(e) = res {
                    if e.is_empty() {
                        continue;
                    }
                }

                break (index, res);
            }
        };

        let _ = rs.swap_remove(index);
        match res {
            Ok(res) => Ok(res),
            Err(e)  => Err(Box::new(e)),
        }
    }
}

/// Selects the first active session. Receives from the selected session.
/// Returns the received value, the continuation of the selected session, and a
/// copy of the input vector without the selected session.
pub fn select<T, S>(rs: Vec<Recv<T, S>>) -> (Result<(T, S), Box<dyn Error>>, Vec<Recv<T, S>>)
where
    T: marker::Send,
    S: Session,
{
    let mut rs = rs;
    let res = select_mut(&mut rs);
    (res, rs)
}
