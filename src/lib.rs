extern crate crossbeam_channel;
extern crate either;

use std::boxed::Box;
use std::error::Error;
use std::marker;
use std::mem;
use std::thread::{JoinHandle, spawn};
use crossbeam_channel::{Sender, Receiver, Select, bounded};
use either::Either;

/// The session types supported.

#[derive(Debug)]
pub struct End {
    sender: Sender<()>,
    receiver: Receiver<()>,
}

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
        let (sender1, receiver1) = bounded::<()>(1);
        let (sender2, receiver2) = bounded::<()>(1);

        return (End { sender: sender1, receiver: receiver2 },
                End { sender: sender2, receiver: receiver1 });
    }
}

impl<T: marker::Send, S: Session> Session for Send<T, S> {
    type Dual = Recv<T, S::Dual>;

    fn new() -> (Self, Self::Dual) {
        let (sender, receiver) = bounded::<(T, S::Dual)>(1);
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


// Send and receive

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



// Cancellation and closing

pub fn cancel<T>(x: T) -> Result<(), Box<Error>> {
    mem::drop(x);
    Ok(())
}

pub fn close(s: End) -> Result<(), Box<Error>> {
    s.sender.send(())?;
    s.receiver.recv()?;
    Ok(())
}


// Fork

pub fn fork_with_thread_id<S, P>(p: P) -> (JoinHandle<()>, S::Dual)
where
    S: Session + 'static,
    P: FnOnce(S) -> Result<(), Box<Error>> + marker::Send + 'static
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
    P: FnOnce(S) -> Result<(), Box<Error>> + marker::Send + 'static
{
    fork_with_thread_id(p).1
}


// Binary choice

pub type Offer<S1, S2> =
    Recv<Either<S1, S2>, End>;
pub type Choose<S1, S2> =
    Send<Either<<S1 as Session>::Dual, <S2 as Session>::Dual>, End>;


pub fn offer_either<'a, S1, S2, F, G, R>(s: Offer<S1, S2>, f: F, g: G)
                                         -> Result<R, Box<Error + 'a>>
where
    S1: Session,
    S2: Session,
    F: FnOnce(S1) -> Result<R, Box<Error + 'a>>,
    G: FnOnce(S2) -> Result<R, Box<Error + 'a>>,
{
    let (e, s) = recv(s)?;
    close(s)?;
    e.either(f, g)
}

pub fn choose_left<'a, S1, S2>(s: Choose<S1, S2>) -> Result<S1, Box<Error + 'a>>
where
    S1: Session + 'a,
    S2: Session + 'a,
{
    let (here, there) = S1::new();
    let s = send(Either::Left(there), s)?;
    close(s)?;
    Ok(here)
}

pub fn choose_right<'a, S1, S2>(s: Choose<S1, S2>) -> Result<S2, Box<Error + 'a>>
where
    S1: Session + 'a,
    S2: Session + 'a,
{
    let (here, there) = S2::new();
    let s = send(Either::Right(there), s)?;
    close(s)?;
    Ok(here)
}


// N-ary choice

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


// Selection

pub fn select<'a, T, S, I>(rs: &[Recv<T, S>]) -> Result<(T, S), Box<Error + 'a>>
where
    T: marker::Send + 'a,
    S: Session + 'a,
{
    let mut sel = Select::new();
    for r in rs {
        sel.recv(&r.channel);
    }
    loop {
        let oper = sel.select();
        let index = oper.index();
        let (x, s) = oper.recv(&rs[index].channel)?;
        break Ok((x, s));
    }
}
