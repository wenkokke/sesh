use std::marker::PhantomData;
use std::mem;
use std::thread;
use std::sync::mpsc::{SyncSender, Receiver};
use std::sync::mpsc;

// Session types and duality
struct Terminate(SyncSender<()>);
struct Wait(Receiver<()>);
struct Send<A, S: Session>(SyncSender<(A, <S as Session>::Dual)>);
struct Receive<A, S: Session>(Receiver<(A, <S as Session>::Dual)>);

trait Session {
    type Dual: Session;

    type Chan;

    fn wrap(c: Self::Chan) -> Self;
}
impl Session for Terminate
{
    type Dual = Wait;
    type Chan = SyncSender<()>;

    fn wrap(c: Self::Chan) -> Self { Terminate(c) }
}
impl Session for Wait
{
    type Dual = Terminate;
    type Chan = Receiver<()>;

    fn wrap(c: Self::Chan) -> Self { Wait(c) }
}
impl<A, S: Session> Session for Send<A, S>
where
    <S as Session>::Dual: Session
{
    type Dual = Receive<A, <S as Session>::Dual>;
    type Chan = SyncSender<(A, <S as Session>::Dual)>;

    fn wrap(c: Self::Chan) -> Self { Send(c) }
}
impl<A, S: Session> Session for Receive<A, S>
where
    <S as Session>::Dual: Session
{
    type Dual = Send<A, <S as Session>::Dual>;
    type Chan = Receiver<(A, <S as Session>::Dual)>;

    fn wrap(c: Self::Chan) -> Self { Receive(c) }
}


fn fork<S: Session, K: FnOnce(S) -> Terminate>(k: K) -> <S as Session>::Dual {
    let (ct, cw) = mpsc::sync_channel::<()>(0);
    let st = <S as Session>::wrap(ct);
    let sw = <<S as Session>::Dual as Session>::wrap(cw);
    thread::spawn(move || {
        mem::forget(k(sw));
    });
    return sw;
}
fn send<A, S: Session>(v: A, s: Send<A,S>) -> S
{
}
fn receive<A, S: Session>(s: Receive<A,S>) -> (A, S)
{
}
fn wait(s: Wait) -> ()
{
}
fn link<S: Session>(s1: S, s2: <S as Session>::Dual) -> Terminate
{
}

#[cfg(test)]
mod tests {
    use std::ops::Not;
    use super::{Send,Terminate};

    // Test if the implementation of Not is correct
    type S = Send<(),Send<(),Terminate>>;
    type NotS = <S as Not>::Output;
    type NotNotS = <NotS as Not>::Output;

    #[allow(dead_code,unused_variables)]
    fn a_equal_to_c(s: S) {
        let not_s: NotS = Not::not(s);
        let not_not_s: NotNotS = Not::not(not_s);
    }
}
