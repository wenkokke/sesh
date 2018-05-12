#![feature(fnbox)]

extern crate either;

#[macro_use]
pub mod rv {

    use either::{Either};
    use std::boxed::{FnBox};
    use std::marker;
    use std::sync::mpsc::{Sender, Receiver};
    use std::sync::mpsc;
    use std::thread;

    pub struct End;
    pub struct Send<T, S: Session> {
        channel: Sender<(T, S::Dual)>,
    }
    pub struct Receive<T, S: Session> {
        channel: Receiver<(T, S)>,
    }
    pub type Offer<S1, S2> =
        Receive<Either<S1, S2>, End>;
    pub type Select<S1, S2> =
        Send<Either<<S1 as Session>::Dual, <S2 as Session>::Dual>, End>;

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

    pub fn send<T: marker::Send, S: Session>(value: T, session: Send<T, S>) -> S {
        let (here, there) = S::new();
        session.channel.send((value, there)).unwrap();
        return here;
    }

    pub fn receive<T: marker::Send, S: Session>(session: Receive<T, S>) -> (T, S) {
        return session.channel.recv().unwrap();
    }

    pub fn close(session: End) -> () {
        let End = session;
    }

    #[macro_export]
    macro_rules! fork {
        (move | $session_name:ident : $session_type:ty | $forked_process:block ) => {{
            let ($session_name, here) = <$session_type as $crate::rv::Session>::new();
            ::std::thread::spawn(move || {
                $forked_process;
            });
            here
        }};
    }

    #[macro_export]
    macro_rules! select {
        ($label:path, $session:expr) => {{
            let (here, there) = <_ as Session>::new();
            let End = send($label(there), $session);
            return here
        }};
    }

    pub fn select_left<S1: Session, S2: Session>(s: Select<S1, S2>) -> S1 {
        select!(Either::Left, s)
    }

    pub fn select_right<S1: Session, S2: Session>(s: Select<S1, S2>) -> S2 {
        select!(Either::Right, s)
    }

    #[macro_export]
    macro_rules! offer {
        ($session:expr, { $($pat:pat => $result:expr,)* }) => {{
            let (l, End) = receive($session);
            match l {
                $($pat => $result,)*
            }
        }};
    }

    pub fn offer<S1: Session, S2: Session, F, G, R>(s: Offer<S1, S2>, f: F, g: G) -> R
    where F: FnOnce(S1) -> R, G: FnOnce(S2) -> R, {
        offer!(s, {
            Either::Left(s) => f(s),
            Either::Right(s) => g(s),
        })
    }
}

#[cfg(test)]
#[allow(dead_code)]
mod tests {
    extern crate rand;

    use rv::*;
    use self::rand::{Rng, thread_rng};

    // Types for a calculator server and its client
    type NegServer<N>  = Receive<N, Send<N, End>>;
    type AddServer<N>  = Receive<N, Receive<N, Send<N, End>>>;
    type CalcServer<N> = Offer<NegServer<N>, AddServer<N>>;
    type NegClient<N>  = <NegServer<N> as Session>::Dual;
    type AddClient<N>  = <AddServer<N> as Session>::Dual;
    type CalcClient<N> = <CalcServer<N> as Session>::Dual;

    #[test]
    fn calculator_server_works() {

        // Fork our calculator server
        let s: CalcClient<i32> = fork!(move |s: CalcServer<i32>| {
            offer(s,
                  |s| {
                      let (x, s) = receive(s);
                      let End = send(-x, s);
                  },
                  |s| {
                      let (x, s) = receive(s);
                      let (y, s) = receive(s);
                      let End = send(x.wrapping_add(y), s);
                  });
        });

        // Pick some random numbers
        let mut rng = thread_rng();
        let x: i32 = rng.gen();
        let y: i32 = rng.gen();

        // Send them to the calculator server
        let s = select_right::<NegClient<i32>, _>(s);
        let s = send(x, s);
        let s = send(y, s);
        let (z, End) = receive(s);

        // Check the results
        assert_eq!(x.wrapping_add(y), z);
    }
}

// */
