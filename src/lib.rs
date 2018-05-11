extern crate either;

#[macro_use]
pub mod rv {

    use either::{Either};
    use std::marker;
    use std::sync::mpsc::{Sender, Receiver};
    use std::sync::mpsc;

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

    pub fn select_left<S1: Session, S2: Session>(s: Select<S1, S2>) -> S1 {
        let (here, there) = S1::new();
        let End = send(Either::Left(there), s);
        return here
    }

    pub fn select_right<S1: Session, S2: Session>(s: Select<S1, S2>) -> S2 {
        let (here, there) = S2::new();
        let End = send(Either::Right(there), s);
        return here
    }

    pub fn offer<S1: Session, S2: Session, P1, P2, R>(s: Offer<S1, S2>, p1: P1, p2: P2) -> R
    where
        P1: FnOnce(S1) -> R,
        P2: FnOnce(S2) -> R,
    {
        let (e, End) = receive(s);
        match e {
            Either::Left(s) => p1(s),
            Either::Right(s) => p2(s),
        }
    }

    #[macro_export]
    macro_rules! fork {
        (move | $session_name:ident : $session_type:ty | $forked_process:block ) => {{
            let ($session_name, there) = <$session_type as $crate::rv::Session>::new();
            ::std::thread::spawn(move || {
                $forked_process;
            });
            there
        }};
    }
}

#[cfg(test)]
mod tests {
    extern crate rand;

    use rv::*;
    use self::rand::{Rng, thread_rng};

    // Types for a calculator server and its client
    type NegServer<N> = Receive<N, Send<N, End>>;
    type AddServer<N> = Receive<N, Receive<N, Send<N, End>>>;
    type CalcServer<N> = Offer<NegServer<N>, AddServer<N>>;
    type NegClient<N> = <NegServer<N> as Session>::Dual;
    type AddClient<N> = <AddServer<N> as Session>::Dual;
    type CalcClient<N> = <CalcServer<N> as Session>::Dual;

    #[test]
    fn calculator_server_works() {

        // Fork our calculator server
        let s: CalcClient<i32> = fork!(move |s: CalcServer<i32>| {
            offer(s,
                  |s: NegServer<i32>| {
                      let (x, s) = receive(s);
                      let End = send(-x, s);
                  },
                  |s: AddServer<i32>| {
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
        let s: AddClient<i32> = select_right::<NegClient<i32>, _>(s);
        let s = send(x, s);
        let s = send(y, s);
        let (z, End) = receive(s);

        // Check the results
        assert_eq!(x.wrapping_add(y), z);
    }
}

// */
