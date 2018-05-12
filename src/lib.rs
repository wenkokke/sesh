#[macro_use]
pub mod rv {

    extern crate either;

    use self::either::Either;
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
            let ($session_name, here) = <$session_type as $crate::rv::Session>::new();
            ::std::thread::spawn(move || {
                $forked_process;
            });
            here
        }};
    }

    pub type Offer<S1, S2> = Receive<Either<S1, S2>, End>;
    pub type Select<S1, S2> = Send<Either<<S1 as Session>::Dual, <S2 as Session>::Dual>, End>;

    pub fn offer<S1: Session, S2: Session, F, G, R>(s: Offer<S1, S2>, f: F, g: G) -> Option<R>
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
        ($session:expr, { $($pat:pat => $result:expr,)* }) => {{
            receive($session).map(
                |(l, End)|
                match l {
                    $(
                        $pat => $result,
                    )*
                });
        }};
    }

    #[macro_export]
    macro_rules! select {
        ($label:path, $session:expr) => {{
            let (here, there) = <_ as Session>::new();
            send($label(there), $session).map(|End| here)
        }};
    }
}

#[cfg(test)]
mod tests {
    extern crate rand;

    use rv::*;
    use self::rand::{Rng, thread_rng};

    /// Simplest test -- sending a ping.
    #[test]
    fn ping_works() {
        let s = fork!(move |s: Send<(), End>| {
            send((), s)
        });
        receive(s);
    }

    type NegServer<N> = Receive<N, Send<N, End>>;
    type AddServer<N> = Receive<N, Receive<N, Send<N, End>>>;
    type NegClient<N> = <NegServer<N> as Session>::Dual;
    type AddClient<N> = <AddServer<N> as Session>::Dual;

    type SimpleCalcServer<N> = Offer<NegServer<N>, AddServer<N>>;
    type SimpleCalcClient<N> = <SimpleCalcServer<N> as Session>::Dual;

    fn calc_server(s: SimpleCalcServer<i32>) -> Option<()> {
        offer(s,
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
    }

    fn calc_client(x: i32, y: i32, s: SimpleCalcClient<i32>) -> Option<i32> {
        let s = select_right::<NegClient<i32>, _>(s)?;
        let s = send(x, s)?;
        let s = send(y, s)?;
        let (z, End) = receive(s)?;
        Some(z)
    }

    #[test]
    fn calc_works() {
        let mut rng = thread_rng();
        let x: i32 = rng.gen();
        let y: i32 = rng.gen();

        let s = fork!(move |s: SimpleCalcServer<i32>| {
            calc_server(s)
        });
        let r = calc_client(x, y, s);

        assert!(r.is_some());
        assert_eq!(x.wrapping_add(y), r.unwrap());
    }
}

// */
// */
// */
// */
// */
