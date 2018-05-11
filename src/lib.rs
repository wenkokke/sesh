extern crate rand;

#[macro_use]
pub mod rv {

    use std::marker;
    use std::sync::mpsc::{Sender,Receiver};
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

    pub fn send<T: marker::Send, S: Session>(value: T, session: Send<T, S>) -> S {
        let (here, there) = S::new();
        session.channel.send((value, there)).unwrap();
        return here;
    }
    pub fn receive<T: marker::Send, S: Session>(session: Receive<T, S>) -> (T, S) {
        return session.channel.recv().unwrap();
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
    use rand::{Rng, thread_rng};
    use rv::{Send,Receive,End,send,receive};

    #[test]
    fn it_works() {

        // Fork our "addition" process
        let s = fork!(move |s: Receive<u32,Receive<u32,Send<u32,End>>>| {
            let (x, s) = receive(s);
            let (y, s) = receive(s);
            let End = send(x.wrapping_add(y), s);
        });

        // Pick some random numbers
        let mut rng = thread_rng();
        let x: u32 = rng.gen();
        let y: u32 = rng.gen();

        // Send them to the "addition" process
        let s = send(x, s);
        let s = send(y, s);
        let (z, End) = receive(s);

        // Check the results
        assert_eq!(x.wrapping_add(y), z);

    }
}
