#![feature(test)]

extern crate rusty_variation;
extern crate session_types;
extern crate rand;
extern crate test;

#[cfg(test)]
mod rusty_variation_bench {

    use std::boxed::Box;
    use std::error::Error;
    use rand::{Rng,thread_rng};
    use test::Bencher;
    use rusty_variation::*;

    // Calculator server

    #[allow(dead_code)]
    enum CalcOp {
        CLOSE(End),
        ADD(Recv<i64, Recv<i64, Send<i64, CalcSrv>>>),
        NEGATE(Recv<i64, Send<i64, CalcSrv>>),
        SQRT(Recv<f64, Send<f64, CalcSrv>>),
        EVAL(Recv<fn(i64) -> bool, Recv<i64, Send<bool, CalcSrv>>>)
    }
    type CalcSrv = Recv<CalcOp, End>;
    type CalcCli = <CalcSrv as Session>::Dual;

    fn calc_server(s: CalcSrv) -> Result<(), Box<Error>> {
        ::rusty_variation::offer!(s, {
            CalcOp::CLOSE(s) => {
                close(s)
            },
            CalcOp::ADD(s) => {
                let (x, s) = recv(s)?;
                let (y, s) = recv(s)?;
                let s = send(x + y, s)?;
                calc_server(s)
            },
            CalcOp::NEGATE(s) => {
                let (x, s) = recv(s)?;
                let s = send(-x, s)?;
                calc_server(s)
            },
            CalcOp::SQRT(s) => {
                let (x, s) = recv(s)?;
                let s = send(x.sqrt(), s)?;
                calc_server(s)
            },
            CalcOp::EVAL(s) => {
                let (f, s) = recv(s)?;
                let (x, s) = recv(s)?;
                let s = send(f(x), s)?;
                calc_server(s)
            },
        })
    }

    fn neg_client(s: CalcCli) -> Result<(), Box<Error>> {
        let n = thread_rng().gen();
        let s = ::rusty_variation::choose!(CalcOp::NEGATE, s)?;
        let s = send(n, s)?;
        let (n_, s) = recv(s)?;
        assert_eq!(-n, n_);
        let s = ::rusty_variation::choose!(CalcOp::CLOSE, s)?;
        close(s)
    }

    #[bench]
    fn bench_calc_server(b: &mut Bencher) {
        b.iter(|| {
            let s = fork!(calc_server);
            assert!(neg_client(s).is_ok());
        });
    }
}

#[cfg(test)]
mod session_types_bench {

    use rand::{Rng,thread_rng};
    use test::Bencher;
    use session_types::*;

    // Calculator server

    type CalcSrv =
        Offer<
                Eps,
            Offer<
                    Recv<i64, Recv<i64, Send<i64, Var<Z>>>>,
                Offer<
                        Recv<i64, Send<i64, Var<Z>>>,
                    Offer<
                            Recv<f64, Choose<Send<f64, Var<Z>>, Var<Z>>>,
                        Recv<fn(i64) -> bool, Recv<i64, Send<bool, Var<Z>>>>,
                        >,
                    >,
                >,
            >;

    type NegCli<R, S> =
        Choose<Eps, Choose<R, Choose<Send<i64, Recv<i64, Var<Z>>>, S>>>;


    fn calc_server(c: Chan<(), Rec<CalcSrv>>) {
        let mut c = c.enter();
        loop {
            c = ::session_types::offer!{ c,
                        CLOSE => {
                            c.close();
                            return
                        },
                        ADD => {
                            let (c, n) = c.recv();
                            let (c, m) = c.recv();
                            c.send(n + m).zero()
                        },
                        NEGATE => {
                            let (c, n) = c.recv();
                            c.send(-n).zero()
                        },
                        SQRT => {
                            let (c, x) = c.recv();
                            if x >= 0.0 {
                                c.sel1().send(x.sqrt()).zero()
                            } else {
                                c.sel2().zero()
                            }
                        },
                        EVAL => {
                            let (c, f) = c.recv();
                            let (c, n) = c.recv();
                            c.send(f(n)).zero()
                        }
            }
        }
    }


    fn neg_client<R, S>(c: Chan<(), Rec<NegCli<R, S>>>) {
        let n = thread_rng().gen();
        let (c, n_) = c.enter().skip2().sel1().send(n).recv();
        c.zero().sel1().close();
        assert_eq!(-n, n_);
    }

    #[bench]
    fn bench_calc_server(b: &mut Bencher) {
        b.iter(|| connect(calc_server, neg_client));
    }
}
