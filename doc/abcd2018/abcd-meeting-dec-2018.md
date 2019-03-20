![right](rv.svg)

# Rusty Variation

(or, Deadlock-free sessions with failure in Rust)

by Wen Kokke

---

# [fit] A Tale of Four Examples

---

# Exceptional GV

(by Fowler et al.)

Looks like this:

$$
\begin{array}{l}
\mathbf{let} \; s = \mathbf{fork}(\lambda (s : \; !\mathbf{1}.\text{End}).\\
\qquad \mathbf{let} \; s = \mathbf{send}((), s)\\
\qquad \mathbf{close}(s)\\
)\\
\mathbf{let} \; ((), s) = \mathbf{recv}(s)\\
\mathbf{close}(s)\\
\end{array}
$$

---

# Rusty Variation

(by me)

Looks like this:

```rust
        let s = fork!(move |s: Send<(), End>| {
            let s = send((), s)?;
            close(s)
        });
        let ((), s) = recv(s)?;
        close(s)
```

---

# [fit] I know, the fonts are very different

---

![right](rv.svg)

# Roadmap

- talk about Exceptional GV
- talk about Rusty Variation
- what are the differences?
- what are the similarities?

---

# Exceptional GV

Let's see how our example EGV program executes!

$$
\bullet\left(
\qquad
\begin{array}{l}
\mathbf{let} \; s = \mathbf{fork}(\lambda (s : \; !\mathbf{1}.\text{End}).\\
\qquad \mathbf{let} \; s = \mathbf{send}((), s)\\
\qquad \mathbf{close}(s)\\
)\\
\mathbf{let} \; ((), s) = \mathbf{recv}(s)\\
\mathbf{close}(s)
\end{array}
\qquad
\right)
$$

We mark the main thread with a $$\bullet$$
Next we evaluate the fork instruction

---

# Exceptional GV

Let's see how our example EGV program executes!

$$
(\nu a)(\nu b)
\left(
\qquad
\begin{array}{lr}
\bullet\left(
\begin{array}{l}
\mathbf{let} \; s = a\\
\mathbf{let} \; ((), s) = \mathbf{recv}(s)\\
\mathbf{close}(s)
\end{array}
\right)
&\|\\
\circ\left(
\begin{array}{l}
\mathbf{let} \; s = \mathbf{send}((), b)\\
\mathbf{close}(s)
\end{array}
\right)
&\|\\
a(\epsilon){\leftrightsquigarrow}b(\epsilon)
\end{array}
\qquad
\right)
$$

This forks off the process and allocates a buffer
Next we evaluate the let binding

---

# Exceptional GV

Let's see how our example EGV program executes!

$$
(\nu a)(\nu b)
\left(
\qquad
\begin{array}{lr}
\bullet\left(
\begin{array}{l}
\mathbf{let} \; ((), s) = \mathbf{recv}(a)\\
\mathbf{close}(s)
\end{array}
\right)
&\|\\
\circ\left(
\begin{array}{l}
\mathbf{let} \; s = \mathbf{send}((), b)\\
\mathbf{close}(s)
\end{array}
\right)
&\|\\
a(\epsilon){\leftrightsquigarrow}b(\epsilon)
\end{array}
\qquad
\right)
$$

The receive instruction blocks on the empty buffer
Next we evaluate the send instruction

---

# Exceptional GV

Let's see how our example EGV program executes!

$$
(\nu a)(\nu b)
\left(
\qquad
\begin{array}{lr}
\bullet\left(
\begin{array}{l}
\mathbf{let} \; ((), s) = \mathbf{recv}(a)\\
\mathbf{close}(s)
\end{array}
\right)
&\|\\
\circ\left(
\begin{array}{l}
\mathbf{let} \; s = b\\
\mathbf{close}(s)
\end{array}
\right)
&\|\\
a((),\epsilon){\leftrightsquigarrow}b(\epsilon)
\end{array}
\qquad
\right)
$$

This moves the value to the buffer
Next we evaluate the let binding

---

# Exceptional GV

Let's see how our example EGV program executes!

$$
(\nu a)(\nu b)
\left(
\qquad
\begin{array}{lr}
\bullet\left(
\begin{array}{l}
\mathbf{let} \; ((), s) = \mathbf{recv}(a)\\
\mathbf{close}(s)
\end{array}
\right)
&\|\\
\circ\left(
\begin{array}{l}
\mathbf{close}(b)
\end{array}
\right)
&\|\\
a((),\epsilon){\leftrightsquigarrow}b(\epsilon)
\end{array}
\qquad
\right)
$$

The close instruction blocks (it is synchronous)
Next we evaluate the receive instruction

---

# Exceptional GV

Let's see how our example EGV program executes!

$$
(\nu a)(\nu b)
\left(
\qquad
\begin{array}{lr}
\bullet\left(
\begin{array}{l}
\mathbf{let} \; ((), s) = ((), a)\\
\mathbf{close}(s)
\end{array}
\right)
&\|\\
\circ\left(
\begin{array}{l}
\mathbf{close}(b)
\end{array}
\right)
&\|\\
a(\epsilon){\leftrightsquigarrow}b(\epsilon)
\end{array}
\qquad
\right)
$$

This moves the value to the main thread
Next we evaluate the let binding

---

# Exceptional GV

Let's see how our example EGV program executes!

$$
(\nu a)(\nu b)
\left(
\qquad
\begin{array}{lr}
\bullet\left(
\begin{array}{l}
\mathbf{close}(a)
\end{array}
\right)
&\|\\
\circ\left(
\begin{array}{l}
\mathbf{close}(b)
\end{array}
\right)
&\|\\
a(\epsilon){\leftrightsquigarrow}b(\epsilon)
\end{array}
\qquad
\right)
$$

The close instructions are no longer blocked

(The buffer is empty and there is a close instruction waiting on either side)

Next we evaluate the close instructions

---

# Exceptional GV

Let's see how our example EGV program executes!

$$
\bullet\,()
$$

Fin

---

# Rusty Variation

What about our Rust program?

```rust
        let s = fork!(move |s: Send<(), End>| {
            let s = send((), s)?;
            close(s)
        });
        let ((), s) = recv(s)?;
        close(s)
```

---

# Rusty Variation

[.code-highlight: 1-4]
```rust
        let s = fork!(move |s: Send<(), End>| {
            let s = send((), s)?;
            close(s)
        });
        let ((), s) = recv(s)?;
        close(s)
```

^
The fork marcro is elaborated.

---

# Rusty Variation

[.code-highlight: 1-12]
```rust
        let (s, here) = <Send<(), End> as Session>::new();
        std::thread::spawn(move || {
            let r = (move || -> Result<_, Box<Error>> {
                let s = send((), s)?;
                close(s)
            })();
            match r {
                Ok(_) => (),
                Err(e) => panic!("{}", e.description()),
            }
        });
        let s = here
        let ((), s) = recv(s)?;
        close(s)
```

^
This reuse of 's' is confusing.
Let me do some alpha renaming.

---

# Rusty Variation

[.code-highlight: 1-11]
```rust
        let (b, a) = <Send<(), End> as Session>::new();
        std::thread::spawn(move || {
            let r = (move || -> Result<_, Box<Error>> {
                let b = send((), b)?;
                close(b)
            })();
            match r {
                Ok(_) => (),
                Err(e) => panic!("{}", e.description()),
            }
        });
        let ((), a) = recv(a)?;
        close(a)
```

---

# Rusty Variation

[.code-highlight: 1]
```rust
        let (b, a) = <Send<(), End> as Session>::new();
        std::thread::spawn(move || {
            let r = (move || -> Result<_, Box<Error>> {
                let b = send((), b)?;
                close(b)
            })();
            match r {
                Ok(_) => (),
                Err(e) => panic!("{}", e.description()),
            }
        });
        let ((), a) = recv(a)?;
        close(a)
```

^
The 'new' allocates a typed buffer with two channels.

---

# Rusty Variation

[.code-highlight: 2,11]
```rust
        let (b, a) = <Send<(), End> as Session>::new();
        std::thread::spawn(move || {
            let r = (move || -> Result<_, Box<Error>> {
                let b = send((), b)?;
                close(b)
            })();
            match r {
                Ok(_) => (),
                Err(e) => panic!("{}", e.description()),
            }
        });
        let ((), a) = recv(a)?;
        close(a)
```

^
The 'spawn' spawns a new thread.

---

# Rusty Variation

[.code-highlight: 3-6]
```rust
        let (b, a) = <Send<(), End> as Session>::new();
        std::thread::spawn(move || {
            let r = (move || -> Result<_, Box<Error>> {
                let b = send((), b)?;
                close(b)
            })();
            match r {
                Ok(_) => (),
                Err(e) => panic!("{}", e.description()),
            }
        });
        let ((), a) = recv(a)?;
        close(a)
```

^
We evaluate the thread body.

^
It's wrapped in a function which returns 'Result', which allows us to use monadic syntax for errors.

---

# Rusty Variation

[.code-highlight: 7-10]
```rust
        let (b, a) = <Send<(), End> as Session>::new();
        std::thread::spawn(move || {
            let r = (move || -> Result<_, Box<Error>> {
                let b = send((), b)?;
                close(b)
            })();
            match r {
                Ok(_) => (),
                Err(e) => panic!("{}", e.description()),
            }
        });
        let ((), a) = recv(a)?;
        close(a)
```

^
We inspect the result, and crash if we have an error at the top-level. Otherwise, the thread terminates.

---

# Rusty Variation

[.code-highlight: 4,12]
```rust
        let (b, a) = <Send<(), End> as Session>::new();
        std::thread::spawn(move || {
            let r = (move || -> Result<_, Box<Error>> {
                let b = send((), b)?;
                close(b)
            })();
            match r {
                Ok(_) => (),
                Err(e) => panic!("{}", e.description()),
            }
        });
        let ((), a) = recv(a)?;
        close(a)
```

^
Send allocates a new buffer with two channels, writes the value and one channel to the old buffer, and returns the other channel.

^
Receive blocks on an empty buffer, and returns a value plus a new channel once the buffer fills.

---

# [fit] Sounds familiar?

---

# [fit] Let's talk about errors

---

# Exceptional GV

(by Fowler et al.)

Looks like this:

$$
\begin{array}{l}
\mathbf{let} \; s = \mathbf{fork}(\lambda (s : \; !\mathbf{1}.\text{End}).\\
\qquad \mathbf{cancel}(s)\\
)\\
\mathbf{let} \; ((), s) = \mathbf{recv}(s)\\
\mathbf{close}(s)\\
\end{array}
$$

---

# Rusty Variation

(by me)

Looks like this:

```rust
        let s = fork!(move |s: Send<(), End>| {
            cancel(s)
        });
        let ((), s) = recv(s)?;
        close(s)
```

---

# [fit] I know, the fonts are very different

---

# Exceptional GV

Let's see how EGV handles errors!

$$
\bullet
\left(
\qquad
\begin{array}{l}
\mathbf{let} \; s = \mathbf{fork}(\lambda (s : \; !\mathbf{1}.\text{End}).\\
\qquad \mathbf{cancel}(s)\\
)\\
\mathbf{let} \; ((), s) = \mathbf{recv}(s)\\
\mathbf{close}(s)\\
\end{array}
\qquad
\right)
$$

We mark the main thread with a $$\bullet$$
Next we evaluate the fork instruction

---
# Exceptional GV

Let's see how EGV handles errors!

$$
(\nu a)(\nu b)
\left(
\qquad
\begin{array}{lr}
\bullet
\left(
\begin{array}{l}
\mathbf{let} \; s = a\\
\mathbf{let} \; ((), s) = \mathbf{recv}(s)\\
\mathbf{close}(s)\\
\end{array}
\right)
&\|\\
\circ
\left(
\mathbf{cancel}(b)
\right)
&\|\\
a(\epsilon){\leftrightsquigarrow}b(\epsilon)
\end{array}
\qquad
\right)
$$

This forks off the process and allocates a buffer
Next we evaluate the let binding

---

# Exceptional GV

Let's see how EGV handles errors!

$$
(\nu a)(\nu b)
\left(
\qquad
\begin{array}{lr}
\bullet
\left(
\begin{array}{l}
\mathbf{let} \; ((), s) = \mathbf{recv}(a)\\
\mathbf{close}(s)\\
\end{array}
\right)
&\|\\
\circ
\left(
\mathbf{cancel}(b)
\right)
&\|\\
a(\epsilon){\leftrightsquigarrow}b(\epsilon)
\end{array}
\qquad
\right)
$$

The receive instruction blocks on the empty buffer
Next we evaluate the cancel instruction

---

# Exceptional GV

Let's see how EGV handles errors!

$$
(\nu a)(\nu b)
\left(
\qquad
\begin{array}{lr}
\bullet
\left(
\begin{array}{l}
\mathbf{let} \; ((), s) = \mathbf{recv}(a)\\
\mathbf{close}(s)\\
\end{array}
\right)
&\|\\
a(\epsilon){\leftrightsquigarrow}b(\epsilon)
&\|\\
\unicode{x21af} a
\end{array}
\qquad
\right)
$$

This cancels the session and creates a zapper thread
Next we evaluate the receive instruction

---

# Exceptional GV

Let's see how EGV handles errors!

$$
(\nu a)(\nu b)
\left(
\qquad
\begin{array}{lr}
\bullet
\left(
\begin{array}{l}
\mathbf{let} \; ((), s) = \mathbf{raise}\\
\mathbf{close}(s)\\
\end{array}
\right)
&\|\\
a(\epsilon){\leftrightsquigarrow}b(\epsilon)
&\|\\
\unicode{x21af} b
&\|\\
\unicode{x21af} a
\end{array}
\qquad
\right)
$$

Receiving on a channel raises an exception 
if the other endpoint is cancelled

---

# Exceptional GV

Let's see how EGV handles errors!

$$
(\nu a)(\nu b)
\left(
\qquad
\begin{array}{lr}
\bullet
\,
\mathbf{halt}
&\|\\
a(\epsilon){\leftrightsquigarrow}b(\epsilon)
&\|\\
\unicode{x21af} a
&\|\\
\unicode{x21af} b
\end{array}
\qquad
\right)
$$

An uncaught exception turns into halt
Next we garbage collect the buffer

---

# Exceptional GV

Let's see how EGV handles errors!

$$
\bullet
\,
\mathbf{halt}
$$

Fin

---

# Rusty Variation

What about the Rust library?

```rust
        let s = fork!(move |s: Send<(), End>| {
            cancel(s)
        });
        let ((), s) = recv(s)?;
        close(s)
```

---

# Rusty Variation

For that, let's look at how `cancel` is implemented:
<br />

```rust
    fn cancel<T>(x: T) -> Result<(), Box<Error>> { 
        Ok(()) 
    }
```

<br />
Wait, what happened to `x`? 

It went out of scope!

^
This is the definition of cancel.

---

# Rusty Variation

What happens when a channel `x` leaves scope unused?
<br />

- destructor is called
- values in buffer are deallocated
- destructors for values in buffer are called
- buffer is marked as `DISCONNECTED`
- calling `recv` on `DISCONNECTED` buffer returns `Err`

---

# [fit] Sounds familiar?

---

# What are the differences?

- **try/catch** vs. **error monad**

  (using the "$$\mathbf{try} \, L \, \mathbf{as} \, x \, \mathbf{in} \, N \, \mathbf{otherwise} \, M$$" instruction)

- **explicit close** vs. **implicit close** 

```rust
  fn close(s: End) -> Result<(), Box<Error>> {
      Ok(()) // `End` doesn't have a buffer
  }
```

- **explicit cancellation** vs. **implicit cancellation**

  (what happens if we forget to complete a session?)

---

# What are the differences?

- **simply-typed linear lambda calculus** vs. **Rust**

  <br />
  <br />
  <br />
  
  this means we have:
  
  - **no recursion** vs. **general recursion**
  
  - **lock freedom** vs. **deadlock freedom**
  
  - etc.

---

# How can we get deadlocks in Rusty Variation?

- by using `mem::forget`

```rust
    let s = fork!(move |s: Send<(), End>| {
        mem::forget(s);
        Ok(())
    });
    let ((), s) = recv(s)?;
    close(s)
```

- by storing channels in manually managed memory 
  and not cleaning up

---

# What are the similarities?

- in theory, everything else?

- can we prove it?

  <br />
  <br />

  > doesn't Rust have formal semantics? 
  > I heard so much about RustBelt!
  
  no.

  RustBelt formalises elaborated Rust and
  doesn't support many features we depend on.

---

# What are the similarities?

- in theory, everything else?

- can we prove it? no.

- can we test it?

```rust
    #[test]
    fn ping_works() {
        assert!(|| -> Result<(), Box<Error>> {

            // ...insert example here...

        }().is_ok()); // it actually is!
    }
```

---

# What are the similarities?

- in theory, everything else?

- can we prove it? no.

- can we test it? yes.

- can we **properly** test it?

---

# Testing Rusty Variation 

Plan:

`(x)` use Feat/Neat[^1] to generate EGV terms 
`( )` run terms in EGV
`( )` run terms in Rust
`( )` test if they behave the same

[^1]: Generating constrained random data with uniform distribution, Claessen, Duregård, & Pałka, 2015

---

# How efficient is Rusty Variation?

- buffers are either empty or non-empty

- size of buffers is statically known

  (unless you're sending boxed references)

- each buffer only involves a single allocation

- size of session is statically known

  (but buffers are allocated lazily)

- **it's really quite efficient y'all**

---

# [fit] Related work

---

# `session-types`

(by Laumann et al.)

- library for session types in Rust

- dibsed the best package name

- embeds LAST[^2] in Rust 

  (a linear language embedded in an affine one)

- **forget to complete a session? segfault!**

[^2]: Linear  type  theory  for asynchronous session types, Gay & Vasconcelos, 2010

---

# [fit] Conclusions

---

![right](rv.svg)

# Rusty Variation

- embeds EGV into Rust

- is unit tested

- will be QuickChecked

- is very efficient

- improves `session-types`
