---
layout: post
title:  "More on Applicative Effects in Free Monads"
date:   2016-04-13
categories:
---
[In my last post](http://elvishjerricco.github.io/2016/04/08/applicative-effects-in-free-monads.html),
I explored the problem of Applicative effects in free monads.
[After some great discussion](https://www.reddit.com/r/haskell/comments/4e0de9/applicative_effects_in_free_monads/)
between Edward Kmett, Dave Menendez, /u/MitchellSalad, and myself,
I think I've come to a new understanding.

The core of the issue is that under my implementation,
`(<*>)` was purposefully different than `ap`.
Turns out, it's a law that `(<*>)` must be morally equivalent to `ap`.
So if the two turn out effectively different, that's illegal.

Dave Menendez gave a good counter example to the original post using `Const`.

```haskell
instance Monoid m => Applicative (Const m) where
  pure _ = mempty
  Const f <*> Const v = Const (f `mappend` v)

liftFree (Const "a") <*>  liftFree (Const "b") = Free (Const "ab")
liftFree (Const "a") `ap` liftFree (Const "b") = Free (Const "a")
```

As you can see, `(<*>)` gives a fundamentally different answer than `ap`.

### Cheating on the Ap Test

The truth is, `(<*>)` doesn't have to literally be assigned `ap`.
`(<*>)` just has to be *morally* equivalent to `ap`.
In the `Const` example, morally different results were computed.
But in my original example with Haxl, the results were legal.
The difference is that with `Const`,
`(<*>)` is morally different than anything `ap` could ever do.
It basically shouldn't be legal to interpret `Free (Const m)`.

According to Edward Kmett, it's all up to interpretation!
The solution is to provide a limited view into `Free`.
If the interpreter of the free monad agrees to certain laws,
then interpretation can be guaranteed valid.
Essentially, we're shifting the burden of proof for `(<*>) = ap`
to the interpreter.

Let's recall the Applicative-optimized instance of Applicative for Free.

```haskell
instance Applicative f => Applicative (Free f) where
  pure = Pure
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> Pure b = Free $ fmap ($ b) <$> ma
  Free ma <*> Free mb = Free $ fmap (<*>) ma <*> mb -- <- The important line
```

In that last line, `(<*>)` is used on the `f` Applicative.
We're basically asking `f` to perform this computation.
So we know that as long as `f` is being responsible with `(<*>)`,
we will get a correct applicative computation.
The question then, is how to use this to make a correct monadic computation?

For a simple example, let's turn to [`retract`](https://hackage.haskell.org/package/free-4.12.4/docs/Control-Monad-Free.html#v:retract).
This function doesn't actually need any porting to work with this `Free`.

```haskell
retract :: Monad f => Free f a -> f a
retract (Pure a) = return a
retract (Free as) = as >>= retract
```

If `(<*>) = ap` in `f`, which it has to, then all the work is done for us.
There's no way to construct a computation that can misuse `(<*>)` and `ap` to
break `f`'s monad laws.

But what about examples where `f` isn't a monad?
The simplest way to use these `f`s is with [`foldFree`](https://hackage.haskell.org/package/free-4.12.4/docs/Control-Monad-Free.html#v:foldFree).

```haskell
foldFree :: (Functor f, Monad m) => (forall x . f x -> m x) -> Free f a -> m a
foldFree _ (Pure a)  = return a
foldFree f (Free as) = f as >>= foldFree f
```

In the conventional `Free`, the first parameter is a natural transformation.
Given any `f`, create a corresponding `m`,
and `foldFree` will handle the monadic bits.
But this interpreter won't work with the Applicative-optimized `Free`.
Well, it will compile and run just fine.
It just won't produce law-abiding results.
Given that `f` is actually an Applicative,
and `(<*>)` was used in the computation, it's possible that the natural
transformation doesn't correctly transcribe applicative effects.
If this is the case, we don't know for sure that the `m`s returned are correct,
so we don't know that the result is equivalent to using `ap`.

That natural transformation has to be something stronger.
It has to preserve the meaning of applicative computations between `f` and `m`.
So it needs to be an applicative homomorphism.
That is, it needs to be a natural transformation that obeys these laws.

```haskell
hm :: (Applicative f, Applicative m) => f x -> m x
hm (pure a) = pure a
hm (f <*> g) = hm f <*> hm g
```

In code, the only change we can make is strengthening that `Functor` constraint.

```haskell
foldFree :: (Applicative f, Monad m) => (forall x . f x -> m x) -> Free f a -> m a
foldFree _ (Pure a)  = return a
foldFree f (Free as) = f as >>= foldFree f
```

We just have to take for granted that the user is supplying
a natural transformation that obeys those laws.
If it does,
then we know that the meanings of applicative computations are being preserved.
Then, as with `retract`, the rest is properly guaranteed by the monad laws.

### Back to Freer

So what about the `Freer` monad?
Considering, once again, that we have a [free applicative](https://hackage.haskell.org/package/free-4.12.4/docs/Control-Applicative-Free.html),
which takes any type of kind `* -> *`,
we can get the freer monad the same way as before.

```haskell
type Freer f = Free (Ap f)
```

Now the interpreter's job is to take `Ap`,
which is effectively a linked list of `f`s,
and use those `f`s to make an applicative homomorphism.
Now, we have a freer monad that turns any `* -> *` into a monad,
and allows the interpreter to optimize applicative effects.

It's still extremely slow.
I plan to look into church encoding it to improve the performance.
But I'm not sure if that will work well with `(<*>)`.
Anyway, I'm very happy just knowing a solution for this exists.
Let me know if I've made any other mistakes.
