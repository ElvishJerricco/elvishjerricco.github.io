---
layout: post
title:  "Applicative Effects in Free Monads"
date:   2016-04-08
categories:
---
Recently, I've been fascinated by the
[Freer monad](http://okmij.org/ftp/Haskell/extensible/more.pdf),
and free monads in general.
While free monads are definitely interesting,
they've got several practical deficiencies.
On such deficiency is the lack of applicative interpretation.
In existing free monads,
each effect hides the next effect, even if it should be statically available.
This essentially means that `(<*>) = ap` in all cases.
While this is fine most of the time,
it can definitely be a problem for some effects.

[Haxl](https://hackage.haskell.org/package/haxl), developed at FaceBook,
is basically a free monad over a union of request types.
The unique thing about Haxl is that it uses `(<*>)` to do requests in parallel.
Essentially, any requests statically available in the arguments to `(<*>)`
will be requested concurrently.
Other free monads as of yet don't have that capability.
Instead, every effect, whether bound with `>>=` or applied with `<*>`,
will hide the next one from analysis.

It'd be nice to bring this ability into a general free monad.
Let's start with the definition of the `Free` monad.

```haskell
data Free f where
  Val :: a -> Free f a
  Free :: f (Free f a) -> Free f a

instance Functor f => Functor (Free f) where
  fmap f (Val a) = Val (f a)
  fmap f (Free a) = Free (fmap (fmap f) a)

instance Functor f => Applicative (Free f) where
  pure = Val
  Val f <*> a = fmap f a
  Free f <*> a = Free $ fmap (<*> a) f

instance Functor f => Monad (Free f) where
  Val a >>= k = k a
  Free a >>= k = Free $ fmap (>>= k) a
```

This is the implementation found in the
[`free`](https://hackage.haskell.org/package/free) package.
I won't go into the details, but it can be proven that `Free` follows the
Functor / Applicative / Monad laws.
The idea behind this implementation is that `Free f` is applied to `f`,
and mapping this `f` will recursively modify `Free` until it reaches a `Val`.

The problem, you may notice, is that `f` needs to be a Functor.
If the goal is a monad instance for any `f`,
restricting `f` to a Functor doesn't make much sense.
The `Freer` monad solves this by applying a left Kan extension,
which is essentially a free Functor, to `Free`.

```haskell
data Lan f a where
  Lan :: (a -> b) -> f a -> Lan f b
instance Functor (Lan f) where
  fmap f (Lan g b) = Lan (f . g) b

type Freer f = Free (Lan f)
-- simplifies to
data Freer f a where
  Val :: a -> Free f a
  Freer :: f a -> (a -> Freer f b) -> Freer f b
```

This `Freer` type will then behave as a monad over any `f` at all,
regardless of whether or not it's a Functor.

Getting back on track,
let's take a look at the Applicative instance for `Freer`.

```haskell
instance Applicative (Freer f) where
  pure = Val
  Val f <*> a = fmap f a
  Freer b k <*> a = Freer b ((<*> a) . k)
```

The apparent problem is that there's no way to interpret more than one instance
of `f` at a time when they're composed applicatively.
For something like Haxl, this a deal breaker.
So let's go (almost) back to the drawing board and start with `Free` again.
Is there any free functor we can apply that would enable
applicative interpretation?

[The free applicative](https://hackage.haskell.org/package/free-4.12.4/docs/Control-Applicative-Free.html)
seems good.

```haskell
data Ap f a where
  Pure :: a -> Ap f a
  (:<*>) :: Ap f (a -> b) -> f a -> Ap f b
instance Functor (Ap f ) where
  fmap f (Pure a) = Pure (f a)
  fmap f (xs :<*> x) = fmap (f .) xs :<*> x
instance Applicative (Ap f ) where
  pure = Pure
  Pure f <*> y = fmap f y
  (xs :<*> x) <*> y = (fmap flip xs <*> y) :<*> x

type Freer f = Free (Ap f)
-- simplifies to
data Freer f a where
  Val :: a -> Freer f a
  Freer :: Ap f (Freer f a) -> Freer f a
```

We can't simplify `Ap` out of `Freer` entirely, unlike `Lan`,
because of `Ap`'s recursive nature.
But the problem seems to be solved. Check out the new instances,
particularly the Applicative instance.

```haskell
instance Functor (Freer f) where
  fmap f (Val a) = Val (f a)
  fmap f (Freer a) = Freer (fmap (fmap f) a)

instance Applicative (Freer f) where
  pure = Val
  Val f <*> a = fmap f a
  Freer f <*> Val a = Freer $ fmap (fmap ($ a)) f
  Freer f <*> Freer a = Freer (fmap (<*>) f <*> a)

instance Monad (Freer f) where
  Val a >>= k = k a
  Freer a >>= k = Freer (fmap (>>= k) a)
```

As you can see, this version of `Freer` has its `(<*>)` specialized
such that applicative effects are properly chained into one `Ap` instance.
And in fact, applying Haxl to this encoding of `Freer` does parallelize.

```haskell
liftFreer :: f a -> Freer f a
liftFreer = Freer . (pure Val :<*>)

runAp :: Applicative f => Ap f a -> f a
runAp (Pure a) = pure a
runAp (f :<*> a) = runAp f <*> a

runM :: Monad m => Freer m a -> m a
runM (Val a) = return a
runM (Freer a) = runAp a >>= runM

haxl3 = runM (liftFreer haxl1 <*> liftFreer haxl2)
io1 = runHaxl env haxl3
```

You'll find that running `io1` runs `haxl1` and `haxl2` concurrently.
This is a demonstrable benefit of this encoding.

The issue, as with most free monads, is performance.
This encoding is essentially a linked list of applicative effects,
resulting in the next monadic effect.
`fmap` alone is *O(n)*, where *n* is the number of applicative effects,
and `(<*>)` is *O(n^2 + m)*.
I'm not sure what steps could be taken to improve performance, but
a [faster type-aligned list](https://hackage.haskell.org/package/type-aligned)
might be a good place to start.

Anyway, that's all I have so far. Let me know if you have any thoughts.

**UPDATE:** Edward Kmett [pointed out](https://www.reddit.com/r/haskell/comments/4e0de9/applicative_effects_in_free_monads/d1wxm3w)
that a better way to think of this is as a variation on `Free` that takes
an Applicative rather than a Functor.

> Anything free is relative to what you forget: Free -\| Forget
>
> The usual free monad is relative to a forgetful functor that takes any monad
`f` and remembers just that `f` is a functor, and which maps monad homomorphisms
to mere natural transformations.
>
> Similarly `Ap` is a free applicative relative to a forgetful functor that
takes an applicative down to its underlying functor, and
operational is a free monad relative to a forgetful functor that takes a monad
all the way down to a type constructor of kind `* -> *`, not even a functor.
>
> On the other hand, you can easily define a 'free' construction that forgets
that `f` is a monad and remembers that `f` is an applicative,
then the free construction can know about the applicative for `f` and dispatch
`(<*>)` through it accordingly.
This is a bit tricky because unlike with `Functor`,
the compatibility between `(<*>)` and `(>>=)` for the monad is less trivial,
but **this** `Free` is the one you want to build such a `Free (Ap f)` on top of.

The point is that if you factor `Ap` out of the `Freer` definition above,
you end up with a `Free` monad that works with Applicatives instead of Functors.
This is a useful distinction to make because this variant of `Free` can be
applied to any Applicative, which would remove the need for `Ap` in many cases.

```haskell
data Free f a where
  Val :: a -> Free f a
  Free :: f (Free f a) -> Free f a

instance Functor f => Functor (Free f) where
  fmap f (Val a) = Val (f a)
  fmap f (Free a) = Free (fmap (fmap f) a)

instance Applicative f => Applicative (Free f) where
  pure = Val
  Val f <*> a = fmap f a
  Free f <*> Val a = Free $ fmap (fmap ($ a)) f
  Free f <*> Free a = Free (fmap (<*>) f <*> a)

instance Applicative f => Monad (Free f) where
  Val a >>= k = k a
  Free a >>= k = Free (fmap (>>= k) a)
```

Notice how the only difference between this and the original `Free` definition
is that the `(<*>)` method is optimized for an Applicative `f`.
Once applied with `Ap f`, this `Free` becomes the `Freer` defined above.
But without `Ap`, this is still a valid `Free` monad,
albeit a less powerful one.
