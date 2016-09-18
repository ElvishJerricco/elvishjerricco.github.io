---
layout: post
title:  "Abstracting Async.Concurrently"
date:   2016-09-17
categories:
---

Applicatives have an interesting relationship to concurrency.
[`fraxl` is my attempt](https://github.com/ElvishJerricco/fraxl)
to abuse this relationship with free monads to get something
more abstract [than `haxl`](https://github.com/facebook/Haxl).
By using a special `Applicative` instance,
`fraxl` allows computations to be implicitly concurrent.

[The `async` package](https://hackage.haskell.org/package/async) has something
[similar in `Control.Concurrent.Async.Concurrently`](https://hackage.haskell.org/package/async-2.1.0/docs/Control-Concurrent-Async.html#t:Concurrently).
The `Concurrently` applicative is a newtype around `IO`.
But its `Applicative` instance parallelizes the `IO` actions.

```haskell
{-# LANGUAGE DeriveFunctor #-}

newtype Concurrently a = Concurrently { runConcurrently :: IO a }
  deriving Functor

instance Applicative Concurrently where
  pure = Concurrently . return
  Concurrently fs <*> Concurrently as =
    Concurrently $ (\(f, a) -> f a) <$> concurrently fs as
```

Because of the monad law `(<*>) = ap`, `Concurrently` cannot be a monad.
Any monadic binding will not be executed concurrently,
forcing `ap` to behave differently than `(<*>)`.
Despite this, you can convert between `Concurrently` and `IO` painlessly
by packing and unpacking the `Concurrently` newtype.

But both `fraxl` and `haxl` fall prey to the same issue.
They have to knowingly break this monad law to parallelize computations.
However, being free monads,
they hide this behind interpreters that make the law hard to observe.
So morally, the law is not being broken.
The free monad accumulates the computation,
and the interpreter runs it while promising not to break any laws.

This solution leaves me uneasy.
Cherry picking when the law doesn't matter feels half-baked.
This is why I like `Concurrently` so much.
It allows seamless conversion between concurrent and sequential code,
without being lenient on the laws.
Abstracting `Concurrently`'s design to other monads would be great.
In essence, `Concurrently` is an applicative tied to the monad `IO`,
with conversions between the two to allow you to use either.
As a class, this would look something like this.

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

import qualified Control.Concurrent.Async as Async

class (Applicative f, Monad m) => Concurrently f m | f -> m, m -> f where
  runConcurrently :: f a -> m a
  runSequentially :: m a -> f a

instance Concurrently Async.Concurrently IO where
  runConcurrently = Async.runConcurrently
  runSequentially = Async.Concurrently
```

One of the key features of `Async.Concurrently`
is that switching between it and `IO` preserves concurrency.
That is,
`runSequentially (runConcurrently x) <*> runSequentially (runConcurrently y)`
is just as concurrent as `x <*> y`.
This suggests the laws:

- `runSequentially . runConcurrently = id`
- `runConcurrently . runSequentially = id`

`Async.Concurrently` abides by this quite trivially,
since it's just a `newtype` around `IO` with a different `Applicative` instance.
Luckily, something like `fraxl` can use a similar trick.
`fraxl` uses a special free monad that abuses an underlying `Applicative`,
and consequently "breaks" the monad law.
However, using `Async.Concurrently`'s trick,
`fraxl` could have a `newtype` for concurrency,
and then use an ordinary free monad for the sequential monad,
instead of its special one.

Recall what `fraxl` looks like with the pseudo-law-breakage.

```haskell
-- Old

data Ap f a = ...
-- Concurrent, law-breaking `Applicative` instance
data Free f a = ...
type Fraxl f = Free (Ap f)

getSite :: String -> Fraxl MyDataSource Site
getSite = ...
printSite :: Site -> Fraxl MyDataSource ()
printSite = ...

example :: Fraxl MyDataSource IO ()
example = do
  --                               Concurrent
  (a,b) <- (,) <$> getSite "site1.com" <*> getSite "site2.com"
  printSite a
  printSite b
```

In this version, we have a free monad with a weird `Applicative` instance
that "breaks" the monad laws.
But we get concurrency in `do` blocks for no extra charge.
Using the `newtype` trick,
we can move the weird `Applicative` instance to a type with no `Monad` instance,
at the cost of slightly more verbose `do` code.

```haskell
-- New

data Ap f a = ...
-- Sequential `Applicative` instance
data Free f a = ...
type Fraxl f = Free (Ap f)

-- Concurrent, law-abiding `Applicative` instance
-- No `Monad` instance
newtype Free' f a = Free' (Free f a)
type Fraxl' f = Concurrently (Ap f)

instance Concurrently (Free' f) (Free f) where
  runConcurrently (Free' a) = a
  runSequentially = Free'

getSite :: String -> Fraxl MyDataSource Site
getSite = ...
printSite :: Site -> Fraxl MyDataSource ()
printSite = ...

getSite' :: String -> Fraxl' MyDataSource Site
getSite' = runSequentially . getSite

example :: Fraxl MyDataSource ()
example = do
  (a,b) <- runConcurrently $ --       Concurrent
             (,) <$> getSite' "site1.com" <*> getSite' "site2.com"
  printSite a
  printSite b
```

Works wonderfully!
We've encoded the same structure into `example`,
without having to subvert the monad laws.

There are probably some other interesting instances of `Concurrently`.
I don't know what they are,
but the pattern of having special applicatives isn't new.
Whatever the instances are, there are some cool consequences.
Traversal, for instance, can become implicitly concurrent.

```haskell
traverse' :: (Concurrently f m, Traversable t) => (a -> m b) -> t a -> m (t b)
traverse' f = runConcurrently . traverse (runSequentially . f)

for' :: (Concurrently f m, Traversable t) => t a -> (a -> m b) -> m (t b)
for' = flip traverse'

example = do
  ys <- for' xs $ \x -> do
    z <- f x
    y <- g z
    return y
  ...
```

You can write monadic code in `for'`,
and have each computation implicitly done in parallel.
This largely accomplishes one of `haxl`'s [motivating examples](https://github.com/facebook/Haxl/tree/master/example/sql).

---

There are some problems though.
The need for `runConcurrently` would likely be controversial.
On one hand, it does make it slightly more verbose,
and will mean you often duplicate combinators as I did with `traverse'`.
On the other hand,
it forces the concurrency of a computation to be documented at the type level.
We know by looking at any expression (or its type) if
it's going to do things concurrently.
This could save some headaches in debugging.

The biggest downside is losing implicit concurrency with `ApplicativeDo`.
With `ApplicativeDo` enabled on the law-breaking version of `fraxl`,
writing `example` like this yields some nice desugared code.

```haskell
{-# LANGUAGE ApplicativeDo #-}

example :: Fraxl MyDataSource ()
example = do
  a <- getSite "site1.com"
  b <- getSite "site2.com"
  printSite a
  printSite b

-- Desugars to:

example :: Fraxl MyDataSource ()
example = join $ (\a b -> printSite a *> printSite b)
              <$> getSite "site1.com"
              <*> getSite "site2.com"
```

With the `Concurrently` version,
the sugared `do` code might look a little worse,
again depending on whether you prefer the concurrency to be explicit.

```haskell
{-# LANGUAGE ApplicativeDo #-}

example :: Fraxl MyDataSource ()
example = do
  (a, b) <- runConcurrently $ do
    a <- getSite' "site1.com"
    b <- getSite' "site2.com"
    return (a, b)
  printSite a
  printSite b

-- Desugars to:

example :: Fraxl MyDataSource ()
example = runConcurrently
  ((,) <$> getSite' "site1.com" <*> getSite' "site2.com")
  >>= \(a, b) -> printSite a *> printSite b
```

Functionally, it's the same result.
The `do` block just had to be much more explicit about its concurrency.
It's unclear to me if this tradeoff is a good or bad one.

#### Pros:

- Law-abiding with no tricks.
- Explicit concurrency can make code easier to reason about.
- Lets `fraxl` use an existing free monad instead of a custom one.

#### Cons

- Explicit concurrency is more verbose.
- Do notation gets in the way instead of being clearer.

I'm leaning towards calling it good,
because I'm often a fan of being explicit about dirty tricks.
But I can definitely see why someone wouldn't like this approach.
