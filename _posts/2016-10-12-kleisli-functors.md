---
layout: post
title:  "Kleisli Functors"
date:   2016-10-12
categories:
---

**Abstract:** I define a typeclass for functors from Kleisli categories to Hask.
This class turns out having more interesting properties than I expected,
encompassing various Haskell patterns such as concurrency,
and monad transformers.

Monads are often described in terms of `do` notation.
Every bind `(=<<)` equates to one statement in the `do` block.
This is sufficient for explaining the usage of monads to newcomers.
But in terms of understanding the functionality of monads,
I think it somewhat misses the mark.
I like to think about monads in terms of composition.
Monads give you a special kind of composition called **"Kleisli composition"**,
written as the `(<=<)` operator.

```haskell
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(g <=< f) a = f a >>= g
```

This operator takes two functions of the form `a -> m b`,
and composes them monadically, passing the input to the first function,
then binding the output to g.
This form of function (`a -> m b`) is called a **"Kleisli arrow"**.
Compare the type signature of this to ordinary function composition.

```haskell
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
(.)   ::            (b ->   c) -> (a ->   b) -> (a ->   c)
```

They're doing very similar jobs,
except that `(<=<)` allows functions to perform monadic effects.
Just as function composition is core to producing pure programs,
Kleisli composition is core to producing monadic programs.

In fact, we have some nice category theory to back this up.
I'm not going to give a crash course of category theory here,
but I'm only going to use some very basic categories.
The only prerequisite knowledge for this post is knowing what a category is,
how the category of sets works (more accurately, the category Hask),
and how functors work.

As we've just seen, Kleisli arrows compose.
This composition forms a proper category,
called a (you guessed it) **"Kleisli category"**.
There is one Kleisli category for every monad instance,
which I will denote as `Kleisli m` (given a monad `m`).
The objects in this category are the same as the objects in Hask:
types in Haskell.
But the morphisms are Kleisli arrows.
So an arrow in `Kleisli m` that points from `A` to `B`
is a function of type `A -> m B`.
Composition in `Kleisli m` is the Kleisli composition operator `(<=<)`,
and the identity morphism (which has type `a -> m a`) is `return`.
The laws are held trivially, so I won't go into a proof here.

This category is useful on its own.
Composing Kleisli arrows represents creating monadic programs.
There's [a `Category` instance](https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Arrow.html#t:Kleisli)
in `base` for it that's pretty useful.
But Kleisli arrows don't give you anything that you can't get from the monad.
So they're really only a notational feature.
They don't let you do anything you couldn't do without them.

---

Kleisli Functors
---

After [experimenting with the `Data.Align` class](https://github.com/isomorphism/these/pull/66),
I realized a common pattern in the way we use monads.
We will often form functors from `Kleisli m` to Hask.
These aren't traditional Haskell functors.
These map Kleisli arrows to Hask arrows.

```haskell
class Monad m => KleisliFunctor m f where
  kmap :: (a -> m b) -> f a -> f b
```

**Terminology:** I will use the phrasing *"`f` is a Kleisli functor of `m`"*
to explain that there is an instance of `KleisliFunctor m f`.

Before I get into the use-cases, it's important to go over the laws,
which are satisfyingly simple.
They're just the laws of functors between categories.

```haskell
kmap return = id
kmap g . kmap f = kmap (g <=< f)
```

Mapping the identity arrow of `Kleisli m` results in the identity arrow of Hask.
And composing two mappings is equivalent to mapping a Kleisli composition.
This actually means `Functor` is a superclass of `f` in `KleisliFunctor`,
which I'll explain later.

```haskell
class (Monad m, Functor f) => KleisliFunctor m f where
  kmap :: (a -> m b) -> f a -> f b
```

Conceptually, this class means you can map Kleisli arrows over `f`,
and `f` will happily absorb the monad `m` into itself.
Somehow, `f` preserves the powers of `m`,
and hides them behind its own interface.
This admits one extremely obvious instance.
A monad `m` should present a good candidate for a Kleisli functor of itself,
since it trivially preserves its own powers behind its own interface.

```haskell
instance Monad m => KleisliFunctor m m where
  kmap f a = a >>= f
```

Any monad can map its own Kleisli arrows easily enough.
It's the same as binding.
This probably shouldn't be surprising.
The Kleisli category wouldn't be very useful if we couldn't use it from Hask,
which is exactly what this represents.

---

Kleisli Maybe
---

Another obvious example is Kleisli functors of `Maybe`.

```haskell
kmap :: (a -> Maybe b) -> f a -> f b
```

[Look familiar?](https://hackage.haskell.org/package/containers-0.5.8.1/docs/Data-Map-Lazy.html#v:mapMaybe)
It's pretty common to filter elements by mapping them to `Maybe`.
The `reflex` FRP library [even has a class for Kleisli functors of `Maybe`](https://hackage.haskell.org/package/reflex-0.4.0/docs/Reflex-Class.html#t:FunctorMaybe)
(though it's only there for domain-specific reason,
despite being a very general tool).
You could probably even make parser combinator monads into this kind of functor.

---

Monad Transformers
---

I also noticed that when the functor supports `pure :: a -> f a`,
it seems to generalize transformers.
Or at least, it can implement a `lift` operation for lifting `m` into `f`.

```haskell
lift :: (KleisliFunctor m f, Applicative f) => m a -> f a
lift = kmap id . pure
```

I'm not sure what implications this has on the laws.
It seems like it means that if `f` is a `Monad` (or maybe just `Applicative`),
it should support laws similar to the `MonadTrans` laws.
Here's an attempt at a law, but I don't have much confidence in it.
**Update:** I fixed a type error in the laws I had and reduced them to one.
Still not all that confident in this being correct though.

```haskell
kmap f (pure a) = kmap id (pure (f a))
```

Any given `MonadTrans` instance forms a `KleisliFunctor` instance,
supporting the theory that Kleisli functors are a more useful abstraction.

```haskell
instance (MonadTrans t, Monad m, Monad (t m)) => KleisliFunctor m (t m) where
  kmap f a = a >>= (lift . f)
```

---

Concurrency
---

I [recently wrote about an abstraction over `Async.Concurrently`](http://elvishjerricco.github.io/2016/09/17/abstracting-async-concurrently.html).
In the [accompanying reddit thread](https://www.reddit.com/r/haskell/comments/53a0kq/abstracting_asyncconcurrently/),
some people pointed out that concurrency probably wasn't the right abstraction.
The idea was that some monads pair with a similar applicative functor
to achieve some interesting extra behavior.
My example was [`Concurrently` from the `async` package](https://hackage.haskell.org/package/async-2.1.0/docs/Control-Concurrent-Async.html#t:Concurrently).

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

With it, I could define some useful functions for concurrent traversal.

```haskell
traverse' :: (Concurrently f m, Traversable t) => (a -> m b) -> t a -> m (t b)
traverse' f = runConcurrently . traverse (runSequentially . f)
```

As it turns out, `KleisliFunctor` admits half of the `Concurrently` class.
The `lift` function I defined before is the `runSequentially` function.
And `traverse'` ends up returning `f` instead of `m`,
which is perhaps more natural, since it means no fundep is needed,
and the type signature visibly uses both parts of the abstraction.

```haskell
traverse' :: (Traversable t, KleisliFunctor m f, Applicative f)
          => (a -> m b) -> t a -> f (t b)
traverse' f = traverse (kmap f . pure)

for' :: (Traversable t, KleisliFunctor m f, Applicative f)
     => t a -> (a -> m b) -> f (t b)
for' = flip traverse'

instance KleisliFunctor IO Async.Concurrently where
  kmap f a = Async.Concurrently (Async.runConcurrently a >>= f)

x :: IO ()
x = Async.runConcurrently $ for' [1..100] $ \n -> do
  -- I can perform arbitrary IO.
  -- Each iteration of this loop will be running concurrently.
  print n
```

We get to write monadic code,
and have it execute according to the optimizations of the Kleisli functor.

One of the examples from the reddit thread was the validation applicative.

```haskell
data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Ord, Show, Functor)

instance Semigroup e => Applicative (Validation e) where
  pure = Success
  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
  Failure e1 <*> Success _  = Failure e1
  Success _  <*> Failure e2 = Failure e2
  Success f  <*> Success a  = Success (f a)
```

`Validation` is equivalent to `Either`,
except that its applicative instance gives you more information.
It would break the `(<*>) = ap` law, so it can't be a monad.
But it still would be nice to perform `Either`-style monadic effects
until we want to switch to `Validation`.
This is a pretty good candidate for `KleisliFunctor`.

```haskell
instance KleisliFunctor (Either e) (Validation e) where
  kmap f (Success a) = either Failure Success $ f a
  kmap _ (Failure e) = Failure e

f :: [Int] -> Validation [String] Int
f ns = sum $ for' ns $ \n -> do
  -- Any errors thrown with `Left` will short circuit this computation.
  -- But all the other iterations of the loop will also yield their errors.
  ...
```

---

Misc. Properties
---

The `KleisliFunctor` class comes with some pretty novel properties,
which I'd like to briefly mention.

- If you give it `Identity` as the monad, you recover the `Functor` class.
  This is because `Kleisli Identity` is equivalent to Hask,
  so a functor between the two is still an endofunctor of Hask.

```haskell
{-# LANGUAGE ConstraintKinds #-}
type Functor' = KleisliFunctor Identity
```

- If you give it `Const a` as the functor,
  you get a Kleisli functor of any monad.
  Of course it's a relatively useless Kleisli functor.

```haskell
instance Monad m => KleisliFunctor m (Const a) where
  kmap _ (Const a) = Const a
```

- And finally, as I mentioned earlier,
  all Kleisli functors make regular Hask functors,
  suggesting that `Functor` should be a superclass of `KleisliFunctor`.

```haskell
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
fmapDefault :: forall m f a b. KleisliFunctor m f => (a -> b) -> f a -> f b
fmapDefault f = kmap ((return @ m) . f)
```

---

The Dual of Kleisli Functors
---

I mentioned that `KleisliFunctor` gives us *one* half of
the `Concurrently` class I wrote about in the past.
In an attempt to find the other half,
I decided to look for duals to `KleisliFunctor`.
If a Kleisli functor is a functor from `Kleisli m` to Hask,
then the dual is a functor from Hask to `Kleisli m`.
So we get an obvious `CoKleisliFunctor` class to describe this.

```haskell
class Monad m => CoKleisliFunctor m g where
  -- | Laws:
  --   cokmap id = return
  --   cokmap (g . f) = cokmap g <=< cokmap f
  cokmap :: (a -> b) -> g a -> m (g b)
```

Unfortunately, this class seems much less useful.
It's hard to imagine any usage of `m` that isn't clearly equivalent to `return`.
The one example I can think of is using `m = IO` to process the pure
function on a collection `g` of elements `a` concurrently.
But even this isn't promising.

Now, when you have two functors going opposite directions,
you'll often find they form an adjunction.
I won't go into what that means mathematically,
but the implications will be perhaps more obvious with some code.

```haskell
class (KleisliFunctor m f, CoKleisliFunctor m g)
    => KleisliAdjunction m f g
    | f g -> m where
  unit :: a -> f (g a)
  counit :: g (f a) -> m a
```

Basically, for two functors of opposite direction, they are adjoint if
we can construct them in one category, and destroy them in the other.
"Destroying" things in a Kleisli category is nice.
It lets our return type use `m`,
meaning "destroying" is more like "converting" to `m`'s effect system.
This `counit` might represent the other half of my `Concurrently` class.
It seems like you can use it to turn `f`s back into `m`s.
I'm just having trouble finding any instances of `g` to do this with.

The crazy thing about adjunctions is that when you have one,
you're given a new monad for free.

```haskell
newtype KleisliMonad m f g a = KleisliMonad { runKleisliMonad :: f (g a) }

instance (KleisliFunctor m f, CoKleisliFunctor m g)
    => Functor (KleisliMonad m f g) where
  fmap f (KleisliMonad a) = KleisliMonad $ kmap @m (cokmap f) a

instance KleisliAdjunction m f g => Applicative (KleisliMonad m f g) where
  pure = return
  (<*>) = ap

instance KleisliAdjunction m f g => Monad (KleisliMonad m f g) where
  return = KleisliMonad . unit
  KleisliMonad a >>= k = KleisliMonad $
    kmap (counit <=< cokmap (runKleisliMonad . k)) a
```

I don't actually have any idea what this monad does.
But that's largely because I can't think of any instances of this adjunction.
Please let me know if you can think of one;
this monad could provide wildly interesting.
Of course, it could also prove to be no more useful than `m` alone,
which would be disappointing, but ultimately unsurprising.

---

Conclusion
---

To summarize:

- Kleisli functors encompass a lot of patterns.
  - Removing elements from a structure is `KleisliFunctor Maybe`.
  - Executing sequential programs concurrently
    is `KleisliFunctor IO Concurrently`.
  - Collecting errors from many computations is
    `KleisliFunctor Either Validation`.
  - There's a reasonable chance that monad transformers are just
    `KleisliFunctor m (t m)`.
- The dual of Kleisli functors is confusing.
  - If there's a useful instance of it, it could yield a pretty wild monad.

I think I've opened about as many questions as I've answered,
so I look forward to hearing people's thoughts on this!
