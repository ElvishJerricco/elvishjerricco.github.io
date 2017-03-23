---
layout: post
title:  "Applicative Sorting"
date:   2017-03-23
categories:
---

Continuing my unending train of thoughts on static analysis of
effects, in this post I'm going to talk about using `Applicative` to
sort any collection. The `Traversable` typeclass is one of my
favorites because it generalizes the idea of a collection so
elegantly. I will show how to use `traverse` to sort any such
collection safely using a special applicative.

```haskell
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable [] where
  traverse _ [] = pure []
  traverse f (a:as) = (:) <$> f a <*> traverse f as
```

Most meaningful collections are instances of `Traversable`. The idea
is that a collection allows you to call an applicative function for
each element, and the results will get put back together in a
collection of the same shape. At first, this seems a little hard to
sort a collection this way, because the function being called only
gets access to one element at a time. But one of the
[defining characteristics of `Traversable`](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf)
is the fact that it's a subclass of `Foldable`, meaning you can always
extract its contents into a list.

```haskell
newtype Const a b = Const { getConst :: a }

instance Functor (Const a) where
  fmap _ (Const a) = Const a

instance Monoid a => Applicative (Const a) where
  pure _ = Const mempty
  Const a <*> Const b = Const (mappend a b)


toList :: Traversable t => t a -> [a]
toList = traverse (\a -> Const [a])

-- Similarly, the reason `Foldable` is a superclass:
foldMap :: Monoid m => (a -> m) -> t a -> m
foldMap f = getConst . traverse (Const . f)
```

Given this, we can sort the contents of any collection by sorting the
result of `toList`. This is fine, but it's not quite there yet. We
really wanted to sort in `t`, not in `[]`. That is, we wanted the
order of the elements in the original traversable structure to change,
so that we can keep using that type instead of being forced to stick
with lists.

Which brings us to the main characteristic that `Traversable` has
*over* `Foldable`; you can always put elements back. How we do this
will start out a little ugly, but it will get better later on. We'll
start by using the `State` monad for our applicative effect to pop
elements off of a list and put them into place. This will be
inherently unsafe, because we can't be sure that the list we're
popping off of will have enough elements for the collection. But rest
assured, the traversable laws have a roundabout way of dictating that
the collection will always preserve its exact shape (and size) when
you use `traverse`.

```haskell
unsafeReifyContents :: Traversable t => t a -> [b] -> t b
unsafeReifyContents t bs = evalState (traverse f t) bs
 where
  f _ = do
    (b:bs') <- get
	put bs'
	return b
```

The pattern we're binding `get` to is unsafe, but as long as the list
is at least as long as the traversable, it will never fail. Meaning we
can safely do this:

```haskell
sortTraversable :: (Ord a, Traversable t) => t a -> t a
sortTraversable as =
  let list = toList as
  in unsafeReifyContents as (sort list)
```

While we have the nice list structure, we can do whatever we want to
it, as long as we don't change the shape of the list itself. This
means we can reorder the elements, as long as we don't change the
length. So great, we can now sort a list, vector, or whatever else
using the same function.

But this is pretty unsatisfying. The use of a partial function is
pretty gross. But there's a really weird little applicative that we
can use to fix this. It's easiest to define using the free
applicative, so I won't be writing an applicative instance by hand
here. And although I'll import `Control.Applicative.Free`, I'll only
use `liftAp` and `runAp`, meaning that this should work with any free
applicative (this matters because the one I'm importing will be very
inefficient).

```haskell
{-# LANGUAGE GADTs #-}

import Control.Applicative.Free (Ap, liftAp, runAp)
import Data.Functor.Identity

data Mono x y a where
  Mono :: x -> Mono x y y

liftMono :: x -> Ap (Mono x y) y
liftMono = liftAp . Mono

unMono :: (x -> y) -> Mono x y a -> a
unMono f (Mono x) = f x

runMono :: (x -> y) -> Ap (Mono x y) a -> a
runMono f = runIdentity . runAp (Identity . unMono f)
```

As I said, this is a *weird* little applicative. `liftMono` is our
main primitive here. Given a value of type `x`, it can create an
effect that yields `y` in an applicative context that uses `Mono` to
relate `x` to `y` in some way. Basically, this applicative just lets
you record `x`s, and promises to turn each one into a `y` later on. I
called it `Mono` because it's basically the free applicative over
`Identity` if you could force `Identity` to be *monomorphic* on
`x`. Now watch what happens when we traverse with this applicative.

```haskell
foo :: Traversable t => (x -> y) -> t x -> t y
foo f = runMono f . traverse liftMono
```

You might recognize that type signature. Yep, it's `fmap`. This
function extracts all the elements of a collection into this `Ap (Mono
x y)` structure, then converts each `Mono x y y` into a `y` using the
supplied function, and finally accumulates all those results back into
the collection using the pure functions that the free applicative kept
hold of. This is important because it proves that we can record all
the elements of a collection into this applicative statically, and
safely reify them back into the traversable with `runMono`. To see
this more clearly, recall the basic free applicative definition.

```haskell
data Ap f a where
  Pure :: a -> Ap f a
  Ap :: f x -> Ap f (x -> a) -> Ap f a

instance Functor (Ap f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Ap x g) = Ap x (fmap (f .) g)

instance Applicative (Ap f) where
  pure = Pure
  Pure f <*> a = fmap f a
  Ap x f <*> a = Ap x (flip <$> f <*> a)
```

Structurally, it's just a linked list of `f x` where each element has
a potentially different type for `x`, meaning it's sort of a
heterogeneous list. The list is terminated by `Pure`. Since the
cons-ing part of the `Ap` constructor uses a function type, we know
that the value in `Pure` will be a pure function with one argument for
each preceding `Ap`, unless of course there was no preceding `Ap`. In
the case of `traverse`, that function will take one argument for every
element of the collection.

When we apply this to `Mono`, we know that we're always pinning `x` to
the `y` type, meaning the applicative becomes homogeneous. To clarify,
here's how it looks with `Mono` specialized in:

```haskell
data Ap x y a where
  Pure :: a -> Ap x y a
  Ap :: x -> Ap x y (y -> a) -> Ap x y a
```

Suddenly, there's no existential types involved, and the "list"
becomes monomorphic. The pure function stored in `Pure` has one
argument for each `x`, except that the type of that argument is `y`,
meaning we have to convert `x` to `y` before we can call that
function. What that function does is up to the code that's calling
`(<*>)` and `fmap`. Again, `traverse` will put in place a function
that takes each `y` as an argument in order to rebuild the collection with `y`s.

Abstractly, we're kind of building an existentially defined, type
level encoding of the length of a collection. But the point is, we now
have this monomorphic structure that we can use to look at `x`.
Specifically, we can take this weird kind of linked list and sort it.

```haskell
insertion :: Ord x => x -> Ap (Mono x y) a -> (x, Ap (Mono x y) a)
insertion x (Pure a        ) = (x, Pure a)
insertion x (Ap (Mono x') g) = if x < x'
  then (x, Ap (Mono x') g)
  else let (x'', g') = insertion x g in (x', Ap (Mono x'') g')

sortAp :: Ord x => Ap (Mono x y) a -> Ap (Mono x y) a
sortAp (Pure a       ) = Pure a
sortAp (Ap (Mono o) f) = let (o', f') = insertion o (sortAp f) in Ap (Mono o') f'

sortTraversable :: (Ord x, Traversable t) => t x -> t x
sortTraversable = runMono id . sortAp . traverse liftMono
```

This reshuffles the `x` elements of `Ap (Mono x y) a` without changing
the `Pure` function at the end in any way. The order of `x` values in
the "list" has been sorted, but the function consuming those values is
unchanged. This mismatch results in `traverse` receiving elements in
sorted order, rather than the original order.

---

Now, that was a really innefficient insertion sort, but I only meant
for it to be a proof of concept. But the point of a free structure is
that anything you can do to it once it's built is something that can
be replicated in a custom version during building. Meaning there's
probably some way to sort elements within `(<*>)` instead of in some
external function, but it's probably not trivial. Here's a start, but
I've left a hole in the part that I didn't care to try any harder to
figure out.

```haskell
{-# LANGUAGE GADTs #-}

data Sort x y a where
  Pure :: a -> Sort x y a
  Sort :: x -> Sort x y (y -> a) -> Sort x y a

instance Show x => Show (Sort x y a) where
  show (Pure _) = "Pure"
  show (Sort o f) = "Sort (" ++ show o ++ ") (" ++ show f ++ ")"

instance Functor (Sort x y) where
  fmap f (Pure x) = Pure (f x)
  fmap f (Sort a b) = Sort a (fmap (f .) b)

instance Ord x => Applicative (Sort x y) where
  pure = Pure
  Pure f <*> a = fmap f a
  f <*> Pure a = fmap ($ a) f
  Sort a f <*> Sort b g =
    if a < b
    then Sort a (flip <$> f <*> Sort b g)
    else _ -- FIXME

liftSort :: x -> Sort x y y
liftSort a = Sort a (Pure id

runSort :: (x -> y) -> Sort x y a -> a
runSort _ (Pure a  ) = a
runSort f (Sort a g) = runSort f g (f a)

sortTraversable :: (Ord a, Traversable t) => t a -> t a
sortTraversable = runSort id . traverse liftSort
```

The `(<*>)` should take two already sorted programs and perform a
sorted merge on them. If the `FIXME` were implemented to do this, it
would run equivalent to mergesort when used with tree traversables
(though I'm not sure on the asymptotics; you have to keep in mind that
`fmap` runs in linear time here). With lists, I think it would be an
insertion sort. And this is actually interesting on its own; what
sorting algorithm this would be depends on the traversable
instance. I'd be interested to see different sorting algorithms
implemented as different combinations of traversable instances and
applicative instances. Then we could start mixing matching those
instances to get potentially brand new sorting algorithms.

---

Anyway, I've got one more thing to show. I'll switch back to using the
free applicative, since I *know* that obeys the applicative laws and
since I already have a working sort for that.

There's another way to represent traversable things. The `lens`
library has the `Traversal` type for representing explict variants of
`traverse` as lens-style combinators. In fact, the definition of
`Traversal` unifies with the type of `traverse`, so we can just plug
that in in place of the type class constraint.

```haskell
sortTraversal :: Ord a => Traversal' s a -> s -> s
sortTraversal tr = runMono id . sortAp . tr liftMono
```

We can use this function to sort a structure over any traversals
defined using the enormous world of lens combinators. For example, if
you want to sort a list of lists as though it were one big
concatenated list, without losing the nested list structure, you can
just use `sortTraversal (traverse.traverse)`.

The coolest thing about this is
[the `Each` class in `lens`](https://hackage.haskell.org/package/lens-4.15.1/docs/Control-Lens-Each.html).
Basically, it's the `lens` alternative to `MonoTraversable`. It just
gives us a `Traversal` for various types that can't have `Traversable`
instances. One nice thing this is used for is traversing all the
values in a homogeneous tuple, using `each :: Traversal (a, a, a) (b,
b, b) a b` (of course this is defined for many arities).

```haskell
sortEach :: (Ord a, Each s s a a) => s -> s
sortEach = sortTraversal each
```

If we go into GHCi:

```haskell
> sortEach (-30, 5, 2, 10, 3)
(-30, 2, 3, 5, 10)
```

So now we have type safe homogeneous tuple sorting for free. Pretty sweet.

---

This is yet another reason that applicatives and traversables are
awesome. The use cases for the `Mono` applicative don't end here. You
can use it to
[get a `Traversing` instance for any `ArrowChoice`](https://github.com/ekmett/profunctors/pull/40/files#diff-9f4c540a750f660a8341736af46009e4R107)
for example (though that definition put much more thought into
optimization). Basically, if you ever need to retain monomorphic
information in an applicative context, `Mono` is the thing for the
job.
