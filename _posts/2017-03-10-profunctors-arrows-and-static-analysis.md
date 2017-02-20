---
layout: post
title:  "Profunctors, Arrows, & Static Analysis"
date:   2017-03-10
categories:
---

In the past, I've talked about using applicative functors to do static analysis
([1](http://elvishjerricco.github.io/2016/04/08/applicative-effects-in-free-monads.html),
[2](http://elvishjerricco.github.io/2016/04/13/more-on-applicative-effects-in-free-monads.html),
[3](http://elvishjerricco.github.io/2016/09/17/abstracting-async-concurrently.html)).
In this post, I'm going to explore a concept known as `Arrow`, and compare its
capabilities to those of monads and applicatives. Arrows have a high granularity
of features. This post will be split into sections for each of those features.
Those sections are:

- Composition
- Mapping
- Strength
- Choice
- Traversing

This post has gotten quite a bit longer than I expected. I thought about
splitting it up into several parts to make it more digestible, but I've been
working on it for a while now, and ultimately just want to get it out the door.
So I hope you'll forgive me if this is a little incoherent and rambling.

*(Disclaimer: Many of the definitions that this post displays from existing
modules are simplified. What you see in this post will be sufficient for using
these definitions, but the exact implementations may differ.)*

Background - Analysis of Expressive Programs
---

Monads are important in Haskell because they provide a mechanism for writing
expressive programs that have some power beyond traditional purity. With `IO`,
this power comes in the form of impure primitives like `getLine` or `putStrLn`.
Applicatives, however, are important because they restrict that power to a
simpler interface, which allows some applicatives to manipulate the program in
more ways.

To see what I mean about applicative, we have to see a computation as being
built up of a bunch of component computations. The difference between
applicatives and monads is that with applicatives, you can see **what** those
components are before you run the program. You don't know what their results
will be, but you can at least see what they're going to try to do. For example,
with this expression, you can clearly see the two components individually from
one another before you have to run them.

```haskell
(,) <$> getUsername userId1 <*> getUsername userId2
```

The two components are the two calls to `getUsername`. Ok, we might not know at
compile time what `userId1` and `userId2` are, but the important part is that `(<*>)`
can see what both of them are at runtime before it has to actually run the
queries. It could in principle batch both calls into one SQL query. This could
be loosely defined as static analysis. It's not *exactly* static, since the
analysis is occurring at runtime. But the thing being analyzed is static for the
duration of the analysis, so for all intents and purposes, it's static analysis.

Batching just two queries with `(<*>)` is one thing. But what if we have a list
of queries to batch? The `Traversable` typeclass provides a mechanism for using
this batching `(<*>)` on a collection of values.

```haskell
class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

instance Traversable [] where
  traverse _ [] = pure []
  traverse f (a:as) = (:) <$> f a <*> traverse f as
```

This allows you to iterate applicative effects over collections of data whose
size and shape are unknown. A traversal will create a computation with one
component for each element in the collection. If the applicative is capable of
analyzing those components, then using `traverse` allows that applicative to
analyze any arbitrary sequence of components. In the case of `traverse
getUsername ids`, you could imagine the applicative building one big long SQL
query that gets the usernames for a bunch of IDs in one go.

The problem with applicatives is that they're pretty much *only* static. A
computation made entirely of `getUsername`, `fmap`, and `(<*>)` will never be
able to use `getUsername`, look at the result, and decide what component comes
next. That is, a component can never depend on the results of the previous
component. They have to be isolated and independent. Meaning code like this
isn't possible using just applicatives:

```haskell
getFriendsUsernames :: Id -> DB [Username]
getFriendsUsernames user = do
  ids <- getFriends user
  traverse getUsername ids
```

This code is necessarily monadic, meaning this won't work if `DB` is just an
applicative. You have to use a monad for this code. You might think it's ok,
since every monad is an applicative. That applicative instance might as well do
the static analysis, right? But unfortunately, that wouldn't exactly be law
abiding. There is a law:

```haskell
f <*> a = f >>= (\f' -> a >>= (\a' -> return (f' a')))
-- Or
f <*> a = do
  f' <- f
  a' <- a
  return (f' a')
```

`(>>=)` cannot do any such static analysis. When a monad instance sees an
expression of the form `a >>= (\x -> f x)`, it cannot see inside the right hand
argument. It has no way to see what computation comes after `a` in order to do
static analysis. It has to run `a` first, meaning it can't possibly batch `a`
with whatever comes next. So if the monad isn't able to do static analysis, and
the applicative instance has to be equivalent to using the monad, then the
applicative instance also must not do any static analysis. This is true for any
monad.

```haskell
do
  friends <- getFriends userId
  getUsername (head friends)
```

In this expression, we can't possibly know which username we'd like to fetch
until *after* we've already executed `getFriends`. So it's not possible for this
monad to batch those queries. Libraries like
[Haxl](https://github.com/facebook/haxl) get away with breaking this law because
they promise that their applicative instance is *close enough*. But it's not
strictly law abiding, and making that promise is dangerous. Plus, it gives the
user of the library a false sense that whatever they do will be batched for
them. In reality, you have to be careful not to accidentally use a monadic
effect where you meant to use an applicative effect. Facebook uses
`-XApplicativeDo` and their own prelude to make this especially rare, but it's
not a catchall.

---

Composition
---

We'll kick off our journey into arrows by talking about categories. Applicatives
aren't the only way to have static component computations. The `Category` class
provides an entirely different way of talking about computations. I'll try not
to get too deep into category theory in this post. Instead, I'll focus on the
Haskell class `Category` from the `Control.Category` module in `base`.

```haskell
import Prelude hiding (id, (.))

-- Laws:
--
-- f . id = id . f = f
-- (f . g) . h = f . (g . h)
class Category cat where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

(>>>) :: Category cat => cat a b -> cat b c -> cat a c
(>>>) = flip (.)

instance Category (->) where
  id = \a -> a
  f . g = \a -> f (g a)
```

Categories are meant to look a lot like functions. There is an input type and an
output type. The methods of this class quite obviously parallel the functions of
the same name in `Prelude`, except that these are meant to be overloaded with
new instances. A category allows you to take function-like things and compose
them, while guaranteeing that an `id` exists that does nothing to its input.

Expressions built using `Category` will have the form `f . g . h ...`. Like
`Applicative`, this has the quality of being made up of statically analyzable
components. Before doing any of the actual work that the expression asks for,
you can analyze it for patterns and form optimizations. The key difference is
that with a category, information is being threaded through the program. Unlike
applicative, where the inputs are known statically, and the outputs are computed
at runtime, categories make inputs and outputs both at runtime. But the
structure of the computation remains static.

But `Applicative` has one feature that `Category` doesn't. You can throw
arbitrary pure expressions into the mix using `pure` and `fmap`. With
`Category`, you can only use whatever expressions the instance presents to you
in its API. `pure` and `fmap` allow you to manipulate applicative expressions
using arbitrary Haskell functions. This is really important if we want to be
able to do any meaningful work with our category.

---

Mapping
---

Disjoint from `Category`, but closely related, is the `Profunctor` class.
Profunctors will give us the means to make up for the lack of pure expressions
in `Category`. `Profunctor` makes this possible by allowing you to map over the
values coming in and out of a category expression.

```haskell
class Profunctor p where
  dimap :: (i' -> i) -> (o -> o') -> p i o -> p i' o'
  dimap l r = lmap l . rmap r

  lmap :: (i' -> i) -> p i o -> p i' o
  lmap f = dimap f id

  rmap :: (o -> o') -> p i o -> p i o'
  rmap f = dimap id f

  {-# MINIMAL dimap | (lmap, rmap) #-}

instance Profunctor (->) where
  dimap f g h = \i' -> g (h (f i'))
```

A profunctor looks sort of like a category, though isn't necessarily a category.
It's got an input type, and an output type. But it allows you to use pure
Haskell functions to modify them, and it doesn't know how to compose. You can
define a profunctor using either `dimap` or the combination of `rmap` and `lmap`
(the relationship should be obvious). You should think of profunctors as regular
functors, except that you can map inputs, not just outputs.

When something is both a `Category` and a `Profunctor`, you get composable
expressions that can be changed with arbitrary Haskell functions. In fact, a
pure Haskell function can be lifted directly into such a type.

```haskell
arr :: (Category p, Profunctor p) => (i -> o) -> p i o
arr f = dimap id f id
```

Those of you familiar with the `Arrow` class will recognize this function from
it. This is because `arr` really ought to exist in a superclass of `Arrow` which
has been nicknamed `PreArrow`, despite not existing. `PreArrow` might as well be
a class synonym for `Category` + `Profunctor`. *(Though in fact, my favorite
representation of arrows in category theory [treats `PreArrow`s as monoids in
the category of profunctors](https://arxiv.org/abs/1406.4823), but I told you I
wouldn't get too deep into category theory here.)*

For the sake of completeness, I'll show how the `PreArrow` class would look
here. But we will not be using it in any way through the rest of this post.

```haskell
class Category p => PreArrow p where
  arr :: (a -> b) -> p a b

dimap' :: PreArrow p => (i' -> i) -> (o -> o') -> p i o -> p i' o'
dimap' l r f = arr r . f . arr l
```

Prearrows change our expressions from looking like this: `f . g . h`, to looking
like this: `dimap l r f . dimap l' r' g . dimap l'' r'' h`. The addition of
`dimap` means there are pure functions gluing all the components together. `l`
must line up with `r'`, `l'` must line up with `r''`, and so on. If we exploit
this knowledge, we can compose those pure glue parts, and change that
representation to this: `arr x . f . arr y . g . arr z . h ...`.

This is our first inkling of the `Arrow` class. We're still one big step away
from `Arrow`, so we're not quite ready to talk about arrows. But we can begin
talking about prearrows, as long as we pretend that "prearrow" is just a synonym
for "category + profunctor".

Free
---

Before we get to an example, I want to talk about free prearrows. A free
prearrow is one that gives us an instance of `Category` and `Profunctor` for any
type with minimal effort from that type. In this context, "minimal effort" can
mean a lot of things. The minimum that we can provide here is the "data source",
meaning the definitions of `f` and `g` in an expression like `dimap l r f .
dimap l' r' g`. A truly free prearrow would provide `(.)` and `dimap`. But we
can actually separate those two and do them one at a time. If we separate `(.)`
and `dimap`, we can start with the data source, give it a free `Profunctor`
instance, and *then* give it a free `Category` instance that preserves the
`Profunctor` instance. This will be valuable because it means we can change the
`Profunctor` instance and the data source in isolation from each other and from
the `Category` instance, which will prove valuable later.

```haskell
{-# LANGUAGE GADTs #-}

-- | Data source
data DataSource a b where
  GetUsernames :: DataSource (Set Id) (Map Id Username)
  GetFriends :: DataSource Id (Set Id)

-- | Free profunctor for any data source
data FreeProfunctor p a b where
  FreeProfunctor :: (a -> x) -> p x y -> (y -> b) -> FreeProfunctor p a b

instance Profunctor (FreeProfunctor p) where
  dimap l r (FreeProfunctor l' f r') = FreeProfunctor (l' . l) f (r . r')

-- | Free prearrow for any profunctor
data Free p a b where
  Hom :: (a -> b) -> Free p a b
  Comp :: p x b -> Free p a x -> Free p a b

instance Profunctor p => Profunctor (Free p) where
  dimap l r (Hom f) = Hom (dimap l r f)
  dimap l r (Comp f g) = Comp (rmap r f) (lmap l g)

instance Profunctor p => Category (Free p) where
  id = Hom id
  Hom g . f = rmap g f
  Comp h g . f = Comp h (g . f)

type Arr p = Free (FreeProfunctor p)
liftArr :: p a b -> Arr p a b
liftArr f = Comp (FreeProfunctor id f id) (Hom id)

getUsernames :: Arr DataSource (Set Id) (Map Id Username)
getUsernames = liftArr GetUsernames

getFriends :: Arr DataSource Id (Set Id)
getFriends = liftArr GetFriends
```

*(Note: The `Free` type is especially cool to me because it is the free monoid
in the category of profunctors. This is explained more fully in that paper I
linked before.)*

`FreeProfunctor` works by surrounding the data source with pure functions that
we know feed into and out of it. We can implement `dimap` by leaving the data
source alone and composing functions with the functions that feed into and out
of the data source. Later, when we pattern match on our data source, the usage
of `GADTs` to pin the types will allow us to see what the types that these
functions provide and take are, so that we can correctly make use of them
somehow.

The `Free` type is a little more complicated. It looks a little bit like a
`Hom`-terminated linked list. We lose the info about the types between all the
`p` values. But we know that we start with `a` in `Hom`, and end with `b` in
`Comp`. Composition will behave similarly to list concatenation. We'll fuse one
of the `Hom`s with one of the `p`s using `p`'s `Profunctor` instance, and then
we'll concatenate the lists. All this does is put all the components into an
accessible data structure.

`Free` is not performing any kind of logic aside from the fusion of the one
`Hom`. It's really quite dumb. It's up to us to figure out how to use that
structure, which is often called "running" it. Traditionally, you run free
structures by converting each component to a normal version. So if we can
convert each `DataSource` value into some normal prearrow, we can use that
prearrow to actually run everything.

```haskell
{-# LANGUAGE RankNTypes #-}

runFree
  :: (Category q, Profunctor q)
  => (forall x y. p x y -> q x y)
  -> Free p a b
  -> q a b
runFree _ (Hom g) = arr g
runFree f (Comp g h) = f g . runFree f h

runPro
  :: Profunctor q
  => (forall x y. p x y -> q x y)
  -> FreeProfunctor p a b
  -> q a b
runPro f (FreeProfunctor l g r) = dimap l r (f g)

runArr
  :: (Category q, Profunctor q)
  => (forall x y. p x y -> q x y)
  -> Arr p a b
  -> q a b
runArr f = runFree (runPro f)
```

These three functions just let you do that conversion. They use rank n types to
say they need a function that can convert *any* `p` into a `q` with the same
types. Once you have this direct conversion, it will replace each `p` with a
`q`, and then collapse the free structures using `q`'s instances of `Profunctor`
and `Category`.

`Control.Arrow` has one really good candidate for `q` in `Kleisli`, which
basically just lets you pack any monad into an arrow interface by writing
functions of the form `a -> m b`. You can convert your data source into these
monadic functions in order to get monadic side effects, and `Kleisli` will take
care of doing all the composition work.

```haskell
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }

instance Functor m => Profunctor (Kleisli m) where
  dimap l r (Kleisli f) = Kleisli (dimap l (fmap r) f)

instance Monad m => Category (Kleisli m) where
  id = Kleisli return
  Kleisli f . Kleisli g = Kleisli (\a -> g a >>= f)

runArrM :: Monad m => (forall x y. p x y -> x -> m y) -> Arr p a b -> (a -> m b)
runArrM f = runKleisli . runArr (Kleisli . f)
```

With all these tools, we can start working with an example. First, we'll create
an expression that takes a user ID in, gets that user's friend list, and gets
the usernames of all those friends.

```haskell
getFriendsUsernames :: Arr DataSource Id [Username]
getFriendsUsernames = arr toList . getUsernames . getFriends
```

Next, we need to convert `DataSource` to `Kleisli IO` with `runArrM` so that we
can actually run this program in `IO`. Just to contrive things slightly, let's
say that we get usernames from a REST API, and we get friend lists from
PostgreSQL. Since they're from different services, they'll need different
resources to access them.

```haskell
runDataSource :: Arr DataSource a b -> (a -> IO b)
runDataSource = runArrM toIO
  where
    toIO :: DataSource x y -> x -> IO y
    toIO GetUsernames users = do
      manager <- newManager defaultManagerSettings
      -- Call the REST API to get usernames
      ...
    toIO GetFriends user = do
      conn <- connectPostgreSQL ""
      -- Query PostgreSQL for friends
      ...
```

This is fine, but it's got a major flaw. Every request calls `connectPostgreSQL`
or `newManager`, which are expensive operations. It'd be better if we created
these resources once and reused them. Obviously, we could just factor them out
above `runArrM`. But we can actually do one better. We have the analyzable
features of `Category` available to us. Specifically, we can examine the
expression and look to see if either of the two types of request is never
called. If so, we can choose not to create that resource. We'll have to do a
once-over to determine which resources need to be created.

To help with that, we can do something similar to the `Const` applicative in
`Data.Functor.Const`, but for prearrows. A `Const` prearrow will ignore its
input and output types, and instead just contain a constant value of some
tertiary type. When we try to `dimap` over `Const`, nothing will happen, since
we've ignored the input and output types. But when we try to compose with
`Const`, we'll want to combine the constant values somehow. Since we also have
to provide a constant value for free for `id`, this is a perfect use-case for
monoids.

```haskell
newtype Const w a b = Const { getConst :: w }

instance Profunctor (Const w) where
  dimap _ _ (Const x) = Const x

instance Monoid w => Category (Const w) where
  id = Const mempty
  Const x . Const y = Const (x <> y)
```

If we target `Const` with `runArr`, we can use a monoid to record the presence
of particular requests. In particular, we can use the `Any` monoid from
`Data.Monoid` by recording `True` when a request is encountered, and we can
abuse the fact that `(,)` is a monoid when both elements are monoids to do this
for both types of requests.

```haskell
resourcesNeeded :: Arr DataSource a b -> (Bool, Bool)
resourcesNeeded f =
  let toAny :: DataSource x y -> Const (Any, Any) x y
      toAny GetUsernames = Const (Any True, Any False) -- Need manager
      toAny GetFriends = Const (Any False, Any True) -- Need postgres
      Const (Any managerNeeded, Any postgresNeeded) = runArr toAny f
  in (managerNeeded, postgresNeeded)

runDataSource' :: Arr DataSource a b -> a -> IO b
runDataSource' f a = do
  let (managerNeeded, postgresNeeded) = resourcesNeeded f
  manager <- if managerNeeded
    then Just <$> newManager defaultManagerSettings
    else return Nothing
  conn <- if postgresNeeded
    then Just <$> connectPostgreSQL ""
    else return Nothing

  let toIO :: DataSource x y -> x -> IO y
      toIO GetUsernames users = do
        let manager' = fromJust manager
        -- Call the REST API to get usernames
        ...
      toIO GetFriends user = do
        let conn' = fromJust conn
        -- Query PostgreSQL for friends
        ...

  runKleisli (runArr (Kleisli . toIO) f) a
```

The use of `fromJust` suggests that the correct thing to do is to lazily load
the resources, rather than trying to abuse `Maybe` this way. But these
implementation details aren't really the point. The point is that this has
implemented an idea that is not possible with (law abiding) monads or
applicatives. Monad doesn't allow you to examine what requests will be made, and
Applicative doesn't allow requests to depend on the results of previous
requests.

---

Strength
---

But we'll quickly hit a wall with this. What happens when we try to get a user's
username *and* their friends list? With the existing framework, it's not
actually possible. The problem is that once a value is piped into the data
source, there's no way to get it back. When we pipe a user ID into the
`GetFriends` request, we have to no way to go back and remember what that user
ID was to pass it into the next request. We would have to have the `DataSource`
requests also return their input arguments so that we can reuse them if we want
them, and ignore them if we don't. Now, we got lucky becuase it happens to be
the case that the `GetUsernames` request already returns the input set as the
key set of the map that is returned. But of course the `GetFriends` request
would have to be changed.

```haskell
data DataSource a b where
  GetUsernames :: DataSource (Set Id) (Map Id Username)
  GetFriends :: DataSource Id (Set Id, Id)
```

It's just a little gross that we have to do this manually. Luckily, the
`profunctors` package already has an abstraction for this. It's a property a
profunctor can have called **strength**, represented by the `Strong` class. A
profunctor is strong if it can freely pass unknown values through it without
modification. This is represented by adding an input to the profunctor in a
tuple, and adding that same type to the output in a tuple, allowing a value to
safely pass through.

```haskell
class Profunctor p => Strong p where
  first' :: p a b -> p (a, c) (b, c)
  first' f = dimap swap swap (second' f)

  second' :: p a b -> p (c, a) (c, b)
  second' f = dimap swap swap (first' f)

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

instance Strong (->) where
  first' f (a, c) = (f a, c)

instance Functor m => Strong (Kleisli m) where
  first' (Kleisli f) = Kleisli $ \(a, c) -> fmap (\b -> (b, c)) (f a)
```

If you have an arrow from `a` to `b`, then you might as well have that arrow tag
a `c` along for the ride. This lets you pass in anything you want to preserve
after the computation is done. So if we wanted to save the user id being passed
into `getFriends`, we would duplicate it in a tuple, let one half go to
`getFriends`, and let the other half pass through safely so that we can recover
it on the output side.

Now, we still need to give an instance of this to our `Arr` type somehow.
Luckily, it's very easy for the `Free` component of it. It simply delegates the
job back to the underlying profunctor.

```haskell
instance Strong p => Strong (Free p) where
  first' (Hom f) = Hom (first' f)
  first' (Comp f g) = Comp (first' f) (first' g)
```

But it's a little more complicated to change `FreeProfunctor`. The `profunctors`
package has something that will do this for us. In `Data.Profunctor.Strong`,
there's a type called `Pastro` that does the same thing as `FreeProfunctor`, but
also gives you strength for free. I won't get into the details, but it does
require reimplementing `runPro` to require `q` to be `Strong`, which of course
cascades to `runArr`.

```haskell
runPro
  :: Strong q
  => (forall x y . p x y -> q x y)
  -> Pastro p a b
  -> q a b
runPro f (Pastro r g l) = dimap l r (first' (f g))

type Arr p = Free (Pastro p)
runArr
  :: (Category q, Strong q)
  => (forall x y . p x y -> q x y)
  -> Arr p a b
  -> q a b
runArr f = runFree (runPro f)
```

Now the contortion we applied to `DataSource` to pass the input through
`GetFriends` can be defined for any strong profunctor. We can write a combinator
that uses strength to pass the input through. With this combinator, we no longer
have to force our data source to do this itself.

```haskell
recoverInput :: Strong p => p a b -> p a (b, a)
recoverInput f = lmap (\a -> (a, a)) (first' f)

friendsAndUsername :: Arr DataSource Id (Set Id, Maybe Username)
friendsAndUsername =
  recoverInput getFriends
    >>> second' (recoverInput $ lmap Set.singleton getUsernames)
    >>> arr (\(ids, (usernames, user)) -> (ids, Map.lookup user usernames))
```

Arrow
---

This all works great, but you may notice that that last example was a little
ugly to write. The biggest advantage to monads is that they have `do` notation
to allow you to write much clearer code that desugars to the powerful `Monad`
abstraction. It'd be nice if our `Arr` abstraction had such a good syntax.

This is what the `Arrow` class from `Control.Arrow` is for. The `Arrow` class is
GHC's way of providing a nice `do` notation for this kind of programming. In
reality, `Arrow` is equivalent to `Category` + `Strong`, but unfortunately,
since the profunctor hierarchy isn't in `base`, `Arrow` has no direct tie to
profunctors. Instead, it reinvents much of the profunctor hierarchy but with
hard `Category` constraints. And as I mentioned before, the mythical `PreArrow`
class is unfortunately not a superclass of `Arrow`. Instead, it looks like this:

```haskell
class Category p => Arrow p where
  {-# MINIMAL arr, (first | (***)) #-}

  arr :: (a -> b) -> p a b

  first :: p a b -> p (a, c) (b, c)
  first = (*** id)

  second :: p a b -> p (c, a) (c, b)
  second = (id ***)

  (***) :: p a b -> p a' b' -> p (a, a') (b, b')
  f *** g = first f >>> arr swap >>> first g >>> arr swap

  (&&&) :: p a b -> p a b' -> p a (b, b')
  f &&& g = arr (\b -> (b,b)) >>> f *** g
```

`Arrow` avoids needing `dimap` by using `arr` instead. As I mentioned earlier,
`arr` is equivalent to `dimap` for prearrows / arrows. `Arrow` duplicates the
methods of `Strong`, but also provides a couple of convenience operators that
I'll touch on at the end of this post. For now we'll ignore them though.

With `Arrow` encompassing strong prearrows, we should be able to use GHC's arrow
notation for everything we've done so far. Arrow notation looks a lot like
monadic `do` notation. You write code in a very monadic looking imperative way,
and GHC converts it to arrow functions, just like it does for monads with `do`
notation. We could rewrite `friendsAndUsername` using arrow notation like this:

```haskell
{-# LANGUAGE Arrows #-}

friendsAndUsername' :: Arr DataSource Id (Set Id, Maybe Username)
friendsAndUsername' = proc user -> do
  friends <- getFriends -< user
  usernames <- getUsernames -< Set.singleton user
  returnA -< (friends, Map.lookup user usernames)
```

Much nicer, right? You start an arrow notation block with `proc arg -> do`. A
statement in arrow notation has three parts. The left hand side of the `<-` is
the pattern to bind results to, just like in monadic `do` notation. But unlike
monadic `do` notation, there are two components to the right of that arrow
instead of one. The middle component (between `<-` and `-<`) is the arrow you
would like to call. The component to the right of the `-<` is the value you'd
like to pipe into that arrow. The last statement of an arrow notation block must
be an arrow without the left-hand binding, and the `do` keyword can be left out
when the last statement is the only statement (just like in monadic `do`
notation). `returnA` is a convenience arrow provided by `Control.Arrow` to make
this a little more familiar. `returnA = id`

There's nothing profound about `Arrow` being equivalent to `Strong` +
`Category`. It's just a different way of representing the same things we did
with profunctors. To prove it, here's a strong profunctor for any arrow, and an
arrow for any strong + category.

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Pro p a b = Pro { runPro :: p a b }
  deriving Category

instance Arrow p => Profunctor (Pro p) where
  dimap l r (Pro f) = Pro (arr l >>> f >>> arr r)

instance Arrow p => Strong (Pro p) where
  first' (Pro f) = Pro (first f)

newtype Str p a b = Str { runStr :: p a b }
  deriving Category

instance (Category p, Strong p) => Arrow (Str p) where
  arr f = Str (rmap f id)
  first (Str f) = Str (first' f)
```

The only reason you need to think about `Arrow` is arrow notation. Otherwise,
you can always think about things in terms of profunctors and categories
instead. Without arrow notation, I would find arrows too cumbersome to use and
maintain in real world code. But with it, we get all the same benefits that
monads get from `do` notation.

---

Choice
---

We'll hit another wall pretty quickly. Arrows allow us to sequence commands one
after the other, passing results between each other. But there is no way to
choose between two arrows based on the results of another arrow. There needs to
be a way to make choices at runtime, which is exactly what the `Choice` class
from `profunctors` does.

```haskell
class Profunctor p => Choice p where
  left' :: p a b -> p (Either a c) (Either b c)
  left' f = dimap mirror mirror (right' f)

  right' :: p a b -> p (Either c a) (Either c b)
  right' f = dimap mirror mirror (left' f)

mirror :: Either a b -> Either b a
mirror (Left a) = Right a
mirror (Right b) = Left b

instance Choice (->) where
  left' f (Left a) = Left (f a)
  left' _ (Right c) = Right c

instance Applicative m => Choice (Kleisli m) where
  left' (Kleisli f) = Kleisli $ \x -> case x of
    Left a -> pure (Left a)
    Right b -> fmap Right (f b)
```

`Choice` runs dual to `Strong`. While strength allows structure to pass through
the arrow, choice allows decisions to pass through. When the decision has been
made to give `Left x` to `left' f`, the arrow respects that and runs the arrow
against `x`. When `Right y` was the decision, the arrow chooses not to run and
passes `y` through. Converting all decisions to a combination of `Either` and
`left'`/`right'` is obviously a painful task, and it's a little hard to see how
this helps us write code that makes decisions. This is why the `Arrow` hierarchy
has an analogue in `ArrowChoice`. Arrow notation will convert `case` and `if`
statements into the appropriate contortion of `Either`s automatically.

```haskell
class Arrow p => ArrowChoice p where
  {-# MINIMAL (left | (+++)) #-}

  left :: p a b -> p (Either a c) (Either b c)
  left = (+++ id)

  right :: p a b -> p (Either c a) (Either c b)
  right = (id +++)

  (+++) :: p a b -> p a' b' -> p (Either a a') (Either b b')
  f +++ g = left f >>> arr mirror >>> left g >>> arr mirror

  (|||) :: p a c -> p b c -> p (Either a b) c
  f ||| g = f +++ g >>> arr untag
    where
      untag (Left x) = x
      untag (Right y) = y
```

Getting `Choice` on our `Arr` type will be a little more complicated than
`Strong` was. There is a `PastroSum` type that does for choice what `Pastro`
does for strength. But it doesn't also preserve strength, so we can't use it.
For now, just take for granted that we have both strength and choice in `Arr`.
I'll show later how we can get both of them, but we're still a step or two away
from that.

If we add an error throwing arrow in our data source, we can use choice to
eliminate the `Maybe` in `friendsAndUsername` by choosing between throwing an
error and returning the name. With arrow notation, we get to write this as a
nice looking `case` expression, but the desugaring will turn this into some kind
of usage of `left`.

```haskell
data DataSource a b where
  DataSourceError :: DataSource String b
  GetUsernames :: DataSource (Set Id) (Map Id Username)
  GetFriends :: DataSource Id (Set Id)

friendsAndUsername'' :: Arr DataSource Id (Set Id, Username)
friendsAndUsername'' = proc user -> do
  friends <- getFriends -< user
  usernames <- getUsernames -< Set.singleton user
  case Map.lookup user usernames of
    Nothing -> liftArr DataSourceError -< "no username for " ++ show user
    Just name -> returnA -< (friends, name)
```

---

Traversing
---

There is one more wall to hit. `GetUsernames` currently uses sets and maps for
its inputs and outputs as a hack to allow getting many usernames at once. But
it's kind of ugly. If we want to get a single username, we have to wrap it in a
set, and then look it up in a map. We really want `GetUsernames` to have the
type `DataSource Id Username`. But we just don't have a way to call an arrow
like that an unbounded number of times. So far, we've only seen how to create
programs that do operations on a constant number of values. We need a way to
iterate an arrow over a collection of inputs.

One of the most important features of Haxl is the fact that you can freely use
`traverse getUsername` and expect it to be properly batched. So however we
decide to iterate with an arrow, it needs to be possible to batch the
collection. Otherwise we'd be making one request for each username we want to
get, instead of getting them all at once.

`profunctors` has a solution for these problems in `Data.Profunctor.Traversing`.

```haskell
class (Choice p, Strong p) => Traversing p where
  traverse' :: Traversable f => p a b -> p (f a) (f b)

instance Traversing (->) where
  traverse' = fmap

instance Applicative m => Traversing (Kleisli m) where
  traverse' (Kleisli f) = Kleisli (traverse f)
```

Traversing profunctors are capable of running once per element of a traversable.
If one of your arrows produces a collection of results, `traverse'` lets you
iterate over those with an arrow. Because of the laws of `Traversable`, we know
that each element in the input collection will have a corresponding result in
the output collection.

It's interesting that `Choice` and `Strong` are superclasses of `Traversing`.
Conceptually, this is because traversing embodies both ideas. You need choice
because there can be zero or many elements in the collection. You need strength
to preserve the skeleton of the collection (such as the keys in a map) between
the input and output. Concretely we can actually show that traversing proves
choice and strength by implementing them in terms of traversing.

```haskell
instance Traversable (Either x) where
  traverse _ (Left x) = pure (Left x)
  traverse f (Right a) = Right <$> f a

instance Traversable ((,) x) where
  traverse f (x, a) = (,) x <$> f a

secondTraversing :: Traversing p => p a b -> p (c, a) (c, b)
secondTraversing = traverse'

rightTraversing :: Traversing p => p a b -> p (Either c a) (Either c b)
rightTraversing = traverse'
```

This is why I didn't try to get choice for free earlier. It's much easier to
just get traversing for free than it is to try and get both choice and strength
without it. And in `Data.Profunctor.Traversing`, we get just that. There's a
`FreeTraversing` type that does all of this for us. With it, we can get
`Traversing`, `Strong`, and `Choice` all on our `Arr` type for free.

To use `FreeTraversing`, we have to make some changes to our stack. First, we
need to change `runArr` to support `Traversing`. We could just change the
constraint to require `Traversing`, so that we can use some underlying
profunctor to actually implement the traversing. But an equivalent option, which
I think is more convenient, is to allow the interpreter function to do the
traversing itself.

```haskell
type Arr p = Free (FreeTraversing p)

liftArr :: p a b -> Arr p a b
liftArr = Comp (FreeTraversing runIdentity f Identity) (Hom id)

runArr
  :: (Category q, Profunctor q)
  => (forall f x y . Traversable f => p x y -> q (f x) (f y))
  -> Arr p a b
  -> q a b
runArr _ (Hom g) = rmap g id
runArr f (Comp (FreeTraversing unpack g pack) h) =
  dimap pack unpack (f g) . runArr f h
```

The first argument to `runArr` (the interpreter) is expected to perform the
traversing itself. Given an instance of your data source, and an input of a
traversable collection, the interpreter traverses that collection and returns
results. Because plain `traverse` takes an applicative, we know it's at least
possible to do static analysis on traversals, meaning we can definitely concoct
some way to batch queries in an interpreter.

Next, we should change the data source a little bit. If we're going to have
`traverse'` available, we probably shouldn't have `GetUsernames` do batching
explicitly. Instead, it should take in one ID, and produce one username. When we
want to get many usernames, we will call `traverse' getUsername`.

```haskell
data DataSource a b where
  GetUsername :: DataSource Id Username
  GetFriends :: DataSource Id (Set Id)

getUsername :: Arr DataSource Id Username
getUsername = liftArr GetUsername

getFriends :: Arr DataSource Id (Set Id)
getFriends = liftArr GetFriends
```

As an example, we'll revisit the task of getting the usernames of someone's
friends.

```haskell
getFriendsUsernames :: Arr DataSource Id (Map Id Username)
getFriendsUsernames = proc user -> do
  friends <- getFriends -< user
  traverse' getUsername -< Map.fromSet id friends
```

I think this is noticably nicer than what we had to do before. Previously, we
had to write `GetUsername` such that it could take in many IDs and return many
usernames. Now, we get to do it much more structurally soundly. One ID means one
username, and traversing with that means we can do it for many IDs.

Finally, we need to be able to run our data source. The skeleton is going to
remain pretty much unchanged. The only thing we need to modify is the `toIO`
function that served as our interpreter, and the `toAny` function that we used
to accumulate those booleans. `toAny` is easy. Since `Const` doesn't actually
use it's input and output types, we can just change it to `toAny :: DataSource x
y -> Const (Any, Any) (f x) (f y)`, without changing the body at all.

But it will take a little more work to fix `toIO`. For `GetUsername`, we'll have
to acquire all the IDs we want to fetch from the traversable, and then find a
way to put the results back into the traversable. Since `Foldable` is a
superclass of `Traversable`, we can just use `foldr` to get the IDs out as a
set. To restore the results to the traversable, we can traverse it and use
`Map.lookup` on the results to map the original inputs to their results.
`traverse` will bubble that `Maybe` out to the top, and we can handle the error
case of `Nothing` accordingly.

```haskell
runDataSource :: Arr DataSource a b -> a -> IO b
runDataSource = do

  ...

  let toIO :: Traversable f => DataSource x y -> f x -> IO (f y)
      toIO GetUsername users = do
        let manager' = fromJust manager
            userIds = foldr Set.insert Set.empty users
        -- Call the REST API to get usernames
        usernames <- ...
        return $ fromJust $ traverse (`Map.lookup` usernames) users

      ...
```

Despite getting rid of the explicit batching in `GetUsername`, we can still
batch its use in `traverse'`. This is a really cool form of static analysis.
We're able to see ahead of time where a query is traversed in order to optimize
that query for many inputs.

Each
---

This works great for *one* or *many* requests. But what if we want to make a
*few* requests? That is, what if we have exactly three user IDs and we want to
get their usernames? We could just call `getUsername` three times in a row, but
that doesn't batch the queries. We need some way of traversing a discrete data
structure, like a tuple of three IDs. It's not immediately obvious how, but we
can use a function called `wander` from `profunctors` to do this.

```haskell
wander
  :: Traversing p
  => (forall f. Applicative f => (a -> f b) -> s -> f t)
  -> p a b
  -> p s t
wander = ...
```

I'm not going to show the definition of `wander` or try to explain it, because I
honestly don't fully understand the implementation. But those of you familiar
with lenses will recognize the first argument as the same type as `Traversal s t
a b`.

```haskell
wander :: Traversing p => Traversal s t a b -> p a b -> p s t
```

And those of you familiar with the profunctor encoding of lenses will
recognize this as a function that converts a van Laarhoven style `Traversal` to
a profunctor style `Traversal`. But anyway, for our purposes, the point is that if you
have a `Traversal`, you can use any traversing arrow over it.

The traversal that we're interested in comes straight out of the `lens` package.
The `Control.Lens.Each` module defines the `Each` class. An instance of `Each`
provides a traversal of a specific type of its choosing.

```haskell
class Each s t a b | s -> a, t -> b, s b -> t, t a -> s where
  each :: Traversal s t a b

instance Each [a] [b] a b where
  each = traverse

instance Each (a,a) (b,b) a b where
  each f (a,b) = (,) <$> f a <*> f b

instance Each (a,a,a) (b,b,b) a b where
  each f (a,b,c) = (,,) <$> f a <*> f b <*> f c

...
```

*(Note: The actual implementations of these instances do some tricks with type
equalities among other things to improve performance, type inference, and error
messages)*

The `Each` instances for tuples (up to 9-tuples) give us homogenous traversal of
tuples, meaning we can use `each` to traverse each element in a tuple whose
values have the same types. If we package it up with `wander`, we get a
convenience function that looks a little like `traverse'`.

```haskell
each' :: (Each s t a b, Traversing p) => p a b -> p s t
each' = wander each

foo :: Arr DataSource X Y
foo = proc x -> do
  ...
  (name1, name2, name3) <- each' getUsername -< (id1, id2, id3)
  ...
```

As you can see, `each'` is letting us call `getUsername` for a discrete set of
IDs. We could have done the same thing "safely" by passing in a list and pattern
matching on the result: `[a,b,c] <- traverse' getUsername -< [x,y,z]`. This is
technically safe because of the `Traversable` laws. But it's deeply
unsatisfying. The `each'` solution gives us a type level guarantee that the
pattern on the left side matches the pattern on the right.

`Each` actually does a pretty good `MonoTraversable` impression. There are
instances of `Each` for traversing `Char`s in `Text`, or `Word`s in
`ByteString`. We can use these to do traversal on mono-traversable things so we
can iterate over them with our arrows and still have those traversals batched.

---

Extra Notes
---

- #### Ahead-of-time performance

  One of the main drawbacks to free monads is that they have a pretty big
  performance cost. If we think of these things as ASTs, free monads build and
  rebuild the AST every time a function is run. This means you need to do
  considerable optimization for them to be practical. There are some great
  resources on this out there ([Reflection Without
  Remorse](http://okmij.org/ftp/Haskell/Reflection.html), or [Free Monads For
  Less 1](http://comonad.com/reader/2011/free-monads-for-less/),
  [2](http://comonad.com/reader/2011/free-monads-for-less-2/), and
  [3](http://comonad.com/reader/2011/free-monads-for-less-3/)). But it remains
  the main problem with free monads.

  I haven't tested this yet, but I hypothesize that the free arrows I used here
  will have a significant performance advantage. Not because building the AST is
  cheaper (it's completely unoptimized in this post), but because the AST only
  needs to be built exactly once. Since you get to convert it to a Kleisli arrow
  ahead of time, you end up being able to just call that prebuilt-arrow.

  Applicatives theoretically also have this advantage, but since applicatives
  take their inputs statically, you end up needing to rebuild them at runtime
  for any change in inputs. With the free arrow, you're more likely to use the
  same static arrow every time you run it, since the changes in inputs usually
  just mean different arguments to the arrow.

- #### Relationship between Arrow and Applicative

  There's some well known correlation between Arrow and Applicative, in that
  every Arrow yields an applicative:

  ```haskell
  instance Arrow p => Applicative (p a) where
    pure a = arr (const a)
    f <*> a = arr (\(f', a') -> f' a') . (f &&& a)
  ```

  There's also an arrow generated for every applicative:

  ```haskell
  newtype AppArr f a b = AppArr { runAppArr :: f (a -> b) }
  ```

  But it's less interesting, because you may notice that it generalizes to this:

  ```haskell
  newtype AppArr p f a b = AppArr { runAppArr :: f (p a b) }
  ```

  Which really just means that Applicatives compose with arrows, not that
  they're as powerful as arrows. Interesting, but less so than if Applicative
  proved equivalent to Arrow. It's not terribly surprising either. `Applicative`
  can compose with pretty much anything made up of static functions like
  `(.)`, `(<>)`, or `(<|>)`. That's why it's the king of static analysis.

- #### Free Category

  There is a standalone free category that doesn't require a profunctor. The
  problem is that it doesn't *preserve* `Profunctor`, meaning you won't get a
  prearrow out of it.

  ```haskell
  data Cat cat a b where
    Id :: Cat cat a a
    Comp :: cat x b -> Cat cat a x -> Cat cat a b

  instance Category (Cat cat) where
    id = Id
    Id . h = h
    Comp f g  . h = Comp f (g . h)
  ```

- #### `(***)` and `(&&&)`

  The `Arrow` class defines these methods so that you can conveniently combine
  two arrows. It's tempting to see them as an opportunity for concurrency. But
  in reality, I'm not sure that's acceptable. Whatever you override them with,
  they're meant to have the same semantics as their default definitions. If you
  use them to do things concurrently, I don't think that counts as the same
  semantics. Furthermore, I was unable to find any `Free` variant that made such
  concurrency possible without *definitely* breaking a whole bunch of laws,
  suggesting at the very least that such concurrency should be handled with
  great care.
