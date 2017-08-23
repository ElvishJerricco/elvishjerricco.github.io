---
layout: post
title:  "MonadFix is Time Travel"
date:   2017-08-22
categories:
---

`MonadFix` is a pretty difficult concept, and I personally found much
of the existing content about it online to be somewhat
unhelpful. [I answered](https://www.reddit.com/r/haskell/comments/6uoogn/weekly_beginner_saturday_ask_anything_0/dlusone/?context=3&st=j6lgtvk6&sh=da213088)
a request for an ELI5 of `MonadFix` on Reddit, and someone suggested I
turn my answer into a blog post. So here it is, edited with more
content and detail.

First, let's lay some groundwork. At the core of all of this is the
`fix` function, which simply uses Haskell's laziness to do recursion.

```haskell
fix :: (a -> a) -> a
fix f = let x = f x in x
```

Now, this is a little hard to grok at first, so I'll just
[link an article by Matt Parsons](http://www.parsonsmatt.org/2016/10/26/grokking_fix.html)
that does a good job explaining it, rather than explaining it here
myself.

Know how to use `fix` now? Good, let's move on. That article only
really describes how to write recursive *functions* with `fix`. It's
important to know how to write other recursive structures. For
instance, here is the infinite list `[1..]` defined in terms of `fix`:

```haskell
[1..] = fix (\xs -> 1 : fmap (+1) xs)
```

If `fix f` is the infinite application of `f` to itself (i.e. `f (f (f
(...)))`), then this fixed point is just `1 : fmap (+1) (1 : fmap (+1)
(1 : ...))`. We know the first element is `1`, we know that the second
element is `(+1) 1`, we know that the third element is `(+1) ((+1)
1)`, and so on. As another example, you can encode the famous
recursive fibonacci definition using `fix`:

```haskell
fibonacci :: [Integer]
fibonacci = 0 : 1 : zipWith (+) fibonacci (tail fibonacci)
-- With fix:
fibonacci = fix $ \xs -> 0 : 1 : zipWith (+) xs (tail xs)
```

You'll notice that it's identical, except that we've replaced the
recursive usage of `fibonacci` with the argument that `fix` gives
us. It's exactly the same definition, but `fix` is the one doing the
recursion. But data structures defined via `fix` don't have to be
infinite. For instance:

    > fix $ \(~(a, b, c)) -> ([1, c-5], head a + 2, b * 4)
	([1,7],3,12)

*(The `~` means to pattern match lazily, so that we're not strict in
 our argument, which would blow up `fix`.)*

Each of the elements in this tuple is defined in terms of another one
of the elements. By using fix, we are gaining access to the future
view of the tuple, and using the constant part `1:[_]` to break the
recursion. The equivalent `let` bound recursive definition would be
this:

```haskell
let a = [1, c-5]
    b = head a + 2
    c = b * 4
```

Since `b` only cares about the constant `1` in `a`, it will not
trigger an infinite loop by trying to evaluate `c`. This brings up a
couple of important concepts that should be described before we move
on to `MonadFix`.

1. `fix` is like time travel. In the function you give it, you have a
   lazy view of what you will eventually return. You get to "see the
   future" and put it somewhere in your result. But with time travel
   comes paradoxes. You can't ask the future for any information that
   would let you change the future, because then the universe is in an
   inconsistent state; you have memory of a time that does not exist
   in the timeline. Similarly, functions passed to `fix` are not
   allowed to ask their views of the future for any information that
   would alter the calculation; i.e. the function must not be strict
   in its argument. Which brings me to my next point.
2. Functions that you give to `fix` should be treated more like values
   than like functions. Since you know that the function will never
   ask its argument for any information, you know that the structure
   of its output is effectively constant. The function returns a
   constant shape, and the content has some holes where the recursive
   parts go. We can say that **if** ``f undefined `seq` ()``
   terminates without error, **then** so will ``fix f `seq` ()`` and
   vice versa. Thus, we can use `f undefined` to freely determine some
   information about the "shape" of `f`, as long as we're careful not
   to evaluate the wrong parts of the structure.

With that, I think we're ready to move on to `MonadFix`.

MonadFix
---

```haskell
class Monad m => MonadFix m where
  mfix :: (a -> m a) -> m a
```

`MonadFix` is just a monadic version of `fix`, and the same principles
apply. It's like time travel; the thunk passed to the function is from
the future, representing the value returned at the end of the
`mfix`. And a function passed to `mfix` will always yield the same
monadic action parameterized only by a thunk it cannot inspect, lest
it risks a temporal paradox.

A few of you may have noticed that there's a definition for `mfix`
that we could use for free.

```haskell
mfix f = fix (>>= f)
```

The problem here is that since `fix` cannot take functions that are
strict in their argument, it doesn't work for instances of `(>>=)`
that are strict. It would expand to `((...) >>= f) >>= f`, and you can
imagine how that wouldn't work for many monads. `Maybe`, for instance:

```haskell
mfix' f = fix (>>= f)

instance Monad Maybe where
  return = Just
  Nothing >>= _ = Nothing
  Just a >>= k = k a

ghci> mfix' $ \_ > Just 1
<hangs forever>
```

It will just keep iterating back, trying to make sure that none of the
binds in the infinite chain are ever given a `Nothing`. It will,
however, work for lazy monads, like `Control.Monad.State.Lazy`. But
these seem to be the exception rather than the rule. You can think of
this definition as running a monadic action infinitely many times,
starting with the one farthest in the future. This obviously won't
work for, say, `IO`. Instead, we want something that will run the
monadic action exactly once, but yield its result back to the
past. This parallels the `let`-based definition of `fix`. The reason
`fix` is defined the way it is, as opposed to `fix f = f (fix f)`, is
because the latter would actually spend time calling `f` multiple
times, whereas the `let` based definition calls `f` just once,
threading a thunk lazily through. Of course since `fix` is pure, the
difference is unobservable without something like `unsafePerformIO`,
but this difference is highly important in the side effecting world of
monads.

So a class that allows each individual monad to choose for itself how
to be lazy enough is necessary. Surprisingly, I think `IO` has one of
the easiest `MonadFix` instances to understand, but it's based on
`unsafeInterleaveIO`, which is the basic primitive for lazy
IO. Essentially, `unsafeInterleaveIO someIOAction` will return a new
IO action that will not do any IO when run. Instead, it returns a lazy
thunk, and when that thunk is evaluated, *then* it will execute
`someIOAction` and return that. Now, as far as I know,
`unsafeInterleaveIO` is not unsafe in the same way as
`unsafePerformIO`, in that I don't believe it violates any of the laws
of pure functional programming or any monad laws or whatever else. I'm
pretty sure it's only considered "unsafe" because it means IO will run
at times that are much harder to predict. It's "safe" because you can
pretend that it just schedules `someIOAction` to run at some later
time, and then predicts the future about what it will return. There's
nothing saying `IO` can't time travel! And by now, it should be clear
that time travel is an asset to fixed points, so it's no surprised
that `unsafeInterleaveIO` is involved in `mfix`.

*(Also,
[David Feuer recommends](https://www.reddit.com/r/haskell/comments/6uoogn/weekly_beginner_saturday_ask_anything_0/dlwa8ch/)
that everyone read
[Dan Doel's blog post](http://comonad.com/reader/2015/on-the-unsafety-of-interleaved-io/)
about the safety of `unsafeInterleaveIO`.)*

Anyway, with that groundwork laid, here's the `MonadFix` instance for
`IO`

```haskell
instance MonadFix IO where
  mfix f = do
    var <- newEmptyMVar                       -- 1
    ans <- unsafeInterleaveIO $ takeMVar var  -- 2
    result <- f ans                           -- 3
    putMVar var result                        -- 4
    return result                             -- 5
```

Let's walk through this line by line in terms of our little time
travel analogy:

1. Create an empty mutable variable.
2. Predict the future value that will be contained in that mutable
   variable.
3. Call the function `f` with the predicted future value.
4. Store the result of `f` in the variable, thus fulfilling the
   prophecy as required by line 2.
5. Return that result.

In short, `f` will be called with the prediction of what `f` will
return, just as we would expect given our understanding of `fix`. If
`f` attempts to evaluate the thunk that it was given, then it will
create a temporal paradox; i.e. it will forcefully execute `takeMVar
var` when `var` has not yet had a result put into it. For the most
part, this is usually only useful when you want to work with lazily
cyclic structures. Like when you need to cough up a thunk to an IO
action so that it can do some IO and create a data structure with that
thunk placed somewhere inside, but you know that it doesn't actually
need to force that
thunk. [The wiki has some decent examples](https://wiki.haskell.org/MonadFix).

```haskell
import Control.Monad.Fix
import Data.IORef

data Node = Node Int (IORef Node)

mknode = mfix $ \p -> do
  p' <- newIORef (Node 0 p)
  putStrLn "node created"
  return p'
```

The most compelling example I've found has been Reflex, the FRP
library. The most common reason `MonadFix` is needed is that you want
to define to widgets in a certain order, but you need their logic to
go in the opposite order.

```haskell
fmap snd $ mfix $ \(~(e', _)) -> do
  text <- textBoxWithClear e'
  e <- button
  return (e, text)
```

The `button` action returns an `Event` that we want to use to clear
the text box, but we want the text box to come before the button in
the layout. Now, we could have solved this by making text boxes
totally mutable, and allowing the clear event to be changed at any
time, but that's not nearly as nice as the pseudo-pure approach we can
take by relying on `mfix`, which requires no mutation at all (as far
as we can see, that is; remember, there is an `MVar` in the `mfix`
instance driving this whole thing).

So now let's try defining a `MonadFix` instance ourselves. `Maybe` is
an easy one. It will have the signature `mfix :: (a -> Maybe a) ->
Maybe a`. The time travel analogy still works well here of course. We
are given a function, and we have to call that function with a
prediction of what it would return. The problem is: What if it returns
`Nothing`?  Well, we're in luck. Since we've already mandated that
it's a paradox when the function is strict in its argument, we can
just pass in a thunk that would error in the event that `f` returns
`Nothing.` Since `Nothing` doesn't contain any values, we can be
certain that it can't leak the false prediction. It's basically safe
for us to unilaterally predict that `f` will return `Just`, since it
doesn't matter when that prediction is wrong. So at this point, the
function begins to sound a little bit like an `a -> a` rather than an
`a -> Maybe a`, which should remind you of regular `fix`.

```haskell
instance MonadFix Maybe where
  mfix f = let a = f (fromJust a) in a
  -- Or:
  mfix f = fix (f . fromJust)
```

With this instance, as long as `f` is not strict in its argument (a
requirement for fixed points anyway), it can just return `Nothing`,
and we don't care, or it can return `Just`, and we'll happily provide
it with that result. This stuff about the `Nothing` case is just a
variation on the point I made earlier. The function is partially
constant, and we're just exploiting knowledge about the constant parts
to figure out what to pass back in time.

The instance for lists is similar, but more complicated.

```haskell
instance MonadFix [] where
  mfix f = case fix (f . head) of
    []    -> []
    (x:_) -> x : mfix (tail . f)
```

We're relying on the same idea, but taking it a bit further. We can
take for granted that `f` won't strictly use its argument, so we can
use `head` to give it whatever it returns because the structure is
constant. Either there will be a head, or there will be an empty
list which can't contain our false prediction. But we can't call `f`
with just the head element. Calling it with only one of its elements
would be arbitrary and weird (and I bet it would violate the
`MonadFix` laws, which I won't get into). So we pattern match on it,
and when it has a head element, we use that. But then we call `mfix`
recursively with `tail . f`. We can do this because we already know
that `f` is going to return a list with a head, so this time we're
going to have the `fix (f . head)` return the *next* element in the
list.

This turns out giving the list monad some weird, but cool
behavior. Normally, you would expect each bind to sort of introduce a
loop. We iterate over the elements being bound out, and for each of
them, we do all the statements that comes after it with that element
bound immutably to a variable. But with `mfix`, it's starts to look
almost mutable:

```haskell
mfix $ \b' -> do
  a <- [1, 2, snd b']
  b <- [3, 4]
  return (a, b)

> [(1, 3), (1, 4), (2, 3), (2, 4), (3, 3), (4, 4)]
```

We start with `1`, and iterate over `[3, 4]`, giving us the sublist
`[(1, 3), (1, 4)]`. Then we do the same thing with `2`. And finally,
we try to do the same thing with `snd b'`, except that `b'` depends on
the statement that comes next because of `mfix`. Since we were lazy
enough, this works out, and we end up with the `a` thunk being the
same as the `b` thunk, which means that on each iteration of `[3, 4]`,
we're changing what `a` is bound to. That's how those last two
elements of the list ended up with changing `fst` values, rather than
constant.

Yea, `MonadFix` is weird.

RecursiveDo
---

The last thing I'll talk about is the `RecursiveDo` language
extension, which adds awesome syntax for using `mfix` more
easily. Here's how the last example with the list monad would look
with `RecursiveDo`.

```haskell
{-# LANGUAGE RecursiveDo #-}

foo = do
  rec a <- [1, 2, b]
      b <- [3, 4]
  return (a, b)
```

Using this syntax, we can just actually refer to a variable bound
downstream, and `RecursiveDo` will handle using `mfix` for us. There
are two different syntaxes available though (for some reason). Here,
I've shown the `rec` syntax. But you can also just replace `do` with
`mdo` and write the code without any `rec` statements.

```haskell
foo = mdo
  a <- [1, 2, b]
  b <- [3, 4]
  return (a, b)
```

I believe the difference is mainly in how many `mfix`es are made, and
where they're put. If I understand correctly, `mdo` will always find
the smallest `mfix`es possible, regardless of how many that
means. Whereas one `rec` always results in one `mfix` around the lines
that it covers.

With this, more people might be able to understand my hastily written
post about
[Nix-style configuration](/2017/04/01/nix-style-configs-in-haskell.html). Recursion
is a useful tool, and laziness is recursion's friend!
