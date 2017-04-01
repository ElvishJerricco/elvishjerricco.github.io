---
layout: post
title:  "Nix Style Configs in Haskell"
date:   2017-04-01
categories:
---

One of the unique things about Nix is its extensible configuration
system. Extensible configs allow you to override their fields in a way
that will update all the other fields that depend on what you
override. In Nix, this is particularly useful for package
management. It allows you to override a package (for example; change
its version), and all the packages that depend on that package will
subsequently use your overridden version.

We can represent this in Haskell, since it's a lazy language. This is
based on chains of functions that each take the previous one's output
as an argument, typically called `super`. By overriding a field in
`super`, you get to change that field for every function that comes
after you.

```haskell
data MyConfig = MyConfig
  { _a :: Int
  , _b :: Int
  }

makeLenses ''MyConfig

initial :: MyConfig
initial = MyConfig { _a = 1, _b = 2 }

overrideB :: MyConfig -> MyConfig
overrideB = b .~ 3

overrideA :: MyConfig -> MyConfig
overrideA super = super & a .~ (super ^. b)

final :: MyConfig
final = (overrideA . overrideB) initial -- final = MyConfig { _a = 3, _b = 3 }
```

This forms a pretty convenient monoid called `Endo`. This monoid is
just the monoid of endomorphism composition. That is, it just composes
functions with the same argument and return types.

```haskell
import Data.Monoid (Monoid (..), (<>))

newtype Endo a = Endo { appEndo :: a -> a }

instance Monoid (Endo a) where
  mempty = Endo id
  Endo f `mappend` Endo g = Endo (f . g)

--------------------------------------------------------------------------------

overrideB :: Endo MyConfig
overrideB = Endo (b .~ 3)

overrideA :: Endo MyConfig
overrideA = Endo $ \super -> super & a .~ (super ^. b)

final :: MyConfig
final = appEndo (overrideA <> overrideB) initial
```

So an extensible config is an `Endo` monoid that you can compose with
`mappend` or `(<>)`. But this only causes updates to the following
overrides. Previous overrides will still see old records, which makes
the config incoherent. You need to give the final version of the
record (typically called `self`) to all overrides so they can use
coherent records whenever possible. In a lazy language, this is easy;
you can recursively give the final thunk to all overrides without
having to evaluate those overrides first. An easy way to do this is to
extend `Endo` to monadic endomorphisms, and use the `Reader` monad.

```haskell
import Control.Monad ((<=<))
import Control.Monad.Reader
import Data.Functor.Identity
import Data.Function (fix)

newtype EndoM m a = EndoM { appEndoM :: a -> m a }

instance Monad m => Monoid (EndoM m a) where
  mempty = EndoM return
  EndoM f `mappend` EndoM g = EndoM (f <=< g)

type Endo = EndoM Identity

type Configurable a = EndoM (Reader a) a

configure :: Configurable a -> a
configure (EndoM f) = fix (\self -> runReaderT (f self) self)

-- | Lens convenience
overriding :: Setter' (EndoM m s) s
overriding = sets $ \f (EndoM g) -> EndoM (g . f)

--------------------------------------------------------------------------------

-- This one serves as a bootstrap, allowing the constructor to
-- evaluate to WHNF. Whenever you don't have initial values for
-- fields, you can set the bootstrap to `EndoM $ \(~MyConfig {..}) ->
-- MyConfig {..}`, using lazy matching and `RecordWildCards` to
-- boostrap the thunk with nonterminating fields.
initial :: Configurable MyConfig
initial = EndoM $ \_ -> return MyConfig { _a = 1 , _b = 2 }

overrideA :: Configurable MyConfig
overrideA = EndoM $ \super -> do
  self <- ask
  return $ super & a .~ (self ^. b)

overrideB :: Configurable MyConfig
overrideB = mempty & overriding . b .~ 3

final :: MyConfig
final = configure (overrideB <> overrideA <> initial) -- MyConfig { _a = 3, _b = 3 }
```

Now, even though `overrideA` comes earlier than `overrideB` in the
chain (remember, function composition, and therefore `EndoM`'s monoid
instance, reads right to left), `a` still gets set to the value that's
defined for `b` later in the chain.

There's also no reason that `self` and `super` have to have the same
type. You can convert `super` into `self` at the end of the chain to
get whatever finalization you need (even at the type level).

```haskell
import Control.MonadFix (mfix)

type Configurable self super = EndoM (Reader self) super

configure :: (super -> self) -> Configurable self super -> self
configure f (EndoM g) = fix (f . runReader (mfix g))
```

And finally, this can of course work for any `MonadFix`, meaning your
configuration steps can be monadic.

```haskell
configureM :: MonadFix m => (super -> m self) -> EndoM (ReaderT self m) super -> m self
configureM f (EndoM g) = mfix (runReaderT (lift . f =<< mfix g))
```
