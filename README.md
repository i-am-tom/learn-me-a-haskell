# Learning Me a Haskell 🎩✨

It's about time I figured out what all the fuss is about. I'll keep all my
findings in this repo.

Although this repo mainly exists for my benefit, I've tried to document
*everything* thoroughly, so I hope it's useful to others. If you have any
questions or suggestions on how to improve the docs, please [send me a
tweet](https://www.twitter.com/am_i_tom) or a PR and we'll improve things :)

## Setup

```bash
$ # Build the project with haddocks
$ cabal new-build
$ cabal new-haddock

$ # Run the tests - they're in the docs!
$ cabal new-run test
```

## Table of Contents

- [OneOf](#oneof)
- [HList](#hlist)
- [HTree](#htree)
- [Bag](#bag)

## [OneOf](/src/OneOf/Types.hs#L38-L40)

### Intro

`OneOf` is a generalised version of `Either` (I think sometimes known as a
`Variant`?). While `Either` is strictly for one of two possibilities, `OneOf`
generalises this to any (non-zero) number of possibilities with a type-level
list. For example:

```haskell
f :: OneOf '[String]            -- `String`
g :: OneOf '[String, Bool]      -- `Either String Bool`
h :: OneOf '[String, Bool, Int] -- `Either String (Either Bool Int)`
```

Rather than using `Left` and `Right`, we construct these values with some
number of `There`s followed by a `Here`. For example:

```haskell
f :: OneOf '[String]
f = Here "Hello!"

g :: OneOf '[String, Bool]
g = There (Here True)

h :: OneOf '[String, Bool, Int]
h = There (There (Here 3))
```

### Injection

The above is quite... ugly, though, right? What we'd really like is a neat way
to "lift" a type into the `OneOf` without having to worry about the number of
`There`s we need. Luckily, the `inject` function gives us just that:

```haskell
f :: OneOf '[String]
f = inject "Hello"

g :: OneOf '[String, Bool]
g = inject True

h :: OneOf '[String, Bool, Integer]
h = inject 3
```

`inject` looks through the list for the first occurrence of our type, and
produces the constructors required to lift our value into the `OneOf`.

### Projection

Cool, we have a type that could hold a value that is one of any number of
types... how do we _use_ it? Well, we could pattern-match, but then we're back
to worrying about everything `Here` and `There`. To help with this, the `fold`
function will generate a Church-style fold for any `OneOf` value:

```haskell
f :: OneOf '[String]
  -> (String -> result)
  -> result
f = fold

g :: OneOf '[String, Bool]
  -> (String -> result)
  -> (Bool   -> result)
  -> result
g = fold

h :: OneOf '[String, Bool, Int]
  -> (String -> result)
  -> (Bool   -> result)
  -> (Int    -> result)
  -> result
h = fold
```

The type is calculated based on the type of the `OneOf` supplied as a first
argument, and the pattern is always the same: _give me a function from each
type to some `r`, and I'll just call the appropriate one._

Alternatively, we can fold a `OneOf` using a constraint, assuming that all
values have an instance:

```haskell
h :: OneOf '[String, Bool, Int] -> String
h = interpret @Show show
```

Here, we use the fact that `String`, `Bool`, and `Int` all have `Show`
instances, and thus have an instance for the `show` function that returns a
`String`. Now, we'll just get the `show` result for whatever is in our value.

## [HList](/src/HList/Types.hs#L43-L45)

### Intro

`HList` is a generalised version of `Tuple` which allows you to carry lists of
values with *possibly-different* types. It's notionally equivalent to tuples:

| Tuple              | HList                 |
| ------------------:|:--------------------- |
| `()`               | `HList '[]`           |
| `(a)`              | `HList '[a]`          |
| `(a, b)`           | `HList '[a, b]`       |
| `(a, (b, c))`      | `HList '[a, b, c]`    |
| `(a, (b, (c, d)))` | `HList '[a, b, c, d]` |

_Of course, `(a)` is just `a` - a one-element tuple _is_ its type._ The H in
`HList` stands for *heterogenous*, as distinct from a _homogeneous_ list where
all elements must be the same type. Similarly to a list, we construct them with
cons and nil constructors, imaginatively named `HCons` and `HNil`:

```haskell
f :: HList '[]
f = HNil

g :: HList '[Bool]
g = HCons True HNil

h :: HList '[String, Bool]
h = HCons "hello" (HCons True HNil)

-- etc.
```

### Construction

As we noted with `OneOf`, this is still rather ugly, and we can use some
type-level trickery to tidy this up. `build` is a function that builds an HList
by taking all its values as parameters in the order they appear (using a
healthy dose of typeclass magic).

```haskell
f :: HList '[]
f = build

g :: Int -> HList '[Int]
g = build

h :: String -> Int -> HList '[String, Int]
h = build
```

_This involves some fun tricks behind the scenes that I've documented in [the
`HList.build` file](src/HList/Build.hs)._

### Updating

Now we have our glorious `HList`, can we change the values – or even _types_ –
within it? You bet!

The `update` function takes a function and an `HList`, and applies that
function to an index that we specify with a *type application*. For example:

```haskell
xs :: HList '[Integer, Double, Bool]
xs = HCons 2 (HCons 2.0 (HCons True HNil))

-- HCons 2 (HCons 2.0 (HCons False (HNil)))
f = update @2 not xs

-- HCons 2 (HCons "hello" (HCons True (HNil)))
g = update @1 (\_ -> "hello") xs

-- ... • 4 is out of bounds for '[Integer, Double, Bool]!
-- ...   We'll need at least 0, and at most 2.
h = update @4 show xs

-- • You can't apply [Char] -> [Char] to the Double at index #1.
--   I'm sorry, captain; I just won't do it.
i = update @1 (++ "!") xs -- Type mismatch!
```

We can see here that our index is updated when the types align, and we
otherwise get some nice custom type errors! _I'd like to add more type errors
here, but I'm at the stage where I have to wrestle a bit with incoherence)_.

### Projection

We have an `HList`, and we can pattern-match as we want, but... can we _fold_
an `HList`? Well, similarly to `OneOf`, we can have a constrained fold:

```haskell
fold
  :: Monoid monoid
  => (forall element. constraint element => element -> monoid)
  -> HList list
  -> monoid
```

If all our `element`s satisfy some `constraint`, we can use this `fold` method
to combine them all under some monoid. For example, `fold @Show show` will take
the string representation of every element of any `HList` and concat the
results together, as long as all the types have a `Show` instance.

Another fun consequence of this generalisation is that we can recover
homogeneous operations like `foldMap` by using a constraint like `((~)
element)` (every element of the list must be equal to some type `element`). In
fact, that's exactly how `foldMap` is implemented within this library!

## [HTree](/src/HTree/Types.hs#L66-L78)

### Intro

`HList` is great and all, but it's not the most efficient structure when our
list grows and access is random. GHC does no caching and **no optimisation**
with list lookups at the type-level, so linear access becomes _very_ expensive.
If this is our use case, we could consider a different structure, such as a
tree!

An `HTree` is exactly this: a heterogeneous tree, as opposed to a list.
Specifically, it's a **binary** tree indexed by a **red-black** tree of types,
which we use to keep the tree (roughly) balanced. A serious hat-tip is due to
**Chris Okasaki** for his **Red-Black Trees in a Functional Setting** paper,
which I used for the implementation of `insert` and `delete`.

### Construction

So, this all sounds well and good, but how do we construct one? We need a way
of ordering types, which we achieve through use of the `Generic` class – we
order types by their names – and the `insert` function:

```haskell
newtype Name
  = Name String
  deriving (Generic, Show)

newtype Age
  = Age Int
  deriving (Generic, Show)

example :: HTree ('Node 'Black ('Node 'Empty Age 'Empty) Name 'Empty)
example
  = insert (Age 25)
  . insert (Name "Tom")
  $ empty
```

`insert` gives us a way to build a tree of types, providing that the types are
`Generic`. However, the type of an `HTree` can quickly become ugly, so you
might want to stick to polymorphic approaches:

```haskell
addSomeTom :: (Insert Name i m, Insert Age m o) => HTree i -> HTree o
addSomeTom = insert (Age 25) . insert (Name "Tom")
```

Rather than talking about what a tree _is_, we can use the `Insert` typeclass
to talk about its state _before_ and _after_ inserting a variable, and let GHC
worry about the full type.

### Deletion

Deletion is as you'd imagine: we use type application to specify the type we
want to delete, and everything else works as `insert` did:

```haskell
-- (.)<-(Age 25)->(.)
demo :: HTree ('Node 'Black 'Empty Age 'Empty)
demo = delete @Name example
```

... and we can use the constraints to deal with a tree polymorphically:

```haskell
anonymise :: Delete Name input output => HTree input -> HTree output
anonymise = delete @Name
```

Note that deleting a type from a tree is a *no-op* if the tree doesn't contain
the type. It is *not* a type error.

### Access

Of course, the last interesting function on an `HTree` is this access we keep
talking about. This is provided by the `getType` function within the `HasType`
class:

```haskell
test :: HasType Name input => HTree input -> Name
test = getType

name = test example -- Name "Tom"
```

All polymorphic, all *beautiful*. Naturally, GHC will help you if you go
looking for a type that isn't in the tree:

```haskell
-- ... I couldn't find any Bool in this tree...
-- ... If it helps, here's what I did find:
-- ... - Age
-- ... - Name
d'oh = getType @Bool example

-- ... You won't find any Bool here!
-- ... Your tree is empty; there's nothing to access!
oops = getType @Bool empty
```

## [Bag](/src/Bag/Types.hs#L99-L100)

_The idea for this came from a talk by Will Jones, our VP Engineering at
[Habito](habito.com), on [Deriving
Strategies](https://www.youtube.com/watch?v=U0j9iIKOj40). Always hiring, etc!_

### Intro

Let's imagine you're building a **web app** that involves a lot of forms:

```haskell
data Account
  = Account
      { email    :: Email
      , password :: Password
      }

data Profile
  = Profile
      { name :: Name
      , age  :: Age
      }

-- ...
```

These forms may be completed in various orders, and to various degrees, which
leaves us with a _lot_ of potentially-partial data. To accommodate this, we
have to make all these fields optional:

```haskell
data PartialAccount
  = Account
      { email    :: Maybe Email
      , password :: Maybe Password
      }

-- ...
```

We no longer have a compile-time way of ensuring we have all the data in this
type, so we have to attempt to build the original types at run-time, `Maybe`
succeeding, maybe failing. As we add more forms to our app, so too do we add
more partial types.

The `Bag` type, however, generalises this notion: it `Maybe` contains **any**
type! Rather than specify near-verbatim copies of every type, we can now just
use `Bag` to collect all the data we need. For example:

```haskell
newtype Name = Name String deriving Generic
newtype Age  = Age  Int    deriving Generic

userData :: Bag'
userData
  = insert (Name "Tom")
  $ insert (Age  25   )

  -- Under the hood, a `Bag` type is really just a newtype around `HashMap`,
  -- which means it's automagically a `Semigroup` and a `Monoid`! As a result,
  -- `mempty` here means "an empty bag".
  $ mempty

-- ... Another place at another time ...

userName :: Name
userName = lookup userData
```

As type inference relies on the **return type** of the `lookup` function, it's
certainly encouraged to use the `TypeApplications` feature to improve
readability (and inference, in more ambiguous cases):

```haskell
f = do
  name <- lookup @Name userData
  -- etc...
```

## Constrained bags

We've so far only looked at `Bag'`, which is a specialisation of the `Bag` type
that enforces no constraints on the contents. The `Bag` type allows us to
specify constraint constructors that hold for any member of the bag. For
example:

```haskell
-- Doesn't have a `Show` instance!
newtype Name = Name String

-- ... No instance for (Show Name) arising from a use of ‘insert’
test :: Bag '[Show]
test = insert (Name "Tom") mempty

-- Does have a show instance!
newtype Age = Age 25 deriving Show

success :: Bag '[Show]
success = insert (Age 25) mempty
```

You can also use these constructors to write instances for the entire bag
(using some [QuantifiedConstraints](/src/Bag/QuantifiedInstances.hs#L92)
magic) - this library provides instances for `Eq` and `Show`. In essence, if
the bag's constraints are enough to imply a `Show` instance, we can write a
`Show` instance for the baga (similarly for `Eq`):

```haskell
newtype Name = Name String deriving (Show, Eq)
newtype Age  = Age  Int    deriving (Show, Eq)

demo :: Bag '[Show, Eq]
demo = insert (Name "Tom") $ insert (Age 25) $ mempty

-- "Bag (fromList [(Age,Age 25),(Name,Name \"Tom\")])"
showable :: String
showable = show demo

-- False
eqable :: Bool
eqable = demo == mempty
```

## Type hydration

If you have a type whose fields are all potentially in a `Bag`, you can use the
`populate` function to attempt to construct the type:

```haskell
data Person
  = Person
      { name :: Name
      , age  :: Age
      }
  deriving (Show, Generic)

-- Sucess (Person {name = Name "Tom", age = Age 25})
yay = populate @Person demo

-- Failure ["Name","Age"]
boo = populate @Person (mempty :: Bag')
```

If every field in the type's record or product can be found in the `Bag`, the
constructed value will be returned. Otherwise, we get a list of the names of
the types that were missing. There's unfortunately not much more you can do at
this stage, but it may be useful information for logging or debugging!

## Bag hydration

The opposite control is to take a record/product type and use it to populate a
`Bag`. For this, we have the `include` function:

```haskell
myBag :: Bag'
myBag = mempty

test :: Bag '[Show]
test = include (Person (Name "Tom") (Age 25)) mempty

x = show test
-- Bag (fromList [(Name,Name "Tom"),(Age,Age 25)])
```

Every time an inner type is encountered, it will attempt to insert this type
into the `Bag`. Note that this means **all types** inside your product must
implement **all constraints** on the `Bag`.
