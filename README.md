# Learning Me a Haskell 🎩✨

It's about time I figured out what all the fuss is about. I'll keep all my
findings in this repo.

## [OneOf](/src/OneOf/Types.hs#L30-L32)

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

h :: OneOf '[String, Bool, Int]
h = inject (3 :: Int) -- 3 is too polymorphic without annotation.
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
