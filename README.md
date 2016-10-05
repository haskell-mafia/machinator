# Machinator

> v.tr. To devise (a scheme or plot). v.intr. To scheme or
> plot. [Latin māchinārī, māchināt-, to design, contrive, from māchina,
> device; see machine.]

Machinator is a data description language in the spirit of
[MaxML](https://github.com/mxswd/maxml/blob/master/Data/MaxML.hs).

We want to specify very simple types that would be idiomatic in
Haskell or Purescript. From this specification, we should get:

- Idiomatic types in Haskell and Purescript
- A human-friendly JSON encoding
- JSON encoders/decoders for Haskell and Purescript
- Exhaustive generators (exhaustive in each shape)
- Randomised generators (QuickCheck/Strongcheck style)
- The ability to create other such backends

We also want a surface syntax, accessible to non-Haskellers, that can
be published, versioned, and statically analysed. This is the main
motivation to build a DSL, rather than embedding directly in Haskell.
We have ways to write types and generic functions, but no nice way to
treat them as data or to decouple them from a Haskell module.

The [Projector](https://github.com/ambiata/projector) language will
support the universe of Machinator types.

## Types

Names subject to bikeshedding.

- Text
- Boolean
- Int32
- Double
- Maybe
- List/Array
- Sums
- Records (Purescript-style, with overloadable field names)

Contentious exclusions for now: bytestrings, proper
products/tuples, Either, Map, type aliased records, extensible
records.

## Syntax mockup

Wasn't kidding about the MaxML. Suggestions welcome.

```haskell
-- machinator v1

data Foo
  = FooV1 Bar Baz Text
  | FooV2 Integer Double (Maybe Baz)
  | FooV3 [Baz]

data Baz = Baz {
    name :: Text
  , age :: Int
  }

data Bar = Bar {
    quuz :: Boolean
  }
```

Backends will be functions over such declarations. You should be able
to use only the backends you need, and in a flexible manner (as
codegen, as a library via TH, etc)

## Alternatives

Most IDLs target embedded systems or Java. We have decidedly fewer
constraints; we just need an expressive common subset of Haskell and
Purescript that is easy to encode in Projector's type system. Anything
else is a bonus.

- [Cauterize](https://github.com/cauterize-tools/cauterize) is one of
  the better IDLs, supporting most of the features we want. Since it
  targets embedded, it is not possible to have an arbitrary-length
  string or array, and we do not suffer this constraint.
- Protobuf has a set of assumptions reflecting its Java and Go
  targets, e.g. every field is nullable, no sums. Existing Haskell
  libs are shallow embeddings. It could be made to fit, probably, but
  would not generate idiomatic Haskell.

## Conceptual reviewers

- Tim H
- Jacob
- Charles
