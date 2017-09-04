# purescript-bundaegi

[![Build Status](https://travis-ci.org/justinwoo/purescript-bundaegi.svg?branch=master)](https://travis-ci.org/justinwoo/purescript-bundaegi)

...because nobody really wants to eat boiled silkworms (or write Typescript)

![](http://i.imgur.com/q7tyceP.png)

A library to generate some Typescript types from Purescript.

The primary purpose of this library is to allow you to write more correct models in Purescript and then have Typescript type signatures for use in your project, and io-ts schemas generated for doing validation on external data (e.g. ajax).

If you want to integrate a Typescript and Purescript, you might be more interested in this project: https://github.com/justinwoo/purescript-ohyes

Please feel free to ask me questions [@jusrin00](https://twitter.com/jusrin00) if you're interested in using this library and need some more information.

## Example

Setup:

```hs
data Fruits
  = Watermelon
  | Grapes Number
  | Banana { color :: String, count :: Number }
derive instance genericFruits :: Generic Fruits _
instance fruitsHasTSRep :: HasTSRep Fruits where
  toTSRep = genericToTSRep
instance fruitsHasIOTSRep :: HasIOTSRep Fruits where
  toIOTSRep = genericToIOTSRep

main = do
  log' $ getTSRep "Fruits" (Proxy :: Proxy Fruits)
  log' $ getTSRep "Thingy" (Proxy :: Proxy {a :: Number, b :: Boolean, c :: {d :: String}})
  log' $ getIOTSRep "Fruits" (Proxy :: Proxy Fruits)
  where
    log' = log <<< format defaultOptions
```

Output:

```ts
type Fruits =
  | { tag: "Watermelon" }
  | { tag: "Grapes", content: number }
  | { tag: "Banana", content: { color: string, count: number } };

type Thingy = { a: number, b: boolean, c: { d: string } };

export const iFruits = t.union([
  t["interface"]({ tag: t.literal("Watermelon") }),
  t["interface"]({ tag: t.literal("Grapes"), content: t.number }),
  t["interface"]({
    tag: t.literal("Banana"),
    content: t["interface"]({ color: t.string, count: t.number })
  })
]);
```
