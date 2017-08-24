# purescript-bundaegi (WIP?)
...because nobody really wants to eat boiled silkworms (or write Typescript)

A library to generate some Typescript types from Purescript using Generics and RowList

Please let me know if you're interested in improving this :-DDD

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
