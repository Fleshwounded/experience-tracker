# How to use:

## 1. Understand the format of the experience journal (`exp.txt`)

It looks like this:

```
weed
weed lsd
weed lsd noopept
weed lsd noopept alcohol
weed lsd dmt
````

So basically, every drug you have taken individually is on its own line. For drug combos, you write the drugs on the same line. Notice how this creates hierarchies of unique combos - because `weed lsd noopept` is a different experience from `weed lsd noopept alcohol`. Newlines are ignored.

The main pitfall with this notation is duplicates, because `weed lsd` is the same experience as `lsd weed`. But the experience tracker can help you with this...

## 2. Use the experience tracker (`exp.hs`)

If you run the experience tracker, it should give you this output:

```
STATS ----------------------------------

Drugs:  7
Combos: 18
Total:  25

DUPLICATES -----------------------------

weed lsd
lsd weed

weed lsd noopept
lsd weed noopept

noopept oxiracetam lions-mane
oxiracetam lions-mane noopept

noopept oxiracetam lions-mane weed alcohol
noopept oxiracetam lions-mane weed alcohol
```

It counts all the drugs and combos and the total unique experiences you’ve had. But also, it checks your entire file for duplicates, irrespective of the order in which you have written them.

You put `exp.hs` in any directory and run it from the command line like so:

`runhaskell exp.hs <optional path to an experiences file>`

It should run on any platform, but you will need to have <a href="https://www.haskell.org/ghc/">GHC</a> installed, because this is coded in Haskell.
