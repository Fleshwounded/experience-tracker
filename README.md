# How to use:

## 1. Understand the format of the experience journal (*exp.txt*)

It looks like this:

```
weed
weed lsd
weed lsd noopept
weed lsd noopept alcohol
weed lsd dmt

alcohol
alcohol weed
alcohol lsd
````

So basically, every drug you have taken by itself is on its own line. For drug combos, you write multiple drugs on the same line. Notice how this creates hierarchies of unique combos - because `weed lsd` is a different experience from `weed lsd noopept`. Empty lines are ignored.

The main pitfall of this notation is duplicates, because `weed lsd` is the same experience as `lsd weed` (unless we want to get technical). But anyway, that is why the experience tracker has a good duplicate detector...

## 2. Use the experience tracker (*exp.hs*)

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

You put *exp.hs* in any directory and run it from the command line like so:

`runhaskell exp.hs <optional path to an experiences file>`

If you don’t specify a path to an experiences file, then it will look for a file called `exp.txt` in the following locations:

* The current directory
* The home directory (platform-agnostic)

Use the `-h` or `--help` flags to view this readme on the command-line.

*exp.hs* should run on any major OS, but it is coded in Haskell so you will need to install <a href="https://www.haskell.org/ghc/">GHC</a> in order to run it using `runhaskell`.
