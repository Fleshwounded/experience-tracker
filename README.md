## How to use:

### 1. Understand the format of the experience journal (*exp.txt*)

It looks like this:

```
weed
weed lsd
weed lsd noopept
weed lsd noopept alcohol
weed lsd dmt

noopept
noopept oxiracetam
noopept oxiracetam lions-mane
noopept oxiracetam lions-mane weed
noopept oxiracetam lions-mane weed alcohol
````

So basically, every drug you have taken by itself is on its own line. For drug combos, you write multiple drugs on the same line. Notice how this creates hierarchies of unique combos - because `weed lsd` is a different experience from `weed lsd noopept`. Empty lines are ignored.

The main pitfall of this notation is duplicates, because `weed lsd` is the same experience as `lsd weed` (unless we want to get technical). But anyway, that is why the experience tracker has a good duplicate detector...

### 2. Use the experience tracker (*exp.hs*)

If you run the experience tracker, it should give you this output:

```
STATS ==================================

Drugs:  7
Combos: 22
——————————
Total:  29

Longest combo (most drugs at once):
  Length: 5
  Combo:  noopept oxiracetam lions-mane weed alcohol

DUPLICATES =============================

weed
weed

weed lsd
lsd weed

weed lsd noopept
lsd weed noopept

noopept oxiracetam
noopept oxiracetam

noopept oxiracetam lions-mane
oxiracetam lions-mane noopept

noopept oxiracetam lions-mane weed alcohol
noopept oxiracetam lions-mane weed alcohol
```

It counts all the drugs and combos and the total unique experiences you’ve had. Additionally, it shows you the longest combo you have ever done (most drugs at once). But also, it checks your entire file for duplicates, irrespective of the order in which you have written them.

To install it, compile *exp.hs* with: `ghc -o exp exp.hs`. Then put the three binary files in a directory such as your `bin`, and create a shell alias to it, such as: `alias exp='/home/$(echo $USER)/bin/exp/exp'`.

If you don’t specify a path to an experiences file, then it will look for a file called `exp.txt` in the following locations:

* The current directory
* The home directory (platform-agnostic)

Use the `-h` or `--help` flags to view this readme on the command-line.

*exp.hs* should run on any major OS, but it is coded in Haskell so you will need to install <a href="https://www.haskell.org/ghc/">GHC</a> in order to run it using `runhaskell`.

## Legal Disclaimer

This script is for educational use only. It is not to be used for real drugs and its author doesn’t condone doing real drugs.
