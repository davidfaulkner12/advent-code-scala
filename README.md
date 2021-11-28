# Advent of Code 2021

This year I'm doing Advent of Code 2021 in Scala!

## Usage

### Command Line

I've used [Mill](https://github.com/com-lihaoyi/mill) to build this project.

You can run an individual problem by just running:

```bash
mill aoc2021 0 1 data/day0.txt
```

You can run all tests with:

```bash
mill aoc2021.test
```

Or you can run the tests for an individual day with:

```bash
mill aoc2021.test aoc2021.days.Day0Tests
```

Realistically you can save a LOT of time by running in the Mill repl (not the Ammonite repl, see below):

```bash
$ mill --repl
@ aoc2021.run("0", "1", "data/day0.txt")()
2
@ aoc2021.test.testOnly("aoc2021.days.Day0Tests")()
-------------------------------- Running Tests --------------------------------
+ aoc2021.days.Day0Tests.testParser 109ms
+ aoc2021.days.Day0Tests.testExample 14ms
res2: ...

```

As I solve problems for real I will likely put the *actual* answers into a separate integration test module.

### Repl

Note that there's no way to hot-reload in the *Ammonite* REPL -- the Mill repl is a different beast. If you are working in the REPL and you want to change the code in a file and reload it, you need to exit out of the Ammonite REPL back to the Mill REPL, and then when you re-enter the REPL it *will* rebuild any files that have changed.

This is quite a bit faster than starting new JVMs all the time, but it's not quite `(refresh)` a la Clojure (AFAIK).

```bash
# Assuming you want to be in the test REPL
$ mill -repl
@ aoc2021.test.repl()()
@ fastparse.parse("123", aoc2021.days.Day0.number(_))
res0: fastparse.Parsed[Int] = Success(123, 3)
@ utest.TestRunner.runAndPrint(aoc2021.days.Day0Tests.tests, "Day0Tests")
+ Day0Tests.testParser 0ms
+ Day0Tests.testExample 0ms
res7: ...
@ aoc2021
cmd0.sc:1: package aoc2021 is not a value
@ exit
# back to the Mill Repl
@ aoc2021
res9: aoc2021.type = ammonite.predef.build#aoc2021:3
```

### Uberjar

I'm sure you can build one!

## Motivation

Just for fun. :-) I've been puttering around with Scala for some time and wanted to do something that was more "typed functional programming" instead of my preferred Lisp-y stuff just to see what all the fuss is about.

Highly motivated by two books:
* [Functional Programming Simplified by Alvin Alexander](https://alvinalexander.gumroad.com/l/lfpis)
* [Hands-On Scala by Li Haoyi](https://www.handsonscala.com/)

