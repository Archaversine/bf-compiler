# BFC - Brainf**k Compiler

A mini-compiler for Branf**k in 100 lines of Haskell. 

## About

This compiler takes in arbitrary Brainf**ck code from a file, applies a small set of optimizations, and outputs C code 
according to the C99 standard that can be compiled into an executable via `gcc` with no extra steps. Note: BFC does not 
have an interpreter.

## Hello World 

Given the file `hello.bf`:

```
>++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<+
+.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-
]<+.
```

Running BFC with the following command:

```
bfc hello.bf output.c               # if BFC in PATH
cabal run bfc -- hello.bf output.c  # if BFC not in PATH
```

Will generate the following `output.c` file:

```c 
#include <stdio.h>

int main(void) {
  char tape[(1000)] = {(0)};
  int curr = (0);
  (curr) += (1);
  ((tape)[curr]) += (8);
  while ((tape)[curr]) {(curr) -= (1);
                        ((tape)[curr]) += (9);
                        (curr) += (1);
                        ((tape)[curr]) -= (1);};
  (curr) -= (1);
  (printf)(("%c"), ((tape)[curr]));
  (curr) += (1);
  ((tape)[curr]) += (4);
  while ((tape)[curr]) {(curr) -= (1);
                        ((tape)[curr]) += (7);
                        (curr) += (1);
                        ((tape)[curr]) -= (1);};
  (curr) -= (1);
  ((tape)[curr]) += (1);
  (printf)(("%c"), ((tape)[curr]));
  ((tape)[curr]) += (7);
  (printf)(("%c"), ((tape)[curr]));
  (printf)(("%c"), ((tape)[curr]));
  ((tape)[curr]) += (3);
  (printf)(("%c"), ((tape)[curr]));
  (curr) += (2);
  ((tape)[curr]) += (6);
  while ((tape)[curr]) {(curr) -= (1);
                        ((tape)[curr]) += (7);
                        (curr) += (1);
                        ((tape)[curr]) -= (1);};
  (curr) -= (1);
  ((tape)[curr]) += (2);
  (printf)(("%c"), ((tape)[curr]));
  ((tape)[curr]) -= (12);
  (printf)(("%c"), ((tape)[curr]));
  (curr) += (1);
  ((tape)[curr]) += (6);
  while ((tape)[curr]) {(curr) -= (1);
                        ((tape)[curr]) += (9);
                        (curr) += (1);
                        ((tape)[curr]) -= (1);};
  (curr) -= (1);
  ((tape)[curr]) += (1);
  (printf)(("%c"), ((tape)[curr]));
  (curr) -= (1);
  (printf)(("%c"), ((tape)[curr]));
  ((tape)[curr]) += (3);
  (printf)(("%c"), ((tape)[curr]));
  ((tape)[curr]) -= (6);
  (printf)(("%c"), ((tape)[curr]));
  ((tape)[curr]) -= (8);
  (printf)(("%c"), ((tape)[curr]));
  (curr) += (3);
  ((tape)[curr]) += (4);
  while ((tape)[curr]) {(curr) -= (1);
                        ((tape)[curr]) += (8);
                        (curr) += (1);
                        ((tape)[curr]) -= (1);};
  (curr) -= (1);
  ((tape)[curr]) += (1);
  (printf)(("%c"), ((tape)[curr]));
}
```

Compiling and running `output.c` will result in the output:

```
Hello, World!
```

## Redundancy Optimizations

Other than translating BF code into C, BFC attempts to minimize the code generating by combining 
consecutive increment/decrement operations and removing redundant code.

### Merge Neighbors Optimization

The first optimization made by BFC is: 

```haskell 
mergeNeighbors :: IR -> Maybe IR
mergeNeighbors (LeftBy  a (LeftBy  b xs)) = Just $ LeftBy  (a + b) xs
mergeNeighbors (RightBy a (RightBy b xs)) = Just $ RightBy (a + b) xs
mergeNeighbors (LeftBy  a (RightBy b xs)) = Just $ LeftBy  (a - b) xs
mergeNeighbors (RightBy a (LeftBy  b xs)) = Just $ RightBy (a - b) xs
mergeNeighbors (IncBy a (IncBy b xs))     = Just $ IncBy   (a + b) xs
mergeNeighbors (DecBy a (DecBy b xs))     = Just $ DecBy   (a + b) xs
mergeNeighbors (IncBy a (DecBy b xs))     = Just $ IncBy   (a - b) xs
mergeNeighbors (DecBy a (IncBy b xs))     = Just $ DecBy   (a - b) xs
mergeNeighbors _ = Nothing -- otherwise no change
```

This takes consecutive `+`, `-`, `<`, and `>` operations to merge them into a single statement.
Consider the second line, `(LeftBy a (LeftBy b xs)) = Just $ LeftBy (a + b) xs`. This line looks 
for two consecutive `LeftBy` statements in the IR (generated by `<`, which moves the pointer to the left) 
and merges them into a single `LeftBy` statement by adding both offsets together

This prevents generating the BF code `<<<` as:

```c
curr -= 1;
curr -= 1;
curr -= 1;
```

And instead generates:

```c
curr -= 3;
```

### Remove Redundant Code Optimization

The next optimization removes any statements from the input program that have no effect. For example, incrementing a cell 
and then immediately decrementing it. Internally, this is represented as:

```haskell 
removeRedundant :: IR -> Maybe IR
removeRedundant (LeftBy  0 xs)  = Just xs
removeRedundant (RightBy 0 xs)  = Just xs
removeRedundant (IncBy   0 xs)  = Just xs
removeRedundant (DecBy   0 xs)  = Just xs
removeRedundant (Loop EndIR xs) = Just xs
removeRedundant _ = Nothing -- otherwise no change
```

In the second line of the code, the compiler identifies any `LeftBy` statements that move the pointer left by 0 (created by 
merging a `LeftBy` and `RightBy` statement of equal values) and simply returns the statements after it; effectively removing it.
The same is done for similar operations. On top of that, this optimization removes any loops that have nothing inside them 
(represented as `[]` in BF code). This is done because a `[]` loop can either hang the program indefinetly, or do nothing to the 
current cell. Since both of these actions are useless, they are removed from the program.

### An Example

Consider the file `redundant.bf`:

```
[+++[--+-++]---]
```

Let's first examine the inner loop `[--+-++]`. In the body of the loop, the current cell is decremented twice, incremented, 
decremented again, and then finally incremented twice. On the inside of the loop, a decrement immediately follows an increment, 
which can be cancelled out according to the `removeRedundant` optimization. This now results in a loop that looks like `[--++]`.
This loop has two increments immediately following two decrements, which will also be removed by the `removeRedundant` optimization.
At this point, the loop is now empty (`[]`). 

Substituting this into the original source code, the loop looks like `[+++[]---]`. The `removeReundant` optimization will remove the 
empty loop, resulting in `[+++---]`. `removeRedundant` will eventually reduce this to `[]`, which in turn is reduced to nothing. 
In other words, the `redundant.bf` program doesn't actually do anything.

This is further demonstrated by using BFC on `redundant.bf`:

```c 
#include <stdio.h>

int main(void) {
  char tape[(1000)] = {(0)};
  int curr = (0);
}
```

The output C program initializes the `tape` and `curr` variables, but does nothing after that.

