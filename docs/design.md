---
title: Design choices
author: Andy Soffer
---

# Design choices

This document describes many of the syntactic and semantic design choices that
have gone into the Icarus programming language.

## Array type syntax

Even among statically-typed languages that have fixed-length arrays, there are
many different syntactic choices.

```
[bool; 10]  // Rust
[10]bool    // Go
bool[10]    // C, C++
```

We believe Go's choice is the most elegant. While any of these could be
reasonable for a one-dimensional array, of the syntactic choices examined here,
Go's approach works nicely for multidimensional arrays in ways that Rust, C, and
C++ do not. Below is a discussion in which we aim to understand what makes Go's
choice elegant by examining it from first principles.

Let's first examine the salient properties when typing multidimensional arrays.
Here are declarations of an array with 5 elements, each of which is an array of
10 boolean elements, in each of these languages, followed by an expression which
indexes into the last element in that array.

```
// Rust
let my_bools: [[bool; 10]; 5]
my_bools[4][9]

// Go
var my_bools: [5][10]bool
my_bools[4][9]

// C, C++
bool my_bools[5][10];
my_bools[4][9];
```

With Rust, the indices 4 and 9 correspond to the array lengths 5 and 10, but are
written in the opposite order! This is because array indexing happens on the
outer layer first (for both Rust and Go). In Rust, the outermost array length
appears to the far right side of the type. As we apply further indices towards
the innermost array, we peel off array layers. In Rust, this means proceeding
inwards from the right.

With Go as well, array layers are peeled off from the outermost to the
innermost, but this happens from the lefthand side. In other words, by placing
array lengths to the left, Go ensures that indexing peels off array types from
left to right, the same direction that array indexing occurs in.

C and C++ are a bit stranger. Despite having its lengths to the right of the
type, they are read in the same direction as Go: left-to-right. This happens
because of the idea that declaration should model usage. What is key here to
this syntactic model is that the array lengths occur not only to the right of
the type, but also to the right of the variable being declared. This approach
often becomes confusing though when we think about just the type without a
variable being declared.

```
using ten_bools = bool[10];
using five_arrays_of_ten_bools = ten_bools[5];
```

While each of these type declarations are correct, we cannot syntactically
substitute `bool[10]` in place of `ten_bools` in the second declaration. This is
often confusing because it is so commonlpace that if computing an expression is
side-effect free (as it is here), we should be able to substitute equivalent
expressions for each other. That principle is violation when we think about
types as expressions in C and C++.


This exploration leads us to two claims:

1. Syntactic substitutability is a valuable goal to strive for.
1. Array indexing should occur in the same order (left-to-right or
   right-to-left) as the lengths specified in the array type.

There are of course other tradeoffs to consider in general, but we do believe
these to be valuable when all else is equal. Moreover, we believe that the
merits of these claims outweigh most other considerations (e.g., being
consistent with any particular existing language).

The first point rules out the approach taken in C and C++, but it does leave us
with another choice: Do we keep array indices on the right as done with Rust,
but place the array lengths on the left of the type as Go does, or do we keep
the lengths on the right of the element type as C and Rust do, but index into an
array on the left?

Given that we are not aware of any language which indexes arrays on the left
(something like `[0]a` to get the the first element of `a`), we have chosen to
follow Go's lead, and place the array length to the left of the array's element
type.

While we could have simply taken Go's syntax directly, we prefer Rust's `[ ; ]`
syntax (without strong technical reason), so an array of 10 bools in Icarus
have type `[10; bool]`. We also provide syntactic sugar for multidimensional
arrays. Rather than writing `[5; [10; bool]]` (which is allowed), one could also
write `[5, 10; bool]` to denote the same type.
