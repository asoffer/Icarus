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
bool[10]    // C, C++
[bool; 10]  // Rust
[10]bool    // Go
```

The first choice to make in this space is regarding the ordering of the type and
the length. C and Rust have chosen to place the length after the type. Go has
chosen to put the length before the type.

We believe Go's choice is more elegant, in particular, when considering
multidimensional arrays. Consider the syntax for an array of length 5, each of
the elements of which are arrays of length 10, and consider how you might access
the very last element in this multidimensional array.

```
// C, C++
bool my_bools[10][5];
my_bools[4][9];

// Rust
let my_bools: [[bool; 10]; 5]
my_bools[4][9]

// Go
var my_bools: [5][10]bool
my_bools[4][9]
```

With C and Rust, the indices 4 and 9 correspond to the array lengths 5 and 10,
but are written in the opposite order! This is because array indexing happens
on the outer layer first (in all languages). In C and Rust, the outermost array
length appears to the far right side of the type. As we apply further indices
towards the innermost array, we peel off array layers. In C and Rust, this means
proceeding inwards from the right.

With Go as well, array layers are peeled off from the outermost to the
innermost, but this happens from the lefthand side. In other words, by placing
array lengths to the left, Go ensures that indexing peels off array types from
left to right, the same direction that array indexing occurs in.

This leads us to the following conclusion: If we want array indexing to occur in
the same order (left-to-right or right-to-left) as the lengths specified in the
array type, it is important that lengths in the type occur on the *opposite*
side from the indices.

This leaves us with the choice: Do we keep array indices on the right, but place
the array lengths on the left of the type as Go does, or do we keep the lengths
on the right of the element type as C and Rust do, but index into an array on
the left?

Given that we are not aware of any language which indexes arrays on the left
(something like `[0]a` to get the the first element of `a`), we have chosen to
follow Go's lead, and place the array length to the left of the array's element
type.

While we could have simply taken Go's syntax directly, we prefer Rust's `[ ; ]`
syntax (without strong technical reason), so an array of 10 bools in Icarus
have type `[10; bool]`. We also provide syntactic sugar for multidimensional
arrays. Rather than writing `[5; [10; bool]]` (which is allowed), one could also
write `[5, 10; bool]` to denote the same type.
