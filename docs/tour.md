---
title: A Tour of Icarus
author: Andy Soffer
---

# A Tour of Icarus

This is not a complete language specification (design is still in progress, so a
complete specification does not exist yet).
Rather, this is a quick guided tour of some of the Icarus essentials.

If you are new to programming entirely, this tour is probably not going to be
particularly helpful. This tour frequently references other languages for means
of comparison. Unfortunately we do not yet have good guidance for *learning to
program in Icarus*.

# Variables

## Basics

Variable declarations are expressed with a `:` separating the variable name from
its type. You may optionally initialize the variable with a value using `=`
following the type If no initialization is present, the value will be
initialized to a "zero-like" value.  This means zero for numeric types and
`false` for booleans.

```
// Initialize a 64-bit integer named `x` to zero.
x: i64

// Initialize a 64-bit floating-point number named `pi` to 3.14.
pi: f64 = 3.14
```

Types can often be deduced from the values that initialize them. To deduce the
type, simply omit it from the declaration. This is typically stylized as
`:=`, but it is perfectly valid to have whitespace between the `:` and the `=`.

```
b := true
pi := 3.14
```

There are times in which it may be a useful performance optimization to leave
variables uninitialized. Assigning the special value `--` denotes this.

```
// An uninitialized 32-bit integer
x: i32 = --
```

All variables must be initialized before being used. Initializing already-declared variables is outside the scope of this tour.

## Constants

Icarus allows you to define constants with values known at compile-time.
These look like normal variable declarations,
replacing `:` with `::`.

```
days_per_week :: i64 = 7
// Type deduction works for constants, too.
pi ::= 3.14
```

# Types

## Functions

Functions in Icarus take the form
`<parameters> -> <return-type> { <statements> }`. For example,

```
// Define a squaring function.
square ::= (n: i64) -> i64 {
  return n * n
}

// Call the squaring function.
square(3) // Evaluates to 9.
```

Notice that we declared a constant `square` and defined it to have the
value of this function. You will see this pattern in Icarus a lot. Where other
languages have special syntax for defining functions, types, or modules, Icarus
uses the same syntax for all kinds of declarations.

In addition to the standard function call syntax, Icarus supports a few other
styles for calling a function:

```
3'square       // Same as `square(3)`.
square(n = 3)  // Icarus also supports named arguments...
(n = 3)'square // ...even when the arguments are passed first.
```

Icarus functions can have arguments with default values.

```
half ::= (x: f64 = 1.0) -> f64 { return x / 2.0 }

half(3.0) // Evaluates to 1.5.
half()    // Use the default. Evaluates to 0.5.
```

All of the functions shown so far are simple enough that the standard function
syntax is overly verbose. Icarus provides a shorthand syntax that replaces
`->` with `=>`, and replaces the return type with a single-expression
function body.
The return type is always deduced.

```
square ::= (n: i64) => n * n
half   ::= (x := 1.0) => x / 2.0
```

## Arrays

Arrays are contiguous, fixed-size blocks of memory that hold data all of the
same type. The type of an array [is written](design) `[N; T]` where `T` is the
type of data held in the array, and `N` is the length of the array.
Arrays can be constructed with a comma-separated list of values surrounded by
square brackets.

```
a: [3; i64] = [1, 4, 9]
b := [2, 4, 6]
```

## Pointers
Pointers represent the location of an object in memory. A pointer to an object
of type `T` has type `*T`. To take the address of an object, we use the unary
`&` operator. To dereference a pointer to an object, we use `@`.

```
n: i64
p: *i64 = &n
@p = 3
// Now `n` holds the value `3`.
```

Unlike C/C++, pointer arithmetic is not allowed on `*T` at all.
Icarus has a second pointer type, called a buffer pointer, denoted `[*]T` which
does allow arithmetic. Taking the address and dereferencing are done with the
same operators.

```
a := [1, 2, 3]
p: [*]i64 = &a[1]  // Okay to use a buffer pointer, because we are pointing into
                   // an array

@(p - 1) = 10      // We can do arithmetic with `p`
@p = 20            //
p[1] = 30          // We can also directly index with brackets.

// Now a == [10, 20, 30]
```

Note that buffer pointers must point into arrays or slices (see below). You may not use buffer pointer arithmetic to dereference a value outside the underlying array or slice.

## Slices

A slice is another reference-type like pointers. It holds effectively is a
buffer pointer and a length, allowing you refer to subsequences of contiguous
blocks of memory. A slice of objects of type `T` is written as `[]T`. Slices
can be indexed and dereferenced just as buffer pointers.

```
a := [1, 2, 3, 4, 5]
s := slice(&a[1], 3)  // A slice refering to the elements in `a` whose values
                      // are currently 2, 3, and 4.
s[1] = 30

// Now a == [1, 2, 30, 4, 5]
```

## Enums and Flags

An enum is a type whose value can be listed as exactly one value from a set of
alternatives. For instance, we might use an enum to represent the suit in a
card game.

```
Suit ::= enum {
  Clubs
  Diamonds
  Hearts
  Spades
}

my_suit := Suit.Clubs
```

Unlike C or C++, enum types must take on exactly one of the listed values. In
those languages, it is common to use enums to hold a collection of flags, any
number of which might be set. For this use-case, Icarus has a separate
construct called `flags`. This indicates that members are not mutually
exclusive. You can use the binary `&` (and), `|` (or), or `^` (xor) to
operate on flags.

```
Color ::= flags {
  Red
  Blue
  Green
}
Yellow ::= Color.Blue | Color.Green
```

## Structs

Users can define their own types with the `struct` keyword.

```
Point ::= struct {
  x: f64
  y: f64
  z: f64
}
```

Struct instantiation and member access uses syntax similar to many
Algol-like programming languages.

```
// Create a default-initialized Point named p.
p: Point
// Update the x member.
p.x = 3.0
```

You can also initialize the values in a struct directly using the designated initializer syntax:

```
p := Point.{
  x = 0.1
  y = 0.2
  z = 0.3
}
```

# Modules

Icarus modules are the primary unit of encapsulation. The importer of a module
gets to choose the name associated to that module.

```
math ::= import "math/core.ic"
math.sqrt(9.0)  // Evaluates to 3.0.
```

Modules can also be assigned to `--`, which makes their contents available
directly without using module's name as a prefix.

```
-- ::= import "math/core.ic"
sqrt(9.0)  // Evaluates to 3.0.
```

When defining your own module, declarations are not exposed publicly by default.
To make a declaration visible, mark it with `#{export}` tag.

```
// Possible implementation in math.ic.
#{export}
sqrt ::= (x: f64) => sqrt_impl(x)

// Not exported. Users of this module cannot see this.
sqrt_impl ::= (x: f64) -> f64 { ... }
```

Moreover, when exporting structs, note that the members of a struct are (by
default) not accessible outside the module, even if the struct itself is
exported. Where other languages use keywords like "public" or "private" to
denote access control to struct fields, Icarus reuses `#{export}`.

```
#{export}
my_public_struct ::= struct {
  #{export}
  my_public_field: i64
  my_private_field: bool
}

my_private_struct ::= struct {
  also_private: f64
}
```

# Control Flow

Perhaps the most distinguishing feature of Icarus is that the core language has
neither `if` statements nor `while` loops. It turns out that both of these are
definable in libraries via user-defined scopes.

```
-- ::= import "core.ic"
io ::= import "io.ic"

// Prints "01234five6789"
i := 0
while (i < 10) do {
  if (i == 5) then {
    io.Print("five")
  } else {
    io.Print(i)
  }
  i += 1
}
```

Commonly used scopes (`if`, `for`, and `while`) are all defined in the standard
library's "core.ic".

Defining your own scopes is more complex than would succinctly fit in this tour.
You can learn more about that [here](user-defined-scopes).

# Generic Functions

## Types as Values
Icarus treats types as values just like integers, strings, and bools.
This means types can be passed into and out of functions, used in expressions, etc.

```
get_int ::= (signed: bool) -> type {
  if (signed) then {
    return i64
  } else {
    return u64
  }
}

my_int :: type = get_int(signed = true)
```

There are two oddities that come out of this. First, because a type such as
`bool` is a value, it must itself have a type. In the same way `true` has type
`bool`, `bool` has type `type`. But going one step further, `type` is itself a
value of type `type`.

The second oddity is that, there are restrictions on when a type can be used to
declare a variable. In particular, a type can only be used to declare a variable
if the type is a constant known at compile-time.

```
f ::= (constant_type :: type, nonconstant_type: type) -> () {
  x: constant_type // Okay, `constant_type` is a constant.
  // `y: nonconstant_type` would be a compiler error.
}
```

## Type Deduction

Rather than use some complex predefined rules for type deduction, Icarus lets
generic functions dictate which types should be deduced. This is done via `$`,
which computes the type of the argument bound to the given parameter. Let's see
an example:

```
square ::= (x: $x) => x * x

square(3) // Evaluates to 9
square(1.1) // Evaluates to 1.21
```

In this example, when the argument `3` is bound to the parameter `x`, we deduce
that `$x` must be `i64`, the type of `3`. From there the entire function must
have type `i64 -> i64`. Similarly, when `1.1` is bound, `$x` is deduced as
`f64` meaning the entire function type is deduced as `f64 -> f64`.

This can be used in a variety of ways. Below we show the same generic function
five times implemented with slightly different deduction semantics.
```
// Deduce the type of the parameters from $x
max ::= (x: $x, y: $x) -> $x {
  if (x < y) then { return y } else { return x }
}

// Deduce the type of the parameters from $y
max ::= (x: $y, y: $y) -> $y { ... }

// Deduce the type of the parameters from $x, but allow it to be explicitly
// overridden by specifying a value for `T`
max ::= (x: T, y: T, T ::= $x) -> $y { ... }


common_type ::= (lhs: type, rhs: type) -> type { ... }

// Looking at the types of both arguments passed in, find a common type that
both convert to and use that.
max ::= (x: common_type($x, $y),
         y: common_type($x, $y)) -> common_type($x, $y) { ... }

eq ::= (lhs: type, rhs: type) {
  if (lhs == rhs) then { return lhs } else { return error("Type mismatch") }
}
// Fail to compile if the types do not match.
max ::= (x: $x, y: eq($x, $y)) -> $x { ... }
```

# Parameterized Structs

Parameterized structs work much the same way as generic functions do, by
accepting constant parameters. Here is an example of a pair type.

```
pair ::= struct (A :: type, B :: type) {
  first: A
  second: B
}

// Usage:
p := pair(i64, bool).{
  first = 3
  second = false
}
```
