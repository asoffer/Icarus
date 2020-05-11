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
its type. You may optionally initialize the variable with a value. If no
initialization is present, the value will be initialized to a "zero-like" value.
This means zero for numeric types and `false` for booleans.

```
// Initialize a 64-bit integer named `x` to zero.
x: int64

// Initialize a 64-bit floating-point number named `pi` to 3.14.
pi: float64 = 3.14
```

Types can often be deduced from the values that initialize them. To deduce the
type, simply omit it from the declaration. This is typically stylized as
`:=`, but it is perfectly valid to have whitespace between the `:` and the `=`.

```
x := 0
pi := 3.14
```

There are times in which it may be a useful performance optimization to leave
variables uninitialized. Assigning the special value `--` denotes this.

```
// An uninitialized 32-bit integer
x: int32 = --
```

## Constants

Icarus allows you to define constants with values known at compile-time.
These look like normal variable declarations,
replacing `:` with `::`.

```
days_per_week :: int64 = 7
// Type deduction works for constants, too.
pi ::= 3.14
```

# Types

## Functions

Functions in Icarus take the form
`<parameters> -> <return-type> { <statements> }`. For example,

```
// Define a squaring function.
square ::= (n: int64) -> int64 {
  return n * n
}

// Call the squaring function.
square(3) // Evaluates to 9.
```

Notice that we declared a constant `square` and defined it to have the
value of this function. You will see this pattern in Icarus a lot. Where other
languages have special syntax for defining functions, types, or modules, Icarus
uses the same syntax for all types of declarations.

In addition to the standard function call syntax, Icarus supports a few other
styles for calling a function:

```
3'square       // Same as `square(3)`.
square(n = 3)  // Icarus also supports named arguments...
(n = 3)'square // ...even when the arguments are passed first.
```

Icarus functions can have arguments with default values.

```
half ::= (x: float64 = 1.0) -> float64 { return x / 2.0 }

half(3.0) // Evaluates to 1.5.
half()    // Use the default. Evaluates to 0.5.
```

All of the functions shown so far are simple enough that the standard function
syntax is overly verbose. Icarus provides a shorthand syntax that replaces
`->` with `=>`, and replaces the return type with a single-expression
function body.
The return type is deduced.

```
square ::= (n: int64) => n * n
half   ::= (x := 1.0) => x / 2.0
```

## Arrays

Arrays are contiguous, fixed-size chunks of memory that hold data all of the
same type. The type of an array is written `[N; T]` where `T` is the type
of data held in the array, and `N` is the length of the array.
Arrays can be constructed with a comma-separated list of values
surrounded by square brackets.

```
a: [3; int64] = [1, 4, 9]
b := [2, 4, 6]
```

## Pointers
TODO

## Tuples
TODO

## Variants
A variant is a value which can be one of a handful of different types. Variant
types are constructed from other types with the `|` binary operator. The type
currently held in the variant is accessed with the `which` keyword.
Note that the type returned from `which` is not a compile-time constant,
so it cannot be used in a variable declaration.

```
v: int64 | bool = 3
which v // Evaluates to int64.
v = true
which v // Evaluates to bool.
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
  x: float64
  y: float64
  z: float64
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

# Modules

Icarus modules are the primary unit of encapsulation. The importer of a module
gets to choose the name associated to that module.

```
math ::= import "examples/lib/math.ic"
math.sqrt(9.0) // Evaluates to 3.0.
```

Modules can also be assigned to `--`, which makes their contents available
directly without using module's name as a prefix.

```
-- ::= import "examples/lib/math.ic"
sqrt(9.0) // Evaluates to 3.0.
```

When defining your own module, declarations are not exposed publicly by default.
To make a declaration visible, mark it with `#{export}` tag.

```
// Possible implementation in math.ic.
#{export}
sqrt ::= (x: float64) => sqrt_impl(x)

// Not exported. Users of this module cannot see this.
sqrt_impl ::= (x: float64) -> float64 { ... }
```

# Control Flow

Perhaps the most distinguishing feature of Icarus is that the core language has
neither `if` statements nor `while` loops. It turns out that both of these are
definable in libraries via user-defined scopes.

```
-- ::= import "examples/lib/core.ic"
io ::= import "examples/lib/io.ic"

// Prints "01234five6789"
i := 0
while (i < 10) do {
  if (i == 5) then {
    io.Print("five")
  } else {
    io.Print(5)
  }
  i += 1
}
```

Defining your own scope requires a few pieces:
1. What parameters are used to initialize your scope? (In a while-loop, this is
   the boolean condition.)
2. What blocks can be jumped to? (In an if-statement, these are the "then" and
   "else" block.)
3. How should the blocks be connected together? 
4. What are the exit conditions?

To define a scope that runs a "do" block forever, we would write:

```
forever ::= scope {
  init ::= jump() { goto do() }
  do ::= block {
    // Nothing special to be done before the do-block is entered.
    before ::= () -> () {} 

    // Restart the do-block
    after: jump() { goto do() }
  }

  // There's no need for an exit condition, because there's no way to
  // exit.
}
```

Using this new scope is simple:

```
forever () do {
  io.Print("yes\n")
}
```

# Generic Functions

## Types as Values
Icarus treats types as values just like integers, strings, and bools.
This means types can be passed into and out of functions, used in expressions, etc.

```
get_int ::= (signed: bool) -> type {
  if (signed) then {
    return int64
  } else {
    return nat64
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
that `$x` must be `int64`, the type of `3`. From there the entire function must
have type `int64 -> int64`. Similarly, when `1.1` is bound, `$x` is deduced as
`float64` meaning the entire function type is deduced as `float64 -> float64`.

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
TODO
