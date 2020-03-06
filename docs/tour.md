---
title: A Tour of Icarus
author: Andy Soffer
---

# A Tour of Icarus

This is not a complete language specification (design is still in progress, so a
complete specification does not exist yet). Rather this is a quick guided tour of
some of the Icarus essentials.

If you are new to programming entirely, this tour is probably not going to be
particularly helpful. This tour frequently references other languages for means
of comparison. Unfortunately we do not yet have good guidance for *learning to
program in Icarus*.

# Variables

## Basics

Variable declarations are expressed with a `:` separating the variable name from
its type. You may optionally initialize the variable with a value. If no
initialization is present, the value will be initialized to a "zero-like" value.
This means zero for numberic types, and `false` for booleans.

```
// Initializes a 64-bit integer named `x` to zero
x:  int64

// Initializes a 64-bit floating-point number named `pi` to 3.14.
pi: float64 = 3.14
```

Types can often be deduced from the values that initialize them. To deduce the
type, simply omit the type from the declaration. This is typically stylized as
`:=`, but it is perfectly valid to have whitespace between the `:` and the `=`.

```
x := 0
pi := 3.14
```

There are times in which it may be a useful performance optimization to leave
variables uninitialized. While this is not the default, it is possible to leave
variables uninitialized by assigning to them from `--`.

```
x: int32 = -- // An uninitialized 32-bit integer
```

## Constants

Icarus also allows you to define constants known to the compiler at
compile-time. Syntactically this looks very much like a variable declaration,
but is done with `::` rather than `:`.

```
days_per_week :: int64 = 7
pi ::= 3.14
```

# Types

## Functions

Functions in Icarus take the form
`<parameters> -> <return-type> { <statements> }`. For example,

```
// Defining a squaring function
square ::= (n: int64) -> int64 {
  return n * n
}

// Calling the squaring function
square(3) // Evaluates to 9.

```

Notice that we have declared a constant `square` and defined it to have the
value of this function. You will see this pattern in Icarus a lot. Where other
languages have special syntax for defining functions, types, or modules, Icarus
consistently uses the same syntax for all of these.

Function calls may look as you would have expected, but there are a few other
ways that a function can be called.

```
3'square       // Same a `square(3)`
square(n = 3)  // Icarus also supports named arguments...
(n = 3)'square // ...even when the arguments are passed first.
```

Icarus also supports default arguments.

```
half ::= (x: float64 = 1.0) -> float64 { return x / 2.0 }

half(3.0) // Evaluates to 1.5.
half()    // Use the default. Evaluates to 0.5.
```

All of the functions shown so far are simple enough that we would like a fast
and simple way to write them. Icarus allows you to use `=>` to avoid specifying
the return type and simply expressing the returned value to the right of the
`=>`. The return type is deduced.

```
square ::= (n: int64) => n * n
half   ::= (x := 1.0) => x / 2.0
```

## Arrays

Arrays are contiguous chunks of memory of a fixed size holding data all of the
same type. The type of an array can be written as `[N; T]` where `T` is the type
of data held in the array, and `N` is the number of contigous values of that
type. Array's can be constructed with a comma-separated list of values
surrounded by square-brackets.

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
types can be constructed from other types with the `|` binary operator. The type
currently held in the variant can be accessed with the `which` operator. However
the type returned from `which` is not a compile-time constant and so it cannot
be used as the type in a variable declaration.

```
v: int64 | bool = 3
which v // Evaluates to int64.
v = true
which v // Evaluates to bool.
```

## Enums and Flags

An enum is type whose value can be listed as exactly one value in a set of
alternatives. For instance, one might use an enum to represent the suit in a
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
those languages, it is common to use enums to hold a collection of flags any
number of which might be set. For this use-case, Icarus has an entirely
different construct. Icarus uses `flags` to indicate that the members are not
mutually exclusive. You can use the binary `&` (and), `|` (or), or `^` (xor) to
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

And instantiate them and access members as you might expect from other languages
with similar constructs:

```
p: Point
p.x = 3.0
```

# Modules

Icarus uses the module as the unit of encapsulation. The importer of a module
gets to choose the name associated to that module.

```
math ::= import "examples/lib/math.ic"
math.sqrt(9.0) // Evaluates to 3.0.
```

Modules can also be assigned to `--` to indicate that their contents should be
available directly without using the `math.` prefix.

```
-- ::= import "examples/lib/math.ic"
sqrt(9.0) // Evaluates to 3.0.
```

When defining your own module, declarations are not exposed publicly by default.
To make a declaration visible, be sure to mark it as `#{export}`.

```
// Possible implementation in math.ic
#{export}
sqrt ::= (x: float64) => sqrt_impl(x)

// Not exported. Users of this module cannot see this.
sqrt_impl ::= (x: float64) -> float64 { ... }
```

# Control Flow

Perhaps the most distinguishing feature of Icarus is that the core languages has
neither if-statements nor while loops. It turns out that both of these scopes
(and more!) are definable in libraries.

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
   the boolean condition).
2. What blocks can be jumped to. (In an if-statement, these are the "then" and
   "else" blocks)
3. How should the blocks be connected together? 
4. What are the exit conditions?

To define a scope that runs a "do" block forever, we would write
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

# Generics Functions

## Types as Values
TODO

## Type Deduction
TODO

# Parameterized Structs
TODO
