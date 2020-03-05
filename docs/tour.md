# A tour of Icarus

This is not a complete language specification (design is still in progress, so a
complete specification does not exist yet). Rather this is a quick guided tour of
what Icarus looks like, and a bit of explanation as to why it looks like that.

If you are new to programming entirely, this tour is probably not going to be
particularly helpful. This tour frequently references other languages for means
of comparison. Unfortunately we do not yet have good guidance for *learning to
program in Icarus*.

## Variables

### Basics

Variable declarations are expressed with a `:` separating the variable name from
its type. You may optionally initialize the variable with a value. If no
initialization is present, the value will be initialized to a "zero-like" value.
This means zero for numberic types, and `false` for booleans.

```
x:  int64           // Initializes a 64-bit integer named `x` to zero
pi: float64 = 3.14  // Initializes a 64-bit floating-point number named `pi` to 3.14.
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

### Constants

Icarus also allows you to define constants known to the compiler at
compile-time. Syntactically this looks very much like a variable declaration,
but is done with `::` rather than `:`.

```
days_per_week :: int64 = 7
pi ::= 3.14
```

## Functions

Functions in Icarus take the form
`<parameters> -> <return-type> { <statements> }`. For example,

```
// Defining a squaring function
square ::= (n: int64) -> int64 {
  return n * n
}

// Calling the squaring function
square(3) // Evaluates to 9

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

half(3.0) // Evaluates to 1.5
half()    // Use the default. Evaluates to 0.5
```

All of the functions shown so far are simple enough that we would like a fast
and simple way to write them. Icarus allows you to use `=>` to avoid specifying
the return type and simply expressing the returned value to the right of the
`=>`. The return type is deduced.

```
square ::= (n: int64) => n * n
half   ::= (x := 1.0) => x / 2.0
```

