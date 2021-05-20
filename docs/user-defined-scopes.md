---
title: User-defined scopes
author: Andy Soffer
---

# User-defined scopes

## Introduction

Before we dive into how to define your own scopes, we need to define some
language for talking about scopes, and identify some components of scopes. We
will start with scopes that are common in most programming languages: `if` and
`while`.

Scopes can consist of one or more *block*s of code. For `while` loops, most
languages do not give a name to the block, but the block is chunk of code that
is executed on each loop iteration. In the Icarus standard library, we would
write `while (condition) do { ... }`, so the block would be `do`.

If statements in most languages can have either one or two blocks: The `then`
block is mandatory, and the `else` block is optional.

You may be wondering about "else if", but this is not actually a special construct in all languages. In particular in Icarus, we allow "else if", but it is syntactic sugar. That is,

```
if (a) then {
  ...
} else if (b) then {
  ...
} else {
  ...
}
```

is syntactic sugar for

```
if (a) then {
} else {
  if (b) then {
    ...
  } else {
    ...
  }
}
```

## A simple example

Let's start by defining a relatively simple scope named `forever` which will
loop forever. Because each scope needs at least one block, and blocks must be
named in Icarus, we will choose the name `do` for the block.

The implementation of this scope looks like:

```
forever ::= scope {
  enter ::= jump () { goto do() }

  do ::= block {
    before ::= () -> () {}
    after ::= jump () { goto do() }
  }
}

// Usage
forever () do {
  do_something()
}
```

When a call to the `forever` scope is reached, the `enter` jump will be executed.
The jump says that it should go to the `do` block, so control jumps to the
`before` function in the `do` block. Once the function executes (it is trivial
in this case, but you may imagine wanting to do something interesting here), the
body of the `do` block is executed. In this example, that's the call to
`do_something`. When the body of the `do` block completes, the `do` block's
`after` jump is executed which once again directs control back to the `do` block
itself, restarting the loop.

TODO: Dive into more details here.
