---
title: Icarus
author: Andy Soffer
---

# Icarus

Icarus is an experimental general-purpose programming language, designed to be
fast to compile, fast to run, and easy to migrate.

## Design Priorities

In designing Icarus, we have the following ordered list of priorities. We hold
these priorities as important axiomatically. Not every language should have the
same list of priorities and not every project should use a language with these
priorities.

1. **Maintainability.** First and foremost Icarus is designed to be upgradable.
Icarus will never prioritize long-term backwards compatibility. Rather, we want
to design a language for which it is easy to write tools to update source code.
When we make breaking changes, we promise to provide migration paths forward,
and we promise to make that migration path as cheap as possible. We want empower
software engineers to be able to change every decision they have made about
their library or application. Similarly, we want the language itself to be able
to evolve with computing.

1. **Performance.** Users should have the ability to tune their software to
obtain the very last drop of performance made available to them by the hardware.
This may mean memory usage, battery life, binary size, or execution time. We
recognize that often it will be impossible to simultaneously address all of
these performance requirements. For this reason, rather than making a tradeoff
in the language, we want to give programmers the ability to design for the
particular performance characteristics relevant to their application.

1. **Freedom.** Users must be free to write software in the format that makes
sense for their use case, and it is not up to the designers of Icarus to make
value judgments or prohibitions.

1. **Simplicity.** All else equal, Icarus strives to provide a simple and
self-consistent user experience. Remember that simplicity does not mean
familiarity. We are willing to be (and often are very) different if we think it 
provides a better user experience. We will provide simplicity where it does not
conflict with other priorities, but we will never impose.

## Where This Leads Us

There are two common questions that are directed to any new programming language.
Let us answer them directly:

* **Icarus has no garbage collector.** Debates about the precise cost of garbage
collection aside, garbage colleciton fundamentally violates our priorities for
performance, because it takes away the ability for users to make fine-grained 
decisions about how they want to manage memory. While we believe there is a place
for garbage-collected languages, Icarus is designed to fill a different role.

* **Icarus has a strong static type system.** The invariants provided by a static
type system greatly increase software understandability and maintainability.
While we believe there is a place for dynamically typed languages, Icarus is
designed to fill a different role.

## More information

* [A tour of Icarus](tour)
* [See the compiler source code](https://github.com/asoffer/Icarus)

