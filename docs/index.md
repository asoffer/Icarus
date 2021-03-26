---
title: Icarus
author: Andy Soffer
---

# Icarus

Icarus is an experimental general-purpose programming language, designed to be
fast to compile, fast to run, and easy to migrate.

## Design Priorities

The design of Icarus follows this list of priorities, in order. We hold
these priorities as important axiomatically. Not every language should have the
same list of priorities and not every project should use a language with these
priorities.

1. **Maintainability.** First and foremost, Icarus is designed to be upgradable.
Icarus will never prioritize long-term backwards compatibility. Rather, we want
to design a language for which it is easy to write tools that update source
code automatically.
When we make breaking changes to the language, we promise to provide cheap
migration paths forward.
We want to empower software engineers to change any decision they made about
their library or application. Similarly, we want the language itself to be able
to evolve as the field of computing advances.

1. **Performance.** Users should have the ability to tune their software to
obtain the highest performance their hardware can support.
This may mean memory usage, battery life, binary size, execution time, network
bandwidth, or any other resource. We recognize that it will often be impossible
to simultaneously address all of these performance requirements. For this
reason, rather than making a tradeoff in the language, we want to give
programmers the ability to design for the particular performance characteristics
relevant to their application.

1. **Freedom.** Users should be free to write software in the format that makes
sense for their use case, and it is not up to the designers of Icarus to make
value judgments or prohibitions.

1. **Simplicity.** All else equal, Icarus strives to provide a simple and
self-consistent user experience. Note that simplicity does not necessarily mean
familiarity; we are willing to be (and often are) different if we think it 
provides a better user experience. We provide simplicity where it does not
conflict with the above priorities, but we will never impose it.

## Where This Leads Us

There are many common questions directed to new programming languages.
Here are our answers to some of them:

* **Icarus has no built-in garbage collector.**
Language-provided garbage collection fundamentally violates our performance
priority, as it carries a non-zero cost and restricts users from making
fine-grained decisions about memory management.

* **Icarus has a strong static type system.**
We believe that the invariants provided by a static type system greatly improve
software understandability and maintainability, our top design priority.

## More information

* [A tour of Icarus](tour)
* [Icarus on Github](https://github.com/asoffer/Icarus)

