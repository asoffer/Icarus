# Icarus

Icarus is an experimental general-purpose programming language, designed to be
fast to compile, fast to run, and easy to migrate.

## Design priorities

In designing Icarus, we have the following ordered list of priorities. We hold
these priorities as important axiomatically. Not every language should have the
same list of priorities and not every project should use a language with these
priorities.

1. **Maintainability.** First and foremost Icarus is designed to be upgradable.
We want empower software engineers to be able to change every decision they have
made about their library or application. Similarly, we want the language itself
to be able to evolve with computing.

1. **Performance.** Users should have the ability to tune their software to
obtain the very last drop of performance made available to them by the hardware.
This may mean memory usage, battery life, binary size, or execution time.

1. **Simplicity.** All else equal, Icarus strives to provide a simple and
self-consistent user experience. Remember that simplicity does not mean
familiarity. We are willing to be (and often are very) different if we think it 
provides a better user experience.

## Where this leads us

There are two common questions that are directed to any new programming language.
Let us answer them directly:

* *Icarus has no garbage collector.* Debates about the precise cost of garbage
collection aside, garbage colleciton fundamentally violates our priorities for
performance, because it takes away the ability for users to make fine-grained 
ecisions about how they want to manage memory.

* *Icarus has a strong static type system.* The invariants provided by a static
type system greatly increase software understandability and maintainability.

## A tour
TODO: A quick tour of the language
