# Icarus

Icarus is an experimental general-purpose programming language, designed to be
fast to compile, fast to run, and easy to migrate. Learn more [here](https://asoffer.github.io/Icarus).

## Building Icarus from Source

[![CI badge](https://github.com/asoffer/Icarus/workflows/CI/badge.svg)](https://github.com/asoffer/Icarus/actions?query=workflow%3ACI)

Compiling from source requires:

* clang++ version >= 14.0.0
* [Bazel](http://bazel.build) version >= 5.1.1.

To compile this project from source:

```
$ git clone https://github.com/asoffer/Icarus.git
$ cd Icarus
$ bazel build -c opt ...
```

## Language Documentation

 * [Design Philosophy](https://asoffer.github.io/Icarus)
 * [Tour of Icarus](https://asoffer.github.io/Icarus/tour)
