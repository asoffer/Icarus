# Icarus

Icarus is an experimental general-purpose programming language, designed to be
fast to compile, fast to run, and easy to migrate.

## Building Icarus from Source

[![CI badge](https://github.com/asoffer/Icarus/workflows/CI/badge.svg)](https://github.com/asoffer/Icarus/actions?query=workflow%3ACI)

Compiling from source requires:

* clang++ version >= 12.0.0
* [Bazel](http://bazel.build) version >= 4.2.1.

To compile this project from source:

```
$ git clone https://github.com/asoffer/Icarus.git
$ cd Icarus
$ bazel build -c opt ...
```

## Language Documentation

 * [Design Philosophy](docs/index.md)
 * [Tour of Icarus](docs/tour.md)
