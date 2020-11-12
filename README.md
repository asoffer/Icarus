# Icarus

Icarus is an experimental general-purpose programming language, designed to be
fast to compile, fast to run, and easy to migrate.

## Building Icarus from Source

[![CI badge](https://github.com/asoffer/Icarus/workflows/CI/badge.svg)](https://github.com/asoffer/Icarus/actions?query=workflow%3ACI)

Compiling from source requires one of the following compilers:

* g++ version >= 8.3.0
* clang++ version >= 10.0.0

You will also need [Bazel](http://bazel.build) version >= 2.0.0.

To compile this project from source:

```
$ git clone https://github.com/asoffer/Icarus.git
$ cd Icarus
$ bazel build -c opt :icarus
```

## Language Documentation

 * [Design Philosophy](docs/index.md)
 * [Tour of Icarus](docs/tour.md)
