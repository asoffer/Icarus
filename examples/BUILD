load('//toolchain:ic_rule.bzl', 'ic_library', 'ic_interpret')

filegroup(
    name = 'example_file',
    srcs = ['example_file.txt'],
)

ic_interpret(
    name = 'c_file',
    src = 'c_file.ic',
    data = [':example_file'],
    deps = [
        '//stdlib:core',
        '//stdlib:cstdio',
        '//stdlib:errno',
        '//stdlib:io',
    ],
)

ic_interpret(
    name = 'enum',
    src = 'enum.ic',
    deps = [
        '//stdlib:core',
        '//stdlib:io',
    ],
)

ic_interpret(
    name = 'factorial',
    src = 'factorial.ic',
    deps = [
        '//stdlib:core',
        '//stdlib:io',
    ],
)

ic_interpret(
    name = 'fibonacci',
    src = 'fibonacci.ic',
    deps = [
        '//stdlib:core',
        '//stdlib:io',
    ],
)

ic_interpret(
    name = 'fizzbuzz',
    src = 'fizzbuzz.ic',
    deps = [
        '//stdlib:core',
        '//stdlib:io',
    ],
)

ic_interpret(
    name = 'function_calls',
    src = 'function_calls.ic',
    deps = [
        '//stdlib:io',
    ],
)

ic_interpret(
    name = 'mandelbrot',
    src = 'mandelbrot.ic',
    deps = [
        '//stdlib:core',
        '//stdlib:io',
    ],
)

ic_interpret(
    name = 'ping_pong',
    src = 'ping_pong.ic',
    deps = [
        '//stdlib:io',
    ],
)

ic_interpret(
    name = 'primes',
    src = 'primes.ic',
    deps = [
        '//stdlib:core',
        '//stdlib:io',
    ],
)
