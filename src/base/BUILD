cc_library(
    name = 'check',
    hdrs = ['check.h'],
    deps = [':string'],
)

cc_library(
    name = 'debug',
    hdrs = ['debug.h'],
    deps = [
        ':check',
        ':log',
    ]
)

cc_library(
    name = 'expected',
    hdrs = ['expected.h'],
)

cc_library(
    name = 'guarded',
    hdrs = ['guarded.h'],
)

cc_library(
    name = 'hash',
    hdrs = ['hash.h'],
)

cc_library(
    name = 'interval',
    hdrs = ['interval.h'],
)

cc_library(
    name = 'lazy',
    hdrs = ['lazy.h'],
)

cc_library(
    name = 'log',
    hdrs = ['log.h'],
    deps = [':string'],
)

cc_library(
    name = 'owned_ptr',
    hdrs = ['owned_ptr.h'],
)

cc_library(
    name = 'string',
    hdrs = ['string.h'],
)

cc_library(
    name = 'strong_types',
    hdrs = ['strong_types.h'],
    deps = [':debug'],
)

cc_library(
    name = 'test',
    hdrs = ['test.h'],
    deps = [':check'],
)

cc_library(
    name = 'types',
    hdrs = ['types.h'],
)

cc_library(
    name = 'untyped_buffer',
    hdrs = ['untyped_buffer.h'],
    srcs = ['untyped_buffer.cc'],
    deps = [':debug'],
)

cc_library(
    name = 'util',
    hdrs = ['util.h'],
    deps = [':debug'],
)
