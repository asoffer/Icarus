package(default_visibility = ["//visibility:public"])
load("//:defs.bzl", "cc_lib_target", "sources")
sources()

cc_lib_target(name = "bag", impl_deps = None, test_deps = None)
cc_lib_target(name = "debug",
              intf_deps = [":matchers", ":log"],
              impl_deps = None,
              test_deps = None)

cc_lib_target(name = "expected",
              intf_deps = [":stringify"],
              impl_deps = None,
              test_deps = None)

cc_lib_target(
    name = "graph",
    intf_deps = [
        "@com_google_absl//absl/container:flat_hash_map",
        "@com_google_absl//absl/container:flat_hash_set",
    ],
    impl_deps = None,
    test_deps = None,)

cc_lib_target(name = "guarded", impl_deps = None, test_deps = None)
cc_lib_target(name = "hash", impl_deps = None, test_deps = None)
cc_lib_target(name = "interval", impl_deps = None, test_deps = None)
cc_lib_target(name = "lazy", impl_deps = None, test_deps = None)
cc_lib_target(name = "log",
              intf_deps = [
                  ":guarded",
                  ":stringify",
                  "@com_google_absl//absl/container:flat_hash_map",
                  "@com_google_absl//absl/container:flat_hash_set",
                  ],
              impl_deps = [],
              test_deps = None)
cc_lib_target(name = "macros", impl_deps = None, test_deps = None)
cc_lib_target(name = "matchers",
              intf_deps = [":tuple"],
              impl_deps = None,
              test_deps = None)
cc_lib_target(name = "meta", impl_deps = None, test_deps = [])
cc_lib_target(name = "move_func", impl_deps = None, test_deps = [])
cc_lib_target(name = "permutation", 
              intf_deps = ["@com_google_absl//absl/random"],
              impl_deps = None,
              test_deps = None)
cc_lib_target(name = "owned_ptr", impl_deps = None, test_deps = None)
cc_lib_target(name = "scope", impl_deps = None, test_deps = [])
cc_lib_target(name = "stringify",
              intf_deps = [":tuple"],
              impl_deps = None,
              test_deps = [])
cc_lib_target(name = "strong_types",
              intf_deps = [
                  ":debug",
                  ":stringify",
                  "@com_google_absl//absl/strings",
              ],
              impl_deps = None,
              test_deps = None)
cc_lib_target(name = "tag",
              intf_deps = [],
              impl_deps = None,
              test_deps = None)
cc_lib_target(name = "tuple",
              intf_deps = [],
              impl_deps = None,
              test_deps = [])
cc_lib_target(
    name = "untyped_buffer",
    intf_deps = [
        ":debug",
    ],
    impl_deps = [
        "@com_google_absl//absl/strings",
    ],
    test_deps = [])

cc_lib_target(
    name = "util",
    intf_deps = [":debug", ":macros"],
    impl_deps = None,
    test_deps = None)

cc_lib_target(name = "ptr_span", intf_deps = [], impl_deps = None, test_deps = None)
