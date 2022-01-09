package(default_visibility = ["//visibility:public"])

cc_library(
    name = "compiled_fn",
    hdrs = ["compiled_fn.h"],
    deps = [
        "//ir/blocks:group",
        "//type:function",
    ],
)

cc_library(
    name = "compiled_scope",
    hdrs = ["compiled_scope.h"],
    deps = ["//ir/value:scope"],
)

cc_library(
    name = "out_params",
    hdrs = ["out_params.h"],
    deps = [
        "//base:extend",
        "//base/extend:serialize",
        "//ir/value:reg",
        "@com_google_absl//absl/types:span",
    ],
)

cc_library(
    name = "module",
    hdrs = ["module.h"],
    srcs = ["module.cc"],
    deps = [
        ":compiled_fn",
        "//base:debug",
        "//base:iterator",
        "//ir/byte_code",
        "//ir/value:native_fn",
        "//ir/value:scope",
        "//type:function",
        "//type:pointer",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)


cc_library(
    name = "scope_state",
    hdrs = ["scope_state.h"],
    deps = [
        "//base:any_invocable",
        "//ir/blocks:basic",
        "//ir/value:label",
        "//ir/value:result_buffer",
        "//ir/value:scope",
        "//type:qual_type",
        "@com_google_absl//absl/container:flat_hash_map",
    ],
)
