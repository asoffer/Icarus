workspace(name = "asoffer_icarus")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "bazel_skylib",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-skylib/releases/download/1.2.1/bazel-skylib-1.2.1.tar.gz",
        "https://github.com/bazelbuild/bazel-skylib/releases/download/1.2.1/bazel-skylib-1.2.1.tar.gz",
    ],
    sha256 = "f7be3474d42aae265405a592bb7da8e171919d74c16f082a5457840f06054728",
)
load("@bazel_skylib//:workspace.bzl", "bazel_skylib_workspace")
bazel_skylib_workspace()

http_archive(
    name = "zlib",
    build_file = "@com_google_protobuf//:third_party/zlib.BUILD",
    sha256 = "c3e5e9fdd5004dcb542feda5ee4f0ff0744628baf8ed2dd5d66f8ca1197cb1a1",
    strip_prefix = "zlib-1.2.11",
    urls = [
        "https://mirror.bazel.build/zlib.net/zlib-1.2.11.tar.gz",
        "https://zlib.net/zlib-1.2.11.tar.gz",
    ],
)

http_archive(
    name = "rules_python",
    sha256 = "9fcf91dbcc31fde6d1edb15f117246d912c33c36f44cf681976bd886538deba6",
    strip_prefix = "rules_python-0.8.0",
    url = "https://github.com/bazelbuild/rules_python/archive/refs/tags/0.8.0.tar.gz",
)

http_archive(
    name = "rules_pkg",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_pkg/releases/download/0.7.0/rules_pkg-0.7.0.tar.gz",
        "https://github.com/bazelbuild/rules_pkg/releases/download/0.7.0/rules_pkg-0.7.0.tar.gz",
    ],
    sha256 = "8a298e832762eda1830597d64fe7db58178aa84cd5926d76d5b744d6558941c2",
)

http_archive(
  name = "com_google_absl",
  urls = ["https://github.com/abseil/abseil-cpp/archive/c86347d4cec43074e64e225a8753728f4bfc5ed6.zip"],
  strip_prefix = "abseil-cpp-c86347d4cec43074e64e225a8753728f4bfc5ed6",
  sha256 = "d468586a90059921b9e1eeee81fd88283a47dc3c699b01b9763e58c87d5a2401",
)

http_archive(
  name = "com_google_googletest",
  urls = ["https://github.com/google/googletest/archive/d166e09483845b9b6a658dccc3d3dbb293676b62.zip"],
  strip_prefix = "googletest-d166e09483845b9b6a658dccc3d3dbb293676b62",
  sha256 = "d27641a853c49d3e8d7b9bbced1ceb861336134cd148bf6db720a40ccde66516",
)

http_archive(
    name = "github_nlohmann_json",
    urls = ["https://github.com/nlohmann/json/archive/e4643d1f1b03fc7a1d7b65f17e012ca93680cad8.zip"],
    sha256 = "1372f344749dd5dad917f96f49d9bf727495297f46fcb7ab998a1176d9504791",
    strip_prefix = "json-e4643d1f1b03fc7a1d7b65f17e012ca93680cad8",
    build_file = "//third_party:nlohmann_json.BUILD",
)

http_archive(
    name = "com_google_protobuf",
    urls = ["https://github.com/protocolbuffers/protobuf/archive/82d04fabd9c6065bafa56161c73e04432818a0ca.zip"],
    sha256 = "e6a44f160bfe7f3a969a7cf58a655ee947ccfe2f5ed61cbcb8fe6c997392831a",
    strip_prefix = "protobuf-82d04fabd9c6065bafa56161c73e04432818a0ca"
)

http_archive(
    name = "rules_proto",
    sha256 = "66bfdf8782796239d3875d37e7de19b1d94301e8972b3cbd2446b332429b4df1",
    strip_prefix = "rules_proto-4.0.0",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_proto/archive/refs/tags/4.0.0.tar.gz",
        "https://github.com/bazelbuild/rules_proto/archive/refs/tags/4.0.0.tar.gz",
    ],
)
