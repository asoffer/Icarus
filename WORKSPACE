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
  urls = ["https://github.com/abseil/abseil-cpp/archive/cde2f0eaaed3fb8581511cb5719d39172a5a2d81.zip"],
  strip_prefix = "abseil-cpp-cde2f0eaaed3fb8581511cb5719d39172a5a2d81",
  sha256 = "da69001e0946624eb3f608b0657d5201f60cac7d242dec4cae4b5a1f4fada0e7",
)

http_archive(
  name = "com_google_googletest",
  urls = ["https://github.com/google/googletest/archive/b53547bf01ee6d5c547bc539a498c49bc6027169.zip"],
  strip_prefix = "googletest-b53547bf01ee6d5c547bc539a498c49bc6027169",
  sha256 = "86edb67028041c5b8f5011794b5ac6e5c1db96c09b1187c9173be62300f2fae2",
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

http_archive(
  name = "asoffer_nth",
  urls = ["https://github.com/asoffer/nth/archive/a08b4f4709969f7be9099f0772343f962edec83f.zip"],
  strip_prefix = "nth-a08b4f4709969f7be9099f0772343f962edec83f",
  sha256 = "073a6b9ecda1a310037b11bf82194d33f52e8e29ab98fb7e5be28939112c8166",
)

http_archive(
  name = "asoffer_jasmin",
  urls = ["https://github.com/asoffer/jasmin/archive/f035ef0752c09846331c8deb2109e4ebfce83200.zip"],
  strip_prefix = "jasmin-f035ef0752c09846331c8deb2109e4ebfce83200",
  sha256 = "8b8ee981e693f6608c53012d09588b5479d724e530ac1b79a46c59c40c734d5a",
)
