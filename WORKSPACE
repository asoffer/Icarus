workspace(name = "asoffer_icarus")
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

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
