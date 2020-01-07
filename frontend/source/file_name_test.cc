#include "frontend/source/file_name.h"

#include "test/catch.h"

namespace frontend {
namespace {

TEST_CASE("Canonicalization") {
  SECTION("Different files") {
    REQUIRE_ASSIGN(auto name1, CanonicalFileName::Make(FileName{
                                   "frontend/source/testdata/empty_file.txt"}));
    REQUIRE_ASSIGN(auto name2,
                   CanonicalFileName::Make(
                       FileName{"frontend/source/testdata/one_line_file.txt"}));
    CHECK(name1 != name2);
  }

  SECTION("Same file") {
    REQUIRE_ASSIGN(auto name1, CanonicalFileName::Make(FileName{
                                   "frontend/source/testdata/empty_file.txt"}));
    REQUIRE_ASSIGN(auto name2, CanonicalFileName::Make(FileName{
                                   "frontend/source/testdata/./../../"
                                   "source/./testdata/empty_file.txt"}));
    CHECK(name1 == name2);
  }
}

TEST_CASE("Open") {
  REQUIRE_ASSIGN(auto name, CanonicalFileName::Make(FileName{
                                "frontend/source/testdata/one_line_file.txt"}));
  auto file         = name.OpenReadOnly();
  char *buf         = nullptr;
  size_t n          = 0;
  ssize_t num_chars = getdelim(&buf, &n, '\n', file.get());
  CHECK(buf == std::string{"hello\n"});
  std::free(buf);
}

}  // namespace
}  // namespace frontend
