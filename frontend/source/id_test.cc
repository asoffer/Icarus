#include "frontend/source/id.h"

#include "test/catch.h"

namespace frontend {
namespace {

TEST_CASE("Make returns unique ids") {
  auto id1 = SourceId::Make();
  auto id2 = SourceId::Make();
  CHECK_FALSE(id1 == id2);
  CHECK(id1 != id2);
}

TEST_CASE("Copied SourceIds compare equal") {
  auto id = SourceId::Make();
  CHECK(id == id);
  CHECK_FALSE(id != id);

  auto id_copy = id;
  CHECK(id_copy == id);
  CHECK_FALSE(id_copy != id);
}

}  // namespace
}  // namespace frontend
