#include "module/unique_id.h"

#include "nth/test/test.h"

namespace module {
namespace {

NTH_TEST("UniqueId/default-construction") {
  UniqueId u;
  NTH_EXPECT(nth::Trace<"u">(u).value() == "");
}

NTH_TEST("UniqueId/string-construction", auto const& id) {
  std::string copy([&]() -> std::string_view {
    if constexpr (std::is_same_v<std::decay_t<decltype(id)>, char const*>) {
      return id;
    } else {
      return * id;
    }
  }());

  UniqueId u1(id);
  auto t1 = nth::Trace<"u1">(u1);
  UniqueId u2(copy);
  auto t2 = nth::Trace<"u2">(u2);
  NTH_EXPECT(t1.value() == copy);
  NTH_EXPECT(t2.value() == copy);
  NTH_EXPECT(t1 == t2);
  NTH_EXPECT(not(t1 != t2));
}

NTH_INVOKE_TEST("UniqueId/string-construction") {
  co_yield "";
  co_yield "something";
  co_yield "something-else";

  std::string s = "";
  co_yield &s;
  s = "something";
  co_yield &s;
  s = "something-entirely-new";
  co_yield &s;
}


NTH_TEST("UniqueId/invalid") {
  NTH_EXPECT(UniqueId::Invalid() == UniqueId::Invalid());
  UniqueId u;
  NTH_EXPECT(u == UniqueId::Invalid());
}

}  // namespace
}  // namespace module
