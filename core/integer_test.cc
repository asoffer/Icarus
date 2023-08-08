#include "core/integer.h"

#include "core/serialize.h"
#include "nth/test/test.h"

namespace nth::test {

// TODO: Provie a proper implementation.
auto ElementsAre = ::nth::ElementsAreSequentially;

}  // namespace nth::test

namespace core {

using ::nth::test::ElementsAre;

NTH_TEST("integer/default-construction") {
  using I = GenericInteger<0>;
  I i;
  NTH_EXPECT(i == 0);
}

NTH_TEST("integer/arithmetic", absl::int128 x, absl::int128 y) {
  using I = GenericInteger<0>;
  NTH_EXPECT(I(x) + I(y) == x + y);
  NTH_EXPECT(I(x) - I(y) == x - y);
  NTH_EXPECT(I(x) * I(y) == x * y);
  if (y != 0) {
    NTH_EXPECT(I(x) / I(y) == x / y);
    NTH_EXPECT(I(x) % I(y) == x % y);
  }
}

NTH_INVOKE_TEST("integer/arithmetic") {
  for (int i : {-20, -1, 0, 1, 20}) {
    for (int j : {-20, -1, 0, 1, 20}) { co_yield nth::TestArguments{i, j}; }
  }
}

NTH_TEST("integer-table/round-trip") {
  using I1 = GenericInteger<1>;

  (void)(I1(3) + I1(7));

  serialization::IntegerTable table;

  Serializer<internal_integer::InternTable> s(table);
  IcarusSerialize(s, internal_integer::Table<1>);
  Deserializer<internal_integer::InternTable> d(table);
  IcarusDeserialize(d, internal_integer::Table<2>);

  (void)(I1(2) + I1(7));

  NTH_EXPECT(internal_integer::Table<1> >>= not ElementsAre(3, 7, 10));
  NTH_EXPECT(internal_integer::Table<2> >>= ElementsAre(3, 7, 10));
}

}  // namespace core
