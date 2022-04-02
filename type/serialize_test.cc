#include "type/serialize.h"

#include "base/meta.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "type/function.h"
#include "type/primitive.h"

namespace type {
using ::testing::Optional;

template <typename T>
std::optional<T> RoundTrip(T value, Type type) {
  GlobalTypeSystem.insert(type);
  base::flyweight_map<std::pair<std::string, Function const *>, void (*)()>
      foreign_fn_map;
  std::string s;
  {
    ir::CompleteResultBuffer buffer;
    buffer.append(value);
    SerializeValue(GlobalTypeSystem, type, buffer[0], s);
  }
  auto span = absl::MakeConstSpan(reinterpret_cast<std::byte const *>(s.data()),
                                  s.size());
  ir::CompleteResultBuffer out;
  ssize_t num_read =
      DeserializeValue(type, span, out, foreign_fn_map, GlobalTypeSystem);
  if (num_read < 0) { return std::nullopt; }
  return out[0].get<T>();
}

template <typename T>
using SignedIntegers = ::testing::Test;
TYPED_TEST_SUITE_P(SignedIntegers);
TYPED_TEST_P(SignedIntegers, RoundTripTest) {
  constexpr auto type = base::meta<TypeParam>;
  Type t;
  if constexpr (type == base::meta<int8_t>) { t = I8; }
  if constexpr (type == base::meta<int16_t>) { t = I16; }
  if constexpr (type == base::meta<int32_t>) { t = I32; }
  if constexpr (type == base::meta<int64_t>) { t = I64; }
  for (TypeParam n : {TypeParam{0}, TypeParam{1}, TypeParam{-1}, TypeParam{15},
                      TypeParam{-15}, std::numeric_limits<TypeParam>::min(),
                      std::numeric_limits<TypeParam>::max()}) {
    EXPECT_THAT(RoundTrip(n, t), Optional(n));
  }
}

using SignedIntegerTypes = ::testing::Types<int8_t, int16_t, int32_t, int64_t>;
REGISTER_TYPED_TEST_SUITE_P(SignedIntegers, RoundTripTest);
INSTANTIATE_TYPED_TEST_SUITE_P(RoundTripTest, SignedIntegers,
                               SignedIntegerTypes);

template <typename T>
using UnsignedIntegers = ::testing::Test;
TYPED_TEST_SUITE_P(UnsignedIntegers);
TYPED_TEST_P(UnsignedIntegers, RoundTripTest) {
  constexpr auto type = base::meta<TypeParam>;
  Type t;
  if constexpr (type == base::meta<uint8_t>) { t = U8; }
  if constexpr (type == base::meta<uint16_t>) { t = U16; }
  if constexpr (type == base::meta<uint32_t>) { t = U32; }
  if constexpr (type == base::meta<uint64_t>) { t = U64; }
  for (TypeParam n : {TypeParam{0}, TypeParam{1}, TypeParam{15},
                      std::numeric_limits<TypeParam>::max()}) {
    EXPECT_THAT(RoundTrip(n, t), Optional(n));
  }
}

using UnsignedIntegerTypes =
    ::testing::Types<uint8_t, uint16_t, uint32_t, uint64_t>;
REGISTER_TYPED_TEST_SUITE_P(UnsignedIntegers, RoundTripTest);
INSTANTIATE_TYPED_TEST_SUITE_P(RoundTripTest, UnsignedIntegers,
                               UnsignedIntegerTypes);

TEST(MiscellaneousPrimitives, RoundTripTest) {
  EXPECT_THAT(RoundTrip(true, Bool), Optional(true));
  EXPECT_THAT(RoundTrip(false, Bool), Optional(false));
  EXPECT_THAT(RoundTrip(ir::Char('a'), Char), Optional(ir::Char('a')));
  EXPECT_THAT(RoundTrip(ir::Char('\0'), Char), Optional(ir::Char('\0')));
  EXPECT_THAT(RoundTrip(ir::Char('!'), Char), Optional(ir::Char('!')));
}

TEST(Types, RoundTripTest) {
  EXPECT_THAT(RoundTrip(I8, Type_), Optional(I8));
  EXPECT_THAT(RoundTrip(I16, Type_), Optional(I16));
  EXPECT_THAT(RoundTrip(I32, Type_), Optional(I32));
  EXPECT_THAT(RoundTrip(I64, Type_), Optional(I64));
  EXPECT_THAT(RoundTrip(U8, Type_), Optional(U8));
  EXPECT_THAT(RoundTrip(U16, Type_), Optional(U16));
  EXPECT_THAT(RoundTrip(U32, Type_), Optional(U32));
  EXPECT_THAT(RoundTrip(U64, Type_), Optional(U64));
  EXPECT_THAT(RoundTrip(Bool, Type_), Optional(Bool));
  EXPECT_THAT(RoundTrip(Char, Type_), Optional(Char));
  EXPECT_THAT(RoundTrip(Type_, Type_), Optional(Type_));

  EXPECT_THAT(RoundTrip(Type(Func({}, {})), Type_), Optional(Func({}, {})));
  EXPECT_THAT(RoundTrip(Type(Func({}, {Bool})), Type_),
              Optional(Func({}, {Bool})));
  EXPECT_THAT(
      RoundTrip(Type(Func({core::Parameter<QualType>{
                              .name = "", .value = QualType::Constant(I32)}},
                          {Bool})),
                Type_),
      Optional(Func({core::Parameter<QualType>{
                        .name = "", .value = QualType::Constant(I32)}},
                    {Bool})));
  EXPECT_THAT(
      RoundTrip(Type(Func({core::Parameter<QualType>{
                               .name = "", .value = QualType::NonConstant(F32)},
                           core::Parameter<QualType>{
                               .name = "", .value = QualType::Constant(I32)}},
                          {Bool})),
                Type_),
      Optional(Func({core::Parameter<QualType>{
                         .name = "", .value = QualType::NonConstant(F32)},
                     core::Parameter<QualType>{
                         .name = "", .value = QualType::Constant(I32)}},
                    {Bool})));
}

}  // namespace type
