#include "precompiled/serialize.h"

#include "base/meta.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "type/function.h"
#include "type/primitive.h"

namespace precompiled {
using ::testing::Optional;

template <typename T>
std::optional<T> RoundTrip(T value, type::Type type) {
  Value proto_value;
  auto [index, inserted] = GlobalTypeSystem.insert(type);
  proto_value.set_type_id(index);
  {
    ir::CompleteResultBuffer buffer;
    buffer.append(value);
    SerializeValue(GlobalTypeSystem, type, buffer[0], proto_value);
  }
  ir::CompleteResultBuffer out =
      DeserializeValue(GlobalTypeSystem, proto_value);
  return out[0].get<T>();
}

template <typename T>
using SignedIntegers = ::testing::Test;
TYPED_TEST_SUITE_P(SignedIntegers);
TYPED_TEST_P(SignedIntegers, RoundTripTest) {
  constexpr auto type = base::meta<TypeParam>;
  type::Type t;
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
  type::Type t;
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
  EXPECT_THAT(RoundTrip(true, type::Bool), Optional(true));
  EXPECT_THAT(RoundTrip(false, type::Bool), Optional(false));
  EXPECT_THAT(RoundTrip(ir::Char('a'), Char), Optional(ir::Char('a')));
  EXPECT_THAT(RoundTrip(ir::Char('\0'), Char), Optional(ir::Char('\0')));
  EXPECT_THAT(RoundTrip(ir::Char('!'), Char), Optional(ir::Char('!')));
}

TEST(Types, RoundTripTest) {
  EXPECT_THAT(RoundTrip(type::I8, Type_), Optional(type::I8));
  EXPECT_THAT(RoundTrip(type::I16, Type_), Optional(type::I16));
  EXPECT_THAT(RoundTrip(type::I32, Type_), Optional(type::I32));
  EXPECT_THAT(RoundTrip(type::I64, Type_), Optional(type::I64));
  EXPECT_THAT(RoundTrip(type::U8, Type_), Optional(type::U8));
  EXPECT_THAT(RoundTrip(type::U16, Type_), Optional(type::U16));
  EXPECT_THAT(RoundTrip(type::U32, Type_), Optional(type::U32));
  EXPECT_THAT(RoundTrip(type::U64, Type_), Optional(type::U64));
  EXPECT_THAT(RoundTrip(type::Bool, Type_), Optional(type::Bool));
  EXPECT_THAT(RoundTrip(type::Char, Type_), Optional(type::Char));
  EXPECT_THAT(RoundTrip(type::Type_, Type_), Optional(type::Type_));

  EXPECT_THAT(RoundTrip(type::Type(type::Func({}, {})), Type_),
              Optional(type::Func({}, {})));
  EXPECT_THAT(RoundTrip(type::Type(type::Func({}, {type::Bool})), Type_),
              Optional(type::Func({}, {type::Bool})));
  EXPECT_THAT(
      RoundTrip(type::Type(type::Func(
                    {core::Parameter<type::QualType>{
                        .name = "", .value = type::QualType::Constant(I32)}},
                    {type::Bool})),
                Type_),
      Optional(
          type::Func({core::Parameter<type::QualType>{
                         .name = "", .value = type::QualType::Constant(I32)}},
                     {type::Bool})));
  EXPECT_THAT(
      RoundTrip(type::Type(type::Func(
                    {core::Parameter<type::QualType>{
                         .name = "", .value = type::QualType::NonConstant(F32)},
                     core::Parameter<type::QualType>{
                         .name = "", .value = type::QualType::Constant(I32)}},
                    {type::Bool})),
                Type_),
      Optional(type::Func(
          {core::Parameter<type::QualType>{
               .name = "", .value = type::QualType::NonConstant(F32)},
           core::Parameter<type::QualType>{
               .name = "", .value = type::QualType::Constant(I32)}},
          {type::Bool})));
}

}  // namespace precompiled
