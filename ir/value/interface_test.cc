#include "ir/value/interface.h"

#include "gtest/gtest.h"
#include "type/primitive.h"

namespace ir {
namespace {

TEST(Interface, Equality) {
  InterfaceManager im;
  EXPECT_EQ(im.Precisely(type::Bool), im.Precisely(type::Bool));
  EXPECT_NE(im.Precisely(type::Bool), im.Precisely(type::Char));
  Interface ib = im.Precisely(type::Bool);
  Interface ic = im.Precisely(type::Char);
  EXPECT_EQ(ib, im.Precisely(type::Bool));
  EXPECT_NE(ic, im.Precisely(type::Bool));
  EXPECT_EQ(ic, im.Precisely(type::Char));
  EXPECT_EQ(im.Precisely(type::Bool), ib);
  EXPECT_NE(im.Precisely(type::Bool), ic);
  EXPECT_EQ(im.Precisely(type::Char), ic);

  Interface callable = im.Callable(core::Arguments<Interface>());
  EXPECT_EQ(callable, im.Callable(core::Arguments<Interface>()));
  EXPECT_EQ(im.Callable(core::Arguments<Interface>()), callable);

  callable = im.Callable(core::Arguments<Interface>({ib}, {}));
  EXPECT_EQ(callable, im.Callable(core::Arguments<Interface>({ib}, {})));
  EXPECT_EQ(im.Callable(core::Arguments<Interface>({ib}, {})), callable);
}

TEST(PreciseInterface, BindsTo) {
  InterfaceManager im;
  EXPECT_TRUE(im.BindsTo(im.Precisely(type::Bool), type::Bool));
  EXPECT_FALSE(im.BindsTo(im.Precisely(type::Char), type::Bool));
  EXPECT_FALSE(im.BindsTo(im.Precisely(type::Bool), type::Char));

  auto ic =
      im.Callable(core::Arguments<Interface>({im.Precisely(type::I64)}, {}));
  auto fn_i64_to_u64 = type::Func(
      {core::AnonymousParameter(type::QualType::NonConstant(type::I64))},
      {type::U64});
  auto fn_u64_to_u64 = type::Func(
      {core::AnonymousParameter(type::QualType::NonConstant(type::U64))},
      {type::U64});
  auto fn_named_i64_to_u64 = type::Func(
      {core::Parameter<type::QualType>{
          .name = "n", .value = type::QualType::NonConstant(type::I64)}},
      {type::U64});
  auto fn_i64_with_defaults_to_u64 = type::Func(
      {core::Parameter<type::QualType>{
           .name = "n", .value = type::QualType::NonConstant(type::I64)},
       core::Parameter<type::QualType>{
           .name  = "n",
           .value = type::QualType::NonConstant(type::I64),
           .flags = core::ParameterFlags::HasDefault()}},
      {type::U64});

  EXPECT_TRUE(im.BindsTo(ic, fn_i64_to_u64));
  EXPECT_FALSE(im.BindsTo(ic, fn_u64_to_u64));
  EXPECT_TRUE(im.BindsTo(ic, fn_named_i64_to_u64));
  EXPECT_TRUE(im.BindsTo(ic, fn_i64_with_defaults_to_u64));
}

}  // namespace
}  // namespace ir
