#include "type/array.h"

#include "absl/strings/str_format.h"
#include "base/no_destructor.h"
#include "gtest/gtest.h"

namespace {

struct FakeType : type::Type {
  explicit FakeType(core::Bytes b, core::Alignment a)
      : type::Type(type::Type::Flags{
            .is_default_initializable = 0,
            .is_copyable              = 0,
            .is_movable               = 0,
            .has_destructor           = 0,
        }),
        b_(b),
        a_(a) {}
  ~FakeType() override {}

  void Accept(type::VisitorBase *visitor, void *ret,
              void *arg_tuple) const override {
    visitor->ErasedVisit(this, ret, arg_tuple);
  }

  void WriteTo(std::string *out) const {
    absl::StrAppendFormat(out, "FakeType(%u, %u)", b_.value(), a_.value());
  }
  core::Bytes bytes(core::Arch const &arch) const override { return b_; }
  core::Alignment alignment(core::Arch const &arch) const override { return a_; }

 private:
  core::Bytes b_;
  core::Alignment a_;
};

template <size_t Bytes, size_t Align>
FakeType const *MyFakeType() {
  static base::NoDestructor<FakeType> t(core::Bytes{Bytes},
                                        core::Alignment{Align});
  return &*t;
}

TEST(Array, Construction) {
  EXPECT_EQ(Arr(3, MyFakeType<1, 2>()), Arr(3, MyFakeType<1, 2>()));
  EXPECT_NE(Arr(3, MyFakeType<1, 2>()), Arr(4, MyFakeType<1, 2>()));
  EXPECT_NE(Arr(3, MyFakeType<1, 2>()), Arr(3, MyFakeType<2, 1>()));

  EXPECT_EQ(Arr(3, MyFakeType<1, 2>())->bytes(core::Host), core::Bytes{6});
  EXPECT_EQ(Arr(3, MyFakeType<3, 2>())->bytes(core::Host), core::Bytes{12});
  EXPECT_EQ(Arr(3, MyFakeType<3, 4>())->bytes(core::Host), core::Bytes{12});
  EXPECT_EQ(Arr(3, MyFakeType<1, 2>())->alignment(core::Host),
            core::Alignment{2});
}



TEST(Array, ToString) {
  EXPECT_EQ(type::Arr(3, MyFakeType<1, 2>())->to_string(), "[3; FakeType(1, 2)]");
  EXPECT_EQ(type::Arr(4, type::Arr(3, MyFakeType<1, 2>()))->to_string(),
            "[4, 3; FakeType(1, 2)]");
}

}  // namespace
