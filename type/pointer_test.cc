#include "type/pointer.h"

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

TEST(Pointer, Construction) {
  type::Type const * t1 = MyFakeType<1, 2>();
  type::Type const * t2 = MyFakeType<2, 2>();
  EXPECT_EQ(type::Ptr(t1), type::Ptr(t1));
  EXPECT_EQ(type::Ptr(t2), type::Ptr(t2));
  EXPECT_NE(type::Ptr(t1), type::Ptr(t2));

  EXPECT_EQ(type::Ptr(t1)->bytes(core::Host), core::Host.pointer().bytes());
  EXPECT_EQ(type::Ptr(t1)->alignment(core::Host), core::Host.pointer().alignment());
  EXPECT_EQ(type::Ptr(t2)->bytes(core::Host), core::Host.pointer().bytes());
  EXPECT_EQ(type::Ptr(t2)->alignment(core::Host), core::Host.pointer().alignment());
}

TEST(BufferPointer, Construction) {
  type::Type const * t1 = MyFakeType<1, 2>();
  type::Type const * t2 = MyFakeType<2, 2>();
  EXPECT_EQ(type::BufPtr(t1), type::BufPtr(t1));
  EXPECT_EQ(type::BufPtr(t2), type::BufPtr(t2));
  EXPECT_NE(type::BufPtr(t1), type::BufPtr(t2));

  EXPECT_EQ(type::BufPtr(t1)->bytes(core::Host), core::Host.pointer().bytes());
  EXPECT_EQ(type::BufPtr(t1)->alignment(core::Host), core::Host.pointer().alignment());
  EXPECT_EQ(type::BufPtr(t2)->bytes(core::Host), core::Host.pointer().bytes());
  EXPECT_EQ(type::BufPtr(t2)->alignment(core::Host), core::Host.pointer().alignment());
}

TEST(Pointer, ToString) {
  type::Type const *t = MyFakeType<1, 2>();
  EXPECT_EQ(type::Ptr(t)->to_string(), "*(FakeType(1, 2))");
  EXPECT_EQ(type::BufPtr(t)->to_string(), "[*](FakeType(1, 2))");
  EXPECT_EQ(type::Ptr(type::Ptr(t))->to_string(), "*(*(FakeType(1, 2)))");
  EXPECT_EQ(type::Ptr(type::BufPtr(t))->to_string(), "*([*](FakeType(1, 2)))");
  EXPECT_EQ(type::BufPtr(type::Ptr(t))->to_string(), "[*](*(FakeType(1, 2)))");
  EXPECT_EQ(type::BufPtr(type::BufPtr(t))->to_string(),
            "[*]([*](FakeType(1, 2)))");
}

}  // namespace
