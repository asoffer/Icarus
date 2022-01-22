#ifndef ICARUS_TYPE_FAKE_H
#define ICARUS_TYPE_FAKE_H

#include <string>

#include "absl/strings/str_format.h"
#include "base/no_destructor.h"
#include "core/arch.h"

namespace type {

struct FakeType : type::LegacyType {
  explicit FakeType(core::Bytes b, core::Alignment a)
      : type::LegacyType(IndexOf<FakeType>(),
                         type::LegacyType::Flags{
                             .is_default_initializable = 0,
                             .is_copyable              = 0,
                             .is_movable               = 0,
                             .has_destructor           = 0,
                         }),
        b_(b),
        a_(a) {}

  Completeness completeness() const override { return Completeness::Complete; }

  void WriteTo(std::string *out) const override {
    absl::StrAppendFormat(out, "Fake(%u, %u)", b_.value(), a_.value());
  }
  core::Bytes bytes(core::Arch const &arch) const override { return b_; }
  core::Alignment alignment(core::Arch const &arch) const override {
    return a_;
  }

 private:
  core::Bytes b_;
  core::Alignment a_;
};

template <size_t Bytes, size_t Align>
FakeType const *Fake() {
  static base::NoDestructor<FakeType> t(core::Bytes{Bytes},
                                        core::Alignment{Align});
  return &*t;
}

}  // namespace type

#endif  // ICARUS_TYPE_FAKE_H
