#include "type/interface/interface.h"
#include "absl/container/node_hash_set.h"
#include "base/global.h"
#include "type/cast.h"

namespace interface {

struct Interface::Impl {
  virtual ~Impl() {}
  virtual bool SatisfiedBy(type::Type) const = 0;
};

bool Interface::SatisfiedBy(type::Type t) const {
  return impl_->SatisfiedBy(t);
}

namespace {

struct Just : Interface::Impl {
  explicit Just(type::Type t) : type_(t) {}
  bool SatisfiedBy(type::Type t) const override { return t == type_; }

  template <typename H>
  friend H AbslHashValue(H h, Just j) {
    return H::combine(std::move(h), j.type_);
  }

  friend bool operator==(Just lhs, Just rhs) {
    return lhs.type_ == rhs.type_;
  }

 private:
  type::Type type_;
};

base::Global<absl::node_hash_set<Just>> just_;

struct ConvertsTo : Interface::Impl {
  explicit ConvertsTo(type::Type t) : type_(t) {}
  bool SatisfiedBy(type::Type t) const override {
    return type::CanCastImplicitly(t, type_);
  }

  template <typename H>
  friend H AbslHashValue(H h, ConvertsTo c) {
    return H::combine(std::move(h), c.type_);
  }

  friend bool operator==(ConvertsTo lhs, ConvertsTo rhs) {
    return lhs.type_ == rhs.type_;
  }

 private:
  type::Type type_;
};

base::Global<absl::node_hash_set<ConvertsTo>> conversions_;

}  // namespace

Interface Interface::Just(type::Type t) {
  return Interface(&*just_.lock()->emplace(t).first);
}

Interface Interface::ConvertsTo(type::Type t) {
  return Interface(&*conversions_.lock()->emplace(t).first);
}

}  // namespace interface
