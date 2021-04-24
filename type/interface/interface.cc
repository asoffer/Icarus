#include "type/interface/interface.h"
#include "absl/container/node_hash_set.h"
#include "base/global.h"
#include "core/call.h"
#include "type/callable.h"
#include "type/cast.h"
#include "type/function.h"

namespace interface {

struct Interface::Impl {
  virtual ~Impl() {}
  virtual void stream(std::ostream &os) const = 0;
  virtual bool SatisfiedBy(type::Type) const  = 0;
};

bool Interface::SatisfiedBy(type::Type t) const {
  return impl_->SatisfiedBy(t);
}

std::ostream &operator<<(std::ostream &os, Interface i) {
  i.impl_->stream(os);
  return os;
}

namespace {

struct Just : Interface::Impl {
  explicit Just(type::Type t) : type_(t) {}
  bool SatisfiedBy(type::Type t) const override { return t == type_; }

  void stream(std::ostream &os) const override {
    os << "Just(" << type_ << ")";
  }

  template <typename H>
  friend H AbslHashValue(H h, Just j) {
    return H::combine(std::move(h), j.type_);
  }

  friend bool operator==(Just lhs, Just rhs) { return lhs.type_ == rhs.type_; }

 private:
  type::Type type_;
};

base::Global<absl::node_hash_set<Just>> just_;

struct ConvertsTo : Interface::Impl {
  explicit ConvertsTo(type::Type t) : type_(t) {}
  bool SatisfiedBy(type::Type t) const override {
    return type::CanCastImplicitly(t, type_);
  }

  void stream(std::ostream &os) const override {
    os << "ConvertsTo(" << type_ << ")";
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

struct Callable : Interface::Impl {
  explicit Callable(core::Arguments<type::Type> const &args) : args_(args) {}
  bool SatisfiedBy(type::Type t) const override {
    // TODO: If we support call-operators in the future, make sure this covers
    // structs as well.
    auto *f = t.if_as<type::Function>();
    if (not f) { return false; }
    return core::IsCallable(f->params(), args_,
                            [](type::Type arg, type::QualType param) {
                              return type::CanCastImplicitly(arg, param.type());
                            });
  }

  void stream(std::ostream &os) const override {
    char const *sep = "";
    os << "Callable(";
    args_.ApplyWithIndex([&](auto &&index, type::Type t) {
      os << std::exchange(sep, ", ");
      if constexpr (base::meta<std::decay_t<decltype(index)>> !=
                    base::meta<size_t>) {
        os << index << " = ";
      }
      os << t;
    });
    os << ")";
  }

  template <typename H>
  friend H AbslHashValue(H h, Callable c) {
    return H::combine(std::move(h), c.args_);
  }

  friend bool operator==(Callable const &lhs, Callable const &rhs) {
    return lhs.args_ == rhs.args_;
  }

 private:
  core::Arguments<type::Type> args_;
};

base::Global<absl::node_hash_set<Callable>> callables_;

}  // namespace

Interface Interface::Just(type::Type t) {
  return Interface(&*just_.lock()->emplace(t).first);
}

Interface Interface::ConvertsTo(type::Type t) {
  return Interface(&*conversions_.lock()->emplace(t).first);
}

Interface Interface::Callable(core::Arguments<type::Type> const &args) {
  return Interface(&*callables_.lock()->emplace(args).first);
}

}  // namespace interface
