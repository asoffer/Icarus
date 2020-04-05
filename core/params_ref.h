#ifndef ICARUS_CORE_PARAMS_REF_H
#define ICARUS_CORE_PARAMS_REF_H

#include <cstdint>

#include "base/lazy_convert.h"
#include "core/params.h"

namespace core {

template <typename T>
struct ParamsRef {
  using value_type     = Param<T>;
  using const_iterator = typename std::vector<Param<T>>::const_iterator;

  explicit ParamsRef() : params_(nullptr) {}
  /* implicit */ ParamsRef(Params<T>&&) = delete;
  /* implicit */ ParamsRef(Params<T> const& params) : params_(&params) {}

  void set(size_t index, Param<T> param) {
    ASSERT((*params_)[index].name == "");
    params_->lookup_.emplace(param.name, index);
    (*params_)[index] = std::move(param);
  }

  constexpr size_t size() const { return params_->size() - lhs_trim_; }
  constexpr bool empty() const { return size() == 0u; }

  constexpr auto begin() const { return params_->begin() + lhs_trim_; }
  constexpr auto end() const { return params_->end(); }

  constexpr auto begin() { return params_->begin() + lhs_trim_; }
  constexpr auto end() { return params_->end(); }

  template <typename Fn>
  auto Transform(Fn&& fn) const {
    using out_t = decltype(fn((*params_)[0].value));
    Params<out_t> result;
    result.reserve(size());
    size_t i = 0;
    for (auto const& param : *this) {
      result.append(param.name, fn(param.value), param.flags);
    }
    return result;
  }

  // Negative indices mean out of range.
  int index(std::string_view name) {
    auto* idx = params_->at_or_null(name);
    return (idx ? *idx : -1) - lhs_trim_;
  }

  explicit operator Params<T>() const {
    return Transform([](auto const& p) { return p; });
  }

  Param<T> const& operator[](size_t i) const& {
    return (*params_)[i + lhs_trim_];
  }

  void remove_prefix(size_t n) { lhs_trim_ += n; }

 private:
  Params<T> const* params_ = nullptr;
  uint32_t lhs_trim_ = 0;
};

}  // namespace core

#endif  // ICARUS_CORE_PARAMS_REF_H
