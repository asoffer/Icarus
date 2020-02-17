#ifndef ICARUS_CORE_PARAMS_REF_H
#define ICARUS_CORE_PARAMS_REF_H

#include "core/params.h"

namespace core {

template <typename T>
struct ParamsRef {
  using value_type     = Param<T>;
  using const_iterator = typename std::vector<Param<T>>::const_iterator;

  /* implicit */ ParamsRef(Params<T>& params) : params_(&params) {}

  void set(size_t index, Param<T> param) {
    ASSERT(params_[index].name == "");
    params_->lookup_.emplace(param.name, index);
    (*params_)[index] = std::move(param);
  }

  constexpr size_t size() const { return params_->size() - lhs_trim_; }
  constexpr bool empty() const { return size() == 0u; }

  constexpr auto begin() const { return params_->begin() + lhs_trim_; }
  constexpr auto end() const { return params_->end(); }

  constexpr auto begin() { return params_->begin() + lhs_trim_; }
  constexpr auto end() { return params_->end(); }

  size_t* at_or_null(std::string_view s) {
    auto iter = params_->lookup_.find(s);
    if (iter == params_->lookup_.end()) { return nullptr; }
    if (iter->second < lhs_trim_) { return nullptr; }
    return &iter->second;
  }

  size_t const* at_or_null(std::string_view s) const {
    auto iter = params_->lookup_.find(s);
    if (iter == params_->lookup_.end()) { return nullptr; }
    if (iter->second < lhs_trim_) { return nullptr; }
    return &iter->second;
  }

  Param<T> const& operator[](size_t i) const& { return params_[i + lhs_trim_]; }

  void remove_prefix(size_t n) { lhs_trim_ += n; }

 private:
  Params<T>* params_ = nullptr;
  uint32_t lhs_trim_ = 0;
};

}  // namespace core

#endif  // ICARUS_CORE_PARAMS_REF_H
