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

// Returns true if and only if a callable with `params` can be called with
// `args`.
template <typename T, typename U, typename ConvertibleFn>
bool IsCallable(ParamsRef<T> params, FnArgs<U> const& args, ConvertibleFn fn) {
  if (params.size() < args.size()) {
    DEBUG_LOG("core::IsCallable")
    ("IsCallable = false due to size mismatch (", params.size(), " vs ",
     args.size());
    return false;
  }

  for (size_t i = 0; i < args.pos().size(); ++i) {
    if (not fn(args.pos()[i], params[i].value)) {
      DEBUG_LOG("core::IsCallable")
      ("IsCallable = false due to convertible failure at ", i);
      return false;
    }
  }

  for (auto const& [name, type] : args.named()) {
    int index = params.index(name);
    if (index < 0) {
      DEBUG_LOG("core::IsCallable")
      ("No such parameter named \"", name, "\"");
      return false;
    }

    if (not fn(type, params[index].value)) {
      DEBUG_LOG("core::IsCallable")
      ("IsCallable = false due to convertible failure on \"", name, "\"");
      return false;
    }
  }

  for (size_t i = args.pos().size(); i < params.size(); ++i) {
    auto const& param = params[i];
    if (param.flags & HAS_DEFAULT) { continue; }
    if (args.at_or_null(param.name) == nullptr) {
      DEBUG_LOG("core::IsCallable")
      ("No argument for non-default parameter named \"", param.name, "\"");
      return false;
    }
  }

  DEBUG_LOG("core::IsCallable")("Yes, it's callable");
  return true;
}

// For each parameter in `params` for which `args` has chosen to use the default
// value, update `args` to contain the appropriate default value, as chosen by
// `fn(param.value)`.
//
// TODO this offset is a hack to get scope state working. Simplify.
template <typename P, typename A, typename Fn>
void FillMissingArgs(ParamsRef<P> params, FnArgs<A>* args, Fn fn, size_t offset = 0) {
  for (size_t i = args->pos().size(); i < params.size(); ++i) {
    auto const& p = params[i + offset];
    if (p.name.empty()) { continue; }
    args->named_emplace(p.name,
                        base::lazy_convert{[&]() { return fn(p.value); }});
  }
}

}  // namespace core

#endif  // ICARUS_CORE_PARAMS_REF_H
