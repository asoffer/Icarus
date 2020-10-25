#ifndef ICARUS_BASE_ANY_INVOCABLE_H
#define ICARUS_BASE_ANY_INVOCABLE_H

#include <utility>

namespace base {

template <typename R, typename... Args>
struct AnyInvocableVTable {
  R (*invoke)(void*, Args... args)  = nullptr;
  void* (*move_construct)(void*)    = [](void*) -> void* { return nullptr; };
  void (*move_assign)(void*, void*) = [](void*, void*) {};
  void (*destroy)(void*)            = [](void*) {};
};

template <typename R, typename... Args>
inline constexpr AnyInvocableVTable<R, Args...> DefaultAnyInvocableVTable{};

template <typename F, typename R, typename... Args>
inline constexpr auto AnyInvocableVTableFor = AnyInvocableVTable<R, Args...>{
    .invoke = [](void* self, Args... args) -> R {
      return (*reinterpret_cast<F*>(self))(std::move(args)...);
    },
    .move_construct = [](void* from) -> void* {
      return new F(std::move(*reinterpret_cast<F*>(from)));
    },
    .destroy = [](void* self) { delete reinterpret_cast<F*>(self); },
};

template <typename F>
struct any_invocable;

template <typename R, typename... Args>
struct any_invocable<R(Args...)> {
 public:
  any_invocable(std::nullptr_t = nullptr) noexcept
      : data_(nullptr), vtable_(&DefaultAnyInvocableVTable<R, Args...>) {}

  template <typename F>
  any_invocable(F&& f) noexcept
      : data_(new std::decay_t<F>(std::forward<F>(f))),
        vtable_(&AnyInvocableVTableFor<std::decay_t<F>, R, Args...>) {}

  any_invocable(any_invocable&& f) noexcept
      : data_(std::exchange(f.data_, nullptr)),
        vtable_(
            std::exchange(f.vtable_, &DefaultAnyInvocableVTable<R, Args...>)) {}

  any_invocable& operator=(any_invocable&& f) noexcept {
    vtable_->destroy(data_);
    vtable_ = f.vtable_;
    data_   = vtable_->move_construct(f.data_);
    return *this;
  }

  template <typename F>
  any_invocable& operator=(F&& f) noexcept {
    vtable_->destroy(data_);
    vtable_ = &AnyInvocableVTableFor<std::decay_t<F>, R, Args...>;
    data_   = new std::decay_t<F>(std::forward<F>(f));
    return *this;
  }

  // TODO: This has the same const-bug as with std::function. Fix me.
  template <typename... Ts>
  R operator()(Ts... args) const {
    return vtable_->invoke(data_, std::forward<Ts>(args)...);
  }

  constexpr explicit operator bool() const { return data_ == nullptr; }
  friend constexpr bool operator==(std::nullptr_t, any_invocable const& a) {
    return not static_cast<bool>(a.data_);
  }
  friend constexpr bool operator==(any_invocable const& a, std::nullptr_t) {
    return not static_cast<bool>(a.data_);
  }
  friend constexpr bool operator!=(std::nullptr_t, any_invocable const& a) {
    return !(a == nullptr);
  }
  friend constexpr bool operator!=(any_invocable const& a, std::nullptr_t) {
    return !(a == nullptr);
  }

 private:
  // TODO: Implement SBO
  void* data_;
  AnyInvocableVTable<R, Args...> const* vtable_;
};

}  // namespace base

#endif  // ICARUS_BASE_ANY_INVOCABLE_H
