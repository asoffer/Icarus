#ifndef ICARUS_CORE_TYPE_SYSTEM_TYPE_SYSTEM_H
#define ICARUS_CORE_TYPE_SYSTEM_TYPE_SYSTEM_H

#include <array>

#include "absl/base/casts.h"
#include "core/type_system/type.h"
#include "jasmin/instruction.h"
#include "nth/container/flyweight_set.h"
#include "nth/meta/concepts.h"
#include "nth/meta/sequence.h"
#include "nth/meta/type.h"

namespace core {
namespace internal_type_system {

template <typename T>
concept RequestedInlineStorage =
    std::is_void_v<typename T::store_inline> or nth::enumeration<T>;

// A struct whose size is guaranteed to be `TotalSize`.
template <typename T, size_t TotalSize, size_t Padding = TotalSize - sizeof(T)>
struct Padded {
  T value;
  std::array<char, TotalSize - sizeof(T)> padding = {};
};

template <typename T, size_t TotalSize>
struct Padded<T, TotalSize, 0> {
  T value;
};

template <typename Cat>
using StateTypesAsFunction = nth::type_t<Cat::state_types.reduce(
    [](auto... ts) { return nth::type<void (*)(nth::type_t<ts>...)>; })>;

}  // namespace internal_type_system

template <typename CrtpDerived, typename... StateTypes>
struct TypeCategory {
  static constexpr auto state_types = nth::type_sequence<StateTypes...>;

 private:
  using state_type_tuple = std::tuple<StateTypes...>;
  static constexpr bool kStoreInline =
      (internal_type_system::RequestedInlineStorage<StateTypes> and ...) and
      sizeof...(StateTypes) == 1 and sizeof(state_type_tuple) <= 4 and
      (std::is_trivially_copyable_v<StateTypes> and ...);

  template <bool InlineStorage>
  struct manager_type_impl {
    template <typename TS>
    void visit_all_stored(TS& sys, auto const& invocable) {}
  };

  template <>
  struct manager_type_impl<false>
      : private nth::flyweight_set<state_type_tuple> {
    template <typename TS>
    void visit_all_stored(TS& sys, auto const& invocable) {
      Type t;
      t.set_category(TS::template index<CrtpDerived>());
      for (size_t i = 0; i < this->size(); ++i) {
        t.set_value(i);
        invocable(TypeCategory::Construct(t, sys));
      }
    }

   private:
    friend TypeCategory;

    using base_type = nth::flyweight_set<state_type_tuple>;

    auto const& from_index(size_t index) {
      ASSERT(index < this->size());
      return base_type::from_index(index);
    }
    using base_type::index;
    using base_type::insert;
  };

 public:
  // Data structure that can be used to hold and ensure uniqueness of types of
  // this category. Each `manager_type` effectively is a flyweight representing
  // a relationship between the categories `state_type_tuple` and an integer
  // value taking the place of that state. However, if the `state_type_tuple` is
  // sufficiently small and trivially copyable, the manager will be empty and
  // the data will be stored inline in the `Type`.
  using manager_type = manager_type_impl<kStoreInline>;

  template <typename... State>
  explicit constexpr TypeCategory(nth::Type auto T, State&&... state) requires(
      TypeSystemSupporting<typename decltype(T)::type, CrtpDerived>and
          kStoreInline)
      : type_{}, manager_(nullptr) {
    static_assert(sizeof...(State) == 1);
    type_.set_category(nth::type_t<T>::template index<CrtpDerived>());
    write_inline_value(state...);
  }
  template <typename... State>
  explicit TypeCategory(TypeSystemSupporting<CrtpDerived> auto& m,
                        State&&... state) requires(not kStoreInline)
      : manager_(&m) {
    type_.set_category(m.template index<CrtpDerived>());
    type_.set_value(manager_->index(
        manager_->insert(state_type_tuple(std::forward<State>(state)...))
            .first));
  }

  static constexpr bool has_inline_storage() { return kStoreInline; }

  constexpr operator Type() const { return type_; }

  uint64_t category() const { return type_.category(); }
  uint64_t index() const { return type_.index(); }

  constexpr state_type_tuple decompose() const requires(kStoreInline) {
    return state_type_tuple(get_inline_value(type_.index()));
  }
  state_type_tuple const& decompose() const requires(not kStoreInline) {
    return manager_->from_index(type_.index());
  }

  template <typename H>
  friend H AbslHashValue(H h, TypeCategory const& t) {
    return H::combine(std::move(h), t.type_);
  }

  friend bool operator==(TypeCategory const& lhs, TypeCategory const& rhs) {
    ASSERT(lhs.manager_ == rhs.manager_);
    return lhs.type_ == rhs.type_;
  }

  friend bool operator!=(TypeCategory const& lhs, TypeCategory const& rhs) {
    ASSERT([&] {
      return lhs.manager_ == rhs.manager_ or lhs.manager_ == nullptr or
             rhs.manager_ == nullptr;
    }());
    return lhs.type_ != rhs.type_;
  }

 private:
  friend Type;
  friend manager_type;

  TypeCategory(Type type, manager_type* manager)
      : type_(type), manager_(manager) {}

  static CrtpDerived Construct(Type t,
                               TypeSystemSupporting<CrtpDerived> auto& sys) {
    ASSERT(t.template is<CrtpDerived>(sys));
    static_assert(std::is_standard_layout_v<CrtpDerived>);
    static_assert(sizeof(CrtpDerived) == sizeof(TypeCategory));
    // TODO: These requirements *should* make it safe to type-pun the object
    // down to `CrtpDerived` without explicit construction, but I haven't found
    // wording that guarantees that in the standard. For now that means we're
    // going to do an extra lookup. We'll either find the wording and make this
    // faster, or add a requirement to all `CrtpDerived` class implementations
    // to provide the necessary construction API that `Type` can hook into
    // without the extra lookup.
    if constexpr (kStoreInline) {
      return CrtpDerived(nth::type<std::decay_t<decltype(sys)>>,
                         get_inline_value(t.index()));
    } else {
      return std::apply(
          [&]<typename... Args>(Args const&... args) {
            return CrtpDerived(sys, args...);
          },
          static_cast<manager_type&>(sys).from_index(t.index()));
    }
  }

  template <typename T = std::tuple_element_t<0, state_type_tuple>>
  static constexpr T get_inline_value(uint32_t value) requires(kStoreInline) {
    return absl::bit_cast<internal_type_system::Padded<T, sizeof(uint32_t)>>(
               value)
        .value;
  }

  template <typename T>
  constexpr void write_inline_value(T t) requires(kStoreInline) {
    type_.set_value(absl::bit_cast<uint32_t>(
        internal_type_system::Padded<T, sizeof(uint32_t)>{.value = t}));
  }

  Type type_;
  manager_type* manager_;
};

template <typename... TypeCategories>
struct TypeSystem : TypeCategories::manager_type... {
  // Returns the index of the type category `Cat` in this type-system.
  template <nth::any_of<TypeCategories...> Cat>
  static constexpr size_t index() {
    return nth::type_sequence<TypeCategories...>.template find_if<[](auto t) {
      return t == nth::type<Cat>;
    }>();
  }

  template <nth::any_of<TypeCategories...> Cat,
            typename = internal_type_system::StateTypesAsFunction<Cat>>
  struct Make;

  template <nth::any_of<TypeCategories...> Cat, typename... States>
  struct Make<Cat, void (*)(States...)>
      : jasmin::StackMachineInstruction<Make<Cat>> {
    static constexpr Type execute(States... states, TypeSystem* system) {
      if constexpr (std::is_empty_v<typename Cat::manager_type>) {
        return Cat(nth::type<TypeSystem>, states...);
      } else {
        return Cat(*system, states...);
      }
    }
  };

  void visit_all_stored(auto const& invocable) {
    (static_cast<typename TypeCategories::manager_type&>(*this)
         .template visit_all_stored(*this, invocable),
     ...);
  }

  template <typename F>
  auto visit(Type t, F&& invocable) {
    using return_type = std::invoke_result_t<
        F, nth::type_t<nth::type_sequence<TypeCategories...>.head()>>;
    auto member_fn = (std::array<return_type (TypeSystem::*)(Type, F &&),
                                 sizeof...(TypeCategories)>{
        &TypeSystem::visit_one<TypeCategories, F>...})[t.category()];
    return (this->*member_fn)(t, std::forward<F>(invocable));
  }

  // Returns whether the type category indexed by `category_index` has inline
  // storage.
  static constexpr bool has_inline_storage(size_t category_index) {
    constexpr std::array kHasInlineStorage{
        TypeCategories::has_inline_storage()...};
    return kHasInlineStorage[category_index];
  }

 private:
  static constexpr auto automatic_jasmin_types =
      nth::type_sequence<TypeCategories...>.template filter<[](auto t) {
        return nth::type_t<t>::state_types.template all<[](auto u) {
          return std::is_convertible_v<nth::type_t<u>, jasmin::Value>;
        }>();
      }>();

  template <typename T, typename F>
  auto visit_one(Type t, F&& invocable) {
    return std::forward<F>(invocable)(t.get<T>(*this));
  }

 public:
  using JasminInstructionSet =
      nth::type_t<automatic_jasmin_types.reduce([](auto... ts) {
        return nth::type<jasmin::MakeInstructionSet<Make<nth::type_t<ts>>...>>;
      })>;
};

}  // namespace core

#endif  // ICARUS_CORE_TYPE_SYSTEM_TYPE_SYSTEM_H
