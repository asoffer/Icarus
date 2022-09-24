#ifndef ICARUS_CORE_TYPE_SYSTEM_TYPE_SYSTEM_H
#define ICARUS_CORE_TYPE_SYSTEM_TYPE_SYSTEM_H

#include <array>

#include "absl/base/casts.h"
#include "base/flyweight_set.h"
#include "base/meta.h"
#include "core/type_system/type.h"
#include "jasmin/instruction.h"

namespace core {
namespace internal_type_system {

template <typename T>
concept RequestedInlineStorage =
    std::is_void_v<typename T::store_inline> or base::is_enum<T>;

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

}  // namespace internal_type_system

template <typename CrtpDerived, typename... StateTypes>
struct TypeCategory {
  using state_types = base::type_list<StateTypes...>;

 private:
  using state_type_tuple = std::tuple<StateTypes...>;
  static constexpr bool kStoreInline =
      (internal_type_system::RequestedInlineStorage<StateTypes> and ...) and
      sizeof...(StateTypes) == 1 and sizeof(state_type_tuple) <= 4 and
      (std::is_trivially_copyable_v<StateTypes> and ...);

  template <bool InlineStorage>
  struct manager_type_impl {};

  template <>
  struct manager_type_impl<false>
      : private base::flyweight_set<state_type_tuple> {
   private:
    friend TypeCategory;

    using base_type = base::flyweight_set<state_type_tuple>;

    using base_type::from_index;
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
  explicit constexpr TypeCategory(State&&... state) requires(kStoreInline)
      : type_{} {
    static_assert(sizeof...(State) == 1);
    write_inline_value(state...);
  }
  template <typename... State>
  explicit TypeCategory(TypeSystemSupporting<CrtpDerived> auto& m,
                        State&&... state) requires(not kStoreInline)
      : manager_(&m) {
    type_.category_ = m.template index<CrtpDerived>();
    type_.value_    = manager_->index(
           manager_->insert(state_type_tuple(std::forward<State>(state)...))
               .first);
  }

  constexpr operator Type() const { return type_; }

  constexpr state_type_tuple decompose() const requires(kStoreInline) {
    return state_type_tuple(get_inline_value(type_.value_));
  }
  state_type_tuple const& decompose() const requires(not kStoreInline) {
    return manager_->from_index(type_.value_);
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
    ASSERT(lhs.manager_ == rhs.manager_);
    return lhs.type_ != rhs.type_;
  }

 private:
  friend Type;

  static CrtpDerived Construct(Type t,
                               TypeSystemSupporting<CrtpDerived> auto& sys) {
    ASSERT(t.template is<CrtpDerived>() == true);
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
      return CrtpDerived(get_inline_value(t.value_));
    } else {
      return std::apply(
          [&]<typename... Args>(Args&&... args) {
            return CrtpDerived(sys, std::forward<Args>(args)...);
          },
          static_cast<manager_type&>(sys).from_index(t.value_));
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
    type_.value_ = absl::bit_cast<uint32_t>(
        internal_type_system::Padded<T, sizeof(uint32_t)>{.value = t});
  }

  Type type_;
  manager_type* manager_;
};

namespace internal_type_system {

template <typename S>
using ConvertibleToJasminValue = std::is_convertible<S, jasmin::Value>;

template <typename Cat>
struct AllStatesConvertibleToJasminValues {
  static constexpr bool value =
      base::all_of<typename Cat::state_types, ConvertibleToJasminValue>;
};

}  // namespace internal_type_system

template <typename... TypeCategories>
struct TypeSystem : TypeCategories::manager_type... {
  // Returns the index of the type category `Cat` in this type-system.
  template <base::one_of<TypeCategories...> Cat>
  constexpr size_t index() const {
    return base::Index<Cat>(base::type_list<TypeCategories...>{});
  }

  template <base::one_of<TypeCategories...> Cat,
            typename = typename Cat::state_types>
  struct Make;

  template <base::one_of<TypeCategories...> Cat, typename... States>
  struct Make<Cat, base::type_list<States...>>
      : jasmin::StackMachineInstruction<Make<Cat>> {
    static constexpr Type execute(States... states, TypeSystem* system) {
      return Cat(*system, states...);
    }
  };

 private:
  using automatic_jasmin_types =
      base::filter<base::type_list<TypeCategories...>,
                   internal_type_system::AllStatesConvertibleToJasminValues>;
  template <typename>
  struct MakeInstructionSet;
  template <typename... Cats>
  struct MakeInstructionSet<base::type_list<Cats...>> {
    using type = jasmin::MakeInstructionSet<Make<Cats>...>;
  };

 public:
  using JasminInstructionSet =
      typename MakeInstructionSet<automatic_jasmin_types>::type;
};

}  // namespace core

#endif  // ICARUS_CORE_TYPE_SYSTEM_TYPE_SYSTEM_H
