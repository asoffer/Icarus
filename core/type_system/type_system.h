#ifndef ICARUS_CORE_TYPE_SYSTEM_TYPE_SYSTEM_H
#define ICARUS_CORE_TYPE_SYSTEM_TYPE_SYSTEM_H

#include "base/flyweight_set.h"
#include "base/meta.h"
#include "core/type_system/type.h"
#include "jasmin/instruction.h"

namespace core {

template <typename CrtpDerived, typename... StateTypes>
struct TypeCategory {
  using state_types = base::type_list<StateTypes...>;

 private:
  using state_type_tuple = std::tuple<StateTypes...>;

 public:
  // Data structure that can be used to hold and ensure uniqueness of types of
  // this category. Each `manager_type` effectively is a flyweight representing
  // a relationship between the categories `state_type_tuple` and an integer
  // value taking the place of that state.
  struct manager_type : private base::flyweight_set<state_type_tuple> {
   private:
    friend TypeCategory;

    using base_type = base::flyweight_set<state_type_tuple>;

    using base_type::from_index;
    using base_type::index;
    using base_type::insert;
  };

  template <typename... State>
  explicit TypeCategory(TypeSystemSupporting<CrtpDerived> auto& m,
                        State&&... state)
      : manager_(&m) {
    type_.category_ = m.template index<CrtpDerived>();
    type_.value_    = manager_->index(
           manager_->insert(state_type_tuple(std::forward<State>(state)...))
               .first);
  }

  operator Type() const { return type_; }

  state_type_tuple const& decompose() const {
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
    return std::apply(
        [&]<typename... Args>(Args&&... args) {
          return CrtpDerived(sys, std::forward<Args>(args)...);
        },
        static_cast<manager_type&>(sys).from_index(t.value_));
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
