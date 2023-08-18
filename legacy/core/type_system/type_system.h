#ifndef ICARUS_CORE_TYPE_SYSTEM_TYPE_SYSTEM_H
#define ICARUS_CORE_TYPE_SYSTEM_TYPE_SYSTEM_H

#include <array>

#include "absl/base/casts.h"
#include "core/parameters.h"
#include "core/type_system/type.h"
#include "jasmin/instruction.h"
#include "nth/container/flyweight_set.h"
#include "nth/meta/concepts.h"
#include "nth/meta/sequence.h"
#include "nth/meta/type.h"
#include "serialization/type_system.pb.h"

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
      NTH_ASSERT(index < this->size());
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
    NTH_ASSERT(lhs.manager_ == rhs.manager_);
    return lhs.type_ == rhs.type_;
  }

  friend bool operator!=(TypeCategory const& lhs, TypeCategory const& rhs) {
    NTH_ASSERT([&] {
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
    NTH_ASSERT(t.template is<CrtpDerived>(sys));
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

// Represents a category of types that can be expressed via a finite set
// specified via an enum whose explicitly listed enumerators are the valid
// values for the type. Note that even though a value of an enumerator is valid
// when it is not explicitly listed, such values are not valid to be passed to
// constructors of `FiniteSetType` instantiations.
template <nth::enumeration E>
struct FiniteSetType : TypeCategory<FiniteSetType<E>, E> {
  explicit constexpr FiniteSetType(nth::Type auto t, E e) requires
      TypeSystemSupporting<typename decltype(t)::type, FiniteSetType<E>>
      : TypeCategory<FiniteSetType<E>, E>(t, e) {}

  constexpr E value() const { return std::get<0>(this->decompose()); }
};

template <nth::enumeration E>
FiniteSetType(E) -> FiniteSetType<E>;

struct ParameterType : TypeCategory<ParameterType, core::Parameters<Type>> {
  explicit ParameterType(TypeSystemSupporting<ParameterType> auto& s,
                         core::Parameters<Type> parameters)
      : TypeCategory(s, std::move(parameters)) {}

  core::Parameters<Type> const& value() const {
    return std::get<0>(decompose());
  }

  struct Begin : jasmin::StackMachineInstruction<Begin> {
    static void execute(jasmin::ValueStack& value_stack) {
      value_stack.push(new core::Parameters<Type>);
    }
  };

  struct Append : jasmin::StackMachineInstruction<Append> {
    static core::Parameters<Type>* execute(core::Parameters<Type>* parameters,
                                           Type t) {
      parameters->append("", t);
      return parameters;
    }
  };

  struct AppendNamed : jasmin::StackMachineInstruction<AppendNamed> {
    static core::Parameters<Type>* execute(core::Parameters<Type>* parameters,
                                           char const* buffer, size_t length,
                                           Type t) {
      parameters->append(std::string(buffer, length), t);
      return parameters;
    }
  };

  template <TypeSystemSupporting<ParameterType> TS>
  struct End : jasmin::StackMachineInstruction<End<TS>> {
    static void execute(jasmin::ValueStack& value_stack, TS* system) {
      std::unique_ptr<core::Parameters<Type>> ptr(
          value_stack.pop<core::Parameters<Type>*>());
      value_stack.push(
          static_cast<Type>(ParameterType(*system, std::move(*ptr))));
    }
  };
};

struct FunctionType
    : TypeCategory<FunctionType, ParameterType, std::vector<Type>> {
  explicit FunctionType(TypeSystemSupporting<FunctionType> auto& s,
                        ParameterType parameters, std::vector<Type> returns)
      : TypeCategory(s, std::move(parameters), std::move(returns)) {}

  ParameterType parameter_type() const { return std::get<0>(decompose()); }

  Parameters<Type> const& parameters() const {
    return parameter_type().value();
  }

  std::span<Type const> returns() const { return std::get<1>(decompose()); }

  template <TypeSystemSupporting<FunctionType> TS>
  struct End : jasmin::StackMachineInstruction<End<TS>> {
    static void execute(jasmin::ValueStack& value_stack, TS* system,
                        size_t num_return_types) {
      std::vector<Type> return_types(num_return_types);
      while (num_return_types != 0) {
        return_types[--num_return_types] = value_stack.pop<Type>();
      }
      value_stack.push(static_cast<Type>(FunctionType(
          *system, value_stack.pop<Type>().get<ParameterType>(*system),
          std::move(return_types))));
    }
  };
};

struct PointerType : TypeCategory<PointerType, Type> {
  explicit PointerType(TypeSystemSupporting<PointerType> auto& s, Type t)
      : TypeCategory(s, t) {}

  Type pointee() const { return std::get<0>(decompose()); }
};

namespace internal_sized_integer {

struct SizedIntegerTypeState {
  using store_inline = void;

  uint16_t size_in_bits : 10;
  // Alignment must always be a power of 2, so we store its base-2 logarithm.
  uint16_t log_alignment_in_bytes : 5;
  uint16_t is_signed : 1;

  template <typename H>
  friend H AbslHashValue(H h, SizedIntegerTypeState state) {
    return H::combine(std::move(h), state.size_in_bits,
                      state.log_alignment_in_bytes, state.is_signed);
  }
  friend bool operator!=(SizedIntegerTypeState,
                         SizedIntegerTypeState) = default;

  friend bool operator==(SizedIntegerTypeState,
                         SizedIntegerTypeState) = default;
};

constexpr uint8_t Log2(uint32_t n) {
  uint8_t result = 0;
  if (n & uint32_t{0b11111111'11111111'00000000'00000000}) { result += 16; }
  if (n & uint32_t{0b11111111'00000000'11111111'00000000}) { result += 8; }
  if (n & uint32_t{0b11110000'11110000'11110000'11110000}) { result += 4; }
  if (n & uint32_t{0b11001100'11001100'11001100'11001100}) { result += 2; }
  if (n & uint32_t{0b10101010'10101010'10101010'10101010}) { result += 1; }
  return result;
}

constexpr uint8_t SmallestPowerOfTwoGreaterThanOrEqualTo(uint32_t n) {
  --n;
  n |= (n >> 1);
  n |= (n >> 2);
  n |= (n >> 4);
  n |= (n >> 8);
  n |= (n >> 16);
  return n + 1;
}

}  // namespace internal_sized_integer

// Represents types whose storage is sufficient to hold values in the range
// -2^{N-1} ... 2^{N-1}-1 (inclusive) for signed types and 0 ... 2^N - 1
// (inclusive) for unsigned types.
struct SizedIntegerType
    : TypeCategory<SizedIntegerType,
                   internal_sized_integer::SizedIntegerTypeState> {
  template <TypeSystemSupporting<SizedIntegerType> TS>
  static SizedIntegerType I(uint16_t bits) {
    return I<TS>(bits, DefaultAlignment(bits));
  }
  template <TypeSystemSupporting<SizedIntegerType> TS>
  static SizedIntegerType I(uint16_t bits, Alignment alignment) {
    size_t log_alignment = internal_sized_integer::Log2(alignment.value());
    NTH_ASSERT(log_alignment <
               static_cast<size_t>(std::numeric_limits<uint16_t>::max()));
    return SizedIntegerType(
        nth::type<TS>,
        {.size_in_bits           = bits,
         .log_alignment_in_bytes = static_cast<uint16_t>(log_alignment),
         .is_signed              = true});
  }
  template <TypeSystemSupporting<SizedIntegerType> TS>
  static SizedIntegerType U(uint16_t bits) {
    return U<TS>(bits, DefaultAlignment(bits));
  }
  template <TypeSystemSupporting<SizedIntegerType> TS>
  static SizedIntegerType U(uint16_t bits, Alignment alignment) {
    size_t log_alignment = internal_sized_integer::Log2(alignment.value());
    NTH_ASSERT(log_alignment <
               static_cast<size_t>(std::numeric_limits<uint16_t>::max()));
    return SizedIntegerType(
        nth::type<TS>,
        {.size_in_bits           = bits,
         .log_alignment_in_bytes = static_cast<uint16_t>(log_alignment),
         .is_signed              = false});
  }

  constexpr bool is_signed() const { return value().is_signed; }
  constexpr size_t bits() const { return value().size_in_bits; }
  constexpr Bytes bytes() const {
    return Bytes((value().size_in_bits + 7) / 8);
  }
  constexpr Alignment alignment() const {
    return Alignment(uint64_t{1} << value().log_alignment_in_bytes);
  }

  static Alignment DefaultAlignment(size_t bits) {
    return Alignment(
        internal_sized_integer::SmallestPowerOfTwoGreaterThanOrEqualTo(
            (bits + 7) / 8));
  }

 private:
  friend TypeCategory;
  template <typename...>
  friend struct TypeSystem;

  constexpr internal_sized_integer::SizedIntegerTypeState const& value() const {
    return std::get<0>(decompose());
  }

  explicit constexpr SizedIntegerType(
      nth::Type auto t,
      internal_sized_integer::SizedIntegerTypeState state) requires
      TypeSystemSupporting<typename decltype(t)::type, SizedIntegerType>
      : TypeCategory(t, state) {}
};

}  // namespace core

#include "data_types/char.h"
#include "module/unique_id.h"

namespace semantic_analysis {

enum class Primitive : uint8_t {
  Bool,
  Char,
  Byte,
  F32,
  F64,
  Type,
  Integer,
  Module,
  NoReturn,
  EmptyArray,
  NullPtr,
  Error
};
using PrimitiveType = core::FiniteSetType<Primitive>;

// In general, arithmetic is not allowed on pointers in Icarus. However, a more
// specialized "buffer pointer" type does allow for arithmetic. Specifically,
// the type category `BufferPointerType` represents the affine space of the
// system's address space. Integers may be added or subtracted to buffer
// pointers to produce new buffer pointers, and buffer pointers may be
// subtracted from each other to produce signed integer values representing the
// distance between those values.
struct BufferPointerType : core::TypeCategory<BufferPointerType, core::Type> {
  explicit BufferPointerType(
      core::TypeSystemSupporting<BufferPointerType> auto& s, core::Type t)
      : core::TypeCategory<BufferPointerType, core::Type>(s, t) {}

  core::Type pointee() const { return std::get<0>(decompose()); }
};

namespace internal_type_system {

struct ArrayTypeState
    : base::Extend<ArrayTypeState>::With<base::AbslHashExtension> {
  core::Type type;
  size_t length;
};

}  // namespace internal_type_system

// The `ArrayType` category represents a compile-time fixed-length block of
// contiguous objects all of the same type.
struct ArrayType
    : core::TypeCategory<ArrayType, internal_type_system::ArrayTypeState> {
  explicit ArrayType(core::TypeSystemSupporting<ArrayType> auto& s,
                     size_t length, core::Type t)
      : ArrayType(s, {.type = t, .length = length}) {}

  core::Type data_type() const { return std::get<0>(decompose()).type; }
  size_t length() const { return std::get<0>(decompose()).length; }

 private:
  friend TypeCategory;

  explicit ArrayType(core::TypeSystemSupporting<ArrayType> auto& s,
                     internal_type_system::ArrayTypeState state)
      : core::TypeCategory<ArrayType, internal_type_system::ArrayTypeState>(
            s, state) {}
};

// The `SliceType` category represents a non-owning view of a range of values of
// a given type stored in contiguous memory (which we call a "slice").
struct SliceType : core::TypeCategory<SliceType, core::Type> {
  explicit SliceType(core::TypeSystemSupporting<SliceType> auto& s,
                     core::Type t)
      : core::TypeCategory<SliceType, core::Type>(s, t) {}

  core::Type pointee() const { return std::get<0>(decompose()); }
};

// The `EnumType` category represents a types that have a fixed set of
// enumerated values defined by the user.
struct EnumType
    : core::TypeCategory<EnumType, module::UniqueId, size_t,
                         serialization::TypeSystem::EnumType const*> {
  explicit EnumType(core::TypeSystemSupporting<EnumType> auto& s,
                    module::UniqueId module_id, size_t index,
                    serialization::TypeSystem::EnumType const* ptr)
      : TypeCategory(s, module_id, index, ptr) {}

  auto const& enumerators() const {
    return std::get<2>(decompose())->enumerator();
  }
  bool has_member(std::string_view name) {
    return enumerators().contains(name);
  }
  std::optional<uint64_t> value(std::string_view name) {
    auto const& e = enumerators();
    auto iter     = e.find(name);
    if (iter == e.end()) { return std::nullopt; }
    return iter->second;
  }

 private:
  friend TypeCategory;
};

struct OpaqueType : core::TypeCategory<OpaqueType, size_t> {
  explicit OpaqueType(core::TypeSystemSupporting<OpaqueType> auto& s)
      : OpaqueType(s, counter.fetch_add(1, std::memory_order_relaxed)) {}

  size_t index() const { return std::get<0>(decompose()); }

  // TODO: Make private.
  explicit OpaqueType(core::TypeSystemSupporting<OpaqueType> auto& s, size_t n)
      : core::TypeCategory<OpaqueType, size_t>(s, n) {}

  static std::atomic<size_t> counter;
};

using TypeSystem =
    core::TypeSystem<PrimitiveType, core::SizedIntegerType, core::ParameterType,
                     core::PointerType, BufferPointerType, ArrayType, SliceType,
                     core::FunctionType, EnumType, OpaqueType>;
inline TypeSystem GlobalTypeSystem;

}  // namespace semantic_analysis

#endif  // ICARUS_CORE_TYPE_SYSTEM_TYPE_SYSTEM_H
