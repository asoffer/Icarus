#ifndef ICARUS_COMMON_CONSTANTS_H
#define ICARUS_COMMON_CONSTANTS_H

#include <span>
#include <utility>
#include <vector>

#include "absl/container/flat_hash_map.h"
#include "common/any_value.h"
#include "common/identifier.h"
#include "common/integer.h"
#include "common/internal/component.h"
#include "common/internal/constant_handle.h"
#include "common/pattern.h"
#include "common/result.h"
#include "common/string_literal.h"
#include "common/strong_identifier_type.h"
#include "common/to_bytes.h"
#include "nth/container/flyweight_map.h"
#include "nth/container/flyweight_set.h"
#include "nth/io/deserialize/deserialize.h"
#include "nth/io/serialize/serialize.h"
#include "nth/numeric/integer.h"
#include "nth/utility/bytes.h"

namespace ic {
namespace type {

struct RefinementType : Type {
  Type underlying() const;

  friend void NthPrint(auto& p, auto& fmt, RefinementType r) {
    fmt(p, r.underlying());
    p.write("[??]");
  }

  bool operator()(AnyValue const&) const;

 private:
  friend Type;
  friend RefinementType Refinement(Type, ::ic::Pattern p);
};

struct OpaqueType : Type {
  friend void NthPrint(auto&, auto&, OpaqueType) { NTH_UNREACHABLE(); }
};
struct DependentFunctionType : Type {
  std::optional<Type> operator()(std::span<AnyValue const>) const {
    NTH_UNIMPLEMENTED();
  }

  friend void NthPrint(auto&, auto&, DependentFunctionType) {
    NTH_UNIMPLEMENTED();
  }
};

// Represents a primitive type built-in to the language.
struct PrimitiveType : Type {
  enum class Kind : uint8_t {
#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling) kind,
#define IC_XMACRO_PRIMITIVE_TYPE_CATEGORY_RANGE(name, start, end)              \
  InternalBeginCategory_##name = static_cast<uint8_t>(Kind::start),            \
  InternalEndCategory_##name   = static_cast<uint8_t>(Kind::end) + 1,
#include "common/language/primitive_types.xmacro.h"
  };

  explicit constexpr PrimitiveType(Kind k)
      : Type(Type::Kind::Primitive, static_cast<uint64_t>(k)) {}

  PrimitiveType::Kind primitive_kind() const {
    return static_cast<PrimitiveType::Kind>(index() & 0xff);
  }

  friend void NthPrint(auto& p, auto& f, PrimitiveType t) {
    switch (t.primitive_kind()) {
#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling)                       \
  case Kind::kind:                                                             \
    p.write(spelling);                                                         \
    return;
#include "common/language/primitive_types.xmacro.h"
      default: NTH_UNREACHABLE();
    }
  }

 private:
  friend Type;
  explicit PrimitiveType() = default;
  explicit constexpr PrimitiveType(uint32_t n)
      : Type(Type::Kind::Primitive, n) {}
};

#define IC_XMACRO_PRIMITIVE_TYPE_CATEGORY_RANGE(category_name, start, end)     \
  inline bool category_name(PrimitiveType p) {                                 \
    return static_cast<uint8_t>(PrimitiveType::Kind::start) <=                 \
               static_cast<uint8_t>(p.primitive_kind()) and                    \
           static_cast<uint8_t>(p.primitive_kind()) <=                         \
               static_cast<uint8_t>(PrimitiveType::Kind::end);                 \
  }
#include "common/language/primitive_types.xmacro.h"

#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling)                       \
  inline PrimitiveType const symbol = PrimitiveType(PrimitiveType::Kind::kind);
#include "common/language/primitive_types.xmacro.h"

struct PointerType : Type {
  Type pointee() const;

  friend void NthPrint(auto& p, auto& f, PointerType ptr) {
    p.write("*");
    f(p, ptr.pointee());
  }

 private:
  friend Type;
  friend struct TypeSystem;
  friend PointerType Ptr(Type t);

  explicit PointerType() = default;
  explicit constexpr PointerType(uint32_t n) : Type(Type::Kind::Pointer, n) {}
};

PointerType Ptr(Type t);

struct BufferPointerType : Type {
  Type pointee() const;

  friend void NthPrint(auto& p, auto& f, BufferPointerType ptr) {
    p.write("[*]");
    f(p, ptr.pointee());
  }

 private:
  friend Type;
  friend struct TypeSystem;
  friend BufferPointerType BufPtr(Type t);

  explicit BufferPointerType() = default;
  explicit constexpr BufferPointerType(uint32_t n)
      : Type(Type::Kind::BufferPointer, n) {}
};

BufferPointerType BufPtr(Type t);

struct SliceType : Type {
  Type element_type() const;

  friend void NthPrint(auto& p, auto& f, SliceType s) {
    p.write("\\");
    f(p, s.element_type());
  }

 private:
  friend Type;
  friend struct TypeSystem;
  friend SliceType Slice(Type t);

  explicit SliceType() = default;
  explicit constexpr SliceType(uint32_t n) : Type(Type::Kind::Slice, n) {}
};

SliceType Slice(Type t);

struct PatternType : Type {
  // A value of `*this` type is a pattern that can be used to match values of
  // type `match_type()`.
  Type match_type() const;

  friend void NthPrint(auto& p, auto& fmt, PatternType pt) {
    p.write("pattern(");
    fmt(p, pt.match_type());
    p.write(")");
  }

 private:
  friend Type;
  friend PatternType Pattern(Type);

  explicit PatternType() = default;
  explicit constexpr PatternType(uint32_t n) : Type(Type::Kind::Pattern, n) {}
};

PatternType Pattern(Type t);

struct Parameter {
  Identifier name;
  Type type;

  friend bool operator==(Parameter, Parameter) = default;

  template <typename H>
  friend H AbslHashValue(H h, Parameter p) {
    return H::combine(std::move(h), p.name, p.type);
  }

  friend void NthPrint(auto& p, auto& fmt, Parameter param) {
    fmt(p, param.name);
    p.write(": ");
    fmt(p, param.type);
  }
};

// Represents a set of parameters to an invocable type.
struct ParametersType : Type {
  explicit ParametersType() = default;

  using Parameter = ::ic::type::Parameter;

  size_t size() const;
  Parameter operator[](size_t index) const;

  std::vector<Type> types() const;

  friend void NthPrint(auto& p, auto& fmt, ParametersType params) {
    std::string_view separator = "";
    for (size_t i = 0; i < params.size(); ++i) {
      auto param = params[i];
      p.write(std::exchange(separator, ", "));
      fmt(p, param);
    }
  }

 private:
  friend Type;
  friend struct FunctionType;
  friend struct TypeSystem;
  friend ParametersType Parameters(std::span<Parameter const>);
  explicit constexpr ParametersType(uint32_t n)
      : Type(Type::Kind::Parameters, n) {}
};

ParametersType Parameters(std::span<Parameter const> p);
ParametersType Parameters(std::initializer_list<Parameter> p);

struct ReturnsType {
  size_t size() const { return size_; }
  bool empty() const { return size() == 0; }
  Type operator[](size_t index) const;

  struct iterator;
  iterator begin() const;
  iterator end() const;

 private:
  friend struct FunctionType;

  ReturnsType(size_t index, size_t size) : index_(index), size_(size) {}
  size_t index_;
  size_t size_;
};

enum Evaluation {
  RequireCompileTime,
  PreferCompileTime,
  PreferRuntime,
  RequireRuntime,
};

struct FunctionType : Type {
  explicit FunctionType() = default;

  Evaluation evaluation() const;
  ParametersType parameters() const;
  ReturnsType returns() const;

  friend void NthPrint(auto& p, auto& fmt, FunctionType f);

 private:
  friend Type;
  friend struct TypeSystem;
  friend FunctionType Function(ParametersType, std::span<Type const>,
                               Evaluation);

  explicit constexpr FunctionType(uint32_t n) : Type(Type::Kind::Function, n) {}
};

FunctionType Function(ParametersType pt, std::initializer_list<Type> rets,
                      Evaluation e = Evaluation::PreferRuntime);

FunctionType Function(ParametersType pt, std::span<Type const> rets,
                      Evaluation e = Evaluation::PreferRuntime);

};  // namespace type

struct Constant {
 private:
  friend Identifier;
  friend Integer;
  friend StringLiteral;

  Constant(uint32_t index) : value_(index) {}

  uint32_t value_;
};

struct ConstantTableBase {
  auto begin() const { return constants_.begin(); }
  auto end() const { return constants_.end(); }

  nth::flyweight_map<nth::integer, Constant> const& integers() const {
    return integers_;
  };

  nth::flyweight_map<std::string, Constant> const& strings() const {
    return strings_;
  };

  nth::flyweight_set<std::string> const& identifiers() const { return ids_; };

 protected:
  friend Constant;
  friend Identifier;
  friend Integer;
  friend type::Type;
  friend StringLiteral;
#define IC_XMACRO_TYPE_KIND(kind) friend ::ic::type::kind##Type;
#include "common/language/type_kind.xmacro.h"
  friend type::PointerType type::Ptr(type::Type);
  friend type::BufferPointerType type::BufPtr(type::Type);
  friend type::SliceType type::Slice(type::Type);
  friend type::PatternType type::Pattern(type::Type);
  friend type::ParametersType type::Parameters(
      std::span<type::Parameter const>);
  friend type::FunctionType type::Function(type::ParametersType,
                                           std::span<type::Type const>,
                                           type::Evaluation);
  friend type::ReturnsType;
  friend type::ReturnsType::iterator;

  using Category = internal_constants::Component::Category;

  std::vector<internal_constants::Component> constants_;

  nth::flyweight_set<std::string> ids_;
  nth::flyweight_map<nth::integer, Constant> integers_;
  nth::flyweight_map<std::string, Constant> strings_;

  // Pointers, slices, and patterns are stored with a single component whose
  // value is the index of the pointee-type.
  absl::flat_hash_map<type::Type, size_t> pointers_;
  absl::flat_hash_map<type::Type, size_t> buffer_pointers_;
  absl::flat_hash_map<type::Type, size_t> slices_;
  absl::flat_hash_map<type::Type, size_t> patterns_;
};

struct ConstantTable : ConstantTableBase {
  ConstantTable() = default;
  ConstantTable(ConstantTable const& c)
      : ConstantTableBase(c),
        parameters_(0, ParameterHash(constants_), ParameterEq(constants_)),
        functions_(0, FunctionHash(constants_), FunctionEq(constants_)) {
    parameters_.insert(c.parameters_.begin(), c.parameters_.end());
    functions_.insert(c.functions_.begin(), c.functions_.end());
  }
  ConstantTable(ConstantTable&& c)
      : ConstantTableBase(std::move(c)),
        parameters_(0, ParameterHash(constants_), ParameterEq(constants_)),
        functions_(0, FunctionHash(constants_), FunctionEq(constants_)) {
    parameters_.insert(std::make_move_iterator(c.parameters_.begin()),
                       std::make_move_iterator(c.parameters_.end()));
    functions_.insert(std::make_move_iterator(c.functions_.begin()),
                      std::make_move_iterator(c.functions_.end()));
  }

  ConstantTable& operator=(ConstantTable const&) = delete;
  ConstantTable& operator=(ConstantTable&&)      = delete;

  friend Result NthSerialize(auto& s, ConstantTable const& table) {
    co_await nth::io::write_integer(s, table.ids_.size());
    for (auto const& n : table.ids_) { co_await nth::io::serialize(s, n); }
    co_await nth::io::write_integer(s, table.integers_.size());
    for (auto const& [n, unused_constant] : table.integers_) {
      co_await nth::io::serialize(s, n);
    }
    co_await nth::io::write_integer(s, table.strings_.size());
    for (auto const& [str, unused_constant] : table.strings_) {
      co_await nth::io::write_integer(s, str.size());
      co_await s.write(ToBytes(str));
    }
    co_await nth::io::serialize(s, nth::io::as_sequence(table.constants_));
    co_return Result::success();
  }

  friend Result NthDeserialize(auto& d, ConstantTable& table) {
    size_t id_size;
    co_await nth::io::read_integer(d, id_size);
    for (size_t i = 0; i < id_size; ++i) {
      size_t id_size;
      co_await nth::io::read_integer(d, id_size);
      std::string id(id_size, '\0');
      co_await d.read(ToBytes(id));
      Identifier(std::move(id));
    }

    size_t integer_size;
    co_await nth::io::read_integer(d, integer_size);
    for (size_t i = 0; i < integer_size; ++i) {
      nth::integer n;
      co_await nth::io::deserialize(d, n);
      (void)Integer(std::move(n));
    }

    size_t strings_table_size;
    co_await nth::io::read_integer(d, strings_table_size);
    for (size_t i = 0; i < strings_table_size; ++i) {
      size_t str_size;
      co_await nth::io::read_integer(d, str_size);
      std::string str(str_size, '\0');
      co_await d.read(ToBytes(str));
      StringLiteral(std::move(str));
    }

    size_t constant_count;
    co_await nth::io::read_integer(d, constant_count);
    constexpr size_t PrimitiveCount = [] {
      size_t i = 0;
#define IC_XMACRO_PRIMITIVE_TYPE(...) ++i;
#include "common/language/primitive_types.xmacro.h"
      return i;
    }();
    d.skip(sizeof(internal_constants::Component) * PrimitiveCount);
    for (size_t i = PrimitiveCount; i < constant_count; ++i) {
      nth::io::deserialize(d, table.constants_.emplace_back());
    }
    size_t index = 0;
    while (index < table.constants_.size()) {
      auto const& component = table.constants_[index];
      switch (component.category()) {
        case Category::Followup: NTH_UNREACHABLE();
        case Category::PrimitiveType:
        case Category::Integer:
        case Category::String: ++index; break;
        case Category::PointerType: {
          table.pointers_.emplace(
              type::Type(type::Type::from_index, component.value()), ++index);
        } break;
        case Category::BufferPointerType: {
          table.buffer_pointers_.emplace(
              type::Type(type::Type::from_index, component.value()), ++index);
        } break;
        case Category::SliceType: {
          table.slices_.emplace(
              type::Type(type::Type::from_index, component.value()), ++index);
        } break;
        case Category::PatternType: {
          table.patterns_.emplace(
              type::Type(type::Type::from_index, component.value()), ++index);
        } break;
        case Category::ParametersType: {
          table.parameters_.insert(index);
          index += 1 + 2 * component.value();
        } break;
        case Category::FunctionType: {
          table.functions_.insert(index);
          index += 3 + component.value();
        } break;
        default: NTH_UNIMPLEMENTED();
      }
    }

    co_return Result::success();
  }

  static ConstantTable Initial();

 private:
  friend type::ParametersType type::Parameters(
      std::span<type::Parameter const>);
  friend type::FunctionType type::Function(type::ParametersType,
                                           std::span<type::Type const>,
                                           type::Evaluation);
  friend type::ReturnsType;
  friend type::ReturnsType::iterator;

  static void ConvertType(internal_constants::Component&) {}

  struct ParameterInsertionType {
    std::span<type::Parameter const> parameters;
    size_t index;

    operator size_t() const { return index; }
  };

  struct ParameterHash {
    using is_transparent = void;

    explicit ParameterHash(std::vector<internal_constants::Component> const&
                               components NTH_ATTRIBUTE(lifetimebound))
        : components_(components) {}

    [[nodiscard]] size_t operator()(size_t index) const {
      NTH_REQUIRE((v.harden), index < components_.size());
      size_t size   = components_[index].value();
      size_t result = 0;
      for (size_t i = 0; i < size * 2; i += 2) {
        result = absl::HashOf(
            result,
            Identifier::FromRepresentation(components_[index + i + 1].value()),
            type::Type(type::Type::from_index, index + i + 2));
      }
      return result;
    }

    [[nodiscard]] size_t operator()(ParameterInsertionType pit) const {
      size_t result = 0;
      for (type::Parameter p : pit.parameters) {
        result = absl::HashOf(result, p.name, p.type);
      }
      return result;
    }

   private:
    std::vector<internal_constants::Component> const& components_;
  };

  struct ParameterEq {
    using is_transparent = void;

    explicit ParameterEq(std::vector<internal_constants::Component> const&
                             components NTH_ATTRIBUTE(lifetimebound))
        : components_(components) {}

    [[nodiscard]] bool operator()(size_t lhs, size_t rhs) const {
      return lhs == rhs;
    }

    [[nodiscard]] bool operator()(size_t lhs,
                                  ParameterInsertionType rhs) const {
      return operator()(rhs, lhs);
    }
    [[nodiscard]] bool operator()(ParameterInsertionType lhs,
                                  size_t rhs) const {
      size_t size = components_[rhs].value();
      if (lhs.parameters.size() != size) { return false; }
      for (size_t i = 0; i < size * 2; i += 2) {
        if (lhs.parameters[i / 2].name !=
            Identifier::FromRepresentation(components_[rhs + i].value())) {
          return false;
        }
        if (lhs.parameters[i / 2].type !=
            type::Type(type::Type::from_index, rhs + i + 1)) {
          return false;
        }
      }
      return true;
    }

   private:
    std::vector<internal_constants::Component> const& components_;
  };

  struct FunctionInsertionType {
    type::ParametersType parameters;
    std::span<type::Type const> returns;
    type::Evaluation evaluation;
    size_t index;

    operator size_t() const { return index; }
  };

  struct FunctionHash {
    using is_transparent = void;

    explicit FunctionHash(std::vector<internal_constants::Component> const&
                              components NTH_ATTRIBUTE(lifetimebound))
        : components_(components) {}

    [[nodiscard]] size_t operator()(size_t index) const {
      size_t ret_count = components_[index].value();
      size_t result    = 0;
      result           = absl::HashOf(components_[index + 1].value());
      for (size_t i = 3; i < ret_count + 3; ++i) {
        result =
            absl::HashOf(result, type::Type(type::Type::from_index, index + i));
      }
      return result;
    }

    [[nodiscard]] size_t operator()(FunctionInsertionType fit) const {
      size_t result = absl::HashOf(fit.parameters, fit.evaluation);
      for (type::Type r : fit.returns) { result = absl::HashOf(result, r); }
      return result;
    }

   private:
    std::vector<internal_constants::Component> const& components_;
  };

  struct FunctionEq {
    using is_transparent = void;

    explicit FunctionEq(std::vector<internal_constants::Component> const&
                            components NTH_ATTRIBUTE(lifetimebound))
        : components_(components) {}

    [[nodiscard]] bool operator()(size_t lhs, size_t rhs) const {
      return lhs == rhs;
    }

    [[nodiscard]] bool operator()(size_t lhs, FunctionInsertionType rhs) const {
      return operator()(rhs, lhs);
    }

    [[nodiscard]] bool operator()(FunctionInsertionType lhs, size_t rhs) const {
      if (lhs.parameters.index() != components_[rhs + 1].value()) {
        return false;
      }
      if (static_cast<int>(lhs.evaluation) != components_[rhs + 2].value()) {
        return false;
      }
      size_t ret_count = components_[rhs].value();
      if (lhs.returns.size() != ret_count) { return false; }
      for (size_t i = 0; i < ret_count; ++i) {
        if (lhs.returns[i] != type::Type(type::Type::from_index, rhs + i + 3)) {
          return false;
        }
      }
      return true;
    }

   private:
    std::vector<internal_constants::Component> const& components_;
  };

  // Parameters are stored with a first component indicating the length followed
  // by that number of components. The number of components is twice the number
  // of parameters, where pairs of components in order represent the name (as an
  // identifier) followed by the parameters type index.
  absl::flat_hash_set<size_t, ParameterHash, ParameterEq> parameters_{
      0, ParameterHash(constants_), ParameterEq(constants_)};

  // Function types are represented as a component indicating the number of
  // returns followed by the index for the parameter type, followed again by the
  // list of returns type indices.
  absl::flat_hash_set<size_t, FunctionHash, FunctionEq> functions_{
      0, FunctionHash(constants_), FunctionEq(constants_)};
};

namespace type {

struct ReturnsType::iterator {
  iterator operator++(int) {
    auto copy = *this;
    ++*this;
    return copy;
  }
  iterator& operator++() {
    ++ptr_;
    return *this;
  }
  Type operator*() const { return Type(Type::from_index, ptr_->value()); }
  friend bool operator==(iterator, iterator) = default;
  friend bool operator!=(iterator, iterator) = default;

 private:
  friend ReturnsType;
  iterator(internal_constants::Component const* ptr) : ptr_(ptr) {}

  internal_constants::Component const* ptr_;
};

void NthPrint(auto& p, auto& fmt, FunctionType f) {
  std::string_view separator = "";
  p.write("(");
  fmt(p, f.parameters());
  auto returns = f.returns();
  p.write(returns.size() != 1 ? ") -> (" : ") -> ");
  separator = "";
  for (Type r : f.returns()) {
    p.write(std::exchange(separator, ", "));
    fmt(p, r);
  }
  if (returns.size() != 1) { p.write(")"); }
}

void NthPrint(auto& p, auto& f, Type t) {
  switch (t.kind()) {
#define IC_XMACRO_TYPE_KIND(kind)                                              \
  case Type::Kind::kind:                                                       \
    f(p, t.As##kind());                                                        \
    return;
#include "common/language/type_kind.xmacro.h"
  }
}

}  // namespace type

ConstantTable& GlobalConstantTable();

}  // namespace ic

#endif  // ICARUS_COMMON_CONSTANTS_H
