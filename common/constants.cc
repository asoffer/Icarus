#include "common/constants.h"

#include "nth/utility/no_destructor.h"

namespace ic {
namespace {

nth::NoDestructor<ConstantTable> constants(ConstantTable::Initial());

}  // namespace

ConstantTable &GlobalConstantTable() { return *constants; }

ConstantTable ConstantTable::Initial() {
  ConstantTable table;

  // Encode all primitive types.
  size_t i = 0;
#define IC_XMACRO_PRIMITIVE_TYPE(...)                                          \
  table.constants_.emplace_back(Category::PrimitiveType, i++);
#include "common/language/primitive_types.xmacro.h"

  return table;
}

Integer::Integer(nth::integer const &n) {
  auto [iter, inserted] = constants->integers_.try_emplace(
      n, Constant(constants->constants_.size()));
  if (inserted) {
    constants->constants_.emplace_back(ConstantTable::Category::Integer,
                                       constants->integers_.index(iter));
  }
  mutable_value() = iter->second.value_;
}

Integer::Integer(nth::integer &&n) {
  auto [iter, inserted] = constants->integers_.try_emplace(
      std::move(n), Constant(constants->constants_.size()));
  if (inserted) {
    constants->constants_.emplace_back(ConstantTable::Category::Integer,
                                       constants->integers_.index(iter));
  }
  mutable_value() = iter->second.value_;
}

bool Integer::LessThan(Integer lhs, Integer rhs) {
  return constants->integers_.from_index(lhs.value()).first <
         constants->integers_.from_index(rhs.value()).first;
}

Integer Integer::operator-() const {
  return Integer(-static_cast<nth::integer const &>(*this));
}

Integer::operator nth::integer const &() const {
  return constants->integers_.from_index(constants->constants_[value()].value())
      .first;
}

StringLiteral::StringLiteral() : StringLiteral(std::string("")) {}

StringLiteral::StringLiteral(std::string &&s) {
  auto [iter, inserted] = constants->strings_.try_emplace(
      std::move(s), Constant(constants->constants_.size()));
  if (inserted) {
    constants->constants_.emplace_back(ConstantTable::Category::String,
                                       constants->strings_.index(iter));
  }
  mutable_value() = iter->second.value_;
}

StringLiteral::StringLiteral(std::string const &s) {
  auto [iter, inserted] = constants->strings_.try_emplace(
      s, Constant(constants->constants_.size()));
  if (inserted) {
    constants->constants_.emplace_back(ConstantTable::Category::String,
                                       constants->strings_.index(iter));
  }
  mutable_value() = iter->second.value_;
}

StringLiteral::StringLiteral(std::string_view s)
    : StringLiteral(std::string(s)) {}

StringLiteral::operator std::string const &() const {
  return constants->strings_.from_index(value()).first;
}

Identifier::Identifier() : Identifier(std::string("")) {}

Identifier::Identifier(std::string &&s) {
  auto [iter, inserted] = constants->ids_.insert(std::move(s));
  mutable_value()       = constants->ids_.index(iter);
}

Identifier::Identifier(std::string const &s) {
  auto [iter, inserted] = constants->ids_.insert(s);
  mutable_value()       = constants->ids_.index(iter);
}

Identifier::Identifier(std::string_view s) : Identifier(std::string(s)) {}

Identifier::operator std::string const &() const {
  return constants->ids_.from_index(value());
}

namespace type {

Type::Type(from_index_t, size_t index) {
  NTH_REQUIRE((v.harden), index < constants->constants_.size());
  mutable_value() = (index << 8) | static_cast<uint8_t>(
                                       constants->constants_[index].category());
}

Type PointerType::pointee() const {
  return Type(Type::from_index, constants->constants_[index()].value());
}

Type SliceType::element_type() const {
  NTH_REQUIRE((v.harden), index() < constants->constants_.size());
  return Type(Type::from_index, constants->constants_[index()].value());
}

Type BufferPointerType::pointee() const {
  return Type(Type::from_index, constants->constants_[index()].value());
}

Type PatternType::match_type() const {
  NTH_REQUIRE((v.harden), index() < constants->constants_.size());
  return Type(Type::from_index, constants->constants_[index()].value());
}

PointerType Ptr(Type t) {
  auto [iter, inserted] =
      constants->pointers_.emplace(t, constants->constants_.size());
  if (inserted) {
    constants->constants_.emplace_back(ConstantTable::Category::PointerType,
                                       t.index());
  }
  return PointerType(iter->second);
}

SliceType Slice(Type t) {
  auto [iter, inserted] =
      constants->slices_.emplace(t, constants->constants_.size());
  if (inserted) {
    constants->constants_.emplace_back(ConstantTable::Category::SliceType,
                                       t.index());
  }
  return SliceType(iter->second);
}

PatternType Pattern(Type t) {
  auto [iter, inserted] =
      constants->patterns_.emplace(t, constants->constants_.size());
  if (inserted) {
    constants->constants_.emplace_back(ConstantTable::Category::PatternType,
                                       t.index());
  }
  return PatternType(iter->second);
}

BufferPointerType BufPtr(Type t) {
  auto [iter, inserted] =
      constants->buffer_pointers_.emplace(t, constants->constants_.size());
  if (inserted) {
    constants->constants_.emplace_back(
        ConstantTable::Category::BufferPointerType, t.index());
  }
  return BufferPointerType(iter->second);
}

size_t ParametersType::size() const {
  return constants->constants_[index()].value();
}

Parameter ParametersType::operator[](size_t index) const {
  NTH_REQUIRE((v.harden), index < size());
  return Parameter{
      .name = Identifier::FromRepresentation(
          constants->constants_[this->index() + index * 2 + 1].value()),
      .type =
          Type(Type::from_index,
               constants->constants_[this->index() + index * 2 + 2].value()),

  };
}

std::vector<Type> ParametersType::types() const {
  std::vector<Type> result;
  for (size_t i = 0; i < size(); ++i) { result.push_back(operator[](i).type); }
  return result;
}

ParametersType Parameters(std::span<Parameter const> p) {
  size_t position = constants->constants_.size();
  auto iter       = constants->parameters_.find(
            ConstantTable::ParameterInsertionType{p, position});
  if (iter == constants->parameters_.end()) {
    constants->constants_.emplace_back(ConstantTable::Category::ParametersType,
                                       p.size());
    for (auto parameter : p) {
      constants->constants_.emplace_back(
          ConstantTable::Category::Followup,
          Identifier::ToRepresentation(parameter.name));
      constants->constants_.emplace_back(ConstantTable::Category::Followup,
                                         parameter.type.index());
    }

    // TODO: Double-lookup because we don't have heterogeneous insertion.
    iter = constants->parameters_.insert(position).first;
  }

  return ParametersType(*iter);
}

ParametersType Parameters(std::initializer_list<Parameter> p) {
  return Parameters(std::span<Parameter const>(p.begin(), p.end()));
}

FunctionType Function(ParametersType pt, std::span<Type const> rets,
                      Evaluation e) {
  size_t position = constants->constants_.size();

  // TODO: Double-lookup because we don't have heterogeneous insertion.
  auto iter = constants->functions_.find(
      ConstantTable::FunctionInsertionType{pt, rets, e, position});

  if (iter == constants->functions_.end()) {
    constants->constants_.emplace_back(ConstantTable::Category::FunctionType,
                                       rets.size());
    constants->constants_.emplace_back(ConstantTable::Category::Followup,
                                       pt.index());
    constants->constants_.emplace_back(ConstantTable::Category::Followup,
                                       static_cast<int>(e));
    for (Type ret : rets) {
      constants->constants_.emplace_back(ConstantTable::Category::Followup,
                                         ret.index());
    }

    iter = constants->functions_.insert(position).first;
  }

  return FunctionType(*iter);
}

FunctionType Function(ParametersType pt, std::initializer_list<Type> rets,
                      Evaluation e ) {
  return Function(pt, std::span(rets.begin(), rets.end()), e);
}

ParametersType FunctionType::parameters() const {
  return ParametersType(constants->constants_[index() + 1].value());
}

Evaluation FunctionType::evaluation() const {
  return static_cast<Evaluation>(constants->constants_[index() + 2].value());
}

ReturnsType FunctionType::returns() const {
  return ReturnsType(constants->constants_[index() + 3].value(),
                     constants->constants_[index()].value());
}

Type ReturnsType::operator[](size_t index) const {
  NTH_REQUIRE((v.harden), index < size());

  return Type(Type::from_index, constants->constants_[index_ + index].value());
}

ReturnsType::iterator ReturnsType::begin() const {
  return iterator(&constants->constants_[index_]);
}
ReturnsType::iterator ReturnsType::end() const {
  return iterator(&constants->constants_[index_ + size_]);
}

Type RefinementType::underlying() const {
  return Type(Type::from_index, constants->constants_[index()].value());
}

#define IC_XMACRO_TYPE_KIND(k)                                                 \
  static_assert(sizeof(Type) == sizeof(k##Type));                              \
  static_assert(alignof(Type) == alignof(k##Type));                            \
                                                                               \
  k##Type Type::As##k() const {                                                \
    NTH_REQUIRE((v.debug), kind() == Kind::k);                                 \
    k##Type t;                                                                 \
    std::memcpy(&t, this, sizeof(Type));                                       \
    return t;                                                                  \
  }
#include "common/language/type_kind.xmacro.h"

bool RefinementType::operator()(AnyValue const &v) const {
  NTH_UNIMPLEMENTED();
  // auto const& pattern = constants->constants_[index() + 1];
  // return pattern(v);
}

}  // namespace type
}  // namespace ic
