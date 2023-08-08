#ifndef ICARUS_CORE_INTEGER_H
#define ICARUS_CORE_INTEGER_H

#include "absl/numeric/int128.h"
#include "absl/strings/str_format.h"
#include "nth/container/flyweight_set.h"
#include "serialization/constants.pb.h"

namespace core {
namespace internal_integer {

struct InternTable {
  using icarus_serialization_type = serialization::IntegerTable;

  absl::int128 const* insert(absl::int128 n) {
    return &*numbers_.insert(n).first;
  }

  size_t size() const { return numbers_.size(); }
  auto begin() const { return numbers_.begin(); }
  auto end() const { return numbers_.end(); }

  friend void IcarusSerialize(auto& serializer, InternTable const& table) {
    auto & output = serializer.output();
    for (absl::int128 value : table) {
      auto& i = *output.add_integers();
      i.set_negative(value < 0);
      std::string s;
      s.resize(sizeof(value));
      std::memcpy(s.data(), &value, sizeof(value));
      *i.mutable_words() = std::move(s);
    }
  }

  friend bool IcarusDeserialize(auto& deserializer, InternTable& table) {
    auto const& input = deserializer.input();
    for (auto const& n : input.integers()) {
      if (n.words().size() != sizeof(absl::int128)) { return false; }
      absl::int128 number;
      std::memcpy(&number, n.words().data(), n.words().size());
      table.insert(number);
    }
    return true;
  }

 private:
  nth::flyweight_set<absl::int128> numbers_;
};

template <size_t Tag>
InternTable Table;

}  // namespace internal_integer

template <size_t Tag>
struct GenericInteger;

using Integer = GenericInteger<0>;

template <size_t Tag>
struct [[clang::preferred_name(Integer)]] GenericInteger {
  GenericInteger(absl::int128 n = 0) : GenericInteger(Table.insert(n)) {}
  GenericInteger(std::integral auto n) : GenericInteger(Table.insert(n)) {}

  friend GenericInteger operator+(GenericInteger lhs, GenericInteger rhs) {
    return GenericInteger(Table.insert(*lhs.number_ + *rhs.number_));
  }
  friend GenericInteger operator-(GenericInteger lhs, GenericInteger rhs) {
    return GenericInteger(Table.insert(*lhs.number_ - *rhs.number_));
  }
  friend GenericInteger operator*(GenericInteger lhs, GenericInteger rhs) {
    return GenericInteger(Table.insert(*lhs.number_ * *rhs.number_));
  }
  friend GenericInteger operator/(GenericInteger lhs, GenericInteger rhs) {
    return GenericInteger(Table.insert(*lhs.number_ / *rhs.number_));
  }
  friend GenericInteger operator%(GenericInteger lhs, GenericInteger rhs) {
    return GenericInteger(Table.insert(*lhs.number_ % *rhs.number_));
  }
  GenericInteger operator-(GenericInteger n) {
    return GenericInteger(Table.insert(-*n.number_));
  }

  friend bool operator==(GenericInteger lhs, GenericInteger rhs) {
    return *lhs.number_ == *rhs.number_;
  }
  friend bool operator<(GenericInteger lhs, GenericInteger rhs) {
    return *lhs.number_ < *rhs.number_;
  }
  friend bool operator>(GenericInteger lhs, GenericInteger rhs) {
    return rhs < lhs;
  }
  friend bool operator<=(GenericInteger lhs, GenericInteger rhs) {
    return not(lhs > rhs);
  }
  friend bool operator>=(GenericInteger lhs, GenericInteger rhs) {
    return not(lhs < rhs);
  }
  friend bool operator!=(GenericInteger lhs, GenericInteger rhs) {
    return not(lhs == rhs);
  }

  friend void NthPrint(auto& printer, GenericInteger n) {
    printer.write(absl::StrFormat("%d", *n.number_));
  }

 private:
  explicit GenericInteger(absl::int128 const* number) : number_(number) {}

  static constexpr auto& Table = internal_integer::Table<Tag>;

  absl::int128 const* number_;
};

}  // namespace core

#endif  // ICARUS_CORE_INTEGER_H
