#include "integer_table.h"

#include <iostream>

#include "base/debug.h"

namespace module {

nth::Integer const* IntegerTable::insert(nth::Integer const& n) {
  auto [iter, inserted] = set_.insert(n);
  return &*iter;
}

void Serialize(IntegerTable const& table, data::IntegerTable& proto) {
  for (auto const& n : table) {
    auto& proto_integer = *proto.add_integers();
    if (n < 0) { proto_integer.set_negative(true); }
    std::span span = n.span();
    if (span.size() == 2 and span[1] == 0) { span.subspan(0, 1); }
    std::string& s = *proto_integer.mutable_words();
    // TODO: `resize_and_overwrite` when available.
    s.resize(sizeof(uintptr_t) * span.size());
    std::memcpy(s.data(), n.span().data(), sizeof(uintptr_t) * span.size());
  }
}

void Deserialize(data::IntegerTable const& proto, IntegerTable& table) {
  for (auto const& proto_integer : proto.integers()) {
    nth::Integer n;
    std::string_view data = proto_integer.words();
    // TODO: We need to make a faster implementation possible here.
    ASSERT(data.size() % sizeof(uintptr_t) == 0);
    uintptr_t word;
    for (size_t i = data.size(); i != 0; i -= sizeof(uintptr_t)) {
      n <<= sizeof(uintptr_t) * CHAR_BIT;
      std::memcpy(&word, data.data() + (i - sizeof(uintptr_t)),
                  sizeof(uintptr_t));
      n += word;
    }
    if (proto_integer.negative()) { n.negate(); }
    table.insert(n);
  }
}

void PrintTo(IntegerTable const& table, std::ostream* os) {
  *os << "[";
  std::string_view separator = "";
  for (auto const& n : table) {
    *os << std::exchange(separator, ", ");
    PrintTo(n, os);
  }
  *os << "]";
}

}  // namespace module
