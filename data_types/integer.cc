#include "data_types/integer.h"

#include <iostream>

#include "base/debug.h"

namespace data_types {

IntegerHandle IntegerTable::insert(absl::int128 const& n) {
  auto [iter, inserted] = set_.insert(n);
  return IntegerHandle(&*iter);
}

void Serialize(IntegerTable const& table, serialization::IntegerTable& proto) {
  for (auto const& n : table) {
    auto& proto_integer = *proto.add_integers();
    if (n < 0) { proto_integer.set_negative(true); }
    std::string& s = *proto_integer.mutable_words();
    s.resize(sizeof(n));
    std::memcpy(s.data(), &n, sizeof(n));
  }
}

void Deserialize(serialization::IntegerTable const& proto,
                 IntegerTable& table) {
  for (auto const& proto_integer : proto.integers()) {
    absl::int128 n;
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
    if (proto_integer.negative()) { n = -n; }
    table.insert(n);
  }
}

void PrintTo(IntegerTable const& table, std::ostream* os) {
  *os << "[";
  std::string_view separator = "";
  for (auto const& n : table) {
    *os << std::exchange(separator, ", ") << n;
  }
  *os << "]";
}

}  // namespace data_types
