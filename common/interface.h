#ifndef ICARUS_COMMON_INTERFACE_H
#define ICARUS_COMMON_INTERFACE_H

#include <cstdint>
#include <limits>
#include <span>

#include "common/strong_identifier_type.h"
#include "jasmin/core/value.h"
#include "jasmin/serialize/reader.h"
#include "jasmin/serialize/writer.h"

namespace ic {

struct Interface : StrongIdentifierType<Interface, uint32_t> {
  Interface() : StrongIdentifierType(std::numeric_limits<uint32_t>::max()) {}

  struct construct_new_t {};
  static constexpr construct_new_t construct_new;

  explicit Interface(construct_new_t);

  friend void NthPrint(auto &p, auto &f, Interface const &i) {
    p.write("intf.");
    f(p, i.value());
  }

  friend bool IcarusDeserializeValue(std::span<jasmin::Value const> data,
                                     Interface &intf) {
    intf = data[0].as<Interface>();
    return true;
  }

  friend bool JasminDeserialize(jasmin::Reader auto &r, Interface &) {
    NTH_UNIMPLEMENTED();
  }
  friend void JasminSerialize(jasmin::Writer auto &w, Interface) {
    NTH_UNIMPLEMENTED();
  }
};

}  // namespace ic

#endif  // ICARUS_COMMON_INTERFACE_H
