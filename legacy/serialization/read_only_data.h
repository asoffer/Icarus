#ifndef ICARUS_SERIALIZATION_READ_ONLY_DATA_H
#define ICARUS_SERIALIZATION_READ_ONLY_DATA_H

#include <string>

#include "nth/container/flyweight_set.h"
#include "serialization/proto/read_only_data.pb.h"

namespace serialization {

// Represents a collection of read-only data merged from all `ReadOnlyData`
// protos passed to `merge`.
struct ReadOnlyData {
  std::pair<size_t, bool> insert(std::string const& content);
  std::pair<size_t, bool> insert(std::string&& content);

  size_t index(std::string const & s) const;

  std::string const& string(size_t n) const;

  static void Serialize(ReadOnlyData const& from, proto::ReadOnlyData& to);
  static bool Deserialize(proto::ReadOnlyData const& from, ReadOnlyData& to);

 private:
  nth::flyweight_set<std::string> data_;
};

}  // namespace serialization

#endif  // ICARUS_SERIALIZATION_READ_ONLY_DATA_H
