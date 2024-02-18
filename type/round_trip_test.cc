#include <string>

#include "nth/io/reader/string.h"
#include "nth/io/serialize/deserialize.h"
#include "nth/io/serialize/serialize.h"
#include "nth/io/writer/string.h"
#include "nth/test/test.h"
#include "type/deserialize.h"
#include "type/serialize.h"
#include "type/type.h"

namespace ic::type {
namespace {

NTH_TEST("round-trip/empty") {
  std::string content;

  TypeSystem ts;
  TypeSystem round_tripped;

  TypeSerializer<nth::io::string_writer> s(content);
  NTH_ASSERT(nth::io::serialize(s, ts));

  TypeDeserializer<nth::io::string_reader> d(content);
  NTH_ASSERT(nth::io::deserialize(d, round_tripped));
  NTH_EXPECT(round_tripped.pointee_types.size() == 0);
  NTH_EXPECT(round_tripped.buffer_pointee_types.size() == 0);
  NTH_EXPECT(round_tripped.slice_element_types.size() == 0);
}

NTH_TEST("round-trip/non-empty") {
  std::string content;

  TypeSystem ts;
  ts.pointer_type(ts.pointer_type(Char));
  ts.slice_type(ts.buffer_pointer_type(Char));
  TypeSystem round_tripped;

  TypeSerializer<nth::io::string_writer> s(content);
  NTH_ASSERT(nth::io::serialize(s, ts));

  TypeDeserializer<nth::io::string_reader> d(content);
  NTH_ASSERT(nth::io::deserialize(d, round_tripped));
  NTH_EXPECT(round_tripped.pointee_types.size() == 2);
  NTH_EXPECT(round_tripped.buffer_pointee_types.size() == 1);
  NTH_EXPECT(round_tripped.slice_element_types.size() == 1);
}

}  // namespace
}  // namespace ic::type
