#include "module/module.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic_function.h"
#include "type/jump.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"
#include "type/struct.h"
#include "type/tuple.h"
#include "type/type.h"
#include "type/visitor.h"

namespace type {
namespace {
struct ProvenanceTag {};

struct ProvenanceVisitor
    : Visitor<ProvenanceTag, module::BasicModule const *()> {
  module::BasicModule const *Visit(Type t) {
    return Visitor<ProvenanceTag, module::BasicModule const *()>::Visit(
        t.get());
  }
  module::BasicModule const *Visit(ProvenanceTag, Array const *a) final {
    return Visit(a->data_type());
  }
  module::BasicModule const *Visit(ProvenanceTag,
                                   BufferPointer const *p) final {
    return Visit(p->pointee());
  }
  module::BasicModule const *Visit(ProvenanceTag, Enum const *e) final {
    return e->defining_module();
  }
  module::BasicModule const *Visit(ProvenanceTag, Flags const *f) final {
    return f->defining_module();
  }
  module::BasicModule const *Visit(ProvenanceTag, Function const *) final {
    return nullptr;
  }
  module::BasicModule const *Visit(ProvenanceTag, GenericFunction const *) final {
    return nullptr;
  }
  module::BasicModule const *Visit(ProvenanceTag, GenericStruct const *) final {
    // TODO: Implement.
    return nullptr;
  }
  module::BasicModule const *Visit(ProvenanceTag, Opaque const *o) final {
    return o->defining_module();
  }
  module::BasicModule const *Visit(ProvenanceTag, Pointer const *p) final {
    return Visit(p->pointee());
  }
  module::BasicModule const *Visit(ProvenanceTag, Slice const *s) final {
    return Visit(s->data_type());
  }
  module::BasicModule const *Visit(ProvenanceTag, Struct const *s) final {
    return s->defining_module();
  }
};

}  // namespace

// Returns a pointer to the module which defines this type (or null if the type
// is constructed from entirely built-in types and type-constructors).
module::BasicModule const *Provenance(Type t) {
  return ProvenanceVisitor{}.Visit(t);
}

}  // namespace type
