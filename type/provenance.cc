#include "module/module.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/generic.h"
#include "type/opaque.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/slice.h"
#include "type/struct.h"
#include "type/type.h"

namespace type {
namespace {

struct ProvenanceVisitor {
  using signature = module::Module const *();

  module::Module const *operator()(type::Type t) {
    return t.visit<ProvenanceVisitor>(*this);
  }

  module::Module const *operator()(auto const *t) {
    using type = std::decay_t<decltype(*t)>;
    if constexpr (requires { t->defining_module(); }) {
      return t->defining_module();
    } else if constexpr (base::meta<type> == base::meta<Array> or
                         base::meta<type> == base::meta<Slice>) {
      return (*this)(t->data_type());
    } else if constexpr (base::meta<type> == base::meta<Pointer> or
                         base::meta<type> == base::meta<BufferPointer>) {
      return (*this)(t->pointee());
    } else {
      return nullptr;
    }
  }
};

}  // namespace

// Returns a pointer to the module which defines this type (or null if the type
// is constructed from entirely built-in types and type-constructors).
module::Module const *Provenance(Type t) {
  ProvenanceVisitor v;
  return t.visit<ProvenanceVisitor>(v);
}

}  // namespace type
