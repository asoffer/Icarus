#include "type/provenance.h"

#include "ir/value/module_id.h"
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

  explicit ProvenanceVisitor(module::ModuleTable const *table)
      : table_(*ASSERT_NOT_NULL(table)) {}

  module::Module const *operator()(type::Type t) {
    return t.visit<ProvenanceVisitor>(*this);
  }

  module::Module const *operator()(auto const *t) {
    constexpr auto type = base::meta<std::decay_t<decltype(*t)>>;
    if constexpr (requires { t->defining_module(); }) {
      if constexpr (base::meta<decltype(t->defining_module())> ==
                    base::meta<ir::ModuleId>) {
        return table_.module(t->defining_module());
      } else {
        return t->defining_module();
      }
    } else if constexpr (type == base::meta<Array> or
                         type == base::meta<Slice>) {
      return (*this)(t->data_type());
    } else if constexpr (type == base::meta<Pointer> or
                         type == base::meta<BufferPointer>) {
      return (*this)(t->pointee());
    } else {
      return nullptr;
    }
  }

 private:
  module::ModuleTable const &table_;
};

}  // namespace

// Returns a pointer to the module which defines this type (or null if the type
// is constructed from entirely built-in types and type-constructors).
module::Module const *Provenance(Type t, module::ModuleTable const &table) {
  ProvenanceVisitor v(&table);
  return t.visit<ProvenanceVisitor>(v);
}

}  // namespace type
