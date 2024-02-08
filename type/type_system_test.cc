#include "type/type_system.h"

#include "nth/test/test.h"

namespace ic::type {

NTH_TEST("TypeSystem/reindex") {
  TypeSystem a;
  Type pba = a.pointer_type(type::Bool);
  Type pca = a.pointer_type(type::Char);

  TypeSystem b;
  Type pcb = b.pointer_type(type::Char);
  Type pbb = b.pointer_type(type::Bool);

  NTH_EXPECT(pca.index() != pcb.index());
  NTH_EXPECT(pba.index() != pbb.index());
  NTH_EXPECT(Reindex(pba, a, b) == pbb);
}

}  // namespace ic::type
