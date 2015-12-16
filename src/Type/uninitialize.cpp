#include "Type.h"

namespace cstdlib {
  extern llvm::Constant* free();
}  // namespace cstdlib

namespace data {
  extern llvm::Value* const_int(int n, bool is_signed = false);
}  // namespace data


extern llvm::Module* global_module;

// This method uninitializes stack space for each particular type.

void Array::uninitialize(llvm::IRBuilder<>& bldr, llvm::Value* alloc) const {
  // TODO look at elements, see if they need deallocation
  auto array_ptr = bldr.CreateLoad(alloc);
  auto basic_ptr_type = get_pointer(get_char())->llvm();

  auto ptr_to_free = bldr.CreateGEP(
      bldr.CreateBitCast(array_ptr, basic_ptr_type),
      { data::const_int(-4, true) }, "ptr_to_free");

  bldr.CreateCall(cstdlib::free(), { ptr_to_free });
}

void UserDefined::uninitialize(llvm::IRBuilder<>& /*bldr*/, llvm::Value* /* alloc*/) const {
  // TODO
}
