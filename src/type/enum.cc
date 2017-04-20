#include "type.h"

extern FileType file_type;
extern llvm::Module *global_module;
extern llvm::LLVMContext GlobalContext;

namespace data {
extern llvm::ConstantInt *const_uint(size_t n);
} // namespace data

Enum::Enum(const std::string &name, const std::vector<std::string> &members)
    : bound_name(name), members(members), string_data(nullptr) {
  auto num_members = members.size();
  for (size_t i = 0; i < num_members; ++i) { int_values[members[i]] = i; }

  if (file_type != FileType::None) {

    llvm_type = *ProxyType();

    std::vector<llvm::Constant *> enum_str_elems(num_members, nullptr);
    for (size_t i = 0; i < num_members; ++i) {
      llvm::Type *char_array_type = *Arr(Char, members[i].size() + 1);

      auto enum_str = new llvm::GlobalVariable(
          /*      Module = */ *global_module,
          /*        Type = */ char_array_type,
          /*  isConstant = */ true,
          /*     Linkage = */ llvm::GlobalValue::ExternalLinkage,
          /* Initializer = */ llvm::ConstantDataArray::getString(
              GlobalContext, members[i], true),
          /*        Name = */ members[i]);
      enum_str->setAlignment(1);
      enum_str_elems[i] = llvm::ConstantExpr::getGetElementPtr(
          char_array_type, enum_str,
          llvm::ArrayRef<llvm::Constant *>{data::const_uint(0),
                                           data::const_uint(0)});
    }

    llvm::Type *llvm_global_type         = *Arr(Ptr(Char), num_members);
    llvm::ArrayType *char_ptr_array_type = (llvm::ArrayType *)llvm_global_type;

    string_data = new llvm::GlobalVariable(
        /*      Module = */ *global_module,
        /*        Type = */ char_ptr_array_type,
        /*  isConstant = */ false,
        /*     Linkage = */ llvm::GlobalValue::ExternalLinkage,
        /* Initializer = */ llvm::ConstantArray::get(char_ptr_array_type,
                                                     enum_str_elems),
        /*        Name = */ bound_name + ".name.array");
  }
}

size_t Enum::IndexOrFail(const std::string &str) const {
  auto iter = int_values.find(str);
  return (iter == int_values.end()) ? FAIL : iter->second;
}

static size_t BytesAndAlignment(const Enum *e) {
  auto num_members = e->members.size();
  if (num_members < (1ul << (1ul << 3ul))) { return 1; }
  if (num_members < (1ul << (1ul << 4ul))) { return 2; }
  if (num_members < (1ul << (1ul << 5ul))) { return 4; }
  // TODO Error message if you have too many members
  return 8;
}

size_t Enum::bytes() const { return BytesAndAlignment(this); }
size_t Enum::alignment() const { return BytesAndAlignment(this); }

Type *Enum::ProxyType() const {
  switch (BytesAndAlignment(this)) {
  case 1: return Char;
  case 2: return U16;
  case 4: return U32;
  case 8: return Uint;
  default: UNREACHABLE;
  }
}

IR::Value Enum::EmitInitialValue() const {
  switch (BytesAndAlignment(this)) {
  case 1: return IR::Value::Char('\0');
  case 2: return IR::Value::U16((uint16_t)0);
  case 4: return IR::Value::U32((uint32_t)0);
  case 8: return IR::Value::Uint(0ul);
  default: UNREACHABLE;
  }
}

IR::Value Enum::EmitLiteral(const std::string &member_name) const {
  switch (BytesAndAlignment(this)) {
  case 1: return IR::Value::Char((char)int_values.at(member_name));
  case 2: return IR::Value::U16((uint16_t)int_values.at(member_name));
  case 4: return IR::Value::U32((uint32_t)int_values.at(member_name));
  case 8: return IR::Value::Uint((size_t)int_values.at(member_name));
  default: UNREACHABLE;
  }
}

std::string Enum::to_string() const { return bound_name; }
bool Enum::private_has_vars() { return false; }

