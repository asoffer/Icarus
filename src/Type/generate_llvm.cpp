#include "Type.h"

#ifdef DEBUG
#define AT(access) .at( (access) )
#else
#define AT(access) [ (access) ]
#endif

extern llvm::Module* global_module;

void Array::generate_llvm() const {
  if (llvm_type) return;
  data_type.get->generate_llvm();
  auto struct_type = llvm::StructType::create(global_module->getContext());

  struct_type->setBody({Uint, TypePtr(Ptr(data_type))}, /* isPacked = */ false);

  struct_type->setName(Mangle(this));
  llvm_type = struct_type;
}

void Pointer::generate_llvm() const {
  if (llvm_type) return;
  pointee.get->generate_llvm();
  llvm_type = llvm::PointerType::getUnqual(pointee.get->llvm_type);
}


void Function::generate_llvm() const {
  if (llvm_type) return;
  input.get->generate_llvm();
  output.get->generate_llvm();
  std::vector<llvm::Type *> llvm_in;
  llvm::Type *llvm_out = Void;

  if (input.is_tuple()) {
    auto in_tup = static_cast<Tuple *>(input.get);
    for (auto t : in_tup->entries) {
      if (!t.get->add_llvm_input(llvm_in)) {
        llvm_type = nullptr;
        return;
      }
    }
  } else {
    if (!input.get->add_llvm_input(llvm_in)) {
      llvm_type = nullptr;
      return;
    }
  }

  if (output.is_tuple()) {
    auto out_tup = static_cast<Tuple *>(output.get);
    for (auto t : out_tup->entries) {
      if (!Ptr(t)->add_llvm_input(llvm_in)) {
        llvm_type = nullptr;
        return;
      }
    }
  } else if (output.is_enum() || output.is_array() || output.is_primitive()) {
    llvm_out = output;
    if (llvm_out == nullptr) {
      llvm_type = nullptr;
      return;
    }

  } else {
    if (!Ptr(output)->add_llvm_input(llvm_in)) {
      llvm_type = nullptr;
      return;
    }
  }

  llvm_type = llvm::FunctionType::get(llvm_out, llvm_in, false);
}

void Tuple::generate_llvm() const {
  if (llvm_type) return;
  for (auto t : entries) t.get->generate_llvm();
}

void Structure::generate_llvm() const {
  if (llvm_type) return;

  auto struct_type = llvm::StructType::create(global_module->getContext());
  llvm_type = struct_type;

  for (const auto &f : field_type) f.get->generate_llvm();

  struct_type->setName(bound_name);
}

void ParametricStructure::generate_llvm() const {}
void DependentType::generate_llvm() const {}
void TypeVariable::generate_llvm() const {}
void QuantumType::generate_llvm() const {}
void RangeType::generate_llvm() const {} // TODO Assert false?

void Enumeration::generate_llvm() const { /* Generated on creation */ }
void Primitive::generate_llvm() const { /* Generated on creation */ }

// TODO make this a member of Structure instead
void AST::StructLiteral::build_llvm_internals() {
  if (params.empty()) {
    assert(type_value.get->is_struct());
    auto tval = static_cast<Structure *>(type_value.get);

    auto llvm_struct_type =
        static_cast<llvm::StructType *>(tval->llvm_type);
    if (!llvm_struct_type->isOpaque()) return;
  
    for (const auto &decl : declarations) {
      if (decl->type.get->has_vars) return;
    }
  
    size_t num_data_fields = tval->field_num_to_llvm_num.size();
    std::vector<llvm::Type *> llvm_fields(num_data_fields, nullptr);
    for (const auto& kv : tval->field_num_to_llvm_num) {
      llvm_fields[kv.second] = tval->field_type AT(kv.first).get->llvm_type;
    }

    static_cast<llvm::StructType *>(tval->llvm_type)
        ->setBody(std::move(llvm_fields), /* isPacked = */ false);
  } else {
    assert(false);
  }
}
