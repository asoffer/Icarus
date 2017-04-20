#include "type.h"

extern llvm::Module* global_module;

void Array::generate_llvm() {
  if (time() == Time::compile || llvm_type) { return; }

  if (fixed_length) {
    llvm_type = llvm::ArrayType::get(*data_type, len);

  } else {
    data_type->generate_llvm();

    auto struct_type = llvm::StructType::create(global_module->getContext());

    struct_type->setBody({*Uint, *Ptr(data_type)}, /* isPacked = */ false);
    struct_type->setName(Mangle(this));
    llvm_type = struct_type;
  }
}

void Pointer::generate_llvm() {
  if (time() == Time::compile || llvm_type) { return; }
  pointee->generate_llvm();
  llvm_type = llvm::PointerType::getUnqual(pointee->llvm_type);
}

static void AddLLVMInput(Type *t, std::vector<llvm::Type *> &input_vec) {
  if (t->is_pointer()) {
    input_vec.push_back(*t);
    return;
  }

  if (t == Void || t == Type_ || t->has_vars()) { return; }
  assert(t->llvm_type);

  if (t->is_primitive() || t->is_enum()) {
    input_vec.push_back(*t);

  } else if (t->is_big() || t->is_function()) {
    input_vec.push_back(*Ptr(t));

  } else {
    std::cerr << *t << std::endl;
    NOT_YET;
  }
}

void Function::generate_llvm() {
  if (time() == Time::compile || time() == Time::mixed || llvm_type) return;
  input->generate_llvm();
  output->generate_llvm();
  // TODO change ptr(char) to opaque pointer
  std::vector<llvm::Type *> llvm_in = {*Ptr(Char)};
  llvm::Type *llvm_out              = *Void;

  if (input->is_tuple()) {
    auto in_tup = (Tuple *)input;
    for (auto t : in_tup->entries) { AddLLVMInput(t, llvm_in); }

  } else {
    AddLLVMInput(input, llvm_in);
  }

  if (!output->is_big()) {
    llvm_out = *output;
    if (llvm_out == nullptr) {
      llvm_type = nullptr;
      return;
    }
  } else if (output->is_tuple()) {
    auto out_tup = (Tuple *)output;
    for (auto t : out_tup->entries) { AddLLVMInput(Ptr(t), llvm_in); }

  } else if (output->is_function()) {
    llvm_out = *Ptr(output);

  } else {
    AddLLVMInput(Ptr(output), llvm_in);
  }

  if (output->is_function()) {
    llvm_type = llvm::FunctionType::get(llvm::PointerType::getUnqual(llvm_out),
                                        llvm_in, false);
  } else {
    llvm_type = llvm::FunctionType::get(llvm_out, llvm_in, false);
  }

  auto struct_type = llvm::StructType::create(global_module->getContext());
  // TODO change ptr(char) to opaque pointer
  struct_type->setBody({*Ptr(Char), llvm::PointerType::getUnqual(llvm_type)},
                       /* isPacked = */ false);
  struct_type->setName(Mangle(this));
  llvm_type = struct_type;
  llvm_type->dump();
}

void Tuple::generate_llvm() {
  if (time() == Time::compile || llvm_type) return;
  for (auto t : entries) t->generate_llvm();
}

void Struct::generate_llvm() {
  if (time() == Time::compile || llvm_type) return;

  auto struct_type = llvm::StructType::create(global_module->getContext());
  llvm_type        = struct_type;

  for (auto &f : field_type) f->generate_llvm();

  size_t num_data_fields = field_type.size();
  std::vector<llvm::Type *> llvm_fields(num_data_fields, nullptr);
  for (size_t i = 0; i < num_data_fields; ++i) {
    auto t = field_type[i];

    if (t->is_function()) {
      t = Ptr(t);
      t->generate_llvm();
    }
    llvm_fields[i] = t->llvm_type;
  }

  ((llvm::StructType *)llvm_type)
      ->setBody(std::move(llvm_fields),
                /* isPacked = */ false);

  struct_type->setName(bound_name);
}

void TypeVariable::generate_llvm() {}
void RangeType::generate_llvm() {} // TODO Assert false?
void SliceType::generate_llvm() {} // TODO Assert false?

void Scope_Type::generate_llvm() { UNREACHABLE; }

void ParamStruct::generate_llvm() {} // Never to be called
void Enum::generate_llvm() {}        // Generated on creation
void Primitive::generate_llvm() {}   // Generated on creation
