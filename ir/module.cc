#include "ir/module.h"

#include "type/pointer.h"

namespace ir {

LocalFnId Module::InsertFunctionIndex(type::Function const *fn_type) {
  auto result = LocalFnId(functions_.size());
  functions_.push_back(NativeFunctionInformation{.fn = Subroutine(fn_type)});
  return result;
}

Fn Module::InsertFunction(ir::Subroutine fn, ir::ByteCode byte_code) {
  LocalFnId n = LocalFnId(functions_.size());
  functions_.push_back(NativeFunctionInformation{
      .fn        = std::move(fn),
      .byte_code = std::move(byte_code),
  });
  return Fn(module_id_, n);
}

Scope Module::InsertScope(type::Scope const *scope_type) {
  auto *s        = &scopes_.emplace_front(scope_type);
  auto data      = std::make_unique<Scope::Data>(Scope::Data{
      .scope = s,
      .type  = scope_type,
  });
  auto *data_ptr = data.get();
  auto [iter, inserted] =
      scope_data_.try_emplace(Scope(data_ptr), ByteCode(), std::move(data));
  ASSERT(inserted == true);
  return Scope(ASSERT_NOT_NULL(iter->second.second.get()));
}

std::pair<Fn, bool> Module::InsertInit(type::Type t) {
  auto [iter, inserted] = init_.try_emplace(t);
  if (not inserted) {
    return std::pair(Fn(module_id_, iter->second), inserted);
  }
  iter->second = InsertFunctionIndex(type::Func(
      core::Parameters<type::QualType>{
          core::AnonymousParameter(type::QualType::NonConstant(type::Ptr(t)))},
      {}));
  return std::pair(Fn(module_id_, iter->second), inserted);
}

std::pair<Fn, bool> Module::InsertDestroy(type::Type t) {
  auto [iter, inserted] = destroy_.try_emplace(t);
  if (not inserted) {
    return std::pair(Fn(module_id_, iter->second), inserted);
  }
  iter->second = InsertFunctionIndex(type::Func(
      core::Parameters<type::QualType>{
          core::AnonymousParameter(type::QualType::NonConstant(type::Ptr(t)))},
      {}));
  return std::pair(Fn(module_id_, iter->second), inserted);
}

std::pair<Fn, bool> Module::InsertCopyAssign(type::Type to, type::Type from) {
  auto [iter, inserted] = copy_assign_.try_emplace(std::pair(to, from));
  if (not inserted) {
    return std::pair(Fn(module_id_, iter->second), inserted);
  }
  iter->second = InsertFunctionIndex(type::Func(
      core::Parameters<type::QualType>{
          core::AnonymousParameter(type::QualType::NonConstant(type::Ptr(to))),
          core::AnonymousParameter(
              type::QualType::NonConstant(type::Ptr(from)))},
      {}));
  return std::pair(Fn(module_id_, iter->second), inserted);
}

std::pair<Fn, bool> Module::InsertMoveAssign(type::Type to, type::Type from) {
  auto [iter, inserted] = move_assign_.try_emplace(std::pair(to, from));
  if (not inserted) {
    return std::pair(Fn(module_id_, iter->second), inserted);
  }
  iter->second = InsertFunctionIndex(type::Func(
      core::Parameters<type::QualType>{
          core::AnonymousParameter(type::QualType::NonConstant(type::Ptr(to))),
          core::AnonymousParameter(
              type::QualType::NonConstant(type::Ptr(from)))},
      {}));
  return std::pair(Fn(module_id_, iter->second), inserted);
}

std::pair<Fn, bool> Module::InsertCopyInit(type::Type to, type::Type from) {
  auto [iter, inserted] = copy_init_.try_emplace(std::pair(to, from));
  auto &entry           = iter->second;
  if (not inserted) {
    return std::pair(Fn(module_id_, iter->second), inserted);
  }
  iter->second = InsertFunctionIndex(
      type::Func(core::Parameters<type::QualType>{core::AnonymousParameter(
                     type::QualType::NonConstant(type::Ptr(from)))},
                 {to}));
  return std::pair(Fn(module_id_, iter->second), inserted);
}

std::pair<Fn, bool> Module::InsertMoveInit(type::Type to, type::Type from) {
  auto [iter, inserted] = move_init_.try_emplace(std::pair(to, from));
  auto &entry           = iter->second;
  if (not inserted) {
    return std::pair(Fn(module_id_, iter->second), inserted);
  }
  iter->second = InsertFunctionIndex(
      type::Func(core::Parameters<type::QualType>{core::AnonymousParameter(
                     type::QualType::NonConstant(type::Ptr(from)))},
                 {to}));
  return std::pair(Fn(module_id_, iter->second), inserted);
}

}  // namespace ir
