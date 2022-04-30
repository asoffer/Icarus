#include "ir/module.h"

#include "type/pointer.h"

namespace ir {

LocalFnId Module::InsertFunctionIndex(type::Function const *fn_type) {
  auto result = LocalFnId(functions_.size());
  functions_.push_back(NativeFunctionInformation{.type_ = fn_type});
  return result;
}

Fn Module::InsertFunction(
    type::Function const *t,
    absl::FunctionRef<void(ir::Subroutine &)> initializer) {
  ir::Subroutine subroutine(t);
  initializer(subroutine);

  auto &info = functions_.emplace_back(NativeFunctionInformation{
      .subroutine = std::move(subroutine),
      .type_      = t,
  });

  return Fn(module_id_, LocalFnId(functions_.size() - 1));
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

std::pair<Fn, bool> Module::InsertInit(
    type::Type t, absl::FunctionRef<void(ir::Subroutine &)> initializer) {
  auto [iter, inserted] = init_.try_emplace(t);
  ir::Fn fn;
  if (inserted) {
    auto &local_fn = iter->second;
    auto const *fn_type =
        type::Func(core::Parameters<type::QualType>{core::AnonymousParameter(
                       type::QualType::NonConstant(type::Ptr(t)))},
                   {});
    fn       = InsertFunction(fn_type, initializer);
    local_fn = fn.local();
  }
  return std::pair(fn, inserted);
}

std::pair<Fn, bool> Module::InsertDestroy(
    type::Type t, absl::FunctionRef<void(ir::Subroutine &)> initializer) {
  auto [iter, inserted] = destroy_.try_emplace(t);
  ir::Fn fn;
  if (inserted) {
    auto &local_fn = iter->second;
    auto const *fn_type =
        type::Func(core::Parameters<type::QualType>{core::AnonymousParameter(
                       type::QualType::NonConstant(type::Ptr(t)))},
                   {});
    fn       = InsertFunction(fn_type, initializer);
    local_fn = fn.local();
  }
  return std::pair(fn, inserted);
}

std::pair<Fn, bool> Module::InsertCopyAssign(
    type::Type to, type::Type from,
    absl::FunctionRef<void(ir::Subroutine &)> initializer) {
  auto [iter, inserted] = copy_assign_.try_emplace(std::pair(to, from));
  ir::Fn fn;
  if (inserted) {
    auto &local_fn      = iter->second;
    auto const *fn_type = type::Func(
        core::Parameters<type::QualType>{
            core::AnonymousParameter(
                type::QualType::NonConstant(type::Ptr(to))),
            core::AnonymousParameter(
                type::QualType::NonConstant(type::Ptr(from)))},
        {});

    fn       = InsertFunction(fn_type, initializer);
    local_fn = fn.local();
  }
  return std::pair(fn, inserted);
}

std::pair<Fn, bool> Module::InsertMoveAssign(
    type::Type to, type::Type from,
    absl::FunctionRef<void(ir::Subroutine &)> initializer) {
  auto [iter, inserted] = move_assign_.try_emplace(std::pair(to, from));
  ir::Fn fn;
  if (inserted) {
    auto &local_fn      = iter->second;
    auto const *fn_type = type::Func(
        core::Parameters<type::QualType>{
            core::AnonymousParameter(
                type::QualType::NonConstant(type::Ptr(to))),
            core::AnonymousParameter(
                type::QualType::NonConstant(type::Ptr(from)))},
        {});

    fn       = InsertFunction(fn_type, initializer);
    local_fn = fn.local();
  }
  return std::pair(fn, inserted);
}

std::pair<Fn, bool> Module::InsertCopyInit(
    type::Type to, type::Type from,
    absl::FunctionRef<void(ir::Subroutine &)> initializer) {
  auto [iter, inserted] = copy_init_.try_emplace(std::pair(to, from));
  ir::Fn fn;
  if (inserted) {
    auto &local_fn = iter->second;
    auto const *fn_type =
        type::Func(core::Parameters<type::QualType>{core::AnonymousParameter(
                       type::QualType::NonConstant(type::Ptr(from)))},
                   {to});

    fn       = InsertFunction(fn_type, initializer);
    local_fn = fn.local();
  }
  return std::pair(fn, inserted);
}

std::pair<Fn, bool> Module::InsertMoveInit(
    type::Type to, type::Type from,
    absl::FunctionRef<void(ir::Subroutine &)> initializer) {
  auto [iter, inserted] = move_init_.try_emplace(std::pair(to, from));
  ir::Fn fn;
  if (inserted) {
    auto &local_fn = iter->second;
    auto const *fn_type =
        type::Func(core::Parameters<type::QualType>{core::AnonymousParameter(
                       type::QualType::NonConstant(type::Ptr(from)))},
                   {to});

    fn       = InsertFunction(fn_type, initializer);
    local_fn = fn.local();
  }
  return std::pair(fn, inserted);
}

}  // namespace ir
