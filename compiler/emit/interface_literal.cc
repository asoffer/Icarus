#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/copy_move_assignment.h"
#include "compiler/module.h"
#include "ir/value/result_buffer.h"
#include "type/typed_value.h"

namespace compiler {

ir::Subroutine MakeInterfaceSubroutine(Compiler &c,
                                       ast::Declaration::Id const *variable,
                                       ast::Expression const *expr) {
  auto *fn_type = type::Func(
      {core::AnonymousParameter(type::QualType::NonConstant(type::Type_))},
      {type::Interface});
  ir::Subroutine fn(fn_type);
  c.push_current(&fn);
  absl::Cleanup cleanup = [&] { c.state().current.pop_back(); };
  c.current_block()     = fn.entry();

  c.state().set_addr(variable, ir::Reg::Parameter(0));
  ir::PartialResultBuffer result;
  c.EmitToBuffer(expr, result);

  c.current_block()->Append(ir::StoreInstruction<ir::Interface>{
      .value    = result[0].get<ir::Interface>(),
      .location = ir::Reg::Output(0),
  });
  c.current_block()->set_jump(ir::JumpCmd::Return());
  return fn;
}

void Compiler::EmitToBuffer(ast::InterfaceLiteral const *node,
                            ir::PartialResultBuffer &out) {
  absl::btree_map<std::string, ir::Subroutine> members;
  for (auto const &[name, intf] : node->members()) {
    members.emplace(name, MakeInterfaceSubroutine(
                              *this, &node->context().ids()[0], intf.get()));
  }
  out.append(resources().interface_manager->UserDefined(std::move(members)));
}

void Compiler::EmitCopyAssign(
    ast::InterfaceLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveAssign(
    ast::InterfaceLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitCopyInit(
    ast::InterfaceLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveInit(
    ast::InterfaceLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

}  // namespace compiler
