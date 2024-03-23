#include "ir/emit.h"

#include "common/debug.h"
#include "common/module_id.h"
#include "common/resources.h"
#include "ir/serialize.h"
#include "jasmin/core/function.h"
#include "jasmin/instructions/arithmetic.h"
#include "nth/container/interval.h"
#include "nth/container/stack.h"
#include "nth/debug/debug.h"
#include "nth/debug/log/log.h"

namespace ic {
namespace {

size_t ValueSize = 8;

void StoreStackValue(IrFunction& f, type::ByteWidth offset, type::Type t) {
  type::ByteWidth type_width = type::Contour(t).byte_width();
  type::ByteWidth end        = offset + type_width;
  type::ByteWidth position =
      end.aligned_backward_to(type::Alignment(ValueSize));
  if (position != end) {
    f.append<jasmin::StackOffset>(position.value());
    f.append<Store>((end - position).value());
  }
  NTH_REQUIRE((v.debug), position >= offset);
  while (position != offset) {
    position -= type::ByteWidth(ValueSize);
    f.append<jasmin::StackOffset>(position.value());
    f.append<Store>(ValueSize);
  }
}

void Load(IrFunction& f, type::ByteWidth width) {
  size_t i = ValueSize;
  for (; i < width.value(); i += ValueSize) {
    f.append<jasmin::Duplicate>();
    f.append<jasmin::Push<uint64_t>>(ValueSize);
    f.append<AddPointer>();
    f.append<jasmin::Swap>();
    f.append<jasmin::Load>(ValueSize);
    f.append<jasmin::Swap>();
  }

  f.append<jasmin::Load>(width.value() - (i - ValueSize));
}

void LoadStackValue(IrFunction& f, type::ByteWidth offset, type::Type t) {
  type::ByteWidth type_width = type::Contour(t).byte_width();
  type::ByteWidth end        = offset + type_width;
  while (offset + type::ByteWidth(ValueSize) <= end) {
    f.append<jasmin::StackOffset>(offset.value());
    f.append<jasmin::Load>(ValueSize);
    offset += type::ByteWidth(ValueSize);
  }
  if (offset < end) {
    f.append<jasmin::StackOffset>(offset.value());
    f.append<jasmin::Load>((end - offset).value());
  }
}

void HandleParseTreeNodeModule(ParseNodeIndex index, EmitContext& context) {
  context.current_function().append<jasmin::Return>();
  context.pop_function();
}

void HandleParseTreeNodeModuleStart(ParseNodeIndex index,
                                    EmitContext& context) {
  auto& f = context.current_module.initializer();
  context.push_function(f, LexicalScope::Index::Root());
  f.append<jasmin::StackAllocate>(context.current_storage().size().value());
}

void HandleParseTreeNodeImportStart(ParseNodeIndex index,
                                    EmitContext& context) {}
void HandleParseTreeNodePointerStart(ParseNodeIndex index,
                                     EmitContext& context) {}
void HandleParseTreeNodeSliceStart(ParseNodeIndex index, EmitContext& context) {
}
void HandleParseTreeNodeBufferPointerStart(ParseNodeIndex index,
                                           EmitContext& context) {}
void HandleParseTreeNodeMinusStart(ParseNodeIndex index, EmitContext& context) {
}

void HandleParseTreeNodeMinus(ParseNodeIndex index, EmitContext& context) {
  auto qt = context.QualifiedTypeOf(index - 1);
  if (qt.type().kind() == type::Type::Kind::Primitive) {
    switch (qt.type().as<type::PrimitiveType>().primitive_kind()) {
      case type::PrimitiveType::Kind::I8:
        context.current_function().append<jasmin::Negate<int8_t>>();
        break;
      case type::PrimitiveType::Kind::I16:
        context.current_function().append<jasmin::Negate<int16_t>>();
        break;
      case type::PrimitiveType::Kind::I32:
        context.current_function().append<jasmin::Negate<int32_t>>();
        break;
      case type::PrimitiveType::Kind::I64:
        context.current_function().append<jasmin::Negate<int64_t>>();
        break;
      case type::PrimitiveType::Kind::Integer:
        context.current_function().append<jasmin::Negate<Integer>>();
        break;
      case type::PrimitiveType::Kind::F32:
        context.current_function().append<jasmin::Negate<float>>();
        break;
      case type::PrimitiveType::Kind::F64:
        context.current_function().append<jasmin::Negate<double>>();
        break;
      default: NTH_UNREACHABLE();
    }
  } else {
    NTH_UNIMPLEMENTED("{}") <<= {qt};
  }
}

void HandleParseTreeNodeAddressStart(ParseNodeIndex index,
                                     EmitContext& context) {
  context.queue.front().value_category_stack.push_back(
      EmitContext::ValueCategory::Reference);
}

void HandleParseTreeNodeDerefStart(ParseNodeIndex index, EmitContext& context) {
  context.queue.front().value_category_stack.push_back(
      EmitContext::ValueCategory::Value);
}

void HandleParseTreeNodeAddress(ParseNodeIndex index, EmitContext& context) {
  context.queue.front().value_category_stack.pop_back();
}

void HandleParseTreeNodeDeref(ParseNodeIndex index, EmitContext& context) {
  context.queue.front().value_category_stack.pop_back();
  NTH_REQUIRE((v.debug),
              not context.queue.front().value_category_stack.empty());
  if (context.queue.front().value_category_stack.back() ==
      EmitContext::ValueCategory::Value) {
    auto qt = context.QualifiedTypeOf(index - 1);
    Load(context.current_function(), type::Contour(qt.type()).byte_width());
  }
}

void HandleParseTreeNodeScopeBlockStart(ParseNodeIndex index,
                                        EmitContext& context) {
  context.current_function().append<NoOp>();
}

void HandleParseTreeNodeScopeBlock(ParseNodeIndex index, EmitContext& context) {
  // TODO: NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeScopeBodyStart(ParseNodeIndex index,
                                       EmitContext& context) {
  context.queue.front().branches.push_back(
      context.current_function().append_with_placeholders<jasmin::Jump>());
}

void HandleParseTreeNodeScope(ParseNodeIndex index, EmitContext& context) {
  nth::interval<jasmin::InstructionIndex> final_jump =
      context.queue.front().branches.back();
  auto land = context.current_function().append<NoOp>();
  context.queue.front().branches.pop_back();
  context.current_function().set_value(
      final_jump, 0, land.lower_bound() - final_jump.lower_bound());
}

void HandleParseTreeNodeScopeLiteral(ParseNodeIndex index,
                                     EmitContext& context) {
  Scope const* s = &context.current_scope();
  context.pop_scope();
  NTH_UNIMPLEMENTED();
  // context.current_function().append<jasmin::Push<void const*>>(s);
}

Iteration HandleParseTreeNodeScopeLiteralStart(ParseNodeIndex index,
                                               EmitContext& context) {
  context.push_scope(context.current_module.add_scope(),
                     context.Node(index).scope_index);
  return Iteration::SkipTo(index + 2);
}

void HandleParseTreeNodeBooleanLiteral(ParseNodeIndex index,
                                       EmitContext& context) {
  auto node = context.Node(index);
  NTH_REQUIRE((v.debug), node.token.kind() == Token::Kind::True or
                             node.token.kind() == Token::Kind::False);
  context.current_function().append<jasmin::Push<bool>>(node.token.kind() ==
                                                        Token::Kind::True);
}

void HandleParseTreeNodeNullTypeLiteral(ParseNodeIndex index,
                                        EmitContext& context) {
  NTH_REQUIRE((v.debug), context.Node(index).token.kind() == Token::Kind::Null);
  context.current_function().append<PushNull>();
}

void HandleParseTreeNodeIntegerLiteral(ParseNodeIndex index,
                                       EmitContext& context) {
  // TODO: Push an actual arbitrary-precision integer.
  // TODO: ToRepresentation is not right for large values.
  context.current_function().append<jasmin::Push<int64_t>>(
      Integer::ToRepresentation(context.Node(index).token.AsInteger()));
}

void HandleParseTreeNodeStringLiteral(ParseNodeIndex index,
                                      EmitContext& context) {
  std::string_view s =
      resources.StringLiteral(context.Node(index).token.AsStringLiteralIndex());
  context.current_function().append<PushStringLiteral>(StringLiteral(s));
}

void HandleParseTreeNodeCharacterLiteral(ParseNodeIndex index,
                                         EmitContext& context) {
  // TODO: Distinguish char from uint8_t and int8_T.
  context.current_function().append<jasmin::Push<char>>(
      context.Node(index).token.AsCharacterLiteral());
}

void HandleParseTreeNodeTypeLiteral(ParseNodeIndex index,
                                    EmitContext& context) {
  auto node = context.Node(index);
  switch (node.token.kind()) {
#define IC_XMACRO_PRIMITIVE_TYPE(kind, symbol, spelling)                       \
  case Token::Kind::kind:                                                      \
    context.current_function().append<jasmin::Push<type::Type>>(type::symbol); \
    break;
#include "common/language/primitive_types.xmacro.h"
    default: NTH_UNREACHABLE();
  }
}

void HandleParseTreeNodeBuiltinLiteral(ParseNodeIndex index,
                                       EmitContext& context) {
  context.current_function().append<jasmin::Push<ModuleId>>(
      ModuleId::Builtin());
}

void HandleParseTreeNodeScopeStart(ParseNodeIndex, EmitContext&) {}

Iteration HandleParseTreeNodeFunctionLiteralStart(ParseNodeIndex index,
                                                  EmitContext& context) {
  auto fn_type = context.QualifiedTypeOf(index).type().as<type::FunctionType>();
  auto parameters    = fn_type.parameters();
  size_t input_size  = 0;
  size_t output_size = 0;
  type::ByteWidth bytes(0);
  std::vector<type::ByteWidth> storage_offsets;
  storage_offsets.reserve(parameters.size());
  for (size_t i = 0; i < parameters.size(); ++i) {
    input_size += type::JasminSize(parameters[i].type);
    storage_offsets.push_back(bytes);
    auto contour = type::Contour(parameters[i].type);
    bytes.align_forward_to(contour.alignment());
    bytes += contour.byte_width();
  }

  for (auto const& t : fn_type.returns()) {
    output_size += type::JasminSize(t);
  }
  context.push_function(
      context.current_module.add_function(input_size, output_size),
      context.Node(index).scope_index);
  auto& f = context.current_function();
  f.append<jasmin::StackAllocate>(context.current_storage().size().value());

  auto storage_iter = storage_offsets.rbegin();
  for (size_t i = 0; i < parameters.size(); ++i, ++storage_iter) {
    StoreStackValue(f, *storage_iter,
                    parameters[parameters.size() - 1 - i].type);
  }

  // TODO: We should be able to jump directly rather than iterate and check,
  // since nested functions make this wrong.
  while (context.Node(index).kind !=
         ParseNode::Kind::FunctionLiteralSignature) {
    ++index;
  }
  return Iteration::SkipTo(index);
}

void HandleParseTreeNodeFunctionLiteralSignature(ParseNodeIndex index,
                                                 EmitContext& context) {}

void HandleParseTreeNodeDeclaration(ParseNodeIndex index,
                                    EmitContext& context) {
  NTH_REQUIRE(not context.queue.front().declaration_stack.empty());
  auto const& decl_info = context.queue.front().declaration_stack.back();
  if (not decl_info.kind.has_initializer()) {
    NTH_REQUIRE((v.debug), not decl_info.kind.inferred_type());
    if (not decl_info.kind.parameter()) {
      auto& f = context.current_function();
      auto t  = context.QualifiedTypeOf(index).type();
      switch (t.kind()) {
        case type::Type::Kind::Primitive:
          switch (t.as<type::PrimitiveType>().primitive_kind()) {
            case type::PrimitiveType::Kind::Bool:
              f.append<jasmin::Push<bool>>(false);
              break;
            case type::PrimitiveType::Kind::Char:
              // TODO: Distinguish char.
              f.append<jasmin::Push<char>>('\0');
              break;
            case type::PrimitiveType::Kind::Byte:
              f.append<jasmin::Push<std::byte>>(std::byte{});
              break;
            case type::PrimitiveType::Kind::I8:
              f.append<jasmin::Push<int8_t>>(0);
              break;
            case type::PrimitiveType::Kind::I16:
              f.append<jasmin::Push<int16_t>>(0);
              break;
            case type::PrimitiveType::Kind::I32:
              f.append<jasmin::Push<int32_t>>(0);
              break;
            case type::PrimitiveType::Kind::I64:
              f.append<jasmin::Push<int64_t>>(0);
              break;
            case type::PrimitiveType::Kind::U8:
              f.append<jasmin::Push<uint8_t>>(0);
              break;
            case type::PrimitiveType::Kind::U16:
              f.append<jasmin::Push<uint16_t>>(0);
              break;
            case type::PrimitiveType::Kind::U32:
              f.append<jasmin::Push<uint32_t>>(0);
              break;
            case type::PrimitiveType::Kind::U64:
              f.append<jasmin::Push<uint64_t>>(0);
              break;
            case type::PrimitiveType::Kind::F32:
              f.append<jasmin::Push<float>>(0);
              break;
            case type::PrimitiveType::Kind::F64:
              f.append<jasmin::Push<double>>(0);
              break;
            default: NTH_UNIMPLEMENTED("Default initialization for {}") <<= {t};
          }
        case type::Type::Kind::Pointer:
        case type::Type::Kind::BufferPointer: f.append<PushNull>(); break;
        default: NTH_UNIMPLEMENTED("Default initialization for {}") <<= {t};
      }
      auto range = context.current_storage().range(index);
      context.current_function().append<jasmin::StackOffset>(
          range.lower_bound().value());
      context.current_function().append<Store>(range.length().value());
    }
  } else if (decl_info.kind.inferred_type()) {
    if (decl_info.kind.constant()) {
      auto& f = context.current_function();
      f.append<jasmin::Return>();
      nth::stack<jasmin::Value> value_stack;
      f.invoke(value_stack);
      auto t = context.QualifiedTypeOf(index).type();
      context.constants.insert_or_assign(
          context.tree.subtree_range(index),
          EmitContext::ComputedConstants(index, std::move(value_stack), {t}));
      delete &f;
      context.pop_function();
    } else {
      auto range = context.current_storage().range(index);
      context.current_function().append<jasmin::StackOffset>(
          range.lower_bound().value());
      context.current_function().append<Store>(range.length().value());
    }
  } else {
    // TODO: Cast to declared type
    if (decl_info.kind.constant()) {
      auto& f = context.current_function();
      f.append<jasmin::Return>();
      nth::stack<jasmin::Value> value_stack;
      f.invoke(value_stack);
      auto t = context.QualifiedTypeOf(index).type();
      context.constants.insert_or_assign(
          context.tree.subtree_range(index),
          EmitContext::ComputedConstants(index, std::move(value_stack), {t}));
      delete &f;
      context.pop_function();
    } else {
      auto range = context.current_storage().range(index);
      context.current_function().append<jasmin::StackOffset>(
          range.lower_bound().value());
      context.current_function().append<Store>(range.length().value());
    }
  }

  if (context.queue.front().declaration_stack.size() == 1 and
      decl_info.kind.constant()) {
    // This is a top-level declaration, we need to export it.
    // TODO: Actually exporting should not be the default.
    context.declarations_to_export.insert(index);
  }
  context.queue.front().declaration_stack.pop_back();
}

void HandleParseTreeNodeStatement(ParseNodeIndex index, EmitContext& context) {
  switch (
      context.Node(context.tree.first_descendant_index(index)).statement_kind) {
    case ParseNode::StatementKind::Expression: {
      auto iter = context.statement_expression_info.find(index);
      NTH_REQUIRE(iter != context.statement_expression_info.end())
          .Log<"For {}">(context.tree.first_descendant_index(index));
      size_t size_to_drop = iter->second.second;
      if (size_to_drop != 0) {
        // TODO: Add a Drop function with an immediate value.
        for (size_t i = 0; i < size_to_drop; ++i) {
          context.current_function().append<jasmin::Drop>();
        }
      }
    } break;
    default: break;
  }

  context.queue.front().value_category_stack.pop_back();
}

void HandleParseTreeNodeStatementSequence(ParseNodeIndex index,
                                          EmitContext& context) {}

Iteration HandleParseTreeNodeIdentifier(ParseNodeIndex index,
                                        EmitContext& context) {
  auto [decl_id_index, decl_index] = context.declarator.at(index);
  // TODO: The declarator that this identifier is mapped to may be a constant we
  // can look up, but it may not be the right one!
  //
  // TODO: We should know a priori if it's a constant or not.
  if (auto const* constant = context.constants.mapped_range(decl_index)) {
    context.Push(constant->second);
    return Iteration::Continue;
  } else if (auto offset = context.current_storage().try_offset(decl_index)) {
    switch (context.queue.front().value_category_stack.back()) {
      case EmitContext::ValueCategory::Value: {
        auto qt = context.QualifiedTypeOf(decl_index);
        LoadStackValue(context.current_function(), *offset, qt.type());
      } break;
      case EmitContext::ValueCategory::Reference:
        context.current_function().append<jasmin::StackOffset>(offset->value());
        break;
    }
    return Iteration::Continue;
  } else {
    context.queue.emplace(context.queue.front()).range =
        context.tree.subtree_range(decl_index);
    return Iteration::PauseRetry;
  }
}

void HandleParseTreeNodeDeclaredIdentifier(ParseNodeIndex index,
                                           EmitContext& context) {}

void HandleParseTreeNodeInfixOperator(ParseNodeIndex index,
                                      EmitContext& context) {}

void HandleParseTreeNodeExpressionPrecedenceGroup(ParseNodeIndex index,
                                                  EmitContext& context) {
  auto iter = context.tree.children(index).begin();
  ++iter;
  auto operator_node = *iter;
  size_t child_count = context.Node(index).child_count;
  switch (operator_node.token.kind()) {
    case Token::Kind::MinusGreater: {
      for (size_t i = 0; i < child_count / 2; ++i) {
        context.current_function().append<ConstructFunctionType>();
      }
    } break;
    case Token::Kind::Plus: {
      for (size_t i = 0; i < child_count / 2; ++i) {
        context.current_function().append<jasmin::Add<int64_t>>();
      }
    } break;
    case Token::Kind::Minus: {
      for (size_t i = 0; i < child_count / 2; ++i) {
        context.current_function().append<jasmin::Subtract<int64_t>>();
      }
    } break;
    case Token::Kind::Percent: {
      for (size_t i = 0; i < child_count / 2; ++i) {
        context.current_function().append<jasmin::Mod<int64_t>>();
      }
    } break;
    case Token::Kind::Star: {
      for (size_t i = 0; i < child_count / 2; ++i) {
        context.current_function().append<jasmin::Multiply<int64_t>>();
      }
    } break;
    case Token::Kind::EqualEqual: {
      context.current_function().append<jasmin::Equal<int64_t>>();
    } break;
    case Token::Kind::NotEqual: {
      context.current_function().append<jasmin::Equal<int64_t>>();
      context.current_function().append<jasmin::Not>();
    } break;
    case Token::Kind::Less: {
      context.current_function().append<jasmin::LessThan<int64_t>>();
    } break;
    case Token::Kind::Greater: {
      context.current_function().append<jasmin::Swap>();
      context.current_function().append<jasmin::LessThan<int64_t>>();
    } break;
    case Token::Kind::LessEqual: {
      context.current_function().append<jasmin::Swap>();
      context.current_function().append<jasmin::LessThan<int64_t>>();
      context.current_function().append<jasmin::Not>();
    } break;
    case Token::Kind::GreaterEqual: {
      context.current_function().append<jasmin::LessThan<int64_t>>();
      context.current_function().append<jasmin::Not>();
    } break;
    case Token::Kind::As: {
      context.current_function().append<jasmin::Drop>();
      // TODO: Cast. For now most integer casts are correct enough at the jasmin
      // level, we can ignore this. (Jasmin debug info would flag it though).
    } break;
    default: NTH_UNIMPLEMENTED("{}") <<= {operator_node.token};
  }
}

Iteration HandleParseTreeNodeDeclarationStart(ParseNodeIndex index,
                                              EmitContext& context) {
  auto info = context.Node(index).declaration_info;
  auto& d   = context.queue.front().declaration_stack.emplace_back();
  d.kind    = info.kind;
  d.index   = index;
  if (not info.kind.has_initializer()) {
    if (info.kind.constant()) {
      NTH_UNIMPLEMENTED();
    } else {
      // Nothing to do here. The type will have already been calculated.
      return Iteration::Continue;
    }
  } else if (info.kind.inferred_type()) {
    if (info.kind.constant()) {
      // TODO: The return might actually be wider and we need to handle that.
      context.push_function(*new IrFunction(0, 1),
                            context.queue.front().function_stack.back());
      return Iteration::PauseMoveOn;
    } else {
      auto iter = context.tree.child_indices(info.index).begin();
      return Iteration::SkipTo(*++iter + 1);
    }
  } else {
    // TODO: Casting to declared type.
    if (info.kind.constant()) {
      // TODO: The return might actually be wider and we need to handle that.
      context.push_function(*new IrFunction(0, 1),
                            context.queue.front().function_stack.back());
      return Iteration::PauseMoveOn;
    } else {
      auto iter = context.tree.child_indices(info.index).begin();
      return Iteration::SkipTo(*++iter + 1);
    }
  }
}

void HandleParseTreeNodeMemberExpression(ParseNodeIndex index,
                                         EmitContext& context) {
  if (context.QualifiedTypeOf(index - 1).type().kind() ==
      type::Type::Kind::Slice) {
    if (context.Node(index).token.Identifier() == Identifier("count")) {
      context.current_function().append<jasmin::Swap>();
    }
    context.current_function().append<jasmin::Drop>();
  } else {
    // TODO: Fix this bug.
    decltype(context.constants.mapped_range(index - 1)) mapped_range = nullptr;
    for (auto const& p : context.constants.mapped_intervals()) {
      if (p.first.contains(index - 1)) {
        mapped_range = &p;
        break;
      }
    }
    // auto const* mapped_range = context.constants.mapped_range(index - 1);
    NTH_REQUIRE((v.harden), mapped_range != nullptr);
    context.current_function().append<jasmin::Drop>();

    // TODO: There's a bug that manifests here from time to time. An example of
    // when it occurs is:
    // ```
    // fn() -> () {
    //    let f = builtin.foreign(...)
    //    f(...)
    // }
    // ```
    // The problem is that the function literal body is evaluated and stored in
    // the constant-mapped-range earlier than the function body is processed.
    // This means that when we go to compute the constant `builtin` when
    // processing the body, it's identified as the constant associated with the
    // entire range (namely the function!).
    ModuleId module_id;
    bool successfully_deserialized =
        IcarusDeserializeValue(mapped_range->second.value_span(), module_id);
    NTH_REQUIRE((v.harden), successfully_deserialized);

    auto symbol = context.module(module_id).Lookup(
        context.Node(index).token.Identifier());
    context.Push(symbol.value(), symbol.type());
  }
}

void HandleParseTreeNodeIndexExpression(ParseNodeIndex index,
                                        EmitContext& context) {
  // TODO: The offset shouldn't need to be traversed again to find the very
  // first node. We could either store these backwards so it's first, have the
  // IndexExpression node store the start location.
  auto iter = context.tree.child_indices(index).begin();
  for (; context.Node(*iter).kind != ParseNode::Kind::IndexArgumentStart;
       ++iter) {}
  auto qt = context.QualifiedTypeOf(*++iter);
  auto& f = context.current_function();
  if (qt.type().kind() == type::Type::Kind::Slice) {
    auto t    = qt.type().as<type::SliceType>().element_type();
    auto size = type::Contour(t).byte_width();
    f.append<jasmin::Swap>();
    f.append<jasmin::Drop>();
    f.append<jasmin::Push<int64_t>>(size.value());
    f.append<jasmin::Multiply<int64_t>>();
    f.append<AddPointer>();

    if (context.queue.front().value_category_stack.back() ==
        EmitContext::ValueCategory::Value) {
      Load(f, size);
    }
  } else if (qt.type().kind() == type::Type::Kind::BufferPointer) {
    auto t    = qt.type().as<type::BufferPointerType>().pointee();
    auto size = type::Contour(t).byte_width();
    f.append<jasmin::Push<int64_t>>(size.value());
    f.append<jasmin::Multiply<int64_t>>();
    f.append<AddPointer>();

    if (context.queue.front().value_category_stack.back() ==
        EmitContext::ValueCategory::Value) {
      Load(f, size);
    }
  } else if (qt.type() == type::Type_) {
    f.append<ConstructRefinementType>();
  } else {
    NTH_UNREACHABLE();
  }
}

void HandleParseTreeNodeCallExpression(ParseNodeIndex index,
                                       EmitContext& context) {
  auto iter = context.instruction_spec.find(index);
  NTH_REQUIRE((v.harden), iter != context.instruction_spec.end());
  auto rotation_spec    = iter->second;
  rotation_spec.returns = 0;
  context.current_function().append<Rotate>(rotation_spec);
  context.current_function().append<jasmin::Call>(iter->second);
}

void HandleParseTreeNodePointer(ParseNodeIndex index, EmitContext& context) {
  context.current_function().append<ConstructPointerType>();
}

void HandleParseTreeNodeBufferPointer(ParseNodeIndex index,
                                      EmitContext& context) {
  context.current_function().append<ConstructBufferPointerType>();
}

void HandleParseTreeNodeIndexArgumentStart(ParseNodeIndex index,
                                           EmitContext& context) {}

void HandleParseTreeNodePrefixInvocationArgumentEnd(ParseNodeIndex index,
                                                    EmitContext& context) {}

void HandleParseTreeNodeNamedArgument(ParseNodeIndex index,
                                      EmitContext& context) {}

void HandleParseTreeNodeNamedArgumentStart(ParseNodeIndex index,
                                           EmitContext& context) {}

void HandleParseTreeNodeInvocationArgumentStart(ParseNodeIndex index,
                                                EmitContext& context) {}

void HandleParseTreeNodeSlice(ParseNodeIndex index, EmitContext& context) {
  context.current_function().append<ConstructSliceType>();
}

void HandleParseTreeNodeImport(ParseNodeIndex index, EmitContext& context) {
  context.current_function().append<jasmin::Push<ModuleId>>(
      context.constants.at(index).value_span()[0].as<ModuleId>());
}

void HandleParseTreeNodeFunctionTypeParameters(ParseNodeIndex index,
                                               EmitContext& context) {
  context.current_function().append<ConstructParametersType>(
      jasmin::InstructionSpecification{
          .parameters = static_cast<uint32_t>(context.Node(index).child_count),
          .returns    = 1});
}

void HandleParseTreeNodeEmptyParenthesis(ParseNodeIndex index,
                                         EmitContext& context) {
  jasmin::Value v = type::Bottom;
  context.Push(std::span(&v, 1), type::Type_);
}

void HandleParseTreeNodeEnumLiteralStart(ParseNodeIndex, EmitContext&) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeEnumLiteral(ParseNodeIndex, EmitContext&) {
  NTH_UNIMPLEMENTED();
}

Iteration HandleParseTreeNodeExtensionStart(ParseNodeIndex index,
                                            EmitContext& context) {
  // TODO: Iteration is wrong in the presence of nesting, and slow when we could
  // have precomputed this.
  while (context.Node(index).kind != ParseNode::Kind::Extension) { ++index; }
  return Iteration::SkipTo(index);
}

void HandleParseTreeNodeExtendWith(ParseNodeIndex, EmitContext&) {}

void HandleParseTreeNodeExtension(ParseNodeIndex, EmitContext&) {}

void HandleParseTreeNodeBinding(ParseNodeIndex, EmitContext&) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodePatternStart(ParseNodeIndex index,
                                     EmitContext& context) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodePattern(ParseNodeIndex, EmitContext&) {
  NTH_UNIMPLEMENTED();
}

void HandleParseTreeNodeInterfaceLiteralStart(ParseNodeIndex, EmitContext&) {}

void HandleParseTreeNodeInterfaceLiteral(ParseNodeIndex index,
                                         EmitContext& context) {
  context.current_function().append<ConstructInterface>(
      jasmin::InstructionSpecification{
          .parameters =
              static_cast<uint32_t>(context.Node(index).child_count) - 2,
          .returns = 1});
}

void HandleParseTreeNodeWhileLoopStart(ParseNodeIndex index,
                                       EmitContext& context) {
  context.queue.front().branches.push_back(
      context.current_function().append<NoOp>());
}

void HandleParseTreeNodeWhileLoopBodyStart(ParseNodeIndex index,
                                           EmitContext& context) {
  context.current_function().append<jasmin::Not>();
  context.queue.front().branches.push_back(
      context.current_function().append_with_placeholders<jasmin::JumpIf>());
  context.push_lexical_scope(context.Node(index).scope_index);
}

void HandleParseTreeNodeWhileLoop(ParseNodeIndex index, EmitContext& context) {
  // TODO: You don't really need all these no-ops.
  context.pop_lexical_scope();

  nth::interval<jasmin::InstructionIndex> jump_to_land =
      context.queue.front().branches.back();
  context.queue.front().branches.pop_back();
  nth::interval<jasmin::InstructionIndex> restart =
      context.queue.front().branches.back();
  context.queue.front().branches.pop_back();

  auto land =
      context.current_function().append_with_placeholders<jasmin::Jump>();

  context.current_function().set_value(
      land, 0, restart.lower_bound() - land.lower_bound());

  land = context.current_function().append<NoOp>();
  context.current_function().set_value(
      jump_to_land, 0, land.lower_bound() - jump_to_land.lower_bound());
}

void HandleParseTreeNodeIfStatementTrueBranchStart(ParseNodeIndex index,
                                                   EmitContext& context) {
  context.push_lexical_scope(context.Node(index).scope_index);
  context.current_function().append<jasmin::Not>();
  context.queue.front().branches.push_back(
      context.current_function().append_with_placeholders<jasmin::JumpIf>());
}

void HandleParseTreeNodeIfStatementFalseBranchStart(ParseNodeIndex index,
                                                    EmitContext& context) {
  nth::interval<jasmin::InstructionIndex> jump =
      context.queue.front().branches.back();
  context.queue.front().branches.back() =
      context.current_function().append_with_placeholders<jasmin::Jump>();
  nth::interval<jasmin::InstructionIndex> land =
      context.current_function().append<NoOp>();
  context.current_function().set_value(jump, 0,
                                       land.lower_bound() - jump.lower_bound());
}

void HandleParseTreeNodeIfStatement(ParseNodeIndex index,
                                    EmitContext& context) {
  nth::interval<jasmin::InstructionIndex> jump =
      context.queue.front().branches.back();
  context.queue.front().branches.pop_back();
  nth::interval<jasmin::InstructionIndex> land =
      context.current_function().append<NoOp>();
  context.current_function().set_value(jump, 0,
                                       land.lower_bound() - jump.lower_bound());
  context.pop_lexical_scope();
}

void HandleParseTreeNodeStatementStart(ParseNodeIndex index,
                                       EmitContext& context) {
  switch (context.Node(index).statement_kind) {
    case ParseNode::StatementKind::Assignment:
      context.queue.front().value_category_stack.push_back(
          EmitContext::ValueCategory::Reference);
      break;
    default:
      context.queue.front().value_category_stack.push_back(
          EmitContext::ValueCategory::Value);
      break;
  }
}

void HandleParseTreeNodeAssignedValueStart(ParseNodeIndex index,
                                           EmitContext& context) {
  context.queue.front().value_category_stack.back() =
      EmitContext::ValueCategory::Value;
}

void HandleParseTreeNodeAssignment(ParseNodeIndex index, EmitContext& context) {
  context.current_function().append<Rotate>(
      jasmin::InstructionSpecification{.parameters = 2, .returns = 0});
  context.current_function().append<Store>(1);
}

void HandleParseTreeNodeReturn(ParseNodeIndex index, EmitContext& context) {
  context.current_function().append<jasmin::Return>();
}

void HandleParseTreeNodeFunctionLiteral(ParseNodeIndex index,
                                        EmitContext& context) {
  jasmin::Value f = &context.current_function();
  context.current_function().append<jasmin::Return>();
  context.pop_function();
  context.Push(std::span(&f, 1), {context.QualifiedTypeOf(index).type()});
}

void HandleParseTreeNodeNoReturns(ParseNodeIndex index, EmitContext& context) {}

template <auto F>
constexpr Iteration Invoke(ParseNodeIndex index, EmitContext& context) {
  constexpr auto return_type = nth::type<
      std::invoke_result_t<decltype(F), ParseNodeIndex, EmitContext&>>;
  if constexpr (return_type == nth::type<Iteration>) {
    return F(index, context);
  } else {
    F(index, context);
    return Iteration::Continue;
  }
}

}  // namespace

void EmitContext::Push(std::span<jasmin::Value const> vs, type::Type t) {
  if (t == type::Scope) {
    auto& c = current_function();
    NTH_REQUIRE((v.harden), vs.size() == 1);
    vs[0].as<Scope const*>()->AppendTo(c);
    return;
  }
  switch (t.kind()) {
    case type::Type::Kind::DependentFunction:
    case type::Type::Kind::Function: {
      NTH_REQUIRE((v.harden), vs.size() == 1);
      NTH_REQUIRE(vs[0].as<jasmin::Function<> const*>() != nullptr);
      current_function().append<jasmin::Push<jasmin::Function<> const*>>(
          vs[0].as<jasmin::Function<> const*>());
    } break;
    case type::Type::Kind::Pointer: {
      NTH_REQUIRE((v.harden), vs.size() == 1);
      current_function().append<jasmin::Push<VoidConstPtr>>(vs[0].as<void*>());
    } break;
    case type::Type::Kind::Slice: {
      NTH_REQUIRE((v.harden), vs.size() == 2);
      if (t == type::Slice(type::Char)) {
        current_function().append<PushStringLiteral>(StringLiteral(
            std::string_view(vs[0].as<char const*>(), vs[1].as<size_t>())));
      } else {
        NTH_UNIMPLEMENTED();
      }
    } break;
    case type::Type::Kind::Primitive: {
      NTH_REQUIRE((v.harden), vs.size() == 1);
      switch (t.as<type::PrimitiveType>().primitive_kind()) {
#define IC_XMACRO_PRIMITIVE_CPP_TYPE_MAPPING(t, cpp)                           \
  case type::PrimitiveType::Kind::t:                                           \
    current_function().append<jasmin::Push<cpp>>(vs[0].as<cpp>());             \
    break;
#include "common/language/primitive_types.xmacro.h"
        case type::PrimitiveType::Kind::NullType:
          current_function().append<PushNull>();
          break;
        default: NTH_LOG("{}") <<= {t}; NTH_UNIMPLEMENTED();
      }
    } break;
    default: {
      NTH_REQUIRE((v.harden), vs.size() == 1);
      NTH_LOG("{}") <<= {t};
      NTH_UNIMPLEMENTED();
    } break;
  }
}

void EmitContext::Push(std::span<jasmin::Value const> vs,
                       std::span<type::Type const> ts) {
  for (type::Type t : ts) {
    size_t width = type::JasminSize(t);
    NTH_REQUIRE((v.debug), width <= vs.size());
    Push(vs.subspan(0, width), t);
    vs.subspan(width);
  }
}

void EmitContext::Push(EmitContext::ComputedConstants const& c) {
  Push(c.value_span(), c.types());
}

void EmitIr(EmitContext& context) {
  while (not context.queue.empty()) {
    auto [start, end] = context.queue.front().range;
    NTH_LOG((v.when(debug::emit)), "Starting emission of [{}, {}) @ {}") <<=
        {start, end, &context.current_function()};
  emit_constant:
    for (auto const& [original_range, constant] :
         context.constants.mapped_intervals()) {
      // It is important to make a copy of this range because we may end up
      // modifying `constants` and invalidating the reference.
      auto range = original_range;
      if (range.lower_bound() < start) {
        continue;
      } else if (range.lower_bound() == start) {
        if (context.Node(range.upper_bound() - 1).kind !=
            ParseNode::Kind::Declaration) {
          context.Push(constant);
        }
        start = range.upper_bound();
        continue;
      } else if (range.lower_bound() < end) {
        // TODO: Can you avoid the duplication of the macros here? You need this
        // in place so that when you copy the front declaration_stack you get
        // updates from this chunk.
        for (; start < range.lower_bound(); ++start) {
          switch (context.Node(start).kind) {
#define IC_XMACRO_PARSE_NODE(node_kind)                                        \
  case ParseNode::Kind::node_kind: {                                           \
    NTH_LOG((v.when(debug::emit)), "Emit node {} {}") <<= {#node_kind, start}; \
    switch (Iteration it =                                                     \
                Invoke<HandleParseTreeNode##node_kind>(start, context);        \
            it.kind()) {                                                       \
      case Iteration::PauseMoveOn: ++start; [[fallthrough]];                   \
      case Iteration::PauseRetry: goto stop_early;                             \
      case Iteration::Continue: break;                                         \
      case Iteration::Skip: start = it.index() - 1; break;                     \
    }                                                                          \
  } break;
#include "parse/node.xmacro.h"
          }
        }
        goto emit_constant;
      } else {
        break;
      }
    }

    for (; start < end; ++start) {
      switch (context.Node(start).kind) {
#define IC_XMACRO_PARSE_NODE(node_kind)                                        \
  case ParseNode::Kind::node_kind: {                                           \
    NTH_LOG((v.when(debug::emit)), "Emit node {} {}") <<= {#node_kind, start}; \
    switch (Iteration it =                                                     \
                Invoke<HandleParseTreeNode##node_kind>(start, context);        \
            it.kind()) {                                                       \
      case Iteration::PauseMoveOn: ++start; [[fallthrough]];                   \
      case Iteration::PauseRetry: goto stop_early;                             \
      case Iteration::Continue: break;                                         \
      case Iteration::Skip: start = it.index() - 1; break;                     \
    }                                                                          \
  } break;
#include "parse/node.xmacro.h"
      }
    }

    NTH_LOG((v.when(debug::emit)), "Done emitting chunk at {}") <<= {start};
    context.queue.pop();
    continue;

  stop_early:
    NTH_LOG((v.when(debug::emit)), "Stopping early just before {}") <<= {start};
    auto* f    = &context.current_function();
    auto& back = context.queue.emplace(std::move(context.queue.front()));
    back.range = nth::interval(start, back.range.upper_bound());
    context.queue.pop();
    continue;
  }
  NTH_LOG((v.when(debug::emit)), "Done emitting!");
  return;
}

void EmitContext::Evaluate(nth::interval<ParseNodeIndex> subtree,
                           nth::stack<jasmin::Value>& value_stack,
                           std::vector<type::Type> types) {
  nth::stack<jasmin::Value> vs;
  size_t size = 0;
  for (type::Type t : types) { size += type::JasminSize(t); }
  IrFunction f(0, size);
  queue.push({.range = subtree});
  push_function(f, LexicalScope::Index::Root());

  EmitIr(*this);
  f.append<jasmin::Return>();

  f.invoke(vs);
  for (jasmin::Value v : vs.top_span(vs.size())) { value_stack.push(v); }
  constants.insert_or_assign(
      subtree, ComputedConstants(subtree.upper_bound() - 1, std::move(vs),
                                 std::move(types)));
}

void SetExported(EmitContext const& context) {
  for (auto index : context.declarations_to_export) {
    auto const& constant = context.constants.at(index);
    auto iter            = context.tree.child_indices(index).begin();
    ++iter;
    std::span types      = constant.types();
    std::span value_span = constant.value_span();
    NTH_REQUIRE((v.harden), types.size() == 1);
    context.current_module.Insert(context.Node(*iter - 1).token.Identifier(),
                                  AnyValue(types[0], value_span));
  }
}

}  // namespace ic
