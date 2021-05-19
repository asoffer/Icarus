#include <optional>
#include <utility>

#include "absl/cleanup/cleanup.h"
#include "absl/strings/str_format.h"
#include "ast/ast.h"
#include "base/extend.h"
#include "base/extend/absl_hash.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/compiled_scope.h"
#include "ir/instruction/core.h"
#include "ir/instruction/inliner.h"
#include "ir/value/char.h"
#include "ir/value/reg.h"
#include "ir/value/scope.h"
#include "ir/value/value.h"

namespace compiler {
namespace {

// If the type `t` is not big, creates a new register referencing the value (or
// register) held in `value`. If `t` is big, `value` is either another register
// or the address of the big value and a new register referencing that address
// (or register) is created.
ir::Reg RegisterReferencing(ir::Builder& builder, type::Type t,
                            ir::Value value) {
  if (t.is_big()) {
    return builder.CurrentBlock()->Append(ir::RegisterInstruction<ir::addr_t>{
        .operand = value.get<ir::RegOr<ir::addr_t>>(),
        .result  = builder.CurrentGroup()->Reserve(),
    });
  } else {
    return ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t,
                      uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                      type::Type, ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn,
                      ir::Jump, ir::Block, ir::GenericFn, interface::Interface>(
        t, [&]<typename T>() {
          return builder.CurrentBlock()->Append(ir::RegisterInstruction<T>{
              .operand = value.get<ir::RegOr<T>>(),
              .result  = builder.CurrentGroup()->Reserve(),
          });
        });
  }
}

// Many different gotos may end up at the same block node, some from the same
// jump, some from different jumps. They may end up calling different overloads
// of the before/entry function. BeforeBlock describes one such possible entry
// path.
struct BeforeBlock : base::Extend<BeforeBlock>::With<base::AbslHashExtension> {
  ast::BlockNode const *block;
  ir::Fn fn;
};

absl::flat_hash_map<
    std::string,
    std::vector<std::pair<core::Arguments<std::pair<ir::Value, type::QualType>>,
                          ir::BasicBlock *>>>
InlineJumpIntoCurrent(ir::Builder &bldr, ir::Jump to_be_inlined,
                      absl::Span<ir::Value const> arguments,
                      ir::LocalBlockInterpretation const &block_interp) {
  auto const *jump           = ir::CompiledJump::From(to_be_inlined);
  auto *start_block          = bldr.CurrentBlock();
  size_t inlined_start_index = bldr.CurrentGroup()->blocks().size();

  auto *into = bldr.CurrentGroup();
  ir::InstructionInliner inl(jump, into, block_interp);

  bldr.CurrentBlock() = start_block;
  size_t i            = 0;
  if (type::Type state_type = jump->type()->state()) {
    RegisterReferencing(bldr, state_type, arguments[i++]);
  }

  for (auto const &p : jump->type()->params()) {
    RegisterReferencing(bldr, p.value, arguments[i++]);
  }

  auto *entry = inl.InlineAllBlocks();

  bldr.CurrentBlock() = start_block;
  bldr.UncondJump(entry);
  return std::move(inl).ArgumentsByName();
}

struct BlockNodeResult {
  ir::BasicBlock *body;
  ir::Builder::BlockTerminationState termination;
};

BlockNodeResult EmitIrForBlockNode(Compiler &c, ast::BlockNode const *node) {
  auto &bldr = c.builder();
  auto *body =
      bldr.AddBlock(absl::StrFormat("body block for `%s`.", node->name()));

  bldr.CurrentBlock() = body;

  for (auto const &decl : node->params()) {
    auto const *param = decl.value.get();
    auto addr = c.builder().Alloca(c.context().qual_types(param)[0].type());
    // TODO: Support multiple declarations?
    c.builder().set_addr(&param->ids()[0], addr);
  }

  bldr.block_termination_state() =
      ir::Builder::BlockTerminationState::kMoreStatements;
  c.EmitValue(node);

  return BlockNodeResult{.body        = body,
                         .termination = bldr.block_termination_state()};
}

std::optional<ir::Reg> InitializeScopeState(Compiler &c,
                                            ir::CompiledScope const &scope) {
  if (type::Type state_type = scope.state_type()) {
    ir::Reg state_ptr = c.builder().Alloca(state_type);
    c.EmitDefaultInit(type::Typed<ir::Reg>(state_ptr, state_type));
    return state_ptr;
  }
  return std::nullopt;
}

std::pair<ir::Jump, std::vector<ir::Value>> EmitIrForJumpArguments(
    Compiler &c,
    core::Arguments<type::Typed<ir::Value>> const &constant_arguments,
    std::optional<ir::Reg> state_ptr,
    core::Arguments<ast::Expression const *> const &arg_exprs,
    ir::CompiledScope const &scope) {
  // TODO: Support dynamic dispatch.
  auto const &inits = scope.inits();
  ASSERT(inits.size() == 1u);
  auto &init = *inits.begin();

  // Arguments provided to a function call need to be "prepared" in the sense
  // that they need to be
  // * Ordered according to the parameters of the function (because named
  //   arguments may be out of order)
  // * Have any implicit conversions applied.
  //
  // Implicit conversions are tricky because we cannot first compute the values
  // and then apply conversions to them. This may work for conversions that take
  // a buffer-pointer and convert it to just a pointer, but some conversions
  // take values and convert them to pointers/references. If we first compute
  // the value, we may end up loading the value from memory and no longer having
  // access to its address. Or worse, we may have a temporary and never have an
  // allocated address for it.
  std::vector<ir::Value> prepared_arguments;

  auto const &param_qts = ir::CompiledJump::From(init)->params().Transform(
      [](auto const &p) { return type::QualType::NonConstant(p.type()); });

  if (state_ptr) { prepared_arguments.emplace_back(*state_ptr); }

  // TODO: With expansions, args_exprs.pos().size() could be wrong.
  for (size_t i = 0; i < arg_exprs.pos().size(); ++i) {
    prepared_arguments.push_back(PrepareArgument(
        c, *constant_arguments[i], arg_exprs[i], param_qts[i].value));
  }

  for (size_t i = arg_exprs.pos().size(); i < param_qts.size(); ++i) {
    std::string_view name                = param_qts[i].name;
    auto const *constant_typed_value     = constant_arguments.at_or_null(name);
    auto const *expr                     = arg_exprs.at_or_null(name);
    ast::Expression const *default_value = nullptr;

    if (not expr) {
      default_value = ASSERT_NOT_NULL(
          ir::CompiledJump::From(init)->params()[i].value.get()->init_val());
    }

    prepared_arguments.push_back(PrepareArgument(
        c, constant_typed_value ? **constant_typed_value : ir::Value(),
        expr ? *expr : default_value, param_qts[i].value));
  }
  return std::make_pair(init, std::move(prepared_arguments));
}

struct BlockAndPhis {
  ir::BasicBlock *block;
  std::vector<ir::Value> phis;
};
void SetBeforeBlockPhi(
    Compiler &c, absl::flat_hash_map<BeforeBlock, BlockAndPhis> &before_blocks,
    ast::BlockNode const *block_node, ir::Fn before_fn,
    ir::BasicBlock *incoming_block,
    core::Arguments<std::pair<ir::Value, type::QualType>> const &args) {
  LOG("SetBeforeBlockPhi", "SetBeforeBlockPhi: %s", block_node->name());

  auto *phi_block = std::exchange(c.builder().CurrentBlock(), incoming_block);

  decltype(before_blocks.begin()) iter;
  bool inserted;
  std::tie(iter, inserted) = before_blocks.try_emplace(
      BeforeBlock{.block = block_node, .fn = before_fn});
  auto const &before_block = iter->first;
  std::vector<ir::Value> prepared_arguments;

  core::Params<type::QualType> const &param_qts =
      before_block.fn.type()->params();

  for (size_t i = 0; i < args.pos().size(); ++i) {
    prepared_arguments.push_back(
        PrepareArgument(c, args[i].first, args[i].second, param_qts[i].value));
  }

  for (size_t i = args.pos().size(); i < param_qts.size(); ++i) {
    // TODO: Default values.
    auto const &[arg_val, arg_qt] = args[param_qts[i].name];
    prepared_arguments.push_back(
        PrepareArgument(c, arg_val, arg_qt, param_qts[i].value));
  }

  auto &phis = iter->second.phis;
  if (inserted) {
    iter->second.block = c.builder().AddBlock(
        absl::StrFormat("Before block for `%s`", block_node->name()));
  }

  c.builder().CurrentBlock() = phi_block;

  size_t i = 0;
  for (auto const &param : param_qts) {
    auto t = before_block.fn.type()->params()[i].value.type();
    if (t.is_big()) {
      ir::PhiInstruction<ir::addr_t> *phi =
          inserted ? c.builder().PhiInst<ir::addr_t>()
                   : &c.builder()
                          .CurrentBlock()
                          ->instructions()[i]
                          .template as<ir::PhiInstruction<ir::addr_t>>();
      if (inserted) { phis.push_back(ir::Value(phi->result)); }
      phi->add(incoming_block,
               prepared_arguments[i].get<ir::RegOr<ir::addr_t>>());
    } else {
      ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t, uint8_t,
                 uint16_t, uint32_t, uint64_t, float, double, type::Type,
                 ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn, ir::Jump, ir::Block,
                 ir::GenericFn, interface::Interface>(t, [&]<typename T>() {
        ir::PhiInstruction<T> *phi =
            inserted ? c.builder().PhiInst<T>()
                     : &c.builder()
                            .CurrentBlock()
                            ->instructions()[i]
                            .template as<ir::PhiInstruction<T>>();
        if (inserted) { phis.push_back(ir::Value(phi->result)); }
        phi->add(incoming_block, prepared_arguments[i].get<ir::RegOr<T>>());
      });
    }
    ++i;
  }

  c.builder().UncondJump(iter->second.block);
};

}  // namespace

ir::Value Compiler::EmitValue(ast::ScopeNode const *node) {
  LOG("ScopeNode", "Emitting IR for ScopeNode");
  ir::Scope scope            = *EvaluateOrDiagnoseAs<ir::Scope>(node->name());
  auto const *compiled_scope = ir::CompiledScope::From(scope);

  // If the scope is stateful, stack-allocate space for the state and
  // default-initialize it.
  //
  // TODO: This interface isn't great beacuse it requires default-initializable
  // state and no way to call any other initializer.
  std::optional<ir::Reg> state_ptr =
      InitializeScopeState(*this, *compiled_scope);

  // Arguments to the scope's start must be re-evaluated on each call to `goto
  // start()`, so we need a block to which we can jump for this purpose.
  auto *args_block =
      builder().AddBlock(absl::StrFormat("args block for scope %p", node));

  // If a scope has a path that exits normally along some path (a call to exit
  // rather than a return or labelled yield), then this is the block on which we land.
  auto *landing_block =
      builder().AddBlock(absl::StrFormat("landing block for scope %p", node));

  // Insert empty phi-instructions for each return type. Any labeled yields will
  // inject their result values here.
  // TODO: Support more than one return type.
  // TODO: Support all types
  type::QualType const qt = context().qual_types(node)[0];
  std::optional<ir::Reg> result;
  if (qt.type() != type::Void) {
    ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t, uint8_t,
               uint16_t, uint32_t, uint64_t, float, double>(
        qt.type(), [&]<typename T>() {
          ir::PhiInstruction<T> phi;
          phi.result = builder().CurrentGroup()->Reserve();
          result     = phi.result;
          landing_block->Append(std::move(phi));
        });
  }

  // Push the scope landing state onto the the vector. Any nested scopes will be
  // able to lookup the label in this vector to determine where they should jump
  // to.
  state().scope_landings.push_back(TransientState::ScopeLandingState{
      .label       = node->label() ? node->label()->value() : ir::Label(),
      .scope       = scope,
      .result_type = qt,
      .block       = landing_block,
  });
  absl::Cleanup c = [&] { state().scope_landings.pop_back(); };

  // Add new blocks
  absl::flat_hash_map<ast::BlockNode const *, ir::BasicBlock *> interp_map;
  for (auto const &block : node->blocks()) {
    interp_map.emplace(&block, builder().AddBlock(absl::StrCat(
                                   "Start of block `", block.name(), "`.")));
  }

  absl::flat_hash_map<BeforeBlock, BlockAndPhis> before_blocks;

  ir::LocalBlockInterpretation local_interp(std::move(interp_map), args_block,
                                            landing_block);

  builder().UncondJump(args_block);
  builder().CurrentBlock() = args_block;

  // Constant arguments need to be computed entirely before being used to
  // instantiate a generic function.
  core::Arguments<type::Typed<ir::Value>> constant_arguments =
      EmitConstantArguments(*this, node->args());

  auto [init, args] = EmitIrForJumpArguments(
      *this, constant_arguments, state_ptr, node->args(), *compiled_scope);
  auto from_block = builder().CurrentBlock();

  auto args_by_name =
      InlineJumpIntoCurrent(builder(), init, args, local_interp);
  for (auto const &[name, args_and_incoming] : args_by_name) {
    for (auto const &[args, incoming_block] : args_and_incoming) {
      // TODO: Handle arguments for start/done blocks.
      if (name == "done" or name == "start") { continue; }
      builder().CurrentBlock()         = local_interp[name];
      ast::BlockNode const *block_node = local_interp.block_node(name);

      auto *scope_block = ir::CompiledBlock::From(compiled_scope->block(name));
      ir::OverloadSet &before = scope_block->before();
      auto arg_types =
          args.Transform([](std::pair<ir::Value, type::QualType> const &v) {
            return v.second;
          });
      ir::Fn before_fn = before.Lookup(arg_types).value();
      SetBeforeBlockPhi(*this, before_blocks, block_node, before_fn,
                        incoming_block, args);
    }
  }

  absl::flat_hash_map<ast::BlockNode const *, ir::BasicBlock *> bodies;
  for (auto const &block_node : node->blocks()) {
    auto const *scope_block =
        ir::CompiledBlock::From(compiled_scope->block(block_node.name()));

    auto [body_block, termination] = EmitIrForBlockNode(*this, &block_node);
    bodies.emplace(&block_node, body_block);

    switch (termination) {
      case ir::Builder::BlockTerminationState::kMoreStatements: UNREACHABLE();
      case ir::Builder::BlockTerminationState::kNoTerminator: {
        auto const &afters = scope_block->after();
        // TODO: Choose the right jump.
        ASSERT(afters.size() == 1u);
        auto &after = *afters.begin();

        std::vector<ir::Value> after_args;
        if (state_ptr) { after_args.emplace(after_args.begin(), *state_ptr); }
        auto args_by_name =
            InlineJumpIntoCurrent(builder(), after, after_args, local_interp);
        for (auto const &[name, args_and_incoming] : args_by_name) {
          for (auto const &[args, incoming_block] : args_and_incoming) {
            // TODO: Handle arguments for start/done blocks.
            if (name == "done" or name == "start") { continue; }
            builder().CurrentBlock()         = local_interp[name];
            ast::BlockNode const *block_node = local_interp.block_node(name);

            auto *scope_block =
                ir::CompiledBlock::From(compiled_scope->block(name));
            ir::OverloadSet &before = scope_block->before();
            auto arg_types          = args.Transform(
                [](std::pair<ir::Value, type::QualType> const &v) {
                  return v.second;
                });
            ir::Fn before_fn = before.Lookup(arg_types).value();
            SetBeforeBlockPhi(*this, before_blocks, block_node, before_fn,
                              incoming_block, args);
          }
        }

      } break;
      case ir::Builder::BlockTerminationState::kGoto: 
      case ir::Builder::BlockTerminationState::kReturn: 
      case ir::Builder::BlockTerminationState::kLabeledYield:
      case ir::Builder::BlockTerminationState::kYield: continue;
    }
  }

  for (auto const &[before_block, block_and_phis] : before_blocks) {
    auto const &[block, phis] = block_and_phis;
    builder().CurrentBlock()  = block;
    LOG("ScopeNode", "%s: before %s", before_block.block->name(),
        before_block.fn.type()->to_string());

    absl::Span<type::Type const> types = before_block.fn.type()->return_types();
    std::vector<ir::Reg> out_regs;
    out_regs.reserve(types.size());
    for (type::Type type : types) {
      absl::Span<ast::Declaration::Id const> ids =
          before_block.block->params()[out_regs.size()].value->ids();
      ASSERT(ids.size() == 1u);
      auto reg = builder().CurrentGroup()->Reserve();
      if (type.is_big()) {
        reg =
            builder().CurrentBlock()->Append(ir::RegisterInstruction<ir::addr_t>{
                .operand = builder().addr(&ids[0]),
                .result  = reg,
            });
      }
      out_regs.push_back(reg);
    }
    ir::OutParams out_params(std::move(out_regs));

    builder().Call(before_block.fn, before_block.fn.type(), phis, out_params);
    ASSERT(out_params.size() == before_block.block->params().size());

    for (size_t i = 0; i < out_params.size(); ++i) {
      absl::Span<ast::Declaration::Id const> ids  = before_block.block->params()[i].value->ids();
      ASSERT(ids.size() == 1u);
      auto t = before_block.fn.type()->params()[i].value.type();
      if (not t.is_big()) {
        ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t, uint8_t,
                   uint16_t, uint32_t, uint64_t, float, double, type::Type,
                   ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn, ir::Jump,
                   ir::Block, ir::GenericFn, interface::Interface>(
            t, [&]<typename T>() {
              builder().Store(ir::RegOr<T>(out_params[i]),
                              builder().addr(&ids[0]));
            });
      }
    }
    builder().UncondJump(bodies.at(before_block.block));
  }

  builder().CurrentBlock() = landing_block;
  builder().block_termination_state() =
      ir::Builder::BlockTerminationState::kMoreStatements;
  return result ? ir::Value(*result) : ir::Value();
}

void Compiler::EmitCopyInit(
    ast::ScopeNode const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  // TODO: Implement this properly.
  EmitCopyAssign(node, to);
}

void Compiler::EmitMoveInit(
    ast::ScopeNode const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  // TODO: Implement this properly.
  EmitMoveAssign(node, to);
}
void Compiler::EmitCopyAssign(
    ast::ScopeNode const *node,
   absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  // TODO: Implement this properly.
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  EmitCopyAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

void Compiler::EmitMoveAssign(
    ast::ScopeNode const *node,
   absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  // TODO: Implement this properly.
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  EmitMoveAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}


}  // namespace compiler
