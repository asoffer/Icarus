#ifndef ICARUS_BACKEND_EMIT_H
#define ICARUS_BACKEND_EMIT_H

#include <vector>
#include <tuple>

#include "absl/container/flat_hash_map.h"
#include "absl/types/span.h"
#include "compiler/module.h"
#include "ir/compiled_fn.h"
#include "ir/value/reg.h"
#include "type/type.h"

namespace backend {

template <typename Traits>
struct EmitContext {
  absl::flat_hash_map<ir::BasicBlock const *,
                      typename Traits::basic_block_type *>
      blocks;
  absl::flat_hash_map<ir::Reg, typename Traits::value_type *> registers;
  absl::flat_hash_map<ir::CompiledFn const *,
                      typename Traits::function_type *> const &functions;
};

template <typename Derived, typename Traits>
struct Emitter {
  using basic_block_type = typename Traits::basic_block_type;
  using function_type    = typename Traits::function_type;
  using module_type      = typename Traits::module_type;
  using value_type       = typename Traits::value_type;

  void EmitModule(compiler::CompiledModule const &module) {
    auto &emitter = *static_cast<Derived *>(this);

    module.context().ForEachCompiledFn([&](ir::CompiledFn const *fn) {
      emitter.DeclareFunction(fn, module_);
    });

    module.context().ForEachCompiledFn(
        [&](ir::CompiledFn const *fn) { emitter.EmitFunction(fn); });
  }

  void EmitFunction(ir::CompiledFn const *fn) {
    auto &emitter = *static_cast<Derived *>(this);
    emitter.DeclareBasicBlocks(fn->blocks(), *context_.functions.at(fn));
    emitter.AllocateLocalVariables(*fn);
    emitter.EmitBasicBlocks(*fn);

    bool returns_void = fn->type()->output().empty();

    for (auto const *block : fn->blocks()) {
      emitter.EmitBasicBlockJump(block, context_, returns_void);
    }
  }

  void AllocateLocalVariables(ir::CompiledFn const &f) {
    auto &emitter = *static_cast<Derived *>(this);
    emitter.PrepareForStackAllocation(f, context_.blocks);
    context_.registers.reserve(f.num_allocs());
    f.for_each_alloc([&](type::Type t, ir::Reg r) {
      context_.registers.emplace(r, emitter.StackAllocate(t));
    });
  }

  void EmitBasicBlocks(ir::CompiledFn const &fn) {
    std::vector<std::tuple<ir::BasicBlock const *, basic_block_type *,
                           absl::Span<ir::Inst const>>>
        to_process;
    for (auto const *block : fn.blocks()) {
      to_process.emplace_back(block, context_.blocks.at(block),
                              block->instructions());
    }

    auto front_iter = to_process.begin();
    auto back_iter  = to_process.end();
    while (front_iter != back_iter) {
      while (front_iter != back_iter) {
        auto &[in_block, out_block, instructions] = *front_iter;
        static_cast<Derived *>(this)->PrepareForBasicBlockAppend(out_block);

        while (not instructions.empty()) {
          if (static_cast<Derived *>(this)->EmitInstruction(
                  instructions.front(), context_)) {
            instructions.remove_prefix(1);
          } else {
            break;
          }
        }

        if (instructions.empty()) {
          *front_iter = *back_iter;
          --back_iter;
        } else {
          ++front_iter;
        }
      }
      front_iter = to_process.begin();
    }
  }

 protected:
  Emitter(absl::flat_hash_map<ir::CompiledFn const *, function_type *> const
              *fn_map,
          module_type *module)
      : module_(*ASSERT_NOT_NULL(module)),
        context_{.functions = *ASSERT_NOT_NULL(fn_map)} {}
  void DeclareBasicBlocks(base::PtrSpan<ir::BasicBlock const> blocks,
                          function_type &output_fn) {
    context_.blocks.reserve(blocks.size());
    for (auto const *block : blocks) {
      context_.blocks.emplace(
          block, static_cast<Derived *>(this)->DeclareBasicBlock(output_fn));
    }
  }

 private:
  module_type &module_;
  EmitContext<Traits> context_;
};

}  // namespace backend

#endif  // ICARUS_BACKEND_EMIT_H
