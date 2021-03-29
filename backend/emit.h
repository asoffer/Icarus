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
  explicit EmitContext(
      absl::flat_hash_map<ir::CompiledFn const *,
                          typename Traits::function_type *> &fs)
      : functions(fs) {}

  absl::flat_hash_map<ir::BasicBlock const *,
                      typename Traits::basic_block_type *>
      blocks;
  absl::flat_hash_map<ir::Reg, typename Traits::value_type *> registers;
  absl::flat_hash_map<ir::CompiledFn const *, typename Traits::function_type *>
      &functions;
};

template <typename Derived, typename Traits>
struct Emitter {
  using basic_block_type = typename Traits::basic_block_type;
  using function_type    = typename Traits::function_type;
  using module_type      = typename Traits::module_type;
  using value_type       = typename Traits::value_type;

  void EmitModule(compiler::CompiledModule const &module) {
    auto &emitter = *static_cast<Derived *>(this);

    module.context().ForEachCompiledFn(
        [&](ir::CompiledFn const *fn, module::Linkage linkage) {
          LOG("Emitter", "Declaring function %p", fn);
          contexts_.try_emplace(fn, functions_);
          functions_.emplace(fn, emitter.DeclareFunction(fn, linkage, module_));
        });

    module.context().ForEachCompiledFn(
        [&](ir::CompiledFn const *fn, module::Linkage linkage) {
          emitter.EmitFunction(fn, linkage);
        });
  }

  function_type *EmitFunction(ir::CompiledFn const *fn,
                              module::Linkage linkage) {
    LOG("Emitter", "Emitting function %p", fn);
    auto &emitter = *static_cast<Derived *>(this);

    contexts_.try_emplace(fn, functions_);
    auto [iter, inserted] = functions_.try_emplace(fn);
    function_type *f;
    if (inserted) {
      f = iter->second = emitter.DeclareFunction(fn, linkage, module_);
    }
    DeclareBasicBlocks(fn);

    emitter.AllocateLocalVariables(*fn);
    emitter.EmitBasicBlocks(*fn);

    bool returns_void = fn->type()->output().empty();

    for (auto const *block : fn->blocks()) {
      emitter.EmitBasicBlockJump(block, contexts_.at(fn), returns_void);
    }
    return f;
  }

  void AllocateLocalVariables(ir::CompiledFn const &f) {
    auto &emitter = *static_cast<Derived *>(this);
    auto &ctx     = contexts_.at(&f);
    emitter.PrepareForStackAllocation(f, ctx.blocks);
    ctx.registers.reserve(f.num_allocs());
    f.for_each_alloc([&](type::Type t, ir::Reg r) {
      ctx.registers.emplace(r, emitter.StackAllocate(t));
    });
  }

 protected:
  Emitter(module_type *module) : module_(*ASSERT_NOT_NULL(module)) {}
  void DeclareBasicBlocks(ir::CompiledFn const *f) {
    auto &output_fn = *functions_.at(f);
    auto &ctx       = contexts_.at(f);
    ctx.blocks.reserve(f->blocks().size());
    for (auto const *block : f->blocks()) {
      ctx.blocks.emplace(
          block, static_cast<Derived *>(this)->DeclareBasicBlock(output_fn));
    }
  }

 private:
  void EmitBasicBlocks(ir::CompiledFn const &fn) {
    std::vector<std::tuple<ir::BasicBlock const *, basic_block_type *,
                           absl::Span<ir::Inst const>>>
        to_process;
    auto &ctx = contexts_.at(&fn);
    for (auto const *block : fn.blocks()) {
      to_process.emplace_back(block, ctx.blocks.at(block),
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
                  instructions.front(), ctx)) {
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

  module_type &module_;

  absl::flat_hash_map<ir::CompiledFn const *, typename Traits::function_type *>
      functions_;
  absl::flat_hash_map<ir::CompiledFn const *, EmitContext<Traits>> contexts_;
};

}  // namespace backend

#endif  // ICARUS_BACKEND_EMIT_H
