#include "backend/function.h"

#include "absl/container/flat_hash_map.h"
#include "backend/type.h"

namespace backend {
namespace {

void EmitLlvmBasicBlock(
    llvm::IRBuilder<> &builder, ir::BasicBlock const *block,
    absl::flat_hash_map<ir::BasicBlock const *, llvm::BasicBlock *>
        &llvm_block_map) {
  builder.SetInsertPoint(llvm_block_map.at(block));
  for (auto const *instruction : block->instructions()) {}
  switch (block->jump().kind()) {
    case ir::JumpCmd::Kind::Return:
      // TODO: Not necessarily void.
      builder.CreateRetVoid();
      return;
    case ir::JumpCmd::Kind::Uncond:
      builder.CreateBr(llvm_block_map.at(block->jump().UncondTarget()));
      return;
    case ir::JumpCmd::Kind::Cond:
      // builder.CreateCondBr(___, llvm_block_map.at(CondTarget(true));
        //                    llvm_block_map.at(CondTarget(false)));
      return;
    case ir::JumpCmd::Kind::Choose:
    default: UNREACHABLE("choose-jump should not be possible.");
  }
}

}  // namespace

llvm::Function *DeclareLlvmFunction(ir::CompiledFn const &fn,
                                    compiler::CompiledModule const &module,
                                    llvm::Module &llvm_module) {
  return llvm::Function::Create(llvm::cast<llvm::FunctionType>(ToLlvmType(
                                    fn.type(), llvm_module.getContext())),
                                llvm::Function::PrivateLinkage, "fn",
                                &llvm_module);
}

void EmitLlvmFunction(llvm::IRBuilder<> &builder, llvm::LLVMContext &context,
                      ir::CompiledFn const &fn, llvm::Function &llvm_fn) {
  absl::flat_hash_map<ir::BasicBlock const *, llvm::BasicBlock *>
      llvm_block_map;

  char const *block_name = "entry";
  for (auto const *block : fn.blocks()) {
    llvm_block_map.emplace(
        block,
        llvm::BasicBlock::Create(builder.getContext(), block_name, &llvm_fn));
    char const *block_name = "block";
  }

  for (auto const *block : fn.blocks()) {
    EmitLlvmBasicBlock(builder, block, llvm_block_map);
  }
}

}  // namespace backend
