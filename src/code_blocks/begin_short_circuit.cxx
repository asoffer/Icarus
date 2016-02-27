auto landing    = make_block("land", parent_fn);
auto curr_block = bldr.GetInsertBlock();

// Setup landing phi node
bldr.SetInsertPoint(landing);
llvm::PHINode *phi =
    bldr.CreatePHI(*Bool, static_cast<unsigned int>(exprs.size()), "phi");

for (size_t i = 1; i < exprs.size(); ++i) {
  bldr.SetInsertPoint(curr_block);
  auto rhs_val = exprs[i]->generate_code(scope);

  llvm::Value *cmp_val = nullptr;
  switch (ops[i - 1]) {
