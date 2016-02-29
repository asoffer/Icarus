bldr.SetInsertPoint(curr_block);
auto lhs_val = exprs[0] -> generate_code(scope);
for (size_t i = 1; i < exprs.size(); ++i) {
  bldr.SetInsertPoint(curr_block);
  auto rhs_val = exprs[i]->generate_code(scope);

  llvm::Value *cmp_val = nullptr;
  switch (ops[i - 1]) {
