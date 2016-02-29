  default: assert(false && "Invalid operator");
  }
  assert(cmp_val && "cmp_val is nullptr");

  auto next_block = make_block("next", parent_fn);
  bldr.CreateCondBr(cmp_val, next_block, landing);
  phi->addIncoming(data::const_false(), curr_block);
  curr_block = next_block;
  lhs_val    = rhs_val;
}

bldr.SetInsertPoint(curr_block);
phi->addIncoming(data::const_true(), curr_block);
bldr.CreateBr(landing);
bldr.SetInsertPoint(landing);
return phi;
