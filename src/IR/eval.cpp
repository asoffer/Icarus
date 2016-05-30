#define NOT_YET assert(false && "Not yet implemented")

namespace IR {
Value Cmd::eval(const std::vector<Value> &vals, const std::vector<Value> &fn_args) {
  std::vector<Value> cmd_inputs = args;
  for (auto &i : cmd_inputs) {
    if (i.flag == ValType::Ref) {
      i = vals[i.val.as_ref];
    } else if (i.flag == ValType::Arg) {
      i = fn_args[i.val.as_arg];
    }
  }

  switch (op_code) {
  case Op::BNot: return Value(!cmd_inputs[0].val.as_bool);
  case Op::INeg: return Value(-cmd_inputs[0].val.as_int);
  case Op::FNeg: return Value(-cmd_inputs[0].val.as_real);
  case Op::Load: NOT_YET;
  case Op::Store: NOT_YET;
  case Op::Phi: NOT_YET;
  case Op::Ret: return cmd_inputs[0];
  case Op::IAdd:
    return Value(cmd_inputs[0].val.as_int + cmd_inputs[1].val.as_int);
  case Op::UAdd:
    return Value(cmd_inputs[0].val.as_uint + cmd_inputs[1].val.as_uint);
  case Op::FAdd:
    return Value(cmd_inputs[0].val.as_real + cmd_inputs[1].val.as_real);
  case Op::ISub:
    return Value(cmd_inputs[0].val.as_int - cmd_inputs[1].val.as_int);
  case Op::USub:
    return Value(cmd_inputs[0].val.as_uint - cmd_inputs[1].val.as_uint);
  case Op::FSub:
    return Value(cmd_inputs[0].val.as_real - cmd_inputs[1].val.as_real);
  case Op::IMul:
    return Value(cmd_inputs[0].val.as_int * cmd_inputs[1].val.as_int);
  case Op::UMul:
    return Value(cmd_inputs[0].val.as_uint * cmd_inputs[1].val.as_uint);
  case Op::FMul:
    return Value(cmd_inputs[0].val.as_real * cmd_inputs[1].val.as_real);
  case Op::IDiv:
    return Value(cmd_inputs[0].val.as_int / cmd_inputs[1].val.as_int);
  case Op::UDiv:
    return Value(cmd_inputs[0].val.as_uint / cmd_inputs[1].val.as_uint);
  case Op::FDiv:
    return Value(cmd_inputs[0].val.as_real / cmd_inputs[1].val.as_real);
  case Op::IMod:
    return Value(cmd_inputs[0].val.as_int % cmd_inputs[1].val.as_int);
  case Op::UMod:
    return Value(cmd_inputs[0].val.as_uint % cmd_inputs[1].val.as_uint);
  case Op::FMod:
    return Value(fmod(cmd_inputs[0].val.as_real, cmd_inputs[1].val.as_real));
  case Op::BXor:
    return Value(cmd_inputs[0].val.as_bool != cmd_inputs[1].val.as_bool);
  }
}

Block *Block::execute_jump(const std::vector<Value> &vals,
                           const std::vector<Value> &fn_args) {
  if (cond.flag == ValType::Ref) {
    return vals[cond.val.as_ref].val.as_bool ? true_block : false_block;

  } else if (cond.flag == ValType::Arg) {
    return fn_args[cond.val.as_ref].val.as_bool ? true_block : false_block;

  } else if (cond.flag == ValType::B) {
    return cond.val.as_bool ? true_block : false_block;

  } else {
    return true_block;
  }
}

Value Call(Func *f, const std::vector<Value>& arg_vals) {
  std::vector<Value> vals(f->num_cmds);
  Block *block_ptr = f->entry();
  size_t cmd_index = 0;

  while (true) {
    vals[cmd_index] = block_ptr->cmds[cmd_index].eval(vals, arg_vals);
    if (block_ptr->cmds[cmd_index].op_code == Op::Ret) {
      return vals[cmd_index];
    }

    ++cmd_index;

    if (cmd_index == block_ptr->cmds.size()) {
      block_ptr = block_ptr->execute_jump(vals, arg_vals);
    }
  }

  auto val = block_ptr->cmds[cmd_index];
  return Value();
}

} // namespace IR

#undef NOT_YET
