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
  case Op::Phi: assert(false && "Implemented elsewhere");
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
  switch(exit.flag) {
  case Exit::Strategy::Uncond: return exit.true_block;
  case Exit::Strategy::Return: return nullptr;
  case Exit::Strategy::Cond: {
    Value v = exit.val;
    if (exit.val.flag == ValType::Ref) {
      v = vals[v.val.as_ref];
    } else if (exit.val.flag == ValType::Arg) {
      v = fn_args[v.val.as_arg];
    }
    return v.val.as_bool ? exit.true_block : exit.false_block;
  }
  }
}

Value Call(Func *f, const std::vector<Value>& arg_vals) {
  std::vector<Value> vals(f->num_cmds);
  Block *prev_block_ptr  = nullptr;
  Block *block_ptr       = f->entry();
  size_t cmd_index = 0;

eval_loop_start:
  // std::cout << "block-" << block_ptr->block_num << ": " << cmd_index
  //           << std::endl;
  if (cmd_index == block_ptr->cmds.size()) {
    prev_block_ptr = block_ptr;
    block_ptr      = block_ptr->execute_jump(vals, arg_vals);

    if (!block_ptr) { // It's a return
      if (prev_block_ptr->exit.val.flag == ValType::Ref) {
        return vals[prev_block_ptr->exit.val.val.as_ref];

      } else if (prev_block_ptr->exit.val.flag == ValType::Arg) {
        return vals[prev_block_ptr->exit.val.val.as_arg];

      } else {
        return prev_block_ptr->exit.val;
      }

    } else {
      cmd_index = 0;
      // std::cout << "  jumped to block-" << block_ptr->block_num << std::endl;
      // std::cin.ignore(1);
    }

  } else {
    auto cmd = block_ptr->cmds[cmd_index];
    assert(cmd.result.flag == ValType::Ref);
    if (cmd.op_code == Op::Phi) {
      Value chosen_val;
      for (size_t i = 0; i < cmd.incoming_blocks.size(); ++i) {
        if (prev_block_ptr == cmd.incoming_blocks[i]) {
          chosen_val = cmd.args[i];
          goto found_match;
        }
      }
      assert(false && "No selection made from phi block");
    found_match:
      vals[cmd.result.val.as_ref] = chosen_val;

    } else {
      vals[cmd.result.val.as_ref] = cmd.eval(vals, arg_vals);
    }

    // std::cout << "  " << cmd.result << " = " << vals[cmd.result.val.as_ref]
    //           << std::endl;
    // std::cin.ignore(1);
    ++cmd_index;
  }
  goto eval_loop_start;
}

} // namespace IR

#undef NOT_YET
