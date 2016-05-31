#define NOT_YET assert(false && "Not yet implemented")

namespace IR {
void Cmd::Execute(StackFrame& frame) {
  std::vector<Value> cmd_inputs = args;

  // Load the command inputs from the registers or arguments if needed.
  for (auto &i : cmd_inputs) {
    if (i.flag == ValType::Ref) {
      i = frame.reg[i.val.as_ref];
    } else if (i.flag == ValType::Arg) {
      i = frame.args[i.val.as_arg];
    }
  }

  switch (op_code) {
  case Op::BNot: {
    frame.reg[result.val.as_ref] = Value(!cmd_inputs[0].val.as_bool);
  } break;
  case Op::INeg: {
    frame.reg[result.val.as_ref] = Value(-cmd_inputs[0].val.as_int);
  } break;
  case Op::FNeg: {
    frame.reg[result.val.as_ref] = Value(-cmd_inputs[0].val.as_real);
  } break;
  case Op::Load: NOT_YET;
  case Op::Store: NOT_YET;
  case Op::Phi: {
    for (size_t i = 0; i < incoming_blocks.size(); ++i) {
      if (frame.prev_block == incoming_blocks[i]) {
        frame.reg[result.val.as_ref] = cmd_inputs[i];
        goto exit_successfully;
      }
    }
    assert(false && "No selection made from phi block");
  exit_successfully:;
  } break;
  case Op::IAdd: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_int + cmd_inputs[1].val.as_int);
  } break;
  case Op::UAdd: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_uint + cmd_inputs[1].val.as_uint);
  } break;
  case Op::FAdd: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_real + cmd_inputs[1].val.as_real);
  } break;
  case Op::ISub: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_int - cmd_inputs[1].val.as_int);
  } break;
  case Op::USub: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_uint - cmd_inputs[1].val.as_uint);
  } break;
  case Op::FSub: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_real - cmd_inputs[1].val.as_real);
  } break;
  case Op::IMul: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_int * cmd_inputs[1].val.as_int);
  } break;
  case Op::UMul: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_uint * cmd_inputs[1].val.as_uint);
  } break;
  case Op::FMul: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_real * cmd_inputs[1].val.as_real);
  } break;
  case Op::IDiv: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_int / cmd_inputs[1].val.as_int);
  } break;
  case Op::UDiv: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_uint / cmd_inputs[1].val.as_uint);
  } break;
  case Op::FDiv: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_real / cmd_inputs[1].val.as_real);
  } break;
  case Op::IMod: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_int % cmd_inputs[1].val.as_int);
  } break;
  case Op::UMod: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_uint % cmd_inputs[1].val.as_uint);
  } break;
  case Op::FMod: {
    frame.reg[result.val.as_ref] =
        Value(fmod(cmd_inputs[0].val.as_real, cmd_inputs[1].val.as_real));
  } break;
  case Op::BXor: {
    frame.reg[result.val.as_ref] =
        Value(cmd_inputs[0].val.as_bool != cmd_inputs[1].val.as_bool);
  } break;
  }
}

Block *Block::ExecuteJump(StackFrame &frame) {
  switch(exit.flag) {
  case Exit::Strategy::Uncond: return exit.true_block;
  case Exit::Strategy::Return: return nullptr;
  case Exit::Strategy::Cond: {
    Value v = exit.val;
    if (exit.val.flag == ValType::Ref) {
      v = frame.reg[v.val.as_ref];
    } else if (exit.val.flag == ValType::Arg) {
      v = frame.args[v.val.as_arg];
    }
    return v.val.as_bool ? exit.true_block : exit.false_block;
  }
  }
}

StackFrame::StackFrame(Func *f, const std::vector<Value> &args)
    : reg(f->num_cmds), args(args), inst_ptr(0), curr_block(f->entry()),
      prev_block(nullptr) {}

Value Call(Func *f, const std::vector<Value>& arg_vals) {
  StackFrame frame(f, arg_vals);

eval_loop_start:
  // std::cout << "block-" << frame.curr_block->block_num << ": " << frame.inst_ptr
  //           << std::endl;
  if (frame.inst_ptr == frame.curr_block->cmds.size()) {
    frame.prev_block = frame.curr_block;
    frame.curr_block = frame.curr_block->ExecuteJump(frame);

    if (!frame.curr_block) { // It's a return
      if (frame.prev_block->exit.val.flag == ValType::Ref) {
        return frame.reg[frame.prev_block->exit.val.val.as_ref];

      } else if (frame.prev_block->exit.val.flag == ValType::Arg) {
        return frame.reg[frame.prev_block->exit.val.val.as_arg];

      } else {
        return frame.prev_block->exit.val;
      }

    } else {
      frame.inst_ptr = 0;
      // std::cout << "  jumped to block-" << frame.curr_block->block_num << std::endl;
      // std::cin.ignore(1);
    }

  } else {
    auto cmd = frame.curr_block->cmds[frame.inst_ptr];
    assert(cmd.result.flag == ValType::Ref);
    if (cmd.op_code == Op::Phi) {
      Value chosen_val;
      for (size_t i = 0; i < cmd.incoming_blocks.size(); ++i) {
        if (frame.prev_block == cmd.incoming_blocks[i]) {
          chosen_val = cmd.args[i];
          goto found_match;
        }
      }
      assert(false && "No selection made from phi block");
    found_match:
      frame.reg[cmd.result.val.as_ref] = chosen_val;

    } else {
      cmd.Execute(frame);
    }

    // std::cout << "  " << cmd.result << " = " << vals[cmd.result.val.as_ref]
    //           << std::endl;
    // std::cin.ignore(1);
    ++frame.inst_ptr;
  }
  goto eval_loop_start;
}

} // namespace IR

#undef NOT_YET
