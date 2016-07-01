#include "Type/Type.h"
#include <cstdio>
#include "IR/Stack.h"

#include <ncurses.h>

namespace debug {
extern bool ct_eval;
} // namespace debug

namespace IR {
extern std::string OpCodeString(Op op_code);

void RefreshDisplay(const StackFrame &frame, LocalStack *local_stack) {
  clear();
  int height = 1 + ((int)local_stack->used) / 4;
  for (int i = 0; i < height; ++i) {
    mvprintw(i, 0, "%2d. [ %2x %2x %2x %2x ]", 4 * i,
             local_stack->allocs[4 * i] & 0xff,
             local_stack->allocs[4 * i + 1] & 0xff,
             local_stack->allocs[4 * i + 2] & 0xff,
             local_stack->allocs[4 * i + 3] & 0xff);
  }
  mvprintw(height, 0, "    [-------------]");

  for (size_t i = 0; i < frame.reg.size(); ++i) {
    mvprintw((int)i, 80, "%2d.> %8d", i, frame.reg[i].as_uint);
  }

  int row = 0;
  mvprintw(0, 30, "block-%ld", frame.curr_block->block_num);
  start_color();
  init_pair(1, COLOR_BLACK, COLOR_WHITE);
  for (const auto &cmd : frame.curr_block->cmds) {
    std::stringstream ss;
    ss << *cmd.result.type;
    if (cmd.result.type == Void) {
      ss << "\t\t  ";
    } else {
      ss << " %" << cmd.result.reg << "\t= ";
    }
    ss << OpCodeString(cmd.op_code) << " " << cmd.args[0];
    for (size_t i = 1; i < cmd.args.size(); ++i) { ss << ", " << cmd.args[i]; }

    auto output = ss.str();

    if (row == (int)frame.inst_ptr) {
      attron(COLOR_PAIR(1));
      mvprintw(++row, 34, "%s", output.c_str());
      attroff(COLOR_PAIR(1));
    } else {
      mvprintw(++row, 34, "%s", output.c_str());
    }
  }
  switch(frame.curr_block->exit.flag) {
  case Exit::Strategy::Uncond: {
    mvprintw(++row, 34, "jmp block-%lu",
             frame.curr_block->exit.true_block->block_num);
  } break;
  case Exit::Strategy::Cond: {
    std::stringstream ss;
    ss << frame.curr_block->exit.val;
    mvprintw(++row, 34, "br %s [T: block-%lu] [F: block-%lu]", ss.str().c_str(),
             frame.curr_block->exit.true_block->block_num,
             frame.curr_block->exit.false_block->block_num);
  } break;
  case Exit::Strategy::Return: {
    std::stringstream ss;
    ss << frame.curr_block->exit.val;
    mvprintw(++row, 34, "ret %s", ss.str().c_str());
  } break;
  case Exit::Strategy::ReturnVoid: mvprintw(++row, 34, "ret void"); break;
  case Exit::Strategy::Unset: UNREACHABLE;
  }

  mvprintw(20, 50, "Stack size:     %ld", local_stack->used);
  mvprintw(21, 50, "Stack capacity: %ld", local_stack->capacity);
  mvprintw(22, 50, "Frame offset:   %ld", frame.offset);
  mvprintw(23, 50, "Instr. Pointer: %ld", frame.inst_ptr);

  getch();
  refresh();
}

static Value ResolveValue(const StackFrame &frame, const Value &v) {
  if (v.flag == ValType::Reg) { return frame.reg[v.as_reg]; }
  if (v.flag == ValType::Arg) { return frame.args[v.as_arg]; }
  if (v.flag == ValType::RelAlloc) {
    return Value::Alloc(v.as_rel_alloc + frame.offset);
  }
  return v;
}

void Cmd::Execute(StackFrame& frame) {
  std::vector<Value> cmd_inputs;
  for (size_t i = 0; i < args.size(); ++i) {
    cmd_inputs.push_back(ResolveValue(frame, args[i]));
  }

  switch (op_code) {
  case Op::BNot: {
    frame.reg[result.reg] = Value(!cmd_inputs[0].as_bool);
  } break;
  case Op::INeg: {
    frame.reg[result.reg] = Value(-cmd_inputs[0].as_int);
  } break;
  case Op::FNeg: {
    frame.reg[result.reg] = Value(-cmd_inputs[0].as_real);
  } break;
  case Op::Call: {
    assert(cmd_inputs[0].flag == ValType::F);

    std::vector<Value> call_args;
    for (size_t i = 1; i < cmd_inputs.size(); ++i) {
      call_args.push_back(cmd_inputs[i]);
    }

    auto call_result =
        IR::Call(cmd_inputs[0].as_func, frame.stack, call_args);
    if (result.type != Void) { frame.reg[result.reg] = call_result; }
  } break;
  case Op::Cast: {
    assert(cmd_inputs[0].flag == ValType::T);
    assert(cmd_inputs[1].flag == ValType::T);
    if (cmd_inputs[0].as_type == Char) {
      auto c = cmd_inputs[2].as_char;
      if (cmd_inputs[1].as_type == Char) {
        frame.reg[result.reg] = Value(c);
      } else if (cmd_inputs[1].as_type == Uint) {
        frame.reg[result.reg] = Value((size_t)c);
      } else {
        NOT_YET;
      }
    } else if (cmd_inputs[0].as_type == Int) {
      auto i = cmd_inputs[2].as_int;
      if (cmd_inputs[1].as_type == Char) {
        frame.reg[result.reg] = Value((char)i);
      } else if (cmd_inputs[1].as_type == Int) {
        frame.reg[result.reg] = Value(i);
      } else if (cmd_inputs[1].as_type == Real) {
        frame.reg[result.reg] = Value((double)i);
      } else if (cmd_inputs[1].as_type == Uint) {
        frame.reg[result.reg] = Value((size_t)i);
      } else {
        NOT_YET;
      }
    } else if (cmd_inputs[0].as_type == Uint) {
      auto u = cmd_inputs[2].as_uint;
      if (cmd_inputs[1].as_type == Char) {
        frame.reg[result.reg] = Value((char)u);
      } else if (cmd_inputs[1].as_type == Int) {
        frame.reg[result.reg] = Value((long)u);
      } else if (cmd_inputs[1].as_type == Real) {
        frame.reg[result.reg] = Value((double)u);
      } else if (cmd_inputs[1].as_type == Uint) {
        frame.reg[result.reg] = Value(u);
      } else {
        NOT_YET;
      }
    } else {
      NOT_YET;
    }
  } break;
  case Op::Load: {
    assert(cmd_inputs[0].flag == ValType::Alloc);
    assert(result.type->is_primitive() &&
           "Non-primitive local variables are not yet implemented");
    size_t offset = cmd_inputs[0].as_alloc;

#define DO_LOAD(StoredType, stored_type)                                       \
  if (result.type == StoredType) {                                             \
    frame.reg[result.reg] =                                                    \
        Value(*(stored_type *)(frame.stack->allocs + offset));                 \
    break;                                                                     \
  }

    DO_LOAD(Bool, bool);
    DO_LOAD(Char, char);
    DO_LOAD(Int, long);
    DO_LOAD(Real, double);
    DO_LOAD(Uint, size_t);
    DO_LOAD(Type_, Type *);

#undef DO_LOAD_IF
  } break;

  case Op::Store: {
    assert(cmd_inputs[0].flag == ValType::T &&
           cmd_inputs[0].as_type->is_primitive());
    assert(cmd_inputs[2].flag == ValType::Alloc);
    size_t offset = cmd_inputs[2].as_alloc;

#define DO_STORE(cpp_type, t, T)                                               \
  if (cmd_inputs[0].as_type == T) {                                            \
    auto ptr = (cpp_type *)(frame.stack->allocs + offset);                     \
    *ptr     = cmd_inputs[1].as_##t;                                           \
    break;                                                                     \
  }

    DO_STORE(bool, bool, Bool);
    DO_STORE(char, char, Char);
    DO_STORE(long, int, Int);
    DO_STORE(double, real, Real);
    DO_STORE(size_t, uint, Uint);
    DO_STORE(Type *, type, Type_);

#undef DO_STORE
  } break;
  case Op::Field: {
    auto struct_type = (Structure *)(cmd_inputs[0].as_type);

    // TODO what if it's heap-allocated?
    assert(cmd_inputs[1].flag == ValType::Alloc);
    assert(cmd_inputs[2].flag == ValType::U);

    auto ptr = cmd_inputs[1].as_alloc;

    size_t field_index = cmd_inputs[2].as_uint;

    // field_index + 1 is correct because it simplifies the offset computation
    // to have the zero present.
    ptr += struct_type->field_offsets AT(field_index + 1);
    frame.reg[result.reg] = Value::Alloc(ptr);
  } break;
  case Op::Access: {
    frame.reg[result.reg] = Value::Alloc(
        cmd_inputs[2].as_alloc +
        cmd_inputs[1].as_uint * cmd_inputs[0].as_type->SpaceInArray());
  } break;
                  /*
  case Op::GEP: {
    if (cmd_inputs[0].as_type->is_array()) {
      auto array_type = (Array *)(cmd_inputs[0].as_type);
      if (array_type->fixed_length) {
        if (cmd_inputs[1].flag == ValType::Alloc) {
          auto ptr = cmd_inputs[1].as_alloc +
                     (size_t)(cmd_inputs[2].as_int) * sizeof(char *);

          for (size_t i = 3; i < cmd_inputs.size(); ++i) {
            ptr += (size_t)(cmd_inputs[i].as_int) *
                   array_type->data_type->SpaceInArray();
          }

          // frame.reg[result.reg] = Value::Alloc(ptr);
        } else {
          NOT_YET;
        }
      } else {
        NOT_YET;
      }
    }
  } break;
  */
  case Op::Phi: {
    assert((cmd_inputs.size() & 1u) == 0u);
    for (size_t i = 0; i < cmd_inputs.size(); i += 2) {
      if (frame.prev_block == cmd_inputs[i].as_block) {
        frame.reg[result.reg] = cmd_inputs[i + 1];
        goto exit_successfully;
      }
    }
    assert(false && "No selection made from phi block");
  exit_successfully:;
  } break;
  case Op::IAdd: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_int + cmd_inputs[1].as_int);
  } break;
  case Op::UAdd: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_uint + cmd_inputs[1].as_uint);
  } break;
  case Op::FAdd: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_real + cmd_inputs[1].as_real);
  } break;
  case Op::ISub: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_int - cmd_inputs[1].as_int);
  } break;
  case Op::USub: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_uint - cmd_inputs[1].as_uint);
  } break;
  case Op::FSub: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_real - cmd_inputs[1].as_real);
  } break;
  case Op::IMul: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_int * cmd_inputs[1].as_int);
  } break;
  case Op::UMul: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_uint * cmd_inputs[1].as_uint);
  } break;
  case Op::FMul: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_real * cmd_inputs[1].as_real);
  } break;
  case Op::IDiv: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_int / cmd_inputs[1].as_int);
  } break;
  case Op::UDiv: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_uint / cmd_inputs[1].as_uint);
  } break;
  case Op::FDiv: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_real / cmd_inputs[1].as_real);
  } break;
  case Op::IMod: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_int % cmd_inputs[1].as_int);
  } break;
  case Op::UMod: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_uint % cmd_inputs[1].as_uint);
  } break;
  case Op::FMod: {
    frame.reg[result.reg] =
        Value(fmod(cmd_inputs[0].as_real, cmd_inputs[1].as_real));
  } break;
  case Op::BXor: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_bool != cmd_inputs[1].as_bool);
  } break;
  case Op::ILT: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_int < cmd_inputs[1].as_int);
  } break;
  case Op::ULT: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_uint < cmd_inputs[1].as_uint);
  } break;
  case Op::FLT: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_real < cmd_inputs[1].as_real);
  } break;
  case Op::ILE: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_int <= cmd_inputs[1].as_int);
  } break;
  case Op::ULE: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_uint <= cmd_inputs[1].as_uint);
  } break;
  case Op::FLE: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_real <= cmd_inputs[1].as_real);
  } break;
  case Op::IEQ: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_int == cmd_inputs[1].as_int);
  } break;
  case Op::UEQ: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_uint == cmd_inputs[1].as_uint);
  } break;
  case Op::FEQ: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_char == cmd_inputs[1].as_char);
  } break;
  case Op::BEQ: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_bool == cmd_inputs[1].as_bool);
  } break;
  case Op::CEQ: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_char == cmd_inputs[1].as_char);
  } break;
  case Op::TEQ: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_type == cmd_inputs[1].as_type);
  } break;
  case Op::FnEQ: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_func == cmd_inputs[1].as_func);
  } break;
  case Op::INE: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_int != cmd_inputs[1].as_int);
  } break;
  case Op::UNE: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_uint != cmd_inputs[1].as_uint);
  } break;
  case Op::FNE: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_char != cmd_inputs[1].as_char);
  } break;
  case Op::BNE: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_bool != cmd_inputs[1].as_bool);
  } break;
  case Op::CNE: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_char != cmd_inputs[1].as_char);
  } break;
  case Op::TNE: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_type != cmd_inputs[1].as_type);
  } break;
  case Op::FnNE: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_func != cmd_inputs[1].as_func);
  } break;
  case Op::IGE: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_int >= cmd_inputs[1].as_int);
  } break;
  case Op::UGE: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_uint >= cmd_inputs[1].as_uint);
  } break;
  case Op::FGE: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_real >= cmd_inputs[1].as_real);
  } break;
  case Op::IGT: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_int > cmd_inputs[1].as_int);
  } break;
  case Op::UGT: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_uint > cmd_inputs[1].as_uint);
  } break;
  case Op::FGT: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_real > cmd_inputs[1].as_real);
  } break;
  case Op::Print: {
    if (cmd_inputs[0].as_type == Bool) {
      fprintf(stderr, cmd_inputs[1].as_bool ? "true" : "false");

    } else if (cmd_inputs[0].as_type == Char) {
      fprintf(stderr, "%c", cmd_inputs[1].as_char);

    } else if (cmd_inputs[0].as_type == Int) {
      fprintf(stderr, "%ld", cmd_inputs[1].as_int);

    } else if (cmd_inputs[0].as_type == Real) {
      fprintf(stderr, "%lf", cmd_inputs[1].as_real);

    } else if (cmd_inputs[0].as_type == Uint) {
      fprintf(stderr, "%lu", cmd_inputs[1].as_uint);

    } else if (cmd_inputs[0].as_type == Type_) {
      fprintf(stderr, "%s", cmd_inputs[1].as_type->to_string().c_str());
    }else {
      NOT_YET;
    }
  } break;
  case Op::TC_Ptr: {
    frame.reg[result.reg] = Value(Ptr(cmd_inputs[0].as_type));
  } break;
  case Op::TC_Arrow: {
    frame.reg[result.reg] =
        Value(::Func(cmd_inputs[0].as_type, cmd_inputs[1].as_type));
  } break;
  case Op::TC_Arr1: {
    frame.reg[result.reg] = Value(Arr(cmd_inputs[0].as_type));
  } break;
  case Op::TC_Arr2: {
    frame.reg[result.reg] =
        Value(Arr(cmd_inputs[1].as_type, cmd_inputs[0].as_uint));
  } break;
  case Op::Bytes: {
    frame.reg[result.reg] = Value(cmd_inputs[0].as_type->bytes());
  } break;
  case Op::Alignment: {
    frame.reg[result.reg] = Value(cmd_inputs[0].as_type->alignment());
  } break;
  }
}

Block *Block::ExecuteJump(StackFrame &frame) {
  switch (exit.flag) {
  case Exit::Strategy::Uncond: return exit.true_block;
  case Exit::Strategy::ReturnVoid:
  case Exit::Strategy::Return: return nullptr;
  case Exit::Strategy::Cond: {
    Value v = exit.val;
    if (exit.val.flag == ValType::Reg) {
      v = frame.reg[v.as_reg];
    } else if (exit.val.flag == ValType::Arg) {
      v = frame.args[v.as_arg];
    }
    return v.as_bool ? exit.true_block : exit.false_block;
  }
  case Exit::Strategy::Unset: UNREACHABLE;
  }
}

StackFrame::StackFrame(Func *f, LocalStack *local_stack,
                       const std::vector<Value> &args)
    : reg(f->num_cmds), stack(local_stack), args(args), func(f), alignment(16),
      size(f->frame_size), offset(0), inst_ptr(0), curr_block(f->entry()),
      prev_block(nullptr) {
  stack->AddFrame(this);
  // TODO determine frame alignment and make it correct! Currently just assuming
  // 16-bytes for safety.
}

void LocalStack::AddFrame(StackFrame *fr) {
  size_t old_use_size = used;
  used       = MoveForwardToAlignment(used, fr->alignment);
  fr->offset = used;
  used += fr->size;

  if (used <= capacity) { return; }

  while (used > capacity) { capacity *= 2; }
  char *new_allocs = (char *)malloc(capacity);
  memcpy(new_allocs, allocs, old_use_size);
  free(allocs);
  allocs = new_allocs;
}

void LocalStack::RemoveFrame(StackFrame *fr) { used = fr->offset; }

Value Call(Func *f, LocalStack *local_stack,
           const std::vector<Value> &arg_vals) {
  StackFrame frame(f, local_stack, arg_vals);

eval_loop_start:
  if (frame.inst_ptr == frame.curr_block->cmds.size()) {
    if (debug::ct_eval) { RefreshDisplay(frame, frame.stack); }
    frame.prev_block = frame.curr_block;
    frame.curr_block = frame.curr_block->ExecuteJump(frame);

    if (!frame.curr_block) { // It's a return (perhaps void)
      return (frame.prev_block->exit.flag == Exit::Strategy::Return)
                 ? ResolveValue(frame, frame.prev_block->exit.val)
                 : Value();
    } else {
      frame.inst_ptr = 0;
    }

  } else {
    if (debug::ct_eval) { RefreshDisplay(frame, frame.stack); }

    frame.curr_block->cmds[frame.inst_ptr].Execute(frame);
    ++frame.inst_ptr;
  }
  goto eval_loop_start;
}
} // namespace IR
