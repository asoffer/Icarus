#include "Type/Type.h"
#include <cstdio>
#include "IR/Stack.h"

#include <ncurses.h>

extern const char *GetGlobalStringNumbered(size_t index);
extern std::stack<Scope *> ScopeStack;

namespace IR {
extern std::string Escape(char c);
extern std::string OpCodeString(Op op_code);
extern std::vector<IR::Value> InitialGlobals;

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
    switch (frame.reg[i].flag) {
    case ValType::B: {
      mvprintw((int)i, 100, "%2d.> %8s", i,
               (frame.reg[i].as_bool ? "true" : "false"));
    } break;
    case ValType::C: {
      switch (frame.reg[i].as_char) {
      case '\n': mvprintw((int)i, 100, "%2d.>      '\\n'", i); break;
      case '\r': mvprintw((int)i, 100, "%2d.>      '\\r'", i); break;
      case '\t': mvprintw((int)i, 100, "%2d.>      '\\t'", i); break;
      case '\b': mvprintw((int)i, 100, "%2d.>      '\\b'", i); break;
      case '\v': mvprintw((int)i, 100, "%2d.>      '\\v'", i); break;
      case '\a': mvprintw((int)i, 100, "%2d.>      '\\a'", i); break;
      default:
        mvprintw((int)i, 100, "%2d.>      '%s'", i,
                 Escape(frame.reg[i].as_char).c_str());
      }
    } break;
    case ValType::I: {
      mvprintw((int)i, 100, "%2d.> %8ldi", i, frame.reg[i].as_int);
    } break;
    case ValType::R: {
      mvprintw((int)i, 100, "%2d.> %8lff", i, frame.reg[i].as_real);
    } break;
    case ValType::U: {
      mvprintw((int)i, 100, "%2d.> %8luu", i, frame.reg[i].as_uint);
    } break;
    case ValType::T: {
      if (frame.reg[i].as_type) {
        mvprintw((int)i, 100, "%2d.> %s", i,
                 frame.reg[i].as_type->to_string().c_str());
      } else {
        mvprintw((int)i, 100, "%2d.> --", i);
      }
    } break;
    case ValType::F: {
      mvprintw((int)i, 100, "%2d.> %8lu (func)", i,
               frame.reg[i].as_func);
    } break;
    case ValType::HeapAddr: {
      mvprintw((int)i, 100, "%2d.> %8lu (heap addr)", i,
               frame.reg[i].as_heap_addr);
    } break;
    case ValType::StackAddr: {
      mvprintw((int)i, 100, "%2d.> %8lu (stack addr)", i,
               frame.reg[i].as_stack_addr);
    } break;
    default: { mvprintw((int)i, 100, "%2d.> ........", i); } break;
    }
  }

  int row = 0;
  mvprintw(0, 30, "%s", frame.curr_block->block_name);
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

  frame.curr_block->exit->ShowExit(row);

  mvprintw(39, 2, "Stack size:     %ld", local_stack->used);
  mvprintw(40, 2, "Stack capacity: %ld", local_stack->capacity);
  mvprintw(41, 2, "Frame offset:   %ld", frame.offset);
  mvprintw(42, 2, "Instr. Pointer: %ld", frame.inst_ptr);

  getch();
  refresh();
}

void Exit::Unconditional::ShowExit(int &row) {
  mvprintw(++row, 34, "jmp %s", block->block_name);
}

void Exit::Conditional::ShowExit(int &row) {
  std::stringstream ss;
  ss << cond;
  mvprintw(++row, 34, "br %s [T: %s] [F: %s]", ss.str().c_str(),
           true_block->block_name, false_block->block_name);
}

void Exit::Return::ShowExit(int &row) {
  std::stringstream ss;
  ss << ret_val;
  mvprintw(++row, 34, "ret %s", ss.str().c_str());
}

void Exit::ReturnVoid::ShowExit(int &row) { mvprintw(++row, 34, "ret"); }

void Exit::Switch::ShowExit(int &row) {
  std::stringstream ss;
  ss << cond;
  mvprintw(++row, 34, "switch (%llu) %s", table.size(), ss.str().c_str());
  for (auto entry : table) {
    std::stringstream ss;
    ss << entry.first;
    mvprintw(++row, 36, "[%s => %s]", ss.str().c_str(),
             entry.second->block_name);
  }
}

static Value ResolveValue(const StackFrame &frame, const Value &v) {
  if (v.flag == ValType::Reg) { return frame.reg[v.as_reg]; }
  if (v.flag == ValType::Arg) { return frame.args[v.as_arg]; }
  if (v.flag == ValType::FrameAddr) {
    return Value::StackAddr(v.as_frame_addr + frame.offset);
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
    if (cmd_inputs[0].flag == ValType::T) {
      assert(cmd_inputs[0].as_type->is_parametric_struct());
      cmd_inputs[0] = IR::Value(((ParametricStructure *)cmd_inputs[0].as_type)
                                    ->ast_expression->ir_func);
    }
    assert(cmd_inputs[0].flag == ValType::F);

    std::vector<Value> call_args;
    for (size_t i = 1; i < cmd_inputs.size(); ++i) {
      call_args.push_back(cmd_inputs[i]);
    }

    auto call_result =
        cmd_inputs[0].as_func->Call(frame.stack, call_args);
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
  case Op::Malloc: {
    auto ptr = malloc(cmd_inputs[0].as_uint);
    if (!ptr) {
      fprintf(stderr, "malloc failed to allocate %lu byte(s).\n",
              cmd_inputs[1].as_uint);
      std::exit(-1);
    }
    frame.reg[result.reg] = Value::HeapAddr(ptr);
  } break;
  case Op::Free: {
    free(cmd_inputs[0].as_heap_addr);
  } break;
  case Op::Memcpy: {
    void *dest;
    switch (cmd_inputs[0].flag) {
    case ValType::HeapAddr: dest = cmd_inputs[0].as_heap_addr; break;
    default: UNREACHABLE;
    }

    const void *source;
    switch (cmd_inputs[1].flag) {
    case ValType::GlobalCStr:
      source = GetGlobalStringNumbered(cmd_inputs[1].as_global_cstr);
      break;
    default: UNREACHABLE;
    }

    memcpy(dest, source, cmd_inputs[2].as_uint);
  } break;
  case Op::Load: {
    assert(
        (result.type->is_primitive() || result.type->is_pointer() ||
         result.type->is_enum() || result.type->is_function()) &&
        "Non-primitive/pointer/enum local variables are not yet implemented");
    if (cmd_inputs[0].flag == ValType::StackAddr) {
      size_t offset = cmd_inputs[0].as_stack_addr;

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

#undef DO_LOAD
      assert(result.type->is_pointer() || result.type->is_enum() ||
             result.type->is_function());
      if (result.type->is_pointer()) {
        // TODO how do we determine if it's heap or stack?
        auto ptr_as_uint = *(size_t *)(frame.stack->allocs + offset);
        if (ptr_as_uint > 0xffff) { // NOTE hack to guess if it's a heap address
          frame.reg[result.reg] = Value::HeapAddr((void *)ptr_as_uint);
        } else {
          frame.reg[result.reg] = Value::StackAddr(ptr_as_uint);
        }
      } else if (result.type->is_function()) {
        frame.reg[result.reg] = Value(*(Func **)(frame.stack->allocs + offset));
      } else {
        frame.reg[result.reg] =
            Value(*(size_t *)(frame.stack->allocs + offset));
      }

    } else if (cmd_inputs[0].flag == ValType::HeapAddr) {
#define DO_LOAD(StoredType, stored_type)                                       \
  if (result.type == StoredType) {                                             \
    frame.reg[result.reg] =                                                    \
        Value(*(stored_type *)(cmd_inputs[0].as_heap_addr));                   \
    break;                                                                     \
  }

      DO_LOAD(Bool, bool);
      DO_LOAD(Char, char);
      DO_LOAD(Int, long);
      DO_LOAD(Real, double);
      DO_LOAD(Uint, size_t);
      DO_LOAD(Type_, Type *);

#undef DO_LOAD
      assert(result.type->is_pointer() || result.type->is_enum());
      if (result.type->is_pointer()) {
        // TODO how do we determine if it's heap or stack?
        auto ptr_as_uint = *(size_t *)(cmd_inputs[0].as_heap_addr);
        if (ptr_as_uint > 0xffff) { // NOTE hack to guess if it's a heap address
          frame.reg[result.reg] = Value::HeapAddr((void *)ptr_as_uint);
        } else {
          frame.reg[result.reg] = Value::StackAddr(ptr_as_uint);
        }
      } else {
        frame.reg[result.reg] = Value(*(size_t *)(cmd_inputs[0].as_heap_addr));
      }

    } else if (cmd_inputs[0].flag == ValType::GlobalAddr) {
      frame.reg[result.reg] = InitialGlobals[cmd_inputs[0].as_global_addr];

    } else if (cmd_inputs[0].flag == ValType::F) {
      frame.reg[result.reg] = cmd_inputs[0];

    } else {
      frame.curr_block->dump();
      UNREACHABLE;
    }
  } break;

  case Op::Store: {
    assert(cmd_inputs[0].flag == ValType::T &&
           (cmd_inputs[0].as_type->is_primitive() ||
            cmd_inputs[0].as_type->is_pointer() ||
            cmd_inputs[0].as_type->is_enum() ||
            cmd_inputs[0].as_type->is_function()));
    assert(cmd_inputs[2].flag == ValType::StackAddr ||
           cmd_inputs[2].flag == ValType::HeapAddr ||
           cmd_inputs[2].flag == ValType::GlobalAddr);

    if (cmd_inputs[2].flag == ValType::StackAddr) {
      size_t offset = cmd_inputs[2].as_stack_addr;

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
      if (cmd_inputs[0].as_type->is_enum()) {
        auto ptr = (size_t *)(frame.stack->allocs + offset);
        *ptr = cmd_inputs[1].as_uint;
        break;
      }

      if (cmd_inputs[0].as_type->is_pointer()) {
        auto ptr = (void **)(frame.stack->allocs + offset);
        switch (cmd_inputs[1].flag) {
        case ValType::HeapAddr: *ptr = cmd_inputs[1].as_heap_addr; break;
        case ValType::Null: *ptr = nullptr; break;
        default: NOT_YET;
        }
        break;
      }

      if (cmd_inputs[0].as_type->is_function()) {
        auto ptr = (void **)(frame.stack->allocs + offset);
        switch (cmd_inputs[1].flag) {
        case ValType::F: *ptr = cmd_inputs[1].as_func; break;
        case ValType::HeapAddr: *ptr = cmd_inputs[1].as_heap_addr; break;
        case ValType::Null: *ptr = nullptr; break;
        default: NOT_YET;
        }
        break;
      }

      if (cmd_inputs[0].as_type == Void) {
        auto ptr = (void *)(frame.stack->allocs + offset);
        *(size_t *)ptr = cmd_inputs[1].as_stack_addr;
        break;
      }
    } else if (cmd_inputs[2].flag == ValType::HeapAddr) {

#define DO_STORE(cpp_type, t, T)                                               \
  if (cmd_inputs[0].as_type == T) {                                            \
    auto ptr = (cpp_type *)(cmd_inputs[2].as_heap_addr);                       \
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
      if (cmd_inputs[0].as_type->is_enum()) {
        NOT_YET;
        break;
      }

      if (cmd_inputs[0].as_type->is_function()) {
        NOT_YET;
        break;

      } else if (cmd_inputs[0].as_type->is_pointer()) {
        NOT_YET;
        break;

      } else if (cmd_inputs[0].as_type == Void) {
        NOT_YET;
        break;
      }
    } else {
      InitialGlobals[cmd_inputs[2].as_global_addr] = cmd_inputs[1];
      break;
    }
    UNREACHABLE;
  } break;
  case Op::Field: {
    auto struct_type = (Structure *)(cmd_inputs[0].as_type);

    // TODO what if it's heap-allocated?
    assert(cmd_inputs[1].flag == ValType::StackAddr);
    assert(cmd_inputs[2].flag == ValType::U);

    auto ptr = cmd_inputs[1].as_stack_addr;

    size_t field_index = cmd_inputs[2].as_uint;

    // field_index + 1 is correct because it simplifies the offset computation
    // to have the zero present.
    ptr += struct_type->field_offsets AT(field_index + 1);
    frame.reg[result.reg] = Value::StackAddr(ptr);
  } break;
  case Op::PtrIncr: {
    switch (cmd_inputs[1].flag) {
    case ValType::StackAddr: {
      assert(cmd_inputs[0].as_type->is_pointer());
      // TODO space in array or bytes()?
      auto pointee_size =
          ((Pointer *)cmd_inputs[0].as_type)->pointee->SpaceInArray();
      frame.reg[result.reg] = Value::StackAddr(
          cmd_inputs[1].as_stack_addr + cmd_inputs[2].as_uint * pointee_size);
    } break;
    case ValType::HeapAddr: {
      assert(cmd_inputs[0].flag == ValType::T);
      assert(cmd_inputs[0].as_type->is_pointer());
      // TODO space in array or bytes()?
      auto pointee_size =
          ((Pointer *)cmd_inputs[0].as_type)->pointee->SpaceInArray();
      frame.reg[result.reg] =
          Value::HeapAddr((char *)cmd_inputs[1].as_heap_addr +
                          cmd_inputs[2].as_uint * pointee_size);
    } break;
    default:
      const_cast<Func *>(frame.func)->dump();
      std::cerr << cmd_inputs[1] << '\n';
      UNREACHABLE;
    }

  } break;
  case Op::Access: {
    switch (cmd_inputs[2].flag) {
    case ValType::StackAddr:
      frame.reg[result.reg] = Value::StackAddr(
          cmd_inputs[2].as_stack_addr +
          cmd_inputs[1].as_uint * cmd_inputs[0].as_type->SpaceInArray());
      break;
    case ValType::HeapAddr:
      frame.reg[result.reg] = Value::HeapAddr(
          (char *)(cmd_inputs[2].as_heap_addr) +
          cmd_inputs[1].as_uint * cmd_inputs[0].as_type->SpaceInArray());
      break;
    default: std::cerr << cmd_inputs[2] << '\n'; UNREACHABLE;
    }
  } break;
  case Op::Phi: {
    assert((cmd_inputs.size() & 1u) == 0u);
    for (size_t i = 0; i < cmd_inputs.size(); i += 2) {
      if (frame.prev_block == cmd_inputs[i].as_block) {
        frame.reg[result.reg] = cmd_inputs[i + 1];
        goto exit_successfully;
      }
    }
    std::cerr << frame.prev_block << '\n';
    assert(false && "No selection made from phi block");
  exit_successfully:;
  } break;
  case Op::CAdd: {
    frame.reg[result.reg] =
        Value((char)(cmd_inputs[0].as_char + cmd_inputs[1].as_char));
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
  case Op::BOr: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_bool || cmd_inputs[1].as_bool);
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
  case Op::PtrEQ: {
    // We can use as_heap_addr because it's the same size as stack_addr
    frame.reg[result.reg] =
        Value(cmd_inputs[0].flag == cmd_inputs[1].flag &&
              cmd_inputs[0].as_heap_addr == cmd_inputs[1].as_heap_addr);
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
  case Op::CGT: {
    frame.reg[result.reg] =
        Value(cmd_inputs[0].as_char > cmd_inputs[1].as_char);
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
  case Op::NOp: break;
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

    } else if(cmd_inputs[0].as_type == String) {
      fprintf(stderr, "%s", cmd_inputs[1].as_cstr);

    } else if (cmd_inputs[0].as_type->is_enum()) {
      auto enum_type = (Enum *)cmd_inputs[0].as_type;
      // TODO read the actual names rather than just the numeric values
      fprintf(stderr, "%s.%s", enum_type->to_string().c_str(),
              enum_type->members[cmd_inputs[1].as_uint].c_str());

    } else if (cmd_inputs[0].as_type->is_pointer()) {
      fprintf(stderr, "0x%lx", cmd_inputs[0].as_uint);

    } else {
      UNREACHABLE;
    }
  } break;
  case Op::TC_Tup: {
    std::vector<Type *> type_vec;
    for (auto v : cmd_inputs) {
      assert(v.flag == ValType::T);
      type_vec.push_back(v.as_type);
    }
    frame.reg[result.reg] = Value(Tup(type_vec));
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
  case Op::Trunc: {
    frame.reg[result.reg] = Value((char)cmd_inputs[0].as_uint);
    break;
  }
  case Op::ZExt: {
    frame.reg[result.reg] = Value((size_t)cmd_inputs[0].as_char);
    break;
  }
  case Op::ArrayLength: {
    if (cmd_inputs[0].flag == ValType::StackAddr) {
      frame.reg[result.reg] = cmd_inputs[0];

    } else if (cmd_inputs[0].flag == ValType::HeapAddr) {
      NOT_YET;
    } else {
      UNREACHABLE;
    }
  } break;
  case Op::ArrayData: {
    if (cmd_inputs[1].flag == ValType::StackAddr) {
      frame.reg[result.reg] =
          IR::Value::StackAddr(cmd_inputs[1].as_stack_addr + sizeof(char *));

    } else if (cmd_inputs[1].flag == ValType::HeapAddr) {
      NOT_YET;
    } else {
      UNREACHABLE;
    }
  } break;
  case Op::PushField: {
    // NOTE: Hack. It's not a heap address, it's a vector pointer
    auto vec_ptr =
        (std::vector<std::tuple<const char *, Value, Value>> *)cmd_inputs[0]
            .as_heap_addr;
    vec_ptr->emplace_back(cmd_inputs[1].as_cstr, cmd_inputs[2], cmd_inputs[3]);
  } break;
  case Op::InitFieldVec: {
    auto vec_ptr = new std::vector<std::tuple<const char *, Value, Value>>;
    vec_ptr->reserve(cmd_inputs[0].as_uint);
    frame.reg[result.reg] = Value::HeapAddr(vec_ptr);
  } break;
  case Op::GetFromCache: {
    assert(cmd_inputs[0].as_type->is_parametric_struct());
    auto param_struct_type = (ParametricStructure *)cmd_inputs[0].as_type;

    std::vector<IR::Value> param_vals;
    for (auto a : frame.args) {
      assert(a.flag == ValType::T); // What if an argument isn't a type?
      param_vals.push_back(IR::Value(a.as_type));
    }
    auto iter = param_struct_type->ast_expression->cache.find(param_vals);
    if (iter == param_struct_type->ast_expression->cache.end()) {
      auto struct_lit = new AST::StructLiteral;
      param_struct_type->ast_expression->cache[param_vals] = struct_lit;
      auto struct_type = new Structure("anon", struct_lit);
      struct_lit->value = IR::Value(struct_type);

      frame.reg[result.reg] = IR::Value::None();
    } else {
      frame.reg[result.reg] = IR::Value(iter->second->value.as_type);
    }
  } break;
  case Op::CreateStruct: {
    auto vec_ptr =
        (std::vector<std::tuple<const char *, Value, Value>> *)cmd_inputs[0]
            .as_heap_addr;

    auto param_struct_lit =
        (AST::ParametricStructLiteral *)cmd_inputs[1].as_heap_addr;
    std::vector<IR::Value> param_vals;
    for (auto a : frame.args) {
      assert(a.flag == ValType::T); // What if an argument isn't a type?
      param_vals.push_back(IR::Value(a.as_type));
    }
    auto struct_lit                             = param_struct_lit->cache[param_vals];
    param_struct_lit->reverse_cache[struct_lit] = param_vals;

    Cursor loc;
    for (const auto &tuple_val : *vec_ptr) {
      auto id          = new AST::Identifier(loc, std::get<0>(tuple_val));
      auto decl        = new AST::Declaration;
      decl->identifier = id;
      id->decl         = decl;

      assert(std::get<1>(tuple_val).flag == ValType::T);
      auto ty = std::get<1>(tuple_val).as_type;
      decl->type_expr = ty ? new AST::DummyTypeExpr(loc, ty) : nullptr;

      auto init_val = std::get<2>(tuple_val);
      if (!(init_val.flag == ValType::T && init_val.as_type == nullptr)) {
        // If it's not null,
        auto term      = new AST::Terminal;
        term->loc      = loc;
        decl->init_val = term;
        switch (init_val.flag) {
        case ValType::B:
          term->terminal_type = init_val.as_bool ? Language::Terminal::True
                                                 : Language::Terminal::False;
          term->type  = Bool;
          break;
        case ValType::C:
          term->terminal_type = Language::Terminal::Char;
          term->type          = Char;
          break;
        case ValType::I:
          term->terminal_type = Language::Terminal::Int;
          term->type          = Int;
          break;
        case ValType::R:
          term->terminal_type = Language::Terminal::Real;
          term->type          = Real;
          break;
        case ValType::U:
          term->terminal_type = Language::Terminal::Uint;
          term->type          = Uint;
          break;

        default: NOT_YET;
        }
        term->value = init_val;
      }
      struct_lit->decls.push_back(decl);
    }

    ScopeStack.push(struct_lit->type_scope);
    for (auto d : struct_lit->decls) { d->assign_scope(); }
    ScopeStack.pop();

    struct_lit->verify_types();
    struct_lit->CompleteDefinition();

    frame.reg[result.reg] = Value(struct_lit->value.as_type);
  } break;
  }
}

Block *Exit::ReturnVoid::JumpBlock(StackFrame &fr) { return nullptr; }
Block *Exit::Return::JumpBlock(StackFrame &fr) { return nullptr; }
Block *Exit::Unconditional::JumpBlock(StackFrame &fr) { return block; }
Block *Exit::Conditional::JumpBlock(StackFrame &fr) {
  Value val = ResolveValue(fr, cond);
  assert(val.flag == ValType::B);
  return val.as_bool ? true_block : false_block;
}

Block *Exit::Switch::JumpBlock(StackFrame &fr) {
  Value val = ResolveValue(fr, cond);
  switch (val.flag) {
  case ValType::C:
    for (auto entry : table) {
      if (entry.first.as_char != val.as_char) { continue; }
      return entry.second;
    }
    return default_block;
  default: UNREACHABLE;
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

Value IR::Func::Call(LocalStack *local_stack,
                     const std::vector<Value> &arg_vals) {
  StackFrame frame(this, local_stack, arg_vals);

eval_loop_start:
  if (frame.inst_ptr == frame.curr_block->cmds.size()) {
    if (debug::ct_eval) { RefreshDisplay(frame, frame.stack); }
    frame.prev_block = frame.curr_block;
    frame.curr_block = frame.curr_block->exit->JumpBlock(frame);

    if (!frame.curr_block) { // It's a return (perhaps void)
      return (frame.prev_block->exit->is_return())
                 ? ResolveValue(
                       frame, ((Exit::Return *)frame.prev_block->exit)->ret_val)
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
