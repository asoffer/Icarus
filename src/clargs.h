#ifndef ICARUS_CLARGS_H
#define ICARUS_CLARGS_H

namespace debug {
extern bool timer;
extern bool parser;
extern bool parametric_struct;
extern bool ct_eval;
} // namespace debug

enum class CLArgFlag { QuitSuccessfully, QuitWithFailure, Continue };
enum class FileType { Bin, IR, Nat };

FileType file_type = FileType::Bin;
const char *output_file_name = "a.out";

namespace TypeSystem {
void initialize();
} // namespace TypeSystem

static void
ShowUsage(char *argv0) {
  fprintf(stderr,
          "Usage: %s [options] input_file... -o output_file\n\n"
          "  -o output_file                 Default is a.out\n\n"
          "  -e, --eval                     Run compile-time evaluator step-by-step (debug).\n\n"
          "  --file-type=[ir|nat|bin]       Output a file of the specified type:\n"
          "                                   ir  - LLVM intermediate representation\n"
          "                                   nat - Output a native object file\n"
          "                                   bin - Output a single native object file and link it (requires gcc).\n"
          "                                         This is the default option.\n\n"
          "  -h, --help                     Display this usage message.\n\n"
          "  -p, --parser                   Display step-by-step file parsing (debug).\n\n"
          "  -s, --param-struct             Display debug information for parametric struct cloning (debug).\n\n"
          "  -t, --timer                    Display timing information for compilation steps (debug).\n\n"
          "\n",
          argv0);
}

static CLArgFlag ParseCLArguments(int argc, char *argv[]) {
  for (int arg_num = 1; arg_num < argc; ++arg_num) {
    auto arg = argv[arg_num];

    if (strcmp(arg, "-o") == 0) {
      if (++arg_num == argc) {
        ShowUsage(argv[0]);
        return CLArgFlag::QuitWithFailure;
      }
      arg = argv[arg_num];
      if (arg[0] == '-') {
        ShowUsage(argv[0]);
        return CLArgFlag::QuitWithFailure;
      }
      output_file_name = arg;
      goto next_arg;
    }

    if (arg[0] == '-') {
      if (arg[1] == '-') {
        /* Long-form argument */
        char *ptr = arg + 2;
        while (*ptr != '=' && *ptr != '\0') { ++ptr; }
        if (*ptr == '=') {
          /* Long-form with value */

          *ptr = '\0';
          ptr++; // points to the argument

          if (strcmp(arg + 2, "file-type") == 0) {
            if (strcmp(ptr, "ir") == 0) {
              file_type = FileType::IR;
              *ptr = '=';
              goto next_arg;

            } else if (strcmp(ptr, "nat") == 0) {
              file_type = FileType::Nat;
              *ptr = '=';
              goto next_arg;

            } else if (strcmp(ptr, "bin") == 0) {
              file_type = FileType::Bin;
              *ptr = '=';
              goto next_arg;

            } else {
              ShowUsage(argv[0]);
              *ptr = '=';
              return CLArgFlag::QuitWithFailure;
            }
          } else {
            ShowUsage(argv[0]);
            *ptr = '=';
            return CLArgFlag::QuitWithFailure;
          }
        } else {
          /* Long-form flag */
          if (strcmp(arg + 2, "eval") == 0) {
            debug::ct_eval = true;
            goto next_arg;

          } else if (strcmp(arg + 2, "help") == 0) {
            ShowUsage(argv[0]);
            return CLArgFlag::QuitSuccessfully;

          } else if (strcmp(arg + 2, "parser") == 0) {
            debug::parser = true;
            goto next_arg;

          } else if (strcmp(arg + 2, "timer") == 0) {
            debug::timer = true;
            goto next_arg;

          } else if (strcmp(arg + 2, "param-struct") == 0) {
            debug::parametric_struct = true;
            goto next_arg;

          } else {
            ShowUsage(argv[0]);
            return CLArgFlag::QuitWithFailure;
          }
        }

      } else {
        /* Short-form arguments */
        for (auto ptr = arg + 1; ptr; ++ptr) {
          switch (*ptr) {
          case 'h': ShowUsage(argv[0]); return CLArgFlag::QuitSuccessfully;
          case 'e': debug::ct_eval           = true; break;
          case 'p': debug::parser            = true; break;
          case 's': debug::parametric_struct = true; break;
          case 't': debug::timer             = true; break;
          case '\0': goto next_arg;
          default: ShowUsage(argv[0]); return CLArgFlag::QuitWithFailure;
          } 
        }

      }
    } else {
      /* File to parse */
      file_queue.emplace(arg);
    }
  next_arg:;
  }

  if (file_queue.empty()) {
    ShowUsage(argv[0]);
    return CLArgFlag::QuitWithFailure;
  }

  return CLArgFlag::Continue;
}



#endif // ICARUS_CLARGS_H
