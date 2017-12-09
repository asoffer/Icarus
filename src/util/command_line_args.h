#ifndef ICARUS_CLARGS_H
#define ICARUS_CLARGS_H

#include <queue>
#include "base/source.h"

enum class FileType { Bin, IR, Nat, None };

namespace debug {
extern bool parser;
extern bool ct_eval;
extern bool no_validation;
extern bool timer;
} // namespace debug

enum class CLArgFlag { QuitSuccessfully, QuitWithFailure, Continue };

FileType file_type = FileType::Bin;
const char *output_file_name = "a.out";
std::queue<Source::Name> file_queue;

static void
ShowUsage(char *argv0) {
  fprintf(stderr,
          R"(Usage: %s [options] input_file... -o output_file

  -o output_file                 Default is a.out

  -e, --eval                     Run compile-time evaluator step-by-step (debug).

  --file-type=[ir|nat|bin|none]  Output a file of the specified type:
                                   ir   - LLVM intermediate representation
                                   nat  - Output a native object file
                                   bin  - Output a single native object file and
                                          link it (requires gcc)
                                   none - Do not write any files (debug)
                                          This is the default option.

  -h, --help                     Display this usage message.

  -n, --no-validation            Do not run property validation

  -p, --parser                   Display step-by-step file parsing (debug).

  -r, --repl                     Invoke Icarus Read-Eval-Print-Loop

  -t, --timer                    Display timing information for each of the
                                 compilation steps (debug).

)",
          argv0);
}

bool repl = false;
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

            } else if (strcmp(ptr, "none") == 0) {
              file_type = FileType::None;
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

          } else if (strcmp(arg + 2, "no-validation") == 0) {
            debug::no_validation = true;
            goto next_arg;

          } else if (strcmp(arg + 2, "parser") == 0) {
            debug::parser = true;
            goto next_arg;

          } else if (strcmp(arg + 2, "timer") == 0) {
            debug::timer = true;
            goto next_arg;

          } else if (strcmp(arg + 2, "repl") == 0) {
            repl = true;
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
          case 'n': debug::no_validation     = true; break;
          case 'p': debug::parser            = true; break;
          case 'r': repl                     = true; break;
          case 't': debug::timer             = true; break;
          case '\0': goto next_arg;
          default: ShowUsage(argv[0]); return CLArgFlag::QuitWithFailure;
          } 
        }

      }
    } else {
      file_queue.push(Source::Name(arg));
    }
  next_arg:;
  }

  if (file_queue.empty() && !repl) {
    ShowUsage(argv[0]);
    return CLArgFlag::QuitWithFailure;
  }

  return CLArgFlag::Continue;
}

#endif // ICARUS_CLARGS_H
