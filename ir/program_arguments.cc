#include "ir/program_arguments.h"

#include "absl/synchronization/mutex.h"
#include "common/slice.h"
#include "nth/utility/no_destructor.h"

namespace ic {
namespace {

absl::Mutex program_arguments_mutex;
nth::NoDestructor<std::vector<std::string>> program_arguments_holder;
nth::NoDestructor<std::vector<Slice>> program_arguments;

}  // namespace

void SetProgramArguments(std::vector<std::string> arguments) {
  absl::MutexLock lock(&program_arguments_mutex);
  *program_arguments_holder = std::move(arguments);
  auto& pa = *program_arguments;
  pa.clear();
  pa.reserve(program_arguments_holder->size());
  for (auto const& argument : *program_arguments_holder) {
    pa.emplace_back(reinterpret_cast<std::byte const*>(argument.data()),
                    argument.size());
  }
}

Slice ProgramArguments() {
  absl::MutexLock lock(&program_arguments_mutex);
  return Slice(reinterpret_cast<std::byte const*>(program_arguments->data()),
               program_arguments->size());
}

}  // namespace ic
