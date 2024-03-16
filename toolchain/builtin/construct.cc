#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "common/to_bytes.h"
#include "ir/serialize.h"
#include "nth/commandline/commandline.h"
#include "nth/debug/log/log.h"
#include "nth/io/file_path.h"
#include "nth/io/serialize/serialize.h"
#include "nth/io/writer/file.h"
#include "nth/io/writer/string.h"
#include "nth/io/writer/writer.h"
#include "nth/process/exit_code.h"
#include "nth/utility/early_exit.h"
#include "toolchain/builtin/module.h"

namespace ic::builtin {
namespace {

// Constructs a serialization of the builtin module, writing it the file named
// by `path`.
nth::exit_code WriteBuiltinToFile(nth::FlagValueSet,
                                  nth::file_path const& path);

}  // namespace
}  // namespace ic::builtin

nth::Usage const nth::program_usage = {
    .description =
        "Constructs the Icarus Builtin Module accessible via the `builtin` "
        "keyword.",
    .execute = ic::builtin::WriteBuiltinToFile,
};

namespace ic::builtin {
namespace {

auto FatalError(std::string_view message) {
  return [=] {
    NTH_LOG("Fatal internal error: {}") <<= {message};
    return nth::exit_code::generic_error;
  };
}

nth::exit_code WriteBuiltinToFile(nth::FlagValueSet,
                                  nth::file_path const& path) {
  absl::InitializeSymbolizer("");
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  nth::io::file_writer writer = co_await nth::on_exit(
      nth::io::file_writer::try_open(path, nth::io::file_writer::create),
      FatalError("Failed to open file"));

  // TODO: Use a buffered file writer directly.
  std::string content;

  Module builtin;
  PopulateModule(builtin);
  ModuleSerializer<nth::io::string_writer> s(content, shared_context);
  co_await nth::on_exit(nth::io::serialize(s, builtin),
                        FatalError("Failed to write module contents"));

  co_await nth::on_exit(writer.write(ToBytes(content)),
                        FatalError("Failed to write module contents"));

  co_return nth::exit_code::success;
}

}  // namespace
}  // namespace ic::builtin
