#include "absl/debugging/failure_signal_handler.h"
#include "absl/debugging/symbolize.h"
#include "common/constant/manifest.h"
#include "common/result.h"
#include "common/to_bytes.h"
#include "nth/commandline/commandline.h"
#include "nth/io/deserialize/deserialize.h"
#include "nth/io/file_path.h"
#include "nth/io/reader/file.h"
#include "nth/io/reader/reader.h"
#include "nth/numeric/integer.h"
#include "nth/process/exit_code.h"
#include "nth/utility/early_exit.h"
#include "type/pointer.h"

namespace ic {
namespace {

// Constructs a serialization of the builtin module, writing it the file named
// by `path`.
nth::exit_code DumpIcm(nth::FlagValueSet, nth::file_path const& path);

}  // namespace
}  // namespace ic::builtin

nth::Usage const nth::program_usage = {
    .description =
        "Shows human-readable debug information for a variety of objects.",
    .commands =
        {
            {
                .name        = "icm",
                .description = "Dumps the contents of a `.icm` file",
                .execute = ic::DumpIcm,

            },
        },
};

namespace ic {
namespace {

#if 0
struct ShowTypes {
  using iterator = decltype(GlobalConstantTable().begin());
  ShowTypes(iterator b, iterator e) : b_(b), e_(e) {}

  friend void NthPrint(auto& p, auto& f, ShowTypes st) {
    size_t i = 0;
    auto iter = st.b_;
    NTH_LOG("{}")<<={st.e_ - st.b_};
    while (iter != st.e_) {
      iter                  = st.b_ + i;
      auto const& component = *iter;
      p.write("\n    ");
      f(p, i);
      p.write(".  ");
      switch (component.category()) {
        case internal_constants::Component::Category::PrimitiveType:
          f(p, type::PrimitiveType(
                   static_cast<type::PrimitiveType::Kind>(component.value())));
          ++i;

          break;
        case internal_constants::Component::Category::String:
          ++i;
          continue;
        case internal_constants::Component::Category::ParametersType: {
          size_t count = component.value();
          ++iter;
          p.write("parameters(");
          std::string_view separator = "";
          for (size_t j = 0; j < count; ++j) {
            p.write(std::exchange(separator, ", "));
            f(p, Identifier::FromRepresentation((iter + 2 * j)->value()));
            p.write(": @");
            f(p, (iter + 2 * j + 1)->value());
          }
          p.write(")");
          i += 2 * count + 1;
        } break;
        case internal_constants::Component::Category::FunctionType: {
          size_t return_count = component.value();
          i += return_count + 3;
          ++iter;
          p.write("@");
          f(p, iter->value());
          p.write(" -> ");
          if (return_count == 0) {
            p.write("()");
            break;
          }
          std::string_view separator = "(@";
          iter += 2;  // Show evaluation model?
          auto end = iter + return_count;
          for (; iter != end; ++iter) {
            p.write(std::exchange(separator, ", @"));
            f(p, iter->value());
          }
          p.write(")");
        } break;
        case internal_constants::Component::Category::SliceType: {
          p.write("\\@");
          f(p, iter->value());
          ++i;
        } break;
        case internal_constants::Component::Category::BufferPointerType: {
          p.write("[*]@");
          f(p, iter->value());
          ++i;
        } break;
        case internal_constants::Component::Category::PointerType: {
          p.write("*@");
          f(p, iter->value());
          ++i;
        } break;
        case internal_constants::Component::Category::Followup:
          for (auto iter = st.b_; iter != st.e_; ++iter) {
            NTH_LOG("{}") <<= {*iter};
          }
          NTH_UNREACHABLE();
        default:
          NTH_LOG("  {}.  {}") <<= {i, (int)component.category()};
          return;
          break;
      }
    }
  }

 private:
  iterator b_, e_;
};
#endif

struct IcmLogger {
  friend Result NthDeserialize(auto& d, IcmLogger&) {
    co_await MergeConstantManifest(d);

    NTH_LOG(
        "\n"
        "  Identifiers: {}\n"
        "  Integers:    {}\n"
        "  Strings:     {}\n"
        "  Types:\n"
        "    Pointers: {}\n"
        "    BufferPointers: {}\n"
        "    Slices: {}\n") <<= {
        internal_common::Identifiers(), internal_common::Integers(),
        internal_common::Strings(),     type::Pointers(),
        type::BufferPointers(),         type::Slices(),
    };
    // co_await nth::io::deserialize(d, foreign);
    // co_await nth::io::deserialize(d, module.program());
    // co_return nth::io::deserialize(d, module.entries());
    co_return Result::success();
  }
};

auto FatalError(std::string_view message) {
  return [=] {
    NTH_LOG("Fatal internal error: {}") <<= {message};
    return nth::exit_code::generic_error;
  };
}

nth::exit_code DumpIcm(nth::FlagValueSet, nth::file_path const& path) {
  absl::InitializeSymbolizer("");
  absl::FailureSignalHandlerOptions opts;
  absl::InstallFailureSignalHandler(opts);

  nth::io::file_reader reader = co_await nth::on_exit(
      nth::io::file_reader::try_open(path), FatalError("Failed to open file."));

  IcmLogger logger;
  co_await nth::on_exit(nth::io::deserialize(reader, logger),
                        FatalError("Failed to read file contents."));

  co_return nth::exit_code::success;
}

}  // namespace
}  // namespace ic
