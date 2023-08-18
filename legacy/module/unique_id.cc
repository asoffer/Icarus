#include "module/unique_id.h"

#include "absl/container/node_hash_set.h"
#include "absl/synchronization/mutex.h"
#include "nth/utility/no_destructor.h"

namespace module {
namespace {

std::string const InvalidString;
std::string const BuiltinString("~builtin~");
std::string const SelfString("~self~");

absl::Mutex mutex;
nth::NoDestructor<absl::node_hash_set<std::string>> intern_set;

}  // namespace

UniqueId UniqueId::Invalid() { return UniqueId(&InvalidString); }
UniqueId UniqueId::Builtin() { return UniqueId(&BuiltinString); }
UniqueId UniqueId::Self() { return UniqueId(&SelfString); }

UniqueId::UniqueId(std::string&& value)
    : value_((absl::MutexLock(&mutex),
              &*intern_set->emplace(std::move(value)).first)) {}

UniqueId::UniqueId(std::string_view value)
    : value_((absl::MutexLock(&mutex), &*intern_set->emplace(value).first)) {}

}  // namespace module
