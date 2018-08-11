#ifndef ICARUS_RUN_H
#define ICARUS_RUN_H

#include <future>
#include <memory>

#include "base/container/unordered_map.h"
#include "base/guarded.h"
#include "frontend/source.h"

struct Module;

void ScheduleModule(const frontend::Source::Name &src);

extern base::guarded<base::unordered_map<
    frontend::Source::Name, std::shared_future<std::unique_ptr<Module>>>>
    modules;

int RunRepl();
int RunCompiler();

#endif  // ICARUS_RUN_H
