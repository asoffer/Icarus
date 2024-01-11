#include "common/interface.h"

#include <deque>

namespace ic {
namespace {

struct InterfaceData {};

std::deque<InterfaceData> interfaces;

}  // namespace

Interface::Interface(construct_new_t)
    : StrongIdentifierType(interfaces.size()) {
  interfaces.emplace_back();
}

}  // namespace ic
