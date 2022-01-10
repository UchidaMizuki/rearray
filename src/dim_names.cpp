#include <set>
#include <algorithm>
#include <string>
#include <cpp11.hpp>
using namespace cpp11;
namespace writable = cpp11::writable;

[[cpp11::register]]
list union_dim_names(list x, list y, bool only_x) {
  return x;
}

[[cpp11::register]]
strings cpp_unique(strings xs) {
  std::set<std::string> set_xs(xs.begin(), xs.end());
  int n = set_xs.size();

  auto it = set_xs.begin();
  writable::strings out(n);

  for(int i=0; i<n; ++i) {
    out[i] = *it++;
  }
  return out;
}

[[cpp11::register]]
strings cpp_unique2(strings xs) {
  std::set<std::string> set_xs(xs.begin(), xs.end());
  int n = set_xs.size();

  auto it = set_xs.begin();
  writable::strings out(n);

  for(int i=0; i<n; ++i) {
    out[i] = *it++;
  }
  return out;
}
