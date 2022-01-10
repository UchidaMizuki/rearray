#include <vector>
#include <cpp11.hpp>
using namespace cpp11;

[[cpp11::register]]
list_of<integers> match_dim_names(list new_dim_names, list old_dim_names) {
  int axes = new_dim_names.size();
  writable::list_of<writable::integers> out_axes(axes);

  for(int axis=0; axis<axes; ++axis) {
    strings new_dim_name = new_dim_names[axis];
    int coords = new_dim_name.size();
    writable::integers out_coords(coords);

    if (old_dim_names[axis] == R_NilValue) {
      out_coords = std::vector<int>(coords, 1);
    } else {
      strings old_dim_name = old_dim_names[axis];

      for (int coord=0; coord<coords; ++coord) {
        auto itr = std::find(old_dim_name.begin(), old_dim_name.end(), new_dim_name[coord]);

        if (itr == old_dim_name.end()) {
          out_coords[coord] = NA_INTEGER;
        } else {
          out_coords[coord] = std::distance(old_dim_name.begin(), itr) + 1;
        }
      }
    }
    out_axes[axis] = out_coords;
  }
  return out_axes;
}
