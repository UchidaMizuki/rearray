#' @export
broadcast <- function(x, dim_names) {
  old_dim_names <- dimnames(x)
  old_dim <- list_sizes(old_dim_names)
  old_axes <- names(old_dim_names)

  new_axes <- names(dim_names)

  # permutation
  x <- aperm(x, intersect(new_axes, old_axes))

  # set dimensions
  new_dim <- vec_rep(1, vec_size(new_axes))
  names(new_dim) <- new_axes
  new_dim[old_axes] <- old_dim

  dim(x) <- new_dim

  # subsetting
  loc <- purrr::modify2(dim_names, new_axes,
                        function(dim_name, axis) {
                          old_dim_name <- old_dim_names[axis]

                          if (is.null(old_dim_name)) {
                            vec_match(dim_name, old_dim_name)
                          } else {
                            vec_rep(1, vec_size(dim_name))
                          }
                        })
  x <- exec(`[`, x, !!!loc)

  dimnames(x) <- dim_names
  new_rearray(x)
}

union_dim_names <- function(x, y) {
  axes <- union(names(x), names(y))
  out <- purrr::map(axes,
                    function(axis) {
                      union(x[[axis]], y[[axis]])
                    })
  names(out) <- axes
  out
}
