#' @export
broadcast <- function(x, dim_names) {
  old_dim_names <- dimnames(x)

  if (identical(old_dim_names, dim_names)) {
    x
  } else {
    old_axes <- names(old_dim_names)
    new_axes <- names(dim_names)

    stopifnot(
      old_axes %in% new_axes
    )

    old_axes <- intersect(new_axes, old_axes)
    old_dim_names <- old_dim_names[old_axes]
    old_dim <- list_sizes(old_dim_names)

    # permutation
    x <- aperm(x, old_axes)

    if (identical(old_dim_names, dim_names)) {
      x
    } else {
      # set dimensions
      new_dim <- vec_rep(1, vec_size(new_axes))
      names(new_dim) <- new_axes
      new_dim[old_axes] <- old_dim
      dim(x) <- new_dim

      # subsetting
      old_dim_names <- old_dim_names[new_axes]
      size_axes <- vec_size(new_axes)
      loc <- vector("list", size_axes)

      for (axis in seq_len(size_axes)) {
        dim_name <- dim_names[[axis]]
        old_dim_name <- old_dim_names[[axis]]

        if (is.null(old_dim_name)) {
          loc[[axis]] <- vec_rep(1, vec_size(dim_name))
        } else {
          loc[[axis]] <- vec_match(dim_name, old_dim_name)
        }
      }

      x <- exec(`[`, x, !!!loc)

      dimnames(x) <- dim_names
      new_rearray(x)
    }
    }
}

union_dim_names <- function(x, y) {
  if (identical(x, y)) {
    x
  } else {
    axes <- union(names(x), names(y))
    out <- lapply(axes,
                  function(axis) {
                    union(x[[axis]], y[[axis]])
                  })
    names(out) <- axes
    out
  }
}

#' @export
Ops.rearray <- function(e1, e2) {
  if (length(e1) != 1 && length(e2) != 1) {
    dim_names_e1 <- dimnames(e1)
    dim_names_e2 <- dimnames(e2)

    dim_names <- union_dim_names(dim_names_e1, dim_names_e2)

    e1 <- broadcast(e1, dim_names)
    e2 <- broadcast(e2, dim_names)
  }
  NextMethod()
}
