new_rearray <- function(data) {
  structure(data,
            class = c("rearray", class(data)))
}

#' @export
rearray <- function(data, dim_names) {
  stopifnot(
    is_named(dim_names),
    !purrr::map_lgl(dim_names,
                    function(x) {
                      vec_duplicate_any(x) && !any(is.na(x))
                    })
  )

  dim <- list_sizes(dim_names)
  size <- prod(dim)

  data <- vec_recycle(data, size)
  data <- array(data, dim, dim_names)

  new_rearray(data)
}

#' @export
is_rearray <- function(x) {
  inherits(x, "rearray")
}

#' @export
rearray_by <- function() {
  # TODO
  stop()
}

#' @export
`[.rearray` <- function(x, ...) {
  out <- NextMethod()

  if (is.array(out)) {
    out <- new_rearray(out)
  }
  out
}

#' @export
as.array.rearray <- function(x, ...) {
  unclass(x)
}

#' @export
print.rearray <- function(x, ...) {
  print(as.array(x), ...)
  cli::cat_line()
  glimpse(x)
  invisible(x)
}

#' @importFrom pillar glimpse
#' @export
glimpse.rearray <- function(x, width = NULL, ...) {
  width <- width %||% getOption("width")

  dim_names <- dimnames(x)
  axes <- pillar::align(c("A rearray: ",
                          stringr::str_c("$ ",
                                         pillar::align(names(dim_names)), " ",
                                         purrr::map_chr(dim_names, pillar::size_sum), ": ")))

  width <- pmax(3, width - utf8::utf8_width(axes[[1]]))
  coords <- stringr::str_trunc(c(pillar::obj_sum(as.array(x)),
                                 purrr::map_chr(dim_names, commas)),
                               width)

  cli::cat_line(axes, coords)

  invisible(x)
}
