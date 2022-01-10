test_that("multiplication works", {
  e1 <- rearray(1:1e4, list(axis1 = 1:1e2, axis2 = 1:1e2))
  e2 <- rearray(1:1e4, list(axis2 = 1:1e2, axis3 = 1:1e2))

  dim_names <- list(axis1 = as.character(1:1e2), axis2 = as.character(1:1e2), axis3 = as.character(1:1e2))
  e1_1 <- broadcast(e1, dim_names)
  e2_1 <- broadcast(e2, dim_names)

  profvis::profvis({
    for (i in 1:1e2) {
      e1 * e2
    }
  })
  profvis::profvis({
    for (i in 1:1e2) {
      e1_1 * e2_1
    }
  })
})
