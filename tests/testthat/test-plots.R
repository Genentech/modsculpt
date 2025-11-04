skip_on_cran()

test_that("facet_specification validates inputs", {
  fs_default <- facet_specification()
  expect_s3_class(fs_default, "facet_specification")
  expect_equal(fs_default$sort, "alphabetical")
  expect_error(
    facet_specification(sort = "unknown"),
    "subset"
  )
  expect_error(
    facet_specification(top_k = 2, subset = "mpg"),
    "either `top_k` or `subset`"
  )
})

test_that("resolve_facet_specification respects ordering and top_k", {
  sc <- fixture_sculpture()
  fs <- facet_specification(sort = "importance", top_k = 2)

  resolved <- modsculpt:::resolve_facet_specification(sc, fs)

  expect_equal(names(resolved$object), c("mpg", "wt"))
  expect_equal(resolved$ncol_c, 2)
  expect_equal(resolved$ncol_d, 0)
  expect_named(resolved$labels)
})

test_that("missings_specification guards conflicting options", {
  expect_error(
    missings_specification(vline = TRUE),
    "no missing values provided"
  )
  expect_error(
    missings_specification(drop_from_plot = TRUE),
    "no missing values provided"
  )
  expect_error(
    missings_specification(vline = TRUE, drop_from_plot = TRUE, values = -1),
    "either `drop_from_plot` or `vline`"
  )

  sc <- fixture_sculpture()
  ms <- missings_specification(values = -1, drop_from_plot = TRUE)
  gi <- suppressWarnings(g_ice(sc, missings_spec = ms, show_PDP = FALSE))
  expect_s3_class(gi$continuous, "ggplot")
})

test_that("g_ice builds plots and enforces PDP colouring guard", {
  sc <- fixture_sculpture()

  gi <- g_ice(
    sc,
    centered = FALSE,
    show_PDP = FALSE,
    coloured = TRUE,
    facet_spec = facet_specification(top_k = 2)
  )
  expect_s3_class(gi$continuous, "ggplot")
  expect_s3_class(gi$discrete, "ggplot")

  expect_error(
    g_ice(sc, coloured = TRUE),
    "Coloured lines are only available without PDP"
  )
})

test_that("g_comparison overlays multiple sculptures", {
  sc1 <- fixture_sculpture()
  sc2 <- fixture_sculpture_alt()

  comp <- g_comparison(
    sculptures = list(sc1, sc2),
    descriptions = c("Base", "Alt"),
    facet_spec = facet_specification(sort = "alphabetical")
  )

  expect_s3_class(comp$continuous, "ggplot")
  expect_s3_class(comp$discrete, "ggplot")
})

test_that("g_var_imp supports multiple variable-importance modes", {
  sc <- fixture_sculpture()

  expect_warning(
    norm_plot <- g_var_imp(sc, show_pdp_plot = FALSE, var_imp_type = "normalized"),
    "Removed 1 row",
    fixed = FALSE
  )
  expect_s3_class(norm_plot, "gtable")
  expect_s3_class(
    g_var_imp(sc, show_pdp_plot = FALSE, var_imp_type = "absolute"),
    "gtable"
  )
  expect_s3_class(
    g_var_imp(sc, show_pdp_plot = FALSE, var_imp_type = "ice_orig_mod"),
    "gtable"
  )
  expect_s3_class(
    g_var_imp(sc, show_pdp_plot = FALSE, var_imp_type = "ice"),
    "gtable"
  )

  ds <- sculpt_detailed_gam(sc)
  expect_error(
    g_var_imp(ds, show_pdp_plot = FALSE, var_imp_type = "ice_orig_mod"),
    "only valid for a rough sculpture"
  )
})
