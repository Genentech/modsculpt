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
  expect_s3_class(gi$perc, "ggplot")

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

test_that("ms_color and facet utilities behave as expected", {
  expect_equal(ms_color(3), c("#0a0a0a", "#14a3a8", "#e3211d"))
  expect_length(ms_color(8, hue_coloring = TRUE), 8)
  expect_false(identical(ms_color(5), ms_color(5, hue_coloring = TRUE)))

  sc <- fixture_sculpture()
  expect_error(
    modsculpt:::resolve_facet_specification(sc, facet_specification(labels = c(foo = "Foo"))),
    "len"
  )

  nc <- modsculpt:::resolve_facet_ncol(idx_c = c(TRUE, FALSE, TRUE), facet_ncol = 2)
  expect_equal(nc$ncol_c, 2)
  expect_equal(nc$ncol_d, 1)
})

test_that("resolve_missings_specification drops rows when requested", {
  dat_c <- data.table::data.table(
    feature = factor("mpg", levels = "mpg"),
    x = c(1, 2),
    y = c(0.1, 0.2),
    line_id = c("pdp", "1")
  )
  miss_spec <- missings_specification(values = 1, drop_from_plot = TRUE)
  missings <- data.table::data.table(feature = factor("mpg", levels = "mpg"), x = 1, y = 0.1)

  res <- suppressWarnings(modsculpt:::resolve_missings_specification(dat_c = dat_c, ms = miss_spec, missings = missings))
  expect_equal(nrow(res$dat_c), 1)
  expect_equal(res$missings$x, 1)
})

test_that("g_var_imp handles PDP view, top_k, and log-odds sculptures", {
  sc <- fixture_sculpture()
  expect_s3_class(g_var_imp(sc, show_pdp_plot = TRUE, plot_ratios = c(1, 1, 1)), "gtable")
  expect_s3_class(g_var_imp(sc, show_pdp_plot = FALSE, top_k = 10), "gtable")

  wf <- build_classification_workflow()
  suppressWarnings({
    expect_s3_class(
      g_var_imp(wf$rough, show_pdp_plot = FALSE, var_imp_type = "ice", logodds_to_prob = TRUE),
      "gtable"
    )
    expect_s3_class(
      g_var_imp(
        wf$rough,
        show_pdp_plot = TRUE,
        logodds_to_prob = TRUE,
        plot_ratios = c(2, 1, 1)
      ),
      "gtable"
    )
  })
})

test_that("g_component adds missings overlays when requested", {
  wf <- build_regression_workflow()
  miss_spec <- missings_specification(
    values = wf$train$mpg[1],
    vline = TRUE,
    hline = TRUE
  )
  comp <- g_component(wf$rough, missings_spec = miss_spec)
  expect_s3_class(comp$continuous, "ggplot")
})

test_that("g_comparison supports hue colouring and missings overlays", {
  wf <- build_regression_workflow()
  miss_spec <- missings_specification(
    values = wf$train$mpg[1],
    vline = TRUE,
    hline = TRUE
  )
  comp <- g_comparison(
    sculptures = list(wf$rough, wf$polished),
    descriptions = c("Rough", "Polished"),
    missings_spec = miss_spec,
    hue_coloring = TRUE
  )
  expect_s3_class(comp$continuous, "ggplot")
})

test_that("g_additivity returns ggplot when plot_only", {
  wf <- build_regression_workflow()
  preds <- predict(wf$rough, wf$train[wf$features])
  add_plot <- g_additivity(
    sp = preds,
    lp = predict(wf$model, newdata = wf$train[wf$features])
  )
  expect_s3_class(add_plot, "ggplot")
})
