## One short two-species FF16 run carried in birth date, shared by the tests.
ff16_run <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      p <- plant::scm_base_parameters("FF16")
      p$max_patch_lifetime <- 40
      p <- plant::add_strategies(p, plant::trait_matrix(c(0.0825, 0.2), "lma"),
                                 birth_rate = c(20, 20))
      cache <<- plant::run_scm(
        p, ctrl = plant::control(node_density_coordinate = "birth_date"),
        collect = TRUE)
    }
    cache
  }
})
