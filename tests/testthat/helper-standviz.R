## One short two-species FF16 run carried in birth date, shared by the tests.
ff16_run <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      p <- plant::default_parameters("FF16", quiet = TRUE)
      p$max_patch_lifetime <- 40
      p <- plant::add_strategies(p, plant::trait_matrix(c(0.0825, 0.2), "lma"),
                                 birth_rate = c(20, 20))
      cache <<- plant::tidy(plant::run(
        p, control = plant::control(node_density_coordinate = "birth_date"),
        record = "steps"))
    }
    cache
  }
})
