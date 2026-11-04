## One short two-species FF16 run, shared by the tests.
ff16_run <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      p <- plant::scm_base_parameters("FF16")
      p$max_patch_lifetime <- 40
      p <- plant::add_strategies(p, plant::trait_matrix(c(0.0825, 0.2), "lma"),
                                 birth_rate = c(20, 20))
      cache <<- plant::run_scm(p, collect = TRUE)
    }
    cache
  }
})

## The model's number of plants per m^2 taller than h_min at the solver step
## nearest time t, taking density log-linear in height between nodes (as
## realise_stand() does) and counting the part of an interval above h_min.
model_count_above <- function(results, species, t, h_min) {
  d <- results$species[results$species$species == species, ]
  t0 <- d$time[which.min(abs(d$time - t))]
  a <- d[d$time == t0, ]
  a <- a[order(a$node), ]
  total <- 0
  for (i in seq_len(nrow(a) - 1)) {
    h1 <- a$height[i]
    h2 <- a$height[i + 1]
    if (h1 <= h_min) next
    slope <- log(max(a$density[i + 1], 1e-300) / max(a$density[i], 1e-300))
    v_max <- if (h2 >= h_min) 1 else (h1 - h_min) / (h1 - h2)
    share <- if (abs(slope) < 1e-8) v_max else expm1(slope * v_max) / slope
    total <- total + (h1 - h2) * a$density[i] * share
  }
  total
}
