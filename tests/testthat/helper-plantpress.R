## Short runs carried in birth date, one per model, shared by the tests.
birth_date_run <- function(model, traits, lifetime, birth_rate = 20, ...) {
  p <- plant::default_parameters(model, quiet = TRUE)
  p$max_patch_lifetime <- lifetime
  p <- plant::add_strategies(p, traits, birth_rate = rep(birth_rate, nrow(traits)))
  plant::run(p, control = plant::control(node_density_coordinate = "birth_date", ...),
             record = "steps")
}
cached <- function(make) {
  cache <- NULL
  function() {
    if (is.null(cache)) cache <<- make()
    cache
  }
}
## Two FF16 species, tidied, as most tests read them.
ff16_run <- cached(function() {
  plant::tidy(birth_date_run("FF16", plant::trait_matrix(c(0.0825, 0.2), "lma"), 40))
})
## TF24 and K93 kept as plant_runs, which realise_stand() also takes.
tf24_run <- cached(function() {
  birth_date_run("TF24", plant::trait_matrix(0.07, "lma"), 3)
})
k93_run <- cached(function() {
  birth_date_run("K93", plant::trait_matrix(c(0.059, 0.08), "b_0"), 40)
})
