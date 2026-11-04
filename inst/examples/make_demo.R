## Demo: a 100-year FF16 stand of two species that differ in leaf mass per
## area: a cheap-leaved pioneer and a slower, shade-tolerant successor.
## Usage: Rscript inst/examples/make_demo.R [output.html]
library(plant)
library(standviz)

out <- commandArgs(trailingOnly = TRUE)[1]
if (is.na(out)) out <- "ff16-two-species.html"

p <- scm_base_parameters("FF16")
p$max_patch_lifetime <- 100
p <- add_strategies(p, trait_matrix(c(0.0825, 0.2), "lma"),
                    birth_rate = c(20, 20))
results <- run_scm(p, collect = TRUE)

stand <- realise_stand(results, side = 20, min_height = 1)
print(stand)
write_viewer(stand, out, title = "Pioneer to successor")
message("Wrote ", out)
