# profile_cold_start.R — where the seconds go when an app worker starts cold.
#
#   docker cp tools/profile_cold_start.R rstudio:/tmp/
#   docker exec -u 1000:1000 -e PROFILE_VERS=v8,v7 rstudio Rscript /tmp/profile_cold_start.R scores
#
# Measured on msens1, 2026-08-27 (v8):
#
#                        scores    species
#   packages              8.6 s      8.3 s   <- dominates, and is irreducible:
#   other globals         1.0 s        -        library(msens) costs 8.1 s when
#   build_bundle(v8)      3.2 s      8.8 s     loaded FIRST but only 0.38 s when
#   build_bundle(v7)      1.0 s        -       loaded LAST, i.e. it is the shared
#   ---------------------------------------    sf/terra/mapgl/duckdb stack that
#   first page           12.8 s     17+ s      the apps attach anyway.
#
# So the lever is NOT trimming packages (~0.4 s available) and NOT caching the
# bundle (worth 3.0 s / 5.7 s — see the note in README.md); it is keeping the
# worker alive, which `app_idle_timeout` in server/rstudio/shiny-server.conf now
# does. Re-run this after any change that adds a dependency or grows a bundle.

# Profile a cold app start: packages vs globals vs per-version bundle.
app <- commandArgs(TRUE)[1]                      # "scores" | "species"
dir <- file.path("/srv/shiny-server", app)
setwd(dir)
src <- readLines("app.R")

# the package block: from `librarian::shelf(` to its closing paren
i0 <- grep("^librarian::shelf\\(", src)[1]
depth <- 0; i1 <- i0
for (i in seq(i0, length(src))) {
  depth <- depth + lengths(regmatches(src[i], gregexpr("\\(", src[i]))) -
                   lengths(regmatches(src[i], gregexpr("\\)", src[i])))
  if (depth <= 0) { i1 <- i; break }
}
el <- function(expr) round(system.time(force(expr))[["elapsed"]], 2)

t_pkg <- el(eval(parse(text = paste(src[i0:i1], collapse = "\n")), envir = globalenv()))

# the rest of the global section (everything except the package block); sourcing
# app.R also defines ui/server and builds the shinyApp object, but does not run it
rest <- src[-(i0:i1)]
f <- tempfile(fileext = ".R"); writeLines(rest, f)
t_glob <- el(sys.source(f, envir = globalenv()))

vers <- strsplit(Sys.getenv("PROFILE_VERS", "v8,v7"), ",")[[1]]
t_bundle <- setNames(numeric(length(vers)), vers)
for (v in vers) t_bundle[[v]] <- el(build_bundle(v))
# second call of the first version: proves the in-process cache
t_again <- el(bundle(vers[1]))

cat(sprintf("\n== %s cold start ==\n", app))
cat(sprintf("  packages (librarian::shelf) : %6.2f s\n", t_pkg))
cat(sprintf("  other globals              : %6.2f s\n", t_glob))
for (v in vers) cat(sprintf("  build_bundle(%-3s)          : %6.2f s\n", v, t_bundle[[v]]))
cat(sprintf("  bundle(%s) again (cached)   : %6.2f s\n", vers[1], t_again))
cat(sprintf("  TOTAL first page (%s)       : %6.2f s\n", vers[1], t_pkg + t_glob + t_bundle[[vers[1]]]))
