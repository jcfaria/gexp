##
## SPE + LSD: split-plot, Latin square design (r is set to 1 internally)
##

library(gexp)

spe_lsd <- gexp(mu     = 20,
                err    = matrix(0, nrow = 18, ncol = 1),
                r      = 1,
                type   = "SPE",
                design = "LSD")

print(spe_lsd)
summary(spe_lsd)

if (interactive()) {
  plot(spe_lsd)
}
