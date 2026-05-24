##
## SPE + RCBD: split-plot, randomized complete block design
##

library(gexp)

spe_rcbd <- gexp(mu     = 20,
                 err    = matrix(0, nrow = 36, ncol = 1),
                 r      = 2,
                 type   = "SPE",
                 design = "RCBD")

print(spe_rcbd)
summary(spe_rcbd)

if (interactive()) {
  plot(spe_rcbd)
}
