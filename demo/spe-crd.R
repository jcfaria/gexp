##
## SPE + CRD: split-plot, completely randomized design (default 2 x 3 layout)
##

library(gexp)

spe_crd <- gexp(mu     = 20,
                err    = matrix(0, nrow = 12, ncol = 1),
                r      = 2,
                type   = "SPE",
                design = "CRD")

print(spe_crd)
summary(spe_crd)

if (interactive()) {
  plot(spe_crd)
}
