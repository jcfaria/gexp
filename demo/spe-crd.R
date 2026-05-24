##
## SPE + CRD: split-plot, completely randomized design
##
## Whole-plot factor (main): fe then fl. Sub-plot factor (sub): fe then fl.
##

library(gexp)

spe_crd <- gexp(mu     = 20,
                err    = matrix(0,
                                nrow = 12,
                                ncol = 1),
                r      = 2,
                fe     = list(main = c(2, 0, -2),
                              sub  = c(1, -1)),
                fl     = list(main = paste0('p', 1:3),
                              sub  = paste0('sp', 1:2)),
                type   = "SPE",
                design = "CRD")

print(spe_crd)
summary(spe_crd)

if (interactive()) {
  plot(spe_crd)
}
