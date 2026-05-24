##
## SIMPLE + CRD: completely randomized design
##
## For each factor: fe = effects (one value per level), then fl = level labels.
## Use the same list name in fe and fl (here: trt = treatment).
##

library(gexp)

crd <- gexp(mu     = 15,
            err    = matrix(0,
                            nrow = 15,
                            ncol = 1),
            r      = 5,
            fe     = list(trt = c(1, 2, 3)),
            fl     = list(trt = paste0('t', 1:3)),
            type   = "SIMPLE",
            design = "CRD")

print(crd)
summary(crd)

if (interactive()) {
  plot(crd)
}
