##
## SIMPLE + CRD: completely randomized design
##

library(gexp)

crd <- gexp(mu     = 15,
            err    = matrix(0, nrow = 6, ncol = 1),
            r      = 3,
            fe     = list(alpha = c(1, -2)),
            type   = "SIMPLE",
            design = "CRD")

print(crd)
summary(crd)

if (interactive()) {
  plot(crd)
}
