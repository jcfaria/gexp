##
## SIMPLE + LSD: Latin square design (r is set to 1 internally)
##

library(gexp)

lsd <- gexp(mu     = 12,
            err    = matrix(0, nrow = 9, ncol = 1),
            r      = 1,
            fe     = list(trt = c(1, 0, -1)),
            type   = "SIMPLE",
            design = "LSD")

print(lsd)
summary(lsd)

if (interactive()) {
  plot(lsd)
}
