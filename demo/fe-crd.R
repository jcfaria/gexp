##
## FE + CRD: 2 x 2 factorial, completely randomized design
##

library(gexp)

fe_crd <- gexp(mu     = 15,
               err    = matrix(0, nrow = 8, ncol = 1),
               r      = 2,
               fl     = list(f1 = c("A", "B"),
                             f2 = c("P", "Q")),
               fe     = list(f1 = c(1, -1),
                             f2 = c(2, -2)),
               type   = "FE",
               design = "CRD")

print(fe_crd)
summary(fe_crd)

if (interactive()) {
  plot(fe_crd)
}
