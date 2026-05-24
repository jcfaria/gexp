##
## FE + CRD: 3 x 3 factorial, completely randomized design
##
## Two factors: fe (effects) before fl (labels) for each; same name in both lists.
## 9 treatment combinations x 3 replications = 27 experimental units.
##

library(gexp)

fe_crd <- gexp(mu     = 15,
               err    = matrix(0,
                               nrow = 27,
                               ncol = 1),
               r      = 3,
               fe     = list(gen = c(2, 0, -2),
                             nit = c(3, 0, -3)),
               fl     = list(gen = paste0('g', 1:3),
                             nit = paste0('n', 1:3)),
               type   = "FE",
               design = "CRD")

print(fe_crd)
summary(fe_crd)

if (interactive()) {
  plot(fe_crd,
       random = FALSE)

  plot(fe_crd)
}
