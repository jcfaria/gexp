##
## FE + LSD: 3 x 3 factorial, Latin square design (r is set to 1 internally)
##
## Factors: fe then fl. Rows: rowe then rowl. Columns: cole then coll.
## Nine treatment combinations in a 9 x 9 Latin square (81 experimental units).
## (Order of the square = product of factor levels: 3 x 3 = 9.)
##

library(gexp)

fe_lsd <- gexp(mu     = 15,
               err    = matrix(0,
                               nrow = 81,
                               ncol = 1),
               r      = 1,
               fe     = list(gen = c(2, 0, -2),
                             nit = c(3, 0, -3)),
               fl     = list(gen = paste0('g', 1:3),
                             nit = paste0('n', 1:3)),
               rowe   = 0:8,
               rowl   = list(Row = paste0('r', 1:9)),
               cole   = 0:8,
               coll   = list(Col = paste0('l', 1:9)),
               type   = "FE",
               design = "LSD")

print(fe_lsd)
summary(fe_lsd)

if (interactive()) {
  plot(fe_lsd)
}
