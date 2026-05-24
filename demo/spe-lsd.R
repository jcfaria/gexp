##
## SPE + LSD: split-plot, Latin square design (r is set to 1 internally)
##
## Factors: fe then fl. Rows: rowe then rowl. Columns: cole then coll.
##

library(gexp)

spe_lsd <- gexp(mu     = 20,
                err    = matrix(0,
                                nrow = 18,
                                ncol = 1),
                r      = 1,
                fe     = list(main = c(2, 0, -2),
                              sub  = c(1, -1)),
                fl     = list(main = paste0('p', 1:3),
                              sub  = paste0('sp', 1:2)),
                rowe   = c(0, 1, 2),
                rowl   = list(Row = paste0('r', 1:3)),
                cole   = c(0, 1, 2),
                coll   = list(Col = paste0('l', 1:3)),
                type   = "SPE",
                design = "LSD")

print(spe_lsd)
summary(spe_lsd)

if (interactive()) {
  plot(spe_lsd)
}
