##
## FE + RCBD: 2 x 2 factorial, randomized complete block design
##
## Factors: fe then fl. Blocks: blke then blkl.
##

library(gexp)

fe_rcbd <- gexp(mu     = 15,
                err    = matrix(0,
                                nrow = 24,
                                ncol = 1),
                r      = 2,
                fe     = list(gen = c(1, -1),
                              nit = c(2, -2)),
                fl     = list(gen = paste0('g', 1:2),
                              nit = paste0('n', 1:2)),
                blke   = c(0, 1, 2),
                blkl   = list(blk = paste0('b', 1:3)),
                type   = "FE",
                design = "RCBD")

print(fe_rcbd)
summary(fe_rcbd)

if (interactive()) {
  plot(fe_rcbd)
}
