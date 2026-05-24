##
## SPE + RCBD: split-plot, randomized complete block design
##
## Factors: fe then fl. Blocks: blke then blkl.
##

library(gexp)

spe_rcbd <- gexp(mu     = 20,
                 err    = matrix(0,
                                 nrow = 36,
                                 ncol = 1),
                 r      = 2,
                 fe     = list(main = c(2, 0, -2),
                               sub  = c(1, -1)),
                 fl     = list(main = paste0('p', 1:3),
                               sub  = paste0('sp', 1:2)),
                 blke   = c(0, 1, 2),
                 blkl   = list(blk = paste0('b', 1:3)),
                 type   = "SPE",
                 design = "RCBD")

print(spe_rcbd)
summary(spe_rcbd)

if (interactive()) {
  plot(spe_rcbd)
}
