##
## SIMPLE + RCBD: randomized complete block design
##
## trt: fe then fl. blk: blke (block effects) then blkl (block labels).
##

library(gexp)

rcbd <- gexp(mu     = 10,
             err    = matrix(0,
                             nrow = 12,
                             ncol = 1),
             r      = 2,
             fe     = list(trt = c(1, -2)),
             fl     = list(trt = paste0('t', 1:2)),
             blke   = c(0, 1, 2),
             blkl   = list(blk = paste0('b', 1:3)),
             type   = "SIMPLE",
             design = "RCBD")

print(rcbd)
summary(rcbd)

if (interactive()) {
  plot(rcbd)
}
