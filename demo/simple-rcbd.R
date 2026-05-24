##
## SIMPLE + RCBD: randomized complete block design
##
## trt: fe then fl. blk: blke (block effects) then blkl (block labels).
## 3 treatments x 7 blocks -> 12 error df in ANOVA (Y1 ~ blk + trt).
##

library(gexp)

# 'err' is optional; if omitted, a normal error is used with mean = 1 and standard deviation = 0.
rcbd <- gexp(mu     = 10,
             err    = matrix(0,
                             nrow = 21,
                             ncol = 1),
             r      = 1,
             fe     = list(trt = c(1, 0, -1)),
             fl     = list(trt = paste0('t', 1:3)),
             blke   = 0:6,
             blkl   = list(blk = paste0('b', 1:7)),
             type   = "SIMPLE",
             design = "RCBD")

print(rcbd)
summary(rcbd)

if (interactive()) {
  plot(rcbd)
}
