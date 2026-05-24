##
## FE + RCBD: 2 x 2 factorial, randomized complete block design
##
## Factors: fe then fl. Blocks: blke then blkl.
## inte: interaction effects (one value per gen:nit combination; length = 2 x 2 = 4).
##      Derived from outer(gen, nit), shifted to positive values (gen rows x nit cols):
##           n1  n2
##      g1    5   1
##      g2    1   5
## 4 treatments x 7 blocks -> 18 error df in ANOVA (Y1 ~ blk + gen * nit).
##

library(gexp)

# 'err' is optional; if omitted, a normal error is used with mean = 1 and standard deviation = 0.
fe_rcbd <- gexp(mu     = 10,
                err    = matrix(0,
                                nrow = 28,
                                ncol = 1),
                r      = 1,
                fe     = list(gen = c(1, -1),
                              nit = c(2, -2)),
                fl     = list(gen = paste0('g', 1:2),
                              nit = paste0('n', 1:2)),
                inte   = c(5, 1,
                           1, 5),
                blke   = 0:6,
                blkl   = list(blk = paste0('b', 1:7)),
                type   = "FE",
                design = "RCBD")

print(fe_rcbd)
summary(fe_rcbd)

if (interactive()) {
  plot(fe_rcbd)

  opar <- par(mfrow = c(2, 1))
  with(fe_rcbd$dfm, {
    cols <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")
    interaction.plot(x.factor     = gen,
                     trace.factor = nit,
                     response     = Y1,
                     fun          = mean,
                     type         = "b",
                     col          = cols[seq_len(nlevels(nit))],
                     legend       = TRUE,
                     trace.label  = "nitrogen",
                     xlab         = "genotype",
                     ylab         = "Mean Y1",
                     main         = "genotype on x-axis")
    interaction.plot(x.factor     = nit,
                     trace.factor = gen,
                     response     = Y1,
                     fun          = mean,
                     type         = "b",
                     col          = cols[seq_len(nlevels(gen))],
                     legend       = TRUE,
                     trace.label  = "genotype",
                     xlab         = "nitrogen",
                     ylab         = "Mean Y1",
                     main         = "nitrogen on x-axis")
  })
  par(opar)
}
