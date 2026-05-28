##
## FE + LSD: 3 x 3 factorial, Latin square design (r is set to 1 internally)
##
## Factors: fe then fl. Rows: rowe then rowl. Columns: cole then coll.
## inte: interaction effects (one value per gen:nit combination; length = 3 x 3 = 9).
##      Derived from outer(gen, nit), shifted to positive values (gen rows x nit cols):
##           n1  n2  n3
##      g1   13   7   1
##      g2    7   7   7
##      g3    1   7  13
## Nine treatment combinations in a 9 x 9 Latin square (81 experimental units).
## 56 error df in ANOVA (Y1 ~ Row + Col + gen * nit).
##

library(gexp)

# 'err' is optional; if omitted, a normal error is used with mean = 1 and standard deviation = 0.
fe_lsd <- gexp(mu     = 10,
               err    = matrix(0,
                               nrow = 81,
                               ncol = 1),
               r      = 1,
               fe     = list(gen = c(2, 0, -2),
                             nit = c(3, 0, -3)),
               fl     = list(gen = paste0('g', 1:3),
                             nit = paste0('n', 1:3)),
               inte   = c(13, 7,  1,
                           7, 7,  7,
                           1, 7, 13),
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

  opar <- par(mfrow = c(2, 1))
  with(fe_lsd$dfm, {
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
