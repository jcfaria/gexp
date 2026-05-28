##
## FE + CRD: 3 x 3 factorial, completely randomized design
##
## Two factors: fe (effects) before fl (labels) for each; same name in both lists.
## inte: interaction effects (one value per gen:nit combination; length = 3 x 3 = 9).
##      Derived from outer(gen, nit), shifted to positive values (gen rows x nit cols):
##           n1  n2  n3
##      g1   13   7   1
##      g2    7   7   7
##      g3    1   7  13
## 9 treatment combinations x 3 replications -> 18 error df in ANOVA (Y1 ~ gen * nit).
##

library(gexp)

# 'err' is optional; if omitted, a normal error is used with mean = 1 and standard deviation = 0.
fe_crd <- gexp(mu     = 10,
               err    = matrix(0,
                               nrow = 27,
                               ncol = 1),
               r      = 3,
               fe     = list(gen = c(2, 0, -2),
                             nit = c(3, 0, -3)),
               fl     = list(gen = paste0('g', 1:3),
                             nit = paste0('n', 1:3)),
               inte   = c(13, 7,  1,
                           7, 7,  7,
                           1, 7, 13),
               type   = "FE",
               design = "CRD")

print(fe_crd)
summary(fe_crd)

if (interactive()) {
  plot(fe_crd)

  plot(fe_crd,
       random = FALSE)

  # Interaction plots (stats::interaction.plot): non-parallel lines indicate interaction.
  opar <- par(mfrow = c(2, 1))
  with(fe_crd$dfm, {
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
