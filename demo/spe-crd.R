##
## SPE + CRD: split-plot, completely randomized design
##
## Whole-plot factor (main): fe then fl. Sub-plot factor (sub): fe then fl.
## inte: interaction effects (one value per main:sub combination; length = 3 x 2 = 6).
## 3 main x 2 sub x 5 replications -> 12 within-plot error df
## (Y1 ~ main * sub + Error(r/main); plot stratum df is not required >= 12).
##

library(gexp)

# 'err' is optional; if omitted, a normal error is used with mean = 1 and standard deviation = 0.
spe_crd <- gexp(mu     = 10,
                err    = matrix(0,
                                nrow = 30,
                                ncol = 1),
                r      = 5,
                fe     = list(main = c(2, 0, -2),
                              sub  = c(1, -1)),
                fl     = list(main = paste0('p', 1:3),
                              sub  = paste0('sp', 1:2)),
                inte   = c(1, 1, 2, 1, 1, 1),
                type   = "SPE",
                design = "CRD")

print(spe_crd)
summary(spe_crd)

if (interactive()) {
  plot(spe_crd)

  # Interaction plots (main x sub): parallel lines suggest no interaction.
  opar <- par(mfrow = c(2, 1))
  with(spe_crd$dfm, {
    cols <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3")
    interaction.plot(x.factor     = main,
                     trace.factor = sub,
                     response     = Y1,
                     fun          = mean,
                     type         = "b",
                     col          = cols[seq_len(nlevels(sub))],
                     legend       = TRUE,
                     trace.label  = "sub-plot",
                     xlab         = "whole-plot (main)",
                     ylab         = "Mean Y1",
                     main         = "whole-plot on x-axis")
    interaction.plot(x.factor     = sub,
                     trace.factor = main,
                     response     = Y1,
                     fun          = mean,
                     type         = "b",
                     col          = cols[seq_len(nlevels(main))],
                     legend       = TRUE,
                     trace.label  = "whole-plot",
                     xlab         = "sub-plot",
                     ylab         = "Mean Y1",
                     main         = "sub-plot on x-axis")
  })
  par(opar)
}
