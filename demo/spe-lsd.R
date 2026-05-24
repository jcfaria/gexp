##
## SPE + LSD: split-plot, Latin square design (r is set to 1 internally)
##
## Factors: fe then fl. Rows: rowe then rowl. Columns: cole then coll.
## inte: interaction effects (one value per main:sub combination; length = 4 x 2 = 8).
##      Derived from outer(main, sub), shifted to positive values (main rows x sub cols):
##           sp1  sp2
##      p1     7    1
##      p2     5    3
##      p3     3    5
##      p4     1    7
## 4 main levels (4 x 4 Latin square) x 2 sub-plot levels -> 12 within-plot error df.
##

library(gexp)

# 'err' is optional; if omitted, a normal error is used with mean = 1 and standard deviation = 0.
spe_lsd <- gexp(mu     = 10,
                err    = matrix(0,
                                nrow = 32,
                                ncol = 1),
                r      = 1,
                fe     = list(main = c(3, 1, -1, -3),
                              sub  = c(1, -1)),
                fl     = list(main = paste0('p', 1:4),
                              sub  = paste0('sp', 1:2)),
                inte   = c(7, 5, 3, 1,
                           1, 3, 5, 7),
                rowe   = 0:3,
                rowl   = list(Row = paste0('r', 1:4)),
                cole   = 0:3,
                coll   = list(Col = paste0('l', 1:4)),
                type   = "SPE",
                design = "LSD")

print(spe_lsd)
summary(spe_lsd)

if (interactive()) {
  plot(spe_lsd)

  opar <- par(mfrow = c(2, 1))
  with(spe_lsd$dfm, {
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
