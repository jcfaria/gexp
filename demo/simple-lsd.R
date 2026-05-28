##
## SIMPLE + LSD: Latin square design (r is set to 1 internally)
##
## trt: fe then fl. Rows: rowe then rowl. Columns: cole then coll.
## 5 x 5 Latin square -> 12 error df in ANOVA (Y1 ~ Row + Col + trt).
##

library(gexp)

# 'err' is optional; if omitted, a normal error is used with mean = 1 and standard deviation = 0.
lsd <- gexp(mu     = 10,
            err    = matrix(0,
                            nrow = 25,
                            ncol = 1),
            r      = 1,
            fe     = list(trt = c(0, 1, 2, 3, 4)),
            fl     = list(trt = paste0('t', 1:5)),
            rowe   = c(0, 1, 2, 3, 4),
            rowl   = list(Row = paste0('r', 1:5)),
            cole   = c(0, 1, 2, 3, 4),
            coll   = list(Col = paste0('l', 1:5)),
            type   = "SIMPLE",
            design = "LSD")

print(lsd)
summary(lsd)

if (interactive()) {
  plot(lsd)
}
