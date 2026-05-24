##
## FE + RCBD: 2 x 2 factorial, randomized complete block design
##

library(gexp)

fe_rcbd <- gexp(mu     = 15,
                err    = matrix(0, nrow = 24, ncol = 1),
                r      = 2,
                fl     = list(f1 = c("A", "B"),
                              f2 = c("P", "Q")),
                fe     = list(f1 = c(1, -1),
                              f2 = c(2, -2)),
                type   = "FE",
                design = "RCBD")

print(fe_rcbd)
summary(fe_rcbd)

if (interactive()) {
  plot(fe_rcbd)
}
