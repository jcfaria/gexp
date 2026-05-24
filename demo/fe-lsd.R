##
## FE + LSD: 2 x 2 factorial, Latin square design (r is set to 1 internally)
##

library(gexp)

fe_lsd <- gexp(mu     = 15,
               err    = matrix(0, nrow = 16, ncol = 1),
               r      = 1,
               fl     = list(f1 = c("A", "B"),
                             f2 = c("P", "Q")),
               fe     = list(f1 = c(1, -1),
                             f2 = c(2, -2)),
               type   = "FE",
               design = "LSD")

print(fe_lsd)
summary(fe_lsd)

if (interactive()) {
  plot(fe_lsd)
}
