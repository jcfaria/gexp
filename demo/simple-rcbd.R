##
## SIMPLE + RCBD: randomized complete block design
##

library(gexp)

rcbd <- gexp(mu     = 10,
             err    = matrix(0, nrow = 12, ncol = 1),
             r      = 2,
             fe     = list(alpha = c(1, -2)),
             type   = "SIMPLE",
             design = "RCBD")

print(rcbd)
summary(rcbd)

if (interactive()) {
  plot(rcbd)
}
