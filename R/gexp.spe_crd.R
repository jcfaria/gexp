gexp.spe_crd <- function(x, ...)
{
  fe <- .gexp_fe(x,
                 list(f1 = rep(1, 3),
                      f2 = rep(1, 2)))

  factors <- list(r = factor(1:x$r))

  intee <- .gexp_inte(x, fe)

  treatments <- .gexp_treatments(x, fe)

  contrast <- .gexp_treatment_contrasts(x, treatments)

  contrasts <- .gexp_contrasts_merge(contrast, x)

  cformula <- paste('~',
                    paste(names(treatments),
                          collapse = '*'))

  factors <- c(factors,
               treatments)

  dados <- expand.grid(factors,
                       KEEP.OUT.ATTRS = FALSE)

  XB <- makeXBeta(cformula,
                  dados,
                  mu        = x$mu,
                  fe        = fe,
                  blke      = x$blke,
                  rowe      = x$rowe,
                  cole      = x$cole,
                  inte      = intee,
                  contrasts = contrasts)

  facplott <- names(treatments)[1]

  cformulaplot <- paste('~ 0 + ',
                        facplott,
                        ':r',
                        sep = '')

  plott <- c(facplott, 'r')

  Z <- .gexp_Z(dados,
               cformulaplot,
               plott)

  return(.gexp_spe_finish(x,
                          XB,
                          Z,
                          dados))
}
