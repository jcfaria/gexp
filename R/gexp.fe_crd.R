gexp.fe_crd <- function(x, ...)
{
  fe <- .gexp_fe(x,
                 list(f1 = rep(1, 3),
                      f2 = rep(1, 2)))

  factors <- list(r = 1:x$r)

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

  Z <- NULL

  e <- .gexp_err(x, dim(XB$XB)[1])
  Y <- .gexp_response(x, XB, e)
  dfm <- .gexp_bind_dfm(x, dados, Y)

  return(.gexp_pack(x, XB, Z, Y, dfm))
}
