gexp.simple_rcbd <- function(x, ...)
{
  fe <- .gexp_fe(x,
                 list(f1 = rep(1, 3)))

  blke <- .gexp_blke(x)

  factors <- list(r = 1:x$r)
  contrast <- list()

  blk <- .gexp_block_setup(x,
                           blke,
                           factors,
                           contrast)
  factors <- blk$factors
  contrast <- blk$contrast

  treatments <- .gexp_treatments(x, fe)

  contrasttreatments <- .gexp_treatment_contrasts(x, treatments)

  contrast <- c(contrast,
                contrasttreatments)

  contrasts <- .gexp_contrasts_merge(contrast, x)

  factors <- c(factors,
               treatments)

  cformula <- paste('~',
                    paste(names(factors)[-1],
                          collapse = '+'))

  dados <- expand.grid(factors,
                       KEEP.OUT.ATTRS = FALSE)

  XB <- makeXBeta(cformula,
                  dados,
                  mu        = x$mu,
                  fe        = fe,
                  blke      = blke,
                  rowe      = x$rowe,
                  cole      = x$cole,
                  inte      = x$inte,
                  contrasts = contrasts)

  Z <- NULL

  e <- .gexp_err(x,
                 dim(XB$XB)[1])
  Y <- .gexp_response(x,
                      XB,
                      e)
  dfm <- .gexp_bind_dfm(x,
                        dados,
                        Y)

  return(.gexp_pack(x,
                    XB,
                    Z,
                    Y,
                    dfm))
}
