gexp.fe_crd <- function(x, ...)
{
  ifelse(is.null(x$fe),
         fe <- list(f1 = rep(1,
                             3),
                    f2 = rep(1,
                             2)),
         fe <- x$fe)
  factors <- list(r = 1:x$r)

  intee <- makeInteraction(mu   = x$mu,
                           fe   = fe,
                           inte = x$inte)

  treatments <- makeTreatments(fl        = x$fl,
                               fe        = fe,
                               quali     = x$qualiquanti$quali,
                               quanti    = x$qualiquanti$quanti,
                               posquanti = x$qualiquanti$posquanti)

  contrast <- makeContrasts(factors   = treatments,
                            quali     = x$qualiquanti$quali,
                            quanti    = x$qualiquanti$quanti,
                            posquanti = x$qualiquanti$posquanti)

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
