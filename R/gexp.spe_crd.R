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
  zformula <- paste('list(',
                    paste(plott,
                          paste('= contrasts(dados$',
                                plott,
                                sep = ''),
                          ',',
                          'contrasts=FALSE)',
                          collapse = ','),
                    ')')

  Z <- model.matrix(eval(parse(text = cformulaplot)),
                    dados,
                    contrasts.arg = eval(parse(text = zformula)))

  e <- .gexp_err(x, dim(XB$XB)[1])
  e_plot <- .gexp_errp(x, Z)
  Y <- .gexp_response(x, XB, e, Z = Z, e_plot = e_plot)
  dfm <- .gexp_bind_dfm(x, dados, Y)

  return(.gexp_pack(x, XB, Z, Y, dfm))
}
