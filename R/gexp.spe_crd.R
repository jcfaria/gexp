gexp.spe_crd <- function(x, ...)
{
  ifelse(is.null(x$fe),
         fe <- list(f1 = rep(1,
                             3),
                    f2 = rep(1,
                             2)),
         fe <- x$fe)

  factors <- list(r = factor(1:x$r))

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
