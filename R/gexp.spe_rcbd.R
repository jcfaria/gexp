gexp.spe_rcbd <- function(x, ...)
{
  fe <- .gexp_fe(x,
                 list(f1 = rep(1, 3),
                      f2 = rep(1, 2)))

  blke <- .gexp_blke(x)

  factors <- list(r = 1:x$r)
  contrast <- list()

  blk <- .gexp_block_setup(x, blke, factors, contrast)
  factors <- blk$factors
  contrast <- blk$contrast

  intee <- .gexp_inte(x, fe)

  treatments <- .gexp_treatments(x, fe)

  contrasttreatments <- .gexp_treatment_contrasts(x, treatments)

  contrast <- c(contrast,
                contrasttreatments)

  contrasts <- .gexp_contrasts_merge(contrast, x)

  factors <- c(factors,
               treatments)

  cformula <- paste('~',
                    names(factors)[2],
                    '+',
                    paste(names(treatments),
                          collapse = '*'))

  dados <- expand.grid(factors,
                       KEEP.OUT.ATTRS = FALSE)

  XB <- makeXBeta(cformula,
                  dados,
                  mu        = x$mu,
                  fe        = fe,
                  blke      = blke,
                  rowe      = x$rowe,
                  cole      = x$cole,
                  inte      = intee,
                  contrasts = contrasts)

  facplott <- names(treatments)[1]

  cformulaplot <- paste('~ 0 + ',
                        paste(facplott,
                              names(factors)[2],
                              sep = ':'))

  plott <- c(facplott,
             names(factors)[2])

  zformula <- paste('list(',
                    paste(plott,
                          paste('= contrasts(dados$',
                                plott,
                                sep = ''),
                          ',',
                          'contrasts = FALSE)',
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
