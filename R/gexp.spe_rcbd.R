gexp.spe_rcbd <- function(x, ...)
{
  ifelse(is.null(x$fe),
         fe <- list(f1 = rep(1,
                             3),
                    f2 = rep(1,
                             2)),
         fe <- x$fe)

  ifelse(is.null(x$blke),
         blke <- rep(1,
                     3),
         blke <- x$blke)

  factors <- list(r = 1:x$r)
  contrast <- list()

  ifelse(is.null(x$blkl),
         {
           factors$Block <- factor(1:dim(as.matrix(blke))[1])
           contrast[["Block"]] <- diag(dim(as.matrix(blke))[1])
         },
         {
           factors[[names(x$blkl)]] <- factor(unlist(x$blkl))
           contrast[[names(x$blkl)]] <- diag(dim(as.matrix(blke))[1])
         })

  intee <- makeInteraction(mu   = x$mu,
                           fe   = fe,
                           inte = x$inte)

  treatments <- makeTreatments(fl        = x$fl,
                               fe        = fe,
                               quali     = x$qualiquanti$quali,
                               quanti    = x$qualiquanti$quanti,
                               posquanti = x$qualiquanti$posquanti)

  contrasttreatments <- makeContrasts(factors   = treatments,
                                      quali     = x$qualiquanti$quali,
                                      quanti    = x$qualiquanti$quanti,
                                      posquanti = x$qualiquanti$posquanti)

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
