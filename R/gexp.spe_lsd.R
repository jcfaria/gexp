gexp.spe_lsd <- function(x, ...)
{
  fe <- .gexp_fe(x,
                 list(f1 = rep(1, 3),
                      f2 = rep(1, 2)))

  x <- .gexp_lsd_force_r1(x)

  intee <- .gexp_inte(x, fe)

  treatments <- .gexp_treatments(x, fe)

  contrast <- .gexp_treatment_contrasts(x, treatments)

  n <- length(treatments[[1]])

  rc <- .gexp_rowe_cole_fe(x, n)
  rowe <- rc$rowe
  cole <- rc$cole

  lsd <- .gexp_rowcolumn_setup(x, rowe, cole, contrast)
  rowcolumn <- lsd$rowcolumn
  contrast <- lsd$contrast

  contrasts <- .gexp_contrasts_merge(contrast, x)

  combfactors <- suppressWarnings(do.call('interaction',
                                          treatments))

  scombfactors <- sort(levels(combfactors))

  nsubplot <- do.call('prod',
                      lapply(treatments[-1],
                             length))

  mcombfac <- matrix(scombfactors,
                     ncol = nsubplot,
                     byrow = TRUE)

  levelsfactors <- apply(mcombfac,
                         1,
                         function(x) paste(x,
                                           collapse = ' '))

  n <- length(treatments[[1]])

  sorttreatment <- latin(n,
                         levelss = levelsfactors,
                         nrand   = 0)

  msortfac <- as.matrix(c(sorttreatment))

  aux_trats8 <- apply(msortfac,
                      1,
                      function(x) unlist(strsplit(x,
                                                  ' ')))
  aux_trats9 <- as.matrix(c(aux_trats8))

  aux_trats10 <- strsplit(as.character(aux_trats9[, 1]),
                          '[.]')

  trats <- do.call('rbind',
                   aux_trats10)

  colnames(trats) <- names(treatments)

  trats <- as.data.frame(trats,
                         stringsAsFactors = TRUE)

  if (!x$qualiquanti$quali) {
    trats[, x$qualiquanti$posquanti] <- as.ordered(trats[, x$qualiquanti$posquanti])
  }

  rowcolumn[[1]] <- rep(rowcolumn[[1]],
                        rep(length(scombfactors),
                            n))
  rowcolumn[[2]] <- rep(rep(rowcolumn[[2]],
                            rep(nsubplot, n)),
                        n)

  combrowcolumn <- data.frame(rowcolumn)

  dados <- data.frame(combrowcolumn,
                      trats)

  cformula <- paste('~',
                    paste(names(rowcolumn),
                          collapse = '+'),
                    '+',
                    paste(names(treatments),
                          collapse = '*'))

  XB <- makeXBeta(cformula,
                  dados,
                  mu        = x$mu,
                  fe        = fe,
                  blke      = x$blke,
                  rowe      = rowe,
                  cole      = cole,
                  inte      = intee,
                  contrasts = contrasts)

  facplott <- colnames(trats)[1]

  cformulaplot <- paste('~ 0 + ',
                        facplott,
                        ':',
                        paste(names(combrowcolumn),
                              collapse = ':'),
                        sep = '')

  parceprincipal <- c(facplott,
                      names(combrowcolumn))

  zformula <- paste('list(',
                    paste(parceprincipal,
                          paste('= contrasts(dados$',
                                parceprincipal,
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
