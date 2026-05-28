gexp.fe_lsd <- function(x, ...)
{
  fe <- .gexp_fe(x,
                 list(f1 = rep(1, 3),
                      f2 = rep(1, 2)))

  x <- .gexp_lsd_force_r1(x)

  intee <- .gexp_inte(x, fe)

  treatments <- .gexp_treatments(x, fe)

  contrast <- .gexp_treatment_contrasts(x, treatments)

  n <- prod(unlist(lapply(treatments,
                          length)))

  rc <- .gexp_rowe_cole_fe(x, n)
  rowe <- rc$rowe
  cole <- rc$cole

  lsd <- .gexp_rowcolumn_setup(x,
                               rowe,
                               cole,
                               contrast)
  rowcolumn <- lsd$rowcolumn
  contrast <- lsd$contrast

  contrasts <- .gexp_contrasts_merge(contrast, x)

  combfactors <- suppressWarnings(do.call('interaction',
                                          treatments))
  levelsfactors <- levels(combfactors)

  cformula <- paste('~',
                    paste(names(rowcolumn),
                          collapse = '+'),
                    '+',
                    paste(names(treatments),
                          collapse = '*'))

  sorttreatment <- latin(n,
                         levelss = levelsfactors,
                         nrand   = 0)

  mtreatments <- as.matrix(c(sorttreatment))

  splittreatments <- strsplit(as.character(mtreatments[, 1]),
                              '[.]')
  trats <- do.call('rbind',
                   splittreatments)

  colnames(trats) <- names(treatments)

  trats <- as.data.frame(trats,
                         stringsAsFactors = TRUE)

  if (!x$qualiquanti$quali) {
    trats[, x$qualiquanti$posquanti] <- as.ordered(trats[, x$qualiquanti$posquanti])
  }

  combrowcolumn <- expand.grid(rowcolumn,
                               KEEP.OUT.ATTRS = FALSE)

  dados <- data.frame(combrowcolumn,
                      trats)

  XB <- makeXBeta(cformula,
                  dados,
                  mu        = x$mu,
                  fe        = fe,
                  blke      = x$blke,
                  rowe      = rowe,
                  cole      = cole,
                  inte      = intee,
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
