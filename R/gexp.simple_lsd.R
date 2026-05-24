gexp.simple_lsd <- function(x, ...)
{
  fe <- .gexp_fe(x,
                 list(f1 = rep(1, 3)))

  rc <- .gexp_rowe_cole_simple(x, fe)
  rowe <- rc$rowe
  cole <- rc$cole

  if (length(fe) != 1) {
    stop('Use only one factor!')
  }

  x <- .gexp_lsd_force_r1(x)

  treatments <- .gexp_treatments(x, fe)

  contrast <- .gexp_treatment_contrasts(x, treatments)

  n <- length(treatments[[1]])

  lsd <- .gexp_rowcolumn_setup(x,
                               rowe,
                               cole,
                               contrast)
  rowcolumn <- lsd$rowcolumn
  contrast <- lsd$contrast

  contrasts <- .gexp_contrasts_merge(contrast, x)

  cformula <- paste('~',
                    paste(c(names(rowcolumn),
                            names(treatments)),
                          collapse = '+'))
  sorttreatment <- latin(n,
                         levelss = treatments[[1]],
                         nrand   = 0)

  if (x$qualiquanti$quanti) {
    sorttreatment <- as.ordered(sorttreatment)
  } else {
    sorttreatment <- c(sorttreatment)
  }

  combrowcolumn <- expand.grid(rowcolumn,
                               KEEP.OUT.ATTRS = FALSE)

  dados <- data.frame(combrowcolumn,
                      T1 = sorttreatment,
                      stringsAsFactors = TRUE)

  names(dados) <- gsub("T1",
                       names(treatments),
                       names(dados))

  XB <- makeXBeta(cformula,
                  dados,
                  mu        = x$mu,
                  fe        = fe,
                  blke      = x$blke,
                  rowe      = rowe,
                  cole      = cole,
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
