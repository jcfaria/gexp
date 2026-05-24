gexp.simple_rcbd <- function(x, ...)
{
  ifelse(is.null(x$fe),
         fe <- list(f1 = rep(1,
                             3)),
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

  e <- .gexp_err(x, dim(XB$XB)[1])
  Y <- .gexp_response(x, XB, e)
  dfm <- .gexp_bind_dfm(x, dados, Y)

  return(.gexp_pack(x, XB, Z, Y, dfm))
}
