gexp.fe_lsd <- function(x, ...)
{
  ifelse(is.null(x$fe),
         fe <- list(f1 = rep(1,
                             3),
                    f2 = rep(1,
                             2)),
         fe <- x$fe)

  if (x$r != 1) {
    x$r <- 1
    warning('Internaly replicates was set to one (r=1)!')
  }

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

  n <- prod(unlist(lapply(treatments,
                          length)))

  ifelse(is.null(x$rowe),
         rowe <- rep(1, n),
         rowe <- x$rowe)

  ifelse(is.null(x$cole),
         cole <- rowe,
         cole <- x$cole)

  rowcolumn <- list()

  ifelse(is.null(x$rowl),
         {
           rowcolumn$Row <- factor(1:dim(as.matrix(rowe))[1])
           contrast[["Row"]] <- diag(dim(as.matrix(rowe))[1])
         },
         {
           rowcolumn[[names(x$rowl)]] <- factor(unlist(x$rowl))
           contrast[[names(x$rowl)]] <- diag(dim(as.matrix(rowe))[1])
         })

  ifelse(is.null(x$coll),
         {
           rowcolumn$Column <- factor(1:dim(as.matrix(cole))[1])
           contrast[["Column"]] <- diag(dim(as.matrix(cole))[1])
         },
         {
           rowcolumn[[names(x$coll)]] <- factor(unlist(x$coll))
           contrast[[names(x$coll)]] <- diag(dim(as.matrix(cole))[1])
         })

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

  e <- .gexp_err(x, dim(XB$XB)[1])
  Y <- .gexp_response(x, XB, e)
  dfm <- .gexp_bind_dfm(x, dados, Y)

  return(.gexp_pack(x, XB, Z, Y, dfm))
}
