gexp.simple_lsd <- function(x, ...)
{
  ifelse(is.null(x$fe),
         fe <- list(f1 = rep(1,
                             3)),
         fe <- x$fe)

  ifelse(is.null(x$rowe),
         rowe <- unlist(fe),
         rowe <- x$rowe)

  ifelse(is.null(x$cole),
         cole <- rowe,
         cole <- x$cole)

  if (length(fe) != 1) {
    stop('Use only one factor!')
  }

  if (x$r != 1) {
    x$r <- 1
    warning('Internaly replicates was set to one (r=1)!')
  }

  treatments <- makeTreatments(fl        = x$fl,
                               fe        = fe,
                               quali     = x$qualiquanti$quali,
                               quanti    = x$qualiquanti$quanti,
                               posquanti = x$qualiquanti$posquanti)

  contrast <- makeContrasts(factors   = treatments,
                            quali     = x$qualiquanti$quali,
                            quanti    = x$qualiquanti$quanti,
                            posquanti = x$qualiquanti$posquanti)

  rowcolumn <- list()

  n <- length(treatments[[1]])

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

  e <- .gexp_err(x, dim(XB$XB)[1])
  Y <- .gexp_response(x, XB, e)
  dfm <- .gexp_bind_dfm(x, dados, Y)

  return(.gexp_pack(x, XB, Z, Y, dfm))
}
