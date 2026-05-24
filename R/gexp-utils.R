# gexp-utils.R — internal shared helpers for the gexp package.
# None of these functions is exported.

.gexp_err <- function(x, n)
{
  if (is.null(x$err)) {
    mvtnorm::rmvnorm(n     = n,
                     sigma = diag(length(x$mu)))
  } else {
    if (!is.matrix(x$err))
      stop("This argument must be a matrix n x 1 univariate or n x p multivariate!")

    x$err
  }
}

.gexp_errp <- function(x, Z)
{
  if (is.null(x$errp)) {
    mvtnorm::rmvnorm(n     = ncol(Z),
                     sigma = diag(length(x$mu)))
  } else {
    if (!is.matrix(x$errp))
      stop("This argument must be a matrix n x 1 univariate or n x p multivariate!")

    x$errp
  }
}

.gexp_response <- function(x, XB, e, Z = NULL, e_plot = NULL)
{
  if (is.null(Z)) {
    yl <- XB$XB + e
  } else {
    yl <- XB$XB + Z %*% e_plot + e
  }

  colnames(yl) <- paste('Y',
                        1:dim(yl)[2],
                        sep = '')

  round(yl, x$round)
}

.gexp_bind_dfm <- function(x, dados, Y)
{
  if (!x$qualiquanti$quali) {
    dados <- lapply(dados,
                    function(x) {
                      if (is.ordered(factor(x))) as.numeric(as.character(x)) else x
                    })

    dados <- as.data.frame(dados)
  }

  cbind(dados, Y)
}

.gexp_pack <- function(x, XB, Z, Y, dfm)
{
  res <- list(X   = XB$X,
              Z   = Z,
              Y   = Y,
              dfm = dfm)

  class(res) <- c(paste('gexp',
                        class(x),
                        sep = '.'),
                  'gexp',
                  'list')

  res
}

.gexp_contrasts_merge <- function(contrast, x)
{
  if (!is.null(x$contrasts)) {
    contrast[names(x$contrasts)] <- x$contrasts
    contrast
  } else {
    contrast
  }
}

.gexp_fe <- function(x, template)
{
  if (is.null(x$fe)) template else x$fe
}

.gexp_blke <- function(x, default = rep(1, 3))
{
  if (is.null(x$blke)) default else x$blke
}

.gexp_inte <- function(x, fe)
{
  makeInteraction(mu   = x$mu,
                  fe   = fe,
                  inte = x$inte)
}

.gexp_treatments <- function(x, fe)
{
  makeTreatments(fl        = x$fl,
                 fe        = fe,
                 quali     = x$qualiquanti$quali,
                 quanti    = x$qualiquanti$quanti,
                 posquanti = x$qualiquanti$posquanti)
}

.gexp_treatment_contrasts <- function(x, treatments)
{
  makeContrasts(factors   = treatments,
                quali     = x$qualiquanti$quali,
                quanti    = x$qualiquanti$quanti,
                posquanti = x$qualiquanti$posquanti)
}

.gexp_block_setup <- function(x, blke, factors, contrast = list())
{
  if (is.null(x$blkl)) {
    factors$Block <- factor(1:dim(as.matrix(blke))[1])
    contrast[["Block"]] <- diag(dim(as.matrix(blke))[1])
  } else {
    factors[[names(x$blkl)]] <- factor(unlist(x$blkl))
    contrast[[names(x$blkl)]] <- diag(dim(as.matrix(blke))[1])
  }

  list(factors  = factors,
       contrast = contrast)
}

.gexp_rowe_cole_simple <- function(x, fe)
{
  rowe <- if (is.null(x$rowe)) unlist(fe) else x$rowe
  cole <- if (is.null(x$cole)) rowe else x$cole

  list(rowe = rowe,
       cole = cole)
}

.gexp_rowe_cole_fe <- function(x, n)
{
  rowe <- if (is.null(x$rowe)) rep(1, n) else x$rowe
  cole <- if (is.null(x$cole)) rowe else x$cole

  list(rowe = rowe,
       cole = cole)
}

.gexp_rowcolumn_setup <- function(x, rowe, cole, contrast)
{
  rowcolumn <- list()

  if (is.null(x$rowl)) {
    rowcolumn$Row <- factor(1:dim(as.matrix(rowe))[1])
    contrast[["Row"]] <- diag(dim(as.matrix(rowe))[1])
  } else {
    rowcolumn[[names(x$rowl)]] <- factor(unlist(x$rowl))
    contrast[[names(x$rowl)]] <- diag(dim(as.matrix(rowe))[1])
  }

  if (is.null(x$coll)) {
    rowcolumn$Column <- factor(1:dim(as.matrix(cole))[1])
    contrast[["Column"]] <- diag(dim(as.matrix(cole))[1])
  } else {
    rowcolumn[[names(x$coll)]] <- factor(unlist(x$coll))
    contrast[[names(x$coll)]] <- diag(dim(as.matrix(cole))[1])
  }

  list(rowcolumn = rowcolumn,
       contrast  = contrast)
}

.gexp_lsd_force_r1 <- function(x)
{
  if (x$r != 1) {
    x$r <- 1
    warning('Internaly replicates was set to one (r=1)!')
  }

  x
}
