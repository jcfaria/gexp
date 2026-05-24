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
