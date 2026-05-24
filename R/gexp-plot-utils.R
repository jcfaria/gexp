# gexp-plot-utils.R — internal shared helpers for plot.gexp.* methods.
# None of these functions is exported.

.gexp_plot_seq_centers <- function(n, start = 1 / n)
{
  aux0 <- start
  aux1 <- aux0 + ((n - 1) * 2 / n)
  seq(aux0, aux1, by = 2 / n)
}

.gexp_plot_centers <- function(rowsquare, columsquare, y0 = NULL)
{
  if (is.null(y0)) {
    y0 <- 1 / rowsquare
  }

  list(posxcentro = .gexp_plot_seq_centers(columsquare, 1 / columsquare),
       posycentro = .gexp_plot_seq_centers(rowsquare, y0))
}

.gexp_plot_centers_square <- function(n)
{
  pos <- .gexp_plot_seq_centers(n, 1 / n)
  list(posxcentro = pos,
       posycentro = pos)
}

.gexp_plot_sub_centers <- function(rowsquare, ncol_sub, y0 = 1 / rowsquare)
{
  list(subposxcentro = .gexp_plot_seq_centers(ncol_sub, 1 / ncol_sub),
       subposycentro = .gexp_plot_seq_centers(rowsquare, y0))
}

.gexp_plot_static_open <- function(main, sub, ...)
{
  op <- par('xaxs', 'yaxs')

  par(xaxs = 'i',
      yaxs = 'i')

  plot(1,
       type = 'n',
       xlim = c(0, 2),
       ylim = c(0, 2),
       axes = FALSE,
       xlab = '',
       ylab = '',
       main = main,
       sub  = sub,
       ...)

  box()

  op
}

.gexp_plot_static_close <- function(op)
{
  par(op)
}

.gexp_plot_grid <- function(columsquare, rowsquare, colgrid, ltygrid, lwdgrid)
{
  grid(nx = columsquare,
       ny = rowsquare,
       col = colgrid,
       lty = ltygrid,
       lwd = lwdgrid)
}

.gexp_plot_grid_spe <- function(columsquare, rowsquare, levelsinter, colgrid, ltygrid, lwdgrid)
{
  grid(nx = columsquare,
       ny = rowsquare,
       col = 1,
       lty = 1,
       lwd = 1)

  grid(nx = length(levelsinter) * columsquare,
       ny = 1,
       col = c(rep(colgrid,
                 length(levelsinter) - 1),
             1),
       lty = c(rep(ltygrid,
                 length(levelsinter) - 1),
             'solid'),
       lwd = lwdgrid)
}

.gexp_plot_text_crd <- function(posxcentro, posycentro, labels, coltext)
{
  text(x = rep(posxcentro,
               length(posycentro)),
       y = rep(posycentro,
               length(posxcentro)),
       labels,
       col = coltext)
}

.gexp_plot_text_rcbd <- function(posxcentro, posycentro, labels, coltext, srt = NULL)
{
  if (is.null(srt)) {
    text(x = rep(posycentro,
                 length(posxcentro)),
         y = rep(posxcentro,
                 length(posycentro)),
         labels,
         col = coltext)
  } else {
    text(x = rep(posxcentro,
                 length(posycentro)),
         y = rep(posycentro,
                 length(posxcentro)),
         labels,
         col = coltext,
         srt = srt)
  }
}

.gexp_plot_text_spe_plot <- function(posxcentro, posycentro, labels, coltext, swap = FALSE)
{
  text(x = rep(posxcentro,
               length(posycentro)),
       y = rep(posycentro,
               length(posxcentro)),
       labels,
       col = coltext)
}

.gexp_plot_text_spe_sub <- function(subposxcentro, subposycentro, labels, colgrid, srttext)
{
  text(x = rep(subposxcentro,
               length(subposycentro)),
       y = rep(subposycentro,
               length(subposxcentro)),
       labels,
       srt = srttext,
       col = colgrid)
}

.gexp_plot_arrows_row <- function(rowsquare)
{
  arrows(-0.05,
         seq(0, 2, by = 2 / rowsquare),
         -0.05,
         seq(2 / rowsquare, 2, by = 2 / rowsquare),
         angle  = 90,
         xpd    = TRUE,
         code   = 3,
         length = 0.06)
}

.gexp_plot_arrows_col <- function(rowsquare)
{
  arrows(seq(0, 2, by = 2 / rowsquare),
         2.05,
         seq(2 / rowsquare, 2, by = 2 / rowsquare),
         2.05,
         angle  = 90,
         xpd    = TRUE,
         code   = 3,
         length = 0.06)
}

.gexp_plot_label_row <- function(pos, labels, colgrid)
{
  text(-0.08,
       pos,
       labels,
       col = colgrid,
       xpd = TRUE,
       srt = 90)
}

.gexp_plot_label_col <- function(pos, labels, colgrid)
{
  text(pos,
       2.08,
       labels,
       col = colgrid,
       xpd = TRUE)
}

.gexp_plot_read_image <- function()
{
  auxin <- tcltk::tk_choose.files()

  auxin1 <- gsub('[\\s\\S]*?\\.',
                 '',
                 auxin,
                 perl = TRUE)

  auxin2 <- toupper(auxin1)

  switch(auxin2,
         PNG = png::readPNG(auxin),
         JPEG = jpeg::readJPEG(auxin),
         JPG = jpeg::readJPEG(auxin))
}

.gexp_plot_dynamic_frame <- function(main, sub, xleftimg, ybottomimg, xrightimg, ytopimg, ...)
{
  myimage <- .gexp_plot_read_image()

  plot(1,
       type = 'n',
       xlab = '',
       ylab = '',
       axes = FALSE,
       main = main,
       sub  = sub,
       ...)

  rasterImage(myimage,
              xleft   = xleftimg,
              ybottom = ybottomimg,
              xright  = xrightimg,
              ytop    = ytopimg)
}

.gexp_plot_locator_text <- function(labels, col, message = NULL, srt = NULL)
{
  if (!is.null(message)) {
    tcltk::tkmessageBox(message = message)
  }

  if (is.null(srt)) {
    text(x = locator(),
         y = NULL,
         labels,
         col = col)
  } else {
    text(x = locator(),
         y = NULL,
         labels,
         srt = srt,
         col = col)
  }
}

.gexp_plot_random_rep <- function(levels, repp, random)
{
  if (!random) {
    rep(levels, repp)
  } else {
    sample(rep(levels, repp))
  }
}
