plot.gexp.simple_lsd <- function(x,
                                 main       = NULL,
                                 sub        = NULL,
                                 colgrid    = 'red',
                                 coltext    = 'blue',
                                 ltygrid    = 'dotted',
                                 lwdgrid    = par('lwd'),
                                 xleftimg   = par()$usr[1],
                                 ybottomimg = par()$usr[3],
                                 xrightimg  = par()$usr[2],
                                 ytopimg    = par()$usr[4],
                                 dynamic    = FALSE,
                                 random     = TRUE,
                                 ...)
{
  if (length(attr(x$X,
                 'contrasts')[-c(1:2)]) != 1) {
    stop('Graphic option only for one factor!')
  }

  mfactors <- attr(x$X,
                   'contrasts')

  labelfac <- names(mfactors)[-c(1:2)]

  labelrow <- names(mfactors)[1]

  labelcol <- names(mfactors)[2]

  levelsfac <- attr(mfactors[[labelfac]],
                    'dimnames')[[1]]

  levelsrow <- attr(mfactors[[labelrow]],
                    'dimnames')[[1]]

  levelscol <- attr(mfactors[[labelcol]],
                    'dimnames')[[1]]

  repp <- dim(x$X)[1]/(length(levelsrow))

  if (is.null(main)) {
    main = 'Latin Square Design'
  }

  if (is.null(sub)) {
    sub <- paste('Factors: ',
                 labelfac,
                 '\n',
                 paste('Levels: ',
                       paste(levelsfac,
                             collapse = ', '),
                       sep = ''),
                 '\n',
                 paste('Rows: ',
                       length(levelsrow),
                       sep = ''),
                 '\n',
                 paste('Columns: ',
                       length(levelscol),
                       sep = ''))
  }

  if (!random) {
    rtreat <- latin(length(levelsfac),
                    levelss = levelsfac,
                    nrand = 0)
    treat <- c(rtreat)
  } else {
    rtreat <- latin(length(levelsfac),
                    levelss = levelsfac)
    treat <- c(rtreat)
  }

  rowsquare <- columsquare <- length(levelsrow)

  centers <- .gexp_plot_centers_square(rowsquare)
  posxcentro <- centers$posxcentro
  posycentro <- centers$posycentro

  if (!dynamic) {
    op <- .gexp_plot_static_open(main = main,
                                 sub  = sub,
                                 ...)

    .gexp_plot_grid(columsquare,
                    rowsquare,
                    colgrid,
                    ltygrid,
                    lwdgrid)

    .gexp_plot_text_rcbd(posxcentro,
                         posycentro,
                         treat,
                         coltext)

    .gexp_plot_arrows_row(rowsquare)
    .gexp_plot_arrows_col(rowsquare)

    .gexp_plot_label_row(posxcentro,
                         levelsrow,
                         colgrid)
    .gexp_plot_label_col(posxcentro,
                         levelscol,
                         colgrid)

    .gexp_plot_static_close(op)
  } else {
    .gexp_plot_dynamic_frame(main       = main,
                             sub        = sub,
                             xleftimg   = xleftimg,
                             ybottomimg = ybottomimg,
                             xrightimg  = xrightimg,
                             ytopimg    = ytopimg,
                             ...)

    .gexp_plot_locator_text(paste(labelrow,
                                   1:rowsquare),
                            coltext,
                            message = 'Click with the left button on row block and end with the right button!')

    .gexp_plot_locator_text(paste(labelcol,
                                   1:columsquare),
                            coltext,
                            message = 'Click with the left button on column block and end with the right button!')

    .gexp_plot_locator_text(treat,
                            coltext,
                            message = 'Now, click with the left button on experimental unit and end with the right button!')
  }
}
