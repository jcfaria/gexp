plot.gexp.fe_lsd <- function(x,
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
  mfactors <- attr(x$X,
                   'contrasts')

  labelfac <- names(mfactors)[-c(1:2)]

  labelrow <- names(mfactors)[1]

  labelcol <- names(mfactors)[2]

  levelsfac <- lapply(mfactors[-c(1:2)],
                      rownames)

  levelsrow <- rownames(mfactors[[1]])

  levelscol <- rownames(mfactors[[2]])

  labelinter <- suppressWarnings(do.call('interaction',
                                         levelsfac))

  levelsinter <- levels(labelinter)

  repp <- dim(x$X)[1]/(length(levelsrow))

  if (is.null(main)) {
    main = 'Factorial Structure: Latin Square Design'
  }

  if (is.null(sub)) {
    sub <- paste('Factors: ',
                 paste(labelfac,
                       collapse = ', '),
                 '\n',
                 paste('Levels: ',
                       paste(unlist(levelsfac),
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
    rtreat <- latin(length(levelsinter),
                    levelss = levelsinter,
                    nrand = 0)
    treat <- c(rtreat)
  } else {
    rtreat <- latin(length(levelsinter),
                    levelss = levelsinter)
    treat <- c(rtreat)
  }

  rowsquare <- columsquare <- length(levelsrow)

  centers <- .gexp_plot_centers_square(rowsquare)
  posxcentro <- centers$posxcentro
  posycentro <- centers$posycentro

  if (!dynamic) {
    op <- .gexp_plot_static_open(main = main, sub = sub, ...)

    .gexp_plot_grid(columsquare, rowsquare, colgrid, ltygrid, lwdgrid)

    .gexp_plot_text_rcbd(posxcentro, posycentro, treat, coltext)

    .gexp_plot_arrows_row(rowsquare)
    .gexp_plot_arrows_col(rowsquare)

    .gexp_plot_label_row(posxcentro, levelsrow, colgrid)
    .gexp_plot_label_col(posxcentro, levelscol, colgrid)

    .gexp_plot_static_close(op)
  } else {
    .gexp_plot_dynamic_frame(main, sub, xleftimg, ybottomimg, xrightimg, ytopimg, ...)

    .gexp_plot_locator_text(paste(labelrow,
                                   1:rowsquare),
                            coltext)

    .gexp_plot_locator_text(paste(labelcol,
                                   1:columsquare),
                            coltext)

    .gexp_plot_locator_text(treat, coltext)
  }
}
