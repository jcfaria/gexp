plot.gexp.fe_rcbd <- function(x,
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

  labelfac <- names(mfactors)[-1]

  labelblock <- names(mfactors)[1]

  levelsfac <- lapply(mfactors[-1],
                      rownames)

  levelsblock <- rownames(mfactors[[1]])

  labelinter <- suppressWarnings(do.call('interaction',
                                         levelsfac))

  levelsinter <- levels(labelinter)

  repp <- dim(x$X)[1]/(length(levelsinter)*length(levelsblock))

  if (is.null(main)) {
    main = 'Factorial Structure \n Random Completely Block Design'
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
                 paste('Replication: ',
                       repp,
                       sep = ''),
                 '\n',
                 paste('Block: ',
                       length(levelsblock),
                       sep = ''))
  }

  if (!random) {
    treat <- rep(levelsinter,
                 repp*length(levelsblock))
  } else {
    lblock <- rep(list(rep(levelsinter,
                           repp)),
                  length(levelsblock))

    rtreat <- lapply(lblock,
                     sample)

    treat <- unlist(rtreat)
  }

  rowsquare <- length(levelsblock)

  columsquare <- dim(x$X)[1]/rowsquare

  centers <- .gexp_plot_centers(rowsquare, columsquare)
  posxcentro <- centers$posxcentro
  posycentro <- centers$posycentro

  if (!dynamic) {
    op <- .gexp_plot_static_open(main = main, sub = sub, ...)

    .gexp_plot_grid(columsquare, rowsquare, colgrid, ltygrid, lwdgrid)

    .gexp_plot_text_rcbd(posxcentro, posycentro, treat, coltext, srt = 40)

    .gexp_plot_arrows_row(rowsquare)

    .gexp_plot_label_row(posycentro, levelsblock, colgrid)

    .gexp_plot_static_close(op)
  } else {
    .gexp_plot_dynamic_frame(main, sub, xleftimg, ybottomimg, xrightimg, ytopimg, ...)

    .gexp_plot_locator_text(paste(labelblock,
                                   1:rowsquare),
                            coltext)

    .gexp_plot_locator_text(treat, coltext)
  }
}
