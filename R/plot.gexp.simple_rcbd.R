plot.gexp.simple_rcbd <- function(x,
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
  if (length(attr(x$X, 'contrasts')[-1]) != 1) {
    stop('Graphic option only for one factor!')
  }

  mfactors <- attr(x$X,
                   'contrasts')

  labelfac <- names(mfactors)[-1]

  labelblock <- names(mfactors)[1]

  levelsfac <- attr(mfactors[[labelfac]],
                    'dimnames')[[1]]

  levelsblock <- attr(mfactors[[labelblock]],
                      'dimnames')[[1]]

  repp <- dim(x$X)[1]/(length(levelsblock)*length(levelsfac))


  if (is.null(main)) {
    main = 'Random Completely Block Design'
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
                 paste('Replication: ',
                       repp,
                       sep = ''),
                 '\n',
                 paste('Block: ',
                       length(levelsblock),
                       sep = ''))

  }

  if (!random) {
    treat <- rep(levelsfac,
                 repp*length(levelsblock))
  } else {
    lblock <- rep(list(rep(levelsfac,
                           repp)),
                  length(levelsblock))

    rtreat <- lapply(lblock,
                     sample)

    treat <- unlist(rtreat)
  }

  aux_colsquare <- length(levelsfac)

  columsquare <- prod(aux_colsquare*repp)

  rowsquare <- length(levelsblock)

  centers <- .gexp_plot_centers(rowsquare,
                                columsquare)
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

    .gexp_plot_label_row(posxcentro,
                         levelsblock,
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

    .gexp_plot_locator_text(paste(labelblock,
                                   1:rowsquare),
                            coltext,
                            message = 'Click with the left button on block and end with the right button!')

    .gexp_plot_locator_text(treat,
                            coltext,
                            message = 'Now, click with the left button on experimental unit and end with the right button!')
  }
}
