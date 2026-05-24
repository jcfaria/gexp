plot.gexp.fe_crd <- function(x,
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

  labelfac <- names(mfactors)

  levelsfac <- lapply(mfactors,
                      rownames)

  labelinter <- suppressWarnings(do.call('interaction',
                                         levelsfac))

  levelsinter <- levels(labelinter)

  repp <- dim(x$X)[1]/(length(levelsinter))

  if (is.null(main)) {
    main = 'Factorial Structure \n Completely Random Design'
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
                       sep = ''))
  }

  treat <- .gexp_plot_random_rep(levelsinter,
                        repp,
                        random)

  rowsquare <- length(levelsinter)
  columsquare <- repp

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

    .gexp_plot_text_crd(posxcentro,
                        posycentro,
                        treat,
                        coltext)

    .gexp_plot_static_close(op)
  } else {
    .gexp_plot_dynamic_frame(main       = main,
                             sub        = sub,
                             xleftimg   = xleftimg,
                             ybottomimg = ybottomimg,
                             xrightimg  = xrightimg,
                             ytopimg    = ytopimg,
                             ...)

    .gexp_plot_locator_text(treat,
                            coltext)
  }
}
