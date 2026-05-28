plot.gexp.spe_crd <- function(x,
                              main       = NULL,
                              sub        = NULL,
                              colgrid    = 'red',
                              coltext    = 'blue',
                              srttext    = 30,
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
                                         levelsfac[-1]))

  levelsinter <- levels(labelinter)

  repp <- dim(x$X)[1]/(length(levelsinter)*length(levelsfac[[1]]))

  if (is.null(main)) {
    main = 'Split plot Structure \n Completely Random Design'
  }

  if (is.null(sub)) {
    sub <- paste('Plot:',
                 labelfac[1],
                 '\n',
                 'Levels Plot: ',
                 paste(levelsfac[[1]],
                       collapse = ', '),
                 '\n',
                 'Subplot: ',
                 paste(labelfac[-1],
                       collapse = ', '),
                 '\n',
                 'Levels Subplot: ',
                 paste(paste(unlist(levelsfac[-1]),
                             collapse = ', '),
                       collapse = ', '),
                 '\n',
                 'Replication: ',
                 repp)
  }

  if (!random) {
    treat <- rep(levelsinter,
                 repp*length(levelsfac[[1]]))
    Labelsplot <- rep(levelsfac[[1]],
                      repp)
  } else {
    lplot <- rep(list(levelsinter),
                 length(levelsfac[[1]])*repp)
    rtreat <- lapply(lplot,
                     sample)
    treat <- unlist(rtreat)
    Labelsplot <- sample(rep(levelsfac[[1]],
                             repp))
  }

  rowsquare <- length(levelsfac[[1]])

  columsquare <- repp

  centers <- .gexp_plot_centers(rowsquare,
                                columsquare,
                                y0 = 2 / rowsquare * 0.9)
  posxcentro <- centers$posxcentro
  posycentro <- centers$posycentro

  subcenters <- .gexp_plot_sub_centers(rowsquare,
                                       length(levelsinter) * repp)
  subposxcentro <- subcenters$subposxcentro
  subposycentro <- subcenters$subposycentro

  if (!dynamic) {
    op <- .gexp_plot_static_open(main = main,
                                 sub  = sub,
                                 ...)

    .gexp_plot_grid_spe(columsquare,
                        rowsquare,
                        levelsinter,
                        colgrid,
                        ltygrid,
                        lwdgrid)

    .gexp_plot_text_spe_plot(posxcentro,
                             posycentro,
                             Labelsplot,
                             coltext)

    .gexp_plot_text_spe_sub(subposxcentro,
                        subposycentro,
                        treat,
                        colgrid,
                        srttext)

    .gexp_plot_static_close(op)
  } else {
    .gexp_plot_dynamic_frame(main       = main,
                             sub        = sub,
                             xleftimg   = xleftimg,
                             ybottomimg = ybottomimg,
                             xrightimg  = xrightimg,
                             ytopimg    = ytopimg,
                             ...)

    .gexp_plot_locator_text(Labelsplot,
                            coltext,
                            message = 'Click with the left button on plot and end with the right button!')

    .gexp_plot_locator_text(treat,
                            coltext,
                            message = 'Now, click with the left button on sub plot and end with the right button!')
  }
}
