plot.gexp.spe_rcbd <- function(x,
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

  labelblock <- names(mfactors)[1]

  labelfac <- names(mfactors)[-1]

  levelsblock <- rownames(mfactors[[1]])

  levelsfac <- lapply(mfactors[-1],
                      rownames)

  labelinter <- suppressWarnings(do.call('interaction',
                                         levelsfac[-1]))

  levelsinter <- levels(labelinter)

  repp <- dim(x$X)[1]/(length(levelsinter)*length(levelsfac[[1]])*length(levelsblock))

  if (is.null(main)) {
    main = 'Split plot Structure \n Random Completely Block Design'
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
                 repp,
                 '\n',
                 'Block: ',
                 length(levelsblock))
  }

  if (!random) {
    treat <- rep(levelsinter,
                 repp*length(levelsblock)*length(levelsfac[[1]]))

    Labelsplot <- rep(levelsfac[[1]],
                      repp)
  } else {
    lplot <- rep(list(levelsinter),
                 length(levelsfac[[1]])*repp*length(levelsblock))

    rtreat <- lapply(lplot,
                     sample)

    treat <- unlist(rtreat)

    lblock <- rep(list(rep(levelsfac[[1]],
                           repp)),
                  length(levelsblock))

    rabelsplot <- lapply(lblock,
                         sample)

    Labelsplot <- unlist(rabelsplot)
  }

  rowsquare <- length(levelsblock)

  columsquare <- length(levelsfac[[1]])*repp

  centers <- .gexp_plot_centers(rowsquare,
                                columsquare,
                                y0 = 2 / rowsquare * 0.9)
  posxcentro <- centers$posxcentro
  posycentro <- centers$posycentro

  ncol_sub <- length(levelsinter) * repp * length(levelsfac[[1]])

  subcenters <- .gexp_plot_sub_centers(rowsquare,
                                      ncol_sub)
  subposxcentro <- subcenters$subposxcentro
  subposycentro <- subcenters$subposycentro

  if (!dynamic) {
    op <- .gexp_plot_static_open(main    = main,
                                 sub     = sub,
                                 cex.sub = 0.8,
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

    .gexp_plot_arrows_row(rowsquare)

    .gexp_plot_label_row(subposycentro,
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

    .gexp_plot_locator_text(Labelsplot,
                            coltext,
                            message = 'Click with the left button on plot and end with the right button!')

    .gexp_plot_locator_text(treat,
                            coltext,
                            message = 'Click with the left button on sub plot and end with the right button!')
  }
}
