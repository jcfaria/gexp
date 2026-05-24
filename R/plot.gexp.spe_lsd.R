plot.gexp.spe_lsd <- function(x,
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

  labelfac <- names(mfactors)[-c(1:2)]

  labelrow <- names(mfactors)[1]

  labelcol <- names(mfactors)[2]

  levelsfac <- lapply(mfactors[-c(1:2)],
                      rownames)

  levelsrow <- rownames(mfactors[[1]])

  levelscol <- rownames(mfactors[[2]])

  labelinter <- suppressWarnings(do.call('interaction',
                                         levelsfac[-1]))

  levelsinter <- levels(labelinter)

  if (is.null(main)) {
    main = 'Split plot Structure: Latin Square Design'
  }

  if (is.null(sub)) {
    sub <- paste('Plot: ',
                 labelfac[1],
                 '\n',
                 'Levels Plot: ',
                 paste(levelsfac[[1]],
                       collapse = ', '),
                 '\n',
                 'Subplot: ',
                 labelfac[-1],
                 '\n',
                 'Levels Subplot: ',
                 paste(unlist(levelsfac[-1]),
                       collapse = ', '),
                 '\n',
                 paste('Rows: ',
                       labelrow,
                       sep = ''),
                 '\n',
                 paste('Columns: ',
                       labelcol,
                       sep = ''))
  }

  if (!random) {
    treat <- rep(levelsinter,
                 length(levelsrow)^2)

    Labelsplot <- latin(n = length(levelsrow),
                        levelss = levelsfac[[1]],
                        nrand = 0)
  } else {
    lplot <- rep(list(levelsinter),
                 length(levelsrow)^2)

    rtreat <- lapply(lplot,
                     sample)

    treat <- unlist(rtreat)

    Labelsplot <- latin(n = length(levelsrow),
                        levelss = levelsfac[[1]])
  }

  rowsquare <- columsquare <- length(levelsrow)

  centers <- .gexp_plot_centers(rowsquare,
                                columsquare,
                                y0 = 2 / rowsquare * 0.9)
  posxcentro <- centers$posxcentro
  posycentro <- centers$posycentro

  ncol_sub <- length(levelsinter) * length(levelsfac[[1]])

  subcenters <- .gexp_plot_sub_centers(rowsquare, ncol_sub)
  subposxcentro <- subcenters$subposxcentro
  subposycentro <- subcenters$subposycentro

  if (!dynamic) {
    op <- .gexp_plot_static_open(main = main, sub = sub, cex.sub = 0.8, ...)

    .gexp_plot_grid_spe(columsquare, rowsquare, levelsinter, colgrid, ltygrid, lwdgrid)

    .gexp_plot_text_spe_plot(posxcentro, posycentro, Labelsplot, coltext, swap = TRUE)

    .gexp_plot_text_spe_sub(subposxcentro, subposycentro, treat, colgrid, srttext)

    .gexp_plot_arrows_row(rowsquare)
    .gexp_plot_arrows_col(rowsquare)

    .gexp_plot_label_row(posxcentro, levelsrow, colgrid)
    .gexp_plot_label_col(posxcentro, levelscol, colgrid)

    .gexp_plot_static_close(op)
  } else {
    .gexp_plot_dynamic_frame(main, sub, xleftimg, ybottomimg, xrightimg, ytopimg, ...)

    .gexp_plot_locator_text(levelsrow,
                            coltext,
                            message = 'Click with the left button on row block and end with the right button!')

    .gexp_plot_locator_text(levelscol,
                            coltext,
                            message = 'Click with the left button on column block and end with the right button!')

    .gexp_plot_locator_text(Labelsplot,
                            coltext,
                            message = 'Click with the left button on plot and end with the right button!')

    .gexp_plot_locator_text(treat,
                            colgrid,
                            message = 'Click with the left button on sub-plot and end with the right button!',
                            srt = srttext)
  }
}
