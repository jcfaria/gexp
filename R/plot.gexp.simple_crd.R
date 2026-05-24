plot.gexp.simple_crd <- function(x,
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
                 'contrasts')) != 1) {
    stop('Graphic option only for one factor!')
  }

  mfactors <- attr(x$X,
                   'contrasts')

  labelfac <- names(mfactors)

  levelss <- attr(mfactors[[labelfac]],
                  'dimnames')[[1]]

  repp <- dim(x$X)[1]/length(levelss)

  if (is.null(main)) {
    main = 'Completely Random Design'
  }

  if (is.null(sub)) {
    s_levels <- paste('Levels: ',
                      paste(levelss,
                            collapse = ', '))

    sub <- paste('Factors: ',
                 labelfac,
                 '\n',
                 s_levels,
                 '\n',
                 'Replication: ',
                 repp)
  }

  treat <- .gexp_plot_random_rep(levelss, repp, random)

  rowsquare <- length(levelss)

  columsquare <- repp

  centers <- .gexp_plot_centers(rowsquare, columsquare)
  posxcentro <- centers$posxcentro
  posycentro <- centers$posycentro

  if (!dynamic) {
    op <- .gexp_plot_static_open(main = main, sub = sub, ...)

    .gexp_plot_grid(columsquare, rowsquare, colgrid, ltygrid, lwdgrid)

    .gexp_plot_text_crd(posxcentro, posycentro, treat, coltext)

    .gexp_plot_static_close(op)
  } else {
    .gexp_plot_dynamic_frame(main, sub, xleftimg, ybottomimg, xrightimg, ytopimg, ...)

    .gexp_plot_locator_text(treat,
                            coltext,
                            message = 'Click with the left button on experimental unit and end with the right button!')
  }
}
