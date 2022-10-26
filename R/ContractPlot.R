#*************************************************************
# Copyright (c) 2015 by ZHAW.
# Please see accompanying distribution file for license.
#*************************************************************

# define the generic cashflowPlot(evs) function 
# not appropriate to export this - no value for users to redefine plot 
#' cashflowPlot < >     generic function to plot cashflow events of a contract
#' 
#'  The instance of this function is cashflowPlot(\code{\link{EventSeries}}) 
#'  where the input \code{\link{EventSeries}} carries cashflow event data for a 
#'  contract to be plotted
#'   
#' @param evs    S4 reference to \code{\link{EventSeries}} object
setGeneric(name = "cashflowPlot",
           def = function(evs) {
             standardGeneric("cashflowPlot")
           })
##############################################################
#'  cashflowPlot  (evs) - create graphical plot of a contract's cashflow events
#'
#'   Creates and displays a graphical representation of the cashflow events in 
#'   an  \code{\link{EventSeries}} object. An \code{\link{EventSeries}} captures
#'   the cashflows of a single ACTUS contract. The exported function
#'   cashflowPlot(\code{\link{EventSeries}}) displays the cashflow events of the
#'   contract graphically. 
#' 
#'   The graphical representation shows the different types of cashflow event
#'   such as Principal Payment, Interest Payment, etc. Cashflows are shown 
#'   as directional arrows. Incoming cashflows appear as arrows towards the 
#'   x-axis; outgoing cashflows appear as arrows away from the x-axis. 
#'   A color code indicates the type of the cashflow as described in detail in 
#'   the displayed legend of the plot. 
#'
#' @include  EventSeries.R
#' @include  bond.R
#' @param evs  a \code{\link{EventSeries}} object with contract events to be 
#'             plotted
#' @return   creates  returns a graphical canvas (plot) displayed as plot
#' @examples {
#'   pam1      <- bond("2013-12-31", maturity = "5 years", nominal = 50000,
#'                      coupon = 0.02, couponFreq = "1 years", role = "long")
#'   serverURL <- "https://demo.actusfrf.org:8080/" 
#'   evs1      <- generateEventSeries(pam1, list(), serverURL)
#'   cashflowPlot(evs1)     
#' }
#' @export
#' @importFrom timeDate as.timeDate
#' @importFrom graphics abline arrows axis legend mtext par text title
setMethod("cashflowPlot", signature("EventSeries"),
          definition = function(evs){
            # get data.frame of cash flow events from EventSeries
            df<- evs$events_df
            # plot
            # I need to distinguish between single and combined contracts!
            ct <- evs$contractType
            id <- evs$contractID
  # if there is at least one riskFactor, put its rfID into id which is 
  # used to set the graph title 
            if (length(evs$riskFactors) >= 1 ) {
               id <- paste0(id, " ",evs$riskFactors[[1]]$riskFactorID)
            }
  # eventually id should be called graphId, and set scenarioID  
            
  # we will focus on getting plots for simole contracts but leave in the
  # code for complex case see if it compiles - suspect not helpful FNP
            stopifnot(! ct %in% c("future", "futur", "option", "optns",
                             "swap", "swaps")) # not doing complex for now FNP
            contractPlot(df,contractType = ct, contractID = id)
          })

# -----------------------------------------------------------
# private helper method (accessed through method 'plot')
contractPlot <- function(events_df, contractType, contractID, ...){

    ##require(timeSeries)
    ## get function arguments
    # the EventSeries events_df has yyyy-mm-dd dates ( no timestamp)
    df <- events_df
    start <- df[1,"time"]    # no need to substr for time
    end <-   df[nrow(df),"time"]    # no need to substr for time
    by <- "1 day"
    optlist <- list(...)
    # everything below this might work without change

    ## basic or combined ct?
    if(tolower(gsub(" ", "", contractType)) %in%
       c("pam", "principalatmaturity", "ann", "annuity",
         "nam", "negativeamortizer", "lam", "linearamortizer",
         "lax", "exoticlinearamortizer","operations",
         "operationalcf","investments","reserves","currentaccount")) {

        ## (1) initialize graphics object
        graph <- initializeBasicCTGraphic(df, start, end, by)
        graph[["title"]] <- paste0("Contract ID: ",contractID)

        ## (2) add layers according to contract type
        graph[["y1.lab"]] <- "Notional/Principal"
        if (contractType == "Investments"){
          graph[["y2.lab"]] <- "Depreciation"
        } else {
          graph[["y2.lab"]] <- "Interest Payments"
        }
        graph <- addNotionalPrincipalPaymentLayer(graph, df, axis = "NULL")
        graph <- addNotionalPrincipalStateLayer(graph, df, axis = "NULL")
        graph <- addPrincipalRedemptionLayer(graph, df, axis = "NULL")
        graph <- addInterestPaymentLayer(graph, df, axis = "NULL")
        graph <- addCapitalisationLayer(graph, df, axis = "NULL")
        graph <- addInterestAccrualsLayer(graph, df, axis = "NULL")
        graph <- addRateResetLayer(graph, df, axis = "NULL")

        ## (3) finally draw graphic
        ## print plot to external file
        ##png(file = file, width = 680, height = 480)
        drawBasicCTGraphic(graph)
        ##dev.off()

    } else if(tolower(gsub(" ", "", contractType)) %in%
              c("stk", "stock")) { ## obviously a Stock

        df$NominalValue <- 0
        ## (1) initialize graphics object
        graph <- initializeBasicCTGraphic(df, start, end, by)
        graph[["title"]] <- paste0("Contract ID: ",contractID)

        ## (2) add layers according to contract type
        graph[["y1.lab"]] <- "Notional/Principal"
        graph[["y2.lab"]] <- "Dividend Payments"
        graph <- addNotionalPrincipalPaymentLayer(graph, df, axis = "NULL")
        #graph <- addNotionalPrincipalStateLayer(graph, df, axis = "NULL")
        graph <- addDividendLayer(graph, df, axis = "NULL")

        ## (3) finally draw graphic
        ## print plot to external file
        ##png(file = file, width = 680, height = 480)
        drawBasicCTGraphic(graph)
        ##dev.off()

    } else { ## combined CT

        ## what is the type of child 1?
        child1Type = optList$childType[1]

        ## (0) preparation: separate child and parent events
        df.parent <- subset(df, subset = Level == "P")
        df.child1 <- subset(df, subset = Level == "C1" | Type == "AD0")
        children <- list(df.child1)
        df.child2 <- subset(df, subset = Level == "C2")
        if(nrow(df.child2)>0) {
            child2Type <- optList$childType[2]
            df.child2 <- subset(df, subset = Level == "C2" | Type == "AD0")
            children[[2]] <- df.child2
        }

        ## (1) initialize graphics object
        graph <- initializeCombinedCTGraphic(df.parent, children, start, end, by)
        graph[["title"]] <- paste0("Contract ID: ",contractID)

        ## (2) add parent layers
        graph <- addNotionalPrincipalPaymentLayer(graph, df.parent, axis = "P")
        graph <- addMarginingLayer(graph, df.parent, axis = "P")

        ## (3) add children layers
        ##     child 1
        childCT <- child1Type
        graph <- addChildLayers(graph, children[[1]], childCT, axis = "C1")
        ##     child 2 (if exists)
        if(length(children)>1){
            childCT <- child2Type
            graph <- addChildLayers(graph, children[[2]], childCT, axis = "C2")
        }

        ## (4) finally draw graphic
        ## print plot to external file
        ##png(file = file, width = 680, height = 480)
        drawCombinedCTGraphic(graph)
        ##dev.off()
    }

}


drawBasicCTGraphic <- function(obj) {

    ## (1) extract graphics parameters
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
    plot.title <- obj[["title"]]
    x.lab <- obj[["x.lab"]]
    y1.lab <- obj[["y1.lab"]]
    y2.lab <- obj[["y2.lab"]]
    xaxis <- obj[["xaxis"]]
    x.stretch <- obj[["x.stretch"]]
    x.lim <- obj[["x.lim"]]
    y.lim <- obj[["y.lim"]]
    y.min <- y.lim[1]
    y.max <- y.lim[2]
    xlabels <- obj[["xlabels"]]
    ylabels <- obj[["ylabels"]]
    y2labels <- obj[["y2labels"]]
    events <- unique(as.character(obj[["events"]]))

    ## (2) Draw empty canvas
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##dev.new(width = window.size$width, height = window.size$height)
    par(mar = c(8, 4, 4, 4) + 0.1)
    plot(xaxis, rep(y.lim, length.out = length(xaxis)),
         type = "n",
         xlim = x.lim, xaxt = "n", xlab = x.lab,
         ylim = y.lim, yaxt = "n", ylab = "")
    axis(side = 1, at = xlabels$at, labels = xlabels$label, las = 2)
    axis(side = 2, at = ylabels$at, labels = ylabels$label, las = 1)
    ## add 0-base line
    abline(h = 0, lty = 1, lwd = 2.5, col = 1)
    ## add title
    title(main = plot.title)
    ## add axis titles

    ## add secondary y axis
    mtext(side = 2, line = 3, text = y1.lab)
    if(!is.null(y2labels)) {
        axis(side = 4, at = y2labels$at, labels = y2labels$label, las = 1)
        mtext(side = 4, line = 3, text = y2.lab)
    }

    ## add legend
    pars.draw <- getEventParameters()[events, ]
    if(nrow(pars.draw)>0) {
        legend("bottom", inset = c(0, -0.35), horiz = TRUE,
               legend = pars.draw$description,
               col = pars.draw$color,
               lty = pars.draw$linetype,
               lwd = 2, cex = 0.7, xpd = TRUE)
    }

    ## (3) Add different layers
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## text
    out <- lapply(obj[["text"]], function(i) {
        text(x = i$x, y = i$y, labels = i$labels, cex = i$cex)
        abline(v = i$x, lty = 2, col = "grey")
    })

    ## lines
    out <- lapply(obj[["lines"]], function(i) {
        apply(i, 1, FUN = function(x){
            lines(as.numeric(x[c("x0", "x1")]), as.numeric(x[c("yStart", "yEnd")]),
                  lty = i$lty, lwd = i$lwd, col = as.character(i$col))
        })
    })

    ## arrows
    out <- lapply(obj[["arrows"]], function(i) {
        arrows(x0 = i$x0, y0 = i$yStart,
               x1 = i$x1, y1 = i$yEnd,
               length = 0.1,
               lty = i$lty, lwd = i$lwd, col = as.character(i$col))
    })

    ## cycles
    out <- lapply(obj[["cycles"]], function(i) {
        lines(i$x, i$y, lty = i$lty, lwd = i$lwd, col = as.character(i$col))
    })

}

drawCombinedCTGraphic <- function(obj) {

    ## (1) extract graphics parameters
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
    plot.title <- obj[["title"]]
    ## x axis
    x.lab <- obj[["x.lab"]]
    xaxis <- obj[["xaxis"]]
    x.stretch <- obj[["x.stretch"]]
    x.lim <- obj[["x.lim"]]
    xlabels <- obj[["xlabels"]]
    ## y axis
    y.lim <- obj[["y11.lim"]]
    y.min <- y.lim[1]
    y.max <- y.lim[2]
    y11.lab <- obj[["y11.lab"]]
    y12.lab <- obj[["y12.lab"]]
    y11labels <- obj[["y11labels"]]
    y12labels <- obj[["y12labels"]]
    y21.lab <- obj[["y21.lab"]]
    y22.lab <- obj[["y22.lab"]]
    y21labels <- obj[["y21labels"]]
    y22labels <- obj[["y22labels"]]
    y31.lab <- obj[["y31.lab"]]
    y32.lab <- obj[["y32.lab"]]
    y31labels <- obj[["y31labels"]]
    y32labels <- obj[["y32labels"]]
    events <- unique(as.character(obj[["events"]]))

    ## (2) Draw empty canvas
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##dev.new(width = window.size$width, height = window.size$height)
    ##win.graph(width = 20, height = 15)
    mar2 <- mar4 <- 8

    ## if we draw only one primary y axis, adapt margin
    if(is.null(y12labels)) {
        mar2 <- 4
    }
     if(is.null(y22labels)) {
       mar4 <- 4
     }
    par(mar = c(8, mar2, 4, mar4) + 0.1)
    plot(xaxis, rep(y.lim, length.out = length(xaxis)),
         type = "n",
         xlim = x.lim, xaxt = "n", xlab = x.lab,
         ylim = y.lim, yaxt = "n", ylab = "")

    ## (3) x (time) axis
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
    axis(side = 1, at = xlabels$at, labels = xlabels$label, las = 2)
    ## no axis title

    ## (4) y axis (primary, secondary and if specified tertiary)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## i. primary y axis
    line = 0
    if(!is.null(y12labels)) {
        ## second primary y axis
        axis(side = 2, at = y12labels$at, labels = y12labels$label, las = 1,
             line = line)
        mtext(side = 2, line = line + 2.5, text = y12.lab)
        line = 3.5
    }
    ## first primary y axis
    axis(side = 2, at = y11labels$at, labels = y11labels$label, las = 1, line = line)
    ## axis title
    mtext(side = 2, line = line + 3, text = y11.lab)

    ## ii. secondary y axis
    ## first secondary y axis
    axis(side = 4, at = y21labels$at, labels = y21labels$label, las = 1, line = 0)
    mtext(side = 4, line = line+2.5, text = y21.lab)


    if(!is.null(y22labels)) {
      axis(side = 4, at = y22labels$at, labels = y22labels$label, las = 1, line=4)
    }

    ## second secondary y axis
    ## iii. tertiary y axis
    if(!is.null(y31labels)) {
        ## first secondary y axis
      axis(side = 4, at = y31labels$at, labels = y31labels$label, las = 1)
      #axis(side = 4, at = y31labels$at, labels = y31labels$label, las = 1, line = 6.0)
        ## second secondary y axis
        axis(side = 4, at = y32labels$at, labels = y32labels$label, las = 1)

        ## axis titles
        y21.lab <- "C1, C2: Notional/Principal"
        y22.lab <- "C1, C2: Cyclical Cashflows"

    ## axis titles
    mtext(side = 4, line = 4.4 + 2.5, text = y21.lab)

    }


    if(!is.null(y22labels)) {
    #mtext(side = 4, line = 2.5, text = y22.lab)
    mtext(side = 4, line = 7, text = y22.lab)
    }

    ## (5) add 0 base line
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
    abline(h = 0, lty = 1, lwd = 2.5, col = 1)

    ## (6) add graphic title
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
    title(main = plot.title)

    ## (8) add legend
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
    pars.draw <- getEventParameters()[events, ]
    if(nrow(pars.draw)>0) {
        legend("bottom", inset = c(0, -0.29), horiz = TRUE,
               legend = pars.draw$description,
               col = pars.draw$color,
               lty = pars.draw$linetype,
               lwd = 2, cex = 0.6, xpd = TRUE)
    }

    ## (9) Add different layers
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## text
    out <- lapply(obj[["text"]], function(i) {
        text(x = i$x, y = i$y, labels = i$labels, cex = i$cex)
        abline(v = i$x, lty = 2, col = "grey")
    })

    ## arrows
    out <- lapply(obj[["arrows"]], function(i) {
        arrows(x0 = i$x0, y0 = i$yStart,
               x1 = i$x1, y1 = i$yEnd,
               length = 0.1,
               lty = i$lty, lwd = i$lwd, col = as.character(i$col))
    })

    ## lines
    out <- lapply(obj[["lines"]], function(i) {
        apply(i, 1, FUN = function(x){
            lines(as.numeric(x[c("x0", "x1")]), as.numeric(x[c("yStart", "yEnd")]),
                  lty = i$lty, lwd = i$lwd, col = as.character(i$col))
        })
    })

    ## cycles
    out <- lapply(obj[["cycles"]], function(i) {
        lines(i$x, i$y, lty = i$lty, lwd = i$lwd, col = as.character(i$col))
    })

}

initializeBasicCTGraphic <- function(rawdata, start, end, by) {

  ## (1) Define axis
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~

  ## define time axis
  ## (as indicated by method arguments start, end, by)
  timeaxis <- as.character(seq(timeDate(start), as.timeDate(end), by = by))
  ## stretch timeaxis in order to use additional points between
  ## dates as rr-cycle support, of the stretched timeaxis we get
  ## our xaxis
  x.stretch <- 10
  xaxis <- seq(as.numeric(as.timeDate(timeaxis[1])), by = 1,
               length.out = (x.stretch * length(timeaxis)))
  x.range <- c(min(xaxis), max(xaxis))

  ## (2) define y-axis range
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
  y1toy2 <- 0.5 ## ratio between y1 and y2 scales
  y1Data <- c(as.numeric(subset(rawdata, subset =
             type %in% c("CDD", "IED", "PRD", "TD", "MD",
                           "OPS","DPR","RES","ETA","ITF"))[,"payoff"]))
  if(!class(rawdata$nominalValue)=="NULL")
  {
    y1Data<-c(y1Data,as.numeric(rawdata[, "nominalValue"]))
  }
  y.max <- max(abs(y1Data), na.rm = TRUE)
  y.max <- 10*ceiling(y.max/10)
  y.min <- -10*ceiling(0.2 * y.max/10)
  y.range <- c(y.min, y.max)

  ## (3) define x-/y-axis ticks and labels
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
  x.tck <- sort(as.numeric(unique(as.timeDate(rawdata$time))))
  x.tck <- x.tck[which(x.tck %in% as.numeric(as.timeDate(timeaxis)), arr.ind = TRUE)]
  x.lbl <- timeaxis[which(as.numeric(as.timeDate(timeaxis)) %in% x.tck, arr.ind = TRUE)]
  xlabels <- data.frame(at = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], x.tck))),
                        label = x.lbl)
  y.tck <- seq(0, y.max, length.out = 5)
  y.lbl <- y.tck
  ylabels <- data.frame(at = y.tck, label = y.lbl)

  ## add secondary y axis (if any data should be drawn on this)
  y2Data <- c(as.numeric(subset(rawdata, subset =
                                  type %in% c("IP", "IPCI", "PR", "DV", "MR", "STD","DPR"))[, "payoff"]))
  if(length(y2Data) > 0) {
    y2.max <- max(abs(y2Data), na.rm = TRUE)
    if(y2.max>0) {
      y2.max <- 10*ceiling(y2.max/10)
      y2.range <- c(y.min, y2.max)
      y2.scale <- y1toy2 * y.max / y2.max
      y2.tck <- seq(0, y2.max, length.out = 5)
      y2.lbl <- y2.tck
      y2labels <- data.frame(at = y2.scale * y2.tck, label = y2.lbl)
    } else {
      y2.range <- y.range
      y2.scale <- y1toy2
      y2labels <- ylabels
    }
  } else {
    y2.range <- NULL
    y2.scale <- NULL
    y2labels <- NULL
  }

  ## add scaling y axis (if any data should be drawn on this)
  yscData <- c(as.numeric(subset(rawdata, subset =
                                   type %in% c("SC"))[, "payoff"]))
  if(length(yscData) > 0) {
    ysc.max <- max(abs(yscData), na.rm = TRUE)
    if(ysc.max>0) {
      ysc.max <- ceiling(ysc.max)
      ysc.range <- c(y.min, ysc.max)
      ysc.scale <- y1toy2 * (y.max-y.max*1/3) / ysc.max
      ysc.tck <- seq(0, ysc.max, length.out = 4)
      ysc.lbl <- ysc.tck
      ysclabels <- data.frame(at = (ysc.scale * ysc.tck)+y.max*2/3, label = ysc.lbl)
    } else {
      ysc.range <- y.range
      ysc.scale <- y1toy2
      ysclabels <- ylabels
    }
  } else {
    ysc.range <- NULL
    ysc.scale <- NULL
    ysclabels <- NULL
  }

  ## (4) create graphics object/a list of all parameters and data
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
  obj <- list(title = "Standardized Contract Type",
              x.lab = "", y1.lab = "Principal/Notional", y2.lab = "Cyclical Cashflows", ysc.lab = "Scaling",
              xaxis = xaxis, x.stretch = x.stretch, x.lim = x.range,
              y.lim = y.range, y2.lim = y2.range, y2.scale = y2.scale,ysc.lim = ysc.range, ysc.scale = ysc.scale,
              xlabels = xlabels, ylabels = ylabels, y2labels = y2labels,ysclabels = ysclabels,
              text = list(),
              lines = list(),
              arrows = list())

  ## add ad0 event only in text form
  ad0Data <- subset(rawdata, subset = type == "AD0")
  if(nrow(ad0Data) > 0) {
    text <- "AD0"
    x.pos <- xaxis[1] + x.stretch *
      cumsum(diff(c(xaxis[1], as.numeric(as.timeDate(ad0Data$time)))))
    y.pos <- y.min / 1.5
    obj[["text"]][[length(obj[["text"]])+1]] <-
      data.frame(x = x.pos, y = y.pos, labels = text, cex = 0.8)
    obj[["events"]] <- "AD0"
  }

  ## return graphics object
  return(obj)
}

initializeCombinedCTGraphic <- function(parent, children, start, end, by) {

  ## (1) Define x-axis
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## define time axis
  ## (as indicated by method arguments start, end, by)
  timeaxis <- as.character(seq(timeDate(start), as.timeDate(end), by = by))
  ## stretch timeaxis in order to use additional points between
  ## dates as rr-cycle support, of the stretched timeaxis we get
  ## our xaxis
  x.stretch <- 10
  xaxis <- seq(as.numeric(as.timeDate(timeaxis[1])), by = 1,
               length.out = (x.stretch * length(timeaxis)))
  x.range <- c(min(xaxis), max(xaxis))
  rawdata <- rbind(parent, children[[1]])
  x.tck <- sort(as.numeric(unique(as.timeDate(rawdata$time))))
  x.tck <- x.tck[which(x.tck %in% as.numeric(as.timeDate(timeaxis)), arr.ind = TRUE)]
  x.lbl <- timeaxis[which(as.numeric(as.timeDate(timeaxis)) %in% x.tck, arr.ind = TRUE)]
  xlabels <- data.frame(at = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], x.tck))),
                        label = x.lbl)

  ## (2) define y-axis range
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##     i) primary y axis (left hand side in the graphic) is parent
  y11toy12 <- 0.5 ## ratio between y1.1 and y1.2 scales (of secondary y-axis)
  rawdata <- parent
  y11Data <- abs(c(as.numeric(subset(rawdata, subset =
                                       type %in% c("CDD", "IED", "PRD", "TD", "MD",
                                                   "STD", "OPPD", "OPXED"))[,"payoff"])))
  if(!class(rawdata$NominalValue)=="NULL")
  {
    y11Data<-abs(c(y11Data,as.numeric(rawdata[, "nominalValue"])))
  }
  if(max(y11Data)==0) {
    y11Data <- unlist(lapply(children, FUN = function(x) {max(abs(x[,c("Value", "nominalValue")]))}))
  }
  y12Data <- abs(c(as.numeric(subset(rawdata, subset =
                                       type %in% c("MR"))[, "payoff"])))
  if(length(y12Data)==0) {
    y12Data <- 0
  }
  y11.max <- max(y11Data, na.rm = TRUE)
  y12.max <- max(y12Data, na.rm = TRUE)
  y11.max <- 10*ceiling(y11.max/10)
  y12.max <- 10*ceiling(y12.max/10)
  ##
  ##       decide whether parent events are spread over two separate
  ##       scales or not -> if yes, create a second primary y axis!
  if(y12.max == 0 || abs(y11.max - y12.max) <= 0.5 * y12.max) {
    y12.tck <- NULL
    y12.lbl <- NULL
    y12labels <- NULL
    y11.lab <- "Parent Cashflows"
    y12.lab <- NULL
    y12.scale <- 1
    y11.max <- max(y11.max, y12.max)
  } else {
    y12.scale <- y11toy12 * y11.max / y12.max
    y12.tck <- seq(0, y12.max, length.out = 5)
    y12.lbl <- y12.tck
    y12labels <- data.frame(at = y12.scale * y12.tck, label = y12.lbl)
    y11.lab <- "P: Notional/Principal"
    y12.lab <- "P: Margining"
  }
  ##       axis 1 of primary y axis (notional values)
  if(length(children) == 1) {
    y.min <- -10*ceiling(0.2 * y11.max/10)
  } else {
    y.min <- -y11.max
  }
  y11.range <- c(y.min, y11.max)
  y11.tck <- seq(0, y11.max, length.out = 5)
  y11.lbl <- y11.tck
  y11labels <- data.frame(at = y11.tck, label = y11.lbl)
  y12.range <- c(y.min, y12.max)

  ##     ii) secondary 1-y axis (upper right hand side in the graphic) is child
  ##         -secondary 2.1 is notional of child 1
  ##         -secondary 2.2 is cyclical cash flows of child 1
  y1toy2 <- 0.6
  y21toy22 <- 0.5 ## ratio between y1.1 and y1.2 scales (of secondary y-axis)
  rawdata <- children[[1]]
  y21Data <- c(as.numeric(subset(rawdata, subset =
                                   type %in% c("CDD", "IED", "PRD", "TD", "MD"))[,"payoff"]))
  if(length(y21Data)==0) {
    y21Data <- 0
  }
  if(!class(rawdata$NominalValue)=="NULL")
  {
    y21Data<-c(y21Data,as.numeric(rawdata[,"nominalValue"]))
  }

  y21.max <- max(abs(y21Data), na.rm = TRUE)
  y21.max <- 10*ceiling(y21.max/10)
  y21.range <- c(y.min, y21.max)
  if(!y21.max==0)
  {
    y21.scale <- y1toy2 * y11.max / y21.max
  }else
  {
    y21.scale<-0
  }
  y21.tck <- seq(0, y21.max, length.out = 5)
  y21.lbl <- y21.tck
  y21labels <- data.frame(at = y21.scale * y21.tck, label = y21.lbl)

  y22Data <- c(as.numeric(subset(rawdata, subset =
                                   type %in% c("IP", "PR", "IPCI", "DV"))[, "payoff"]))

  ##         in case of a Stock underlying, we don't want to consider the notional
  ##         for definition of axis
  #   if(tolower(gsub(" ", "", child2Type)) %in%
  #        c("stk", "stock")) {
  #     y22Data <- as.numeric(subset(rawdata, subset =
  #                                    type %in% c("CDD", "IED", "PRD", "TD", "MD"))[,"payoff"])
  #   }

  if(length(y22Data) > 0) {
    y22.max <- max(abs(y22Data), na.rm = TRUE)
    y22.max <- 10*ceiling(y22.max/10)
    y22.range <- c(y.min, y22.max)
    if(!y22.max==0)
    {
      y22.scale <- y1toy2 * y21toy22 * y11.max / y22.max
    }else
    {
      y22.scale<-0
    }

    y22.tck <- seq(0, y22.max, length.out = 5)
    y22.lbl <- y22.tck
    y22labels <- data.frame(at = y22.scale * y22.tck, label = y22.lbl)
  } else {
    y22.range <- NULL
    y22.scale <- NULL
    y22labels <- NULL
  }

  ## (3) create graphics object/a list of all parameters and data
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
  obj <- list(title = "Standardized Contract :",
              x.lab = "",
              y11.lab = y11.lab, y12.lab = y12.lab,
              y21.lab = "C1: Notional/Principal", y22.lab = "C1: Cyclical Cashflows",
              y31.lab = NULL, y32.lab = NULL,
              xaxis = xaxis, x.stretch = x.stretch, x.lim = x.range,
              y11.lim = y11.range, y12.lim = y12.range,
              y21.lim = y21.range, y22.lim = y22.range,
              y31.lim = NULL, y32.lim = NULL,
              y12.scale = y12.scale,
              y21.scale = y21.scale, y22.scale = y22.scale,
              y31.scale = NULL, y32.scale = NULL,
              xlabels = xlabels, y11labels = y11labels, y12labels = y12labels,
              y21labels = y21labels, y22labels = y22labels,
              y31labels = NULL, y32labels = NULL,
              events = character(),
              text = list(),
              lines = list(),
              arrows = list())

  ## (2) define y-axis of second child
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##     secondary 2-y axis (lower right hand side in the graphic) is child 2
  ##     -secondary 3.1 is notional of child 2
  ##     -secondary 3.2 is cyclical cash flows of child 2
  if(length(children)>1) {
    y1toy3 <- (-1) * y1toy2 ## -1 term forces the lines/arrows on the lower half graph
    y31toy32 <- y21toy22 ## ratio between y3.1 and y3.2 scales (of secondary y-axis)
    rawdata <- children[[2]]
    y31Data <- c(as.numeric(subset(rawdata, subset =
                                     type %in% c("CDD", "IED", "PRD", "TD", "MD"))[,"payoff"]))
    if(!class(rawdata$NominalValue)=="NULL")
    {
      y31Data<-c(y31Data,as.numeric(rawdata[,"nominalValue"]))
    }

    y32Data <- c(as.numeric(subset(rawdata, subset =
                                     type %in% c("IP", "PR", "IPCI", "DV"))[, "payoff"]))

    if(length(y32Data) > 0) {
      y32.max <- max(abs(y32Data), na.rm = TRUE)
      y32.max <- 10*ceiling(y32.max/10)
      y32.range <- c(y.min, y32.max)
      if(!y32.max==0)
      {
        y32.scale <- y1toy3 * y31toy32 * y11.max / y32.max
      }else
      {
        y32.scale<-0
      }

      y32.tck <- seq(0, y32.max, length.out = 5)
      y32.lbl <- y32.tck
      y32labels <- data.frame(at = y32.scale * y32.tck, label = y32.lbl)
    } else {
      y32.range <- NULL
      y32.scale <- NULL
      y32labels <- NULL
    }

    y31.max <- max(abs(y31Data), na.rm = TRUE)
    y31.max <- 10*ceiling(y31.max/10)
    y31.range <- c(y.min, y31.max)
    if(!y31.max==0)
    {
      y31.scale <- y1toy3 * y11.max / y31.max
    }else
    {
      y31.scale<-0
    }
    y31.tck <- seq(0, y31.max, length.out = 5)
    y31.lbl <- y31.tck
    y31labels <- data.frame(at = y31.scale * y31.tck, label = y31.lbl)

    ## add y-axis of the second child to the graph object
    obj[["y31.lab"]] <- "C2: Notional/Principal"
    obj[["y32.lab"]] <- "C2: Cyclical Cashflows"
    obj[["y31.lim"]] <- y31.range
    obj[["y32.lim"]] <- y32.range
    obj[["y31.scale"]] <- y31.scale
    obj[["y32.scale"]] <- y32.scale
    obj[["y31labels"]] <- y31labels
    obj[["y32labels"]] <- y32labels
  }

  ## add ad0 event only in text form
  ad0Data <- subset(parent, subset = type == "AD0")
  if(nrow(ad0Data) > 0) {
    text <- "AD0"
    x.pos <- xaxis[1] + x.stretch *
      cumsum(diff(c(xaxis[1], as.numeric(as.timeDate(ad0Data$time)))))
    y.pos <- y.min / 2
    obj[["text"]][[length(obj[["text"]])+1]] <-
      data.frame(x = x.pos, y = y.pos, labels = text, cex = 0.8)
    obj[["events"]] <- unique("AD0")
  }

  ## return graphics object
  return(obj)
}

addNotionalPrincipalStateLayer <- function(obj, rawdata, axis) {
    ## what is the axis to draw on?
    if(axis == "NULL") {
        y1.lim <- obj[["y.lim"]]
        y1.scale <- 1
    } else if(axis == "P") {
        y1.lim <- obj[["y11.lim"]]
        y1.scale <- obj[["y11.scale"]]
    } else if(axis == "C1") {
        y1.lim <- obj[["y21.lim"]]
        y1.scale <- obj[["y21.scale"]]
    } else if(axis == "C2") {
        y1.lim <- obj[["y31.lim"]]
        y1.scale <- obj[["y31.scale"]]
    } else {
        stop("Please give a valid level: {NULL, P, C1, C2}!")
    }

    ## extract graphics-parameters
    xaxis <- obj[["xaxis"]]
    x.stretch <- obj[["x.stretch"]]
    y.min <- y1.lim[1]
    y.max <- y1.lim[2]
    y.scale <- y1.scale

    ## transform to data frame
    df <- as.data.frame(rawdata)

    ## get graphical parameters
    pars <- getEventParameters()["NominalValue", ]  #FNP Nom->nom

    ## extract layer relevant data and bring in graphics form
    ## notice that we only need AD0 on Parent level (in a combined contract)
    subs <- c("IED", "PRD", "IPCI", "PR", "MD", "TD","DPR","ETA","ITF")
    if(!(axis %in% c("C1", "C2"))) {
        subs <- c("AD0", subs)
    }
    data <- subset(x = rawdata, subset = type %in% subs)[,c("time", "type", "payoff", "nominalValue")]
    ## if corresponding events (at least 2 for states) exist, add to the graphic
    if(nrow(data) > 1) {
        xStart <- as.numeric(as.timeDate(matrix(rbind(data$time[-nrow(data)],
                          data$time[-1]), ncol = 1, byrow = FALSE)))
        xEnd <- as.numeric(as.timeDate(matrix(rbind(data$time[-1],
                          data$time[-1]), ncol = 1, byrow = FALSE)))
        x0 <- xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xStart)))
        x1 <- xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xEnd)))
        yStart <- y.scale * abs(matrix(rbind(data$nominalValue[-nrow(data)],
                          data$nominalValue[-nrow(data)]), ncol = 1, byrow = FALSE))
        yEnd <- y.scale * abs(matrix(rbind(data$nominalValue[-nrow(data)],
                          data$nominalValue[-1]), ncol = 1, byrow = FALSE))
        type <- matrix(rbind(as.character(data$type[-nrow(data)]),
                          as.character(data$type[-1])), ncol = 1, byrow = FALSE)
        ntData <- data.frame(x0 = x0, x1 = x1,
                             yStart = yStart, yEnd = yEnd,
                             Type = type,
                             lty = pars$linetype,
                             lwd = pars$linewidth,
                             col = pars$color                             )
        ntData$Type <- as.character(ntData$Type)

        ## add graphical parameters
        obj[["lines"]][[length(obj[["lines"]])+1]] <- ntData
    }

    ## return the updated object
    return(obj)
}


addNotionalPrincipalPaymentLayer <- function(obj, rawdata, axis) {
    ## what is the axis to draw on?
    if(axis == "NULL") {
        y1.lim <- obj[["y.lim"]]
        y1.scale <- 1
    } else if(axis == "P") {
        y1.lim <- obj[["y11.lim"]]
        y1.scale <- 1
    } else if(axis == "C1") {
        y1.lim <- obj[["y21.lim"]]
        y1.scale <- obj[["y21.scale"]]
    } else if(axis == "C2") {
        y1.lim <- obj[["y31.lim"]]
        y1.scale <- obj[["y31.scale"]]
    } else {
        stop("Please give a valid level: {NULL, P, C11, C21}!")
    }

    ## extract graphics-parameters
    xaxis <- obj[["xaxis"]]
    x.stretch <- obj[["x.stretch"]]
    y.min <- y1.lim[1]
    y.max <- y1.lim[2]
    y.scale <- y1.scale

    ## transform to data frame
    df <- as.data.frame(rawdata)

    ## get graphical parameters
    if ("OPS" %in% df$type) {
      pars <- getEventParameters()["OPS", ]
    }else {
      pars <- getEventParameters()["IED", ]
    }


    ## extract layer relevant data and bring in graphics form
    data <- subset(x = df, subset = type %in% c("IED", "MD", "PRD", "TD",
                           "STD", "OPPD", "OPXED","OPS","RES","ETA"))

    if(nrow(data) > 0) {
        ## prepare x and y positions of cashflows
        yStart <- y.scale * abs(data$payoff)
        yStart[which(data$payoff<0)] <- 0
        yEnd <- y.scale * abs(data$payoff)
        yEnd[which(data$payoff>0)] <- 0
        xStart <- as.numeric(as.timeDate(data$time))
        xEnd <- as.numeric(as.timeDate(data$time))
        ntData <- data.frame(x0 = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xStart))),
                             x1 = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xEnd))),
                             yStart = yStart, yEnd = yEnd, Type = data$type,
                             lty = pars$linetype,
                             lwd = pars$linewidth,
                             col = pars$color)
        ## prepare x and y positions of event text
        text <- as.character(ntData$Type)
        x.pos <- ntData$x0
        y.pos <- y.min / 2
        ## do only draw arrows for events with values > 0
        ntData <- subset(x = ntData, subset = yStart != yEnd)
        ## add event text and cashflows (arrows) to the graphic object
        obj[["text"]][[length(obj[["text"]])+1]] <-
            data.frame(x = x.pos, y = y.pos, labels = text, cex = 0.8)
        obj[["arrows"]][[length(obj[["arrows"]])+1]] <- ntData
        obj[["events"]] <- unique(c(obj[["events"]], as.character(data$type)))
    }

    ## add cdd event only in text form
    cddData <- subset(df, subset = type == "CDD")
    if(nrow(cddData) > 0) {
        text <- "CDD"
        x.pos <- xaxis[1] + x.stretch *
            cumsum(diff(c(xaxis[1], as.numeric(as.timeDate(cddData$time)))))
        y.pos <- y.min / 1.5
        obj[["text"]][[length(obj[["text"]])+1]] <-
            data.frame(x = x.pos, y = y.pos, labels = text, cex = 0.8)
        obj[["events"]] <- unique(c(obj[["events"]], "CDD"))
    }


    ## return the updated object
    return(obj)
}


addInterestPaymentLayer <- function(obj, rawdata, axis) {

    ## what is the axis to draw on?
    if(axis == "NULL") {
        y2.lim <- obj[["y2.lim"]]
        y2.scale <- obj[["y2.scale"]]
    } else if(axis == "P") {
        y2.lim <- obj[["y12.lim"]]
        y2.scale <- obj[["y12.scale"]]
    } else if(axis == "C1") {
        y2.lim <- obj[["y22.lim"]]
        y2.scale <- obj[["y22.scale"]]
    } else if(axis == "C2") {
        y2.lim <- obj[["y32.lim"]]
        y2.scale <- obj[["y32.scale"]]
    } else {
        stop("Please give a valid level: {NULL, P, C11, C21}!")
    }

    ## extract graphics-parameters
    if(!is.null(y2.lim)) {
        xaxis <- obj[["xaxis"]]
        x.stretch <- obj[["x.stretch"]]
        y.max <- y2.lim[2]
        y.scale <- y2.scale
    }

    ## transform to data frame
    df <- as.data.frame(rawdata)

    ## get graphical parameters
    if ("DPR" %in% df$type) {
      pars <- getEventParameters()["DPR", ]
    } else {
      pars <- getEventParameters()["IP", ]
    }

    ## extract layer relevant data and bring in graphics form
    data <- subset(x = df, subset = type %in% c("IP","ITF","DPR"))
    ## aggregate raw data to timeaxis dates
    ## ytd ...
    if(nrow(data)>0) {
        ## prepare x and y positions of cashflows
        yStart <- y.scale * abs(data$payoff)
        yStart[which(data$payoff<0)] <- 0
        yEnd <- y.scale * abs(data$payoff)
        yEnd[which(data$payoff>0)] <- 0
        xStart <- as.numeric(as.timeDate(data$time))
        xEnd <- as.numeric(as.timeDate(data$time))
        ipData <- data.frame(x0 = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xStart))),
                             x1 = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xEnd))),
                             yStart = yStart, yEnd = yEnd, Type = data$type,
                             lty = pars$linetype,
                             lwd = pars$linewidth,
                             col = pars$color)
        ## prepare x and y positions of event text
        ## we always want to have the event name written above the arrow,
        ## no matter what direction it points to (in/outflow). Since, either
        ## yStart or yEnd of an arrow must be 0 by definition, we use their sum
        text <- as.character(ipData$Type)
        x.pos <- ipData$x0
        y.pos <- yStart + yEnd + y.scale*y.max/15
        ## do only draw arrows for events with values > 0
        ipData <- subset(x = ipData, subset = yStart != yEnd)
        ## add event text and cashflows (arrows) to the graphic object
        obj[["text"]][[length(obj[["text"]])+1]] <-
            data.frame(x = x.pos, y = y.pos, labels = text, cex = 0.8)
        obj[["arrows"]][[length(obj[["arrows"]])+1]] <- ipData
        obj[["events"]] <- c(obj[["events"]], "IP")
    }

    ## return the updated object
    return(obj)

}

addPrincipalRedemptionLayer <- function(obj, rawdata, axis) {
    ## what is the axis to draw on?
    if(axis == "NULL") {
        y2.lim <- obj[["y2.lim"]]
        y2.scale <- obj[["y2.scale"]]
    } else if(axis == "P") {
        y2.lim <- obj[["y12.lim"]]
        y2.scale <- obj[["y12.scale"]]
    } else if(axis == "C1") {
        y2.lim <- obj[["y22.lim"]]
        y2.scale <- obj[["y22.scale"]]
    } else if(axis == "C2") {
        y2.lim <- obj[["y32.lim"]]
        y2.scale <- obj[["y32.scale"]]
    } else {
        stop("Please give a valid level: {NULL, P, C11, C21}!")
    }

    ## extract graphics-parameters
    if(!is.null(y2.lim)) {
        xaxis <- obj[["xaxis"]]
        x.stretch <- obj[["x.stretch"]]
        y.max <- y2.lim[2]
        y.scale <- y2.scale
    }

    ## transform to data frame
    df <- as.data.frame(rawdata)

    ## get graphical parameters
    pars <- getEventParameters()["IED", ]

    ## extract layer relevant data and bring in graphics form
    data <- subset(x = df, subset = type %in% c("PR"))

    ## aggregate raw data to timeaxis dates
    ## ytd ...
    if(nrow(data)>0) {
        ## extract interest payment data (since we want to stack the two)
        ipData <- subset(x = df, subset = type %in% c("IP"))
        ipData <- ipData[as.character(ipData$time) %in% as.character(data$time), ]

        ## prepare x and y positions of cashflows
        yStart <- y.scale * (abs(data$payoff) + abs(ipData$payoff))
        yStart[which(data$payoff<0)] <- y.scale * abs(ipData$payoff)
        yEnd <- y.scale * (abs(data$payoff) + abs(ipData$payoff))
        yEnd[which(data$payoff>0)] <- y.scale * abs(ipData$payoff)
        xStart <- as.numeric(as.timeDate(data$time))
        xEnd <- as.numeric(as.timeDate(data$time))
        prData <- data.frame(x0 = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xStart))),
                             x1 = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xEnd))),
                             yStart = yStart, yEnd = yEnd, Type = data$type,
                             lty = pars$linetype,
                             lwd = pars$linewidth,
                             col = pars$color)
        ## prepare x and y positions of event text
        ## we always want to have the event name written above the arrow,
        ## no matter what direction it points to (in/outflow). Since, either
        ## yStart or yEnd of an arrow must be 0 by definition, we use their sum
        text <- as.character(prData$Type)
        x.pos <- prData$x0
        y.pos <- yStart + yEnd + y.scale*y.max/15
        ## do only draw arrows for events with values > 0
        prData <- subset(x = prData, subset = yStart != yEnd)
        ## add event text and cashflows (arrows) to the graphic object
        obj[["text"]][[length(obj[["text"]])+1]] <-
            data.frame(x = x.pos, y = y.pos, labels = text, cex = 0.8)
        obj[["arrows"]][[length(obj[["arrows"]])+1]] <- prData
        obj[["events"]] <- c(obj[["events"]], "PR")
    }

    ## return the updated object
    return(obj)

}

addCapitalisationLayer <- function(obj, rawdata, axis) {

    ## what is the axis to draw on?
    if(axis == "NULL") {
        y2.lim <- obj[["y2.lim"]]
        y2.scale <- obj[["y2.scale"]]
    } else if(axis == "P") {
        y2.lim <- obj[["y12.lim"]]
        y2.scale <- obj[["y12.scale"]]
    } else if(axis == "C1") {
        y2.lim <- obj[["y22.lim"]]
        y2.scale <- obj[["y22.scale"]]
    } else if(axis == "C2") {
        y2.lim <- obj[["y32.lim"]]
        y2.scale <- obj[["y32.scale"]]
    } else {
        stop("Please give a valid level: {NULL, P, C11, C21}!")
    }

    ## extract graphics-parameters
    if(!is.null(y2.lim)) {
        xaxis <- obj[["xaxis"]]
        x.stretch <- obj[["x.stretch"]]
        y.max <- y2.lim[2]
        y.scale <- y2.scale
    }

    ## transform to data frame
    df <- as.data.frame(rawdata)

    ## get graphical parameters
    pars <- getEventParameters()["IPCI", ]

    ## extract layer relevant data and bring in graphics form
    data <- subset(x = df, subset = type %in% c("IPCI"))
    ## aggregate raw data to timeaxis dates
    ## ytd ...
    if(nrow(data)>0) {
        ## prepare x and y positions of cashflows
        yStart <- y.scale * abs(data$payoff)
        yStart[which(data$payoff<0)] <- 0
        yEnd <- y.scale * abs(data$payoff)
        yEnd[which(data$payoff>0)] <- 0
        xStart <- as.numeric(as.timeDate(data$time))
        xEnd <- as.numeric(as.timeDate(data$time))
        ipciData <- data.frame(x0 = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xStart))),
                               x1 = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xEnd))),
                               yStart = yStart, yEnd = yEnd, Type = data$type,
                               lty = pars$linetype,
                               lwd = pars$linewidth,
                               col = pars$color)
        ## prepare x and y positions of event text
        ## we always want to have the event name written above the arrow,
        ## no matter what direction it points to (in/outflow). Since, either
        ## yStart or yEnd of an arrow must be 0 by definition, we use their sum
        text <- as.character(ipciData$type)
        x.pos <- ipciData$x0
        y.pos <- yStart + yEnd + y.scale*y.max/15
        ## do only draw arrows for events with values > 0
        ipciData <- subset(x = ipciData, subset = yStart != yEnd)
        ## add event text and cashflows (arrows) to the graphic object
        obj[["text"]][[length(obj[["text"]])+1]] <-
            data.frame(x = x.pos, y = y.pos, labels = text, cex = 0.8)
        obj[["arrows"]][[length(obj[["arrows"]])+1]] <- ipciData
        obj[["events"]] <- c(obj[["events"]], "IPCI")
    }

    ## return updated graphic object
    return(obj)
}


addInterestAccrualsLayer <- function(obj, rawdata, axis) {
    ## what is the axis to draw on?
    if(axis == "NULL") {
        y2.lim <- obj[["y2.lim"]]
        y2.scale <- obj[["y2.scale"]]
    } else if(axis == "P") {
        y2.lim <- obj[["y12.lim"]]
        y2.scale <- obj[["y12.scale"]]
    } else if(axis == "C1") {
        y2.lim <- obj[["y22.lim"]]
        y2.scale <- obj[["y22.scale"]]
    } else if(axis == "C2") {
        y2.lim <- obj[["y32.lim"]]
        y2.scale <- obj[["y32.scale"]]
    } else {
        stop("Please give a valid level: {NULL, P, C11, C21}!")
    }

    ## transform to data frame
    df <- as.data.frame(rawdata)

    ## extract graphics-parameters and if secondary y-axis should be drawn,
    ## prepare
    if(!is.null(y2.lim)) {
        xaxis <- obj[["xaxis"]]
        x.stretch <- obj[["x.stretch"]]
        y.max <- y2.lim[2]
        y.scale <- y2.scale

        ## extract and prepare relevant data
        pars <- getEventParameters()["IA", ]
        subs <- c("IED", "PRD", "IP", "IPCI", "RR", "TD","ETA","ITF")
        if(!("IED" %in% df$type | "PRD" %in% df$type)) {
            subs <- c("AD0", subs)
        }
        data <- subset(x = df, subset = type %in% subs)

        if(nrow(data) > 1) {
            ## for RR and TD events, replace event Value with state variable for accrued interest
            if(nrow(subset(x = data, subset = type == "RR")) > 0){
                data[which(data$type == "RR"), "payoff"] <- data[which(data$type == "RR"), "nominalAccrued"]

            }
            if(nrow(subset(x = data, subset = type == "TD")) > 0){
                data[which(data$type == "TD"), "payoff"] <- data[which(data$type == "TD"), "nominalAccrued"]

            }
            if(nrow(subset(x = data, subset = type == "ETA")) > 0){
              data[which(data$type == "ETA"), "payoff"] <- data[which(data$type == "ETA"), "nominalAccrued"]
            }
            if(nrow(subset(x = data, subset = type == "ITF")) > 0){
              data[which(data$type == "ITF"), "payoff"] <- data[which(data$type == "ITF"), "nominalAccrued"]
            }
            ## prepare line x and y coordinates
            xStart <- as.numeric(as.timeDate(data$time[1:(nrow(data)-1)]))
            xEnd <- as.numeric(as.timeDate(data$time[2:nrow(data)]))
            x0 <- xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xStart)))
            x1 <- xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xEnd)))
            yStart <- y.scale * abs(data$nominalAccrued[1:(nrow(data)-1)])
            yEnd <- y.scale * abs(data$payoff[2:nrow(data)])

            ## ... bring in standard layer format
            ipacData <- data.frame(x0 = x0, x1 = x1,
                                   yStart = yStart, yEnd = yEnd,
                                   Type = data[1:(nrow(data)-1), "type"],
                                   lty = pars$linetype, lwd = pars$linewidth,
                                   col = pars$color)
            ## add accrual lines
            obj[["lines"]][[length(obj[["lines"]])+1]] <- ipacData

            ## add accrual pseudo event if accruing is drawn
            obj[["events"]] <- c(obj[["events"]], "IA")
        }
    }
    ## return the updated object
    return(obj)

}


addRateResetLayer <- function(obj, rawdata, axis) {
    ## what is the axis to draw on?
    if(axis == "NULL") {
        y1.lim <- obj[["y.lim"]]
        y1.scale <- 1
        y.max <- y1.lim[1]
    } else if(axis == "P") {
        y1.lim <- obj[["y11.lim"]]
        y1.scale <- 1
        y.max <- y1.lim[1] * y1.scale
    } else if(axis == "C1") {
        y1.lim <- obj[["y11.lim"]]
        y1.scale <- obj[["y21.scale"]]
        y.max <- y1.lim[1] * y1.scale
        if(!is.null(obj[["y31.lim"]])) {
            y.max <- (-1) * y.max
        }
    } else if(axis == "C2") {
        y1.lim <- obj[["y11.lim"]]
        y1.scale <- obj[["y31.scale"]]
        y.max <- y1.lim[1] * (-1) * y1.scale
    } else {
        stop("Please give a valid level: {NULL, P, C11, C21}!")
    }

    ## transform to data frame
    df <- as.data.frame(rawdata)

    ## extract graphics-parameters
    xaxis <- obj[["xaxis"]]
    x.stretch <- obj[["x.stretch"]]
    y.max <- 0.33 * y.max

    ## extract and prepare relevant data
    data <- subset(x = df, subset = type %in% c("IED", "PRD", "RR", "RRY", "TD", "MD"))
    if(nrow(subset(x = data, subset = type %in% c("RR","RRY"))) > 0) {
        data$payoff[1] <- data$nominalRate[1]
        aux <- data.frame(xStart = as.numeric(as.timeDate(data$time[1:(nrow(data) - 1)])),
                          xEnd = as.numeric(as.timeDate(data$time[2:nrow(data)])),
                          Value = data$payoff[1:(nrow(data) - 1)],
                          Type = data$type[1:(nrow(data) - 1)])
        ## add event text
        data <- subset(x = df, subset = type %in% c("RR", "RRY"))
        text <- as.character(data$type)
        x.pos <- xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], as.numeric(as.timeDate(data$time)))))
        y.pos <- y.max
        obj[["text"]][[length(obj[["text"]])+1]] <- data.frame(x = x.pos, y = y.pos, labels = text, cex = 0.8)

        ## add rate reset cycles
        ## get graphical parameters
        pars <- getEventParameters()["RR", ]
        for(i in 1:nrow(aux)) {
            x <- aux[i, ]
            xVals <- xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], as.numeric(x[c("xStart", "xEnd")]))))
            x.eval <- subset(data.frame(x = xaxis), x > xVals[1] & x < xVals[2])[,1]
            x.std <- x.eval - min(x.eval)
            x.std <- x.std / max(x.std)
            y.sin <- y.max * sin(x.std * pi)
            rrData <- data.frame(x = x.eval, y = y.sin, lty = pars$linetype,
                                 lwd = pars$linewidth, col = pars$color)
            ## add to graph
            obj[["cycles"]][[length(obj[["cycles"]])+1]] <- rrData
        }
        ## add accrual event if rate resetting is drawn
        obj[["events"]] <- c(obj[["events"]], "RR")

    }

    ## return the updated object
    return(obj)
}


addDividendLayer <- function(obj, rawdata, axis) {
    ## what is the axis to draw on?
    if(axis == "NULL") {
        y2.lim <- obj[["y2.lim"]]
        y2.scale <- obj[["y2.scale"]]
    } else if(axis == "P") {
        y2.lim <- obj[["y12.lim"]]
        y2.scale <- obj[["y12.scale"]]
    } else if(axis == "C1") {
        y2.lim <- obj[["y22.lim"]]
        y2.scale <- obj[["y22.scale"]]
    } else if(axis == "C2") {
        y2.lim <- obj[["y32.lim"]]
        y2.scale <- obj[["y32.scale"]]
    } else {
        stop("Please give a valid level: {NULL, P, C11, C21}!")
    }

    ## extract graphics-parameters
    if(!is.null(y2.lim)) {
        xaxis <- obj[["xaxis"]]
        x.stretch <- obj[["x.stretch"]]
        y.max <- y2.lim[2]
        y.scale <- y2.scale
    }

    ## transform to data frame
    df <- as.data.frame(rawdata)

    ## get graphical parameters
    pars <- getEventParameters()["DV", ]

    ## extract layer relevant data and bring in graphics form
    data <- subset(x = df, subset = type %in% c("DV"))
    ## aggregate raw data to timeaxis dates
    ## ytd ...
    if(nrow(data)>0) {
        ## prepare x and y positions of cashflows
        yStart <- y.scale * abs(data$payoff)
        yStart[which(data$payoff<0)] <- 0
        yEnd <- y.scale * abs(data$payoff)
        yEnd[which(data$payoff>0)] <- 0
        xStart <- as.numeric(as.timeDate(data$time))
        xEnd <- as.numeric(as.timeDate(data$time))
        dvData <- data.frame(x0 = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xStart))),
                             x1 = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xEnd))),
                             yStart = yStart, yEnd = yEnd, Type = data$type,
                             lty = pars$linetype,
                             lwd = pars$linewidth,
                             col = pars$color)
        ## prepare x and y positions of event text
        ## we always want to have the event name written above the arrow,
        ## no matter what direction it points to (in/outflow). Since, either
        ## yStart or yEnd of an arrow must be 0 by definition, we use their sum
        text <- as.character(dvData$type)
        x.pos <- dvData$x0
        y.pos <- yStart + yEnd + y.scale*y.max/15
        ## do only draw arrows for events with values > 0
        dvData <- subset(x = dvData, subset = yStart != yEnd)
        ## add event text and cashflows (arrows) to the graphic object
        obj[["text"]][[length(obj[["text"]])+1]] <-
            data.frame(x = x.pos, y = y.pos, labels = text, cex = 0.8)
        obj[["arrows"]][[length(obj[["arrows"]])+1]] <- dvData
        obj[["events"]] <- c(obj[["events"]], "DV")
    }

    ## return the updated object
    return(obj)

}


addMarginingLayer <- function(obj, rawdata, axis) {
    ## what is the axis to draw on?
    if(axis == "NULL") {
        y2.lim <- obj[["y2.lim"]]
        y2.scale <- obj[["y2.scale"]]
    } else if(axis == "P") {
        y2.lim <- obj[["y12.lim"]]
        y2.scale <- obj[["y12.scale"]]
    } else if(axis == "C1") {
        y2.lim <- obj[["y22.lim"]]
        y2.scale <- obj[["y22.scale"]]
    } else if(axis == "C2") {
        y2.lim <- obj[["y32.lim"]]
        y2.scale <- obj[["y32.scale"]]
    } else {
        stop("Please give a valid level: {NULL, P, C11, C21}!")
    }

    ## extract graphics-parameters
    if(!is.null(y2.lim)) {
        xaxis <- obj[["xaxis"]]
        x.stretch <- obj[["x.stretch"]]
        y.max <- y2.lim[2]
        y.min <- y2.lim[1]
        y.scale <- y2.scale
    }

    ## transform to data frame
    df <- as.data.frame(rawdata)

    ## get graphical parameters
    pars <- getEventParameters()["MR", ]

    ## extract layer relevant data and bring in graphics form
    data <- subset(x = df, subset = type %in% c("MR"))
    ## aggregate raw data to timeaxis dates
    ## ytd ...
    if(nrow(data)>0) {
        ## prepare x and y positions of cashflows
        yStart <- y.scale * abs(data$payoff)
        yStart[which(data$payoff<0)] <- 0
        yEnd <- y.scale * abs(data$payoff)
        yEnd[which(data$payoff>0)] <- 0
        xStart <- as.numeric(as.timeDate(data$time))
        xEnd <- as.numeric(as.timeDate(data$time))
        mrData <- data.frame(x0 = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xStart))),
                             x1 = xaxis[1] + x.stretch * cumsum(diff(c(xaxis[1], xEnd))),
                             yStart = yStart, yEnd = yEnd, Type = data$type,
                             lty = pars$linetype,
                             lwd = pars$linewidth,
                             col = pars$color)
        ## prepare x and y positions of event text
        ## we always want to have the event name written above the arrow,
        ## no matter what direction it points to (in/outflow). Since, either
        ## yStart or yEnd of an arrow must be 0 by definition, we use their sum
        text <- as.character(mrData$type)
        x.pos <- mrData$x0
        y.pos <- yStart + yEnd + y.scale*y.max/15
        ## do only draw arrows for events with values > 0
        mrData <- subset(x = mrData, subset = yStart != yEnd)
        ## STD event gets a different y position (same as notional events)
        if("STD" %in% text) {
            y.pos[which(text == "STD")] <- y.min / 2
            obj[["events"]] <- c(obj[["events"]], "STD")
        }
        ## do only draw arrows for events with values > 0
        mrData <- subset(x = mrData, subset = yStart != yEnd)

        ## add event text and cashflows (arrows) to the graphic object
        obj[["text"]][[length(obj[["text"]])+1]] <-
            data.frame(x = x.pos, y = y.pos, labels = text, cex = 0.8)

        obj[["arrows"]][[length(obj[["arrows"]])+1]] <- mrData
        obj[["events"]] <- c(obj[["events"]], "MR")

    }

    ## return the updated object
    return(obj)
}


addChildLayers <- function(obj, childEvents, childCT, axis) {

    ## process child only if at least 1 event is in its event list!
    if(nrow(childEvents) > 0) {

        ## extract graphic object
        graph <- obj

        ## add child 1 layers according to child contract type:
        df <- as.data.frame(childEvents)
        type <- childCT
        if(tolower(gsub(" ", "", type)) %in%
           c("pam", "principalatmaturity", "ann", "annuity", "nam", "negativeamortizer",
             "lam", "linearamortizer", "lax", "exoticlinearamortizer")) { ## maturity CT

            ## add layers
            graph <- addNotionalPrincipalPaymentLayer(graph, df, axis = axis)
            graph <- addNotionalPrincipalStateLayer(graph, df, axis = axis)
            graph <- addPrincipalRedemptionLayer(graph, df, axis = axis)
            graph <- addInterestPaymentLayer(graph, df, axis = axis)
            graph <- addCapitalisationLayer(graph, df, axis = axis)
            graph <- addInterestAccrualsLayer(graph, df, axis = axis)
            graph <- addRateResetLayer(graph, df, axis = axis)
            if(axis == "C1") {
                graph[["y21.lab"]] <- "C1: Notional/Principal"
                graph[["y22.lab"]] <- "C1: Interest Payments"
            } else if(axis == "C2") {
                graph[["y31.lab"]] <- "C2: Notional/Principal"
                graph[["y32.lab"]] <- "C2: Interest Payments"
            }

        } else if(tolower(gsub(" ", "", type)) %in%
                  c("stk", "stock")) { ## obviously a Stock

            ## add layers
            graph <- addNotionalPrincipalPaymentLayer(graph, df, axis = axis)
            graph <- addDividendLayer(graph, df, axis = axis)
            if(axis == "C1") {
                graph[["y21.lab"]] <- "C1: Notional/Principal"
                graph[["y22.lab"]] <- "C1: Dividend Payments"
            } else if(axis == "C2") {
                graph[["y31.lab"]] <- "C2: Notional/Principal"
                graph[["y32.lab"]] <- "C2: Dividend Payments"
            }

        }
    }
    ## return graphic object
    return(graph)

}


getEventParameters <- function() {

    ## initialise event information
    descriptions <- c("CDD: Contract \nDeal Date",
                      "IED: Initial \nExchange Date",
                      "PRD: Purchase \nDate",
                      "IP: Interest \nPayment",
                      "Interest Accrual",
                      "IPCI: Interest \nCapitalisation",
                      "PR: Principal \nRedemption",
                      "RR: Rate \nResetting",
                      "MD: Maturity \nDate",
                      "TD: Termination \nDate",
                      "DV: Dividend \nPayment",
                      "MR: Margining \nPayment",
                      "STD: Settlement \nDate",
                      "OPPD: Option Premium \nPayment",
                      "OPXED: Option Exercise \nEnd Date",
                      "AD0: Analysis \nDate",
                      "OPS: Operational \nCashflow",
                      "DPR: Depreciation",
                      "RES: Reserves",
                      "ETA: External \nTransaction",
                      "ITF: Internal \nTransfer",
                      "NominalValue: ")
    colors <- c("black", "red", "red", "darkgreen", "darkgreen",
                "darkgreen", "red", "green", "red", "red", "blue",
                "darkblue", "darkblue", "red", "red", "black", "darkgreen",
                "darkgreen", "red", "red", "red", "red")
    linetypes <- c(1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,2,1,1,1,2)
    linewidths <- c(2, 2, 2, 1.5, 1.5, 1.5, 2, 1.5, 2, 2, 1.5, 1.5, 1.5, 2, 2, 2, 2,2,2,2,2,2)
    pars <- data.frame(description = descriptions,
                       color = as.character(colors),
                       linetype = linetypes,
                       linewidth = linewidths)
    pars$description <- as.character(descriptions)
    pars$color <- as.character(colors)
    rownames(pars) <- c("CDD", "IED", "PRD", "IP", "IA", "IPCI", "PR",
                        "RR", "MD", "TD", "DV", "MR", "STD", "OPPD", "OPXED", "AD0",
                        "OPS","DPR","RES","ETA","ITF","NominalValue")

    return(pars)
}


## ---------------------- End of plot ------------------------------


######################## End of Methods Definition #################
