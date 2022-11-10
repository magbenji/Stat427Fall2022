library(tibble)

plotModel <- function(data, sampleFraction, type = lm, model = y ~ x, ...){
  require(tibble)
  options(warn = -1)
  on.exit(options(warn = 0))
  
  model <- as.formula(model)
  optionalArgs <- list(...)
  
  if(is.null(data[["pch"]])) data$pch <- 1
  
  resample <- ifelse(is.null(optionalArgs$replace), F, optionalArgs$replace)
  
  sampleObs <- sample(nrow(data), nrow(data)*sampleFraction, replace = resample)
  sampData <- data[sampleObs,]
  
  if(identical(lm, type)){
    theMod <- lm(formula = model, data = data)
    altMod <- lm(formula = model, data = sampData)
  } else if(identical(glm, type)){
    theFamily <- ifelse(is.null(optionalArgs$family), "gaussian", optionalArgs$family)
    theMod <- glm(formula = model, data = data, family = theFamily)
    altMod <- glm(formula = model, data = sampData, family = theFamily)
  } else {
    stop("Model type must either be lm or glm." ,call.=F)
  }
  
  if(is.null(optionalArgs[["pointCols"]])){
    optionalArgs$pointCols <- 1:2 
  } else if(length(optionalArgs$pointCols) != 2){
    optionalArgs$pointCols <- rep(optionalArgs$pointCols, length = 2)
    warning("The number of colors for points in pointCols should be 2; adjusting vector length.")
  }
  
  #plot points
  plot(y ~ x, data=data, col = optionalArgs$pointCols[[1]], pch = data$pch, ...)
  points(y ~ x, data=sampData, col = optionalArgs$pointCols[[2]], pch = sampData$pch)
  
  if(is.null(optionalArgs[["lineTypes"]])){
    optionalArgs$lineTypes <- 1:2 
  } else if(length(optionalArgs$lineTypes) != 2){
    optionalArgs$lineTypes <- rep(optionalArgs$lineTypes, length = 2)
    warning("The number of line types in lineTypes should be 2; adjusting vector length.")
  }
  
  if(is.null(optionalArgs[["lineWidths"]])){
    optionalArgs$lineWidths <- rep(1,2) 
  } else if(length(optionalArgs$lineWidths) != 2){
    optionalArgs$lineWidths <- rep(optionalArgs$lineWidths, length = 2)
    warning("The number of line widths in lineWidths should be 2; adjusting vector length.")
  }
  
  if(is.null(optionalArgs[["lineCols"]])){
    optionalArgs$lineCols <- 1:2 
  } else if(length(optionalArgs$lineCols) != 2){
    optionalArgs$lineCols <- rep(optionalArgs$lineCols, length = 2)
    warning("The number of line colors in lineCols should be 2; adjusting vector length.")
  }
  
  plotLines <- tibble(
    x = seq(min(data$x), max(data$x), length = 100),
    y1 = predict(theMod, data.frame(x = x), type = "response"),
    y2 = predict(altMod, data.frame(x = x), type = "response")
  )
  
  lines(plotLines$x, plotLines$y1, lwd = optionalArgs$lineWidths[[1]], lty = optionalArgs$lineTypes[[1]], col = optionalArgs$lineCols[[1]])
  lines(plotLines$x, plotLines$y2, lwd = optionalArgs$lineWidths[[2]], lty = optionalArgs$lineTypes[[2]], col = optionalArgs$lineCols[[2]])
  
  legend("topleft", legend = c("Base Data and Model", "Subsampled Version"), text.col = "white",
         col = optionalArgs$lineCols, lty = optionalArgs$lineTypes, lwd = optionalArgs$lineWidths, pch = c(NA,NA))
  legend("topleft", legend = c("   Base Data and Model", "   Subsampled Version"), 
         col = optionalArgs$pointCols, lty = c(0,0), pch = 1, bty="n")
  
}

fauxData <- tibble(x = runif(100, 0, 10), y = x^2 + 100 + rnorm(100, x^2 + 100, 10) )
testData <- tibble(x = runif(100, -10, 10), y = rbinom(100,1,1/(exp(-x) + 1)))




