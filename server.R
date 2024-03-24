library(shiny)
library(tidyverse)
library(LearnBayes)
library(HDInterval)
library(spatstat.utils)

# Set conditions
nSim = 10^5

# Non-reactive Functions
## Calculate the shape value
fn.shape = function(slider, nGroup = 0, events = 0) {
  # Find the initial value based on the slider
  rawVal = LearnBayes::beta.select(list(x = slider[1]/100, p = 0.025),
                                   list(x = slider[2]/100, p = 0.975))
  
  # Calculate shape values (relevant for posterior)
  shape = c(rawVal[1] + events,
            rawVal[2] + nGroup - events)
  
  # Return
  shape
}

fn.plotPost = function(priorCon, priorInt, con1, con0, int1, int0, setHDI, setROPE) {
  #browser()
  # Tease these out
  conEvent = con1
  conN = con1 + con0
  intEvent = int1
  intN = int1 + int0
  
  # Calculate shape values
  shapePostCon = fn.shape(slider = priorCon,
                          nGroup = conN,
                          events = conEvent)
  shapePostInt = fn.shape(slider = priorInt,
                          nGroup = intN,
                          events = intEvent)
  
  # Simulate data
  data = data.frame(
    postCon = rbeta(n = nSim,
                    shape1 = shapePostCon[1],
                    shape2 = shapePostCon[2]),
    postInt = rbeta(n = nSim,
                    shape1 = shapePostInt[1],
                    shape2 = shapePostInt[2])
  ) |>
    mutate(postDiff = postInt - postCon)
  
  
  # Calculate function of the difference in the posterior distribution using approxfun()
  postDiffFun = data |>
    pull(postDiff) |>
    density() |>
    approxfun()
  
  
  # Calculate the highest density interval from the raw data
  hdi = data |>
    select(postDiff) |>
    HDInterval::hdi(credMass = setHDI/100) |>
    unname() |>
    round(digits = 2)
  
  # Calculate the range of the actual density distribution, to ensure we specify the ROPE limits correctly
  postDens = data |>
    pull(postDiff) |>
    density() |>
    getElement("x")
  
  # Work out the ROPE limits
  rangeROPE = vector(mode = "numeric",
                     length = 2L)
  
  rangeROPE[1] = max(min(postDens), -setROPE/100)
  rangeROPE[2] = min(max(postDens), setROPE/100)
  
  # Calculate area of the ROPE
  areaROPE = integrate(postDiffFun,
                       lower = rangeROPE[1],
                       upper = rangeROPE[2]) |>
    getElement("value")
  
  # Calculate area of overlap
  overlapDist = spatstat.utils::intersect.ranges(hdi, rangeROPE,
                                                 fatal = FALSE)
  
  overlapArea = ifelse(overlapDist[1] == overlapDist[2],
                       FALSE,
                       integrate(postDiffFun,
                                 lower = overlapDist[1],
                                 upper = overlapDist[2]) |>
                         getElement("value")
                       )
    
  # Generate Plot
  plot = ggplot(data = data.frame(x = c(-1, 1)),
                aes(x)) +
    # Vertical line at 0
    geom_vline(data = NULL,
               xintercept = 0,
               colour = "black",
               alpha = 0.3) +
    # Curve
    stat_function(fun = postDiffFun,
                  geom = "line",
                  colour = "#F8766D") +
    # HDI
    stat_function(fun = postDiffFun,
                  aes(fill = "95% HDI",
                      colour = "95% HDI"),
                  geom = "area",
                  xlim = c(hdi[1], hdi[2]),
                  alpha = 0.5) +
    # ROPE
    stat_function(fun = postDiffFun,
                  aes(fill = "ROPE",
                      colour = "ROPE"),
                  geom = "area",
                  xlim = c(-rangeROPE, rangeROPE),
                  alpha = 0.5) +
    # Cosmetics
    scale_x_continuous(expand = c(0, 0),
                       limits = c(min(postDens) * 1.1, max(postDens) * 1.1),
                       labels = scales::label_number(accuracy = 0.01),
                       breaks = c(hdi,
                                  rangeROPE,
                                  0),
                       guide = guide_axis(n.dodge = 2)) +
    scale_y_continuous(limits=c(0, NA),
                       expand=expansion(mult=c(0, 0.05))) +
    theme_light() +
    theme(axis.text.y = element_blank()) +
    labs(x = expression(theta[1] - theta[2]),
         fill = NULL,
         colour = NULL,
         linetype = NULL,
         y = NULL)
  
  # List outputs
  out = list("plot" = plot,
             "hdi" = hdi,
             "ropeRange" = rangeROPE,
             "ropeArea" = areaROPE,
             "overDist" = overlapDist,
             "overArea" = overlapArea)
  out
}

fn.plotPrior = function(priorCon, priorInt) {
  # Calculate shape values
  shapePriorCon = fn.shape(slider = priorCon)
  shapePriorInt = fn.shape(slider = priorInt)
  
  # Simulate data
  data = data.frame(
    priorCon = rbeta(n = nSim,
                     shape1 = shapePriorCon[1],
                     shape2 = shapePriorCon[2]),
    priorInt = rbeta(n = nSim,
                     shape1 = shapePriorInt[1],
                     shape2 = shapePriorInt[2])
  ) |>
    mutate(priorDiff = priorInt - priorCon)
  
  
  # Calculate function for diff in posterior dist
  priorDiffFun = data |>
    pull(priorDiff) |>
    density() |>
    approxfun()
  
  # Get the density ranges
  priorDens = data |>
    pull(priorDiff) |>
    density() |>
    getElement("x")
  
  # Generate Plot
  ggplot(data = data.frame(x = c(-1, 1)),
         aes(x)) +
    # Vertical line at 0
    geom_vline(data = NULL,
               xintercept = 0,
               colour = "black",
               alpha = 0.3) +
    # Curve
    stat_function(fun = priorDiffFun,
                  geom = "area",
                  alpha = 0.3,
                  colour = "darkgreen",
                  fill = "darkgreen") +
    # Cosmetics
    scale_x_continuous(expand = c(0, 0),
                       limits = c(min(priorDens) * 1.1, max(priorDens) * 1.1),
                       labels = scales::label_number(accuracy = 0.01)) +
    scale_y_continuous(limits=c(0, NA),
                       expand=expansion(mult=c(0, 0.05))) +
    theme_light() +
    theme(axis.text.y = element_blank()) +
    labs(x = expression(theta[1] - theta[2]),
         fill = NULL,
         colour = NULL,
         linetype = NULL,
         y = NULL)
}

# Define Server Logic
server = function(input, output, session) {
  
  #Row and column totals
  output$int_tot = renderText({
    input$int_neg + input$int_pos
  })
  
  output$con_tot = renderText({
    input$con_neg + input$con_pos
  })
  
  output$tot_pos = renderText({
    input$int_pos + input$con_pos
  })
  
  output$tot_neg= renderText({
    input$int_neg + input$con_neg
  })
  
  output$tot_tot = renderText({
    input$int_neg + input$con_neg + input$int_pos + input$con_pos
  })
  
  # Plots
  ## Plot Placeholder
  plotHold = reactiveValues(plotPost = NULL,
                            plotPrior = NULL,
                            hdi = NULL,
                            ropeRange = NULL,
                            ropeArea = NULL,
                            overlap = NULL)
  
  ## Button Observer
  observeEvent(input$plotButton, {
    posterior = fn.plotPost(priorCon = input$priorCon,
                            priorInt = input$priorInt,
                            con1 = input$con_pos,
                            con0 = input$con_neg,
                            int1 = input$int_pos,
                            int0 = input$int_neg,
                            setHDI = input$hdi,
                            setROPE = input$rope)
    
    plotHold$plotPost = posterior[["plot"]]
    plotHold$hdi = posterior[["hdi"]]
    plotHold$ropeRange = posterior[["ropeRange"]]
    plotHold$ropeArea = posterior[["ropeArea"]]
    plotHold$overDist = posterior[["overDist"]]
    plotHold$overArea = posterior[["overArea"]]
    
    
    plotHold$plotPrior = fn.plotPrior(priorCon = input$priorCon,
                                      priorInt = input$priorInt)
  })
  
  ## Actual Plots
  output$plotPost = renderPlot(plotHold$plotPost,
                               res = 96)
  
  output$plotPrior = renderPlot(plotHold$plotPrior,
                                res = 96)
  
  output$plotDesc = renderText({
    #browser()
    if(is.null(plotHold$overArea)){
      paste0("")
    } else if (plotHold$overArea == 0) {
      paste0("The ", input$hdi, "% HDI encompasses ", plotHold$hdi[1], " to ", plotHold$hdi[2],
             ". The ROPE covers ", round(plotHold$ropeRange[1], 2), " to ", round(plotHold$ropeRange[2], 2), ", and includes ",
             round(plotHold$ropeArea * 100, digits = 1), "% of the area under the curve.
             
             The HDI and ROPE do not overlap, suggesting a clinically significant result.")      
    } else {
      paste0("The ", input$hdi, "% HDI encompasses ", plotHold$hdi[1], " to ", plotHold$hdi[2],
             ". The ROPE covers ", round(plotHold$ropeRange[1], 2), " to ", round(plotHold$ropeRange[2], 2), ", and includes ",
             round(plotHold$ropeArea * 100, digits = 1), "% of the area under the curve. <br> <br>
             
             The HDI and ROPE overlap between ", plotHold$overDist[1], " and ", plotHold$overDist[2],
             ". This overlap covers ", round(plotHold$overArea * 100, 2), "% of the area under the curve, suggesting a clinically insignificant result.")
    }
  })
}