## Helper function to evaluate a string formula ------------------------------- 
EvalFormula <- function(k, expr_str) {
  .env <- new.env()
  sapply(c(k), function(k) {
    .env$k <- k
    eval(expr = parse(text = expr_str), 
         envir = .env)
  })
}

## PARAMS --------------------------------------------------------------------- 
MAX_TERMS <- 10 
r_PTS <- c(1, 7/8, 3/4, 1/2)
color_palette <- RColorBrewer::brewer.pal(n = 3 + length(r_PTS), name = "Set3")

## INPUTS --------------------------------------------------------------------- 

# example 1
pattern <- "(-1)^(k)"
abel_series_sum <- "1/(1+r)"

# # example 2
# pattern <- "(-1)^(k+1)"
# abel_series_sum <- "-1/(1+r)"

# # example 3
# pattern <- "(-1)^(k)*(k+1)"
# abel_series_sum <- "1/(1+r)^2"

## Comparison and Demonstration of Summation Methods -------------------------- 

# construct terms of the sequence 
indices <- seq(from = 0, to = MAX_TERMS - 1, by = 1) # starting from 0 
series <- EvalFormula(indices, pattern)

# compute Cauchy partial sum 
partial_sum <- cumsum(series)

# compute Cesaro partial mean 
cesaro_mean <- cumsum(partial_sum) / ( indices + 1 )

# compute Abel mean at a few points
abel_mean_func <- function(r) 
  EvalFormula(r, gsub(pattern = "r", replacement = "k", x = abel_series_sum))
abel_mean <- abel_mean_func(r_PTS)
abel_weights <- t(sapply(r_PTS, function(r) r^indices))
abel_weights <- abel_weights[2:nrow(abel_weights),]

# plot

p <- plotly::plot_ly()

p <- plotly::add_trace(
  p, type = "scatter", mode = "lines", 
  x = as.ordered(indices), y = abel_mean[1], yaxis = "y", 
  name = "Limit --> Sum", hoverlabel = list(namelength = -1), 
  line = list(color = "#000000", width = 2) )

for ( idx in seq(from = 2, to = length(r_PTS)) ) {
  p <- plotly::add_trace(
    p, type = "bar", alpha = 0.5, yaxis = "y2", 
    x = as.ordered(indices), y = abel_weights[idx-1,], 
    name = paste0("Abel Weights with r = ", sprintf("%.4f", r_PTS[idx])), 
    hoverlabel = list(namelength = -1), 
    color = color_palette[idx] )
  p <- plotly::add_trace(
    p, type = "scatter", mode = "lines", 
    x = as.ordered(indices), y = abel_mean[idx], yaxis = "y", 
    name = paste0("Abel Sum with r = ", sprintf("%.4f", r_PTS[idx])), 
    hoverlabel = list(namelength = -1), 
    color = color_palette[idx], alpha = 0.5 )
}

p <- plotly::add_trace(
  p, type = "scatter", mode = "lines+markers", 
  x = as.ordered(indices), y = series, yaxis = "y", 
  name = "Sequence a_k", hoverlabel = list(namelength = -1), 
  color = color_palette[length(r_PTS) + 1], alpha = 0.7 )
p <- plotly::add_trace(
  p, type = "scatter", mode = "lines+markers", 
  x = as.ordered(indices), y = partial_sum, yaxis = "y", 
  name = "Cauchy Partial Sum s_k", hoverlabel = list(namelength = -1), 
  color = color_palette[length(r_PTS) + 2] )
p <- plotly::add_trace(
  p, type = "scatter", mode = "lines+markers", 
  x = as.ordered(indices), y = cesaro_mean, yaxis = "y", 
  name = "Cesaro Partial Mean sigma_k", hoverlabel = list(namelength = -1), 
  color = color_palette[length(r_PTS) + 3] )

p <- plotly::layout(
  p, hovermode = "compare", legend = list(orientation = "h", yanchor = "top"), 
  barmode = "overlay",  # bargap = 0, 
  yaxis  = list(title = "Value", side = "left", automargin = TRUE, 
                showgrid = TRUE, zeroline = FALSE, overlaying = "y2"), 
  yaxis2 = list(title = "Abel Weight", side = "right", automargin = TRUE, tickformat = ".2%", 
                showgrid = FALSE, zeroline = FALSE), 
  xaxis  = list(title = "Index", dtick = "L1", 
                showgrid = TRUE, automargin = TRUE)
)

p
