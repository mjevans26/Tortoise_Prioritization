#' Calculate sigmoid function of an input
#' @param x observed value
#' @param x0 midpoint
#' @param k scale factor
#' @return float
sigmoid <- function(x, x0, k) {
  1 / (1 + exp(-k*(x-x0)))
}

#' Calculate step function of an input
#' @param x observed value
#' @param t threshold
#' @return float
step <- function(x, t){
  ifelse(x < t, 0, 1)
}

#' Calculate rectified linear unit
#' @param x observed value
#' @param b activation threshold
#' @return float
relu <- function(x, b){
  ifelse(x*b>1, 1, x*b)
}

#' Plot curves providing examples of function types
#' @param slope slope or scaling parameter for each function
#' @return plotly line chart
plot_example <- function(slope){
  plot_ly()%>%
    add_lines(x = curve(sigmoid(x, x0 = 0.5, k = slope*10))$x,
              y = curve(sigmoid(x, x0 = 0.5, k = slope*10))$y,
              name = 'Logistic',
              line = list(color = 'blue'))%>%
    add_lines(x = curve(relu(x, slope), from = 0, to = 1)$x,
              y = curve(relu(x, slope), from = 0, to = 1)$y,
              name = "Linear",
              line = list(color = 'black'))%>%
    add_lines(x = curve(x^slope, from = 0, to = 1)$x,
              y = curve(x^slope, from = 0, to = 1)$y,
              name = 'Exponential',
              line = list(color = 'orange'))%>%
    add_lines(x = curve(x^(1/slope), from = 0, to = 1)$x,
              y = curve(x^(1/slope), from = 0, to = 1)$y,
              name = 'Logarithmic',
              line = list(color = 'purple'))%>%
    layout(title = 'Example Valuation Functions',
           xaxis = list(title = 'Connectivity/Suitability',
                        titlefont = list(size = 14, color = 'black')),
           yaxis = list(title = 'Importance',
                        titlefont = list(size = 14, color = 'black')),
           margin = list(b = 10, l = 10, r = 10),
           #autosize = FALSE,
           #height = 500,
           legend = list(x = 0.8, y = 0.1))
}

#' Create a lattice plot of value surfaces
#' @description 
#' given a suitability and connectivity scaling factor, creates a multi-faceted plot
#' showing the relationship between connectivity/suitability and value for each valuation function
#' as well as 2d value surfaces for each combination
#' @param suit scaling factor for suitability valuation
#' @param conn scaling factor for connectivity valuation
#' @return plotly subplot
plot_grid <- function(suit, conn){
  
  df <- expand.grid(suitability = seq(0, 1, 0.01), connectivity = seq(0, 1, 0.01))%>%
    mutate(exp_suit = suitability^suit,
           exp_conn = connectivity^conn,
           root_suit = suitability^(1/suit),
           root_conn = connectivity^(1/conn),
           log_suit = sigmoid(suitability, 0.5, suit*10),
           log_conn = sigmoid(connectivity, 0.5, conn*10),
           lin_suit = relu(suitability, suit),
           lin_conn = relu(connectivity, conn)
    )
  
  #df$lin_suit <- vapply(df$suitability, function(x){min(x*suit, 1)}, FUN.VALUE = numeric(1))
  #df$lin_conn <- vapply(df$connectivity, function(x){min(x*conn, 1)}, FUN.VALUE = numeric(1))
  
  c <- group_by(df, connectivity)%>%
    summarize(exp_conn = first(exp_conn),
              root_conn = first(root_conn),
              log_conn = first(log_conn),
              lin_conn = first(lin_conn))
  
  s <- group_by(df, suitability)%>%
    summarize(exp_suit = first(exp_suit),
              root_suit = first(root_suit),
              log_suit = first(log_suit),
              lin_suit = first(lin_suit))
  
  c_traces <- lapply(c[,c(2,3,4,5)], function(i){
    plot_ly(data = c, type = 'scatter', mode = 'lines',
            line = list(color = 'black'),
            x = ~i,
            y = ~connectivity,
            showlegend = FALSE,
            id = colnames(i)
    )%>%
      layout(yaxis = list(tickmode = 'auto', 
                          nticks = 3,
                          title = 'Value',
                          showgrid = FALSE,
                          side = 'right'),
             xaxis = list(tickmode = 'auto',
                          nticks = 3,
                          title = 'Connectivity',
                          showgrid = FALSE),
             showlegend = FALSE)
  }
  )
  
  s_traces <- lapply(s[,c(2,3,4,5)], function(i){
    plot_ly(data = s, type = 'scatter', mode = 'lines',
            line = list(color = 'black'),
            x = ~suitability,
            y = ~i,
            showlegend = FALSE,
            id = colnames(i)
            )%>%
      layout(yaxis = list(tickmode = 'auto',
                          nticks = 3,
                          title = 'Value',
                          showgrid = FALSE),
             xaxis = list(tickmode = 'auto',
                          nticks = 3,
                          title = 'Suitability',
                          showgrid = FALSE),
             showlegend = FALSE)
  }
  )
  
  blank <- plotly_empty()
    # plot_ly(id = 'blank')%>%
    # layout(
    #   xaxis = list(
    #     showgrid = FALSE,
    #     visible = FALSE
    #   ),
    #   yaxis = list(
    #     showgrid = FALSE,
    #     visible = FALSE
    #   )
    # )
  
  p <- append(c_traces, s_traces)
  x <- 9
  for(i in c(3,5,7,9)){
    for(j in c(4,6,8,10)){
      df$value <- df[, i] + df[, j]
      mat <- acast(df, suitability ~ connectivity, value.var = "value")
      pl <- plot_ly()%>%
        add_contour(
          z = mat,
          autocontour = FALSE,
          #colorscale = viridis(256),
          contours = list(
            start = 2/3,
            end = 2,
            size = 2/3,
            coloring = 'heatmap'
            ),
          colorbar = list(
            title = 'Importance',
            len = 0.4,
            x = 1.1,
            y = 0.5)
        )%>%
        layout(
          xaxis = list(visible = FALSE, showticklabels = FALSE, title = ""),
          yaxis = list(visible = FALSE, showticklabels = FALSE, title = '')
        )
      p[[x]] <- pl
      names(p)[x] <- paste(colnames(df)[i], colnames(df)[j], sep = "_")
      x <- x+1
    }
  }
  p <- append(p, blank)
  return(p[c(names(p)[5], names(p)[9:12], 
             names(p)[6], names(p)[13:16], 
             names(p)[7], names(p)[17:20], 
             names(p)[8], names(p)[21:24], 
             names(p)[25], names(p)[1:4])])
}

#' Create matrix of 'Values' that can be converted to raster
#' @param df data frame with columns containing x,y coordinates, suitability and connectivity values
#' @param suit (int) suitability scaling factor
#' @param conn (int) connectivity scaling factor
#' @param sfn (str) suitability scaling function
#' @param cfn (srt) connectivity scaling function
#' @return (data.frame) rescaled connectivity and suitabilty values
make_value_matrix <- function(df, suit, conn, sfn, cfn){
  df <- dplyr::select(df, X.x, Y.x, Suitability, ScaledConn)
  colnames(df) <- c('X', 'Y', 'Suitability', 'ScaledConn')
  
  if(sfn == 'exp'){
    df$Suitability <- df$Suitability^suit
  }else if (sfn == 'logistic'){
    df$Suitability <- sigmoid(df$Suitability, k = suit, x0 = 0.5)
  }else if (sfn == 'linear'){
    df$Suitability <- vapply(df$Suitability, function(x){relu(x, suit)}, FUN.VALUE = numeric(1))
  }else if (sfn == 'log'){
    df$Suitability <- df$Suitability^(1/suit)
  }
  
  if(cfn == 'exp'){
    df$Connectivity <- df$ScaledConn^conn
  }else if (cfn == 'logistic'){
    df$Connectivity <- sigmoid(df$ScaledConn, k = conn, x0 = 0.5)
  }else if (cfn == 'linear'){
    df$Connectivity <- vapply(df$ScaledConn, function(x){relu(x, conn)}, FUN.VALUE = numeric(1))
  }else if (cfn == 'log'){
    df$Connectivity <- df$ScaledConn^(1/conn)
  }
  
  df$value <- df$Suitability + df$Connectivity
  #matrix <- acast(df, Y~X, value.var = "value")
  xyz <- df[, c(1,2,6)]
  
  return(xyz)
}

#Function to calculate area that is low/med/high
low_med_high <- function(raster){
  values <- raster@data@values
  values[is.na(values)] <- -999
  low <- length(values[values >=0 & values < 2/3])
  med <- length(values[values >= 2/3 & values < 4/3])
  high <- length(values[values >= 4/3])
  return(data.frame(Low = low, Medium = med, High = high))
}
