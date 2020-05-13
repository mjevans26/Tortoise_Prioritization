library(dplyr)
library(googledrive)
library(ggplot2)
library(plotly)
library(readr)
library(raster)
library(reshape2)
library(tidyr)
library(viridis)

con <- read.table("data/raw/connectivity_varlands.txt", header = TRUE, sep = ",")
suit<- read.table("data/raw/suithab_varlands.txt", header = TRUE, sep = ",")

colnames(con) <- c("ID", "ConnectivityTif", "X", "Y", "Connectivity")
colnames(suit) <- c("ID", "SuitabilityTif", "X", "Y", 'Suitability')

join <- left_join(con, suit, by = c("ID"))

mutated <- mutate(join, ScaledConn = (Connectivity - min(Connectivity))/(max(Connectivity) - min(Connectivity)))

rm(con, suit, join)

#2d frequency histogram of suitability and rescaled connectivity values
plot_ly(data= sample_n(mutated, 500000, replace = FALSE),
        type = 'histogram2d',
        x = ~ScaledConn,
        y = ~Suitability)

logistic <- function(x,B, M = 0, K = 1, A = 0, C = 1, Q = 0.5, v = 1){
  Y = (K-A)/((C+Q*exp(-B*(x-M)))^1/v)
  return(Y)
}

sigmoid <- function(x, x0, k) {
  1 / (1 + exp(-k*(x-x0)))
}

step <- function(x, t){
  ifelse(x < t, 0, 1)
}

relu <- function(x, b){
  ifelse(x*b>1, 1, x*b)
}

tri_plot <- function(suit_value, conn_value){
  p <- plot_ly(data= sample_n(mutated, 500000, replace = FALSE), type = 'histogram2d',
               x = ~(ScaledConn/1)^conn_value,
               y = ~(Suitability/1)^suit_value,
               showscale = FALSE)
  
  s <- plot_ly(type = 'scatter', mode = 'lines', y = curve(x^suit_value, from = 0, to = 1)$x,
               x = curve(x^suit_value, from = 0, to = 1)$y)%>%
    layout(
      yaxis = list(title = "Value"),
      xaxis = list(title = "Suitability")
    )
  c <- plot_ly(type = 'scatter', mode = 'lines',
               x = curve(x^conn_value, from = 0, to = 1)$x,
               y = curve(x^conn_value, from = 0, to = 1)$y)%>%
    layout(
      yaxis = list(title = 'value'),
      xaxis = list(title = 'Connectivity', id='xaxis2')
    )
  
  subplot(p, s, c, nrows = 2,         
          heights = c(0.8, 0.2),
          widths = c(0.8, 0.2))%>%
    layout(showlegend = FALSE, showlegend3 = FALSE)


}

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

  d <- group_by(df, connectivity)%>%
    summarize(exp_conn = first(exp_conn),
              root_conn = first(root_conn),
              log_conn = first(log_conn),
              lin_conn = first(lin_conn))
  
  c <- lapply(d[,c(2,3,4,5)], function(i){
    plot_ly(data = d, type = 'scatter', mode = 'lines',
            x = ~i,
            y = ~connectivity
            )%>%
      layout(yaxis = list(tickmode = 'array', tickvals = seq(0, 1, 0.5), ticktext = seq(0, 1, 0.5),
                          title = 'Value',
                          showgrid = FALSE,
                          side = 'right'),
             xaxis = list(tickmode = 'array', tickvals = seq(0, 1, 0.5), ticktext = seq(0, 1, 0.5),
                          title = 'Connectivity',
                          showgrid = FALSE),
             showlegend = FALSE)
  }
  )
  
  d <- group_by(df, suitability)%>%
    summarize(exp_suit = first(exp_suit),
              root_suit = first(root_suit),
              log_suit = first(log_suit),
              lin_suit = first(lin_suit))
  
  s <- lapply(d[,c(2,3,4,5)], function(i){
    plot_ly(data = d, type = 'scatter', mode = 'lines',
            x = ~suitability,
            y = ~i)%>%
      layout(yaxis = list(tickmode = 'array', tickvals = seq(0, 1, 0.5), ticktext = seq(0, 1, 0.5),
                          title = 'Value',
                          showgrid = FALSE),
             xaxis = list(tickmode = 'array', tickvals = seq(0, 1, 0.5), ticktext = seq(0, 1, 0.5),
                          title = 'Suitability',
                          showgrid = FALSE),
             showlegend = FALSE)
  }
  )
  
  p <- append(c, s)
  x <- 9
  for(i in c(3,5,7,9)){
    for(j in c(4,6,8,10)){
      df$value <- df[, i] + df[, j]
      mat <- acast(df, suitability ~ connectivity, value.var = "value")
      pl <- plot_ly()%>%
        add_contour(
          z = mat,
          autocontour = FALSE,
          contours = list(
            start = 0.667,
            end = 2,
            size = 0.667,
            coloring = 'heatmap'),
          colorbar = list(
            len = 0.2,
            x = 0.8,
            y = 0.2)
        )%>%
        layout(
          xaxis = list(showticklabels = FALSE, title = ""),
          yaxis = list(showticklabels = FALSE, title = '')
        )
      p[[x]] <- pl
      names(p)[x] <- paste(i,j,sep = "_")
      x <- x+1
    }
  }
  return(p[c(names(p)[9:12], names(p)[1],
             names(p)[13:16], names(p)[2],
             names(p)[17:20], names(p)[3],
             names(p)[21:24], names(p)[4],
             names(p)[5:8])])
}

test <- plot_grid(2, 2)

subplot(test, nrows = 5, 
        heights = c(0.22, 0.22, 0.22, 0.22, 0.08),
        widths = c(0.22, 0.22, 0.22, 0.22, 0.08),
        margin = c(0, 0.01, 0, 0.01),
        shareX = FALSE,
        shareY = FALSE)


#Create a plane of values with 'low', 'medium', 'high' delieation contours
plane_plot <- function(suit_value, conn_value){
  df <- expand.grid(suit = seq(0,1,0.01), conn = seq(0,1,0.01))%>%
    mutate(value = suit^suit_value + conn^conn_value)
  mat <- acast(df, suit~conn, value.var = "value")
  p <- plot_ly()%>%
    add_contour(z = mat,
          autocontour = FALSE, 
          contours = list(
            start = 0.667,
            end = 2,
            size = 0.667,
            coloring = 'heatmap'),
          colorbar = list(
            len = 0.2,
            x = 0.8,
            y = 0.2)
          )%>%

    layout(
      xaxis = list(tickmode = 'array', tickvals = seq(0, 100, 10), ticktext = seq(0, 1, 0.1),
                   title = ""),
      yaxis = list(tickmode = 'array', tickvals = seq(0, 100, 10), ticktext = seq(0, 1, 0.1),
                   title = 'Suitability')
    )
  s <- plot_ly(type = 'scatter', mode = 'lines',
               y = curve(x^suit_value, from = 0, to = 1)$x,
               x = curve(x^suit_value, from = 0, to = 1)$y)%>%
    layout(
      yaxis = list(title = "Value"),
      xaxis = list(title = "Suitability"),
      showlegend = FALSE
    )
  c <- plot_ly(type = 'scatter', mode = 'lines',
               x = curve(x^conn_value, from = 0, to = 1)$x,
               y = curve(x^conn_value, from = 0, to = 1)$y)%>%
    layout(
      yaxis = list(title = 'value'),
      xaxis = list(title = 'Connectivity', id='xaxis2'),
      showlegend = FALSE
    )
  
  subplot(p, s, c, nrows = 2,         
          heights = c(0.8, 0.2),
          widths = c(0.8, 0.2),
          margin = 0.05,
          shareX = FALSE,
          shareY = FALSE)
    #layout(showlegend = FALSE, showlegend3 = FALSE)
}

df <- expand.grid(suit = seq(0,1,0.01), conn = seq(0,1,0.01))%>%
  mutate(value = suit^2 + conn^2)
mat <- acast(df, suit~conn, value.var = "value")

plot_ly()%>%
  add_histogram2d(data= sample_n(mutated, 500000, replace = FALSE),
                  type = 'histogram2d',
                  x = ~(ScaledConn/1)^2,
                  y = ~(Suitability/1)^2,
                  showscale = FALSE)%>%
  add_trace(type = 'contour', z = mat,
              autocontour = FALSE, 
              contours = list(
                start = 0,
                end = 2,
                size = 0.667,
                coloring = 'none'),
              opacity = 0.2)%>%
  
  layout(
    xaxis = list(tickmode = 'array', tickvals = seq(0, 100, 10), ticktext = seq(0, 1, 0.1),
                 title = "Suitability"),
    yaxis = list(tickmode = 'array', tickvals = seq(0, 100, 10), ticktext = seq(0, 1, 0.1),
                 title = 'Connectivity')
  )

#Example plot of possible value functions 
plot_ly()%>%
  add_lines(x = curve(sigmoid(x, x0 = 0.5, k = 10))$x,
            y = curve(sigmoid(x, x0 = 0.5, k = 10))$y,
            name = 'Logistic')%>%
  add_lines(x = curve(step(x, 0.5), from = 0, to = 1)$x,
            y = curve(step(x, 0.5), from = 0, to = 1)$y,
            name = "Threshold")%>%
  add_lines(x = curve(x^5, from = 0, to = 1)$x,
            y = curve(x^5, from = 0, to = 1)$y,
            name = 'Industry')%>%
  add_lines(x = curve(x^0.2, from = 0, to = 1)$x,
            y = curve(x^0.2, from = 0, to = 1)$y,
            name = 'Conservation')%>%
  add_lines(x = c(0, 1), y = c(0, 1), name = 'Linear')%>%
  layout(xaxis = list(title = 'Connectivity/Suitability',
                      titlefont = list(size = 14, color = 'black')),
         yaxis = list(title = 'Value',
                      titlefont = list(size = 14, color = 'black')),
         legend = list(x = 0.8, y = 0.1))

#Function to create matrix of 'Values' that can be converted to raster
make_value_matrix <- function(suit, conn, sfn, cfn){
  df <- dplyr::select(mutated, X.x, Y.x, Suitability, ScaledConn)
  colnames(df) <- c('X', 'Y', 'Suitability', 'ScaledConn')
  
  if(sfn == 'exp'){
    df$Suitability <- df$Suitability^suit
  }else if (sfn == 'logistic'){
    df$Suitability <- sigmoid(df$Suitability, k = suit, x0 = 0.5)
  }else if (sfn == 'linear'){
    df$Suitability <- vapply(df$Suitability, function(x){relu(x, suit)}, FUN.VALUE = numeric(1))
  }
  
  if(cfn == 'exp'){
    df$Connectivity <- df$ScaledConn^conn
  }else if (cfn == 'logistic'){
    df$Connectivity <- sigmoid(df$ScaledConn, k = conn, x0 = 0.5)
  }else if (cfn == 'linear'){
    df$Connectivity <- vapply(df$ScaledConn, function(x){relu(x, conn)}, FUN.VALUE = numeric(1))
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


#Example
mat <- make_value_matrix(1/2, 1/2, 'exp', 'exp')
r <- rasterFromXYZ(mat, crs = CRS("+init=epsg:4269"))
plot(r, breaks = seq(0,2,2/3), col = viridis(3))
low_med_high(r)/900
rm(r)
writeRaster(r, 'temp.tif', 'GTiff')
drive_upload("temp.tif", path = as_id('1vtKyE60rRIh0FjizSwFfu0BY520gsZzJ'))

