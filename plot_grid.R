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
  
  c <- group_by(df, connectivity)%>%
    summarize(exp_conn = first(exp_conn),
              root_conn = first(root_conn),
              log_conn = first(log_conn),
              lin_conn = first(lin_conn)
              )
  
  s <- group_by(df, suitability)%>%
    summarize(exp_suit = first(exp_suit),
              root_suit = first(root_suit),
              log_suit = first(log_suit),
              lin_suit = first(lin_suit)
              )
  
  c_traces <- lapply(c[,c(2,3,4,5)], function(i){
    plot_ly(data = c,
            type = 'scatter',
            mode = 'lines',
            x = ~i,
            y = ~connectivity,
            showlegend = FALSE,
            line = list(color = 'black'),
            xaxis = paste('x', i*5, sep = ""),
            yaxis = paste('y', i*5, sep = "")
    )
  }
  )
  
  s_traces <- lapply(s[,c(2,3,4,5)], function(i){
    plot_ly(data = s,
            type = 'scatter',
            mode = 'lines',
            x = ~suitability,
            y = ~i,
            line = list(color = 'black'),
            showlegend = FALSE,
            xaxis = paste('x', i, sep = ""),
            yaxis = paste('y', i, sep = "")
            )
  }
  )
  
  y_layout <- lapply(c(1, 2, 3, 4), function(i){
    anchor <- paste('x', i+1, sep = "")
    paste('yaxis', i+1,
          " = list(showgrid = FALSE,",
          "domain = c(0, 0.1),",
          "anchor = '", anchor,"',",
          "tickmode = 'auto',",
          "nticks = 3)",
          sep = "")
  }
  )
  
  #y_layout = {'yaxis%s' %(i+1): dict(showgrid = False,
  #                                   domain = [0, 0.1],
  #                                   anchor = 'x%s' %(i+1),
  #                                   tickmode = 'auto',
  #                                   nticks = 3) for i in range(1, len(s.columns))}
  
  x_layout <- lapply(c(1, 2, 3, 4), function(i){
    anchor <- paste('y', i+1, sep = "")
    paste('xaxis', i+1,
          " = list(showgrid = FALSE,",
          "domain = c(", 0.1+(0.02*i)+((i-1)/5), ",", (i/5)+0.1+(0.02*i), "),",
          "anchor = '", anchor,"',",
          "tickmode = 'auto',",
          "nticks = 3)",
          sep = "")
  }
  )
  
  #x_layout = {'xaxis%s' %(i+1): dict(showgrid = False,
  #                                   domain = [0.1+(0.02*i)+((i-1)/5), (i/5)+0.1+(0.02*i)],
  #                                   anchor = 'y%s' %(i+1),
  #                                   tickmode = 'auto',
  #                                   nticks = 3) for i in range(1, len(s.columns))}
  
  y_additions <- lapply(c(1,2,3,4), function(i){
    anchor = paste('x', (i*5)+1, sep = "")
    paste('yaxis', (i*5)+1, " = list(",
          "showgrid = FALSE,",
          "domain = c(", 0.1+(0.02*i)+((i-1)/5), ",", (i/5)+0.1+(0.02*1), "),",
          "anchor = '", anchor,"',",
          "tickmode = 'auto',",
          "nticks = 3)",
          sep = "")
  })
                          
#  y_layout.update({'yaxis%s' %((i*5)+1): dict(showgrid = False,
#                                              domain = [0.1+(0.02*i)+((i-1)/5), (i/5)+0.1+(0.02*i)],
#                                             anchor = 'x%s' %((i*5)+1),
#                                              tickmode = 'auto',
#                                              nticks = 3) for i in range(1, len(c.columns))})
  
  x_additions <- lapply(c(1,2,3,4), function(i){
    anchor = paste('y', (i*5)+1, sep = "")
    paste('xaxis', (i*5)+1, " = list(",
          "showgrid = FALSE,",
          "domain = c(0, 0.1),",
          "anchor = '", anchor,"',",
          "tickmode = 'auto',",
          "nticks = 3)",
          sep = "")
  })
  
#  x_layout.update({'xaxis%s' %((i*5)+1): dict(showgrid = False,
#                                              domain = [0, 0.1],
#                                              anchor = 'y%s' %((i*5)+1),
#                                              tickmode = 'auto',
#                                              nticks = 3) for i in range(1, len(c.columns))})
  x_layout <- c(x_layout, x_additions)
  y_layout <- c(y_layout, y_additions)
  
  h_traces = list()
  
  for (i in c(1,2,3,4)){
    for (j in c(1,2,3,4)){
      J <- j
      I <- i
      index = (I*5)+J+1
      df['test'] <- df[i] + df[j]
      mat <- matrix(df$test, nrow = 101, ncol = 101)
      trace <- plot_ly(
        type = 'contour',
        z = mat,
        autocontour = FALSE,
        countours = list(
          start = 0.666,
          end = 2, 
          size = 0.666,
          coloring = 'heatmap'
        ),
        colorbar = list(
          len = 0.2,
          x = 0, 
          y = 0
        ),
        xaxis = paste('x', index, sep = ""),
        yaxis = paste('y', index, sep = "")
      )
      xanchor = paste('y', index, sep = "")
      yanchor = paste('x', index, sep = "")
      xpar <- paste('xaxis', index, " = list(visible = FALSE,", "domain = c(", 0.1+(0.02*J)+((J-1)/5), ",", (J/5)+0.1+(0.02*J), "),", "anchor = '", xanchor, "')", sep = "")
      ypar <- paste('yaxis', index, " = list(visible = FALSE,", "domain = c(", 0.1+(0.02*I)+((I-1)/5), ",", (I/5)+0.1+(0.02*I), "),", "anchor = '", yanchor, "')", sep = "")
      y_layout <- append(y_layout, ypar)
      x_layout <- append(x_layout, xpar)
      h_traces <- append(h_traces, trace)
    }
  }
  
  # for i in c.columns[1:]:
  #   for j in s.columns[1:]:
  #   J = s.columns.get_loc(j)
  # I = c.columns.get_loc(i)
  # index = ((I)*5)+J+1
  # df['test'] = df[i] + df[j]
  # 
  # mat = np.array(df.test.values).reshape(101, 101)
  # 
  # trace = go.Contour(z = mat,
  #                    autocontour = False,
  #                    contours = dict(start = 0.666,
  #                                    end = 2,
  #                                    size = 0.666,
  #                                    coloring = 'heatmap'),
  #                    colorbar = dict(len = 0.2,
  #                                    x = 0, 
  #                                    y = 0),
  #                    xaxis = 'x%s' %index,
  #                    yaxis = 'y%s' %index)
  # 
  # xpar = dict(visible = False, domain = [0.1+(0.02*J)+((J-1)/5), (J/5)+0.1+(0.02*J)], anchor = 'y%s' %index)
  # ypar = dict(visible = False, domain = [0.1+(0.02*I)+((I-1)/5), (I/5)+0.1+(0.02*I)], anchor = 'x%s' %index)
  # h_traces.append(trace)
  # x_layout.update({'xaxis%s' %index: xpar})
  # y_layout.update({'yaxis%s' %index: ypar})
  
  layout <- append(x_layout, y_layout)
  
  #layout = {**x_layout, **y_layout}
  #layout['xaxis3'].update({'title':'Suitability'})
  #layout['yaxis11'].update({'title':'Connectivity'}) 
  
  subplot(h_traces)
  fig = go.Figure(data = h_traces+c_traces+s_traces, layout = layout)       
  return fig