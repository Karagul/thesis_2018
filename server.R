#rsconnect::showLogs(account = "zilfi", appName = "thesis_r")
#install.packages("shinydashboard")
#install.packages("shiny")
#install.packages("graphics")
#install.packages("readr")
#install.packages("plotly")
#install.packages("data.table")
library(data.table)
library(shiny)
library(graphics)
library(shinydashboard)
library(readr)
library(plotly)
library(dplyr)
library(dynlm)
library(lmtest)
library(tidyr)
library(forecast)
library(here)
setwd(here())

#if (!require("devtools")) install.packages("devtools")
#devtools::install_github("ropensci/plotly")


options(encoding = "UTF-8") 
data1<-read.csv("data/data1.csv", stringsAsFactors = FALSE)#line
data1pie <- read_csv("data/data1pie.csv")
data1pie_ <- data.frame(data1pie)
data2 <- read_csv("data/data2.csv", col_types = cols(group_ = col_skip()))
data3 <- read_csv("data/data3.csv", col_types = cols(Group_2 = col_skip()))
data3<-as.data.frame(data3)
data4<- read_csv("data/data4_migration.csv")
data5 <- read_csv("data/data4_uxevor.csv",col_types = cols(X5 = col_skip()))
data6<- read_csv("data/data4_dest_country.csv")
data5_ <- read_csv("data/data5_LMI.csv", col_types = cols(`2015_%` = col_number(),`2016_%` = col_number(), `Country Name` = col_skip()))
data5_CIS <- read_csv("data/data5_CIS.csv", col_types = cols(`2015_%` = col_number(), `2016_%` = col_number()))
data5_ECA <- read_csv("data/data5_ECA.csv", col_types = cols(`2014` = col_number(), `2014_rem` = col_number(), `2015_%` = col_number(), `2015_rem` = col_number(), `2016_%` = col_number(),   `2016_rem` = col_number(), Country = col_character()))
dt5 <- read_csv("data/data5_Armenia.csv", col_types = cols(`REM/dolar` = col_number()))
data5_volat <- read_csv("data/data5_volat.csv", 
                        col_types = cols(FDI = col_number(), 
                                         ODA = col_number(), Rem = col_number()))
data5_GDP <- read_csv("data/data5_GDP.csv", col_types = cols(Arm_Rem_gr = col_number(), 
                                                        Arm_gdp_grow = col_number(), RF_GDP_grow = col_number(), 
                                                        USA_GDP_grow = col_number(), Ukraine_GDP_grow = col_number()))

##______________ARDL1

ardl1 <- read_csv("data/ardl1.csv")
ardl1$IM<-ts(ardl1$IM_SA_DETRENDED, start = c(2004,1), frequency = 4)
ardl1$rem<-ts(ardl1$D_REM_DRAM_SA, start = c(2004,1), frequency = 4)

##_____________ ARDL2
ardl2 <- read_csv("data/ardl2.csv")
ardl2$gov<-ts(ardl2$D_GOV_EXTERNAL_DEBT, start = c(2004,1), frequency = 4)
ardl2$rem<-ts(ardl2$D_REM_DRAM_SA, start = c(2004,1), frequency = 4)
ardl2$ex<-ts(ardl2$D_EXCH_RATE, start = c(2004,1), frequency = 4)
#_______ARIMA
dt <- read_csv("data/arima.csv")

# Define server
shinyServer(function(input, output, session) {
#____TIME____
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  }) 
#____________BOP_PICTURE_____________  
  
  output$BOP<- renderImage({
    
    filename <- normalizePath(file.path('./images',
                                        paste('BOP', '.PNG', sep='')))
    
    list(
        src = "data/capture.PNG",
        contentType = "image/PNG",
        alt = "BOP",width = 750,
        height = 400
        
      )},deleteFile = FALSE)

  
#___________PLOT  1__________
  output$plot1<-renderPlotly({
    
    if (input$name == "Developing") {
      
      plot_ly(data1, x = data1$Year, y = data1$rem_l_m, name = 'Remittances', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = data1$fdi_l_m, name = 'FDI', mode = 'lines', lty=2) %>%
        add_trace(y = data1$od_l_m, name = 'ODA', mode = 'lines')%>%
        layout( title = "Developing countries", yaxis = list(title = "Billion dollors",titlefont = TRUE))
      
    } 
    else if(input$name == "Developed") {
      plot_ly(data1, x = data1$Year, y = data1$rem_h, name = 'Remittances', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = data1$fdi_h, name = 'FDI', mode = 'lines', lty=2) %>%
        add_trace(y = data1$od_h, name = 'ODA', mode = 'lines')%>%
        layout( title = "Developed countries", yaxis = list(title = "Billion dollors",titlefont = TRUE))
    }
    else if(input$name == "World") {
      plot_ly(data1, x = data1$Year, y = data1$rem_w, name = 'Remittances', type = 'scatter', mode = 'lines+markers') %>%
        add_trace(y = data1$fdi_w, name = 'FDI', mode = 'lines', lty=2) %>%
        add_trace(y = data1$od_w, name = 'ODA', mode = 'lines')%>%
        layout( title = "World", yaxis = list(title = "Billion dollors",titlefont = TRUE))
    }
    else {"CHOSE SOMETHING"}
    
  })
#___________PLOT  2__________
  
  output$plot2<-renderPlotly({
    
    df_trend = data1pie_[ ,colnames(data1pie_)==input$piename]
    plot_ly(data1pie_, labels = data1pie_$Country.Name, values = df_trend  , type = 'pie') %>%
      layout(title =paste0( 'Remittances by income groups in',"  ",substring( input$piename,2) ),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    
  })  
  
#___________PLOT  3,4__________
  output$plot3<-renderPlotly({
    
    
    ay <- list(tickfont = list(color = "blue"),overlaying = "y",side = "right",title = "% GDP")
    plot_ly()%>%
      add_trace(x = data2$variable_, y = data2$rem_bil,name = "Remittances",type = "bar",
                marker = list(color = c( 'rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)',"lightpink", 'rgb(158,202,225)','rgb(158,202,225)',"lightpink",'rgb(158,202,225)','rgb(158,202,225)','rgb(158,202,225)',"lightpink","lightpink",'rgb(158,202,225)','rgb(158,202,225)'),
                              
                              line = list(color = c('rgb(8,48,107)', 'rgb(8,48,107)','rgb(8,48,107)','rgb(8,48,107)',"lightcoral", 'rgb(8,48,107)','rgb(8,48,107)',"lightcoral",'rgb(8,48,107)','rgb(8,48,107)','rgb(8,48,107)',"lightcoral","lightcoral",'rgb(8,48,107)','rgb(8,48,107)'),
                                          width = 2)))%>%
      add_trace(x = data2$variable_, y = data2$perc_GDP, name = 'Remittances % to GDP',yaxis = "y2",type = "scatter" ,
                marker = list(color = 'pink',line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title = "Top 10 received 2016", yaxis = list(title = 'Billion dollars'), barmode = 'group',yaxis2 = ay, margin=15)
    
    
  })
  output$plot4<-renderPlotly({
    
    
    ay <- list(tickfont = list(color = "blue"),overlaying = "y",side = "right",title = "Billion dollar")
    plot_ly()%>%
      add_trace(x = data3$variable_3, y = data3$per_t_Gdp,name = "%GDP",type = "bar",
                marker = list(color = 'rgb(158,202,225)',
                              line = list(color = 'rgb(8,48,107)',  width = 2)))%>%
      add_trace(x = data3$variable_3, y = data3$rem_bil, name = 'Remimttances',yaxis = "y2",type = "scatter" ,
                marker = list(color = 'pink',line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
      layout(title = "Top 10 % GDP 2016", yaxis = list(title = '%'), barmode = 'group',yaxis2 = ay, margin=15)
    
  })
  
#________________migration ___________PLOT 5,6,7
  
  output$plot5<-renderPlotly({
    plot_ly(data4,x = data4$Year,y = data4$`Total growth`, name = 'Total growth',type = "scatter", mode="line+marker",
            line = list(color = "#F59FC2",  width = 2))%>%
      add_trace(y = data4$`Natural growth`, name = 'Natural growth',type = "bar" ,
                marker = list(color = "#B0C5ED",
                              line = list(color = "#2653AD", width = 1.5)))%>%
      add_trace(y = data4$`Migration saldo`,name = "Migration Saldo",type = "bar" ,
                marker = list(color = "#C5E3FC",line = list(color =" #2653AD", width = 1.5)))%>%
      layout(title = "Armenian migration", yaxis = list(title = 'People 1000'), barmode = 'group')
    
  })
  
  output$plot6<-renderPlotly({
    ay <- list(tickfont = list(color = "#960655"),overlaying = "y",side = "right",title = "Saldo people 1000")
    plot_ly()%>%
      add_trace(x = data5$Year,y = data5$Saldo, name = 'Saldo',type = "scatter", mode="line+marker",
                line = list(color = "#960655",  width = 2),yaxis = "y2")%>%
      add_trace(x = data5$Year,y = data5$Arrival, name = 'Arrivals',type = "bar" ,
                marker = list(color = "#E6D1E4",
                              line = list(color = "#8A1182", width = 1.5)))%>%
      add_trace(x = data5$Year,y = data5$Departure,name = "Departures",type = "bar" ,
                marker = list(color = "#DDB1E8",line = list(color =" #8206A1", width = 1.5)))%>%
      layout(title = "Armenian migration", yaxis = list(title = 'People 1000'), barmode = 'group',yaxis2 = ay)
    
    
  })
  output$plot7<-renderPlotly({
    data66<-data.frame(data6, stringsAsFactors = FALSE)
    data66$Destination.country <- factor(data66$Destination.country, levels = unique(data66$Destination.country)[order(data66$Armenia_migrants, decreasing = TRUE)])
    data6_<-head(data66,input$number1)
    plot_ly(data6_,x = data6_$Destination.country,y = data6_$Armenia_migrants,type = "bar" ,
            marker = list(color = "#E89DA9",line = list(color = "#C9001E", width = 1.5)))%>%
      layout(title = 'Migration by destination 2013', yaxis = list(title = 'People'), margin=15)
  })
  
#__________________________REMIT
  
  output$plot8<-renderPlotly(
    { 
      if(input$name2 == "CIS") {
        
        ay <- list(tickfont = list(color = "black"),overlaying = "y",side = "right",title = "Billion dollars")
        
        plot_ly()%>%
          add_trace(x = data5_CIS$Country,y = data5_CIS$`2015_rem`, name = 'Remittances 2015 ',type = "scatter", 
                    marker = list(color = "#BF1988"),  width = 2,yaxis = "y2"  )%>%
          add_trace(x = data5_CIS$Country,y = data5_CIS$`2016_rem`, name = 'Remittances 2016 ',type = "scatter", 
                    marker = list(color = "#CF1F1F"),  width = 2,yaxis = "y2" )%>%
          
          add_trace(x = data5_CIS$Country,y = data5_CIS$`2015_%`, name = 'Remittances % GDP 2015',type = "bar",
                    marker = list(color = "#7B7BB0",
                                  line = list(color = "#212185", width = 1.5)))%>%
          add_trace(x = data5_CIS$Country,y = data5_CIS$`2016_%`, name = 'Remittances % GDP 2016',type = "bar",
                    marker = list(color = "#87C2C2",line = list(color =" #212185", width = 1.5)))%>%
          layout(title = "CIS", yaxis = list(title = '%'),xaxis=list(margin=25), barmode = 'group',yaxis2 = ay)
        
      }
      
      else if (input$name2 == "Low and middle income") {
        data5df<-data.frame(data5_, stringsAsFactors = FALSE)
        ay <- list(tickfont = list(color = "black"),overlaying = "y",side = "right",title = "Billion dollars",showgrid = FALSE, zeroline = FALSE)
        if(input$button1 == "2016") {
          for( i in 1:4){
            data5df[,i] <- factor(data5df[,i], levels = unique(data5df[,i])[order(data5df$X2016_., decreasing = TRUE)])
          }
          data5_LMI<-head(data5df,input$number2)
          plot_ly()%>%
            add_trace(x = data5_LMI$Country,y = data5_LMI$`X2016_rem`, name = 'Remittances 2016 ',type = "scatter", mode="markets",
                      marker = list(color = "#CF1F1F"),yaxis = "y2" )%>%
            add_trace(x = data5_LMI$Country,y = data5_LMI$`X2016_.`, name = 'Remittances % GDP 2016',type = "bar",
                      marker = list(color = "#87C2C2",line = list(color =" #212185", width = 1.5)))%>%
            layout(title = "Low and middle income", yaxis = list(title = '%GDP'),margin=15, barmode = 'group',yaxis2 = ay)
        } 
        else if(input$button1 == "2015") {
          for( i in 1:2){
            data5df[,i] <- factor(data5df[,i], levels = unique(data5df[,i])[order(data5df$X2015_., decreasing = TRUE)])
          }
          data5_LMI<-head(data5df,input$number2)
          plot_ly()%>%
            add_trace(x = data5_LMI$Country,y = data5_LMI$`X2015_rem`, name = 'Remittances 2015 ',type = "scatter", mode="markets", 
                      marker = list(color = "#CF1F1F"),yaxis = "y2" )%>%
            add_trace(x = data5_LMI$Country,y = data5_LMI$`X2015_.`, name = 'Remittances % GDP 2015',type = "bar",
                      marker = list(color = "#87C2C2",line = list(color =" #212185", width = 1.5)))%>%
            layout(title = "Low and middle income", yaxis = list(title = '%GDP'),margin=15, barmode = 'group',yaxis2 = ay)
          
        }
        
      } 
      else if(input$name2 == "Europe and Central Asia") {
        
        data5ec<-data.frame(data5_ECA, stringsAsFactors = FALSE)
        for( i in 1:7){
          data5ec[,i] <- factor(data5ec[,i], levels = unique(data5ec[,i])[order(data5ec$X2016_., decreasing = TRUE)])
        }
        data5_ECA2<-head(data5ec, input$number2)
        ay <- list(tickfont = list(color = "black"),overlaying = "y",side = "right",title = "Billion dollars")
        plot_ly()%>%
          
          add_trace(x = data5_ECA2$Country,y = data5_ECA2$`X2015_.`, name = 'Remittances % GDP 2015',type = "bar",
                    marker = list(color = "#7B7BB0",
                                  line = list(color = "#212185", width = 1.5)))%>%
          add_trace(x = data5_ECA2$Country,y = data5_ECA2$X2016_., name = 'Remittances % GDP 2016',type = "bar",
                    marker = list(color = "#87C2C2",line = list(color =" #212185", width = 1.5)))%>%
          add_trace(x = data5_ECA2$Country,y = data5_ECA2$X2015_rem, name = 'Remittances 2015 ',type = "scatter", 
                    marker = list(color = "#BF1988"),  width = 2,yaxis = "y2"  )%>%
          add_trace(x = data5_ECA2$Country,y = data5_ECA2$X2016_rem, name = 'Remittances 2016 ',type = "scatter", 
                    marker = list(color = "#CF1F1F"),  width = 2,yaxis = "y2" )%>%
          layout( title = "Europe and Central Asia", yaxis = list(title = "%",titlefont = TRUE),yaxis2 = ay) 
      }
      else {"CHOSE SOMETHING"}
      
    })
  output$plot9<-renderPlotly({
    p1 <- dt5 %>% plot_ly() %>%  
      add_trace(x = ~dt5$Var, y = ~dt5$`REM/dolar`,name="%", yaxis="y1", type="scatter",
                mode="lines+markers",line = list(color = c('#0B5C0B') )) %>%
      add_trace(x = ~dt5$Var, y = dt5$Rem_milion,name="Total remittances", yaxis = "y2",  type="scatter",
                mode="line",
                #line= list(color='rgba(168, 216, 234, 0.5)'),
                fill = 'tozeroy' 
                #            fillcolor = c('rgba(168, 216, 234, 0.5)')
      ) %>%
      add_trace(x = ~dt5$Var, y = dt5$Rem_per_cap,name="Remittances per capita",yaxis = "y2", type="scatter",
                mode="line",
                fill = 'tozeroy'
                #  fillcolor = "#BBB1E0",
                #  line=list(color="#BBB1E0")
      ) %>%
      layout(title ="Armenian remittances dynamic",
             yaxis = list(title="%"),
             legend = list(x = 1, y = 1),
             yaxis2 = list(side = "right",  overlaying = "y1",title = "USA dollors"),
             xaxis = list(title="Year")
      )
    p1
    
  })
  output$plot10<-renderPlotly({plot_ly(data5_volat,x = data5_volat$country, y = data5_volat$Rem,type = "bar" ,name="Volatility of remittances",
                                       marker = list(color = "#C6C3FA",line = list(color = "#C6C3FA", width = 1.5)))%>%
      add_trace(
        y = data5_volat$FDI,type = "bar" ,name="Volatility of FDI",
        marker = list(color = "#FCEE9C",line = list(color = "#FCEE9C", width = 1.5))
      )%>%
      add_trace(
        y = data5_volat$ODA,type = "bar" , name="Volatility of ODA",
        marker = list(color = "#A4D6BB",line = list(color = "#A4D6BB", width = 1.5))
      )%>%
      layout(title = 'Volatility', yaxis = list(title = '%'))
  })
  output$plot11<-renderPlotly({  
    
    data5_GDP %>% 
      plot_ly(x = ~data5_GDP$Year, y = ~data5_GDP$Arm_Rem_gr,name="Remittance grow %", type="scatter",
              mode="lines+markers",line = list(color = c('#281087') )) %>%
      add_trace(y = data5_GDP$Arm_gdp_grow,name="GDP ARM %", type="scatter",
                mode="line", line= list(color='#F2AD96'))%>%
      add_trace(y = data5_GDP$RF_GDP_grow,name="GDP RF %", type="scatter",
                mode="line", line= list(color='rgba(164, 216, 234, 0.5)'))%>%
      add_trace(y = data5_GDP$Ukraine_GDP_grow,name="GDP Ukrain %", type="scatter",
                mode="line", line= list(color='#8E8EBA'))%>%
      add_trace(y = data5_GDP$USA_GDP_grow,name="GDP USA %", type="scatter",
                mode="line", line= list(color='#A31D91'))%>%
      layout(title ="Dependance",
             yaxis = list(title="%"),
             xaxis = list(title="Year")
      )
  })
  
  #_MAP_
  output$plot12<-renderPlotly({
    map2 <- read_csv("data/map2.csv")
    
    # light grey boundaries
    l <- list(color = toRGB("grey"), width = 0.5)
    
    # specify map projection/options
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(map2) %>%
      add_trace(
        z = map2$`2016`, color = map2$`2016`, colors = 'Blues',
        text = ~map2$Country, locations = ~map2$CODE,
        marker = list(line = l)
      ) %>%
      colorbar(title = 'Rem Millions US$', tickprefix = '$') %>%
      layout(
        title = paste(' Remittances sent to Armenia 2016'),
        geo = g
      )
    
  })
  output$plot13<-renderPlotly({map2_ <- read_csv("data/map2_.csv")
  
  
  plot_ly(map2_, x =  map2_$Country, y = map2_$`Russian Federation`, type = 'bar', name = 'RF') %>%
    add_trace(y = map2_$world, name = 'World', type="scatter", mode="line", fill="tozeroy")%>%
    add_trace(y = map2_$`United States`, name = 'US') %>%
    add_trace(y = map2_$Ukraine, name = 'UKR') %>%
    add_trace(y = map2_$France, name = 'FRN') %>%
    add_trace(y = map2_$Germany, name = 'GER') %>%
    layout(yaxis = list(title = 'Million dollars'), barmode = 'stack')
  })
  
#______________________     ARDL1
  
  
  output$plot14<-renderPlot({
    import.ardl.fit <- dynlm(ardl1$IM ~ L(ardl1$IM, input$ar1)  + L(ardl1$rem, 0:input$dl1), data=ardl1)
    
    par(mfrow=c(3,1))
    residuals_imp_1<-summary(import.ardl.fit)$resid
    ts.plot(ardl1$IM, col='blue', ylab='Import')
    lines(fitted(import.ardl.fit), col="red")
    legend("topright", bty="n", horiz= TRUE,legend =c("actual", "fitted"), col =c("blue", "red"), lty = 1)
    ts.plot(residuals_imp_1, col="blue")
    abline(0, 0)
    acf(residuals_imp_1, type='correlation', plot=TRUE,na.action = na.pass)
      }  )
  
  output$summ1<-renderPrint({
    import.ardl.fit <- dynlm(ardl1$IM ~ L(ardl1$IM, input$ar1)  + L(ardl1$rem, 0:input$dl1), data=ardl1)
    
    summary(import.ardl.fit)})
  
  output$table1<-renderTable({
    
    import.ardl.fit <- dynlm(ardl1$IM ~ L(ardl1$IM, input$ar1)  + L(ardl1$rem, 0:input$dl1), data=ardl1)
    
    residuals_imp_1<-summary(import.ardl.fit)$resid
    k<-input$lag1+1
    correlations <- acf(residuals_imp_1, type='correlation', plot=FALSE)$acf[2:k]
      z.score <- sqrt(length(residuals_imp_1)) * correlations
      df <- data.frame(lag=c(1:input$lag1),correlations,  abs(z.score))
      colnames(df)<-c("lags", "residuals", "z")
      df
        }  )
  
  #______________________     ARDL2  
  
  
  output$plot15<-renderPlot({
    gov.ardl.fit <- dynlm(ardl2$gov ~ L(ardl2$gov, input$ar2)  + L(ardl2$rem, 0:input$dl2)+ardl2$ex, data=ardl2)
   
  residuals_gov_2<-summary(gov.ardl.fit)$resid
  par(mfrow=c(3,1))
  ts.plot(ardl2$gov, col='blue', ylab='Government external debt')
  lines(fitted(gov.ardl.fit), col="red")
  legend("topright", bty="n", horiz= TRUE,legend =c("actual", "fitted"), col =c("blue", "red"), lty = 1)
  ts.plot(residuals_gov_2, col="blue")
  abline(0, 0)
  acf(residuals_gov_2, type='correlation', plot=TRUE,na.action = na.pass)  })
  
  output$summ2<-renderPrint({ 
    
    gov.ardl.fit <- dynlm(ardl2$gov ~ L(ardl2$gov, input$ar2)  + L(ardl2$rem, 0:input$dl2)+ardl2$ex, data=ardl2)
    
    summary(gov.ardl.fit) })
    
  output$table2<-renderTable({
    
    gov.ardl.fit <- dynlm(ardl2$gov ~ L(ardl2$gov, input$ar2)  + L(ardl2$rem, 0:input$dl2)+ardl2$ex, data=ardl2)
  k<-input$lag2+1  
    residuals_gov_2<-summary(gov.ardl.fit)$resid
    correlations2 <- acf(residuals_gov_2, type='correlation', plot=FALSE)$acf[2:k]
    z.score2 <- sqrt(length(residuals_gov_2)) * correlations2
    df2 <- data.frame(lag=c(1:input$lag2),correlations2,  abs(z.score2))
    colnames(df2)<-c("lags", "residuals", "z")
    df2  
  })
  

  #______________________ARIMA
  
  output$plot16<-renderPlot({
    
    remittance<-ts(dt$d_rem_ln)
    par(mfrow=c(2,1))
    acf(remittance, type='correlation', plot=TRUE,na.action = na.pass, lag.max = 24,lwd=1.5)
    pacf(remittance, plot=TRUE,na.action = na.pass,lag.max = 24,lwd=1.5)
  })
  output$summ3<-renderPrint({
    remittance<-ts(dt$d_rem_ln)
    model=arima(remittance,order=c(input$arr,0,input$ma),seasonal = list(order = c(input$sar, 0, input$sma), period=4))
    model
  })
  output$plot17<-renderPlot({
    remittance<-ts(dt$d_rem_ln)
    model=arima(remittance,order=c(input$arr,0,input$ma),seasonal = list(order = c(input$sar, 0, input$sma), period=4))
    tsdisplay(residuals(model), lag.max=15, main=' Model Residuals')
  })
  output$plot18<-renderPlot({
    remittance<-ts(dt$d_rem_ln)
    model=arima(remittance,order=c(input$arr,0,input$ma),seasonal = list(order = c(input$sar, 0, input$sma), period=4))
    fcast <- forecast(model, h=5)
    plot(fcast)
  })
  
  #____________ARIMA_PICTURE_____________  
  
  output$ARIMA<- renderImage({
    
    filename <- normalizePath(file.path('./images',
                                        paste('ARIMA', '.PNG', sep='')))
    
    list(
      src = "data/arima.PNG",
      contentType = "image/PNG",
      alt = "arima",width = 570,
      height = 350,
      align="center"
    )},deleteFile = FALSE)
  
  
  
})#shinyServer 
