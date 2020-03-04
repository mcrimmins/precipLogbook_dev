# Cumulative precip chart generator
# borrowing code from SPI Tool
# 6/8/16
# DEVELOPMENT VERSION TO MESS AROUND WITH!!!
# posted to precipChart as v1.1 on 7/3/2019

# To do --
# - add percent average for DTP
# - use to forecast outcomes? like in proposal
# - incorporate NDVI data?


# --- add in libraries, need all of them? -----
library(RCurl)
library(jsonlite)
library(reshape)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(caTools)
library(leaflet)
library(rmarkdown)
library(xtable)
library(geosphere)
library(stringr)
library(httr)
library(plotly)
library(RColorBrewer)
library(ggmap)

# API key
source('APIkey.R', local=TRUE)

# initial point for marker ----
latIn<-32
lonIn<--110

# add in CSS to change look? 
# tags$link(rel = 'stylesheet', type = 'text/css', href = 'styles.css'),
#  includeCSS("styles.css"),


## UI section ----  

ui<-tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ), 
  navbarPage("Precipitation Logbook Generator",
             tabPanel("About Tool",
                      sidebarLayout(
                        sidebarPanel(
                          HTML('<body>
                               <p> This tool was developed through the support of the
                               <a href="http://westrme.wsu.edu/">Western Extension Risk Management Education Center
                               </a></p>
                               Links to other cumulative precipitation tracking resources:<br>
                               <ul>
                               <li><a href="http://www.cpc.ncep.noaa.gov/products/global_monitoring/precipitation/global_precip_accum.shtml">
                               NOAA-CPC precipitation time series</a></li>
                               <li><a
                               href="http://cals.arizona.edu/climate/misc/CoolSeason/CoolSeason_summaries.html">AZ/NM Cool season climate summary plots</a></li>
                               <li><a
                               href="http://cals.arizona.edu/climate/misc/monsoon/monsoon_summaries.html">AZ/NM Summer season climate summaries</a></li>
                               </ul>
                               </body>'
                          )
                          ),
                        mainPanel(
                          h4("About the Precipitation Logbook Generator"),
                          
                          tags$head(HTML(
                            "<!-- Global site tag (gtag.js) - Google Analytics -->
                            <script async src='https://www.googletagmanager.com/gtag/js?id=UA-108612767-1'></script>
                            <script>
                            window.dataLayer = window.dataLayer || [];
                            function gtag(){dataLayer.push(arguments);}
                            gtag('js', new Date());
                            
                            gtag('config', 'UA-108612767-1');
                            </script>"
                            
                          )),
                          
                          HTML('<body>
                               <p>Precipitation is the key variable in assessing drought status
                               and tracking changes in drought conditions. Tracking precipitation,
                               though, can be a challenging task in remote areas. One strategy is
                               to deploy rain gauges that can be checked occasionally by reading
                               the gauge directly (reading depth of accumulated rain) or downloading
                               data collected by a datalogger. An additional challenge is to put these
                               observations into a longer-term context and assessing whether or not
                               they represent drought conditions.</p>
                               
                               <p>This tool creates a reference precipitation climatology for any
                               location by leveraging a spatially continuous, gridded, long-term
                               <a href="http://www.prism.oregonstate.edu/">dataset</a> of daily 
                               precipitation estimates for all locations in the continental United States.
                               The reference climatology can be created for any time frame within the
                               year (e.g. summer season) and produces a chart of the typical (i.e. median) cumulative
                               precipitation pattern as well as extremely wet or dry values
                               based on historical data (1981-2018 historical period).  A printable table
                               is also produced for tracking and manually recording observations in the field. Together
                               the chart and table create a custom <em> logbook </em> for that rain gauge location. When an 
                               observation is made in the field, the entry for that date can be compared to the typical
                               and extreme historical values to provide context and inform a possible management
                               decision. For example, if the reference chart and table indicate that an observation is
                               unusually dry for that date and location it could support triggering management actions
                               specified in a drought plan.</p> 
                               </body>
                               '),
                          h4("How to use the tool"),
                          p("1. Click on the 'Choose a location' tab at the top of the page. Pan and zoom to your
                            location of interest and click on the map. Click on 'Download Data' to load historical climate data into tool."),
                          p("2. Click on the 'Generate Logbook' tab at the top of the page. Customize the format of your reference chart and table by
                            selecting the beginning month and day and length in days of the summary. Enter a description of the site or gauge name
                            for reference. Click on the 'Generate Chart' button to generate the custom chart and table in the right panel. Repeat
                            steps on this page to make any adjustments on beginning date and/or summary length."),
                          p("3. The reference table and chart can be printed directly from this page. A printable version can be also be generated by
                            clicking on 'Download' button. This will prompt your browser to save an html file locally which can be opened and printed
                            by a browser as well. Repeat steps 1 through 3 to generate additional charts and tables of other locations."),
                          p("4. Click on the 'Explore the Data' tab to see all of the yearly data from 1981 to present used in constructing
                            the logbook charts and tables for the specified period. The interactive charts show the cumulative precipitation
                            for each day including the current year and the distribution of seasonal total values to identify extreme dry or
                            wet years."),
                          h4("Using the logbook"),
                          p("The custom generated chart and table provide a continuous reference climatology of running cumulative
                            precipitation over the period specified. The values presented are the percentile ranks of cumulative
                            precipitation for each day derived from the historical dataset that covers the 1981-2018 period."),
                          tags$ul(
                            tags$li("2nd %tile: Very dry conditions with only 2% of historical values at or below this precipitation depth; roughly corresponds to 2 standard deviations below average in a normal distribution."), 
                            tags$li("16th %tile: Dry conditions with 16% of historical values at or below this precipitation depth; roughly corresponds to 1 standard deviation below average in a normal distribution."), 
                            tags$li("50th %tile: Near normal conditions representing the median or middle of historical values; roughly corresponds to average in a normal distribution."),
                            tags$li("84th %tile: Wet conditions with 84% of historical values below this precipitation depth; roughly corresponds to 1 standard deviation above average in a normal distribution."),
                            tags$li("98th %tile: Very conditions with 98% of historical values below this precipitation depth (inversely, only 2% of observations greater than this value); roughly corresponds to 2 standard deviations above average in a normal distribution.")
                          ),
                          HTML('<body> 
                               <p>
                               By using a simple precipitation depth gauge like this <a href="http://cals.arizona.edu/climate/misc/PVCgauge.pdf"> one </a> or this
                               <a href="https://extension.arizona.edu/sites/extension.arizona.edu/files/pubs/az1747-2017_0.pdf"> one </a>  and reading it
                               several times over a season, the depths in the gauge can be directly compared to the values on the chart and in the table. Also,
                               use the notes section in the printable logbook file to make observations that will be helpful in making management decisions over time.
                               For example, notes on vegetation conditions, forage amounts, levels in water tanks relative to precipitation observations together will 
                               help build a case for management actions specified in a drought plan and can help serve as historic data over time.
                               </p>
                               
                               </body>'
                          ),
                          
                          
                          hr(),
                          HTML('<table
                               style="width: 90%; height: 75px; text-align: left; margin-left: auto; margin-right: auto;"
                               border="0" cellpadding="2" cellspacing="2">
                               <tbody>
                               <tr>
                               <td style="text-align: center; width: 373px;"><a
                               href="http://cals.arizona.edu/climate"><img
                               style="border: 0px solid ; width: 121px; height: 50px;"
                               alt="cals"
                               src="cals.jpg"></a></td>
                               <td style="text-align: center; width: 405px;"><a
                               href="http://www.climas.arizona.edu"><img
                               style="border: 0px solid ; width: 62px; height: 58px;"
                               alt="climas"
                               src="climas.png"></a></td>
                               <td style="text-align: center; width: 442px;"><a
                               href="http://www.rcc-acis.org/"><img
                               style="border: 0px solid ; width: 195px; height: 25px;"
                               alt="acis"
                               src="acis_logo.png"></a></td>
                               </tr>
                               </tbody>
                               </table>'
                          ),
                          HTML('<div style="text-align: center;">Contact Mike Crimmins (<a
                               href="mailto:crimmins@email.arizona.edu">crimmins@email.arizona.edu</a>)
                               with questions or comments. Version 1.1, updated 07/03/2019</div>'
                          )
                          )
                          )
                          ),
             
             tabPanel("Choose a location",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Set location and download data"),
                          p("1. Click map to select location (use +/- buttons to zoom, use cursor to pan -- only works for locations within continental U.S.)"),
                          p("2. Click 'Download data' button (this may take a couple of seconds, look to lower right corner for progress message)"),
                          p("3. Proceed to Generate Logbook page"),
                          actionButton("refresh","Download data"),
                          hr(),
                          p("Selected location"),
                          verbatimTextOutput("latSel"),
                          verbatimTextOutput("lonSel")
                          #p("Center of data grid cell:")
                          #div(img(src="cals.jpg",height="50",width="121"))
                          
                        ),
                        mainPanel(
                          leafletOutput("MyMap",width = "800px", height = "800px")
                        )
                      )
             ),
             tabPanel("Generate Logbook",
                      sidebarLayout(
                        sidebarPanel(h4("Generate custom chart and logbook"),
                                     p("1. Select the month and day that the summary chart will begin on and
                                       then select the chart length in days"),
                                     p("2. Label the chart with a site description or gauge name."),
                                     p("3. Click on the 'Generate chart' button
                                       and the custom chart and fillable table will appear on the left."),
                                     p("4. Click on the 'Download' button to save a printable file."),
                                     selectInput("firstMonth", "Start Month:", c(1:12), width = '100px'),
                                     selectInput("firstDay", "Start Day:", c(1:31),width = '100px'),
                                     #numericInput("firstMonth","Month:",1,min=1,max=12, width = '100px'),
                                     #numericInput("firstDay","Day:",1,min=1,max=28, width = '100px'),
                                     numericInput("lengthDays","Chart length (days):",90,min=30,max=365,width = '100px'),
                                     textInput("gaugeName", "Site name:", "Enter site name ",width = '400px'),
                                     actionButton("refreshChart","Generate Chart"),
                                     p(),
                                     #radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                     #              inline = TRUE),
                                     hr(),
                                     p(strong("Download printable chart/table (html file that can be opened and printed with browser)")),
                                     downloadButton('downloadReport')
                                     ),
                        mainPanel(
                          h5("Cumulative Precipitation Chart"),
                          plotOutput("precipChart", width = "809", height = "500"),
                          hr(),
                          h5("Selected location"),
                          textOutput("locLat"),
                          textOutput("locLon"),
                          textOutput("NEDelev"),
                          h5("Center of data grid cell"),
                          textOutput("prismLat"),
                          textOutput("prismLon"),
                          textOutput("elev"),
                          textOutput("pointDist"),
                          hr(),
                          h5("Reference table"),
                          tableOutput('chartTable'),
                          tags$head(tags$style(type="text/css", "#chartTable table {
                                               width: 809px;
                                               border: 1px solid #ccc;
                                               }
                                               
                                               th {
                                               background-color: #F5F5F5; // background for table header 
                                               color: #ffffff;
                                               }
                                               
                                               td
                                               {
                                               text-align:right;        // justify column
                                               background-color: #FF0000;
                                               }"))
                        )
                          )
                          ),
             
             tabPanel("Explore the Data",
                      sidebarLayout(
                        sidebarPanel(
                          h4("Explore the daily precipitation data used to create the logbook"),
                          p("The top chart displays the cumulative daily precipitation for the 
                          time period specified on the 'Generate Logbook' page for each year from 1981 to present.
                          You can use your cursor to hover over the time series lines to see actual values in each year
                          and can click on years on the legend to highlight or hide selections"),
                          p("The bottom chart shows the distribution of seasonal total precipitation for the selected
                            time period. Years are organized into 1 inch bins and stacked from lowest to highest values
                            in each bin. Use your cursor to see the year and precipitation value
                            associated with each dot."),
                          hr(),
                          h5("Statistics for selected period"),
                          p(textOutput("meanPrecip")),
                          p(textOutput("medianPrecip")),
                          hr(),
                          h5("Location of selected point and extent of PRISM grid cell"),
                          plotOutput("mapPRISM"),
                          h5("Red Dot - Selected Point, Blue Rectangle - PRISM Grid Cell", align="center"),
                          hr(),
                          p()
                          #p("Selected location"),
                          #verbatimTextOutput("latSel"),
                          #verbatimTextOutput("lonSel")
                          #p("Center of data grid cell:")
                          #div(img(src="cals.jpg",height="50",width="121"))
                        ),
                        mainPanel(
                          plotlyOutput("allPrecipChart", width = "809", height = "500"),
                          p(),
                          plotlyOutput("dotplotChart", width = "809", height = "500")
                                  )
                                  )
                      )
             
             
                          )
                          )

## Server Section ----
server <- function(input, output, session) {
  # add in leaflet map, overlay PRISM avg precip map or DEM grid...
  output$MyMap <- renderLeaflet({
    m <- leaflet() %>% setView(lng = -111.740979, lat = 34.394685, zoom = 7)
    m %>% addProviderTiles("Esri.WorldTopoMap")
  })
  
  #  output$out <- renderPrint({
  #    validate(need(input$MyMap_click, FALSE))
  #    str(input$MyMap_click)
  #     })
  
  observeEvent(input$MyMap_click, {
    leafletProxy("MyMap")%>% clearMarkers() %>%
      addMarkers(input$MyMap_click$lng, input$MyMap_click$lat)
    latIn<-input$MyMap_click$lat
    lonIn<-input$MyMap_click$lng
    output$latSel<-renderText({paste("Latitude: ",latIn)})
    output$lonSel<-renderText({paste("Longitude: ",lonIn)})
  })
  
  
  # download and process data  
  observeEvent(input$refresh, {
    withProgress(message = 'Downloading data set',
                 detail = 'Please wait...',{
                   lat=input$MyMap_click$lat # input from map
                   lon=input$MyMap_click$lng # input from map
                   #download daily PRISM -----
                   #endDate<-"2020-12-31"
                   endDate<-paste0(as.numeric(format(Sys.Date(),"%Y"))+1,"-12-31")
                   jsonQuery=paste0('{"loc":"',lon,',',lat,'","sdate":"1981-01-01","edate":"',endDate,'","grid":"21",
                                    "meta":"ll,elev","elems":[{"name":"pcpn","interval":"dly"}]}')
                   
                   outDaily<-postForm("http://data.rcc-acis.org/GridData",.opts = list(postfields = jsonQuery, 
                                                                                       httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
                   
                   # download monthly PRISM 1981-2010
                   jsonQuery=paste0('{"loc":"',lon,',',lat,'","sdate":"1981-01","edate":"2010-12","grid":"21","elems"
                                    :[{"name":"mly_pcpn","interval":"mly","units":"inch"}]}')
                   
                   outMonthly<-postForm("http://data.rcc-acis.org/GridData",.opts = list(postfields = jsonQuery, 
                                                                                         httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
                   
                   
})
    
    # NOT INTERACTIVE Format data frame ----
    # convert JSON to dataframes
    outDaily<-fromJSON(outDaily)
    outMonthly<-fromJSON(outMonthly)
    
    # meta output ----
    output$elev<-renderText({paste("Elevation (ft): ",as.character(outDaily$meta$elev))})
    output$prismLat<-renderText({paste("Lat: ",as.character(outDaily$meta$lat))})
    output$prismLon<-renderText({paste("Lon: ",as.character(outDaily$meta$lon))})
    # location output
    output$locLat<-renderText({paste("Lat: ",as.character(lat))})
    output$locLon<-renderText({paste("Lon: ",as.character(lon))})
    # distance between selected location and PRISM cell
    output$pointDist<-renderText({paste("Distance between selected location and center of grid cell (ft): ",as.character(round((distm (c(lon,lat), c(pointMeta$lon, pointMeta$lat), fun = distVincentyEllipsoid))*3.281)))})
    # get selected elevation
    NEDelev<-fromJSON(paste0('http://ned.usgs.gov/epqs/pqs.php?x=',lon,'&y=',lat,'&units=Feet&output=json'))
    output$NEDelev<-renderText({paste("Elevation (ft):",as.character(NEDelev$USGS_Elevation_Point_Query_Service$Elevation_Query$Elevation))})
    # vars for rmarkdown
    gridLat<-as.numeric(outDaily$meta$lat)
    gridLon<-as.numeric(outDaily$meta$lon)
    gridElev<-outDaily$meta$elev
    locElev<-NEDelev$USGS_Elevation_Point_Query_Service$Elevation_Query$Elevation
    # 

    # get metadata
    pointMeta<-outDaily$meta
        
    # ggmap of PRISM grid
    lat=input$MyMap_click$lat # input from map
    lon=input$MyMap_click$lng # input from map
    latlon<-as.data.frame(cbind(lat,lon))
    
    lonP = pointMeta$lon
    latP = pointMeta$lat
    res=0.04166667
    myLocation <- c(lon = lonP, lat = latP)
    myMap <- get_map(location=myLocation,
                     source="google", maptype="terrain", zoom = 13)
    
    p<-ggmap(myMap)+
      annotate('rect', xmin=lonP-(res/2), ymin=latP-(res/2), xmax=lonP+(res/2), ymax=latP+(res/2), col="blue", size=1.5,fill=NA)+
      labs(x = 'Longitude', y = 'Latitude') +
      geom_point(aes(x = lon, y = lat),data = latlon, color="darkred", size=2)+
      ggtitle("PRISM Data Grid Cell")
    output$mapPRISM<-renderPlot(p)
    # ----
    
    

    
    # write data to frames
    dataDaily<-data.frame(outDaily$data)
    dataMonthly<-data.frame(outMonthly$data)
    
    # clean up dataframes
    colnames(dataDaily)<-c("date","precip")
    colnames(dataMonthly)<-c("date","precip")
    dataMonthly$date<-as.Date(paste(dataMonthly$date,"-01",sep=""))
    dataMonthly$precip<-as.numeric(as.character(dataMonthly$precip))
    dataDaily$date<-as.Date(dataDaily$date)
    dataDaily$precip<-as.numeric(as.character(dataDaily$precip))
    
    # set -999 to NA
    dataMonthly[dataMonthly == -999] <- NA
    dataDaily[dataDaily == -999] <- NA
    
    # get months and years
    dataMonthly$month<-as.numeric(format(dataMonthly$date, "%m"))
    dataMonthly$year<-as.numeric(format(dataMonthly$date, "%Y"))
    dataDaily$month<-as.numeric(format(dataDaily$date, "%m"))
    dataDaily$year<-as.numeric(format(dataDaily$date, "%Y"))
    dataDaily$day<-as.numeric(format(dataDaily$date, "%d"))
    
    # Calculate monthly correction factors --------
    # using simple annual monthly to daily totals ratio (adjustments are not by month)
    sumYrDly<-dataDaily %>% group_by(year) %>% summarize(sumYr=sum(precip))
    sumYrMthly<-dataMonthly %>% group_by(year) %>% summarize(sumYr=sum(precip))
    corrFactor<-mean(sumYrMthly$sumYr, na.rm = T)/mean(sumYrDly$sumYr, na.rm = T)
    dataDaily$precip<-dataDaily$precip*corrFactor  # TURN THIS OFF?
    
    # generate chart ----
    observeEvent(input$refreshChart,{
      # create trimmed and adjusted data frame
      # first date of interest to plot
      month=as.integer(input$firstMonth)
      day=as.integer(input$firstDay)
      tempDaily<-dataDaily
      
      #tempDaily<-filter(tempDaily, month!=2 | day!=29)
      
      # trim first and last ajd year
      firstDate<-as.Date(paste(month,day,min(tempDaily$year)), format="%m %d %Y")
      lastDate<-as.Date(paste(month,day,max(tempDaily$year)), format="%m %d %Y")
      tempDaily<-filter(tempDaily, date>=firstDate)
      tempDaily<-filter(tempDaily, date<lastDate)
      
      # generate last date based on length of chart
      days=as.integer(input$lengthDays)
      lastChartDate<-firstDate+days
      monthEnd=as.numeric(format(lastChartDate, "%m"))
      dayEnd=as.numeric(format(lastChartDate, "%d"))
      
      # create adj date sequence -- create date seq with current years, then add 1 to match length
      adjDates<-seq(as.Date(paste(1,1,min(tempDaily$year)+1), format="%m %d %Y"),by='days', length.out = nrow(tempDaily)) # nrows causes length diff
      adjDateMo<-as.numeric(format(adjDates, "%m"))
      adjDateDay<-as.numeric(format(adjDates, "%d"))
      adjDates<-as.data.frame(adjDates)
      adjDates<-cbind(adjDates,adjDateMo,adjDateDay)
      #adjDates<-filter(adjDates, adjDateMo!=2 | adjDateDay!=29) # remove leap days
      adjDates$adjYear<-as.numeric(format(adjDates$adjDates, "%Y"))
      # add day of year, days; negates leap years
      adjDates$doy<-as.numeric((as.Date(paste(adjDates$adjDateMo,adjDates$adjDateDay,"1999"),
                                        format="%m %d %Y")-as.Date(paste("1","1","1999"), format="%m %d %Y"))+1)
      
      # remove leap year days from temp df
      #tempDaily<-filter(tempDaily, month!=2 | day!=29) # remove leap days
      # add adjusted doy and year back in...
      tempDaily$doy<-adjDates$doy
      tempDaily$adjYear<-adjDates$adjYear
      #tempDaily<-filter(tempDaily, month!=2 | day!=29) # remove leap days
      #tempDaily<-filter(tempDaily, month!=3 | day!=1) # remove leap days
      # replace NA with day 
      
      # cumulative totals using dplyr
      cumPrecip <- tempDaily %>% 
        group_by(adjYear, doy) %>% # still doesn't quite work adjYear kicks off before adjDOY
        summarise(value = sum(precip, na.rm = T)) %>%
        mutate(csum = cumsum(value))
      tempDaily$cumPrecip<-cumPrecip$csum
      
      # calculate stats by day
      #dayQuant<- ddply(tempDaily,.(doy),summarise,
      #                 q05 = quantile(cumPrecip,0.05,na.rm='TRUE'),
      #                 q25 = quantile(cumPrecip,0.25,na.rm='TRUE'),
      #                 q33 = quantile(cumPrecip,0.33,na.rm='TRUE'),
      #                 q50 = quantile(cumPrecip,0.50,na.rm='TRUE'),
      #                 q66 = quantile(cumPrecip,0.66,na.rm='TRUE'),
      #                 q75 = quantile(cumPrecip,0.75,na.rm='TRUE'),
      #                 q95 = quantile(cumPrecip,0.95,na.rm='TRUE'),
      #                 min = min(cumPrecip,na.rm='TRUE'),
      #                 max = max(cumPrecip,na.rm='TRUE'),
      #                 avg = mean(cumPrecip,na.rm='TRUE'))
      
      dayQuant<- ddply(tempDaily,.(doy),summarise,
                       q02 = quantile(cumPrecip,0.02,type=7,na.rm='TRUE'),
                       q16 = quantile(cumPrecip,0.16,type=7,na.rm='TRUE'),
                       q50 = quantile(cumPrecip,0.50,type=7,na.rm='TRUE'),
                       q84 = quantile(cumPrecip,0.84,type=7,na.rm='TRUE'),
                       q98 = quantile(cumPrecip,0.98,type=7,na.rm='TRUE'))
      #avg = mean(cumPrecip,na.rm='TRUE'))
      
      # drop day 366 if it exists
      dayQuant<-dayQuant[complete.cases(dayQuant), ]
      
      dayQuant$date<-as.POSIXct(seq(as.Date(paste(month,day,1999), format="%m %d %Y"),by='days', length.out = 365)+1)
      endDate<-as.POSIXct(as.Date(paste(monthEnd,dayEnd,1999), format="%m %d %Y")+1)
      
      # SMOOTHING -----
      # # hold on to original dayQuant vals
      # dayQuantTemp<-dayQuant
      # 
      # # # # # apply smoothing, uses caTools
      # for(i in 2:6){
      #   dayQuant[,i]<-runmean(dayQuant[,i], 15, align = "center", endrule = "mean") # window width, set at 60
      # }
      # 
      # # replace first smoothed value with original
      # dayQuant[1,]<-dayQuantTemp[1,]
      # END SMOOTHING ----
      
      
      # # alternate smoothing alg - loess
      # for(i in 2:6){
      #   loessOut<-loess(dayQuant[,i]~dayQuant[,1], dayQuant, span=0.15) # loess paramters
      #   dayQuant[,i]<-loessOut$fitted 
      #   dayQuant[dayQuant<0] <- 0
      # }
      
      # # alternate smoothing alg - kernal density   
      # for(i in 2:6){
      #   densityOut<-density(dayQuant[,i], bw=0.15, n=365 ) # loess paramters
      #   #dayQuant[,i]<-loessOut$fitted 
      #   #dayQuant[dayQuant<0] <- 0
      # }
      # 
      
      
      # gauge name
      gaugeTitle<-input$gaugeName
      
      # plot cumulative precip stats ----
      dayQuantLong<-melt(dayQuant, id.vars = "date", measure.vars = 2:6)
      output$precipChart<-renderPlot({
        p<-ggplot(dayQuantLong, aes(date,value))+
          geom_line(aes(linetype=variable), size=1)+
          #geom_area(aes(color=variable))+
          scale_x_datetime(labels = date_format("%m/%d"), breaks=date_breaks(
            ifelse(days<180,"1 weeks","1 months")),
            limits = as.POSIXct(c(dayQuant$date[1],dayQuant$date[days])),expand=c(0,0))+
          #ylim(0,ceiling(dayQuant$q98[match(endDate, dayQuant$date)]))+
          scale_y_continuous(breaks=seq(0, ceiling(dayQuant$q98[match(days, dayQuant$doy)]),1),
                             minor_breaks=seq(0, 13, 0.5),
                             limits=c(0,ceiling(dayQuant$q98[match(days, dayQuant$doy)])),
                             expand=c(0,0))+
          scale_linetype_manual(values=c("dotted", "twodash","solid","twodash","dotted"))+
          labs(title=gaugeTitle,x="Day of year",y="cumulative precip (in)")+
          annotate("text", label = "Very Wet", x = as.POSIXct(dayQuant$date[days-(days*0.05)]), y = (dayQuant$q98[days-(days*0.05)]+dayQuant$q84[days-(days*0.05)])/2, fontface = 3)+
          annotate("text", label = "Wet", x = as.POSIXct(dayQuant$date[days-(days*0.05)]), y = (dayQuant$q84[days-(days*0.05)]+dayQuant$q50[days-(days*0.05)])/2, fontface = 3)+
          annotate("text", label = "Dry", x = as.POSIXct(dayQuant$date[days-(days*0.05)]), y = (dayQuant$q50[days-(days*0.05)]+dayQuant$q16[days-(days*0.05)])/2, fontface = 3)+
          annotate("text", label = "Very Dry", x = as.POSIXct(dayQuant$date[days-(days*0.05)]), y = (dayQuant$q16[days-(days*0.05)]+dayQuant$q02[days-(days*0.05)])/2, fontface = 3)+
          
          annotate("text", label = "98th", x = as.POSIXct(dayQuant$date[days-(days*0.02)]), y = dayQuant$q98[days]+(0.01*dayQuant$q98[days]), size=3)+
          annotate("text", label = "84th", x = as.POSIXct(dayQuant$date[days-(days*0.02)]), y = dayQuant$q84[days]+(0.01*dayQuant$q98[days]), size=3)+
          annotate("text", label = "50th", x = as.POSIXct(dayQuant$date[days-(days*0.02)]), y = dayQuant$q50[days]+(0.01*dayQuant$q98[days]), size=3)+
          annotate("text", label = "16th", x = as.POSIXct(dayQuant$date[days-(days*0.02)]), y = dayQuant$q16[days]+(0.01*dayQuant$q98[days]), size=3)+
          annotate("text", label = "2nd",  x = as.POSIXct(dayQuant$date[days-(days*0.02)]), y = dayQuant$q02[days]+(0.01*dayQuant$q98[days]), size=3)
        
        p+  theme_bw(base_size = 12, base_family = "")+
          theme(legend.position="none")+
          theme(panel.grid.major = element_line(size = 0.5, color="grey"))+
          theme(panel.grid.minor = element_line(size = 0.5, color="grey", linetype = "dotted"))
        
      })
      
      # set cumPrecip to NAs
      tempDaily$cumPrecip <- ifelse(is.na(tempDaily$precip), NA, tempDaily$cumPrecip)
      # NEW add in plotly of all years ###############
      tempDailyLong<-melt(tempDaily, id.vars = c("year","date","doy"), measure.vars = 8)
      
      # add in generic date
      tempDailyLong$datePlot<-format(as.Date(paste0("2000-",format(tempDailyLong$date, "%m-%d"))),"%b-%d")
      # filter out >days
      tempDailyLong<-subset(tempDailyLong, doy<=days)
      tempDailyLong$value<-round(tempDailyLong$value,2)

      # correct years to fit 365 days
          wtr_yr <- function(dates, start_month) {
            # Convert possible character vector into date
            d1 = as.Date(dates)
            # Year offset
            offset = ifelse(as.integer(format(d1, "%m")) < start_month, 0, 1)
            # Water year
            adj.year = as.integer(format(d1, "%Y")) + offset
            # Return the water year
            return(adj.year)
          }
      tempDailyLong$adjYear<-as.factor(wtr_yr(tempDailyLong$date,month)-1)
      # adjust col names
      colnames(tempDailyLong)<-c("origYear","date","doy","var","Precip","Day","Year")

      # color ramp
      colourCount = length(unique(tempDailyLong$Year))
      getPalette = colorRampPalette(brewer.pal(9, "Set1"))

      output$allPrecipChart<-renderPlotly({
        p<-ggplot(tempDailyLong, aes(doy,Precip,color=Year, group=1,text=Day))+
            geom_step()+
            scale_x_continuous(breaks=seq(0,days,15),
                             labels=format(seq.Date(firstDate,firstDate+days,by="15 days"),"%m-%d"))+
            scale_color_manual(name="Year",values = getPalette(colourCount))+
            xlab("Day of Year")+
            ylab("Inches")+
            ggtitle(paste0("Cumulative Daily Precipitation ",min(tempDailyLong$origYear),"-",max(tempDailyLong$origYear)))+
            theme_bw()
        # fix y axis to lim to period of days, add date sequence to x axis
        p <- ggplotly(p)
      })

      # dot plot histogram
      tempSeasTotal<-subset(tempDailyLong, doy==days)
      tempSeasTotal$percAvg<-round((tempSeasTotal$Precip/mean(tempSeasTotal$Precip, na.rm = T))*100,0)
      # color ramp
      colourCount = length(unique(tempSeasTotal$Year))
      getPalette = colorRampPalette(brewer.pal(9, "Set1"))
      # Transform a litte bit the dataset to make dots
      tempSeasTotal = tempSeasTotal %>%
        arrange(Precip) %>% # sort using the numeric variable that interest you
        mutate(var_rounded = (Precip+1) - ( (Precip+1) %% 1 ) ) %>% # This attributes a bin to each observation. Here 0.2 is the size of the bin.
        mutate(y=ave(var_rounded, var_rounded, FUN=seq_along)) # This calculates the position on the Y axis: 1, 2, 3, 4...
      # add in text
      tempSeasTotal=tempSeasTotal %>% mutate(text=paste("Year: ", Year, "\n", "Precip (in): ", Precip,"\n","% of Avg: ",percAvg,sep="" ))

      # get stats of seas totals
      output$meanPrecip<-renderText({paste("Average Precip (in): ",as.character(round(mean(tempSeasTotal$Precip, na.rm = TRUE),2)))})
      output$medianPrecip<-renderText({paste("Median Precip (in): ",as.character(round(median(tempSeasTotal$Precip, na.rm = TRUE),2)))})
      
      # Improve the plot, and make it interactive
      #tempSeasTotal=tempSeasTotal %>% mutate(text=paste("Year: ", rownames(iris), "\n", "Sepal Length: ", Sepal.Length, "\n", "Species:: ", Species, sep="" ))
      output$dotplotChart<-renderPlotly({
          p<-ggplot(tempSeasTotal, aes(x=var_rounded, y=y) ) +
            geom_point( aes(text=text, color=Year), size=6) +
            xlab('Total Precip (in)') +
            ylab('# of Years') +
            scale_color_manual(name="Year",values = getPalette(colourCount))+
            # geom_vline(xintercept = mean(tempSeasTotal$Precip, na.rm = T), linetype="solid",
            #            color = "black", size=0.5)+
            # geom_text(aes(x=mean(tempSeasTotal$Precip, na.rm = T), 
            #           label=paste0("Avg: ",round(mean(tempSeasTotal$Precip, na.rm = T),2)," in.\n"), 
            #           y=max(tempSeasTotal$y, na.rm = T)-0.5), 
            #           colour="black", nudge_x = 1, text=element_text(size=11)) +
            # geom_vline(xintercept = mean(tempSeasTotal$Precip, na.rm = T)*0.5, linetype="dotted",
            #            color = "black", size=0.5)+
            # geom_vline(xintercept = mean(tempSeasTotal$Precip, na.rm = T)*1.5, linetype="dotted",
            #            color = "black", size=0.5)+
            ggtitle(paste0("Seasonal Total Precipitation ",min(tempDailyLong$origYear),"-",max(tempDailyLong$origYear)))+
            theme_classic() +
            theme(
              legend.position="none",
              axis.line.y = element_blank(),
              axis.text=element_text(size=15)
            )
          p<-ggplotly(p, tooltip="text")
      })
      
      # END NEW ############
      
      # add in single year....does not quite work...
      selectYr<-2012+1
      yrPos<-match(selectYr,tempDaily$adjYear)
      selectYrData<-tempDaily[yrPos:(yrPos+364),]
      selectYrData$plotDate<-as.POSIXct(seq(as.Date(paste(selectYrData$month[1],selectYrData$day[1],1999), format="%m %d %Y"), by='days', length.out = nrow(selectYrData))+1)
      #selectYrData$plotDate<-as.POSIXct(as.Date(paste(selectYrData$month,selectYrData$day,1999), format="%m %d %Y")+1)
      selectYrDataforTable<-selectYrData[1:(days),]
      
      # render table
      tempTable<-dayQuant[1:(days),]
      #    tempTable$date <- format(tempTable$date,'%m-%d')
      tempTable$date <- format(as.Date(tempTable$date)-1,'%m-%d')
      names(tempTable) <- c("doy", "Very Dry (2nd %)","Dry (16th %)","Normal (50th %)","Wet (84th %)","Very Wet (98th %)","Month/Day")
      tempTable$Precip.Depth<-"."
      #tempTable$Precip.Depth<-selectYrDataforTable$cumPrecip
      tempTable<-tempTable[,c(7,8,2,3,4,5,6)]
      output$chartTable<-renderTable(tempTable,include.rownames = F)
      
      # create download file ----
      output$downloadReport <- downloadHandler('precipChart.html',
                                               # filename = function() {
                                               #      ('precipChart.html')
                                               # },
                                               # 
                                               # content = function(file) {
                                               #   src <- normalizePath('precipChartTemplate.Rmd')
                                               #   
                                               #   # temporarily switch to the temp dir, in case you do not have write
                                               #   # permission to the current working directory
                                               #   owd <- setwd(tempdir())
                                               #   on.exit(setwd(owd))
                                               #   file.copy(src, 'precipChartTemplate.Rmd')
                                               content = function(file) {  
                                                 
                                                 out <- render('precipChartTemplate.Rmd', 'html_document')
                                                 file.rename(out, file)
                                               }
      )
      
      
    })
    
  }) # observe input
  
} # server 

## Call app ----
shinyApp(ui=ui, server=server)


