# Cumulative precip chart generator
# borrowing code from SPI Tool
# 6/8/16

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
                          h4("Using the logbook"),
                          p("The custom generated chart and table provide a continuous reference climatology of running cumulative
                            precipitation over the period specified. The values presented are the smoothed percentile ranks of cumulative
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
                               By using a simple precipitation depth gauge like this <a href="http://cals.arizona.edu/climate/misc/PVCgauge.pdf"> one </a> and reading it
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
                               with questions or comments.</div>'
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
                   jsonQuery=paste0('{"loc":"',lon,',',lat,'","sdate":"1981-01-01","edate":"2018-12-31","grid":"21",
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
    corrFactor<-mean(sumYrMthly$sumYr)/mean(sumYrDly$sumYr)
    dataDaily$precip<-dataDaily$precip*corrFactor
    
    # generate chart ----
    observeEvent(input$refreshChart,{
      # create trimmed and adjusted data frame
      # first date of interest to plot
      month=as.integer(input$firstMonth)
      day=as.integer(input$firstDay)
      tempDaily<-dataDaily
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
      
      # create adj date sequence
      adjDates<-seq(as.Date(paste(1,1,min(tempDaily$year)+1), format="%m %d %Y"),by='days', length.out = nrow(tempDaily))
      adjDateMo<-as.numeric(format(adjDates, "%m"))
      adjDateDay<-as.numeric(format(adjDates, "%d"))
      adjDates<-as.data.frame(adjDates)
      adjDates<-cbind(adjDates,adjDateMo,adjDateDay)
      adjDates<-filter(adjDates, adjDateMo!=2 | adjDateDay!=29) # remove leap days
      adjDates$adjYear<-as.numeric(format(adjDates$adjDates, "%Y"))
      # add day of year, days; negates leap years
      adjDates$doy<-as.numeric((as.Date(paste(adjDates$adjDateMo,adjDates$adjDateDay,"1999"),
                                        format="%m %d %Y")-as.Date(paste("1","1","1999"), format="%m %d %Y"))+1)
      
      # remove leap year days from temp df
      tempDaily<-filter(tempDaily, month!=2 | day!=29) # remove leap days
      # add adjusted doy and year back in...
      tempDaily$doy<-adjDates$doy
      tempDaily$adjYear<-adjDates$adjYear
      
      # cumulative totals using dplyr
      cumPrecip <- tempDaily %>% 
        group_by(adjYear, doy) %>% # still doesn't quite work adjYear kicks off before adjDOY
        summarise(value = sum(precip)) %>%
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
      
      dayQuant$date<-as.POSIXct(seq(as.Date(paste(month,day,1999), format="%m %d %Y"),by='days', length.out = 365)+1)
      endDate<-as.POSIXct(as.Date(paste(monthEnd,dayEnd,1999), format="%m %d %Y")+1)
      
      # hold on to original dayQuant vals
      dayQuantTemp<-dayQuant
      
      # # # # apply smoothing, uses caTools
      for(i in 2:6){
        dayQuant[,i]<-runmean(dayQuant[,i], 15, align = "center", endrule = "mean") # window width, set at 60
      }
      
      # replace first smoothed value with original
      dayQuant[1,]<-dayQuantTemp[1,]
      
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


