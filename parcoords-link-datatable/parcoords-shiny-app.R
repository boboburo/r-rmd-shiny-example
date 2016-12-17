library(shiny)
library(shiny dashboard
        library(dplyr)
        library(htmlwidgets)
        # Install main branch of parallel coordinates
        # devtools::install_github("timelyportfolio/parcoords")
        # currently, I'm using the Dimensions branch of the Parallel Coordinates github repo
        # devtools::install_github("timelyportfolio/parcoords@feature/dimensions")
        library(parcoords)
        library(survival)
        
        # Load data
        pc <- readRDS("data/pc.rds")
        
        server = function(input,output,session){
          # Prepare data frame for figure
          pcfig <- pc %>%
            filter(timetotreat<=60 & timetotreat>0)  %>% # filter patients with long time to treat for aesthetics, and some     had firstline prior to diagnosis of metastatic disease
            # filter(secondline_agent!="Other") %>%  # need this when making figure for second line
            select(firstline_agent, 
                   prior_nephrectomy,
                   kps_1, 
                   timetotreat, 
                   corrca_1, 
                   hb_1, 
                   anc_1, 
                   platelets_1, 
                   vitalstatus,
                   survival)
          
          # Generate parallel coordinates figure  
          output$parcoords = renderParcoords(
            parcoords(pcfig 
                      ,rownames=F
                      ,brushMode = "1D-axes"
                      ,alpha=0.2
                      ,axisDots = F
                      ,reorderable = T
                      ,queue = F 
                      ,color = list(
                        colorBy = "firstline_agent",
                        colorScale = htmlwidgets::JS("d3.scale.category10()"))
                      ,dimensions = list(
                        firstline_agent = list(
                          title="Drug"
                        ),
                        prior_nephrectomy = list(
                          title= "Prior Nephrectomy"
                          ,tickValues=c("Yes","No")
                        ),
                        kps_1 = list(
                          title = "Karnofsky PS"
                        ),
                        corrca_1 = list(
                          title = "Calcium"
                          ,tickValues = c(0, 6, 8, 10, 12, 14)
                        ),
                        timetotreat = list(
                          title = "Months to Treatment"
                          ,tickValues = c(0,12,24,36,48)
                        ),
                        hb_1 = list(
                          title="Hemoglobin"
                          ,tickValues = c(0, 6, 8, 10, 12, 14, 16)
                        ),
                        anc_1 = list(
                          title= "Neutrophil Count"
                          ,tickValues = c(0, 2, 4, 6, 8, 10, 12, 14, 16)
                        ),
                        platelets_1 = list(
                          title = "Platelets"
                        ),
                        survival = list(
                          title = "Months Survival"
                          ,tickValues = c(0, 24,48, 72, 96)
                        )
                      )
                      ,tasks = list(
                        htmlwidgets::JS(
                          "
                          function(){
                          debugger
                          // reverse order of timetotreat
                          this.parcoords.dimensions()['timetotreat']
                          .yscale
                          .domain(
                          this.parcoords.dimensions()['timetotreat'].yscale.domain().reverse()
                          
                          );
                          this.parcoords.hideAxis(['names', 'vitalstatus']);
                          //this.parcoords.hideAxis(['names']);
                          this.parcoords.removeAxes();
                          this.parcoords.render();
                          
                          // duplicated from the widget js code
                          //  to make sure reorderable and brushes work
                          if( this.x.options.reorderable ) {
                          this.parcoords.reorderable();
                          } else {
                          this.parcoords.createAxes();
                          }
                          
                          if( this.x.options.brushMode ) {
                          //reset the brush with None
                          this.parcoords.brushMode('None')
                          this.parcoords.brushMode(this.x.options.brushMode);
                          this.parcoords.brushPredicate(this.x.options.brushPredicate);
                          }
                          }
                          
                          " 
                        )
                        )
                        )
                        )  
          
          # This function takes the selected rown as input
          # and calculates the median survival with interquartile range
          # If no patients are selected, reports median not reached
          # Also returns correct errors if not enough patients to calculate IQR or if upper confidence interval not reached
          medianSurvival <- function(x) {
            survival <- Surv(time = pcfig[x,"survival"], event = pcfig[x,"vital status"]
                             survfit <- survfit(survival ~ 1)
                             median <- sprintf("%.1f", round(summary(survfit)$table["median"],1))
                             cilow <- sprintf("%.1f", round(summary(survfit)$table["0.95LCL"],1))
                             cihigh <- sprintf("%.1f", round(summary(survfit)$table["0.95UCL"],1))
                             
                             if(median=="NA") {
                               paste("Median Not Reached")
                             } else if(cilow=="NA") {
                               paste(median, " (Not enough patients at risk to calculate interquartile range)", sep="")
                             } else if(cihigh=="NA") {
                               paste(median, " (Interquartile Range: ", cilow, " - Not reached)", sep="")
                             } else {
                               paste(median, " (Interquartile Range: ", cilow, " - ", cihigh, ")", sep="")
                             }
          }
          
          output$mediansurvival <- renderText({
            ifelse(length(input$parcoords_brushed_row_names)>0, 
                   medianSurvival(input$parcoords_brushed_row_names),"")})
          
          output$patientnumber <- renderText({length(input$parcoords_brushed_row_names)})
          }
        ```))