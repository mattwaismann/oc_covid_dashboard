## app. R ##
source("global.R")

fonts()

  
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Daily Updates", tabName = "daily"),
    menuItem("Important Metrics", tabName = "metrics")
  )
)
    
  
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "daily",
            fluidRow(
             infoBox("New cases", subtitle = paste("updated: ", today_date), df$daily_cases_repo %>% tail(1), color = "yellow", fill = T),
             infoBox("New deaths", subtitle = paste("updated: ", today_date), df$daily_dth.x %>% tail(1), color = "yellow", fill = T )
            ),
            fluidRow(
              box(sliderInput("n_days",label = "Select a number of days", value = 60, min = 14, max = 365),
                  width = 4,
                  status = "primary"
              )
            ),
            fluidRow(
              tabBox(
                tabPanel("By reported date", 
                        plotlyOutput("new_cases"),
                         
                ),
                tabPanel("By specimen collection date",
                         plotlyOutput("new_specimen")
                )
              ),
              tabBox(
                tabPanel("By reported date", 
                         plotlyOutput("new_deaths"),
                         
                ),
                tabPanel("By actual death date",
                         plotlyOutput("death_date")
                )
              )
              
            ),
            fluidRow(
              box(
                plotlyOutput("hospicu"),
                status = "primary"
              ),
              box(
                plotlyOutput("daily_spec"),
                status = "primary"
              )
            )
    ),
    tabItem(tabName = "metrics",
            fluidRow(
              infoBox("This week's positive test rate", subtitle = paste("updated: ", today_date), paste(pos_mini_df$positivity_for_week %>% tail(1),"%"), color = "yellow", fill = T),
              infoBox("This week's adjusted case rate", subtitle = paste("updated: ", today_date), pos_mini_df$case_rate_for_week %>% tail(1), color = "yellow", fill = T),
              infoBox("This week's Health Equity Index Quartile", subtitle = paste("updated: ", today_date), paste(pos_mini_df$equity_index_for_week %>% tail(1), "%"), color = "yellow", fill = T),
            ),
            fluidRow(
              box(tags$h4("Tier Information"),"The state is now using three statistics to judge counties. With some exceptions, metrics must surpass the benchmark for the next tier in order to loosen restrictions. The latest numbers, along with tier reassignments, are released each Tuesday. The first is called the",strong("adjusted case rate"),". It takes new cases in a recent seven-day period — excluding cases at prisons and jails — and adjusts for population. In some areas, that number is modified to account for the volume of testing. Orange County's most recent adjusted case rate was", pos_mini_df$case_rate_for_week %>% tail(1),". 
                The second metric is the", strong("positive test rate")," which is the percentage of tests for the virus that come back positive. Orange County's most recent positive test rate was",pos_mini_df$positivity_for_week %>% tail(1),"%.
                The third metric measures whether positive tests in the most disadvantaged neighborhoods have significantly exceeded a county's overall rate — a disparity that's been widespread during the pandemic. The statistic is known as the", strong("Health Equity Index"),". Orange County’s most recent score is",pos_mini_df$equity_index_for_week %>% tail(1),"%.",
                status = "primary"
              ),
              box(plotOutput("positivity"), status = "primary"
              )
            ),
            fluidRow(
              box(plotOutput("case_rate"),
                  status = "primary"
              ),
              box(plotOutput("equity_index"),
                  status = "primary"
              )
            )
    )
    
  )
)

ui <- dashboardPage(skin = 'black',
  dashboardHeader(title = "Orange County COVID-19 Tracker", titleWidth = 450),
  sidebar, 
  body
)


server <- function(input,output){
  
  present <- df$total_cases_repo %>% length()
  start <-  reactive(present - input$n_days)
  
  output$new_cases <- renderPlotly({
    #new cases
    title <- 'Daily cases reported'
    sev_day_avg <- rollmean(df$daily_cases_repo,7,na.pad = TRUE, align = 'right')
    df['sev_day_avg_daily_cases_repo'] <-sev_day_avg
    
    g <- ggplot(df[start():present,],aes(x=date))+
          geom_col(data = df[start():present,],aes(y=daily_cases_repo, text = paste("New cases: ",daily_cases_repo,
                                                                                    "<br>7-day average: ",round(sev_day_avg_daily_cases_repo,1),
                                                                                    "<br>Date: ", date)),
                   fill = '#fa7e05', col = "#FF6F00") +
          geom_line(aes(y=sev_day_avg_daily_cases_repo), col = 'black', size = .7, alpha = .8) +
          labs(title = title,
               subtitle = paste("updated: ", today_date),
               x = "Date",
               y = "Reported cases") + 
          theme_few() +
          theme(axis.title = element_text(), text = element_text(family = "Verdana"))
    font <- list(family = "Verdana", size = 13,color = 'white')
    label <- list(bgcolor = "#232F34",
                  bordercolor = "transparent",
                  font = font)
    ggplotly(g, tooltip = "text") %>% layout(title = list(text = paste(title,
                                                                       '<br>',
                                                                       '<sup>',
                                                                       'updated: ',
                                                                       today_date)),hovermode = 'compare', font = font) %>% style(hoverlabel = label)
    
  })
  
  output$new_specimen <- renderPlotly({
    #new cases
    title <- 'Cases by specimen collection date'
    sev_day_avg <- rollmean(df$daily_cases_spec,7,na.pad = TRUE, align = 'right')
    df['sev_day_avg_daily_cases_spec'] <-sev_day_avg
    g <- ggplot(df[start():present,],aes(x=date)) +
      geom_col(data=df[start():present,],aes(y=daily_cases_spec, text = paste("New cases: ",daily_cases_spec,
                                                                              "<br>7-day average: ",round(sev_day_avg_daily_cases_spec,1),
                                                                              "<br>Date: ", date)), fill = '#fa7e05', col = "#FF6F00") +
      geom_line(aes(y=sev_day_avg_daily_cases_spec), col = 'black', size = .7, alpha = .8) +
      labs(title = title,
           subtitle = paste("updated: ", today_date),
           x = "Date",
           y = "Reported cases") + 
      theme_few() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana"))
    font <- list(family = "Verdana", size = 13,color = 'white')
    label <- list(bgcolor = "#232F34",
                  bordercolor = "transparent",
                  font = font)
    ggplotly(g, tooltip = "text") %>% layout(title = list(text = paste(title,
                                                                       '<br>',
                                                                       '<sup>',
                                                                       'updated: ',
                                                                       today_date)),hovermode = 'compare', font = font) %>% style(hoverlabel = label)
  })
  
  output$new_deaths <- renderPlotly({
    #new deaths
    title <- 'Daily deaths reported'
    sev_day_avg <- rollmean(df$daily_dth.x,7,na.pad = TRUE, align = 'right')
    df['sev_day_avg_daily_deaths_repo'] <- sev_day_avg
    g <- ggplot(df[start():present,],aes(x=date,daily_dth.x)) +
      geom_col(data=df[start():present,],aes(y=daily_dth.x, text = paste("New deaths: ",daily_dth.x,
                                                                         "<br>7-day average: ",round(sev_day_avg_daily_deaths_repo,1),
                                                                         "<br>Date: ", date)), fill = "#9354AB", col = "#B600FF") + 
      labs(title = title, 
           subtitle = paste("updated: ", today_date),
           x = "Date",
           y = "Reported deaths")+
      geom_line(aes(y=sev_day_avg_daily_deaths_repo), col = 'black', size = .7, alpha = .8) +
      theme_few() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana"))
    font <- list(family = "Verdana", size = 13,color = 'white')
    label <- list(bgcolor = "#232F34",
                  bordercolor = "transparent",
                  font = font)
    ggplotly(g, tooltip = "text") %>% layout(title = list(text = paste(title,
                                                                       '<br>',
                                                                       '<sup>',
                                                                       'updated: ',
                                                                       today_date)),hovermode = 'compare', font = font) %>% style(hoverlabel = label)
    
  })
  
  output$death_date <- renderPlotly({
    #new deaths
    title <- 'Deaths by date they occured'
    sev_day_avg <- rollmean(df$dth_date,7,na.pad = TRUE, align = 'right')
    df['sev_day_avg_daily_deaths_date'] <- sev_day_avg
    g <- ggplot(df[start():present,],aes(x=date,dth_date))+
      geom_col(data=df[start():present,],aes(y=dth_date, text = paste("New deaths: ",dth_date,
                                                                      "<br>7-day average: ",round(sev_day_avg_daily_deaths_date,1),
                                                                      "<br>Date: ", date)), fill = "#9354AB", col = "#B600FF") + 
      labs(title = title, 
           subtitle = paste("updated: ", today_date),
           x = "Date",
           y = "Reported deaths")+
      geom_line(aes(y=sev_day_avg_daily_deaths_date), col = 'black', size = .7, alpha = .8) +
      theme_few() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana"))
    font <- list(family = "Verdana", size = 13,color = 'white')
    label <- list(bgcolor = "#232F34",
                  bordercolor = "transparent",
                  font = font)
    ggplotly(g, tooltip = "text") %>% layout(title = list(text = paste(title,
                                                                       '<br>',
                                                                       '<sup>',
                                                                       'updated: ',
                                                                       today_date)),hovermode = 'compare', font = font) %>% style(hoverlabel = label)
    
  })
  
  
  output$hospicu <- renderPlotly({
    #hospital/icu capacities
    title <- 'Hospital/ICU capacities'
    g <- df[start():present,] %>% select('date','hospital','icu') %>%
      ggplot(aes(x=date)) +
      geom_col(data=df[start():present,],aes(y=hospital, text = paste("Hospital patients: ",hospital,
                                                                      "<br>ICU patients: ", icu,
                                                                      "<br>Date: ", date)), fill = '#14DFEB', col = "#3B9EC4") +
      geom_col(data=df[start():present,],aes(y=icu), fill = "#F23B0D", col = "#FF0F00") +
      labs(title = "Hosptial/ICU capacities",
           x = "Date",
           y="Patients",
           subtitle = paste("updated: ", today_date)) +
      theme_few() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana"))
    font <- list(family = "Verdana", size = 13,color = 'white')
    label <- list(bgcolor = "#232F34",
                  bordercolor = "transparent",
                  font = font)
    ggplotly(g, tooltip = "text") %>% layout(title = list(text = paste(title,
                                                                       '<br>',
                                                                       '<sup>',
                                                                       'updated: ',
                                                                       today_date)),hovermode = 'compare', font = font) %>% style(hoverlabel = label)
  })
  
  output$daily_spec <- renderPlotly({
    #specimen collected
    title <- 'Specimen collected by date'
    sev_day_avg <- rollmean(df$daily_spec,7,na.pad = TRUE, align = 'right')
    df['sev_day_avg_daily_spec'] <- sev_day_avg
    g <- ggplot(df[start():present,],aes(x=date,daily_spec))+
      geom_col(data=df[start():present,],aes(y=daily_spec, text = paste("Specimen collected: ",daily_spec,
                                                                      "<br>7-day average: ",round(sev_day_avg_daily_spec,1),
                                                                      "<br>Date: ", date)), fill = '#98DD22', col = "#3DE21D") + 
      labs(title = title, 
           subtitle = paste("updated: ", today_date),
           x = "Date",
           y = "Specimen Collected")+
      geom_line(aes(y=sev_day_avg_daily_spec), col = 'black', size = .7, alpha = .8) +
      theme_few() +
      theme(axis.title = element_text(), text = element_text(family = "Verdana"))
    font <- list(family = "Verdana", size = 13,color = 'white')
    label <- list(bgcolor = "#232F34",
                  bordercolor = "transparent",
                  font = font)
    ggplotly(g, tooltip = "text") %>% layout(title = list(text = paste(title,
                                                          '<br>',
                                                          '<sup>',
                                                          'updated: ',
                                                          today_date)),
                                             hovermode = 'compare', font = font) %>% 
      style(hoverlabel = label)
    
  })
  
  

  output$positivity <- renderPlot({
    tier_cutoffs <- levels(cut(pos_mini_df$positivity_for_week, breaks = c(0,2,5,8,100)))
    pos_mini_df$tier <- cut(pos_mini_df$positivity_for_week, breaks = c(0,2,5,8,100))
    levels(pos_mini_df$tier) <- c("Yellow tier","Orange tier","Red tier","Purple tier")
    ymin = pos_mini_df$tier
    ymax <- pos_mini_df$tier
    levels(ymin) <- c(0,2,5,8)
    levels(ymax) <- c(2,5,8,100)
    pos_mini_df$ymax <- as.numeric(levels(ymax))[ymax]
    pos_mini_df$ymin <- as.numeric(levels(ymin))[ymin]
    #positivy 
    colors = c('#F58C0A','#F0240F','#BF28D7','#ECF20D')
    g <- pos_mini_df %>%
      ggplot(aes(x=week, y = positivity_for_week)) + 
      geom_point(data = pos_mini_df, aes(y=positivity_for_week,text = paste("Positivity rate: ",positivity_for_week)))+
      geom_line() +
      labs(title = "Orange County test postivity by week", 
           subtitle = paste("updated: ", today_date),
           x = "Week",
           y = "Test Positivity",
           fill = "Tier") +
      scale_x_continuous(labels = paste(pos_mini_df$start_week,"-", pos_mini_df$end_week), breaks = 1:length(pos_mini_df$start_week))+
      # tiers c(0,2,5,8,100)
      annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = 2,
               alpha = .4, fill = colors[4])+
      annotate("rect", xmin = 0, xmax = Inf, ymin = 2, ymax = 5,
               alpha = .4, fill = colors[1])+
      annotate("rect", xmin = 0, xmax = Inf, ymin = 5, ymax = 8,
               alpha = .4, fill = colors[2])+
      annotate("rect", xmin = 0, xmax = Inf, ymin = 8, ymax = 25,
               alpha = .4, fill = colors[3])+
      theme_few() + 
      ylim(c(0,25)) + 
      theme(axis.title = element_text(), text = element_text(family = "Verdana")) +
      theme(legend.position = 'top') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    g
    
  })
  
  output$case_rate <- renderPlot({
    #case rate 
    colors = c('#F58C0A','#F0240F','#BF28D7','#ECF20D')
    tier_cutoffs <- levels(cut(pos_mini_df$positivity_for_week, breaks = c(0,1,4,7,30)))
    pos_mini_df$tier <- cut(pos_mini_df$positivity_for_week, breaks = c(0,1,4,7,30))
    levels(pos_mini_df$tier) <- c("Yellow tier","Orange tier","Red tier","Purple tier")
    ymin = pos_mini_df$tier
    ymax <- pos_mini_df$tier
    levels(ymin) <- c(0,1,4,7)
    levels(ymax) <- c(0,1,4,7)
    pos_mini_df$ymax <- as.numeric(levels(ymax))[ymax]
    pos_mini_df$ymin <- as.numeric(levels(ymin))[ymin]
    g <- pos_mini_df %>%
      ggplot(aes(x=week, y =case_rate_for_week)) + 
      geom_point(data = pos_mini_df) +
      geom_line() +
      ylim(c(0,30)) +
      scale_x_continuous(labels = paste(pos_mini_df$start_week,"-", pos_mini_df$end_week), breaks = 1:length(pos_mini_df$start_week))+
      labs(title = "Orange County adjusted case rate by week", 
           subtitle = paste("updated: ", today_date),
           x = "Week",
           y = "Case rate",
           fill = "Tier") +
      # tiers c(0,2,5,8,100)
      annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = 2,
               alpha = .4, fill = colors[4])+
      annotate("rect", xmin = 0, xmax = Inf, ymin = 2, ymax = 4,
               alpha = .4, fill = colors[1])+
      annotate("rect", xmin = 0, xmax = Inf, ymin = 4, ymax = 7,
               alpha = .4, fill = colors[2])+
      annotate("rect", xmin = 0, xmax = Inf, ymin = 7, ymax = 30,
               alpha = .4, fill = colors[3])+
      theme_few() + 
      ylim(c(0,30)) + 
      theme(axis.title = element_text(), text = element_text(family = "Verdana")) +
      theme(legend.position = 'top') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    g
    
  })

  output$equity_index <- renderPlot({
    #equity index rate 
    colors = c('#F58C0A','#F0240F','#BF28D7','#ECF20D')
    tier_cutoffs <- levels(cut(pos_mini_df$positivity_for_week, breaks = c(0,2.2,5.2,8,100)))
    pos_mini_df$tier <- cut(pos_mini_df$positivity_for_week, breaks = c(0,2.2,5.2,8,100))
    levels(pos_mini_df$tier) <- c("Yellow tier","Orange tier","Red tier","Purple tier")
    ymin = pos_mini_df$tier
    ymax <- pos_mini_df$tier
    levels(ymin) <- c(0,2.1,5.2,8)
    levels(ymax) <- c(0,2.1,5,2.8)
    pos_mini_df$ymax <- as.numeric(levels(ymax))[ymax]
    pos_mini_df$ymin <- as.numeric(levels(ymin))[ymin]
    g <- pos_mini_df %>%
      ggplot(aes(x=week, y =equity_index_for_week)) + 
      geom_point(data = pos_mini_df) +
      geom_line() +
      scale_x_continuous(labels = paste(pos_mini_df$start_week,"-", pos_mini_df$end_week), breaks = 1:length(pos_mini_df$start_week))+
      labs(title = "Orange County Health Equity Index Quartile by week", 
           subtitle = paste("updated: ", today_date),
           x = "Week",
           y = "Equity index",
           fill = "Tier") +
      # tiers c(0,2,5,8,100)
      annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = 2.2,
               alpha = .4, fill = colors[4])+
      annotate("rect", xmin = 0, xmax = Inf, ymin = 2.2, ymax = 5.3,
               alpha = .4, fill = colors[1])+
      annotate("rect", xmin = 0, xmax = Inf, ymin = 5.3, ymax = 8,
               alpha = .4, fill = colors[2])+
      annotate("rect", xmin = 0, xmax = Inf, ymin = 8, ymax = 25,
               alpha = .4, fill = colors[3])+
      theme_few() + 
      ylim(c(0,25)) + 
      theme(axis.title = element_text(), text = element_text(family = "Verdana")) +
      theme(legend.position = 'top') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    g
    
  })
  
}

shinyApp(ui,server)

    