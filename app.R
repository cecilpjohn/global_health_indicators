
# Libraries ---------------------------------------------------------------


library(shiny)
library(shinydashboard)
library(scales)
library(fresh)
library(readxl)
library(DT)
library(tidyverse)



# Load required files -----------------------------------------------------


file <- read_excel("data/names.xlsx")

c("fj-total_funding.csv",
  "fj-cc_funding.csv",
  "fj-pheic.csv",
  "gini2023-05-28_21-52-08.csv",
  "corruption2023-05-28_17-06-27.csv",
  "espar2023-05-28_17-26-51.csv",
  "jee2023-05-28_17-55-56.csv",
  "ghsindex2023-05-28_17-18-01.csv",
  "rand2023-05-28_17-34-23.csv",
  "worldriskindex2023-05-28_17-29-52.csv",
  "inform2023-05-28_18-05-29.csv",
  "naphs2023-05-28_18-27-53.csv",
  "sdg2023-05-28_18-01-56.csv",
  "pvs2023-05-28_18-03-30.csv") %>%
  map(
    ~ file.path("data", .x) %>%
      read_csv()) %>%
  set_names("df_total_funding",
            "df_cc_funding",
            "df_pheic",
            "df_gini",
            "df_corruption",
            "df_spar",
            "df_jee",
            "df_ghsindex",
            "df_rand",
            "df_worldrisk",
            "df_inform",
            "df_naphs",
            "df_sdg",
            "df_pvs") %>%
  list2env(.GlobalEnv)


# ui ----------------------------------------------------------------------



ui <- dashboardPage(title = "Global Health Security Country Indicators and Financing", 
                    
                    dashboardHeader(disable = TRUE),
                    
                    dashboardSidebar(disable = TRUE),
                    
                    dashboardBody(
                      
                      # Setup googlefonts
                      
                      use_googlefont("IBM Plex Sans"),
                      use_theme(create_theme(
                        theme = "default",
                        bs_vars_font(
                          family_sans_serif = "'IBM Plex Sans', sans-serif")
                      )),
                      
                      tags$head(
                        
                        tags$style(HTML(".content-wrapper, .right-side {
                                  background-color: #001F3F;
                                  }
                                  
                                  a {
                                  color: #000000;
                                  }
                                  
                                  div.box-header {
                                  text-align: center;
                                  }
                                  
                                  .tooltip {
                                  position: relative;
                                  display: inline-block;
                                  }
                                  
                                  .tooltip .tooltiptext {
                                  visibility: hidden;
                                  width: 120px;
                                  text-align: center;
                                  padding: 5px 0;
                                  border-radius: 6px;
                                  }
                                  ")
                        )),
                      
                      # Title
                      
                      fluidRow(
                        column(width = 8,
                               box(width = NULL, 
                                   height = 100, 
                                   solidHeader=TRUE,
                                   background = "navy",
                                   h1("GLOBAL HEALTH SECURITY COUNTRY INDICATORS AND FINANCING", 
                                      style = "font-size: 20pt;
                                      font-weight: bold;"))),
                        
                      ),
                      
                      fluidRow(column(width = 12,
                                      style = "background-color:#FFFFFF;"
                      )),
                      
                      fluidRow(column(width = 12),
                               br(),
                               br()
                      ),
                      
                      fluidRow(column(width = 12,
                                      style = "background-color:#FFFFFF;",
                                      column(width = 10, 
                                             br(),
                                             offset = 1,
                                             h2("Overview"),
                                             p("This dashboard is a compilation of global health security and financing indicators for different countries. The respective data sources for each indicator can be accessed directly by clicking on title of the chart. The charts reflect data as retrieved in May 2023 from the respective data sources."),
                                             br()
                                             
                                      ))),
                      
                      fluidRow(
                        column(width = 2, 
                               offset = 1, 
                               br(),
                               br(),
                               box(width = NULL, 
                                   background = "blue",
                                   solidHeader=TRUE,
                                   selectizeInput(
                                     inputId = "country",
                                     label = "Select:",
                                     choices = file$country,
                                     multiple = FALSE,
                                     options = list(placeholder = "Select an option...")
                                   )),
                               box(width = NULL, 
                                   solidHeader=TRUE,
                                   background = "navy",
                                   uiOutput("ccfundingBox")),
                               box(width = NULL, 
                                   solidHeader=TRUE,
                                   background = "navy",
                                   uiOutput("pheicfundingBox"))
                        ),
                        
                        column(width = 8,
                               box(
                                 width = NULL,
                                 solidHeader = TRUE,
                                 background = "navy",
                                 br(),
                                 column(width = 8,
                                        box(width = NULL, 
                                            title = tags$a(href="https://tracking.ghscosting.org/" , 
                                                           title = "Click for data source",
                                                           target="_blank",
                                                           "International Financing For Global Health Security"), 
                                            solidHeader = TRUE,
                                            plotOutput("plot_cc_funding", height = 400))
                                 ),
                                 column(width = 4,
                                        box(width = NULL, 
                                            title = tags$a(href="https://tracking.ghscosting.org/" ,
                                                           title = "Click for data source",
                                                           target="_blank",
                                                           "International Financing For PHEICs"),
                                            solidHeader = TRUE,
                                            plotOutput("plot_pheic", height = 200))
                                 )
                               ))),
                      
                      fluidRow(
                        column(
                          width = 10,
                          offset = 1,
                          box(
                            width = NULL,
                            solidHeader = TRUE,
                            background = "navy",
                            br(),
                            column(width = 6,
                                   box(width = NULL, 
                                       title = tags$a(href="https://extranet.who.int/e-spar/#capacity-score" ,
                                                      title = "Click for data source",
                                                      target="_blank",
                                                      "SPAR Scores"),
                                       solidHeader = TRUE,
                                       plotOutput("plot_spar", height = 500))),
                            column(width = 6,
                                   box(width = NULL,
                                       title = tags$a(href="https://extranet.who.int/sph/jee" ,
                                                      title = "Click for data source",
                                                      target="_blank",
                                                      "JEE Scores"),
                                       solidHeader = TRUE,
                                       plotOutput("plot_jee", height = 500)))
                          ))),
                      
                      
                      fluidRow(column(width = 7,
                                      offset = 1,
                                      box(
                                        title = "Risk Indices",
                                        width = NULL,
                                        solidHeader = TRUE,
                                        background = "navy",
                                        column(width = 6, 
                                               br(),
                                               box(width = NULL,
                                                   title = tags$a(href="https://www.ghsindex.org/report-model/" ,
                                                                  title = "Click for data source",
                                                                  target="_blank",
                                                                  "GHS Index"),
                                                   solidHeader = TRUE,
                                                   plotOutput("plot_ghsindex", height = 150)),
                                               br(),
                                               br(),
                                               box(width = NULL,
                                                   title = tags$a(href="https://www.rand.org/pubs/research_reports/RR1605.html" ,
                                                                  title = "Click for data source",
                                                                  target="_blank",
                                                                  "Infectious Diseases Vulnerability Index"),
                                                   solidHeader = TRUE,
                                                   plotOutput("plot_rand", height = 150))),
                                        
                                        column(width = 6,
                                               br(),
                                               box(width = NULL,
                                                   title = tags$a(href="https://www.rand.org/pubs/research_reports/RR1605.html",
                                                                  title = "Click for data source",
                                                                  target="_blank",
                                                                  "World Risk Index"),
                                                   solidHeader = TRUE,
                                                   plotOutput("plot_worldrisk", height = 150)),
                                               br(),
                                               br(),
                                               box(width = NULL,
                                                   title = tags$a(href="https://drmkc.jrc.ec.europa.eu/inform-index" ,
                                                                  title = "Click for data source",
                                                                  target="_blank",
                                                                  "Inform Risk"),
                                                   solidHeader = TRUE,
                                                   plotOutput("plot_inform", height = 150))))),
                               
                               
                               column(width = 3,
                                      box(
                                        title = "Indices",
                                        width = NULL,
                                        solidHeader = TRUE,
                                        background = "navy", 
                                        column(width = 12,
                                               br(),
                                               box(width = NULL,
                                                   title = tags$a(href="https://dashboards.sdgindex.org/" ,
                                                                  title = "Click for data source",
                                                                  target="_blank",
                                                                  "SDG Performance"),
                                                   solidHeader = TRUE,
                                                   plotOutput("plot_sdg", height = 86)),
                                               box(width = NULL,
                                                   title = tags$a(href="https://data.worldbank.org/indicator/SI.POV.GINI" ,
                                                                  title = "Click for data source",
                                                                  target="_blank",
                                                                  "Gini Index"),
                                                   solidHeader = TRUE,
                                                   plotOutput("plot_gini", height = 86)),
                                               box(width = NULL,
                                                   title = tags$a(href="https://www.transparency.org/en/cpi/2022" ,
                                                                  title = "Click for data source",
                                                                  target="_blank",
                                                                  "Corruption Perceptions Index"),
                                                   solidHeader = TRUE,
                                                   plotOutput("plot_corruption", height = 86))
                                        )))),
                      
                      
                      
                      fluidRow(column(width = 5, 
                                      offset = 1, 
                                      box(width = NULL,
                                          solidHeader=TRUE,
                                          height = 250,
                                          title = tags$a(href="https://extranet.who.int/sph/naphs" ,
                                                         title = "Click for data source",
                                                         target="_blank",
                                                         "NAPHS Status"),
                                          DT::dataTableOutput("naphs_table"))),
                               column(width = 5, 
                                      box(width = NULL,
                                          solidHeader=TRUE,
                                          height = 250,
                                          title = tags$a(href="https://www.woah.org/en/what-we-offer/improving-veterinary-services/pvs-pathway/pvs-pathway-state-of-play-and-mission-reports/" ,
                                                         title = "Click for data source",
                                                         target="_blank",
                                                         "PVS Status"),
                                          DT::dataTableOutput("pvs_table"))
                                      
                               ))
                      
                    )
)


# server ------------------------------------------------------------------


server <- function(input, 
                   output, 
                   session) 
{
  
  
  # output$logo <- renderImage({
  #   
  #   list(src = "logo.jpg",
  #        contentType = "image/jpg",
  #        alt = "logo",
  #        height = 150,
  #        width = "auto"
  #   )
  # }, deleteFile = FALSE)
  
  
  output$plot_cc_funding <- renderPlot({ 
    
    ### Load file for total funding
    
    # df_total_funding <- read_csv("data/fj-total_funding.csv")
    
    ### Filter to country
    
    dfplot_total_funding <- df_total_funding %>% 
      filter(Economy == input$country)
    
    ### Load file for Core capacity funding
    # df_cc_funding <- read_csv("data/fj-cc_funding.csv")
    
    ### Filter to country
    
    dfplot_cc_funding <- df_cc_funding %>% 
      filter(Economy == input$country) %>%
      filter(`core capacity` != "Unspecified") %>% 
      top_n(10, `total disbursed`) %>%
      arrange(desc(`total disbursed`))
    
    ### Set string length for labels to avoid overflow
    
    dfplot_cc_funding$`core capacity` <- str_wrap(dfplot_cc_funding$`core capacity`,
                                                  width = 20)
    
    ### Plot for core capacities
    
    ggplot(dfplot_cc_funding, 
           aes(x = `total disbursed`, 
               y = reorder(`core capacity`, `total disbursed`))) +
      geom_bar(stat = "identity", 
               fill = "#001F3F", 
               width = 0.7) +
      geom_text(aes(label = label_dollar(scale_cut = cut_short_scale(), 
                                         accuracy = 0.01)(`total disbursed`)), 
                hjust = -0.3) +
      scale_x_continuous(limits = c(0, max(dfplot_cc_funding$`total disbursed`) * 1.4), 
                         expand=c(0,0)) +
      scale_y_discrete()+
      xlab("") +
      ylab("") +
      
      theme(
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "#ced4da", 
                                          size = 0.4),
        panel.grid.major.x = element_blank(),
        axis.ticks.length = unit(0, "mm"),
        axis.title = element_blank(),
        axis.line.y.left  = element_line(color = "#000000", 
                                         size = 1.5),
        axis.text.y = element_text(margin = margin(r = 5)),
        #aspect.ratio = 4/5,
        axis.text.x = element_blank(),
        text = element_text(color = "#000000")
      )
    
  })
  
  output$ccfundingBox <- renderValueBox({
    
    ###Load file for total funding
    
    # df_total_funding <- read_csv("data/fj-total_funding.csv")
    
    ### Filter to country
    
    dfplot_total_funding <- df_total_funding %>% 
      filter(Economy == input$country)
    
    valueBox(
      value = label_dollar(scale_cut = cut_short_scale(), 
                           accuracy = 0.01)(sum(dfplot_total_funding$`total funding disbursed`,
                                                na.rm=T)), 
      subtitle = "Total Funding disbursed during 2014-2022", 
      color = "navy",
      width = 12)
  })
  
  output$pheicfundingBox <- renderValueBox({
    
    ###Load file
    # df_pheic <- read_csv("data/fj-pheic.csv")
    
    ### Filter to country
    dfplot_pheic <- df_pheic %>% 
      filter(Economy == input$country)
    
    valueBox(
      value = label_dollar(scale_cut = cut_short_scale(), 
                           accuracy = 0.01)(sum(dfplot_pheic$`Total disbursed to recipient`,
                                                na.rm=T)),
      subtitle = "PHEIC funding disbursed during 2014-2022",
      color = "navy",
      width = 12)
  })
  
  output$plot_pheic <- renderPlot({
    
    ### Load file
    
    # df_pheic <- read_csv("data/fj-pheic.csv")
    
    ### Filter to country
    dfplot_pheic <- df_pheic %>% 
      filter(Economy == input$country)
    
    ### Set string length for labels to avoid overflow
    dfplot_pheic$`PHEIC name` <- str_wrap(dfplot_pheic$`PHEIC name`, 
                                          width = 10)
    
    ### Create plot for country
    ggplot(dfplot_pheic) +
      geom_col(aes((`PHEIC name`), 
                   `Total disbursed to recipient`), 
               fill = "#001F3F", 
               width = 0.4) +
      geom_text(aes(x=`PHEIC name`,  
                    y=`Total disbursed to recipient`, 
                    label = label_dollar(scale_cut = cut_short_scale(), 
                                         accuracy = 0.01)(`Total disbursed to recipient`)), 
                vjust = -0.7) +
      scale_x_discrete()+
      scale_y_continuous(
        limits = c(0, max(dfplot_pheic$`Total disbursed to recipient`) * 1.4),
        expand = c(0, 0)) +
      xlab("") +
      ylab("") +
      
      theme(
        panel.background = element_rect(fill = "white"),
        panel.grid.major.x = element_line(color = "#ced4da", 
                                          size = 0.4),
        axis.ticks.length = unit(0, "mm"),
        axis.title = element_blank(),
        axis.line.x.bottom = element_line(color = "#000000", 
                                          size = 1),
        axis.text.y = element_blank(),
        axis.text.x = element_text(margin = margin(t = 5, 
                                                   unit="pt")),
        text=element_text(color = "#000000")
      )
    
  })
  
  output$plot_sdg <- renderPlot({
    
    # ###Load file
    # df_sdg <- read.csv("data/sdg2023-05-28_18-01-56.csv")
    
    df <- tibble(x = 1, y = 1) %>% 
      mutate(y_negative = 1 - y) %>% 
      pivot_longer(cols = -x) 
    
    dfplot_sdg <- df_sdg %>% 
      filter(Economy == input$country)
    
    ggplot(data = df, aes(x = x, y = 1)) +
      geom_col(show.legend = FALSE, fill = dfplot_sdg$SDG3_dash_color) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("green" = "#307351",
                                   "yellow" = "#f4d35e",
                                   "orange" = "#fb8500",
                                   "red" =  "#9E2B25",
                                   "grey" = "#ced4da"))+
      
      theme_void()+
      
      labs(
        title = paste(paste("Status:", dfplot_sdg$SDG3_dash_value, "\n", "Trend:", 
                            dfplot_sdg$SDG3_trend_value)))+
      theme(
        text = element_text(color = "#000000"),
        aspect.ratio = 1/1,
        plot.title = element_text(hjust=0.5),
        legend.position = "none")
    
  })
  
  output$plot_gini <- renderPlot({
    
    ###Load file
    # df_gini <- read_csv("data/gini2023-05-28_21-52-08.csv")
    
    
    dfplot_gini <- df_gini %>% 
      filter(Economy == input$country)
    
    score_gini <- dfplot_gini$Gini
    
    total_score_gini <- 100
    
    gini_country <- tibble(x = 1, y = score_gini/100) %>% 
      mutate(y_negative = 1 - y) %>% 
      pivot_longer(cols = -x) 
    
    ggplot(gini_country, 
           aes(x = x, 
               y = value, 
               fill = name)) +
      geom_col(show.legend = FALSE) +
      geom_text(x = -2, 
                aes(label = paste(dfplot_gini$Gini))) +
      coord_polar(theta = "y",direction = -1) +
      xlim(c(-2, 2)) +
      scale_fill_manual(values = c("#001F3F", "#ced4da")) +
      theme_void() +
      labs(title = paste("Index value from", dfplot_gini$Year)) +
      theme(
        text = element_text(color = "#000000"),
        aspect.ratio = 1/1,
        plot.title = element_text(hjust = 0.5))
    
  })
  
  output$plot_corruption <- renderPlot({
    
    ###Load file
    # df_corruption <- read_csv("data/corruption2023-05-28_17-06-27.csv")
    
    ###
    dfplot_corruption <- df_corruption %>% 
      filter(Economy == input$country)
    
    score_corruption <- dfplot_corruption$`CPI score 2022`
    
    total_score_corruption <- 100
    
    corruption_country <- tibble(x = 1, 
                                 y = score_corruption/100) %>% 
      mutate(y_negative = 1 - y) %>% 
      pivot_longer(cols = -x) 
    
    ggplot(corruption_country, 
           aes(x = x, 
               y = value, 
               fill = name)) +
      geom_col(show.legend = FALSE) +
      geom_text(x = -2, 
                aes(label = paste(dfplot_corruption$`CPI score 2022`))) +
      coord_polar(theta = "y", 
                  direction = -1) +
      xlim(c(-2, 2)) +
      scale_fill_manual(values = c("#001F3F", 
                                   "#ced4da")) +
      theme_void() +
      labs(title = paste("Rank",dfplot_corruption$Rank,"/", 
                         max(df_corruption$Rank, 
                             na.rm = TRUE)))+
      theme(
        text = element_text(family = "IBM Plex Sans", 
                            color = "#000000"),
        aspect.ratio = 1/1,
        plot.title = element_text(hjust = 0.5))
  })
  
  output$plot_spar <- renderPlot({
    
    ###Load file
    # df_spar <- read_csv("data/espar2023-05-28_17-26-51.csv")
    
    ###Load key for full names of capacities
    key_spar <- read_excel("data/key-spar.xlsx")
    
    df_spar_long <- df_spar %>%
      pivot_longer(cols = starts_with("C."), 
                   names_to = "Capacity", 
                   values_to = "Score") %>% 
      mutate(Capacity = case_when(
        Capacity %in% key_spar$Capacity ~ key_spar$Title[match(Capacity, key_spar$Capacity)],
        TRUE ~ Capacity)) %>% 
      mutate(category = ifelse(Score > 60, "green", 
                               ifelse(Score > 20, "yellow",
                                      ifelse(Score > 0,"red", NA))))
    
    
    ###Filter to country
    dfplot_spar_long <- df_spar_long %>% 
      filter(Economy == input$country) 
    
    ###String length for labels
    dfplot_spar_long$Capacity <- str_wrap(dfplot_spar_long$Capacity, 
                                          width = 12)
    
    ###plot for spar
    ggplot(dfplot_spar_long, 
           aes(x = Capacity, 
               y = Score, 
               fill = category)) +
      geom_bar(stat = "identity") +
      scale_y_continuous(
        limits = c(-20, 120),
        expand = c(0, 0),
        breaks = c(0, 
                   20, 
                   60, 
                   100)) +
      coord_polar(clip = "off") +
      scale_fill_manual(values = c("red" =  "#9E2B25",
                                   "yellow"= "#f4d35e",
                                   "green" = "#307351"))+
      xlab("") +
      ylab("") +
      
      theme(
        text = element_text(color = "#000000", 
                            family = "IBM Plex Sans"),
        aspect.ratio = 2/2,
        axis.text.x = element_text(margin = margin(t=5, 
                                                   r = 5, 
                                                   b=5, 
                                                   l=5)),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(face = "bold", 
                                  size = 9, 
                                  hjust=0.5),
        plot.subtitle = element_text(size = 8, 
                                     hjust = 0.5),
        plot.caption = element_text(size = 8),
        panel.background = element_rect(fill = "white", 
                                        color = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "#ced4da",
                                          size = 0.5, 
                                          linetype = 2),
        legend.position = "None") +
      
      labs(subtitle = "Scores from 2022\n")
    
  })
  
  output$plot_jee <- renderPlot({
    
    # df_jee <- read_csv("data/jee2023-05-28_17-55-56.csv")
    
    dfplot_jee <- df_jee %>% 
      filter(Economy == input$country)
    
    dfplot_jee$capacity <- str_wrap(dfplot_jee$capacity, 
                                    width = 20)
    
    ggplot(dfplot_jee, 
           aes(x = capacity, 
               y = score, 
               color = category)) +
      geom_segment(aes(x = capacity, 
                       xend = capacity, 
                       y = 0, 
                       yend = score), 
                   color = "#ced4da", 
                   linewidth = 0.5) +
      geom_point(size = 4)+
      facet_wrap(~capacity_category, 
                 nrow = 2, 
                 ncol = 2, 
                 scales = "free", 
                 dir = "v")+
      scale_color_manual(values = c("red" =  "#9E2B25",
                                    "yellow"= "#f4d35e",
                                    "green" = "#307351"))+ 
      scale_y_continuous(limits = c(0, 100), 
                         breaks = c(0, 
                                    20,
                                    60,
                                    100)) +
      coord_flip(clip = "off") +
      
      theme_light() +
      theme(
        text = element_text(family = "IBM Plex Sans", 
                            color="#000000"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#FFB30F", 
                                          size = 0.5, 
                                          linetype = 2),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "IBM Plex Sans"),
        axis.line.x.bottom = element_line(color = "#000000", 
                                          size = 2),
        strip.background = element_rect(fill ="#001F3F"),
        strip.text = element_text(color="#FFFFFF"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
      xlab("") +
      ylab("") +
      labs(subtitle = paste("Scores from", 
                            max(dfplot_jee$year)))
    
  })
  
  output$plot_ghsindex <- renderPlot({
    
    ###Load file
    # df_ghsindex <- read_csv("data/ghsindex2023-05-28_17-18-01.csv")
    
    ###Filter to country
    dfplot_ghsindex <- df_ghsindex %>% 
      filter(Economy == input$country)
    
    ###plot ghsindex
    ggplot(df_ghsindex, 
           aes(x = `OVERALL SCORE`)) +
      geom_histogram(fill = "#ced4da", 
                     alpha=1, 
                     bins=50, 
                     color = "white") +
      geom_vline(xintercept = dfplot_ghsindex$`OVERALL SCORE`, 
                 color = "#001F3F", 
                 linewidth = 1.5, 
                 linetype ="dashed") +
      
      scale_x_continuous(limits = c(0, 100), 
                         breaks = c(0,
                                    20,
                                    40,
                                    60,
                                    80,
                                    100), 
                         expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      xlab("") +
      ylab("") +
      
      theme_light() +
      
      theme(
        text = element_text(family = "IBM Plex Sans"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "#000000", 
                                    linewidth = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        plot.title = element_text(
          family = "IBM Plex Sans", 
          hjust  = 0.5,
          colour = "#000000"))+
      
      labs(title = paste("Rank",dfplot_ghsindex$RANK,"out of",
                         max(df_ghsindex$RANK, na.rm = TRUE), "|",
                         "Score:",dfplot_ghsindex$`OVERALL SCORE`))
    
  })
  
  output$plot_rand <- renderPlot({
    
    ###Load file
    # df_rand <- read_csv("data/rand2023-05-28_17-34-23.csv")
    
    ###Filter to country
    dfplot_rand <- df_rand %>% 
      filter(Economy == input$country)
    
    ###Plot infectious diseases vulnerability index
    ggplot(df_rand, 
           aes( x = `Overall Score Normed`)) +
      geom_histogram(fill = "#ced4da", 
                     alpha = 1, 
                     bins = 50, 
                     color = "white") +
      geom_vline(xintercept = dfplot_rand$`Overall Score Normed`, 
                 color = "#001F3F", 
                 linewidth = 1.5, 
                 linetype ="dashed")+
      
      scale_x_continuous(limits = c(0, 1), 
                         breaks = c(0, 
                                    0.20,
                                    0.40,
                                    0.60,
                                    0.80,
                                    1.00),
                         expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0))+
      xlab("") +
      ylab("") +
      
      theme_light() +
      
      theme(
        text = element_text(family = "IBM Plex Sans"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "#000000", 
                                    linewidth = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(hjust=0),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust  = 0.5,
                                  colour = "#000000")) +
      
      labs(title = paste("Rank",dfplot_rand$Rank,"out of",
                         max(df_rand$Rank, na.rm = TRUE), "|","Score:",
                         dfplot_rand$`Overall Score Normed`)
      )
    
  })
  
  output$plot_worldrisk <- renderPlot({
    
    ###Load file
    # df_worldrisk <- read_csv("data/worldriskindex2023-05-28_17-29-52.csv")
    
    ###Filter to country
    dfplot_worldrisk <- df_worldrisk %>% 
      filter(Economy == input$country)
    
    ###Plot World Risk Index
    ggplot(df_worldrisk, 
           aes(x = WRI)) +
      geom_histogram(fill = "#ced4da", 
                     alpha = 1, 
                     bins = 50, 
                     color = "white")+
      geom_vline(xintercept = dfplot_worldrisk$WRI, 
                 color = "#001F3F", 
                 linewidth = 1.5, 
                 linetype = "dashed") +
      
      scale_x_continuous(limits = c(0, NA),
                         expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      xlab("") +
      ylab("") +
      
      theme_light() +
      
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(hjust=0),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "#000000", 
                                    linewidth = 0.5),
        plot.title = element_text(hjust  = 0.5,
                                  colour = "#000000")) +
      
      labs(
        title = paste("Rank",dfplot_worldrisk$Rank,"out of",
                      max(df_worldrisk$Rank, na.rm = TRUE), "|","WRI:",
                      dfplot_worldrisk$WRI)
      )
    
  })
  
  
  output$plot_inform <- renderPlot({
    
    ###Load File
    # df_inform <- read_csv("data/inform2023-05-28_18-05-29.csv")
    
    ###Filter to country
    dfplot_inform <- df_inform %>% 
      filter(Economy == input$country)
    
    ###Plot inform risk chart
    ggplot(df_inform, 
           aes(x=`INFORM RISK`)) +
      geom_histogram(fill = "#ced4da", 
                     alpha = 1, 
                     bins = 50, 
                     color = "white")+
      geom_vline(xintercept = dfplot_inform$`INFORM RISK`, 
                 color = "#001F3F", 
                 linewidth = 1.5, 
                 linetype ="dashed") +
      
      scale_x_continuous(limits = c(0, 10), 
                         expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      xlab("") +
      ylab("") +
      
      theme_light()+
      
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "#000000", 
                                    linewidth = 0.5),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(hjust=0),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        plot.title = element_text(hjust  = 0.5,
                                  colour = "#000000"))+
      
      labs(title = paste("Score:", dfplot_inform$`INFORM RISK`))
    
  })
  
  output$naphs_table <- DT::renderDataTable({
    
    ###Load File
    # df_naphs <- read_csv("data/naphs2023-05-28_18-27-53.csv")
    
    ###Filter to country
    dfplot_naphs <- df_naphs %>% 
      mutate(Year = year) %>% 
      filter(Economy == input$country) %>% 
      select(Status, Year, Documents)
    
    dfplot_naphs$Documents <- ifelse(is.na(dfplot_naphs$Status), 
                                     "No Records Found", 
                                     ifelse(is.na(dfplot_naphs$Documents), 
                                            "NA",
                                            paste0("<a href='",dfplot_naphs$Documents,
                                                   "'>",dfplot_naphs$Documents,"</a>")))
    
    datatable(dfplot_naphs, 
              options = list(dom = 't'), 
              escape = FALSE)
    
  })
  
  output$pvs_table <- DT::renderDataTable({
    
    ###Load File
    # df_pvs <- read_csv("data/pvs2023-05-28_18-03-30.csv")
    
    ###Filter to country
    dfplot_pvs <- df_pvs %>% 
      filter(Economy == input$country) %>% 
      select(`Report Type`, 
             Version, 
             Year, 
             `Report Link`)
    
    dfplot_pvs$`Report Link` <- ifelse(is.na(dfplot_pvs$`Report Type`), 
                                       "No Records Found",
                                       ifelse(is.na(dfplot_pvs$`Report Link`),
                                              "NA",
                                              paste0("<a href='",dfplot_pvs$`Report Link`,
                                                     "'>",dfplot_pvs$`Report Link`,"</a>")))
    
    datatable(dfplot_pvs, 
              options = list(dom = 't'), 
              escape = FALSE)
  })
  
}

shinyApp(ui, server)