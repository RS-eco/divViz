rm(list=ls()); invisible(gc())

# Load packages ----
#library(shinyWidgets)
#library(DT) # datatable()
library(ggplot2)
library(data.table)
library(dplyr)
library(dbplyr)
library(sf)
#library(tidyr)
library(patchwork)
#library(ggspatial)
#library(scico)
library(plotly)

# Needed for radar plot!!!
# Almost identical to coord_polar()
coord_straightpolar <- function(theta = 'x', start = 0, direction = 1, clip = "off") {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto(NULL, CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction), clip = clip,
          # This is the different bit
          is_linear = function(){TRUE})
}

# Load data ----
load("data/districts.rda")
districts <- sf::st_transform(districts, 31468)
load("data/landkreise.rda")
landkreise <- sf::st_transform(landkreise, 31468)

art_data <- readRDS("inst/extdata/art_data.rds")

taxa <- c("Vögel", "Schmetterlinge", "Libellen", "Heuschrecken")

CSS <- "
/* CSS for the labels */
/* CSS for the checkboxes */
.pretty input[value=Vögel]~.state label:after,
.pretty input[value=Vögel]~.state label:before {
  background-color: #1b9e77;
}
.pretty input[value=Schmetterlinge]~.state label:after, 
.pretty input[value=Schmetterlinge]~.state label:before {
  background-color: #d95f02;
}
.pretty input[value=Libellen]~.state label:after, 
.pretty input[value=Libellen]~.state label:before {
  background-color: #7570b3;
}
.pretty input[value=Heuschrecken]~.state label:after,
.pretty input[value=Heuschrecken]~.state label:before {
  background-color: #e7298a;
}
"

# User interface ----
ui <- fluidPage(
  tags$head(tags$style(HTML(CSS))),
  
  ## This is the overall title
  titlePanel("Visualisierungs-Tool für Biodiversitäts-Daten"),
  
  ## Menu on the side
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year_weight", label = "Zeitraum:", value = c(1980, 2019),
                  min = 1980, max = 2019, step = 1, ticks=F, sep=""),
      sliderInput(inputId = "month_weight", label = "Monate:", value = c(1, 12),
                  min = 1, max = 12, step = 1, ticks=F, sep=""),
      shinyWidgets::prettyCheckboxGroup(inputId = "class_order",
                                        label = "Wähle ein oder mehrere Taxa:",
                                        choiceNames = taxa, choiceValues = taxa, 
                                        selected = taxa, icon = icon("check"), fill=T, inline=T),
      uiOutput("family_choice"),
      uiOutput("cat_choice"),
      helpText("Arten sind anhand des gewählten Zeitraums, Monats und Taxons ausgewählt."),
      shinyWidgets::pickerInput(inputId = "district", 
                  label = "Wähle einen oder mehrere Regierungsbezirk(e):", 
                  choices = sort(na.omit(unique(art_data$district))),
                  selected = sort(na.omit(unique(art_data$district))),
                  options = list(
                    `actions-box` = TRUE, 
                    size = 10,
                    `selected-text-format` = "count > 3",
                    `count-selected-text` = "{0} Bezirke ausgewählt (von insgesamt {1})"
                  ), multiple = TRUE),
      helpText("Achtung: Das Tool stellt nur die gesammelten Rohdaten dar. 
      Um die Daten räumlich oder zeitlich zu filtern, ändere die Mindestanzahl an Beobachtungen:"),
      numericInput(inputId = "sp_weight", label = "Mindestanzahl an Beobachtungen pro Gridzelle:", value = 1, min = 1, max = 50), 
      numericInput(inputId = "temp_weight", label = "Mindestanzahl an Beobachtungen pro Jahr:", value = 1, min = 1, max = 1000), 
      shinyWidgets::prettyRadioButtons(inputId = "res",
                                       label = "Wähle die räumliche Auflösung:",
                                       choices = c("TK25", "TK"),
                                       selected = "TK25", thick=T, animation = "pulse",
                                       status = "info", inline = T),
      helpText("TK25 entspricht einer räumlichen Auflösung von ca. 6 x 6 km, während TK einer Auflösung von ca. 12 x 12 km entspricht. 
               Karten sind in der 3-degree Gauss-Kruger zone 4 Projektion dargestellt (EPSG:31468)."),
      width = 3),
    # Set the different tabs in the main panel
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Arten-Übersicht",
                           h3("Arten-Übersicht"),
                           h4(textOutput("subtitle1")),
                           plotlyOutput("plot123", height = 400, width = 1200),  
                           h4("Anzahl an Beobachtungen"),
                           plotlyOutput("plot456", height = 400, width = 1230), 
                  ),
                  tabPanel("Taxon-Vergleich",
                           h3("Taxon-Vergleich"),
                           h4("Artenvielfalt"),
                           plotlyOutput("plot7", height = 400, width = 1200),  
                           h4("Anzahl an Beobachtungen"),
                           plotlyOutput("plot8", height = 400, width = 1200)
                  ),
                  tabPanel("zeitlicher Vergleich",
                           h3("zeitlicher Vergleich"),
                           selectInput("interval", "Wähle ein Zeit-Interval:",
                                       c("7 Jahre" = 7,
                                         "10 Jahre" = 10,
                                         "15 Jahre" = 15)),
                           h4(textOutput("subtitle2")),
                           plotlyOutput("plot9", height = 325, width = 1295),
                           h4("Anzahl an Beobachtungen"),
                           plotlyOutput("plot10", height = 325, width = 1330)
                  ),
                  tabPanel("Arten-Vergleich",
                           h3("Arten-Vergleich"),
                           fluidRow(
                             column(1),
                             column(3, uiOutput("cat_choice2")),
                             column(3, uiOutput("cat_choice3")),
                             column(5)
                           ),
                           h4("Artvorkommen"),
                           plotlyOutput("plot11", height = 325, width = 640),
                           h4("Anzahl an Beobachtungen"),
                           plotlyOutput("plot12", height = 325, width = 730)
                  ),
                  tabPanel("räumliche Übersicht",
                           h3("räumliche Übersicht"),
                           DT::dataTableOutput("table1")
                  ),
                  tabPanel("zeitliche Übersicht",
                           h3("zeitliche Übersicht"),
                           DT::dataTableOutput("table2")
                  )
      ), width=7
    )
  )
)

# Server logic ----
server <- function(input, output) {
  datclassorder <- reactive({art_data %>% filter(class_order %in% input$class_order)}) %>% bindCache(input$class_order)
  
  datyearmon <- reactive({
    datclassorder() %>% filter(jahr >= input$year_weight[1],
                        jahr <= input$year_weight[2]) %>%
      filter(mon >= input$month_weight[1],
             mon <= input$month_weight[2]) 
  })
  
  output$family_choice <- renderUI({
    selectInput(inputId="family",
                label="Wähle eine Familie:", 
                choices = c("Alle Familien", sort(unique(datclassorder() %>% dplyr::select(family) %>% unlist()))),
                selected="Alle Familien")
  })
  
  datclass <- reactive({
    if(input$family != "Alle Familien"){
      datyearmon() %>% filter(family == input$family)
    } else{
      datyearmon()
    }
  })
  
  output$cat_choice <- renderUI({
    selectInput(inputId="spec",
                label="Wähle eine Art:", 
                choices = c("Alle Arten", sort(unique(datclass() %>% dplyr::select(art2) %>% unlist()))),
                selected="Alle Arten")
  })
  
  output$cat_choice2 <- renderUI({
    selectInput(inputId="spec2",
                label="Wähle die erste Art:", 
                choices = sort(unique(datclass() %>% dplyr::select(art2) %>% unlist())))
  })
  
  output$cat_choice3 <- renderUI({
    selectInput(inputId="spec3",
                label="Wähle die zweite Art:", 
                choices = 
                  sort(unique(datclass() %>% dplyr::select(art2) %>% 
                                unlist())[unique(datclass() %>% dplyr::select(art2) %>% unlist()) != input$spec2]))
  })
  
  datclassdist <- reactive({
    datclass() %>% filter(district %in% input$district)
  })
  
  dataspec <- reactive({
    if(input$spec != "Alle Arten"){
      datclassdist() %>% filter(art2 == input$spec)
    } else{
      datclassdist()
    }
  })
  
  dataset <- reactive({
    if(input$res == "TK25"){
      dataspec() %>% group_by(XLU, XRU, YLU, YLO, karte) %>% 
        summarise(`Species richness`=n_distinct(art2), 
                  `Number of records`=n()) %>% 
        filter(`Number of records` >= input$sp_weight)
    } else {
      dataspec() %>% group_by(XLU_rough, XRU_rough, YLU_rough, YLO_rough, karte) %>% 
        summarise(`Species richness`=n_distinct(art2), 
                  `Number of records`=n()) %>% 
        rename(XLU=XLU_rough, XRU=XRU_rough, YLU=YLU_rough, YLO=YLO_rough) %>%
        filter(`Number of records` >= input$sp_weight)
    }
  })
  
  dataspeccomp <- reactive({
    if(input$res == "TK25"){
      datclassdist() %>% filter(art2 %in% c(input$spec2, input$spec3)) %>% 
        group_by(XLU, XRU, YLU, YLO, art2, class_order) %>% 
        summarise(`Species richness`=n_distinct(art2), 
                  `Number of records`=n()) %>% 
        filter(`Number of records` >= input$sp_weight)
    } else {
      datclassdist() %>% filter(art2 %in% c(input$spec2, input$spec3)) %>% 
        group_by(XLU_rough, XRU_rough, YLU_rough, YLO_rough, art2, class_order) %>% 
        summarise(`Species richness`=n_distinct(art2), 
                  `Number of records`=n()) %>% 
        rename(XLU=XLU_rough, XRU=XRU_rough, YLU=YLU_rough, YLO=YLO_rough) %>%
        filter(`Number of records` >= input$sp_weight)
    }
  })
  
  datatime <- reactive({
    if(input$res == "TK25"){
      dataspec() %>% group_by(jahr, class_order) %>% 
        summarise(`Species richness`= n_distinct(art2), 
                  `Number of records`= n(),
                  `Number of occupied grid cells`= n_distinct(XLU, XRU, YLU, YLO)) %>% 
        filter(`Number of records` >= input$temp_weight)
    } else {
      dataspec() %>% group_by(jahr, class_order) %>% 
        summarise(`Species richness`= n_distinct(art2), 
                  `Number of records`= n(),
                  `Number of occupied grid cells`= n_distinct(XLU_rough, XRU_rough, YLU_rough, YLO_rough)) %>% 
        filter(`Number of records` >= input$temp_weight)
    }
  })
  
  dataspacetime <- reactive({
    if(input$res == "TK25"){
      dataspec() %>% group_by(jahr, XLU, XRU, YLU, YLO, karte, class_order) %>% 
        summarise(`Species richness`=n_distinct(art2), 
                  `Number of records`=n()) %>% 
        filter(`Number of records` >= input$sp_weight)
    } else {
      dataspec() %>% ungroup() %>% 
        group_by(jahr, XLU_rough, XRU_rough, YLU_rough, YLO_rough, class_order) %>% 
        summarise(`Species richness`=n_distinct(art2), 
                  `Number of records`=n()) %>% 
        rename(XLU=XLU_rough, XRU=XRU_rough, YLU=YLU_rough, YLO=YLO_rough) %>%
        filter(`Number of records` >= input$sp_weight)
    }
  })
  
  datagroup <- reactive({
    if(input$res == "TK25"){
      datclassdist() %>% 
        group_by(XLU, XRU, YLU, YLO, class_order) %>% 
        summarise(`Species richness`=n_distinct(art2),
                  `Number of records`=n()) %>% 
        filter(`Number of records` >= input$sp_weight)
    } else {
      datclassdist() %>% 
        group_by(XLU_rough, XRU_rough, YLU_rough, YLO_rough, class_order) %>% 
        summarise(`Species richness`=n_distinct(art2),
                  `Number of records`=n()) %>% 
        rename(XLU=XLU_rough, XRU=XRU_rough, YLU=YLU_rough, YLO=YLO_rough) %>%
        filter(`Number of records` >= input$sp_weight)
    }
  })
  
  datadistrict <- reactive({
    dataspec() %>% group_by(district, class_order) %>% 
      summarise(`Species richness`=n_distinct(art2),
                `Number of records`=n(),
                `Number of occupied grid cells`= n_distinct(XLU_rough, XRU_rough, YLU_rough, YLO_rough)) %>% 
      tidyr::pivot_longer(names_to="var", values_to="val", cols=-c(district, class_order)) %>%
      tidyr::drop_na()
  })
  
  shape <- reactive({
    if (length(input$district) > 1){
      districts %>% filter(BEZ_RBZ %in% input$district)
    } else{
      landkreise %>% filter(BEZ_RBZ == input$district)
    }
  })
  
  output$subtitle1 <- renderText({
    if(input$spec == "Alle Arten"){
      "Artenvielfalt"
    } else{
      "Artvorkommen"
    }
  })
  
  output$subtitle2 <- renderText({
    if(input$spec == "Alle Arten"){
      "Artenvielfalt"
    } else{
      "Artvorkommen"
    }
  })
  
  output$plot123 <- renderPlotly({
    if(input$spec == "Alle Arten"){
      p1 <- dataset() %>% ggplot() + 
        geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                             ymax="YLO", fill="`Species richness`")) + 
        scico::scale_fill_scico(name="Artenvielfalt", palette="roma", na.value= "grey50", direction=-1) + 
        geom_sf(data=shape(), fill="transparent", col="black") +
        labs(x="Breitengrad", y="Längengrad") + 
        coord_sf(xlim = c(min(st_coordinates(shape())[,'X']),
                          max(st_coordinates(shape())[,'X'])),
                 ylim = c(min(st_coordinates(shape())[,'Y']),
                          max(st_coordinates(shape())[,'Y']))) + 
        theme_bw() + theme(legend.key.height=unit(1.5, "in"), 
                           legend.title=element_text(size=12, face="bold", vjust=0.85), 
                           legend.background = element_blank())
    } else {
      p1 <- dataset() %>% ggplot() + 
        geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", ymax="YLO", 
                             fill="class_order")) + 
        scale_fill_manual(values = c("Vögel" = '#1b9e77', "Schmetterlinge"='#d95f02',
                                     "Libellen"='#7570b3', "Heuschrecken"='#e7298a')) + 
        geom_sf(data=shape(), fill="transparent", col="black") +
        labs(x="Breitengrad", y="Längengrad") + 
        coord_sf(xlim = c(min(st_coordinates(shape())[,'X']),
                          max(st_coordinates(shape())[,'X'])),
                 ylim = c(min(st_coordinates(shape())[,'Y']),
                          max(st_coordinates(shape())[,'Y']))) + 
        theme_bw() + theme(legend.position="none", legend.background = element_blank())
    }
    if(input$spec == "Alle Arten"){
      p2 <- datatime() %>% ggplot() + 
        geom_bar(aes_string(x="jahr", y="`Species richness`",
                            fill="class_order"), stat="identity") + 
        scale_x_continuous(expand=expansion(add=c(0,0))) + 
        scale_y_continuous(expand=expansion(add=c(0,1))) + 
        scale_fill_manual(values = c("Vögel" = '#1b9e77', "Schmetterlinge"='#d95f02',
                                     "Libellen"='#7570b3', "Heuschrecken"='#e7298a')) + 
        labs(x="Jahr", fill="Taxon") + theme_bw() +
        theme(legend.position="bottom", legend.title=element_text(size=12, face="bold"), 
              legend.text = element_text(size=12), legend.key.size = unit(0.5, 'cm'),
              legend.background = element_blank())
    } else {
      p2 <- datatime() %>% ggplot() + 
        geom_bar(aes_string(x="jahr", y="`Number of occupied grid cells`",
                            fill="class_order"), stat="identity") + 
        scale_x_continuous(expand=expansion(add=c(0,0))) + 
        scale_y_continuous(expand=expansion(add=c(0,1))) + 
        scale_fill_manual(values = c("Vögel" = '#1b9e77', "Schmetterlinge"='#d95f02',
                                     "Libellen"='#7570b3', "Heuschrecken"='#e7298a')) + 
        labs(x="Jahr", y="Anzahl an besetzten Gridzellen", fill="Taxon") + theme_bw() + 
        theme(legend.position="bottom", legend.title=element_text(size=12, face="bold"), 
              legend.text = element_text(size=12), legend.key.size = unit(0.5, 'cm'),
              legend.background = element_blank())
    }
    if(input$spec == "Alle Arten"){
      sub_dat1 <- datadistrict() %>% tidyr::drop_na() %>% filter(var == "Species richness") %>% dplyr::select(-c(var))
      p3 <- sub_dat1 %>% ggplot() + geom_bar(aes(x=district, y=val, fill=class_order), stat="identity") + 
        scale_fill_manual(values = c("Vögel" = '#1b9e77', "Schmetterlinge"='#d95f02',
                                     "Libellen"='#7570b3', "Heuschrecken"='#e7298a')) + 
        labs(x="", y="Species richness") + 
        scale_y_continuous(expand=expansion(add=c(0,3))) + theme_bw() + 
        theme(legend.position = "none", axis.text.x = element_text(angle=45))
    } else{
      sub_dat2 <- datadistrict() %>% tidyr::drop_na() %>% filter(var == "Number of occupied grid cells") %>% 
        dplyr::select(-c(var))
      p3 <- sub_dat2 %>% ggplot() + geom_bar(aes(x=district, y=val, fill=class_order), stat="identity") + 
        scale_fill_manual(values = c("Vögel" = '#1b9e77', "Schmetterlinge"='#d95f02',
                                     "Libellen"='#7570b3', "Heuschrecken"='#e7298a')) + 
        labs(x="", y="Anzahl an besetzten Gridzellen") + 
        scale_y_continuous(expand=expansion(add=c(0,3))) + theme_bw() + 
        theme(legend.position = "none", axis.text.x = element_text(angle=45))
    }
    plotly::subplot(plotly::ggplotly(p1), plotly::ggplotly(p2), plotly::ggplotly(p3), nrows = 1) %>%
      layout(xaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'), 
             yaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'))
  })
  
  output$plot456 <- renderPlotly({
    p4 <- dataset() %>% ggplot() + 
      geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                           ymax="YLO", fill="`Number of records`")) + 
      scico::scale_fill_scico(name="Anzahl an\nBeobachtungen", palette="roma", na.value= "grey50", direction=-1) + 
      geom_sf(data=shape(), fill="transparent", col="black") +
      labs(x="Breitengrad", y="Längengrad") + 
      coord_sf(xlim = c(min(st_coordinates(shape())[,'X']),
                        max(st_coordinates(shape())[,'X'])),
               ylim = c(min(st_coordinates(shape())[,'Y']),
                        max(st_coordinates(shape())[,'Y']))) + 
      theme_bw() + theme(legend.key.height=unit(2, "in"),
                         legend.title=element_text(size=12, face="bold", vjust=1.2), 
                         legend.background = element_blank())
    p5 <- datatime() %>% ggplot() + 
      geom_bar(aes_string(x="jahr", y="`Number of records`",
                          fill="class_order"), stat="identity") + 
      scale_x_continuous(expand=expansion(add=c(0,0))) + 
      scale_y_continuous(expand=expansion(add=c(0,5))) + 
      scale_fill_manual(values = c("Vögel" = '#1b9e77', "Schmetterlinge"='#d95f02',
                                   "Libellen"='#7570b3', "Heuschrecken"='#e7298a')) + 
      labs(x="Jahr", y="Anzahl an Beobachtungen", fill="Taxon") + theme_bw() +
      theme(legend.position="bottom", legend.title=element_text(size=12, face="bold"), 
            legend.text = element_text(size=12), legend.key.size = unit(0.5, 'cm'),
            legend.background = element_blank())
    
    sub_dat3 <- datadistrict() %>% tidyr::drop_na() %>% filter(var == "Number of records") %>% dplyr::select(-c(var))
    p6 <- sub_dat3 %>% ggplot() + geom_bar(aes(x=district, y=val, fill=class_order), stat="identity") + 
      scale_fill_manual(values=c("Vögel" = '#1b9e77', "Schmetterlinge"='#d95f02',
                                 "Libellen"='#7570b3', "Heuschrecken"='#e7298a')) + 
      labs(x="", y="Anzahl an Beobachtungen") + 
      scale_y_continuous(expand=expansion(add=c(0,50))) + theme_bw() + 
      theme(legend.position = "none", axis.text.x = element_text(angle=45))
    plotly::subplot(plotly::ggplotly(p4), plotly::ggplotly(p5), plotly::ggplotly(p6), nrows = 1) %>%
      layout(xaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'), 
             yaxis = list(zerolinecolor = '#ffff', zerolinewidth = 2, gridcolor = 'ffff'))
  })
  
  output$plot7 <- renderPlotly({
    if(input$spec == "Alle Arten"){
      p <- ggplot(data=datagroup()) + 
        geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                             ymax="YLO", fill="`Species richness`")) + 
        facet_grid(.~class_order) + 
        scico::scale_fill_scico(name="Artenvielfalt", palette="roma", na.value= "grey50", direction=-1) + 
        geom_sf(data=shape(), fill="transparent", col="black") +
        labs(x="Breitengrad", y="Längengrad") + 
        coord_sf(xlim = c(min(st_coordinates(shape())[,'X']),
                          max(st_coordinates(shape())[,'X'])),
                 ylim = c(min(st_coordinates(shape())[,'Y']),
                          max(st_coordinates(shape())[,'Y']))) + 
        theme_bw() + theme(strip.background = element_blank(), legend.key.height=unit(0.5, "in"),
                           legend.title=element_text(size=12, face="bold"), 
                           strip.text=element_text(size=12, face="bold"))
      ggplotly(p)
    }
  })
  
  output$plot8 <- renderPlotly({
    if(input$spec == "Alle Arten"){
      p <- ggplot(data=datagroup()) + geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                                                           ymax="YLO", fill="`Number of records`")) + 
        facet_grid(.~class_order) + 
        scico::scale_fill_scico(name="Anzahl an\nBeobachtungen", palette="roma", 
                                na.value= "grey50", direction=-1) + 
        geom_sf(data=shape(), fill="transparent", col="black") +
        labs(x="Breitengrad", y="Längengrad") + 
        coord_sf(xlim = c(min(st_coordinates(shape())[,'X']),
                          max(st_coordinates(shape())[,'X'])),
                 ylim = c(min(st_coordinates(shape())[,'Y']),
                          max(st_coordinates(shape())[,'Y']))) + 
        theme_bw() + theme(strip.background = element_blank(), legend.key.height=unit(0.5, "in"),
                           legend.title=element_text(size=12, face="bold"), 
                           strip.text=element_text(size=12, face="bold"))
      ggplotly(p)
    } else{
      print("Bitte wähle 'Alle Arten' bei der Arten-Auswahl im linken Menü aus.")
    }
  })
  
  output$plot9 <- renderPlotly({
    if(input$spec == "Alle Arten"){
      p <- dataspacetime() %>% mutate(jahr2 = cut(jahr, breaks=seq(input$year_weight[1], 
                                                                   input$year_weight[2], by=as.numeric(input$interval)))) %>% 
        tidyr::drop_na() %>% ggplot() + geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                                                      ymax="YLO", fill="`Species richness`")) + 
        facet_grid(.~jahr2) + 
        scico::scale_fill_scico(name="Artenvielfalt", palette="roma", na.value= "grey50", direction=-1) + 
        geom_sf(data=shape(), fill="transparent", col="black") +
        labs(x="Breitengrad", y="Längengrad") + 
        coord_sf(xlim = c(min(st_coordinates(shape())[,'X']),
                          max(st_coordinates(shape())[,'X'])),
                 ylim = c(min(st_coordinates(shape())[,'Y']),
                          max(st_coordinates(shape())[,'Y']))) + 
        theme_bw() + theme(strip.background = element_blank(), legend.key.height=unit(0.5, "in"),
                           legend.title=element_text(size=12, face="bold"), 
                           strip.text=element_text(size=12, face="bold"))
      ggplotly(p)
    } else {
      p <- dataspacetime() %>% mutate(jahr2 = cut(jahr, breaks=seq(input$year_weight[1], input$year_weight[2], 
                                                                   by=as.numeric(input$interval)))) %>% 
        tidyr::drop_na() %>% ggplot() + geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", ymax="YLO", 
                                                             fill="class_order")) + 
        facet_grid(.~jahr2) + geom_sf(data=shape(), fill="transparent", col="black") +
        scale_fill_manual(values=c("Vögel" = '#1b9e77', "Schmetterlinge"='#d95f02',
                                   "Libellen"='#7570b3', "Heuschrecken"='#e7298a')) + 
        labs(x="Breitengrad", y="Längengrad") + 
        coord_sf(xlim = c(min(st_coordinates(shape())[,'X']),
                          max(st_coordinates(shape())[,'X'])),
                 ylim = c(min(st_coordinates(shape())[,'Y']),
                          max(st_coordinates(shape())[,'Y']))) + 
        theme_bw() + theme(legend.position="none", legend.background = element_blank(),
                           strip.background = element_blank(), strip.text=element_text(size=12, face="bold"))
      ggplotly(p)
    }
  })
  
  output$plot10 <- renderPlotly({
    p <- dataspacetime() %>% mutate(jahr2 = cut(jahr, breaks=seq(input$year_weight[1], input$year_weight[2], 
                                                                 by=as.numeric(input$interval)))) %>% tidyr::drop_na() %>% 
      ggplot() + geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", ymax="YLO", fill="`Number of records`")) + 
      facet_grid(.~jahr2) + 
      scico::scale_fill_scico(name="Anzahl an\nBeobachtungen", palette="roma", 
                              na.value= "grey50", direction=-1) + 
      geom_sf(data=shape(), fill="transparent", col="black") +
      labs(x="Breitengrad", y="Längengrad") + 
      coord_sf(xlim = c(min(st_coordinates(shape())[,'X']),
                        max(st_coordinates(shape())[,'X'])),
               ylim = c(min(st_coordinates(shape())[,'Y']),
                        max(st_coordinates(shape())[,'Y']))) + 
      theme_bw() + theme(strip.background = element_blank(), legend.key.height=unit(0.5, "in"),
                         legend.title=element_text(size=12, face="bold"), 
                         strip.text=element_text(size=12, face="bold"))
    ggplotly(p)
  })
  
  output$plot11 <- renderPlotly({
    p <- dataspeccomp() %>% ggplot() + 
      geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", ymax="YLO", 
                           fill="class_order")) + 
      facet_grid(.~art2) + geom_sf(data=shape(), fill="transparent", col="black") +
      scale_fill_manual(values=c("Vögel" = '#1b9e77', "Schmetterlinge"='#d95f02',
                                 "Libellen"='#7570b3', "Heuschrecken"='#e7298a')) + 
      labs(x="Breitengrad", y="Längengrad") + 
      coord_sf(xlim = c(min(st_coordinates(shape())[,'X']),
                        max(st_coordinates(shape())[,'X'])),
               ylim = c(min(st_coordinates(shape())[,'Y']),
                        max(st_coordinates(shape())[,'Y']))) + 
      theme_bw() + theme(legend.position="none", legend.background = element_blank(),
                         strip.background = element_blank(), strip.text=element_text(size=12, face="bold"))
    ggplotly(p)
  })
  
  output$plot12 <- renderPlotly({
    p <- dataspeccomp() %>% ggplot() + 
      geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                           ymax="YLO", fill="`Number of records`")) + facet_grid(.~art2) + 
      scico::scale_fill_scico(name="Anzahl an\nBeobachtungen", palette="roma", na.value= "grey50", direction=-1) + 
      geom_sf(data=shape(), fill="transparent", col="black") +
      labs(x="Breitengrad", y="Längengrad") + 
      coord_sf(xlim = c(min(st_coordinates(shape())[,'X']),
                        max(st_coordinates(shape())[,'X'])),
               ylim = c(min(st_coordinates(shape())[,'Y']),
                        max(st_coordinates(shape())[,'Y']))) + 
      theme_bw() + theme(legend.position="right", legend.key.height=unit(0.5, "in"),
                         legend.title=element_text(size=12, face="bold", vjust=1.2), 
                         legend.background = element_blank(), strip.background = element_blank(), 
                         strip.text=element_text(size=12, face="bold"))
    ggplotly(p)
  })
  
  output$table1 <- DT::renderDataTable(
    dataset() %>% ungroup() %>%
      dplyr::select(karte, `Species richness`, `Number of records`) %>%
      rename(TK25 = karte),
    options = list(
      lengthMenu = list(c(18, 50, 100, -1), c('18', '50', '100', 'All')),
      pageLength = 18
    ))
  
  output$table2 <- DT::renderDataTable(
    datatime() %>% dplyr::select(class_order, jahr, `Species richness`, 
                                 `Number of records`, `Number of occupied grid cells`) %>%
      rename(Taxon = class_order, Year = jahr),
    options = list(
      lengthMenu = list(c(18, 50, 100, -1), c('18', '50', '100', 'All')),
      pageLength = 18
    ))
}

# Run app ----

shinyApp(ui, server)
