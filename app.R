rm(list=ls()); invisible(gc())

# Load packages ----
library(data.table)
library(ggplot2)
library(patchwork)
library(shinyWidgets)
#library(sf)
library(dplyr) # Needed for sf filtering & piping!!!
#library(ggspatial)
#library(scico)

load("data/districts.rda")
districts <- sf::st_transform(districts, 31468)
load("data/landkreise.rda")
landkreise <- sf::st_transform(landkreise, 31468)

art_data <- readRDS("inst/extdata/art_data.rds")

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

taxa <- c("Aves", "Lepidoptera", "Odonata", "Orthoptera")

CSS <- "
/* CSS for the labels */
/* CSS for the checkboxes */
.pretty input[value=Aves]~.state label:before {
  background-color: #1b9e77;
}
.pretty input[value=Aves]~.state label:after {
}
.pretty input[value=Lepidoptera]~.state label:after, 
.pretty input[value=Lepidoptera]~.state label:before {
  background-color: #d95f02;
}
.pretty input[value=Odonata]~.state label:after, 
.pretty input[value=Odonata]~.state label:before {
  background-color: #7570b3;
}
.pretty input[value=Orthoptera]~.state label:after {
  background-color: #e7298a;
},
.pretty input[value=Orthoptera]~.state label:before, 
"

# User interface ----
ui <- fluidPage(
  tags$head(tags$style(HTML(CSS))),
  ## This is the overall title
  titlePanel("Visualisation tool for biodiversity data"),
  
  ## Menu on the side
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year_weight", label = "Time period:", value = c(1980, 2019),
                  min = 1980, max = 2019, step = 1, ticks=F, sep=""),
      sliderInput(inputId = "month_weight", label = "Months:", value = c(1, 12),
                  min = 1, max = 12, step = 1, ticks=F, sep=""),
      shinyWidgets::prettyCheckboxGroup(inputId = "class_order",
                                        label = "Choose one or multiple taxa to display:",
                                        choiceNames = taxa, choiceValues = taxa, 
                                        selected = taxa, icon = icon("check"), fill=T, inline=T),
      uiOutput("family_choice"),
      uiOutput("cat_choice"),
      helpText("Species are filtered according to time period, month and taxon."),
      shinyWidgets::pickerInput(inputId = "district", 
                                label = "Choose one or multiple districts to display:", 
                                choices = sort(unique(districts$BEZ_RBZ)),
                                selected = sort(unique(districts$BEZ_RBZ)),
                                options = list(
                                  `actions-box` = TRUE, 
                                  size = 10,
                                  `selected-text-format` = "count > 3",
                                  `count-selected-text` = "{0} districts chosen (of a total of {1})"
                                ), multiple = TRUE),
      helpText("Note: This tool only displays the collected raw data. 
             To apply a spatial or temporal filtering change the appropriate minimum number of values below."),
      numericInput(inputId = "sp_weight", label = "Minimum number of records per grid cell:", value = 1, min = 1, max = 50), 
      numericInput(inputId = "temp_weight", label = "Minimum number of records per year:", value = 1, min = 1, max = 1000), 
      shinyWidgets::prettyRadioButtons(inputId = "res",
                                       label = "Specify spatial resolution:",
                                       choices = c("TK25", "TK"),
                                       selected = "TK25", thick=T, animation = "pulse",
                                       status = "info", inline = T),
      helpText("TK25 corresponds to a resolution of ca. 6 x 6 km, while TK corresponds to a resolution of ca. 12 x 12 km. 
               Maps are shown in 3-degree Gauss-Kruger zone 4 projection (EPSG:31468)."),
      width = 3),
    # Set the different tabs in the main panel
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Species overview",
                           h3("Species overview"),
                           h4(textOutput("subtitle1")),
                           plotOutput("plot123", height = 375, width = 1200),  
                           h4("Number of observations"),
                           plotOutput("plot456", height = 375, width = 1200)
                  ),
                  tabPanel("Taxon comparison",
                           h3("Taxon comparison"),
                           h4("Species richness"),
                           plotOutput("plot7", height = 375, width = 1250),
                           h4("Number of observations"),
                           plotOutput("plot8", height = 375, width = 1290)
                  ),
                  tabPanel("Temporal comparison",
                           h3("Temporal comparison"),
                           selectInput("interval", "Specify a time interval:",
                                       c("7 years" = 7,
                                         "10 years" = 10,
                                         "15 years" = 15)),
                           h4(textOutput("subtitle2")),
                           plotOutput("plot9", height = 325, width = 1295),
                           h4("Number of observations"),
                           plotOutput("plot10", height = 325, width = 1330)
                  ),
                  tabPanel("Species comparison",
                           h3("Species comparison"),
                           fluidRow(
                             column(1),
                             column(3, uiOutput("cat_choice2")),
                             column(3, uiOutput("cat_choice3")),
                             column(5)
                           ),
                           h4("Species presence"),
                           plotOutput("plot11", height = 325, width = 640),
                           h4("Number of observations"),
                           plotOutput("plot12", height = 325, width = 730)
                  ),
                  tabPanel("Spatial summary",
                           h3("Spatial summary"),
                           DT::dataTableOutput("table1")
                  ),
                  tabPanel("Temporal summary",
                           h3("Temporal summary"),
                           DT::dataTableOutput("table2")
                  )
      ), width=7
    )
  )
)

# Server logic ----
server <- function(input, output) {
  datclassorder <- reactive({art_data[class_order %in% input$class_order,]}) %>% bindCache(input$class_order)
  
  output$family_choice <- renderUI({
    selectInput(inputId="family",
                label="Choose a family to display:", 
                choices = c("All Families", sort(unique(unlist(datclassorder()[,family])))),
                selected="All Families")
  })
  
  datclass <- reactive({
    if(input$family != "All Families"){
      datclassorder()[family == input$family,]
    } else{
      datclassorder()
    }
  })
  
  datclassdist <- reactive({
    datclass()[jahr >= input$year_weight[1] & jahr <= input$year_weight[2] & 
                 mon >= input$month_weight[1] & mon <= input$month_weight[2],
    ][district %in% input$district,]
  })
  
  output$cat_choice <- renderUI({
    selectInput(inputId="spec",
                label="Choose a species to display:", 
                choices = c("All Species", sort(unique(unlist(datclass()[,art2])))),
                selected="All Species")
  })
  
  output$subtitle1 <- renderText({
    if(input$spec == "All Species"){"Species richness"} else{"Species presence"}
  })
  
  output$subtitle2 <- renderText({
    if(input$spec == "All Species"){"Species richness"} else{"Species presence"}
  })
  
  output$cat_choice2 <- renderUI({
    selectInput(inputId="spec2",
                label="Choose first species to display:", 
                choices = sort(unique(unlist(datclass()[,art2]))))
  })
  output$cat_choice3 <- renderUI({
    selectInput(inputId="spec3",
                label="Choose second species to display:", 
                choices = sort(unique(unlist(datclass()[,art2]))[unique(unlist(datclass()[,art2])) != input$spec2]))
  })
  
  dataspec <- reactive({
    dat <- datclassdist()
    if(input$spec != "All Species"){
      dat <- dat[art2 == input$spec,]
    }
    dat
  })
  
  dataset <- reactive({
    if(input$spec != "All Species"){
      if(input$res == "TK25"){
        dat <- dataspec()[,list(`Species richness`=length(unique(art2)), 
                                `Number of records`=.N) , by = .(XLU, XRU, YLU, YLO, karte, class_order)
        ][`Number of records` >= input$sp_weight,]
      } else {
        dat <- dataspec()[,list(`Species richness`=length(unique(art2)), 
                                `Number of records`=.N) , by = .(XLU_rough, XRU_rough, YLU_rough, YLO_rough, karte, class_order)
        ][`Number of records` >= input$sp_weight,]
        setnames(dat, old = c("XLU_rough", "XRU_rough", "YLU_rough", "YLO_rough"), 
                 new = c("XLU", "XRU", "YLU", "YLO"))
        dat
      }
    } else{
      if(input$res == "TK25"){
        dat <- dataspec()[,list(`Species richness`=length(unique(art2)), 
                                `Number of records`=.N) , by = .(XLU, XRU, YLU, YLO, karte)
        ][`Number of records` >= input$sp_weight,]
      } else {
        dat <- dataspec()[,list(`Species richness`=length(unique(art2)), 
                                `Number of records`=.N) , by = .(XLU_rough, XRU_rough, YLU_rough, YLO_rough, karte)
        ][`Number of records` >= input$sp_weight,]
        setnames(dat, old = c("XLU_rough", "XRU_rough", "YLU_rough", "YLO_rough"), 
                 new = c("XLU", "XRU", "YLU", "YLO"))
        dat
      }
    }
  })
  
  dataspeccomp <- reactive({
    if(input$res == "TK25"){
      datclassdist()[art2 %in% c(input$spec2, input$spec3),
      ][,list(`Species richness`=length(unique(art2)), 
              `Number of records`=.N), by = .(XLU, XRU, YLU, YLO, art2, class_order)
      ][`Number of records` >= input$sp_weight,]
    } else {
      dat <- datclassdist()[,list(`Species richness`=length(unique(art2)), 
                                  `Number of records`=.N), by = .(XLU_rough, XRU_rough, YLU_rough, YLO_rough, art2, class_order)
      ][`Number of records` >= input$sp_weight,]
      setnames(dat, old = c("XLU_rough", "XRU_rough", "YLU_rough", "YLO_rough"), 
               new = c("XLU", "XRU", "YLU", "YLO"))
      dat
    }
  })
  
  datatime <- reactive({
    if(input$res == "TK25"){
      dataspec()[,list(`Species richness`=length(unique(art2)), 
                       `Number of records`=.N,
                       `Number of occupied grid cells`= length(unique(XLU, XRU, YLU, YLO))), by = .(jahr, class_order)
      ][`Number of records` >= input$temp_weight,]
    } else {
      dataspec()[,list(`Species richness`=length(unique(art2)), 
                       `Number of records`=.N,
                       `Number of occupied grid cells`= length(unique(XLU_rough, XRU_rough, YLU_rough, YLO_rough))), by = .(jahr, class_order)
      ][`Number of records` >= input$temp_weight,]
    }
  })
  
  dataspacetime <- reactive({
    if(input$res == "TK25"){
      dataspec()[,list(`Species richness`=length(unique(art2)), 
                       `Number of records`=.N), 
                 by = .(jahr, XLU, XRU, YLU, YLO, karte, class_order)
      ][`Number of records` >= input$sp_weight,]
    } else {
      dat <- dataspec()[,list(`Species richness`=length(unique(art2)), 
                              `Number of records`=.N), 
                        by = .(jahr, XLU_rough, XRU_rough, YLU_rough, YLO_rough, karte, class_order)
      ][`Number of records` >= input$sp_weight,]
      setnames(dat, old = c("XLU_rough", "XRU_rough", "YLU_rough", "YLO_rough"), 
               new = c("XLU", "XRU", "YLU", "YLO"))
      dat
    }
  })
  
  datagroup <- reactive({
    if(input$res == "TK25"){
      datclassdist()[,list(`Species richness`=length(unique(art2)), 
                           `Number of records`=.N), 
                     by = .(XLU, XRU, YLU, YLO, class_order)
      ][`Number of records` >= input$sp_weight,]
    } else {
      dat <- datclass_dist()[,list(`Species richness`=length(unique(art2)), 
                                   `Number of records`=.N), 
                             by = .(jahr, XLU_rough, XRU_rough, YLU_rough, YLO_rough, karte, class_order)
      ][`Number of records` >= input$sp_weight,]
      setnames(dat, old = c("XLU_rough", "XRU_rough", "YLU_rough", "YLO_rough"), 
               new = c("XLU", "XRU", "YLU", "YLO"))
      dat
    }
  })
  
  datadistrict <- reactive({
    dat <- dataspec()[,list(`Species richness`=length(unique(art2)), 
                            `Number of records`=.N,
                            `Number of occupied grid cells`= n_distinct(XLU_rough, XRU_rough, YLU_rough, YLO_rough)), 
                      by = .(district, class_order)]
    dat <- melt(dat, variable.name="var", value.name="val", id.vars=c("district", "class_order"))
    na.omit(dat)
  })
  
  shape <- reactive({
    if (length(input$district) > 1){
      districts %>% filter(BEZ_RBZ %in% input$district)
    } else{
      landkreise %>% filter(BEZ_RBZ == input$district)
    }
  })
  
  output$plot123 <- renderPlot({
    if(input$spec == "All Species"){
      p1 <- dataset() %>% ggplot() + 
        geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                             ymax="YLO", fill="`Species richness`")) + 
        scico::scale_fill_scico(name="SR", palette="roma", na.value= "grey50", direction=-1) + 
        geom_sf(data=shape(), fill="transparent", col="black") +
        labs(x="Longitude", y="Latitude") + 
        coord_sf(xlim = c(min(sf::st_coordinates(shape())[,'X']),
                          max(sf::st_coordinates(shape())[,'X'])),
                 ylim = c(min(sf::st_coordinates(shape())[,'Y']),
                          max(sf::st_coordinates(shape())[,'Y']))) + 
        ggspatial::annotation_scale(location="br", width_hint = 0.2) +
        ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                          height = unit(1, "cm"), width = unit(1, "cm"),
                                          style = ggspatial::north_arrow_fancy_orienteering) +
        theme_bw() + theme(legend.position="bottom", legend.key.width=unit(0.5, "in"), 
                           legend.title=element_text(size=12, face="bold", vjust=0.85), 
                           legend.background = element_blank())
    } else {
      p1 <- dataset() %>% ggplot() + 
        geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", ymax="YLO", fill="class_order")) + 
        scale_fill_manual(values = c("Aves" = '#1b9e77', "Lepidoptera"='#d95f02',
                                     "Odonata"='#7570b3', "Orthoptera"='#e7298a')) + 
        geom_sf(data=shape(), fill="transparent", col="black") +
        labs(x="Longitude", y="Latitude") + 
        coord_sf(xlim = c(min(sf::st_coordinates(shape())[,'X']),
                          max(sf::st_coordinates(shape())[,'X'])),
                 ylim = c(min(sf::st_coordinates(shape())[,'Y']),
                          max(sf::st_coordinates(shape())[,'Y']))) + 
        ggspatial::annotation_scale(location="br", width_hint = 0.2) +
        ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                          height = unit(1, "cm"), width = unit(1, "cm"),
                                          style = ggspatial::north_arrow_fancy_orienteering) +
        theme_bw() + theme(legend.position="none", legend.background = element_blank())
    }
    
    if(input$spec == "All Species"){
      p2 <- datatime() %>% ggplot() + 
        geom_bar(aes_string(x="jahr", y="`Species richness`", fill="class_order"), stat="identity") + 
        scale_x_continuous(expand=expansion(add=c(0,0))) + 
        scale_y_continuous(expand=expansion(add=c(0,1))) + 
        scale_fill_manual(values = c("Aves" = '#1b9e77', "Lepidoptera"='#d95f02',
                                     "Odonata"='#7570b3', "Orthoptera"='#e7298a')) + 
        labs(x="Year", fill="Taxon") + theme_bw() +
        theme(legend.position="bottom", legend.title=element_text(size=12, face="bold"), 
              legend.text = element_text(size=12), legend.key.size = unit(0.5, 'cm'),
              legend.background = element_blank())
    } else {
      p2 <- datatime() %>% ggplot() + 
        geom_bar(aes_string(x="jahr", y="`Number of occupied grid cells`",
                            fill="class_order"), stat="identity") + 
        scale_x_continuous(expand=expansion(add=c(0,0))) + 
        scale_y_continuous(expand=expansion(add=c(0,1))) + 
        scale_fill_manual(values = c("Aves" = '#1b9e77', "Lepidoptera"='#d95f02',
                                     "Odonata"='#7570b3', "Orthoptera"='#e7298a')) + 
        labs(x="Year", y="Number of occupied grid cells", fill="Taxon") + theme_bw() + 
        theme(legend.position="bottom", legend.title=element_text(size=12, face="bold"), 
              legend.text = element_text(size=12), legend.key.size = unit(0.5, 'cm'),
              legend.background = element_blank())
    }
    
    if(input$spec == "All Species"){
      sub_dat1 <- datadistrict() %>% na.omit() %>% .[var == "Species richness",] %>% .[,c("district", "class_order", "val"), with=F] %>%
        .[order(district)]
      if(length(unique(sub_dat1$district)) >= 3){
        df <- data.frame(x=rep(0, times=length(unique(sub_dat1$district))), 
                         y=round(seq(0,max(sub_dat1$val), length.out=length(unique(sub_dat1$district)))/10,0)*10,
                         label=round(seq(0,max(sub_dat1$val), length.out=length(unique(sub_dat1$district)))/10,0)*10)
        p3 <- ggplot() +
          geom_point(data=sub_dat1, aes(x = district, y = val, colour=class_order, group=class_order)) + 
          geom_polygon(data=sub_dat1, aes(x = district, y = val, colour=class_order, group=class_order), alpha=0.0001) +
          coord_straightpolar(theta = 'x') + 
          geom_text(data=df, aes(x=x, y=y, label=label), size=9/.pt, colour="black") +
          scale_colour_manual(values=c("Aves" = '#1b9e77', "Lepidoptera"='#d95f02',
                                       "Odonata"='#7570b3', "Orthoptera"='#e7298a')) + 
          scale_y_continuous(expand=c(0,0), limits=c(0,NA), breaks=df$label) +
          theme_minimal() + theme(axis.text.y = element_blank(), 
                                  axis.ticks.y = element_blank(), legend.position="none",
                                  plot.background = element_rect(fill=NA, colour=NA),
                                  panel.ontop=T, axis.title = element_blank(),
                                  axis.text = element_text(colour="black", size=7),
                                  panel.grid.major.x = element_line(colour="gray60", linewidth=1/.pt,
                                                                    linetype="dashed"),
                                  panel.grid.major.y = element_line(colour= c(rep("gray60", nrow(df)), NA), linewidth=1/.pt,
                                                                    linetype="dashed"))
      } else{
        p3 <- sub_dat1 %>% ggplot() + geom_bar(aes(x=district, y=val, fill=class_order), stat="identity") + 
          scale_fill_manual(values = c("Aves" = '#1b9e77', "Lepidoptera"='#d95f02',
                                       "Odonata"='#7570b3', "Orthoptera"='#e7298a')) + 
          labs(x="", y="Species richness") + 
          scale_y_continuous(expand=expansion(add=c(0,3))) + theme_bw() + 
          theme(legend.position = "none")
      }
    } else{
      sub_dat2 <- datadistrict() %>% na.omit() %>% .[var == "Number of occupied grid cells",] %>% 
        .[,c("district", "class_order", "val"), with=F] %>% .[order(district)]
      if(length(unique(sub_dat2$district)) >= 3){
        df <- data.frame(x=rep(0, times=length(unique(sub_dat2$district))), 
                         y=round(seq(0,max(sub_dat2$val), length.out=length(unique(sub_dat2$district)))/10,0)*10,
                         label=round(seq(0,max(sub_dat2$val), length.out=length(unique(sub_dat2$district)))/10,0)*10)
        p3 <- ggplot() +
          geom_point(data=sub_dat2, aes(x = district, y = val, colour=class_order, group=class_order)) + 
          geom_polygon(data=sub_dat2, aes(x = district, y = val, colour=class_order, group=class_order), alpha=0.0001) +
          coord_straightpolar(theta = 'x') + 
          geom_text(data=df, aes(x=x, y=y, label=label), size=9/.pt, colour="black") +
          scale_colour_manual(values=c("Aves" = '#1b9e77', "Lepidoptera"='#d95f02',
                                       "Odonata"='#7570b3', "Orthoptera"='#e7298a')) + 
          scale_y_continuous(expand=c(0,0), limits=c(0,NA), breaks=df$label) +
          theme_minimal() + theme(axis.text.y = element_blank(), 
                                  axis.ticks.y = element_blank(), legend.position="none",
                                  plot.background = element_rect(fill=NA, colour=NA),
                                  panel.ontop=T, axis.title = element_blank(),
                                  axis.text = element_text(colour="black", size=7),
                                  panel.grid.major.x = element_line(colour="gray60", linewidth=1/.pt,
                                                                    linetype="dashed"),
                                  panel.grid.major.y = element_line(colour= c(rep("gray60", nrow(df)), NA), linewidth=1/.pt,
                                                                    linetype="dashed"))
      } else{
        p3 <- sub_dat2 %>% ggplot() + geom_bar(aes(x=district, y=val, fill=class_order), stat="identity") + 
          scale_fill_manual(values = c("Aves" = '#1b9e77', "Lepidoptera"='#d95f02',
                                       "Odonata"='#7570b3', "Orthoptera"='#e7298a')) + 
          labs(x="", y="Number of occupied grid cells") + 
          scale_y_continuous(expand=expansion(add=c(0,3))) + theme_bw() + 
          theme(legend.position = "none")
      }
    }
    p1 + p2 + p3
  })
  
  output$plot456 <- renderPlot({
    p4 <- dataset() %>% ggplot() + 
      geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                           ymax="YLO", fill="`Number of records`")) + 
      scico::scale_fill_scico(name="Number\nof records", palette="roma", na.value= "grey50", direction=-1) + 
      geom_sf(data=shape(), fill="transparent", col="black") +
      labs(x="Longitude", y="Latitude") + 
      coord_sf(xlim = c(min(sf::st_coordinates(shape())[,'X']),
                        max(sf::st_coordinates(shape())[,'X'])),
               ylim = c(min(sf::st_coordinates(shape())[,'Y']),
                        max(sf::st_coordinates(shape())[,'Y']))) + 
      ggspatial::annotation_scale(location="br", width_hint = 0.2) +
      ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                        height = unit(1, "cm"), width = unit(1, "cm"),
                                        style = ggspatial::north_arrow_fancy_orienteering) +
      theme_bw() + theme(legend.position="bottom", legend.key.width=unit(0.5, "in"),
                         legend.title=element_text(size=12, face="bold", vjust=1.2), 
                         legend.background = element_blank())
    p5 <- datatime() %>% ggplot() + 
      geom_bar(aes_string(x="jahr", y="`Number of records`",
                          fill="class_order"), stat="identity") + 
      scale_x_continuous(expand=expansion(add=c(0,0))) + 
      scale_y_continuous(expand=expansion(add=c(0,5))) + 
      scale_fill_manual(values = c("Aves" = '#1b9e77',
                                   "Lepidoptera"='#d95f02',
                                   "Odonata"='#7570b3',
                                   "Orthoptera"='#e7298a')) + 
      labs(x="Year", y="Number of records", fill="Taxon") + theme_bw() +
      theme(legend.position="bottom", legend.title=element_text(size=12, face="bold"), 
            legend.text = element_text(size=12), legend.key.size = unit(0.5, 'cm'),
            legend.background = element_blank())
    
    sub_dat3 <- datadistrict() %>% na.omit() %>% .[var == "Number of records",] %>% 
      .[,c("district", "class_order", "val"), with=F] %>% .[order(district)]
    if(length(input$district) >= 3){
      df <- data.frame(x=rep(0, times=length(unique(sub_dat3$district))), 
                       y=round(seq(0,max(sub_dat3$val), length.out=length(unique(sub_dat3$district)))/10,0)*10,
                       label=round(seq(0,max(sub_dat3$val), length.out=length(unique(sub_dat3$district)))/10,0)*10)
      p6 <- ggplot() +
        geom_point(data=sub_dat3, aes(x = district, y = val, colour=class_order, group=class_order)) + 
        geom_polygon(data=sub_dat3, aes(x = district, y = val, colour=class_order, group=class_order), alpha=0.0001) +
        coord_straightpolar(theta = 'x') + 
        geom_text(data=df, aes(x=x, y=y, label=label), size=9/.pt, colour="black") +
        scale_colour_manual(values=c("Aves" = '#1b9e77', "Lepidoptera"='#d95f02',
                                     "Odonata"='#7570b3', "Orthoptera"='#e7298a')) + 
        scale_y_continuous(expand=c(0,0), limits=c(0,NA), breaks=df$label) +
        theme_minimal() + theme(axis.text.y = element_blank(), 
                                axis.ticks.y = element_blank(), legend.position="none",
                                plot.background = element_rect(fill=NA, colour=NA),
                                panel.ontop=T, axis.title = element_blank(),
                                axis.text = element_text(colour="black", size=7),
                                panel.grid.major.x = element_line(colour="gray60", linewidth=1/.pt,
                                                                  linetype="dashed"),
                                panel.grid.major.y = element_line(colour= c(rep("gray60", nrow(df)), NA), linewidth=1/.pt,
                                                                  linetype="dashed"))
    } else{
      p6 <- sub_dat3 %>% ggplot() + geom_bar(aes(x=district, y=val, fill=class_order), stat="identity") + 
        scale_fill_manual(values = c("Aves" = '#1b9e77', "Lepidoptera"='#d95f02',
                                     "Odonata"='#7570b3', "Orthoptera"='#e7298a')) + 
        labs(x="", y="Number of records") + 
        scale_y_continuous(expand=expansion(add=c(0,50))) + theme_bw() + 
        theme(legend.position = "none")
    }
    p4 + p5 + p6
  })
  
  output$plot7 <- renderPlot({
    if(input$spec == "All Species"){
      ggplot(data=datagroup()) + 
        geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                             ymax="YLO", fill="`Species richness`")) + 
        facet_grid(.~class_order) + 
        scico::scale_fill_scico(name="SR", palette="roma", na.value= "grey50", direction=-1) + 
        geom_sf(data=shape(), fill="transparent", col="black") +
        labs(x="Longitude", y="Latitude") + 
        coord_sf(xlim = c(min(sf::st_coordinates(shape())[,'X']),
                          max(sf::st_coordinates(shape())[,'X'])),
                 ylim = c(min(sf::st_coordinates(shape())[,'Y']),
                          max(sf::st_coordinates(shape())[,'Y']))) + 
        ggspatial::annotation_scale(location="br", width_hint = 0.15) +
        ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                          height = unit(1, "cm"), width = unit(1, "cm"),
                                          style = ggspatial::north_arrow_fancy_orienteering) +
        theme_bw() + theme(strip.background = element_blank(), legend.key.height=unit(0.5, "in"),
                           legend.title=element_text(size=12, face="bold"), 
                           strip.text=element_text(size=12, face="bold"))
    }
  })
  
  output$plot8 <- renderPlot({
    if(input$spec == "All Species"){
      ggplot(data=datagroup()) + geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                                                      ymax="YLO", fill="`Number of records`")) + 
        facet_grid(.~class_order) + 
        scico::scale_fill_scico(name="Number\nof records", palette="roma", na.value= "grey50", direction=-1) + 
        geom_sf(data=shape(), fill="transparent", col="black") +
        labs(x="Longitude", y="Latitude") + 
        coord_sf(xlim = c(min(sf::st_coordinates(shape())[,'X']),
                          max(sf::st_coordinates(shape())[,'X'])),
                 ylim = c(min(sf::st_coordinates(shape())[,'Y']),
                          max(sf::st_coordinates(shape())[,'Y']))) + 
        ggspatial::annotation_scale(location="br", width_hint = 0.2) +
        ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                          height = unit(1, "cm"), width = unit(1, "cm"),
                                          style = ggspatial::north_arrow_fancy_orienteering) +
        theme_bw() + theme(strip.background = element_blank(), legend.key.height=unit(0.5, "in"),
                           legend.title=element_text(size=12, face="bold"), 
                           strip.text=element_text(size=12, face="bold"))
    } else{
      print("Please select 'All Species' at the species input selection on the left hand side.")
    }
  })
  
  output$plot9 <- renderPlot({
    if(input$spec == "All Species"){
      dataspacetime()[, jahr2 := cut(jahr, breaks=seq(input$year_weight[1], input$year_weight[2], by=as.numeric(input$interval)))] %>% 
        na.omit() %>% 
        ggplot() + geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", ymax="YLO", fill="`Species richness`")) + 
        facet_grid(.~jahr2) + 
        scico::scale_fill_scico(name="SR", palette="roma", na.value= "grey50", direction=-1) + 
        geom_sf(data=shape(), fill="transparent", col="black") +
        labs(x="Longitude", y="Latitude") + 
        coord_sf(xlim = c(min(sf::st_coordinates(shape())[,'X']),
                          max(sf::st_coordinates(shape())[,'X'])),
                 ylim = c(min(sf::st_coordinates(shape())[,'Y']),
                          max(sf::st_coordinates(shape())[,'Y']))) + 
        ggspatial::annotation_scale(location="br", width_hint = 0.2) +
        ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                          height = unit(1, "cm"), width = unit(1, "cm"),
                                          style = ggspatial::north_arrow_fancy_orienteering) +
        theme_bw() + theme(strip.background = element_blank(), legend.key.height=unit(0.5, "in"),
                           legend.title=element_text(size=12, face="bold"), 
                           strip.text=element_text(size=12, face="bold"))
    } else {
      dataspacetime()[, jahr2 := cut(jahr, breaks=seq(input$year_weight[1], input$year_weight[2], 
                                                      by=as.numeric(input$interval)))] %>% 
        na.omit() %>% 
        ggplot() + geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", ymax="YLO", fill="class_order")) + 
        facet_grid(.~jahr2) + 
        scale_fill_manual(values=c("Aves" = '#1b9e77', "Lepidoptera"='#d95f02',
                                   "Odonata"='#7570b3', "Orthoptera"='#e7298a')) + 
        geom_sf(data=shape(), fill="transparent", col="black") +
        labs(x="Longitude", y="Latitude") + 
        coord_sf(xlim = c(min(sf::st_coordinates(shape())[,'X']),
                          max(sf::st_coordinates(shape())[,'X'])),
                 ylim = c(min(sf::st_coordinates(shape())[,'Y']),
                          max(sf::st_coordinates(shape())[,'Y']))) + 
        ggspatial::annotation_scale(location="br", width_hint = 0.2) +
        ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                          height = unit(1, "cm"), width = unit(1, "cm"),
                                          style = ggspatial::north_arrow_fancy_orienteering) +
        theme_bw() + theme(legend.position="none", legend.background = element_blank(),
                           strip.background = element_blank(), strip.text=element_text(size=12, face="bold"))
    }
  })
  
  output$plot10 <- renderPlot({
    dataspacetime()[, jahr2 := cut(jahr, breaks=seq(input$year_weight[1], input$year_weight[2], by=as.numeric(input$interval)))] %>% 
      na.omit() %>% 
      ggplot() + geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", ymax="YLO", fill="`Number of records`")) + 
      facet_grid(.~jahr2) + geom_sf(data=shape(), fill="transparent", col="black") +
      scico::scale_fill_scico(name="Number\nof records", palette="roma", na.value= "grey50", direction=-1) + 
      labs(x="Longitude", y="Latitude") + 
      coord_sf(xlim = c(min(sf::st_coordinates(shape())[,'X']),
                        max(sf::st_coordinates(shape())[,'X'])),
               ylim = c(min(sf::st_coordinates(shape())[,'Y']),
                        max(sf::st_coordinates(shape())[,'Y']))) + 
      ggspatial::annotation_scale(location="br", width_hint = 0.2) +
      ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                        height = unit(1, "cm"), width = unit(1, "cm"),
                                        style = ggspatial::north_arrow_fancy_orienteering) +
      theme_bw() + theme(strip.background = element_blank(), legend.key.height=unit(0.5, "in"),
                         legend.title=element_text(size=12, face="bold"), 
                         strip.text=element_text(size=12, face="bold"))
  })
  
  output$plot11 <- renderPlot({
    dataspeccomp() %>% ggplot() + 
      geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", ymax="YLO", fill="class_order")) +  facet_grid(.~art2) + 
      scale_fill_manual(values=c("Aves" = '#1b9e77', "Lepidoptera"='#d95f02',
                                 "Odonata"='#7570b3', "Orthoptera"='#e7298a')) + 
      geom_sf(data=shape(), fill="transparent", col="black") +
      labs(x="Longitude", y="Latitude") + 
      coord_sf(xlim = c(min(sf::st_coordinates(shape())[,'X']),
                        max(sf::st_coordinates(shape())[,'X'])),
               ylim = c(min(sf::st_coordinates(shape())[,'Y']),
                        max(sf::st_coordinates(shape())[,'Y']))) + 
      ggspatial::annotation_scale(location="br", width_hint = 0.2) +
      ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                        height = unit(1, "cm"), width = unit(1, "cm"),
                                        style = ggspatial::north_arrow_fancy_orienteering) +
      theme_bw() + theme(legend.position="none", legend.background = element_blank(),
                         strip.background = element_blank(), strip.text=element_text(size=12, face="bold"))
  })
  
  output$plot12 <- renderPlot({
    dataspeccomp() %>% ggplot() + 
      geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                           ymax="YLO", fill="`Number of records`")) + facet_grid(.~art2) + 
      scico::scale_fill_scico(name="Number\nof records", palette="roma", na.value= "grey50", direction=-1) + 
      geom_sf(data=shape(), fill="transparent", col="black") +
      labs(x="Longitude", y="Latitude") + 
      coord_sf(xlim = c(min(sf::st_coordinates(shape())[,'X']),
                        max(sf::st_coordinates(shape())[,'X'])),
               ylim = c(min(sf::st_coordinates(shape())[,'Y']),
                        max(sf::st_coordinates(shape())[,'Y']))) + 
      ggspatial::annotation_scale(location="br", width_hint = 0.2) +
      ggspatial::annotation_north_arrow(location = "tl", which_north = "true", 
                                        height = unit(1, "cm"), width = unit(1, "cm"),
                                        style = ggspatial::north_arrow_fancy_orienteering) +
      theme_bw() + theme(legend.position="right", legend.key.height=unit(0.5, "in"),
                         legend.title=element_text(size=12, face="bold", vjust=1.2), 
                         legend.background = element_blank(), strip.background = element_blank(), 
                         strip.text=element_text(size=12, face="bold"))
  })
  
  output$table1 <- DT::renderDataTable(dataset()[, c(karte, `Species richness`, `Number of records`)],
                                       #setnames(dat1, karte, TK25),
                                       options = list(
                                         lengthMenu = list(c(18, 50, 100, -1), c('18', '50', '100', 'All')),
                                         pageLength = 18
                                       ))
  
  output$table2 <- DT::renderDataTable(
    datatime()[,c(class_order, jahr, `Species richness`, 
                  `Number of records`, `Number of occupied grid cells`)],
    #setnames(dat2, c(class_order, jahr), c(Taxon, Year))
    options = list(
      lengthMenu = list(c(18, 50, 100, -1), c('18', '50', '100', 'All')),
      pageLength = 18
    ))
}

# Run app ----

shinyApp(ui, server)
