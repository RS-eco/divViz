rm(list=ls()); invisible(gc())

# Load packages ----
library(shiny)
library(shinyWidgets)
library(DT) # datatable()
library(ggplot2)
library(data.table)
library(dplyr)
library(dbplyr)
library(sf)
library(tidyr)
library(DBI)
library(RSQLite)
library(patchwork)
#devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(ggradar)

# See dbplot
# https://edgararuiz.github.io/dbplot/#raster
# https://db.rstudio.com/best-practices/visualization/

# Load data ----

load("data/bavaria.rda")
bavaria <- sf::st_transform(bavaria, 31468)
load("data/districts.rda")
districts <- sf::st_transform(districts, 31468)

# Load taxonomy for adding class/order values
#temp <- tempfile(fileext = ".rda")
#download.file("https://github.com/RS-eco/bdc/raw/main/data/taxonomyStd.rda", destfile=temp)
#taxonomyStd <- load(temp)

# Load ASK database
my_db <- DBI::dbConnect(RSQLite::SQLite(), dbname = "inst/extdata/database.db")
DBI::dbListTables(my_db)

# Pull part of database including data on species 
art_data <- dplyr::tbl(my_db, paste("art")) %>%  
  dplyr::select(-c(gkk_rw, gkk_hw, objnr, rw, hw)) %>% 
  dplyr::collect()

# Pull part of database including fundorte
fuo_data <- dplyr::tbl(my_db, paste("fuo")) %>% dplyr::collect()

# Load background grid
tk25 <- dplyr::tbl(my_db, "tk25") %>% dplyr::collect() %>%
  tidyr::unite(col="quadrant", c("KARTE", "QUADRANT"), sep="", remove = TRUE) %>% 
  dplyr::select(-c("KARTE_QUAD")) %>% mutate(quadrant = as.numeric(quadrant))

r_tk4tel <- raster::rasterFromXYZ(tk25[,c("XQMITTE","YQMITTE", "quadrant")], 
                                  crs=sp::CRS("+init=epsg:31468"),
                                  res=c(6100, 5550), digits=0)

districts$SCH <- as.numeric(districts$SCH)
r_districts <- rasterDT::fasterizeDT(districts, r_tk4tel, field="SCH")
districts_df <- raster::stack(r_tk4tel, r_districts) %>% raster::rasterToPoints() %>% 
  as.data.frame(); rm(r_tk4tel, r_districts)
colnames(districts_df) <- c("XQMITTE", "YQMITTE", "quadrant", "district")
districts_df <- districts_df %>% dplyr::select(c(quadrant, district)) %>%
  mutate(quadrant = as.numeric(quadrant))
districts_df$district <- as.character(factor(districts_df$district, 
                                             levels=as.numeric(districts$SCH), 
                                             labels=districts$BEZ_RBZ))

# Disconnect from database
DBI::dbDisconnect(my_db); rm(my_db); invisible(gc())

# combine gridded map from dat_ask with full locations of recorded species and taxonomy
art_data <- art_data %>% left_join(fuo_data) %>% 
  mutate(quadrant = as.numeric(sub("/", "", quadrant))) %>%
  #left_join(taxonomyStd, by=c("art"=="scientificName")) %>% 
  #tidyr::drop_na(class, order) %>%
  left_join(tk25 %>% left_join(districts_df)); rm(fuo_data, tk25, districts_df); invisible(gc())

# Create custom taxon vector (Aves, Lepidoptera, Odonata, Orthoptera)
#art_data$class_order <- "Aves"
#art_data$class_order[art_data$class == "Insecta"] <- art_data$order[art_data$class == "Insecta"]
spec_data <- data.frame(art=unique(art_data$art),
                        class_order=c(rep(c("Aves", "Lepidoptera", "Odonata", "Orthoptera"), each=6), "Aves", "Lepidoptera"))
art_data <- left_join(art_data, spec_data); rm(spec_data); invisible(gc())
unique(art_data$class_order)

art_data <- art_data %>% 
  dplyr::select(XQMITTE, YQMITTE, XLU, YLU, XRU, YRU, XRO, YRO, XLO, YLO, jahr, 
                mon, district, class_order, art)
invisible(gc())

# User interface ----
ui <- fluidPage(
  ## This is the overall title
  titlePanel("Visualisation tool for biodiversity data"),
  
  # Set the slider options in the side bar panel
  sidebarPanel(
    setSliderColor(c("green", "orange", "red"),c(1,2,3)), 
    #helpText("First, select the time period you are interested in:"),
    sliderInput(inputId = "year_weight", label = "Time period:", value = c(1980, 2019),
                min = 1980, max = 2019, step = 1, ticks=F, sep=""),
    helpText("If only grid cells with at least 2 records per time period 
             should be considered, check the box below:"),
    checkboxInput('filter', 'Spatial filtering'),
    shinyWidgets::prettyRadioButtons(inputId = "class_order",
                                     label = "Choose a taxon to display:",
                                     choices = c("All Taxa", unique(art_data$class_order)),
                                     selected = "All Taxa", icon = icon("check"), animation = "pulse",
                                     status = "default", inline = F),
    uiOutput("cat_choice"),
    helpText("Species to display are filtered according to time period and taxon chosen previously."),
    #helpText("If you are only interested in a specific district, you can select one here."),
    shinyWidgets::prettyRadioButtons(inputId = "district",
                                     label = "Choose a district to display:",
                                     choices = c("All districts", na.omit(unique(art_data$district))),
                                     selected = "All districts", icon = icon("check"),
                                     animation = "pulse", status = "default", inline = F),
    helpText("If you are interested in differences among taxa or districts, you can choose 
              one of them at a time."),
    selectInput('facet', 'Comparison of:', c(None='.', 'Taxa', 'Districts')),
    helpText("Make sure to scroll down to see the comparison figures!"),
    helpText("Taxon comparison is only displayed if 'All taxa' are selected, 
             while district comparison is only displayed if 'All districts' are selected."),
    #checkboxInput('facet1', 'Taxon facet'),
    #checkboxInput('facet2', 'District facet'),
    #checkboxInput('smooth', 'Smooth'),
    #selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
    #h3("Select official development assistance (ODA) countries (coming soon)"),
    #actionButton("action", "ODA only"),
    #h3("Download report of the evaluation results (coming soon)"),
    #downloadButton("report", "Generate report"),
    width = 3
  ),
  # Set the different tabs in the main panel
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("Species map",
               h3(textOutput("subtitle1")),
               plotOutput("plot1", height = 325, width = 900),
               h3("Number of observations"),
               plotOutput("plot2", height = 325, width = 900),
               h3(textOutput("subtitle2")),
               plotOutput("plot3", height = 350, width = 1000)
      ),
      tabPanel("Summary table", 
               h3("Summary"),
               h4("Overall summary"),
               verbatimTextOutput("sum1"),
               h4("Spatial summary"),
               DT::dataTableOutput("table1"),
               h4("Temporal summary"),
               DT::dataTableOutput("table2")
      ),
      tabPanel("Species table",
               h3("Species table"),
               DT::dataTableOutput("sp_table") ## Change data table test
      )
    ), 
    width=7
  )
)

# Server logic ----
server <- function(input, output) {
  datyear <- reactive({
    art_data %>% filter(jahr >= input$year_weight[1],
                        jahr <= input$year_weight[2])
  })
  
  dataset <- reactive({
    dat <- datyear()
    if(input$class_order != "All Taxa"){
      dat <- dat %>% filter(class_order == input$class_order)
    }
    if(input$spec != "All Species"){
      dat <- dat %>% filter(art == input$spec)
    }
    if(input$district != "All districts"){
      dat <- dat %>% filter(district == input$district)
    }
    dat <- dat %>% group_by(XQMITTE, YQMITTE, XLU, XRU, YLU, YLO) %>% 
      summarise(`Species richness`=n_distinct(art), 
                `Number of records`=n())
    if (input$filter){
      dat <- dat %>% filter(`Number of records` >= 2)
    }
    dat
  })
  
  datatime <- reactive({
    dat <- datyear()
    if(input$class_order != "All Taxa"){
      dat <- dat %>% filter(class_order == input$class_order)
    }
    if(input$spec != "All Species"){
      dat <- dat %>% filter(art == input$spec)
    }
    if(input$district != "All districts"){
      dat <- dat %>% filter(district == input$district)
    }
    dat <- dat %>% group_by(jahr, class_order) %>% 
      summarise(`Species richness`=n_distinct(art), 
                `Number of records`=n(),
                `Number of occupied grid cells`=n_distinct(XLU, XRU, YLU, YLO))
    #if (input$filter){
    #  dat <- dat %>% filter(`Number of records` >= 2)
    #}
  })
  
  datagroup <- reactive({
    dat <- datyear()
    if(input$district != "All districts"){
      dat <- dat %>% filter(district == input$district)
    }
    dat <- dat %>% 
      group_by(XQMITTE, YQMITTE, XLU, XRU, YLU, YLO, class_order) %>% 
      summarise(`Species richness`=n_distinct(art),
                `Number of records`=n())
    if (input$filter){
      dat <- dat %>% filter(`Number of records` >= 2)
    }
    dat
  })
  
  datadistrict <- reactive({
    dat <- datyear()
    if(input$district != "All districts"){
      dat <- dat %>% filter(district == input$district)
    }
    if(input$class_order != "All Taxa"){
      dat <- dat %>% filter(class_order == input$class_order)
    }
    dat %>% group_by(district, class_order) %>% 
      summarise(`Species richness`=n_distinct(art),
                `Number of records`=n()) %>% 
      pivot_longer(names_to="var", values_to="val", cols=-c(district, class_order)) %>%
      drop_na()
  })
  
  dataraw <- reactive({
    dat <- datyear()
    if(input$class_order != "All Taxa"){
      dat <- dat %>% filter(class_order == input$class_order)
    }
    if(input$spec != "All Species"){
      dat <- dat %>% filter(art == input$spec)
    }
    if(input$district != "All districts"){
      dat <- dat %>% filter(district == input$district)
    }
    dat %>% group_by(XQMITTE, YQMITTE) %>% distinct(jahr, art)
  })
  
  shape <- reactive({
    if (input$district == "All districts"){
      sf::st_as_sf(bavaria)
    } else{
      districts %>% filter(BEZ_RBZ == input$district)
    }
  })
  
  output$subtitle1 <- renderText({
    if(input$spec == "All Species"){
      "Species richness"
    } else{
      "Species presence"
    }
  })
  
  output$subtitle2 <- renderText({
    if (input$facet == 'Taxa'){
      if(input$spec == "All Species" & input$class_order == "All Taxa"){
        "Species richness per taxon"
      }
    } else if(input$facet == 'Districts'){
      "Species richness & Number of records per district"
    }
  })
  
  output$cat_choice <- renderUI({
    selectInput(inputId="spec",
                label="Choose a species to display:", 
                choices = c("All Species", 
                            sort(unique(
                              if(input$class_order != "All Taxa"){
                                art_data %>% filter(class_order==input$class_order) %>% 
                                  filter(jahr >= input$year_weight[1],
                                         jahr <= input$year_weight[2]) %>%
                                  dplyr::select(art) %>% unlist()
                              } else{
                                art_data %>% 
                                  filter(jahr >= input$year_weight[1],
                                         jahr <= input$year_weight[2]) %>%
                                  dplyr::select(art) %>% unlist()
                              })))
    )
  })
  
  output$plot1 <- renderPlot({
    if(input$spec == "All Species"){
      p <- dataset() %>% ggplot() + 
        geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                             ymax="YLO", fill="`Species richness`")) + 
        scico::scale_fill_scico(name="SR", palette="roma", na.value= "grey50")
    } else {
      p <- dataset() %>% ggplot() + 
        geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", ymax="YLO", 
                             fill=factor("`Species richness`"))) + 
        scale_fill_manual(name="", values="darkgrey", labels=c("Presence")) 
    } 
    p <- p + geom_sf(data=shape(), fill="transparent", col="black") +
      labs(x="Longitude", y="Latitude") + 
      coord_sf(xlim = c(min(st_coordinates(shape())[,'X']),
                        max(st_coordinates(shape())[,'X'])),
               ylim = c(min(st_coordinates(shape())[,'Y']),
                        max(st_coordinates(shape())[,'Y']))) + 
      ggspatial::annotation_scale(location="bl", width_hint = 0.2) +
      ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                        pad_x = unit(0.18, "in"), pad_y = unit(0.22, "in"),
                                        style = ggspatial::north_arrow_fancy_orienteering) +
      theme_bw()
    if(input$spec == "All Species"){
      p <- p  + theme(legend.position=c(0.9,0.8), legend.background = element_blank())
      p2 <- datatime() %>% ggplot() + 
        geom_histogram(aes_string(x="jahr", y="`Species richness`",
                                  fill="class_order"), stat="identity") + 
        scale_x_continuous(expand=expansion(add=c(0,0))) + 
        scale_y_continuous(expand=expansion(add=c(0,1))) + 
        scale_fill_manual(values = c("Aves" = '#1b9e77',
                                     "Lepidoptera"='#d95f02',
                                     "Odonata"='#7570b3',
                                     "Orthoptera"='#e7298a')) + 
        labs(x="Year", y="SR", fill="Taxon") + theme_bw() +
        theme(legend.position = c(0.1,0.8), legend.background = element_blank())
    } else {
      p <- p + theme(legend.position=c(0.85,0.95), legend.background = element_blank())
      p2 <- datatime() %>% ggplot() + 
        geom_histogram(aes_string(x="jahr", y="`Number of occupied grid cells`",
                                  fill="class_order"), stat="identity") + 
        scale_x_continuous(expand=expansion(add=c(0,0))) + 
        scale_y_continuous(expand=expansion(add=c(0,1))) + 
        scale_fill_manual(values = c("Aves" = '#1b9e77',
                                     "Lepidoptera"='#d95f02',
                                     "Odonata"='#7570b3',
                                     "Orthoptera"='#e7298a')) + 
        labs(x="Year", y="Number of occupied grid cells", fill="Taxon") + theme_bw() + 
        theme(legend.position = c(0.1,0.8), legend.background = element_blank())
    } 
    p + p2 + plot_layout(widths=c(4,7))
    
    #if (input$color != 'None')
    #  p <- p + aes_string(color=input$color)
  })
  
  output$plot2 <- renderPlot({
    p1 <- dataset() %>% ggplot() + 
      geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                           ymax="YLO", fill="`Number of records`")) + 
      scico::scale_fill_scico(name="Number\nof records", palette="roma", na.value= "grey50") + 
      geom_sf(data=shape(), fill="transparent", col="black") +
      labs(x="Longitude", y="Latitude") + 
      coord_sf(xlim = c(min(st_coordinates(shape())[,'X']),
                        max(st_coordinates(shape())[,'X'])),
               ylim = c(min(st_coordinates(shape())[,'Y']),
                        max(st_coordinates(shape())[,'Y']))) + 
      ggspatial::annotation_scale(location="bl", width_hint = 0.2) +
      ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                        pad_x = unit(0.18, "in"), pad_y = unit(0.22, "in"),
                                        style = ggspatial::north_arrow_fancy_orienteering) +
      theme_bw() + theme(legend.position=c(0.9,0.8), legend.background = element_blank())
    p2 <- datatime() %>% ggplot() + 
      geom_histogram(aes_string(x="jahr", y="`Number of records`",
                                fill="class_order"), stat="identity") + 
      scale_x_continuous(expand=expansion(add=c(0,0))) + 
      scale_y_continuous(expand=expansion(add=c(0,5))) + 
      scale_fill_manual(values = c("Aves" = '#1b9e77',
                                   "Lepidoptera"='#d95f02',
                                   "Odonata"='#7570b3',
                                   "Orthoptera"='#e7298a')) + 
      labs(x="Year", y="Number of records", fill="Taxon") + theme_bw() +
      theme(legend.position = c(0.1,0.8), legend.background = element_blank())
    p1 + p2 + plot_layout(widths=c(4,7))
  })
  
  output$plot3 <- renderPlot({
    if (input$facet == 'Taxa'){
      if(input$spec == "All Species" & input$class_order == "All Taxa"){
        ggplot(data=datagroup()) + 
          geom_rect(aes_string(xmin="XLU", xmax="XRU", ymin="YLU", 
                               ymax="YLO", fill="`Species richness`")) + 
          facet_grid(.~class_order) + 
          scico::scale_fill_scico(name="SR", palette="roma", na.value= "grey50") + 
          geom_sf(data=shape(), fill="transparent", col="black") +
          labs(x="Longitude", y="Latitude") + 
          coord_sf(xlim = c(min(st_coordinates(shape())[,'X']),
                            max(st_coordinates(shape())[,'X'])),
                   ylim = c(min(st_coordinates(shape())[,'Y']),
                            max(st_coordinates(shape())[,'Y']))) + 
          ggspatial::annotation_scale(location="bl", width_hint = 0.15) +
          ggspatial::annotation_north_arrow(location = "bl", which_north = "true", 
                                            pad_x = unit(0, "in"), pad_y = unit(0.18, "in"),
                                            style = ggspatial::north_arrow_fancy_orienteering) +
          theme_bw() + theme(strip.background = element_blank(), 
                             strip.text=element_text(size=12, face="bold"))
      }
    } else if(input$facet == 'Districts'){
      if(input$spec == "All Species"){
        sub_dat1 <- datadistrict() %>% drop_na() %>% filter(var == "Species richness") %>% dplyr::select(-c(var))
        p1 <- sub_dat1 %>% pivot_wider(names_from=district, values_from=val, values_fill=0) %>% 
          ggradar(values.radar = c(0,max(sub_dat1$val)/2,max(sub_dat1$val)), 
                  grid.mid=max(sub_dat1$val)/2, grid.max = max(sub_dat1$val), 
                  group.colours = c("Aves" = '#1b9e77', "Lepidoptera"='#d95f02',
                                    "Odonata"='#7570b3', "Orthoptera"='#e7298a'),
                  axis.label.size = 4, plot.title="Species richness") + 
          theme_minimal() + theme(strip.background = element_blank(), 
                                  axis.text = element_blank(), 
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  strip.text=element_text(size=12, face="bold"),
                                  legend.position = "none")
        sub_dat2 <- datadistrict() %>% drop_na() %>% filter(var == "Number of records") %>% dplyr::select(-c(var))
        p2 <- sub_dat2 %>% pivot_wider(names_from=district, values_from=val, values_fill=0) %>%
          ggradar(values.radar = c(0,max(sub_dat2$val)/2,max(sub_dat2$val)), 
                  grid.mid=max(sub_dat2$val)/2, grid.max = max(sub_dat2$val), 
                  plot.title="Number of records", axis.label.size = 4, 
                  group.colours = c("Aves" = '#1b9e77', "Lepidoptera"='#d95f02',
                                    "Odonata"='#7570b3', "Orthoptera"='#e7298a')) + 
          theme_minimal() + theme(strip.background = element_blank(), 
                                  axis.text = element_blank(), 
                                  panel.grid.major = element_blank(),
                                  panel.grid.minor = element_blank(),
                                  strip.text=element_text(size=12, face="bold"))
        p1 + p2
      }
    }
  })
  
  output$sum1 <- renderPrint({
    dataset() %>% ungroup() %>% 
      dplyr::select(XQMITTE, YQMITTE, `Species richness`, `Number of records`) %>%
      as.data.frame() %>% summary()
  })
  
  output$table1 <- renderDataTable(
    dataset() %>% ungroup() %>%
      dplyr::select(XQMITTE, YQMITTE, `Species richness`, `Number of records`)
  )
  output$table2 <- renderDataTable(datatime())
  output$sp_table <- renderDataTable(dataraw(),
                                     options = list(
                                       pageLength = 25,
                                       initComplete = I("function(settings, json) {alert('Done.');}")
                                     )
  )
}

# Run app ----
shinyApp(ui, server)
