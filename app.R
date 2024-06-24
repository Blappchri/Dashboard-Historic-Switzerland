library(shiny)
library(dplyr)
library(ggiraph)
# add description text
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        '.js-irs-0 .irs-single,
        .js-irs-0 .irs-from,
        .js-irs-0 .irs-to,
        .js-irs-0 .irs-bar-edge,
        .js-irs-0 .irs-bar{  
          border-color: transparent;
          background: #E11A27; 
          border-top: 1px #c0c0c0; 
          border-bottom: 1px #c0c0c0;}'
      )
    )
  ),
  tags$body(
    tags$style(
      HTML(
        'h2 {
        font-family: "News Gothic", monospace;
        font-size: 50px;
        background-color: #E11A27;
        text-align: center;
        margin-top: 0px;
        margin-left: -200px;
        padding-left: 220px;
        padding-top:20px;
        padding-bottom:20px;
        margin-right:-100px;
        padding-right:110px;
        color: white;
        };'
      )
    )
  ),
  titlePanel("Snapshots of old Switzerland"),
  shinyWidgets::setBackgroundColor(c("#E0E0E0")),
  fluidRow(
    column(
      p("The Federal Archive for Historic Monuments within Switzerland freely provides thousands of digitalized historic photographs and postcards under a license for public use. This dashboard provides an interface to explore this treasure trove."),
      p(HTML('<a href="https://opendata.swiss/de/dataset/archiv_photoglob-wehrli">Data Source</a>
        <a href="https://github.com/Blappchri/Dashboard-Historic-Switzerland">Code</a>
        <a href="https://www.linkedin.com/in/christoph-blapp-2659a11bb/">Creator</a>')),
      girafeOutput("map"),
      sliderInput(
        "year",
        "Select Years", 
        min =1896,
        max=1932,
        c(1896,1932),
        step = 1,
        sep="",
        width ="95%",
        ticks = FALSE
      ),width=8
    ),
    column(
      br(),
      br(),
      br(),
      DT::DTOutput("out"),width=4
    )
  )
)

server <- function(input, output) {
  load("./data/ch.rdata")
  links_raw <- read.csv(
    "./data/photoglob-wehrli_metadata.csv", 
    sep=";"
  )
  links<-links_raw%>%
    transmute(
      title=Titel,
      canton=Kanton,
      year=Datierung_von%>%substr(1,4),
      link=Hochauflösendes_Bild
    )%>%
    filter(nchar(link)>10)%>%
    filter(!canton=="")%>%
    filter(!is.na(year))%>%
    filter(!title=="")%>%
    arrange(title)%>%
    mutate(
      canton=forcats::fct_recode(
        canton,
        "Fribourg"="Fribourg/Freiburg",
        "St. Gallen"="Sankt Gallen",
        "Valais"="Wallis",
        "Vaud"="Waadt",
        "Neuchâtel"="Neuenburg"
      )
    )%>%
    mutate(
      title=paste0('<a href="', link,'" > ', title ,'</a>')
    )
  
  links_year<-reactive(
    links%>%
      filter(year>=input$year[1])%>%
      filter(year<=input$year[2])
  )
  
  canton_alphas=reactive(
    data.frame(
      canton=unique(links$canton%>%as.character())
    )%>%
      mutate(
        color_for_map=case_when(
          canton%in%links_year()$canton~"white",
          TRUE~"black"
        )
      )%>%
      rename(name=canton)
  )
  
  enriched_CH<-reactive(
    CH%>%left_join(canton_alphas())
  )
  
  output$map<-renderGirafe(
      {
      (
      ggplot(enriched_CH())+
        geom_sf_interactive(
          aes(
            tooltip=name,
            data_id=name,
            fill=color_for_map
          ),
          col="black")+
        theme_void()+
        scale_fill_identity()+
        theme(
          panel.background = element_rect(fill="transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )
    )%>%
      girafe(ggobj=.,bg="transparent")%>%
      girafe_options(
        opts_selection(css="stroke:black;fill:#E11A27"),
        opts_hover(css="stoke:black;fill:#909090"),
        opts_toolbar(hidden=c("selection","zoom","misc"))
      )}
  )
  
  cantons_to_pick=reactive(
    if (isTruthy(input$map_selected)) {
      input$map_selected
    }else{
      unique(links$canton)
    }
  )
  
  links_final<-reactive(
    links_year()%>%
      filter(canton%in%cantons_to_pick())%>%
      select(Link=title, Year=year)
  )
  
  output$out=DT::renderDT(links_final(),escape=FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
