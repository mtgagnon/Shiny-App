library(ggvis)
library(shiny)
library(leaflet)
library(htmltools)
library(shinythemes)

# For dropdown menu
#actionLink <- function(inputId, ...) {
 #   tags$a(href='javascript:void',
  #         id=inputId,
   #        class='action-button',
    #       ...)
#}


navbarPage("Exploration of US Land",
    
   theme = shinytheme("darkly"),
   tabPanel("Home",
            fluidRow(
              column(10, align="left", offset = 1,
                     tags$h1(
                       tags$b("Land use and ecosystem loss in the lower 48 states"
                       )
                     ),
                     tags$h3(
                        tags$p("   In recent centuries technology has vastly improved, aiding in the explosion of the human population which,
                               now in 2020, has surpassed 7.6 billion globally. This explosion of population has undoubtedly affected many ecosystems,
                               and as humans continue to expand agricultural and urban land use, examining which ecosystems have been impacted is becoming 
                               increasingly urgent. "
                              ),
                        tags$br(),
                        tags$p("   The United States is the fourth largest country in the world and has 2.3 billion acres of land, with the lower 48 states comprising
                                1.9 billion of those acres. In our web app we explore how the ~1,000 ecosystems of the US have changed, where the changes have been 
                                the greatest, as well as how the land of the US has is used between urban settings, agriculture, and natural.")
                     )
              ),
              column(4, align = "right",
                     tags$img(src = 'aerial-architectural-design-architecture-buildings-373912.jpg', height = "100%", width = "80%")
              ),
              column(4, align = "middle",
                     tags$img(src = 'photo-of-green-field-near-mountains-974314.jpg', height = "100%", width = '80%')
              ),
              column(4, align = "left",
                     tags$img(src = 'nature-forest-trees-park-38136.jpg', height = '100%', width = '80%')
              )
            )
   ),
   tabPanel("Graph", 
        titlePanel("Ecosystem Explorer"),
        fluidRow(
            column(3,
                   wellPanel(
                       h4("Filter"),
                       sliderInput("diff", "Minimum number of disturbed acres (millions)",
                                   0, 51, 0, step = .01),
                       sliderInput("curAcres", "Current Acres (millions)",
                                   0, 65, c(0, 70), step = .01),
                       sliderInput("perChange", "Percent change",
                                   0, 100, c(0, 100), step = .01),
                   ),
            ),
            column(9,
                   wellPanel(ggvisOutput("plot1")
                   ),
                   wellPanel(
                       span("Number of ecosystems selected:",
                            textOutput("n_ecosystems")
                       )
                   )
            )
        )
    ),
    tabPanel("US Map",
        tags$style(type = "text/css", "#shinyMap {height: calc(100vh - 50px) !important;}"),
        leafletOutput("shinyMap"
                       ),
        absolutePanel( id = "controls", class = "panel panel-default", fixed = TRUE,
                      draggable = TRUE, top = "auto", left = 10, right = "auto", bottom = 10,
                      width = 160, height = 170, offset = 0.5,
                      
                      tags$h3(
                        tags$b("Color By: ")
                      ),
                      radioButtons("colorBy", label = NULL,
                                    choices = list("    Percent Natural" = 1, "    Percent Agriculture" = 2, "    Percent Urban" = 3),
                                    selected = 1
                      ),
                      tags$h6('(Wait for Map to Load)')
        
            #sliderInput("percentSelect", "Percent",
            #            0, 100, c(0, 100), step = 0.01),
            #span("Number of Counties Shown: ", textOutput("n_counties"))
        )
    ),
    tabPanel("About",
             #this page has about what code we used how we created this page
             fluidRow(
               column(7, align = 'left', offset = 1,
                      tags$h1(
                        tags$b('About Our App')
                      ),
                      tags$h3(
                        tags$ul(
                          tags$li('App developed by Mathurin Gagnon (coding and design) and Randy Swaty (data preparation)'),
                          tags$br(),
                          tags$li('We developed the app in ', tags$a(href = 'https://rstudio.com/', "R-Studio"),  ' using the Shiny package for interactivity.'),
                          tags$br(),
                          tags$li('Project developed for internship with the ', tags$a(href = 'https://google.com', 'Conservation Data Lab'), '.'),
                          tags$br()
                        )
                      ),
                      tags$h1(
                        tags$b('Input Data:')
                      ),
                      tags$h3(
                        tags$ul(
                          tags$li('LANDFIRE Biophysical Settings for historical ecosystems and Existing Vegetation Types for conversion (learn more at ',
                            tags$a(href = 'https://www.landfire.gov/', 'www.landfire.gov'), ').' 
                          ),
                          tags$br(),
                          tags$li('US County Shape File ', tags$a( href = 'https://community.esri.com/thread/24614/', 'download '), 'page.'),
                          tags$br(),
                          tags$li('US County Data can be found ', tags$a(href = 'https://www.google.com/', 'here'), "."),
                          tags$br()
                        )
                      ),
                      tags$h1(
                        tags$b('Photo Credit:')
                      ),
                      tags$h3(
                        tags$ul(
                          tags$li('Photo of Green Field Near Mountains: Photo by ', tags$a(href = 'https://www.pexels.com/@burst', 'Burst'), ' via ', 
                                  tags$a(href = 'https://www.pexels.com/', 'pexels'), '.'),
                          tags$br(),
                          tags$li('Aerial Photo of City Commercial Buildings: Photo by ', tags$a(href = 'https://www.pexels.com/@timmossholder', 'Tim Mossholder'), ' via ', 
                                  tags$a(href = 'https://www.pexels.com/', 'pexels'), '.'),
                          tags$br(),
                          tags$li('Green Leafed Tree: Photo by ', tags$a(href = 'https://www.pexels.com/@veeterzy', 'Veeterzy'), ' via ', 
                                  tags$a(href = 'https://www.pexels.com/', 'pexels'), '.'),
                          tags$br(),
                          tags$br()
                        )
                      )
               ),
               column(3, align = 'right', offset = 0,
                 tags$img(src = 'me.PNG', height = '100%', width = "80%")
               )
             )
    )
)
