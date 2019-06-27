shinyUI(fluidPage(
  # Add Javascript
  tags$head(
    tags$link(rel="stylesheet", type="text/css",href="style.css"),
    tags$script(type="text/javascript", src = "md5.js"),
    tags$script(type="text/javascript", src = "passwdInputBinding.js")
  ),
  useShinyjs(),
  
  #titlePanel(""),
  
  uiOutput("app")
))









#############INTERFACCIA GRAFICA###################################################
ui<-navbarPage("Piano paratubercolosi",
       tabPanel("Situazione aziendale",
                fluidPage(
                  #useShinyalert(),
                  sidebarPanel(
                    selectInput("cod", "Codice Aziendale",
                                c(unique(as.character(dati$codaz))))
                    
                 
                  ),
                  mainPanel(
                       DT::dataTableOutput("dt2"),
                       plotOutput("distPlot")
                  )

                )
                ),
       
       tabPanel("Inserimento qualifiche"),
       
       tabPanel("Database",
                fluidPage(
                  
                  mainPanel(
                    DT::dataTableOutput("dt")
                  )
                  
                )

                )
       
 
           )
       
       
       







