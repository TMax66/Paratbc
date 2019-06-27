credentials <- list("test" = "123")

shinyServer(function(input, output) {
  
  
  USER <- reactiveValues(Logged = FALSE)
  
  observeEvent(input$.login, {
    if (isTRUE(credentials[[input$.username]]==input$.password)){
      USER$Logged <- TRUE
    } else {
      show("message")
      output$message = renderText("Invalid user name or password")
      delay(2000, hide("message", anim = TRUE, animType = "fade"))
    }
  })
  
  output$app = renderUI(
    if (!isTRUE(USER$Logged)) {
      fluidRow(column(width=4, offset = 4,
                      wellPanel(id = "login",
                                textInput(".username", "Username:"),
                                passwordInput(".password", "Password:"),
                                div(actionButton(".login", "Log in"), style="text-align: center;")
                      ),
                      textOutput("message")
      ))
    } else {
      
      navbarPage("Piano paratubercolosi",
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
      

}
)
  output$dt<- DT::renderDataTable(
    dati,server= FALSE,filter = 'top',extensions = 'Buttons',class = 'cell-border stripe', 
    rownames = FALSE, options = list(
      dom = 'Bfrtip',searching = FALSE,paging = TRUE,autoWidth = TRUE,
      pageLength = 10,buttons = c("csv",'excel'))
  )
      

      
  output$distPlot <- renderPlot({
    dati%>%
      filter(codaz==input$cod)%>%
      ggplot(aes(anno, prevalenza))+geom_line()+geom_point()
  })

    
  output$dt2<-DT::renderDataTable(
    
    dati %>% 
      filter(codaz==input$cod) %>% 
      select("data"=datareg, "conferimento"=nconf, proprietario, anno, veterinario, prevalenza) %>% 
      arrange(anno)
    
    
  )
  
  })
