

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(
        
            # Application title
            img(src = "JN logo.png", height = 100, width = 300, align = "center"),
            navbarPage(
                tabPanel(
                    title = "Podcasts",
                    sidebarLayout(
                        sidebarPanel(
                            width = 30,
                            h3("Explore os podcasts do Jovem Nerd!"),
                            
                            selectInput(
                                inputId = "podcast_choice",
                                label = "Selecione seu podcast favorito:",
                                choices = sort(unique(df_NC$Product))
                                        )
                                    ),
                        mainPanel(
                            fluidRow(plotOutput("duracao_categorias", width = 1200, height = 750),
                                plotOutput("convidados_barra", width = 1200, height = 750)),
                            plotOutput("episodios_linha", width = 1200, height = 750)
                                 )
                                )
                        )
                       )
                )

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$duracao_categorias <- renderPlot({
        df_NC %>%
            filter(Product == input$podcast_choice) %>%
           group_by(Subject_1) %>%
           summarize(Tempo_total_hr = round(sum(Duration)/3600, 2)) %>%
           mutate(Subject_1 = fct_reorder(Subject_1, Tempo_total_hr)) %>%
           ggplot(aes(Subject_1, Tempo_total_hr))+
           geom_col()+
           theme_classic()+
           labs(title = "Duracao total por categoria de NC.",
                subtitle = "Separados por classe",
                x = "",
                y = "Tempo [horas]")+
           coord_flip()
   })
   
   df_NC$Episode <- as.numeric(df_NC$Episode)
   output$episodios_linha <- renderPlot({df_NC %>%
           filter(Product == input$podcast_choice) %>%
           filter(!is.na(as.numeric(Episode))) %>% 
           mutate(Tempo_total_hr = round(Duration/3600, 2)) %>%
           ggplot(aes(Episode, Tempo_total_hr, color = Product))+
           geom_line()+
           facet_grid(Product~.)+
           theme_bw()+
           labs(title = "Duracao dos episodios",
                subtitle = "Por Podcast, excluindo NerdCast",
                x = "# do episodio",
                y = "Duracao [Horas]")})
   
   output$convidados_barra <- renderPlot({
      NC_guests %>%
         filter(Product == input$podcast_choice) %>%
         group_by(Guest_name) %>%
         filter(Guest_name != "NA") %>% 
         summarize(Tempo_total_hr = round(sum(Duration)/3600, 2)) %>% 
         top_n(7) %>%
         mutate(Guest_name = fct_reorder(Guest_name, Tempo_total_hr)) %>%
         ggplot(aes(Guest_name, Tempo_total_hr, fill = Guest_name))+
         geom_col()+
         theme_classic()+
         theme(legend.position = "none")+
         labs(title = "Duracao da participacao por convidado.",
              subtitle = "",
              x = "Convidados",
              y = "Tempo [horas]")+
         coord_flip()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
