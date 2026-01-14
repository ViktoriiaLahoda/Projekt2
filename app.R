#install.packages(c("shiny", "shinydashboard", "tidyverse", "plotly", "DT"))

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

czyszczenie <- function(x) {
  x <- as.character(x)
  x[x == ":" | x == ""] <- NA
  x <- gsub("[^0-9.]", "", x)
  return(as.numeric(x))
}

wczytaj <- function(plik, zmienna, pomijaj_wiersze) {
  github_url <- "https://raw.githubusercontent.com/ViktoriiaLahoda/Projekt2/main/"
  full_url <- paste0(github_url, plik)
  
  df <- read.csv(full_url, skip = pomijaj_wiersze, header = TRUE)
  colnames(df)[1] <- "Kraj"
  
  df <- df %>%
    select(Kraj, contains("2020"), contains("2021"), contains("2022"), contains("2023"), contains("2024")) %>%
    mutate(across(-Kraj, as.character))
  
  df <- df %>%
    pivot_longer(cols = -Kraj, names_to = "Rok", values_to = zmienna) %>%
    mutate(
      Rok = as.numeric(gsub("[^0-9]", "", Rok)),
      !!sym(zmienna) := czyszczenie(!!sym(zmienna)),
      Kraj = trimws(gsub("\\(.*\\)", "", Kraj))
    )
  
  return(df)
}

#Połączenie wszystkich danych
przygotuj_dane <- function() {
  biznes <- wczytaj("business_registration.csv", "Biznes", 10)
  rd <- wczytaj("R_D.csv", "RD", 8)
  edu <- wczytaj("educational_level.csv", "Edu", 10)
  bezrob <- wczytaj("unemployment.csv", "Unemp", 9)
  internet <- wczytaj("internet_access.csv", "Net", 8)
  
  if(is.null(biznes) | is.null(rd) | is.null(edu) | is.null(bezrob) | is.null(internet)) {
    return(NULL)
  }
  
  # Łączenie danych
  dane <- biznes %>%
    inner_join(rd, by = c("Kraj", "Rok")) %>%
    inner_join(edu, by = c("Kraj", "Rok")) %>%
    inner_join(bezrob, by = c("Kraj", "Rok")) %>%
    inner_join(internet, by = c("Kraj", "Rok")) %>%
    filter(!grepl("European Union|Euro area", Kraj)) %>%
    drop_na()
  
  # Standaryzacja 
  dane <- dane %>%
    mutate(
      Biznes = as.numeric(scale(Biznes)),
      RD = as.numeric(scale(RD)),
      Edu = as.numeric(scale(Edu)),
      Unemp = as.numeric(scale(Unemp)),
      Net = as.numeric(scale(Net))
    )
  
  return(dane)
}

# Interfejs aplikacji

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Analiza UE 2020-2024"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("chart-line")),
      menuItem("Korelacje", tabName = "corr", icon = icon("table")),
      menuItem("Model Regresji", tabName = "reg", icon = icon("calculator")),
      menuItem("Dane", tabName = "status", icon = icon("database"))
    )
  ),
  dashboardBody(
    tabItems(
      # ZAKŁADKA 1: Dashboard
      tabItem(tabName = "dash",
              fluidRow(
                box(width = 4, selectInput("kraj", "Wybierz Kraj (Trend):", choices = NULL)),
                box(width = 8, plotlyOutput("trend_plot"))
              ),
              fluidRow(
                box(width = 12, title = "Średnia pozycja państw: Biznes vs R&D", 
                    plotlyOutput("bubble_avg_plot"))
              )
      ),
      
      # ZAKŁADKA 2: Korelacje
      tabItem(tabName = "corr",
              fluidRow(
                box(width = 12, title = "Macierz korelacji (Współczynniki Pearsona)", 
                    tableOutput("corr_table"),
                    footer = "Korelacja powyżej 0.7 jest silna, poniżej 0.3 słaba.")
              )
      ),
      
      # ZAKŁADKA 3: Model Regresji
      tabItem(tabName = "reg",
              box(width = 12, title = "Wyniki Modelu", verbatimTextOutput("reg_summary"))
      ),
      
      # ZAKŁADKA 4: Dane
      tabItem(tabName = "status",
              DTOutput("preview_data")
      )
    )
  )
)


server <- function(input, output, session) {
  
  moje_dane <- reactive({
    przygotuj_dane()
  })
  
  observe({
    df <- moje_dane()
    if(!is.null(df)) {
      updateSelectInput(session, "kraj", choices = sort(unique(df$Kraj)))
    }
  })
  
  # WYKRES TRENDU
  output$trend_plot <- renderPlotly({
    req(moje_dane(), input$kraj)
    
    d <- moje_dane() %>% filter(Kraj == input$kraj)
    
    plot_ly(d, x = ~Rok) %>%
      add_lines(y = ~Biznes, name = "Biznes", line = list(color = "black")) %>%
      add_lines(y = ~RD, name = "R&D", line = list(color = "blue", dash = "dot")) %>%
      layout(title = paste("Trend:", input$kraj))
  })
  
  # WYKRES BĄBELKOWY
  output$bubble_avg_plot <- renderPlotly({
    req(moje_dane())
    
    avg_data <- moje_dane() %>%
      group_by(Kraj) %>%
      summarise(
        avg_Biznes = mean(Biznes),
        avg_RD = mean(RD),
        avg_Edu = mean(Edu),
        .groups = 'drop'
      )
    
    plot_ly(avg_data, x = ~avg_RD, y = ~avg_Biznes, size = ~avg_Edu, color = ~Kraj,
            sizes = c(10, 40), type = 'scatter', mode = 'markers',
            hoverinfo = 'text',
            text = ~paste("<br>Państwo:", Kraj, 
                          "<br>Śr. R&D:", round(avg_RD, 2), 
                          "<br>Śr. Biznes:", round(avg_Biznes, 2)),
            marker = list(sizemode = 'area', opacity = 0.6, line = list(width = 1, color = 'white'))) %>%
      layout(
        xaxis = list(title = "Średnie nakłady na R&D (standaryzowane)"),
        yaxis = list(title = "Średni indeks firm (standaryzowany)"),
        showlegend = FALSE
      )
  })
  
  # TABELA KORELACJI
  output$corr_table <- renderTable({
    req(moje_dane())
    
    d_corr <- moje_dane() %>% select(Biznes, RD, Edu, Unemp, Net)
    res <- cor(d_corr)
    as.data.frame(res) %>% rownames_to_column("Zmienna")
  }, digits = 3)
  
  # MODEL REGRESJI
  output$reg_summary <- renderPrint({
    req(moje_dane())
    summary(lm(Biznes ~ RD + Edu + Unemp + Net + factor(Kraj), data = moje_dane()))
  })
  
  # TABELA Z DANYMI
  output$preview_data <- renderDT({
    req(moje_dane())
    datatable(moje_dane())
  })
}

shinyApp(ui, server)
