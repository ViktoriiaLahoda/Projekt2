install.packages(c("shiny", "shinydashboard", "tidyverse", "plotly", "DT"))
# To run: shiny::runApp()

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
  github_url <- "https://github.com/ViktoriiaLahoda/Projekt2/main/"
  full_url <- paste0(github_url, plik)
  
  tryCatch({
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
  }, error = function(e) {
    warning(paste("Błąd przy ładowaniu:", plik, "-", e$message))
    return(NULL)
  })
}

# Funkcja dla przygotowanie danych
przygotuj_dane <- function() {
  biznes <- wczytaj("business_registration.csv", "Biznes", 10)
  rd <- wczytaj("R_D.csv", "RD", 8)
  edu <- wczytaj("educational_level.csv", "Edu", 10)
  bezrob <- wczytaj("unemployment.csv", "Unemp", 9)
  internet <- wczytaj("internet_access.csv", "Net", 8)
  
  if(is.null(biznes) | is.null(rd) | is.null(edu) | is.null(bezrob) | is.null(internet)) {
    return(NULL)
  }
  
  # Łączenie wszystkich zbiorów danych
  dane <- biznes %>%
    inner_join(rd, by = c("Kraj", "Rok")) %>%
    inner_join(edu, by = c("Kraj", "Rok")) %>%
    inner_join(bezrob, by = c("Kraj", "Rok")) %>%
    inner_join(internet, by = c("Kraj", "Rok")) %>%
    filter(!grepl("European Union|Euro area", Kraj)) %>%
    drop_na()
  
  # Standaryzacja zmiennych
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

# UI

ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "Analiza Wskaźników UE 2020-2024",
    titleWidth = 350
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dash", icon = icon("chart-line")),
      menuItem("Korelacje", tabName = "corr", icon = icon("table")),
      menuItem("Model Regresji", tabName = "reg", icon = icon("calculator")),
      menuItem("Dane", tabName = "status", icon = icon("database")),
      br(),
      p("Źródło: Eurostat", style = "color: #888; font-size: 12px; padding: 10px;")
    )
  ),
  dashboardBody(
    tabItems(
      
     #Dashboard
      
      tabItem(
        tabName = "dash",
        h2("Analiza Trendów i Wskaźników"),
        fluidRow(
          box(
            width = 4,
            selectInput(
              "kraj",
              "Wybierz Kraj (Trend):",
              choices = NULL
            ),
            status = "primary",
            solidHeader = TRUE
          )
        ),
        fluidRow(
          box(
            width = 12,
            plotlyOutput("trend_plot"),
            title = "Trend zmian: Biznes vs R&D",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Średnia pozycja państw: Biznes vs R&D",
            plotlyOutput("bubble_avg_plot"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE
          )
        )
      ),
      
     #Correlation
      tabItem(
        tabName = "corr",
        h2("Macierz Korelacji"),
        fluidRow(
          box(
            width = 12,
            title = "Współczynniki Korelacji Pearsona",
            tableOutput("corr_table"),
            footer = "Korelacja > 0.7 jest silna, < 0.3 słaba.",
            status = "primary",
            solidHeader = TRUE
          )
        )
      ),
      
     # Regression Model
      tabItem(
        tabName = "reg",
        h2("Model Regresji Liniowej"),
        fluidRow(
          box(
            width = 12,
            title = "Wyniki Modelu: Biznes ~ RD + Edu + Unemp + Net + Kraj",
            verbatimTextOutput("reg_summary"),
            status = "primary",
            solidHeader = TRUE
          )
        )
      ),
      
      tabItem(
        tabName = "status",
        h2("Podgląd Danych"),
        fluidRow(
          box(
            width = 12,
            DTOutput("preview_data"),
            status = "primary",
            solidHeader = TRUE
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive expression for data
  moje_dane <- reactive({
    przygotuj_dane()
  })
  
  # Update country choices when data loads
  observe({
    df <- moje_dane()
    if(!is.null(df)) {
      updateSelectInput(session, "kraj", choices = sort(unique(df$Kraj)))
    }
  })
  
  # Trend plot
  
  output$trend_plot <- renderPlotly({
    req(moje_dane(), input$kraj)
    
    d <- moje_dane() %>% filter(Kraj == input$kraj)
    
    plot_ly(d, x = ~Rok) %>%
      add_lines(y = ~Biznes, name = "Biznes", line = list(color = "rgb(0,0,0)", width = 2)) %>%
      add_lines(y = ~RD, name = "R&D", line = list(color = "rgb(31,119,180)", dash = "dot", width = 2)) %>%
      layout(
        title = list(text = paste("Trend dla:", input$kraj)),
        xaxis = list(title = "Rok"),
        yaxis = list(title = "Wartość standaryzowana"),
        hovermode = "x unified",
        template = "plotly_white"
      )
  })
  
# Bubble plot
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
    
    plot_ly(
      avg_data,
      x = ~avg_RD,
      y = ~avg_Biznes,
      size = ~avg_Edu,
      color = ~Kraj,
      sizes = c(10, 40),
      type = 'scatter',
      mode = 'markers',
      hoverinfo = 'text',
      text = ~paste(
        "Państwo:", Kraj, "<br>",
        "Śr. R&D:", round(avg_RD, 2), "<br>",
        "Śr. Biznes:", round(avg_Biznes, 2), "<br>",
        "Śr. Edu:", round(avg_Edu, 2)
      ),
      marker = list(
        sizemode = 'area',
        opacity = 0.7,
        line = list(width = 1, color = 'white')
      )
    ) %>%
      layout(
        title = "Porównanie średnich wskaźników",
        xaxis = list(title = "Średnie nakłady na R&D (standaryzowane)"),
        yaxis = list(title = "Średni indeks firm (standaryzowany)"),
        showlegend = FALSE,
        hovermode = "closest",
        template = "plotly_white"
      )
  })
  
  output$corr_table <- renderTable({
    req(moje_dane())
    
    d_corr <- moje_dane() %>% select(Biznes, RD, Edu, Unemp, Net)
    res <- cor(d_corr)
    as.data.frame(res) %>% rownames_to_column("Zmienna")
  }, digits = 3)
  
  output$reg_summary <- renderPrint({
    req(moje_dane())
    summary(lm(Biznes ~ RD + Edu + Unemp + Net + factor(Kraj), data = moje_dane()))
  })
  
  output$preview_data <- renderDT({
    req(moje_dane())
    datatable(
      moje_dane(),
      options = list(
        pageLength = 10,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Polish.json')
      )
    )
  })
}

shinyApp(ui, server)

