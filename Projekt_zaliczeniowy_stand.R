# --- 1. BIBLIOTEKI ---
libs <- c("shiny", "shinydashboard", "tidyverse", "plotly", "DT", "corrplot", "Hmisc")
for(l in libs) { 
  if(!require(l, character.only = T)) install.packages(l)
  library(l, character.only = T) 
}

# --- 2. FUNKCJA CZYSZCZĄCA ---
clean_eurostat_vals <- function(x) {
  x <- as.character(x)
  x[x == ":" | x == "" | is.na(x) | grepl(":", x)] <- NA
  x <- gsub("[^0-9.]", "", x)
  return(as.numeric(x))
}

# --- 3. UI ---
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
      # Dashboard z czystym wykresem bąbelkowym
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
      # Nowa zakładka: Tabela korelacji
      tabItem(tabName = "corr",
              fluidRow(
                box(width = 12, title = "Macierz korelacji (Współczynniki Pearsona)", 
                    tableOutput("corr_table"),
                    footer = "Korelacja powyżej 0.7 jest silna, poniżej 0.3 słaba.")
              )
      ),
      tabItem(tabName = "reg",
              box(width = 12, title = "Wyniki Modelu", verbatimTextOutput("reg_summary"))
      ),
      tabItem(tabName = "status",
              DTOutput("preview_data")
      )
    )
  )
)

# --- 4. SERVER ---
server <- function(input, output, session) {
  
  process_file <- function(fname, colname, skip_n) {
    if(!file.exists(fname)) return(NULL)
    df <- read.csv(fname, skip = skip_n, header = TRUE, check.names = TRUE)
    colnames(df)[1] <- "Kraj"
    df %>%
      select(Kraj, matches("2020|2021|2022|2023|2024")) %>%
      mutate(across(-Kraj, as.character)) %>% 
      pivot_longer(-Kraj, names_to = "Rok", values_to = colname) %>%
      mutate(Rok = as.numeric(gsub("[^0-9]", "", Rok)),
             !!sym(colname) := clean_eurostat_vals(!!sym(colname)),
             Kraj = trimws(gsub("\\(.*\\)", "", Kraj)))
  }
  
  full_data <- reactive({
    f_biz <- process_file("business_registration.csv", "Biznes", 10)
    f_rd  <- process_file("R_D.csv", "RD", 8)
    f_edu <- process_file("educational_level.csv", "Edu", 10)
    f_une <- process_file("unemployment.csv", "Unemp", 9)
    f_net <- process_file("internet_access.csv", "Net", 8)
    
    if(any(sapply(list(f_biz, f_rd, f_edu, f_une, f_net), is.null))) return(NULL)
    
    final <- f_biz %>%
      inner_join(f_rd, by = c("Kraj", "Rok")) %>%
      inner_join(f_edu, by = c("Kraj", "Rok")) %>%
      inner_join(f_une, by = c("Kraj", "Rok")) %>%
      inner_join(f_net, by = c("Kraj", "Rok")) %>%
      filter(!grepl("European Union|Euro area", Kraj)) %>% 
      drop_na()
    
    # Standaryzacja wszystkich zmiennych
    final %>% mutate(across(c(Biznes, RD, Edu, Unemp, Net), ~ as.numeric(scale(.))))
  })
  
  observe({
    df <- full_data()
    if(!is.null(df)) updateSelectInput(session, "kraj", choices = sort(unique(df$Kraj)))
  })
  
  # --- WYKRES BĄBELKOWY (Uśrednione dane, bez dużych napisów) ---
  output$bubble_avg_plot <- renderPlotly({
    req(full_data())
    avg_data <- full_data() %>%
      group_by(Kraj) %>%
      summarise(avg_Biznes = mean(Biznes), avg_RD = mean(RD), avg_Edu = mean(Edu), .groups = 'drop')
    
    plot_ly(avg_data, x = ~avg_RD, y = ~avg_Biznes, size = ~avg_Edu, color = ~Kraj,
            sizes = c(10, 40), type = 'scatter', mode = 'markers',
            # Napis pojawia się tylko po najechaniu myszką
            hoverinfo = 'text',
            text = ~paste("Państwo:", Kraj, 
                          "<br>Śr. R&D:", round(avg_RD, 2), 
                          "<br>Śr. Biznes:", round(avg_Biznes, 2)),
            marker = list(sizemode = 'area', opacity = 0.6, line = list(width = 1, color = 'white'))) %>%
      layout(xaxis = list(title = "Średnie nakłady na R&D (standaryzowane)"),
             yaxis = list(title = "Średni indeks firm (standaryzowany)"),
             showlegend = FALSE)
  })
  
  # --- TABELA KORELACJI ---
  output$corr_table <- renderTable({
    req(full_data())
    d_corr <- full_data() %>% select(Biznes, RD, Edu, Unemp, Net)
    res <- cor(d_corr)
    # Formatowanie tabeli dla lepszej czytelności
    as.data.frame(res) %>% rownames_to_column("Zmienna")
  }, digits = 3)
  
  output$trend_plot <- renderPlotly({
    req(full_data(), input$kraj)
    d <- full_data() %>% filter(Kraj == input$kraj)
    plot_ly(d, x = ~Rok) %>%
      add_lines(y = ~Biznes, name = "Biznes", line = list(color = "black")) %>%
      add_lines(y = ~RD, name = "R&D", line = list(color = "blue", dash = "dot")) %>%
      layout(title = paste("Trend:", input$kraj))
  })
  
  output$reg_summary <- renderPrint({
    req(full_data())
    summary(lm(Biznes ~ RD + Edu + Unemp + Net + factor(Kraj), data = full_data()))
  })
  
  output$preview_data <- renderDT({
    req(full_data())
    datatable(full_data())
  })
}

shinyApp(ui, server)
