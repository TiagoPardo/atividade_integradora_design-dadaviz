library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(bsicons)

# Ler arquivo csv:
df <- read.csv("df_incrementado.csv")
df$order_purchase_timestamp <- as.Date(df$order_purchase_timestamp)

df_filtrado <- df %>%
  filter(seller_id == '001cca7ae9ae17fb1caed9dfb1094831')

ui <- page_sidebar(
  
  tags$head(
    tags$style(
      HTML("
        body {
          background-color: #white;
        }
      ")
    )
  ),
  title = "Dashboard do Vendedor Olist",
  fillable = FALSE,
  fillable_mobile = FALSE,
  sidebar = sidebar( class = "secundary",
                     selectInput(
                       "periodo", "Período",
                       choices = c("Todos", "Últimos 30 dias", "Trimetre Anterior", "Ano Anterior"),
                       selected = "Todos"
                     ),
                     selectInput(
                       "estado", "Estado",
                       choices = c("Todos", unique(df_filtrado$geolocation_state)),
                       selected = "Todos"
                       
                     ),
                     selectInput(
                       "categoria", "Categoria",
                       choices = c("Todas", unique(df_filtrado$product_category_name)),
                       selected = "Todas"
                     ),
                     checkboxGroupInput(
                       "avaliacao", "Avaliação",
                       choices = sort(unique(df_filtrado$review_score), decreasing = TRUE),
                       selected = unique(df_filtrado$review_score),
                       inline = TRUE
                     )
  ),
  
  layout_column_wrap(
    width = 1/4,
    
    valueBoxOutput('vbpedidos'),
    valueBoxOutput('vbfaturamento'),
    valueBoxOutput('vbentrega'),
    valueBoxOutput('vbavaliacao')
  ),
  layout_columns(
    card(
      card_header(class = 'bg-dark',"Gráfico 1"),
      card_body("Gráfico de linhas com evolução de vendas ao longo do tempo (meses)")
    ),
    card(
      card_header(class = 'bg-dark',"Gráfico 2"),
      card_body("Mapa: Pontos em local de entrega e cores avaliação ou tempo de entrega (definir)")
    ),
    card(
      card_header(class = 'bg-dark',"Gráfico 3"),
      card_body("Gráfico de colunas empilhadas com categoria no eixo X e cores por nota de avaliação")
    )
  ),
  layout_columns(
    card(
      card_header(class = 'bg-dark',"Gráfico 4"),
      card_body("Nuvem de palavras de avaliações")
    )
  )
)

server <- function(input, output, session) {
  
  # Função reativa para filtrar os dados com base no período selecionado e na avaliação selecionada
  df_filtrado_periodo <- reactive({
    periodo_selecionado <- input$periodo
    avaliacao_selecionada <- input$avaliacao
    estado_selecionado <-  input$estado
    categoria_selecionada <- input$categoria
    
    # Calcula a data de início com base no período selecionado
    if (periodo_selecionado == "Últimos 30 dias") {
      data_inicio <- max(df_filtrado$order_purchase_timestamp) - days(30)
    } else if (periodo_selecionado == "Trimetre Anterior") {
      data_inicio <- lubridate::floor_date(max(df_filtrado$order_purchase_timestamp) - months(3), "quarter")
    } else if (periodo_selecionado == "Ano Anterior") {
      data_inicio <- lubridate::floor_date(max(df_filtrado$order_purchase_timestamp) - years(1), "year")
    } else if (periodo_selecionado == "Todos") {
      data_inicio <- min(df_filtrado$order_purchase_timestamp)
    } else {
      data_inicio <- min(df_filtrado$order_purchase_timestamp)  # Data mínima presente nos dados
    }
    
    if(categoria_selecionada == "Todas") {
      categoria_selecionada <- unique(df_filtrado$product_category_name)
    }
    
    if(estado_selecionado == "Todos") {
      estado_selecionado <- unique(df_filtrado$geolocation_state)
    }
    
    # Filtra os dados com base na data de início e na avaliação selecionada
    df_filtrado %>%
      filter(order_purchase_timestamp >= data_inicio,
             review_score %in% avaliacao_selecionada,
             geolocation_state %in% estado_selecionado,
             product_category_name %in% categoria_selecionada)
  })
  
  # Atualiza o value box com o número de pedidos filtrados
  output$vbpedidos <- renderValueBox({
    value_box(
      title = "Pedidos",
      value = nrow(df_filtrado_periodo()),
      showcase = bs_icon("graph-up"),
      theme = "teal",
      p("Aumento de 12% no total"),
      height = "150px"
    )
  })
  
  output$vbfaturamento <- renderValueBox({
    faturamento <- sum(df_filtrado_periodo()$price)
    
    # Formatação personalizada de moeda
    faturamento_formatado <- paste("R$", format(faturamento, nsmall = 2, decimal.mark = ",", big.mark = ".", trim = TRUE))
    
    value_box(
      title = "Faturamento",
      value = faturamento_formatado,
      showcase = bs_icon("graph-up"),
      theme = "teal",
      p("+12%"),
      height = "150px"
    )
  })
  
  
  output$vbentrega <- renderValueBox({
    value_box(
      title = "Entrega",
      value = paste0(round(mean(as.numeric(difftime(df_filtrado_periodo()$order_delivered_customer_date, df_filtrado_periodo()$order_purchase_timestamp, units = "days"))),0)," dias"),
      showcase = bs_icon("graph-up"),
      theme = "teal",
      p("12 pedidos atrasados"),
      height = "150px"
    )
  })
  output$vbavaliacao <- renderValueBox({
    value_box(
      title = "Avaliação",
      value = paste0(round(mean(df_filtrado_periodo()$review_score),1)," de 5"),
      showcase = bs_icon("graph-up"),
      theme = "teal",
      p("13 avaliações negativas"),
      height = "150px"
    )
  })
  
  
  output$map1 <- renderLeaflet({
    leaflet(df_filtrado_periodo(), options = leafletOptions(minZoom = 3, maxZoom = 8)) %>%
      addTiles() %>%
      addMarkers(lng = ~geolocation_lng, 
                 lat = ~geolocation_lat,
                 label = paste("City ", ~geolocation_city, "<br>",
                               "Category", ~product_category_name, "<br>"))
  })
}

shinyApp(ui, server)