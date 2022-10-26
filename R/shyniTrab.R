library(shiny)
library(ggplot2)
library(DBI)
library(RMySQL)
library(reshape2)

id_host = "localhost"
db <- dbConnect(RMySQL::MySQL(),
                dbname = "bd_gestiondatos",
                host = id_host,
                user = "root",
                password = rstudioapi::askForPassword("Database password"),
                Port     = 3306)

getMonthDay = function(arrayMes) {
  month = as.Date(arrayMes)
  fec = format(month, "%Y-%m")
}

getFormatedMonth <- function(arrayMes) {
  nombre_meses <- c("01"="Enero", "02"="Febrero", "03"="Marzo",
                    "04"="Abril", "05"="Mayo", "06"="Junio",
                    "07"="Julio", "08"="Agosto", "09"="Setiembre",
                    "10"="Octubre", "11"="Noviembre", "12"="Diciembre")
  mo = nombre_meses[substr(arrayMes, 6, 7)]
}

df <- dbGetQuery(db, 'SELECT * FROM estudiante')
df2 = df

df2$month = getMonthDay(df2$FECHA_MATRICULA)
df2 = data.frame(df2$FECHA_MATRICULA, df2$ID_PERSONA, df2$month)

df_graph = aggregate(x = df2.ID_PERSONA ~ df2.month, 
                     data = df2, 
                     FUN = length)

df_graph$df2.month <- factor(df_graph$df2.month, levels = df_graph$df2.month, ordered=T)

df_graph$formated_month = getFormatedMonth(df_graph$df2.month)
df_graph$anho = substr(df_graph$df2.month, 1, 4)

#subset1 = df_graph[df_graph$anho == '2021', ]
#subset2 = df_graph[df_graph$anho == '2022', ]

ui <- fluidPage(
  selectInput(
    "anho",
    "Año:",
    c("2020" = "2020",
      "2021" = "2021",
      "2022" = "2022"),
    selected = "2020"
  ),
  plotOutput("histogMatriculas")
)

server <- function(input, output, session) {
  #anho_selected = renderText({paste("You chose", input$anho) })
  
  #output$saludo <- anho_selected
  
  output$histogMatriculas <- renderPlot({
   # Gráfico
    subset = df_graph[df_graph$anho == input$anho, ]

    
    id=factor(subset$formated_month, levels=c("Enero", "Febrero", "Marzo",
                                                      "Abril", "Mayo", "Junio",
                                                      "Julio", "Agosto", "Setiembre",
                                                      "Octubre", "Noviembre", "Diciembre"))
    
    ggplot(subset, aes(x = id, y = df2.ID_PERSONA, label=df2.ID_PERSONA, group = 1)) +
      geom_line(linetype = 2, colour = "#3B80BD", size = 1) + 
      #geom_point(aes(size = 0.1)) +
      #geom_point(aes(colour = "red")) +
      geom_point(colour = "#3B80BD", size = 5) + 
      geom_text(hjust=0, vjust=0) + 
      labs(x = "Meses", y = "# Matrículas", 
           title = "Evolución mensual de matrículas") +
      theme(panel.background = element_blank())
    
  }, res = 96)
}

shinyApp(ui, server)