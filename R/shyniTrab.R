library(shiny)
library(ggplot2)
library(DBI)
library(RMySQL)
library(reshape2)
library(dplyr)
library(scales)
library(lessR)

id_host = "dbmysql.ckvpndo3tqd7.us-east-1.rds.amazonaws.com"
db <- dbConnect(RMySQL::MySQL(),
                dbname = "bd_gestiondatos",
                host = id_host,
                user = "admin",
                password = rstudioapi::askForPassword("DB password"),
                Port     = 3306)

df <- dbGetQuery(db, 'SELECT * FROM estudiante')

######################################################################################
# Procesamiento para evaluacion mensual de matrículas
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

# Evolución mensual de matrículas

df2 = df

df2$month = getMonthDay(df2$FECHA_MATRICULA)
df2 = data.frame(df2$FECHA_MATRICULA, df2$ID_PERSONA, df2$month)

df_graph = aggregate(x = df2.ID_PERSONA ~ df2.month, 
                     data = df2, 
                     FUN = length)

df_graph$df2.month <- factor(df_graph$df2.month, levels = df_graph$df2.month, ordered=T)

df_graph$formated_month = getFormatedMonth(df_graph$df2.month)
df_graph$anho = substr(df_graph$df2.month, 1, 4)

# subset1 = df_graph[df_graph$anho == '2020', ]
# subset2 = df_graph[df_graph$anho == '2021', ]
# subset3 = df_graph[df_graph$anho == '2022', ]
# 
# id1=factor(subset1$formated_month, levels=c("Enero", "Febrero", "Marzo",
#                                             "Abril", "Mayo", "Junio",
#                                             "Julio", "Agosto", "Setiembre",
#                                             "Octubre", "Noviembre", "Diciembre"))
# id2=factor(subset2$formated_month, levels=c("Enero", "Febrero", "Marzo",
#                                             "Abril", "Mayo", "Junio",
#                                             "Julio", "Agosto", "Setiembre",
#                                             "Octubre", "Noviembre", "Diciembre"))
# id3=factor(subset3$formated_month, levels=c("Enero", "Febrero", "Marzo",
#                                             "Abril", "Mayo", "Junio",
#                                             "Julio", "Agosto", "Setiembre",
#                                             "Octubre", "Noviembre", "Diciembre"))
# require(gridExtra)
# ss1 = ggplot(subset1, aes(x = id1, y = df2.ID_PERSONA, label=df2.ID_PERSONA, group = 1)) +
#   geom_line(linetype = 2, colour = "#3B80BD", size = 1) +
#   geom_point(colour = "#3B80BD", size = 5) +
#   geom_text(hjust=0, vjust=0) +
#   labs(x = "Meses", y = "# Matrículas",
#        title = "Evolución mensual de matrículas 2020") +
#   theme(panel.background = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
# ss2 = ggplot(subset2, aes(x = id2, y = df2.ID_PERSONA, label=df2.ID_PERSONA, group = 1)) +
#   geom_line(linetype = 2, colour = "#3B80BD", size = 1) +
#   geom_point(colour = "#3B80BD", size = 5) +
#   geom_text(hjust=0, vjust=0) +
#   labs(x = "Meses", y = "# Matrículas",
#        title = "Evolución mensual de matrículas 2021") +
#   theme(panel.background = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
# ss3 = ggplot(subset3, aes(x = id3, y = df2.ID_PERSONA, label=df2.ID_PERSONA, group = 1)) +
#   geom_line(linetype = 2, colour = "#3B80BD", size = 1) +
#   geom_point(colour = "#3B80BD", size = 5) +
#   geom_text(hjust=0, vjust=0) +
#   labs(x = "Meses", y = "# Matrículas",
#        title = "Evolución mensual de matrículas 2022") +
#   theme(panel.background = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
# grid.arrange(ss1, ss2, ss3, ncol=2)

######################################################################################
# Niveles educativos
niveles_educativos = dbGetQuery(db, 'select NIVEL_EDUCATIVO,ID_ANIO,count(*)CNT from estudiante group by NIVEL_EDUCATIVO,ID_ANIO')

# s1 = niveles_educativos[niveles_educativos$ID_ANIO == '2020', ]
# s2 = niveles_educativos[niveles_educativos$ID_ANIO == '2021', ]
# s3 = niveles_educativos[niveles_educativos$ID_ANIO == '2022', ]

# 
# gg1 = ggplot(s1,
#        aes(x = NIVEL_EDUCATIVO, y = CNT, label=CNT)) +
#   geom_bar(stat = "identity",fill='#3B80BD')+
#   geom_text(hjust=0, vjust=0) +
#   theme(panel.background = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())+
#   labs(title ='Nivel educativo 2020',
#        x = 'Nivel educativo',
#        y = 'Cantidad',
#        subtitle = 'cantidades por nivel educativo',
#        caption = 'Fuente de datos : Ministerio de educación',
#   )
# gg2 = ggplot(s2,
#        aes(x = NIVEL_EDUCATIVO, y = CNT, label=CNT)) +
#   geom_bar(stat = "identity",fill='#3B80BD')+
#   geom_text(hjust=0, vjust=0) +
#   theme(panel.background = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())+
#   labs(title ='Nivel educativo 2021',
#        x = 'Nivel educativo',
#        y = 'Cantidad',
#        subtitle = 'cantidades por nivel educativo',
#        caption = 'Fuente de datos : Ministerio de educación',
#   )
# gg3 = ggplot(s2,
#        aes(x = NIVEL_EDUCATIVO, y = CNT, label=CNT)) +
#   geom_bar(stat = "identity",fill='#3B80BD')+
#   geom_text(hjust=0, vjust=0) +
#   theme(panel.background = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())+
#   labs(title ='Nivel educativo 2022',
#        x = 'Nivel educativo',
#        y = 'Cantidad',
#        subtitle = 'cantidades por nivel educativo',
#        caption = 'Fuente de datos : Ministerio de educación',
#   )
# grid.arrange(gg1, gg2, gg3, ncol=2)

######################################################################################
# Dona con la descripcion del sexo

ValidCampos= df[,c(1, 20, 21)]
ValidSexo = ValidCampos
ValidSexo$SEXO = substr(ValidSexo$SEXO, 1, 6)
#filter(ValidSexo, SEXO == "")
ValidSexo <- ValidSexo[!is.na(ValidSexo$VALIDACION_DNI),]
#filter(ValidSexo, SEXO == "")
gruposSexo <- group_by(ValidSexo, ID_ANIO ,SEXO)
agrupSexo = summarise(gruposSexo, num = n())
agrupSexo2 = agrupSexo %>% as.data.frame()
agrupSexo2
agrupSexo2$porcentaje <- prop.table(agrupSexo2$num)
agrupSexo2

# t1 = agrupSexo2[agrupSexo2$ID_ANIO == '2020', ]
# t2 = agrupSexo2[agrupSexo2$ID_ANIO == '2021', ]
# t3 = agrupSexo2[agrupSexo2$ID_ANIO == '2022', ]
# 
# gt1 = ggplot(t1,aes(x=2,y=porcentaje, fill=SEXO))+
#   geom_bar(stat = "identity",
#            color="white")+
#   geom_text(aes(label=percent(porcentaje)),
#             position=position_stack(vjust=0.5),color="white",size=3)+
#   coord_polar(theta = "y")+
#   scale_fill_manual(values=c("#E11E1E","#3B80BD"))+
#   theme_void()+
#   labs(title="Género 2020")+
#   xlim(0.5,2.5)
# 
# gt2 = ggplot(t2,aes(x=2,y=porcentaje, fill=SEXO))+
#   geom_bar(stat = "identity",
#            color="white")+
#   geom_text(aes(label=percent(porcentaje)),
#             position=position_stack(vjust=0.5),color="white",size=3)+
#   coord_polar(theta = "y")+
#   scale_fill_manual(values=c("#E11E1E","#3B80BD"))+
#   theme_void()+
#   labs(title="Género 2021")+
#   xlim(0.5,2.5)
# 
# gt3 = ggplot(t3,aes(x=2,y=porcentaje, fill=SEXO))+
#   geom_bar(stat = "identity",
#            color="white")+
#   geom_text(aes(label=percent(porcentaje)),
#             position=position_stack(vjust=0.5),color="white",size=3)+
#   coord_polar(theta = "y")+
#   scale_fill_manual(values=c("#E11E1E","#3B80BD"))+
#   theme_void()+
#   labs(title="Género 2022")+
#   xlim(0.5,2.5)
# grid.arrange(gt1, gt2, gt3, ncol=2)
######################################################################################
# Validacion de DNI

ValidDNI = ValidCampos
ValidDNI$SEXO = substr(ValidDNI$SEXO, 1, 6)
ValidDNI <- ValidDNI[!is.na(ValidDNI$VALIDACION_DNI),]
head(ValidDNI)
gruposValDNI <- group_by(ValidDNI, ID_ANIO, VALIDACION_DNI)
agrupValDNI = summarise(gruposValDNI, num = n())
agrupValDNI2 = agrupValDNI %>% as.data.frame()
agrupValDNI2$porcentaje <- prop.table(agrupValDNI2$num)
agrupValDNI2

# u1 = agrupValDNI2[agrupValDNI2$ID_ANIO == '2020', ]
# u2 = agrupValDNI2[agrupValDNI2$ID_ANIO == '2021', ]
# u3 = agrupValDNI2[agrupValDNI2$ID_ANIO == '2022', ]
# 
# ug1 = ggplot(u1,aes(x=2,y=porcentaje, fill=VALIDACION_DNI))+
#   geom_bar(stat = "identity",
#            color="white")+
#   geom_text(aes(label=percent(porcentaje)),
#             position=position_stack(vjust=0.5),color="white",size=3)+
#   coord_polar(theta = "y")+
#   scale_fill_manual(values=c("salmon","steelblue","orange","gray"))+
#   theme_void()+
#   labs(title="DNI Validados 2020")+
#   xlim(0.5,2.5)
# 
# ug2 = ggplot(u2,aes(x=2,y=porcentaje, fill=VALIDACION_DNI))+
#   geom_bar(stat = "identity",
#            color="white")+
#   geom_text(aes(label=percent(porcentaje)),
#             position=position_stack(vjust=0.5),color="white",size=3)+
#   coord_polar(theta = "y")+
#   scale_fill_manual(values=c("salmon","steelblue","orange","gray"))+
#   theme_void()+
#   labs(title="DNI Validados 2021")+
#   xlim(0.5,2.5)
# 
# ug3 = ggplot(u3,aes(x=2,y=porcentaje, fill=VALIDACION_DNI))+
#   geom_bar(stat = "identity",
#            color="white")+
#   geom_text(aes(label=percent(porcentaje)),
#             position=position_stack(vjust=0.5),color="white",size=3)+
#   coord_polar(theta = "y")+
#   scale_fill_manual(values=c("salmon","steelblue","orange","gray"))+
#   theme_void()+
#   labs(title="DNI Validados 2022")+
#   xlim(0.5,2.5)
# grid.arrange(ug1, ug2, ug3, ncol=2)
######################################################################################
#PROCESAMIENTO DE DATA PARA DISCAPACIDAD

dfdisc = df[c("ID_ANIO","DSC_DISCAPACIDAD","ID_PERSONA")]

dfdiscFilt <- dfdisc %>% filter(DSC_DISCAPACIDAD != "")

dfdiscFilt

grupoDisc <- dfdiscFilt %>%
  group_by(ID_ANIO,DSC_DISCAPACIDAD) %>%
  summarise(
    T_dfdisc = n()
  )

#grupoDisc

#Gráfico ggplot2
# g1 = grupoDisc[grupoDisc$ID_ANIO == '2020', ]
# g2 = grupoDisc[grupoDisc$ID_ANIO == '2021', ]
# g3 = grupoDisc[grupoDisc$ID_ANIO == '2022', ]
# 
# gd1 = ggplot(g1, aes(x = DSC_DISCAPACIDAD, y=T_dfdisc)) +
#   geom_bar(stat = "identity", fill='#3B80BD') +
#   coord_flip() +
#   labs(title ='Discapacidad 2020',
#        x = 'DISCAPACIDAD',
#        y = 'CANTIDAD DE ESTUDIANTES',
#        subtitle = 'cantidades por nivel educativo',
#        caption = 'Fuente de datos : Ministerio de educación'
#   ) + 
#   theme(#axis.text.x =element_blank(),
#     axis.ticks.x = element_blank(),
#     axis.ticks.y = element_blank(),
#     panel.background = element_blank()
#   )
# gd2 = ggplot(g2, aes(x = DSC_DISCAPACIDAD, y=T_dfdisc)) +
#   geom_bar(stat = "identity", fill='#3B80BD') +
#   coord_flip() +
#   labs(title ='Discapacidad 2021',
#        x = 'DISCAPACIDAD',
#        y = 'CANTIDAD DE ESTUDIANTES',
#        subtitle = 'cantidades por nivel educativo',
#        caption = 'Fuente de datos : Ministerio de educación'
#   ) +
#   theme(#axis.text.x =element_blank(),
#     axis.ticks.x = element_blank(),
#     #axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     panel.background = element_blank()
#   )
# gd3 = ggplot(g3, aes(x = DSC_DISCAPACIDAD, y=T_dfdisc)) +
#   geom_bar(stat = "identity", fill='#3B80BD') +
#   coord_flip() +
#   labs(title ='Discapacidad 2022',
#        x = 'DISCAPACIDAD',
#        y = 'CANTIDAD DE ESTUDIANTES',
#        subtitle = 'cantidades por nivel educativo',
#        caption = 'Fuente de datos : Ministerio de educación'
#   ) +
#   theme(#axis.text.x =element_blank(),
#     axis.ticks.x = element_blank(),
#     #axis.text.y = element_blank(),
#     axis.ticks.y = element_blank(),
#     panel.background = element_blank()
#   )
# grid.arrange(gd1, gd2, gd3, ncol=2)
######################################################################################
#PROCESAMIENTO DE DATA PARA NACIONALIDAD

dfpais <- df[c("ID_ANIO","NACIONALIDAD","ID_PERSONA")]

dfpaisFilt <- dfpais %>%
  filter(NACIONALIDAD != "Perú" & NACIONALIDAD != "PERU" & NACIONALIDAD != " " & NACIONALIDAD != "")

dfpaisFilt 

grupoPais <- dfpaisFilt %>%
  group_by(ID_ANIO,NACIONALIDAD) %>%
  summarise(
    CANTIDAD = n()
  )

######################################################################################

ui <- fluidPage(
  # theme = bslib::bs_theme(bootswatch = "yeti"),
  title = "Análisis de estudiantes",
  titlePanel(
    # Título del app o descripción
    "Análisis de estudiantes"
  ),
  fluidRow(
    selectInput(
      "anho",
      "Año:",
      c("2020" = "2020",
        "2021" = "2021",
        "2022" = "2022"),
      selected = "2020"
    )
  ),
  fluidRow(
    column(4,
      plotOutput("histogNivEducativos")
    ),
    column(5,
      plotOutput("histogMatriculas")
    ),
    column(3,
      plotOutput("grupDisc")
    )
  ),
  fluidRow(
    column(6,
           "Estudiantes de otra nacionalidad",
           dataTableOutput("grupPais") 
    ),
    column(3,
           plotOutput("histogSexo")
    ),
    column(3,
           plotOutput("histogValidacionDNI")
    )
  )
)

server <- function(input, output, session) {
  # thematic::thematic_shiny()
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
      theme(panel.background = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
  }, res = 96)
  
  output$histogNivEducativos <- renderPlot({
    subsetNivEducativos = niveles_educativos[niveles_educativos$ID_ANIO == input$anho, ]
    
    ggplot(subsetNivEducativos, 
           aes(x = NIVEL_EDUCATIVO, y = CNT, label=CNT)) +
      geom_bar(stat = "identity",fill='#3B80BD')+
      geom_text(hjust=0, vjust=0) +
      theme(panel.background = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())+
      labs(title ='Nivel educativo',
           x = 'Nivel educativo',
           y = 'Cantidad',
           subtitle = 'cantidades por nivel educativo',
           caption = 'Fuente de datos : Ministerio de educación'
      )
    
  }, res = 96)

  output$histogSexo <- renderPlot({
    # Gráfico
    subsetSexo = agrupSexo2[agrupSexo2$ID_ANIO == input$anho, ]

    ggplot(subsetSexo,aes(x=2,y=porcentaje, fill=SEXO))+
      geom_bar(stat = "identity",
               color="white")+
      geom_text(aes(label=percent(porcentaje)),
                position=position_stack(vjust=0.5),color="white",size=3)+
      coord_polar(theta = "y")+
      scale_fill_manual(values=c("#E11E1E","#3B80BD"))+
      theme_void()+
      labs(title="Género")+
      xlim(0.5,2.5)
  }, res = 96)

  output$histogValidacionDNI <- renderPlot({
    subsetValDNI = agrupValDNI2[agrupValDNI2$ID_ANIO == input$anho, ]

    ggplot(subsetValDNI,aes(x=2,y=porcentaje, fill=VALIDACION_DNI))+
      geom_bar(stat = "identity",
               color="white")+
      geom_text(aes(label=percent(porcentaje)),
                position=position_stack(vjust=0.5),color="white",size=3)+
      coord_polar(theta = "y")+
      scale_fill_manual(values=c("salmon","steelblue","orange","gray"))+
      theme_void()+
      labs(title="DNI Validados")+
      xlim(0.5,2.5)
  }, res = 96)
  
  output$grupDisc <- renderPlot({
    subsetDiscapacidad = grupoDisc[grupoDisc$ID_ANIO == input$anho, ]
    
    ggplot(subsetDiscapacidad, aes(x = DSC_DISCAPACIDAD, y=T_dfdisc)) +
      geom_bar(stat = "identity", fill='#3B80BD') +
      coord_flip() +
      labs(title ='Discapacidad',
           x = 'DISCAPACIDAD',
           y = 'CANTIDAD DE ESTUDIANTES',
           subtitle = 'cantidades por nivel educativo',
           caption = 'Fuente de datos : Ministerio de educación'
      ) +
      theme(
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank()
      ) 
  })
  
  output$grupPais <- renderDataTable(
    grupoPais[grupoPais$ID_ANIO == input$anho, ],
    options = list(pageLength = 10, searching = FALSE)
  )
}

shinyApp(ui, server)