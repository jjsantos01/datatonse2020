

ui = dashboardPage(
  dashboardHeader(title="Empleo"),
  dashboardSidebar(
    sidebarMenu(               menuItem("Introducción", tabName = "intro00", icon=icon("chart-bar")),   menuItem("Nacional", tabName = "nac00", icon=icon("chart-bar")),  
                                  menuItem("Estatal", tabName = "est00", icon=icon("chart-bar")),
                                  menuItem("Municipal", tabName = "mun00", icon=icon("chart-bar"))
                             

    
    
  )),
  dashboardBody(  tags$head(tags$style(tags$style(css))), tabItems(
    tabItem("intro00", box(title="Introducción", solidHeader = TRUE,"El Semáforo de Recuperación del Empleo Formal es una herramienta que permitirá consultar la tendencia en los puestos de trabajos registrados ante el Instituto Mexicano del Seguro Social  para los municipios del país, comparando el momento previo al inicio de la cuarentena (febrero) con el mes más reciente que cuente con información pública.
   Los datos del IMSS tienen frecuencia mensual y tienen desagregación a nivel municipal, lo que los hace idóneos para tener una visión actualizada y a detalle de la situación laboral. 
  Este tablero puede resumir de forma rápida la situación del empleo formal y puede apoyar a que las autoridades federales, estatales y locales tengan un panorama equilibrado de la situación en los municipios, tomando en cuenta a los grupos que pueden ser más afectados." , status="warning"), box(title="Notación", solidHeader = TRUE, "Consideramos una escala de semáforo para cada indicador, definiendo los colores a partir de los siguientes rangos:
Rojo: si la variación del empleo con respecto a febrero es menor a -10%
Naranja: si la variación del empleo con respecto a febrero está entre -10% y 0%
Verde: si la variación del empleo con respecto a febrero es igual o mayor a 0%", status="success"), box(title="Limitante", solidHeader = TRUE,
 "Los datos de empleo formal registrados ante el IMSS, aunque son bastante útiles, no muestran el panorama completo porque buena parte del empleo en el país es informal y todavía no existen medios para medirlo a nivel municipal.", status="danger"),
 
 htmlOutput('pdfviewer'), box(title="Integrantes del equipo", solidHeader = TRUE, "Juan Javier Santos Ochoa, Daniela Jiménez Lara, Raquel Yunoen Badillo Salas", status="info")),
    
           tabItem(tabName="nac00",  box(title="Comparación internacional",solidHeader = TRUE, "La pandemia ocasionada por la COVID-19 ha ocasionado la mayor caída del empleo que se haya registrado en México. Entre febrero y mayo de 2020 la tasa de ocupación cayó 21.8%, 
                                         por lo que se perdieron más de 12 millones de puestos de trabajo 
                                         tanto formales como informales.", status="info", width=12),
                   box(fluidPage(column(6,radioButtons("indicador_int", label = h5('Indicador'),
                                                                        choices = list("Empleo Total"="pob_ocup", "Empleo asegurado"="pob_aseg"), 
                                                                        selected = "pob_ocup"), box("Fuente: BID observatorio laboral, https://observatoriolaboral-bid.herokuapp.com", width=12), width=6), 
                                                  column(3,radioButtons("tipo_int", label = h5('Tipo de estadístico'),
                                                                        choices = list("Absoluto"=0, "Cambio Relativo"=1, "Cambio absoluto"=2), 
                                                                        selected = 1 )),
                                                  
                                                  column(3, radioButtons("month_base_int", label = h5('Mes Base'),
                                                                         choices = list("Enero"=1, "Febrero"=2, "Marzo"=3, "Abril"=4, "Mayo"=5, "Junio"=6, "Julio"=7, "Agosto"=8, "Septiembre"=9, "Octubre"=10, "Noviembre"=11,  "Diciembre"=12), 
                                                                         selected = 3)), width=12 ), width=6), box(plotlyOutput("absoluto_int"), width=6), box(plotlyOutput("plotEmH1"), width=12),
                   
                   box("Pese a que todos sufrimos las consecuencias de la pandemia, no todos hemos sido afectados de igual manera. Los datos disponibles nos dejan ver que las personas con ingresos más bajos fueron quienes más perdieron su empleo", status="warning", width=12), 
                    box(plotOutput("plotsa_caida"), width=6), box(plotOutput("plotsa"), width=6),  
                   box("Asimismo, los jóvenes se vieron afectados en mucha mayor proporción y las actividades de servicios, especialmente en hoteles y restaurantes, tuvieron la caída más profunda en el número de empleos", status="warning", width=12),
                   box(plotlyOutput("plotEdad"), width=6), box(plotlyOutput("plotEdad_caida"), width=6),
                   
                   box("A partir de junio la economía ha dado señales de recuperación, mostrando importantes avances en algunos sectores. En el periodo julio-octubre se han vuelto a crear casi 400 mil empleos formales. Los trabajadores de menores ingresos y los jóvenes son quienes han recuperado más rápido los empleos perdidos, especialmente en los sectores de industria de la transformación y la construcción. Los estados industrializados del centro y el norte del país son los que has despegado más rápido, mientras que los menos industrializados y más dependientes del turismo todavía no logran rebotar.", status="warning", width=12),
                   box(plotlyOutput("plotsector_caida"), width=6), box(plotlyOutput("plotsector"), width=6)
                   , box(plotlyOutput("plotte_caida"), width=6), box(plotlyOutput("plotte"), width=6)),
           tabItem(tabName="est00",        box("Esta sección muestra los semáforos de crecimiento para cada municipio de los estados de la república. Un semáforo rojo significa que el crecimeinto del empleo está por debajo del -10% respecto a febrero un semáforo naranja es que estuvo entre -10 y 0 porciento y un semáforo verde es que tuvo crecimiento positivo", status="warning"),      box(shiny::selectInput("selEdo", 
                                                                   "Seleccione estado", 
                                                                   choices = unique(map$NOM_ENT), 
                                                                   selected = "Morelos"
           )),   leafletOutput("mapaYun") %>% withSpinner(), box(title="Interpretación", "Los estados más afectados por la pandemia fueron los estados turísticos, tal el el caso de  Quintana Roo, Nayarit, Sinaloa, Yucatán, Campeche y Guerrero ; mientras que los estados más recuperados fueron  Querétaro y Tabasco ", solidHeader =TRUE, width=12, status="success")),
           tabItem(tabName="mun00",    
                   box(selectInput(
             "Estado", label = h3("Estado"), choices = mylist),   uiOutput('my_inputs'), width=6) , 
            box( selectInput(
               "Estado2", label = h3("Estado"), choices = mylist),   uiOutput('my_inputs2'), width=6) , 
             box(plotlyOutput("plotmunSemaforo"), width=6),
            box(plotlyOutput("plotmunSemaforo2"), width=6),
             box(plotlyOutput("plotmunTotal"), width=6),  
            box(plotlyOutput("plotmunTotal2"), width=6),  
             box(plotlyOutput("plotmunGenero"), width=6), 
            box(plotlyOutput("plotmunGenero2"), width=6),
             box(plotlyOutput("plotmunEdad"), width=6),
            box(plotlyOutput("plotmunEdad2"), width=6),
             box(plotlyOutput("plotmunPymes"), width=6), 
            box(plotlyOutput("plotmunPymes2"), width=6), 
             box(plotlyOutput("plotmunSalario"), width=6),
            box(plotlyOutput("plotmunSalario2"), width=6),
            box(plotOutput("plotcajeros"), width=6),
            box(plotOutput("plotcajeros2"), width=6))
)
)
)


