server = function(input, output) {
  output$NacIndex<- renderText({"Esta sección muestra los datos empleos y salarios reales a nivel nacional. Los datos están desglosados por 
  caracteristicas demográficas como sexo y edad y características de la empresa como sector y tamaño de la empresa. En esta sección se pueden comparar las variaciones por fechas. Año (mes) base se refiere al año inicial y Año (mes)
  de comparación se refiere al año (mes) final a comparar. Las secciones permiten múltiples selecciones entonces es posible comparar entre grupos dentro de una misma categoría.
  Los salarios están deflactados con el INPC públicado por INEGI y representan el salario promedio mensual en pesos de Julio de 2018."})
  output$HistIndex<- renderText({ "Esta página muestra los salarios y número de empleados a nivel nacional para el periodo 2005-2020. 
  Los datos están desglosados tanto en indicadores sociodemográficos (sexo, edad); 
  como en indicadores del tipo de ocupación y empresa (Tamaño de la empresa, Sector al que pertenece). Uno o más indicadores pueden ser seleccionados para comprar entre grupos.
    Los salarios están deflactados con el INPC públicado por INEGI y representan el salario promedio mensual en pesos de Julio de 2018. Esto es el salario diario de cotización multiplicado por 30.41. "})
  
output$EmpleoIndex<- renderText({"Los mapas y gráficas aquí mostrados representan a los municipios mexicanos que presentaron mayores variaciones en su salario real respecto a un mes y año base "})
output$SalarioIndex<- renderText({"Los mapas y gráficas aquí mostrados representan a los municipios mexicanos que presentaron mayores variaciones en su empleo respecto a un mes y año base "})
output$introIndex<- renderText({"Esta sección es a nivel municipal y muestra la correlación entre la variación en salarios reales y empleo total. Los scatterplots mostrados representan a cada municipio en la población objetivo. 
  Las regresiones pueden ser filtradas por características de la empresa y de los trabajadores. Todas las regresiones están ponderadas por por población de cada municipio en 2015."})


xxx<-reactive({
  municipios_filtered<-municipios_names%>%
    filter(state_cod==input$Estado)%>%
    filter(cvemun %in% total_orig$cve_mun )
  
  choices_mun = data.frame(
    var = unique(municipios_filtered$nom_mun),
    num =  unique(municipios_filtered$cvemun))
  
  # List of choices for selectInput
  mylist_mun <- as.list(choices_mun$num)
  # Name it
  names(mylist_mun) <- choices_mun$var
  return( mylist_mun)
})


output$my_inputs <- renderUI({
  selectInput("municipios", 
              label = "Municipio", 
              choices = xxx())
  
})

xxx2<-reactive({
  municipios_filtered<-municipios_names%>%
    filter(state_cod==input$Estado2)%>%
    filter(cvemun %in% total_orig$cve_mun )
  
  choices_mun = data.frame(
    var = unique(municipios_filtered$nom_mun),
    num =  unique(municipios_filtered$cvemun))
  
  # List of choices for selectInput
  mylist_mun <- as.list(choices_mun$num)
  # Name it
  names(mylist_mun) <- choices_mun$var
  return( mylist_mun)
})


output$my_inputs2 <- renderUI({
  selectInput("municipios2", 
              label = "Municipio", 
              choices = xxx2())
  
})


output$plotmunTotal<- renderPlotly({
  total <-total_orig %>% dplyr::select(-c(Month,`Month ID`)) 
  g_empleo_total <- total %>% 
    filter(mes<9) %>% 
    mutate(mes=factor(mes),
           year=as.character(year)) %>% 
    filter(cve_mun==input$municipios) %>% #no se como conectar el dropdown para que se modifique el muncipio
    ggplot(aes(mes,asegurados, group=year, color=year))+
    geom_line(size=2)+
    scale_color_manual(values=c( "#d48626","#26d494"), name="año")+
    scale_x_discrete(labels=c("1" = "Ene", "2" = "Feb",
                              "3"="Mar", "4"="Abr",
                              "5"="Mayo", "6"="Jun",
                              "7"="Jul", "8"="Ago"))+
    labs(title = "Empleo en 2019 vs 2020")+
    ylab("Número de empleados asegurados en el IMSS")+
    theme_dataton()
  
  
 ggplotly(g_empleo_total)
  
})


output$plotmunTotal2<- renderPlotly({
  total <-total_orig %>% dplyr::select(-c(Month,`Month ID`)) 
  g_empleo_total <- total %>% 
    filter(mes<9) %>% 
    mutate(mes=factor(mes),
           year=as.character(year)) %>% 
    filter(cve_mun==input$municipios2) %>% #no se como conectar el dropdown para que se modifique el muncipio
    ggplot(aes(mes,asegurados, group=year, color=year))+
    geom_line(size=2)+
    scale_color_manual(values=c( "#d48626","#26d494"), name="año")+
    scale_x_discrete(labels=c("1" = "Ene", "2" = "Feb",
                              "3"="Mar", "4"="Abr",
                              "5"="Mayo", "6"="Jun",
                              "7"="Jul", "8"="Ago"))+
    labs(title = "Empleo en 2019 vs 2020")+
    ylab("Número de empleados asegurados en el IMSS")+
    theme_dataton()
  
  
  ggplotly(g_empleo_total)
  
})

output$plotmunGenero<- renderPlotly({
  genero <-  sexo %>% 
    dplyr::select(-c(Month, `Month ID`))
  
  g_genero <- genero %>% 
    filter(mes<9) %>% 
    mutate(mes=factor(mes),
           year=as.character(year)) %>% 
    filter(cve_mun==input$municipios) %>% #no se como conectar el dropdown para que se modifique el muncipio
    ggplot(aes(mes,asegurados, group=year, color=year))+
    geom_line(size=2)+
    scale_color_manual(values=c( "#d48626","#26d494"), name="año")+
    scale_x_discrete(labels=c("1" = "Ene", "2" = "Feb",
                              "3"="Mar", "4"="Abr",
                              "5"="Mayo", "6"="Jun",
                              "7"="Jul", "8"="Ago"))+
    labs(title = "Empleo en 2019 vs 2020 por sexo")+
    facet_wrap(~sexo)+
    ylab("Número de empleados asegurados en el IMSS")+
    theme_dataton()
  
  g_genero <- ggplotly(g_genero)
  
})

output$plotmunGenero2<- renderPlotly({
  genero <-  sexo %>% 
    dplyr::select(-c(Month, `Month ID`))
  
  g_genero <- genero %>% 
    filter(mes<9) %>% 
    mutate(mes=factor(mes),
           year=as.character(year)) %>% 
    filter(cve_mun==input$municipios2) %>% #no se como conectar el dropdown para que se modifique el muncipio
    ggplot(aes(mes,asegurados, group=year, color=year))+
    geom_line(size=2)+
    scale_color_manual(values=c( "#d48626","#26d494"), name="año")+
    scale_x_discrete(labels=c("1" = "Ene", "2" = "Feb",
                              "3"="Mar", "4"="Abr",
                              "5"="Mayo", "6"="Jun",
                              "7"="Jul", "8"="Ago"))+
    labs(title = "Empleo en 2019 vs 2020 por sexo")+
    facet_wrap(~sexo)+
    ylab("Número de empleados asegurados en el IMSS")+
    theme_dataton()
  
  g_genero <- ggplotly(g_genero)
  
})


output$plotmunEdad<- renderPlotly({

  g_edad <- edad %>% 
    filter(mes<9) %>% 
    mutate(mes=factor(mes),
           year=as.character(year)) %>% 
    filter(cve_mun==input$municipios) %>% #no se como conectar el dropdown para que se modifique el muncipio
    ggplot(aes(mes,asegurados, group=year, color=year))+
    geom_line(size=2)+
    scale_color_manual(values=c( "#d48626","#26d494"), name="año")+
    scale_x_discrete(labels=c("1" = "Ene", "2" = "Feb",
                              "3"="Mar", "4"="Abr",
                              "5"="Mayo", "6"="Jun",
                              "7"="Jul", "8"="Ago"))+
    labs(title = "Empleo en 2019 vs 2020 por grupo de edad")+
    facet_wrap(~categoria_edad)+
    ylab("Número de empleados asegurados en el IMSS")+
    theme_dataton()
  
  g_edad <- ggplotly(g_edad)
})



output$plotmunEdad2<- renderPlotly({
  
  g_edad <- edad %>% 
    filter(mes<9) %>% 
    mutate(mes=factor(mes),
           year=as.character(year)) %>% 
    filter(cve_mun==input$municipios2) %>% #no se como conectar el dropdown para que se modifique el muncipio
    ggplot(aes(mes,asegurados, group=year, color=year))+
    geom_line(size=2)+
    scale_color_manual(values=c( "#d48626","#26d494"), name="año")+
    scale_x_discrete(labels=c("1" = "Ene", "2" = "Feb",
                              "3"="Mar", "4"="Abr",
                              "5"="Mayo", "6"="Jun",
                              "7"="Jul", "8"="Ago"))+
    labs(title = "Empleo en 2019 vs 2020 por grupo de edad")+
    facet_wrap(~categoria_edad)+
    ylab("Número de empleados asegurados en el IMSS")+
    theme_dataton()
  
  g_edad <- ggplotly(g_edad)
})


output$plotmunPymes<- renderPlotly({
  
  #pymes----

  
  pymes %>%
    filter(mes<9) %>% 
    mutate(mes=factor(mes),
           year=as.character(year)) %>% 
    filter(cve_mun==input$municipios) %>% #no se como conectar el dropdown para que se modifique el muncipio
    ggplot(aes(mes,asegurados, group=year, color=year))+
    geom_line(size=2)+
    scale_color_manual(values=c( "#d48626","#26d494"), name="año")+
    scale_x_discrete(labels=c("1" = "Ene", "2" = "Feb",
                              "3"="Mar", "4"="Abr",
                              "5"="Mayo", "6"="Jun",
                              "7"="Jul", "8"="Ago"))+
    labs(title = "Empleo en 2019 vs 2020 en Pymes (<=50 empleados)")+
    ylab("Número de empleados asegurados en el IMSS")+
    theme_dataton()
})


output$plotmunPymes2<- renderPlotly({
  
  #pymes----
  
  
  pymes %>%
    filter(mes<9) %>% 
    mutate(mes=factor(mes),
           year=as.character(year)) %>% 
    filter(cve_mun==input$municipios2) %>% #no se como conectar el dropdown para que se modifique el muncipio
    ggplot(aes(mes,asegurados, group=year, color=year))+
    geom_line(size=2)+
    scale_color_manual(values=c( "#d48626","#26d494"), name="año")+
    scale_x_discrete(labels=c("1" = "Ene", "2" = "Feb",
                              "3"="Mar", "4"="Abr",
                              "5"="Mayo", "6"="Jun",
                              "7"="Jul", "8"="Ago"))+
    labs(title = "Empleo en 2019 vs 2020 en Pymes (<=50")+
    ylab("Número de empleados asegurados en el IMSS")+
    theme_dataton()
})



output$plotmunSalario<- renderPlotly({
  

  
  g_salario <- salario %>% 
    filter(mes<9) %>% 
    mutate(mes=factor(mes),
           year=as.character(year)) %>% 
    filter(cve_mun==input$municipios) %>% #no se como conectar el dropdown para que se modifique el muncipio
    ggplot(aes(mes,asegurados, group=year, color=year))+
    geom_line(size=2)+
    scale_color_manual(values=c( "#d48626","#26d494"), name="año")+
    scale_x_discrete(labels=c("1" = "Ene", "2" = "Feb",
                              "3"="Mar", "4"="Abr",
                              "5"="Mayo", "6"="Jun",
                              "7"="Jul", "8"="Ago"))+
    labs(title = "Empleo en 2019 vs 2020 en umas (<=50)")+
    ylab("Número de empleados asegurados en el IMSS")+
    theme_dataton()
  
})



output$plotmunSalario2<- renderPlotly({
  
  
  
  g_salario <- salario %>% 
    filter(mes<9) %>% 
    mutate(mes=factor(mes),
           year=as.character(year)) %>% 
    filter(cve_mun==input$municipios2) %>% #no se como conectar el dropdown para que se modifique el muncipio
    ggplot(aes(mes,asegurados, group=year, color=year))+
    geom_line(size=2)+
    scale_color_manual(values=c( "#d48626","#26d494"), name="año")+
    scale_x_discrete(labels=c("1" = "Ene", "2" = "Feb",
                              "3"="Mar", "4"="Abr",
                              "5"="Mayo", "6"="Jun",
                              "7"="Jul", "8"="Ago"))+
    labs(title = "Empleo en 2019 vs 2020 en umas (<=50)")+
    ylab("Número de empleados asegurados en el IMSS")+
    theme_dataton()
  
})

output$plotmunSemaforo<- renderPlotly({
  colors <- c( "darkred","darkorange2","chartreuse4" )

  
  t_semaforo<- semaforo%>%  
    mutate(sem_total=case_when(total<=-10~1,
                               total>-10&total<=0~2,
                               total>0~3),
           sem_mujeres=case_when(mujeres<=-10~1,
                                 mujeres>-10&mujeres<=0~2,
                                 mujeres>0~3),
           sem_jovenes=case_when(jovenes<=-10~1,
                                 jovenes>-10&jovenes<=0~2,
                                 jovenes>0~3),
           sem_mayores=case_when(mayores<=-10~1,
                                 mayores>-10&mayores<=0~2,
                                 mayores>0~3),
           sem_uma=case_when(uma<=-10~1,
                             uma>-10&uma<=0~2,
                             uma>0~3),
           sem_pyme=case_when(pyme<=-10~1,
                              pyme>-10&pyme<=0~2,
                              pyme>0~3)) %>% 
    dplyr::select(cve_mun, starts_with("sem")) %>% 
    pivot_longer(-cve_mun, names_to="categoria",values_to="valor") %>% 
    mutate(cve_mun=as.character(cve_mun))
  
  
  t_o_semaforo <- semaforo %>% 
    pivot_longer(-cve_mun, names_to="categoria", values_to="original") %>% 
    mutate(cve_mun=as.character(cve_mun))
  
  
  moda <- t_semaforo %>% 
    group_by(cve_mun) %>% 
    summarise(moda=Mode(valor, na.rm=TRUE)) %>% 
    ungroup()
  
  t2_semaforo <-  t_semaforo %>% 
    pivot_wider(names_from = categoria, values_from=valor) %>% 
    left_join(moda) %>%
    pivot_longer(-cve_mun, names_to="categoria",values_to="valor") %>% 
    mutate(cve=case_when(categoria=="sem_total"~"total",
                         categoria=="sem_mujeres"~"mujeres",
                         categoria=="sem_jovenes"~"jovenes", 
                         categoria=="sem_mayores"~"mayores",
                         categoria=="sem_uma"~"uma",     
                         categoria=="sem_pyme"~"pyme")) %>% 
    left_join(t_o_semaforo, by=c("cve_mun","cve"="categoria"))
  
  
  t2_semaforo %>%
    filter(cve_mun==input$municipios) %>%
    ggplot(aes(x = categoria, y = cve_mun, fill = factor(valor)))+
    geom_tile(color="black") +
    coord_equal()+
    geom_text(aes(label=round(original,2)))+
    scale_fill_manual(values=c("1"="darkred",
                               "2"="darkorange2",
                               "3"="chartreuse4"))+
    labs(title = "Semáforo de empleo")+
    ylab("Clave del municipio")+
    scale_x_discrete(labels=c("moda"="General", "sem_jovenes"="Empleo en\njóvenes",
                              "sem_mayores"="Empleo en\nadultos mayores",
                              "sem_mujeres"= "Empleo en\n mujeres",
                              "sem_pyme"="Empleo en\nPYMES",
                              "sem_total"="Empleo\ntotal",
                              "sem_uma"="Empleo en\nUMAS(<=50)"))+
    theme_dataton()+
    theme(legend.position = "none",
          panel.grid = element_blank())
  
})
  



output$plotmunSemaforo2<- renderPlotly({
  
  colors <- c( "darkred","darkorange2","chartreuse4" )
  
  t_semaforo<- semaforo%>%  
    mutate(sem_total=case_when(total<=-10~1,
                               total>-10&total<=0~2,
                               total>0~3),
           sem_mujeres=case_when(mujeres<=-10~1,
                                 mujeres>-10&mujeres<=0~2,
                                 mujeres>0~3),
           sem_jovenes=case_when(jovenes<=-10~1,
                                 jovenes>-10&jovenes<=0~2,
                                 jovenes>0~3),
           sem_mayores=case_when(mayores<=-10~1,
                                 mayores>-10&mayores<=0~2,
                                 mayores>0~3),
           sem_uma=case_when(uma<=-10~1,
                             uma>-10&uma<=0~2,
                             uma>0~3),
           sem_pyme=case_when(pyme<=-10~1,
                              pyme>-10&pyme<=0~2,
                              pyme>0~3)) %>% 
    dplyr::select(cve_mun, starts_with("sem")) %>% 
    pivot_longer(-cve_mun, names_to="categoria",values_to="valor") %>% 
    mutate(cve_mun=as.character(cve_mun))
  
  
  t_o_semaforo <- semaforo %>% 
    pivot_longer(-cve_mun, names_to="categoria", values_to="original") %>% 
    mutate(cve_mun=as.character(cve_mun))
  
  
  moda <- t_semaforo %>% 
    group_by(cve_mun) %>% 
    summarise(moda=Mode(valor, na.rm=TRUE)) %>% 
    ungroup()
  
  t2_semaforo <-  t_semaforo %>% 
    pivot_wider(names_from = categoria, values_from=valor) %>% 
    left_join(moda) %>%
    pivot_longer(-cve_mun, names_to="categoria",values_to="valor") %>% 
    mutate(cve=case_when(categoria=="sem_total"~"total",
                         categoria=="sem_mujeres"~"mujeres",
                         categoria=="sem_jovenes"~"jovenes", 
                         categoria=="sem_mayores"~"mayores",
                         categoria=="sem_uma"~"uma",     
                         categoria=="sem_pyme"~"pyme")) %>% 
    left_join(t_o_semaforo, by=c("cve_mun","cve"="categoria"))
  
  
  t2_semaforo %>%
    filter(cve_mun==input$municipios2) %>%
    ggplot(aes(x = categoria, y = cve_mun, fill = factor(valor)))+
    geom_tile(color="black") +
    coord_equal()+
    geom_text(aes(label=round(original,2)))+
    scale_fill_manual(values=c("1"="darkred",
                               "2"="darkorange2",
                               "3"="chartreuse4"))+
    labs(title = "Semáforo de empleo")+
    ylab("Clave del municipio")+
    scale_x_discrete(labels=c("moda"="General", "sem_jovenes"="Empleo en\njóvenes",
                              "sem_mayores"="Empleo en\nadultos mayores",
                              "sem_mujeres"= "Empleo en\n mujeres",
                              "sem_pyme"="Empleo en\nPYMES",
                              "sem_total"="Empleo\ntotal",
                              "sem_uma"="Empleo en\nUMAS(<=50)"))+
    theme_dataton()+
    theme(legend.position = "none",
          panel.grid = element_blank())
  
})





output$plotcajeros<- renderPlot({
  
 x<- cajeros %>% 
    pivot_longer(-c(cve_ent,cve_mun,mes), names_to="tipo", values_to="valor") %>% 
    mutate( mes=factor(mes)) %>%
    filter(cve_mun==input$municipios) %>% 
    ggplot(aes(mes,valor, color=tipo, group=tipo))+
    geom_line(size=4)+
    scale_x_discrete(labels=c( "2" = "Feb",
                               "3"="Mar", "4"="Abr",
                               "5"="Mayo", "6"="Jun",
                               "7"="Jul", "8"="Ago"))+
    scale_color_manual(name = "",values=c("#8931e0","#313cd4"),
                       labels = c("cajeros automáticos", "terminal punto de venta"))+
    labs(title = "Crecimiento de transacciones con respecto a enero en:")+
    ylab("crecimiento de transacciones")+
    theme_dataton()+
    theme(legend.position = "top",
          legend.text = element_text(size = 12))
 return(x)
  
})

output$plotcajeros2<- renderPlot({
  
  x<-  cajeros %>% 
    pivot_longer(-c(cve_ent,cve_mun,mes), names_to="tipo", values_to="valor") %>% 
    mutate( mes=factor(mes)) %>%
    filter(cve_mun==input$municipios2) %>% 
    ggplot(aes(mes,valor, color=tipo, group=tipo))+
    geom_line(size=4)+
    scale_x_discrete(labels=c( "2" = "Feb",
                               "3"="Mar", "4"="Abr",
                               "5"="Mayo", "6"="Jun",
                               "7"="Jul", "8"="Ago"))+
    scale_color_manual(name = "",values=c("#8931e0","#313cd4"),
                       labels = c("cajeros automáticos", "terminal punto de venta"))+
    labs(title = "Crecimiento de transacciones con respecto a enero en:")+
    ylab("crecimiento de transacciones")+
    theme_dataton()+
    theme(legend.position = "top",
          legend.text = element_text(size = 12))
  return(x)
  
})



########################


tabla_r<- reactive({
  
  x<-tabla_resumen%>%
    mutate(date2=as.Date(as.yearmon(paste(year, "/", month, sep=""), format="%Y/%m")))%>%
    group_by(pais)%>%
    filter(indicador==input$indicador_int & month>=input$month_base_int) %>%
    mutate(temp= ifelse(year==2020 & month==input$month_base_int, empleo, 0)) %>% ## aqui pon el año base 
    mutate(temp2=max(temp)) %>% 
    mutate(idx_ta=((empleo/temp2-1))*100) %>% 
    mutate(dif_ta=empleo-temp2) %>%
    mutate(label_g=sprintf("País: %s, <br> Empleo %s, <br> cambio porcentual %s, cambio absoluto (en miles) %s del mes %s", pais,  empleo, round(idx_ta, digits=2), dif_ta/1000, month))%>%
    ungroup()
  return(x)
})

output$absoluto_int <- renderPlotly({
  if (input$"tipo_int"==0){
    x<-ggplot(data=tabla_r(), aes(x=date2, y=empleo, colour=pais, text=label_g,  group = as.factor(pais))) +
      geom_line(linetype = "solid") +
      theme_classic()+
      xlab("")+
      ylab("Empleo")+
      theme_dataton()+labs(title = "Evolución del empleo:comparación internacional")
  }
  if (input$"tipo_int"==1){
    x<-ggplot(data=tabla_r(), aes(x=date2, y=idx_ta, colour=pais, text=label_g,  group = as.factor(pais))) +
      geom_line(linetype = "solid") +
      theme_classic()+
      xlab("")+
      ylab("% Empleo")+
      theme_dataton()+labs(title = "Evolución del empleo:comparación internacional")
  }
  if (input$"tipo_int"==2){
    x<-ggplot(data=tabla_r(), aes(x=date2, y=dif_ta, text=label_g, colour=pais,  group = as.factor(pais))) +
      geom_line(linetype = "solid") +
      theme_classic()+
      xlab("")+
      ylab("Empleo")+
      theme_dataton()+labs(title = "Evolución del empleo:comparación internacional")
  }
  ggplotly(x, tooltip="text")
})


line_nacional<-reactive({
  x<-mapa1Mex%>%
    filter(r_edad ==0) %>%
    filter(sexo ==0)%>%
    filter(month!=0)%>%
    filter(year>=2018)%>%
    mutate(lab_1=paste("Empleo:", ta_sal, "Fecha:", year, "-", month, sep=" "))%>%
    mutate(lab_1_va=paste("Variación Anual Empleo:", round(var_anual_ta, digits=2), "Fecha:", year, "-", month, sep=" "))%>%
    mutate(lab_1_vm=paste("Variación Mensual Empleo:", round(var_mens_ta, digits=2), "Fecha:", year, "-", month, sep=" "))%>%
    mutate(lab_1_da=paste("Diferencia Anual Empleo:", round(dif_anual_ta, digits=2), "Fecha:", year, "-", month, sep=" "))%>%
    mutate(lab_1_dm=paste("Diferencia Mensual Empleo:", round(dif_mens_ta, digits=2), "Fecha:", year, "-", month, sep=" "))%>%
    mutate(lab_11=paste( "\n Salario : ", round(ingm, digits=2), "Fecha:", year, "-", month, sep=" "))%>%
    mutate(lab_11_va=paste( "\n Variación Anual Salario: ", round(var_anual_ingm, digits=2), "Fecha:", year, "-", month, sep=" "))%>%
    mutate(lab_11_vm=paste( "\n Variación Mensual Salario: ", round(var_mens_ingm, digits=2), "Fecha:", year, "-", month, sep=" "))%>%
    mutate(lab_11_da=paste( "\n Diferencia Anual Salario: ", round(dif_anual_ingm, digits=2), "Fecha:", year, "-", month, sep=" "))%>%
    mutate(lab_11_dm=paste( "\n Diferencia Mensual Salario: ", round(dif_mens_ingm, digits=2), "Fecha:", year, "-", month, sep=" "))
  
  
  
  
  x$date2<-as.Date(as.yearmon(paste(x$year, "/", x$month, sep=""), format="%Y/%m")) 
  
  
  return(x)
})


output$plotEmH1<- renderPlotly({
  
  data1=line_nacional()
  ggplotly(ggplot(data=data1, aes(x=date2, y=ta_sal,  colour=label_g, text=lab_1,  group = as.factor(label_g))) +
             geom_line(linetype = "solid")+
             theme_classic()+
             scale_x_date(date_labels = "20%y-%m", date_breaks = "6 months") + 
             xlab("")+
             scale_y_continuous()  +
             ylab("Empleados")+
             theme_dataton()+labs(title = "Evolución del empleo en México"), tooltip='text')%>%
    layout(legend = list(orientation = "h",  x = 0, y =-.25))
  
  
})

estado <- reactive({
  estado <- estado2 %>% 
    filter(NOM_ENT == input$selEdo)  
})

# Renderizar mapa
output$mapaYun <- renderLeaflet({
  gen_edo(estado())
})



line_edad<-reactive({
  x<-mapa1Mex%>%
    filter(sexo!=0 & r_edad!=0 & r_edad!=1 ) %>%
    filter(month!=0)%>%
    filter(year>=2018)%>%
    filter(sexo==1)%>%
    group_by(sexo, r_edad) %>% 
    mutate(temp= ifelse(year==2020 & month==2, ta_sal, 0)) %>% ## aqui pon el año base 
    mutate(temp2=max(temp)) %>% 
    mutate(idx_ta=((ta_sal/temp2-1))*100) %>% 
    mutate(dif_ta=ta_sal-temp2) %>% 
    ungroup()%>%
    mutate(label_g2=label_g) %>% 
    separate(label_g2, c("A","B"), sep = "([-])")%>%
    filter(year==2020 & month==7 ) 
  
  
  #x$date2<-as.Date(as.yearmon(paste(x$year, "/", x$month, sep=""), format="%Y/%m")) 
  
  
  
  return(x)
})


line_edadm<-reactive({
  y<-mapa1Mex%>%
    filter(sexo!=0 & r_edad!=0 & r_edad!=1 ) %>%
    filter(month!=0)%>%
    filter(year>=2018)%>%
    filter(sexo==2)%>%
    group_by(sexo, r_edad) %>% 
    mutate(temp= ifelse(year==2020 & month==2, ta_sal, 0)) %>% ## aqui pon el año base 
    mutate(temp2=max(temp)) %>% 
    mutate(idx_ta=((ta_sal/temp2-1))*100) %>% 
    mutate(dif_ta=ta_sal-temp2) %>% 
    ungroup()%>%
    mutate(label_g2=label_g) %>% 
    separate(label_g2, c("A","B"), sep = "([-])")%>%
    filter(year==2020 & month==7 ) 
  
  
  #y$date2<-as.Date(as.yearmon(paste(x$year, "/", x$month, sep=""), format="%Y/%m")) 
  
  
  
  return(y)
})


output$plotEdad<- renderPlotly({
  x<- line_edad()
  y<- line_edadm()
  
  plot<-ggplot(y) +
    geom_bar( aes(x=A, y=idx_ta, fill=B, text=A), stat="identity",  alpha=1, position = position_dodge()) +
    geom_bar( data=x,  aes(x=A, y=idx_ta, fill=B,  text=A), stat="identity",  alpha=0.5, position = position_dodge()) +
    theme_classic()+xlab("")+
    ylab("%")+ theme(legend.position="bottom")+
    theme_dataton()+labs(title = "Caída del empleo por edad y sexo: Febrero-Julio") + guides(fill=guide_legend(title=" "))
  ggplotly(plot, tooltip="text")
  
})

line_edad_caida<-reactive({
  x<-mapa1Mex%>%
    filter(sexo!=0 & r_edad!=0 & r_edad!=1 ) %>%
    filter(month!=0)%>%
    filter(year>=2018)%>%
    filter(sexo==1)%>%
    group_by(sexo, r_edad) %>% 
    mutate(temp= ifelse(year==2020 & month==2, ta_sal, 0)) %>% ## aqui pon el año base 
    mutate(temp2=max(temp)) %>% 
    mutate(idx_ta=((ta_sal/temp2-1))*100) %>% 
    mutate(dif_ta=ta_sal-temp2) %>% 
    ungroup()%>%
    mutate(label_g2=label_g) %>% 
    separate(label_g2, c("A","B"), sep = "([-])")%>%
    filter(year==2020 & month==10 ) 
  
  
  #x$date2<-as.Date(as.yearmon(paste(x$year, "/", x$month, sep=""), format="%Y/%m")) 
  
  
  
  return(x)
})


line_edadm_caida<-reactive({
  y<-mapa1Mex%>%
    filter(sexo!=0 & r_edad!=0 & r_edad!=1 ) %>%
    filter(month!=0)%>%
    filter(year>=2018)%>%
    filter(sexo==2)%>%
    group_by(sexo, r_edad) %>% 
    mutate(temp= ifelse(year==2020 & month==7, ta_sal, 0)) %>% ## aqui pon el año base 
    mutate(temp2=max(temp)) %>% 
    mutate(idx_ta=((ta_sal/temp2-1))*100) %>% 
    mutate(dif_ta=ta_sal-temp2) %>% 
    ungroup()%>%
    mutate(label_g2=label_g) %>% 
    separate(label_g2, c("A","B"), sep = "([-])")%>%
    filter(year==2020 & month==8 ) 
  
  
  #y$date2<-as.Date(as.yearmon(paste(x$year, "/", x$month, sep=""), format="%Y/%m")) 
  
  
  
  return(y)
})


output$plotEdad_caida<- renderPlotly({
  x<- line_edad_caida()
  y<- line_edadm_caida()
  
  plot<-ggplot(y) +
    geom_bar( aes(x=A, y=idx_ta, fill=B, text=A), stat="identity",  alpha=1, position = position_dodge()) +
    geom_bar( data=x,  aes(x=A, y=idx_ta, fill=B,  text=A), stat="identity",  alpha=0.5, position = position_dodge()) +
    theme_classic()+xlab("")+
    ylab("%")+ theme(legend.position="bottom")+
    theme_dataton()+labs(title = "Recuperación del empleo por edad y sexo: Julio-Agosto") + guides(fill=guide_legend(title=" "))
  ggplotly(plot, tooltip="text")
  
})



#### Sector



line_sector_caida<-reactive({
  x<-mapa2Mex%>%
    filter(sector!=0 & tam_emp==0 ) %>%
    filter(month!=0)%>%
    filter(year>=2018)%>%
    group_by(sector, tam_emp) %>% 
    mutate(temp= ifelse(year==2020 & month==2, ta_sal, 0)) %>% ## aqui pon el año base 
    mutate(temp2=max(temp)) %>% 
    mutate(idx_ta=((ta_sal/temp2-1))*100) %>% 
    mutate(dif_ta=ta_sal-temp2) %>% 
    ungroup()%>%
    mutate(label_g2=label_g) %>% 
    separate(label_g2, c("A","B"), sep = "([-])")%>%
    filter(year==2020 & month==7 ) 
  
  
  #x$date2<-as.Date(as.yearmon(paste(x$year, "/", x$month, sep=""), format="%Y/%m")) 
  
  
  
  return(x)
})


output$plotsector_caida<- renderPlotly({
  x<- line_sector_caida()
  
  plot<-ggplot(x) +
    geom_bar( aes(x=A, y=idx_ta, fill=B, text=A), stat="identity",  alpha=1, position = position_dodge()) +
    theme_classic()+xlab("")+
    ylab("%")+ theme(legend.position="bottom")+
    theme_dataton()+labs(title = "Caída del empleo por sector: Febrero-Julio") + guides(fill=guide_legend(title=" "))
  ggplotly(plot, tooltip="text")
  
})

line_sector<-reactive({
  x<-mapa2Mex%>%
    filter(sector!=0 & tam_emp==0 ) %>%
    filter(month!=0)%>%
    filter(year>=2018)%>%
    group_by(sector, tam_emp) %>% 
    mutate(temp= ifelse(year==2020 & month==7, ta_sal, 0)) %>% ## aqui pon el año base 
    mutate(temp2=max(temp)) %>% 
    mutate(idx_ta=((ta_sal/temp2-1))*100) %>% 
    mutate(dif_ta=ta_sal-temp2) %>% 
    ungroup()%>%
    mutate(label_g2=label_g) %>% 
    separate(label_g2, c("A","B"), sep = "([-])")%>%
    filter(year==2020 & month==8 ) 
  
  
  #x$date2<-as.Date(as.yearmon(paste(x$year, "/", x$month, sep=""), format="%Y/%m")) 
  
  
  
  return(x)
})


output$plotsector<- renderPlotly({
  x<- line_sector()
  
  plot<-ggplot(x) +
    geom_bar( aes(x=A, y=idx_ta, fill=B, text=A), stat="identity",  alpha=1, position = position_dodge()) +
    theme_classic()+xlab("")+
    ylab("%")+ theme(legend.position="bottom")+
    theme_dataton()+labs(title = "Recuperación del empleo por sector: Julio-Agosto") + guides(fill=guide_legend(title=" "))
  ggplotly(plot, tooltip="text")
  
})



#### tamaño de empresa



line_te_caida<-reactive({
  x<-mapa2Mex%>%
    filter(sector==0 & tam_emp!=0 ) %>%
    filter(month!=0)%>%
    filter(tam_emp!=7)%>%
    filter(year>=2018)%>%
    group_by(sector, tam_emp) %>% 
    mutate(temp= ifelse(year==2020 & month==2, ta_sal, 0)) %>% ## aqui pon el año base 
    mutate(temp2=max(temp)) %>% 
    mutate(idx_ta=((ta_sal/temp2-1))*100) %>% 
    mutate(dif_ta=ta_sal-temp2) %>% 
    ungroup()%>%
    mutate(label_g2=label_g) %>% 
    separate(label_g2, c("B","A"), sep = "([-])")%>%
    filter(year==2020 & month==7 ) 
  
  
  #x$date2<-as.Date(as.yearmon(paste(x$year, "/", x$month, sep=""), format="%Y/%m")) 
  
  
  
  return(x)
})


output$plotte_caida<- renderPlotly({
  x<- line_te_caida()
  
  plot<-ggplot(x) +
    geom_bar( aes(x=A, y=idx_ta, fill=B, text=A), stat="identity",  alpha=1, position = position_dodge()) +
    theme_classic()+xlab("")+
    scale_color_manual(name = " ")+
    ylab("%")+ theme(legend.position="bottom")+
    theme_dataton()+labs(title = "Caída del empleo por tamaño de empresa: Febrero-Julio") + guides(fill=guide_legend(title=" "))
  ggplotly(plot, tooltip="text")
  
})

line_te<-reactive({
  x<-mapa2Mex%>%
    filter(sector==0 & tam_emp!=0 ) %>%
    filter(month!=0)%>%
    filter(tam_emp!=7)%>%
    filter(year>=2018)%>%
    group_by(sector, tam_emp) %>% 
    mutate(temp= ifelse(year==2020 & month==7, ta_sal, 0)) %>% ## aqui pon el año base 
    mutate(temp2=max(temp)) %>% 
    mutate(idx_ta=((ta_sal/temp2-1))*100) %>% 
    mutate(dif_ta=ta_sal-temp2) %>% 
    ungroup()%>%
    mutate(label_g2=label_g) %>% 
    separate(label_g2, c("B","A"), sep = "([-])")%>%
    filter(year==2020 & month==8 ) 
  
  
  #x$date2<-as.Date(as.yearmon(paste(x$year, "/", x$month, sep=""), format="%Y/%m")) 
  
  
  
  return(x)
})


output$plotte<- renderPlotly({
  x<- line_te()
  
  plot<-ggplot(x) +
    geom_bar( aes(x=A, y=idx_ta, fill=B, text=A), stat="identity",  alpha=1, position = position_dodge()) +
    theme_classic()+xlab("")+
    ylab("%")+ theme(legend.position="bottom")+
    theme_dataton()+labs(title = "Recuperación del empleo por tamaño de empresa: Julio-Agosto") + guides(fill=guide_legend(title=" "))
  ggplotly(plot, tooltip="text")
  
})


### Pérdida de empleo por salario minimo

output$plotsa_caida<- renderPlot({

  
  ggplot(perdida_empleo1) +
    geom_bar( aes(x=sal_lab, y=dif_emp), stat="identity",  alpha=1, position = position_dodge()) +
    theme_classic()+xlab("")+
    ylab("Empleo")+ theme(legend.position="bottom")+
    theme_dataton()+labs(title = "Caída del empleo por salario: Febrero-Julio")

  
})



output$plotsa<- renderPlot({
  
  
  ggplot(perdida_empleo2) +
    geom_bar( aes(x=sal_lab, y=dif_emp), stat="identity",  alpha=1, position = position_dodge()) +
    scale_color_manual(name = " ")+
    theme_classic()+xlab("")+
    ylab("Empleo")+ theme(legend.position="bottom")+
    theme_dataton()+labs(title = "Caída del empleo por salario: Febrero-Julio")

  
})

output$pdfviewer <- renderText({
  return(paste('<iframe style="height:600px; width:100%" src="', "Semaforo.pdf", '"></iframe>', sep = ""))
})

}

