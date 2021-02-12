# shiny server side code for each call
shinyServer(function(input, output, session){


	#plotting function using ggplot2
  data <- reactive( { 
    a <- data_app %>% filter ( rango_capitas %in% input$rango_capitas ) %>% 
      filter (sector %in% input$sector ) %>% 
      filter (provincia %in% input$provincia ) %>% 
      filter (salario >= input$salario[1] & salario <= input$salario[2]) %>% 
      filter (Region %in% input$Region )%>% 
      filter (Ettp %in% input$Ettp )%>% 
      filter (art_amiga %in% input$art_amiga )%>% 
      filter (ciiu2_desc %in% input$ciiu2_desc )%>% 
      filter (bins_stros_pc %in% input$bins_stros_pc )
    
    a <- a$alicuotaVerdeTopeo
    return(a)
  })
  data1 <- reactive( { 
    a <- data_app %>% filter ( rango_capitas %in% input$rango_capitas ) %>% 
      filter (sector %in% input$sector ) %>% 
      filter (provincia %in% input$provincia ) %>% 
      filter(salario >= input$salario[1] & salario <= input$salario[2]) %>% 
      filter (Region %in% input$Region )%>% 
      filter (Ettp %in% input$Ettp )%>% 
      filter (art_amiga %in% input$art_amiga )%>% 
      filter (ciiu2_desc %in% input$ciiu2_desc )%>% 
      filter (bins_stros_pc %in% input$bins_stros_pc )
    
    a <- a$Variable.Piscys..Cotizador.
    return(a)
    
  })
	output$p <- renderPlotly({
	  
	  fig <- plot_ly(y = data(), type = "box",name="alicuotaVerdeTopeo")
	  fig <- fig %>% add_trace(y = data1(), type = "box",name="Piscys")
	   
	  fig

	})
	
	output$casos <- renderText({ 
	  paste( "Cantidad de cotizaciones: " , length(data1()))
	})
	output$aclaraciones <- renderText({ 
	  paste("Se filtraron las cotizaciones de alicuota modelo mayor a 25, dado que distorcionaba la visualización. Porcentaje de la base filtrado : % ", z)
	  
	  
	})
	
	
	data2 <- reactive( { 
	  a <- data_app_1  %>% 
	    filter ( rango_capitas %in% input$rango_capitas)%>% 
	    filter (sector %in% input$sector ) %>% 
	    filter (provincia %in% input$provincia ) %>% 
	    filter (salario >= input$salario[1] & salario <= input$salario[2]) %>% 
	    filter (Region %in% input$Region )%>% 
	    filter (Ettp %in% input$Ettp )%>% 
	    filter (art_amiga %in% input$art_amiga )%>% 
	    filter (ciiu2_desc %in% input$ciiu2_desc )%>% 
	    filter (bins_stros_pc %in% input$bins_stros_pc )
	  a <- a$dif_piscys_vs_vblecontrato
	  return(a) })
output$pp <- renderPlotly({

	fig1 <- plot_ly(y = data2(), type = "box",name="dif_Piscys_Contrato")
	
	fig1

})

data3 <- reactive( { 
  a <- data_app  %>% 
    filter ( rango_capitas %in% input$rango_capitas)%>% 
    filter (sector %in% input$sector ) %>% 
    filter (provincia %in% input$provincia ) %>% 
    filter (salario >= input$salario[1] & salario <= input$salario[2]) %>% 
    filter (Region %in% input$Region )%>% 
    filter (Ettp %in% input$Ettp )%>% 
    filter (art_amiga %in% input$art_amiga )%>% 
    filter (ciiu2_desc %in% input$ciiu2_desc )%>% 
    filter (bins_stros_pc %in% input$bins_stros_pc )
  a <- a %>% select(CUIT,Variable.Piscys..Cotizador.,alicuotaVerdeTopeo)
  return(a) })
#browser()
output$ppp <- renderPlotly({
  
  fig3 <- plot_ly(data = data3(),
                  x = ~alicuotaVerdeTopeo, 
                  y = ~Variable.Piscys..Cotizador.,
                  text= ~CUIT,
                  type="scatter",
                  source = "ppp",
                  key= ~CUIT
                  ) %>%
    add_segments(x = 0, xend = 30, y = 0, yend = 30) 
   # add_segments(x = 0, xend = 30, y = 3, yend = 28) %>%
  #  add_segments(x = 0, xend = 30, y = -3, yend = 22)
  fig3
  
})


output$casos1 <- renderText({ 
  paste( "Cantidad de cotizaciones: " , length(data2()))
})


  # output$Tabla_Cotizados <- renderTable({
  #   e <- event_data(event = "plotly_click", source = "ppp")
  #   if(length(e_save) == 2 ){
  #   e_save <<- c(e_save,e$key)
  #     e <- data.frame(e_save)
  #     rbind(data_app %>% filter( CUIT ==e[1,1]) %>%
  #             select(CUIT, ciiu2_desc, provincia,alicuotaVerdeTopeo,Variable.Piscys..Cotizador. ),
  #           data_app %>% filter( CUIT ==e[2,1]) %>%
  #             select(CUIT, ciiu2_desc, provincia,alicuotaVerdeTopeo,Variable.Piscys..Cotizador. ))
  #   } else {
  #     e_save <<- NULL
  #     e <- data.frame(e_save)
  # 
  #   }

     
#   #
#
#
     #isolate(print(e))
 # })



output$imageLink <- renderPlotly({
  d <- event_data(event = "plotly_click", source = "ppp")
  if(length(d_save) < 2 ){
   d_save <<- c(d_save,d$key)
   a <- data.frame(d_save)
  } else {
    d_save <<- NULL
    a <- data.frame(d_save)

  }


  
   
  isolate(print(a))
  isolate(print(length(d_save)))
  fig1 = plot_ly(
          data = data_app %>% filter(CUIT == a[1,1]),
          x =  c(~relatividad_especies_ciiu,~relatividad_especies_pcia, ~relatividad_especies_tamano_empresa),
          y = c("Ciiu2" , "Provincia", "tamano_empresa"),
          type = "bar",
          orientation = 'h',
          name = a[1,1],
          text= c(~ciiu2_desc,~provincia,~tamano_empresa),
          color =  I("lightblue"),
          showlegend = FALSE)%>% 
    add_bars(data = data_app %>% filter(CUIT == a[2,1]),
             x =  c(~relatividad_especies_ciiu,~relatividad_especies_pcia, ~relatividad_especies_tamano_empresa),
             y = c("Ciiu2","Provincia", "tamano_empresa"),
             type = "bar",
             orientation = 'h',
             name = a[2,1],
             text= c(~ciiu2_desc,~provincia,~tamano_empresa),
             color =  I("grey"),
             showlegend = FALSE) %>%
    layout(annotations = list(
      text = "Especies",
      font = list(
        family = "Courier New, monospace",
        size = 18,
        color = "black"),
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    ))
  fig2 = plot_ly(
    data = data_app %>% filter(CUIT == a[1,1]),
    x =  c(~relatividad_ilt_ciiu,~relatividad_ilt_pcia,~relatividad_ilt_tamano_empresa),
    y = c("Ciiu2","Provincia","tamano_empresa"),
    type = "bar",
    orientation = 'h',
    name = a[1,1],
    text= c(~ciiu2_desc,~provincia, ~tamano_empresa),
    color =  I("lightblue"),
    showlegend = FALSE)%>% 
    add_bars(data = data_app %>% filter(CUIT == a[2,1]),
             x =  c(~relatividad_ilt_ciiu,~relatividad_ilt_pcia,~relatividad_ilt_tamano_empresa),
             y = c("Ciiu2","Provincia","tamano_empresa"),
             type = "bar",
             orientation = 'h',
             name = a[2,1],
             text= c(~ciiu2_desc,~provincia, ~tamano_empresa),
             color =  I("grey"),
             showlegend = FALSE) %>%
    layout(annotations = list(
      text = "ILT",
      font = list(
        family = "Courier New, monospace",
        size = 18,
        color = "black"),
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    ))
  
  fig3 = plot_ly(
    data = data_app %>% filter(CUIT == a[1,1]),
    x =  c(~relatividad_incap_ciiu,~relatividad_incap_pcia,~relatividad_incap_tamano_empresa),
    y = c("Ciiu2","Provincia","tamano_empresa"),
    type = "bar",
    orientation = 'h',
    name = a[1,1],
    text= c(~ciiu2_desc,~provincia, ~tamano_empresa),
    color =  I("lightblue"),
    showlegend = FALSE)%>% 
    add_bars(data = data_app %>% filter(CUIT == a[2,1]),
             x =  c(~relatividad_incap_ciiu,~relatividad_incap_pcia,~relatividad_incap_tamano_empresa),
             y = c("Ciiu2","Provincia","tamano_empresa"),
             type = "bar",
             orientation = 'h',
             name = a[2,1],
             text= c(~ciiu2_desc,~provincia, ~tamano_empresa),
             color =  I("grey"),
             showlegend = FALSE) %>%
    layout(annotations = list(
      text = "Incapacidades",
      font = list(
        family = "Courier New, monospace",
        size = 18,
        color = "black"),
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    ))
  
  fig4 = plot_ly(
    data = data_app %>% filter(CUIT == a[1,1]),
    x =  c(~relatividad_juicios_ciiu,~relatividad_juicios_pcia,~relatividad_juicios_tamano_empresa),
    y = c("Ciiu2","Provincia","tamano_empresa"),
    type = "bar",
    orientation = 'h',
    name = a[1,1],
    text= c(~ciiu2_desc,~provincia, ~tamano_empresa),
    color =  I("lightblue"),
    showlegend = FALSE)%>% 
    add_bars(data = data_app %>% filter(CUIT == a[2,1]),
             x =  c(~relatividad_juicios_ciiu,~relatividad_juicios_pcia,~relatividad_juicios_tamano_empresa),
             y = c("Ciiu2","Provincia","tamano_empresa"),
             type = "bar",
             orientation = 'h',
             name = a[2,1],
             text= c(~ciiu2_desc,~provincia, ~tamano_empresa),
             color =  I("grey"),
             showlegend = FALSE) %>%
    layout(annotations = list(
      text = "Juicios",
      font = list(
        family = "Courier New, monospace",
        size = 18,
        color = "black"),
      xref = "paper",
      yref = "paper",
      yanchor = "bottom",
      xanchor = "center",
      align = "center",
      x = 0.5,
      y = 1,
      showarrow = FALSE
    ))
   fig <- subplot(fig4,fig3, fig2, fig1)
   fig
})




}
)

