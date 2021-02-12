# UI for app
shinyUI(pageWithSidebar(
	# title
	headerPanel("Boxplot Pricing"),
	
	#input
	sidebarPanel
	(
	  selectizeInput("rango_capitas", "Segmento", choices = unique(data_app$rango_capitas), multiple = TRUE, selected =unique(data_app$rango_capitas),
	                 options = list(
	                   plugins = list("remove_button")
	                 )
	                 ),
	  pickerInput("sector","Sector", choices=unique(data_app$sector), options = list(`actions-box` = TRUE),multiple = T,selected =unique(data_app$sector) )
	  ,
	  pickerInput("ciiu2_desc","Sector_2", choices=unique(data_app$ciiu2_desc), options = list(`actions-box` = TRUE),multiple = T,selected =unique(data_app$ciiu2_desc))
	  ,
	  pickerInput("bins_stros_pc","bins_stros_pc", choices=unique(data_app$bins_stros_pc), options = list(`actions-box` = TRUE),multiple = T,selected ="b. 2-6")
	  ,
	  pickerInput("provincia","Provincia", choices=unique(data_app$provincia), options = list(`actions-box` = TRUE),multiple = T,selected =unique(data_app$provincia))
	  ,
	  pickerInput("Region","Region", choices=unique(data_app$Region), options = list(`actions-box` = TRUE),multiple = T,selected =unique(data_app$Region))
	  ,
	  selectizeInput("Ettp", "Etp", choices = unique(data_app$Ettp), multiple = TRUE, selected =unique(data_app$Ettp),
	                 options = list(
	                   plugins = list("remove_button")
	                 )
	  ),
	  selectizeInput("art_amiga", "art_amiga", choices = unique(data_app$art_amiga), multiple = TRUE, selected =unique(data_app$art_amiga),
	                 options = list(
	                   plugins = list("remove_button")
	                 )
	  ),
	  
	  sliderInput(inputId = "salario",
	              label = "Salario prom",
	              min = 0,
	              max = 130000,
	              value = c(0, 130000),
	              pre = "$")
	
		
	),	

	# output				
	mainPanel( tags$style(type="text/css",
	                      ".shiny-output-error { visibility: hidden; }",
	                      ".shiny-output-error:before { visibility: hidden; }"),
	  tabsetPanel(
	    tabPanel("Modelo vs Piscys", "",
	  plotlyOutput("p") ,
	  textOutput("casos"),
	  textOutput("aclaraciones")), 
	  
	 tabPanel("Scatter", "",
	           plotlyOutput("ppp"),
	          br(),
	          br(),
	          #tableOutput("Tabla_Cotizados"),
	          plotlyOutput("imageLink")
	  ),
	  tabPanel("Piscys vs Vble_contrato", "",
	           plotlyOutput("pp"),
	           textOutput("casos1")
	  )
	)
)
)
)
