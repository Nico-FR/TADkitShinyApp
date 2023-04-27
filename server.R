#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


function(input, output, session) {
  
  
  ####################################################################
  #input values
  ##################################
  returns <- reactiveValues(
    chr.lst = NULL,
    chr_name =  NULL,
    chr_size = NULL,
    bin_width =  NULL,
    matrix = NULL,
    from = NULL,
    to = NULL,
    subset_matrix = NULL,
    melted_subset = NULL,
    upperDom = NULL,
    lowerDom = NULL,
    MATplot = NULL
  )
  ##################################
  ##################################
  ####################################################################
  #mcool files path start the apps
  ##################################
  root = c(wd='.') #getVolumes()
  observe({  
    shinyFileChoose(input, "Btn_GetFile", roots = root, session = session, filetypes = c("", "mcool"))
  })
  output$chr.lst <- renderPrint({1:5})
  observeEvent(input$Btn_GetFile, {
    file_selected<-parseFilePaths(root, input$Btn_GetFile)
    output$txt_file <- renderText(as.character(file_selected$datapath))
  })

  
  
}