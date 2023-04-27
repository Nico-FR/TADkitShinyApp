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
    mcool.path = NULL,
    bin_width.lst = NULL,
    chr.df = NULL,
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
  #select mcool file
  ##################################
  root = c(wd='.') #getVolumes()
  observe({  
    shinyFileChoose(input, "Btn_GetFile", roots = root, session = session, filetypes = c("", "mcool"))
  })
  output$chr.lst <- renderPrint({1:5})
  observeEvent(input$Btn_GetFile, {
    
    #mcool path
    file_selected = parseFilePaths(root, input$Btn_GetFile)
    returns$mcool.path = as.character(file_selected$datapath)
    output$txt_file <- renderText(returns$mcool.path)
  })
  ##################################
  ##################################
  ####################################################################
  #load metadata from mcool file when mcool.path changed
  ##################################  
  observeEvent(req(returns$mcool.path),{
    
    #bin_width.lst
    returns$bin_width.lst = (rhdf5::h5ls(returns$mcool.path) %>% filter(group == "/resolutions"))$name %>% 
      as.numeric() %>% sort(decreasing = TRUE) %>% format(scientific = FALSE)
    updateSelectInput(session, "my_res", choices = returns$bin_width.lst)
    
    #chr.df
    returns$chr.df = data.frame(
      names = rhdf5::h5read(file = returns$mcool.path, name = paste0("resolutions/", mcool.resolutions[1],"/chroms/name")),
      lengths = rhdf5::h5read(file = returns$mcool.path, name = paste0("resolutions/", mcool.resolutions[1],"/chroms/length")))
    updateSelectInput(session, "my_chr", choices = returns$chr.df$names)
    observe(print(head(returns$chr.df)))
    observe(print(head(returns$chr.df$names)))
    observe(print(returns$mcool.path))
  })
  
  ##################################
  ##################################
  ####################################################################
  #load matrix + from + to when chr, res & balanced changed
  ##################################
  observeEvent(list(req(returns$mcool.path), input$my_balanced, input$my_chr, input$my_res), {
    
    #updates datas (reactiveValues)
    returns$chr_name = input$my_chr
    returns$chr_size = returns$chr.df[returns$chr.df$names == returns$chr_name, 2] 
    returns$bin_width = as.numeric(input$my_res)
    
    #message for loading matrix
    nb_bin = round(returns$chr_size/returns$bin_width, digit=0)
    showModal(modalDialog(
      paste0("Loading of ", returns$chr_name," matrix at ", 
             format(returns$bin_width/1e3, scientific=F, big.mark=","), 
             "kb. (i.e. ", nb_bin, "x",  nb_bin, " matrix)."), 
      footer=NULL))
    
    #load matrix
    returns$matrix = cool2matrix(cool.path = returns$mcool.path, chr = returns$chr_name, 
                                 bin.width = returns$bin_width, balance = input$my_balanced)
    
    #close message
    removeModal()

  })
  ##################################
  ##################################
  ##################################
  
  
  
}