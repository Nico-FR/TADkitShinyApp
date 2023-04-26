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
  #load matrix + from + to when chr, res & balanced changed
  ##################################
  observeEvent(list(input$my_balanced, input$my_chr, input$my_res), {
    
    #updates datas (reactiveValues)
    returns$chr_name = input$my_chr
    returns$chr_size = chromosomes.df[chromosomes.df$names == returns$chr_name, 2]
    returns$bin_width = as.numeric(input$my_res)
    
    #message for loading matrix
    nb_bin = round(returns$chr_size/returns$bin_width, digit=0)
    showModal(modalDialog(
      paste0("Loading of ", returns$chr_name," matrix at ", 
             format(returns$bin_width/1e3, scientific=F, big.mark=","), 
             "kb. (i.e. ", nb_bin, "x",  nb_bin, " matrix)."), 
      footer=NULL))
    
    #load matrix
    returns$matrix = cool2matrix(cool.path = mcool.path, chr = returns$chr_name, 
                                 bin.width = returns$bin_width, balance = input$my_balanced)
    
    #close message
    removeModal()
    
    #update slider
    updateSliderInput(session, 
                      "start_end", 
                      min = 1,
                      max =  returns$chr_size,
                      value = c(ifelse(input$start_end[1] < returns$chr_size, input$start_end[1], returns$chr_size),  
                                ifelse(input$start_end[2] <= returns$chr_size, input$start_end[2], returns$chr_size)),
                      step = returns$bin_width)
    
    #bin to read from matrix
    returns$from = input$start_end[1] %/% returns$bin_width + 1
    returns$to = input$start_end[2] %/% returns$bin_width
  })
  ##################################
  ##################################
  ##################################
  ####################################################################
  #update from + to when slider updated
  ##################################
  observeEvent(input$start_end, {
    #bin to read from matrix
    returns$from = input$start_end[1] %/% returns$bin_width + 1
    returns$to = input$start_end[2] %/% returns$bin_width
  })
  ##################################
  ##################################
  ##################################
  ####################################################################
  #subset of the matrix when "matrix + from + to" are updated
  ##################################
  observeEvent(list(returns$from, returns$to, returns$matrix), {
    
    #check correlation between chr and chr_size
    validate(need(
      input$start_end[1] <= returns$chr_size, "updating chr_size..." #if not wait that chr_size is updated
    ))
    validate(need(
      input$start_end[2] <= returns$chr_size, "updating chr_size..." #if not wait that chr_size is updated
    ))
    
    #filter matrix ranges
    mat = Matrix::triu(returns$matrix[returns$from:returns$to, returns$from:returns$to]) 
    mat[Matrix::triu(mat == 0)] <- NA
    returns$subset_matrix = mat
  })
  ##################################
  ##################################
  ##################################
  ####################################################################
  #melt matrix + log + MATplot when "matrix + from + to + log" are updated
  ##################################
  observeEvent(list(input$my_log2, returns$from, returns$to, returns$matrix), {
    
    #melt matrix
    upper_mat = Matrix::summary(Matrix::triu(returns$subset_matrix, 1))
    diag_mat = data.frame(i = 1:nrow(returns$subset_matrix), j = 1:nrow(returns$subset_matrix), x = Matrix::diag(returns$subset_matrix))
    lower_mat = Matrix::summary(Matrix::tril(BiocGenerics::t(returns$subset_matrix), -1)) #data.frame(i = upper_mat$j, j = upper_mat$i, x = upper_mat$x)
    melted_mat = rbind(upper_mat, lower_mat, diag_mat)
    
    #add genomic coordinates
    melted_mat$j = (melted_mat$j + returns$from - 1) * - returns$bin_width + returns$bin_width / 2
    melted_mat$i = (melted_mat$i + returns$from - 1) * returns$bin_width - returns$bin_width / 2
    
    #get log2
    if (input$my_log2) {melted_mat$x = log2(melted_mat$x)}
    returns$melted_subset = melted_mat
  })
  ##################################
  ##################################
  ##################################
  ####################################################################
  #upperDom when "upper_Domain" are loaded
  ##################################  
  observeEvent(list(input$upper_Domain, returns$chr_name), {
    
    validate(need(!is.null(input$upper_Domain), "no upperDomain file"))
    
    input.df = read.table(input$upper_Domain$datapath, h=F, sep="\t")
    
    #check input file
    validate(
      need(length(input.df) >= 3, message = "Please upload bed file format (no header, tab separated)"))
    validate(
      need(is.numeric(input.df[,2]), message = "Column 2 is not numeric"),
      need(is.numeric(input.df[,3]), message = "Column 3 is not numeric"))
    
    #prepare datas
    domains.bed = input.df[,1:3]
    names(domains.bed) = c("chr", "s", "e")
    subset_domains.bed = dplyr::filter(domains.bed, chr == returns$chr_name)
    validate(need(nrow(subset_domains.bed) >= 1, message = "there is no domains for this chr in upperDomain file"))
    returns$upperDom = subset_domains.bed
  })
  ##################################
  ##################################
  ##################################
  ####################################################################
  #lowerDom when "lower_Domain" are loaded
  ##################################  
  observeEvent(list(input$lower_Domain, returns$chr_name), {
    
    validate(need(!is.null(input$lower_Domain), "no lowerDomain file"))
    
    input.df = read.table(input$lower_Domain$datapath, h=F, sep="\t")
    
    #check input file
    validate(
      need(length(input.df) >= 3, message = "Please upload bed file format (no header, tab separated)"))
    validate(
      need(is.numeric(input.df[,2]), message = "Column 2 is not numeric"),
      need(is.numeric(input.df[,3]), message = "Column 3 is not numeric"))
    
    #prepare datas
    domains.bed = input.df[,1:3]
    names(domains.bed) = c("chr", "s", "e")
    subset_domains.bed = dplyr::filter(domains.bed, chr == returns$chr_name)
    validate(need(nrow(subset_domains.bed) >= 1, message = "there is no domains for this chr in lowerDomain file"))
    returns$lowerDom = subset_domains.bed
  })
  ##################################
  ##################################
  ##################################
  ####################################################################
  #Plots if "melted_subset + upperDom + lowerDom" are updated
  ################################## 
  observeEvent(list(returns$melted_subset, returns$upperDom, returns$lowerDom), {
    
    #MAPplot
    returns$MAPplot <- ggplot2::ggplot()+ggplot2::geom_tile(data = returns$melted_subset, ggplot2::aes(x = i, y = j, fill = x))+
      viridis::scale_fill_viridis(na.value = "black", option = "H")+
      ggplot2::scale_x_continuous(labels = scales::unit_format(unit = "Mb", scale = 1e-6), limits = c(input$start_end[1], input$start_end[2]))+
      ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "Mb", scale = 1e-6), limits = c(-input$start_end[2], -input$start_end[1]))+
      ggplot2::coord_fixed()+ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), legend.title = ggplot2::element_blank())+
      ggplot2::ggtitle(paste0(returns$chr_name, " (", format(returns$bin_width/1e3, scientific=F, big.mark=","),"kb)"))
    
    #upperDom
    if (!is.null(returns$upperDom)) {
      
      #split Domains that are cut through the window
      tad <- dplyr::filter(returns$upperDom, e > input$start_end[1], s < input$start_end[2])
      tad$e2 <- ifelse(tad$e >= input$start_end[2], input$start_end[2], tad$e)
      tad$s2 <- ifelse(tad$s <= input$start_end[1], input$start_end[1], tad$s)
      
      returns$MAPplot <- returns$MAPplot+
        ggplot2::geom_segment(data = tad, ggplot2::aes(x = s, y = -s, xend = e2, yend = -s2), color = "red", size = 0.5)+
        ggplot2::geom_segment(data = tad, ggplot2::aes(x = e2, y = -s2, xend = e, yend = -e), color = "red", size = 0.5)
    }
    
    #lowerDom
    if (!is.null(returns$lowerDom)) {
      
      #split Domains that are cut through the window
      tad <- dplyr::filter(returns$lowerDom, e > input$start_end[1], s < input$start_end[2])
      tad$e2 <- ifelse(tad$e >= input$start_end[2], input$start_end[2], tad$e)
      tad$s2 <- ifelse(tad$s <= input$start_end[1], input$start_end[1], tad$s)
      
      returns$MAPplot <- returns$MAPplot+
        ggplot2::geom_segment(data = tad, ggplot2::aes(x = s2, y = -e2, xend = e, yend = -e), color = "red", size = 0.5)+
        ggplot2::geom_segment(data = tad, ggplot2::aes(x = s, y = -s, xend = s2, yend = -e2), color = "red", size = 0.5)
    }
  })
  ##################################
  ##################################
  ##################################

  
  output$render_MATplot <- renderPlot({returns$MAPplot}) 
  
  observe(print(c(returns$from,  returns$to)))
  observe(print(c(input$start_end[1],  input$start_end[2])))
  observe(print(returns$matrix[1:10,1:10]))
  observe(print(head(returns$melted_subset)))
  observe(print(returns$MAPplot))
  
  
  
  
}

##################################
#
##################################
