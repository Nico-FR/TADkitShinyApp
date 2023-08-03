#' @title shiny application to plot matrices
#'
#' @import shiny
#' @import rhdf5
#' @importFrom methods as
#' @importFrom magrittr %>%
#' @importFrom BiocGenerics t
#' @import shinyFiles
#' @import shinycssloaders
#' @import viridis
#' @import ggplot2
#' @import Matrix
#' @import dplyr
#' 
#'
#' @export


TADkitShinyApp <- function() {
  
  #set all dataframes names (ie columns) as NULL (to avoid warnings: "no visible binding for global variable")
  chr <- chrom <- e <- e2 <- group <- i <- j <- s <- s2 <- x <- NULL
  
  ui <- shiny::fluidPage(
    
    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        
        shiny::tabsetPanel(
          shiny::tabPanel("(m)MATplot", 
                          shiny::conditionalPanel(condition = "input.tabselected==1",
                                    #mcool file path
                                    shinyFiles::shinyFilesButton("Btn_GetFile", "Choose .mcool file" ,
                                                                 title = "Please select a mcool file:", multiple = FALSE,
                                                                 buttonType = "default", class = NULL)
                                    ),
                          shiny::conditionalPanel(condition = "input.tabselected==2",
                                                  #mcool file path
                                                  shinyFiles::shinyFilesButton("Btn_GetFile2", "Choose 2nd .mcool file (upper)" ,
                                                                               title = "Please select a mcool file:", multiple = FALSE,
                                                                               buttonType = "default", class = NULL)
                   ),
                   #add horizontal line
                   shiny::hr(style="height:5px;background:#000000;"),
                   
                   #chr list
                   shiny::selectInput("my_chr", "chromosome", choices = NULL),
                   
                   #bin width list
                   shiny::selectInput("my_res", "bin width", choices = NULL),
                   
                   #add horizontal line
                   shiny::hr(style="height:5px;background:#000000;"),
                   
                   #balanced box
                   shiny::checkboxInput("my_balanced", "balanced counts", value = FALSE),
                   
                   #scale colors
                   shiny::selectInput("balanced_name", "balancing type", choices = NULL),
                   
                   #log2 box
                   shiny::checkboxInput("my_log2", "log2(counts)", value = TRUE),
                   
                   #scale colors
                   shiny::selectInput("scale_colors", "scale color",
                               choices = c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"), 
                               selected = "turbo"),
                   
                   #plot size
                   numericInput("width", "plot size:", 800, min = 100, max = 8000, step = 100),
                   
                   #add horizontal line
                   shiny::hr(style="height:5px;background:#000000;"),
                   
                   ###########################
                   #Domain files
                   ###########################
                   #upper Domains
                   shiny::fileInput("upper_Domain", "Upper Domains File",
                             multiple = FALSE,
                             accept = c(".bed",".txt")),
                   
                   #lower Domains
                   shiny::fileInput("lower_Domain", "lower Domains File",
                             multiple = FALSE,
                             accept = c(".bed",".txt", ".csv")),
                   
                   #reset domains files
                   shiny::actionButton('reset', 'Reset Domains files'),
                   
                   #add horizontal line
                   shiny::hr(style="height:5px;background:#000000;"),
                   
                   ###########################
                   #Bed & bedgraphs
                   ###########################
                   #bedgraphs
                   shinyFiles::shinyFilesButton("bedgraphs1", "Choose .bedgraph files set 1" ,
                                                title = "Select bedgraph files (no header, tab separated):", multiple = TRUE,
                                                buttonType = "default", class = NULL),
                   shinyFiles::shinyFilesButton("bedgraphs2", "Choose .bedgraph files set 2" ,
                                                title = "Please select bedgraph files (no header, tab separated):", multiple = TRUE,
                                                buttonType = "default", class = NULL),
                   shinyFiles::shinyFilesButton("beds", "Choose .bed files" ,
                                                title = "Please select bed files (no header, tab separated):", multiple = TRUE,
                                                buttonType = "default", class = NULL),
                   
                   #add horizontal line
                   shiny::hr(style="height:5px;background:#000000;"),
                   
                   #begraphs offset
                   numericInput("offset_bg1", "left offset (bedgraph set 1):", 0.07, min = -1, max = 4, step = 0.01),
                   numericInput("offset_bg2", "left offset (bedgraph set 2):", 0.07, min = -1, max = 4, step = 0.01),
                   numericInput("offset_bed", "left offset (bed):", 0.07, min = -1, max = 4, step = 0.01),
                   numericInput("annot.col", "bed: column number with annotations", NA, min = 1, max = 20, step = 1),
                   
                   #add horizontal line
                   shiny::hr(style="height:5px;background:#000000;"),
                   
                   #downloadplot
                   shiny::downloadButton("download_MATplot", label = "MATplot"),
                   shiny::downloadButton("download_mMATplot", label = "mMATplot"),
                   
                   #clear session
                   shiny::actionButton("reload", "Clear session"),
          )
        ),
        
        
        
        
      ),
      
      ####################################################################
      #MainPanel
      ################################## 
      shiny::mainPanel(
        shiny::sliderInput("start_end","range:", 
                    min = 1, max =  2, 
                    value = c(1, 2), 
                    width = "100%", sep=",", post="bp"),
        #TABset
        shiny::tabsetPanel(type = "tabs",
                    
                    #MATplot
                    shiny::tabPanel("MATplot", value = 1, align = "center",
                                    
                             # mcool path txt
                             shiny::verbatimTextOutput("txt_mcoolfile"),
                             
                             #MATplot
                             shiny::plotOutput("render_MATplot", inline = TRUE, width = "auto", height = "800px") %>% shinycssloaders::withSpinner(),
                             
                             #BGplot1
                             shiny::plotOutput("render_BGplot1", inline = TRUE, width = "auto", height = "300px") %>% shinycssloaders::withSpinner(),
                             
                             #BGplot2
                             shiny::plotOutput("render_BGplot2", inline = TRUE, width = "auto", height = "300px") %>% shinycssloaders::withSpinner(),
                             
                             #BEDplot
                             shiny::plotOutput("render_BEDplot", inline = TRUE, width = "auto", height = "400px") %>% shinycssloaders::withSpinner(),
                            
                    ),
                    
                    #mMATplot
                    shiny::tabPanel("mMATplot", value = 2, align = "center",
                                    
                                    shiny::verbatimTextOutput("txt_mcoolfile2"),
                             
                                    shiny::plotOutput("render_mMATplot", inline = TRUE, width = "auto", height = "800px") %>% shinycssloaders::withSpinner(),
                                    
                                    shiny::plotOutput("render_BGplot1B", inline = TRUE, width = "auto", height = "800px") %>% shinycssloaders::withSpinner(),
                                    
                                    shiny::plotOutput("render_BGplot2B", inline = TRUE, width = "auto", height = "800px") %>% shinycssloaders::withSpinner(),
                                    
                                    shiny::plotOutput("render_BEDplotB", inline = TRUE, width = "auto", height = "800px") %>% shinycssloaders::withSpinner(),
                                    ),
                    id = "tabselected"
        )
        
        
      )
    )
  )
  
  server <- function(input, output, session) {
    
    ####################################################################
    #input values
    ##################################
    returns <- shiny::reactiveValues(
      mcool.path = NULL,
      bin_width.lst = NULL,
      chr.df = NULL,
      chr_name =  NULL,
      chr_size = NULL,
      bin_width =  NULL,
      balanced_name = NULL,
      matrix = NULL,
      from = NULL,
      to = NULL,
      subset_matrix = NULL,
      melted_subset = NULL,
      upperDom = NULL,
      lowerDom = NULL,
      MATplot = NULL,
      mcool.path2 = NULL,
      matrix2 = NULL,
      subset_matrix2 = NULL,
      melted_subset2 = NULL,
      mMATplot = NULL,
      bedgraph1.df = NULL,
      BGplot1 = NULL,
      bedgraph2.df = NULL,
      BGplot2 = NULL,
      bed.df = NULL,
      BEDplot = NULL
    )
    widthSize <- function() {
      input$width
    }
    ##################################
    ##################################
    ####################################################################
    #select and read metadata of mcool file
    ##################################
    
    root = shinyFiles::getVolumes() #getVolumes() c(wd='.')
    shiny::observe({  
      shinyFiles::shinyFileChoose(input, "Btn_GetFile", roots = root, session = session, filetypes = c("", "mcool"))
    })
    shiny::observeEvent(list(input$Btn_GetFile), {
      
      #mcool path
      file_selected = shinyFiles::parseFilePaths(root, input$Btn_GetFile)
      if (nrow(file_selected)) {
        returns$mcool.path = as.character(file_selected$datapath)
        output$txt_mcoolfile <- shiny::renderText(returns$mcool.path)
        
        #bin_width.lst
        returns$bin_width.lst = (rhdf5::h5ls(returns$mcool.path) %>% dplyr::filter(group == "/resolutions"))$name %>% 
          as.numeric() %>% sort(decreasing = TRUE) %>% format(scientific = FALSE)
        shiny::updateSelectInput(session, "my_res", choices = returns$bin_width.lst, selected = returns$bin_width.lst[1])
        #validate(shiny::need(input$my_res != "", message = "resolution loading..."))
        
        #chr.df
        returns$chr.df = data.frame(
          names = rhdf5::h5read(file = returns$mcool.path, name = paste0("resolutions/", returns$bin_width.lst[1],"/chroms/name")),
          lengths = rhdf5::h5read(file = returns$mcool.path, name = paste0("resolutions/", returns$bin_width.lst[1],"/chroms/length")))
        shiny::updateSelectInput(session, "my_chr", choices = returns$chr.df$names, selected = returns$chr.df$names[1])
        shiny::updateSliderInput(session, 
                          "start_end", 
                          min = 1,
                          max =  returns$chr.df$lengths[1],
                          value = c(1, returns$chr.df$lengths[1]),
                          step = returns$bin_width.lst[1])
        
        #balanced_name
        tmp = as.data.frame(rhdf5::h5read(file = returns$mcool.path, name = paste0("resolutions/", returns$bin_width.lst[1],"/bins"))) %>% names
        balanced_name.lst = tmp[!tmp %in% c("start", "end", "chrom")] 
        returns$balanced_name = balanced_name.lst
        shiny::updateSelectInput(session, "balanced_name", choices = returns$balanced_name, 
                          selected = ifelse(match('weight', balanced_name.lst) %>% is.na, 
                                            returns$balanced_name[1],
                                            "weight") #add "weigth" by default (if it exists)
        )
      }
    })
    
    ##################################
    ##################################
    ####################################################################
    #load matrix + from + to when chr, res, balanced changed
    ##################################
    shiny::observeEvent(list(returns$mcool.path, input$my_balanced, input$my_chr, input$my_res, input$balanced_name), {
      
      #updates metadatas (reactiveValues)
      returns$chr_name = input$my_chr
      returns$chr_size = returns$chr.df[returns$chr.df$names == returns$chr_name, 2]
      returns$bin_width = as.numeric(input$my_res)
      
      #check bin_width & my_chr are not equal to ""
      shiny::validate(
        shiny::need(returns$chr_name != "", message = "loading mcool metadatas..."),
        shiny::need(returns$bin_width != "", message = "loading mcool metadatas..."))
      
      #message for loading matrix
      nb_bin = round(returns$chr_size/returns$bin_width, digits = 0)
      shiny::showModal(shiny::modalDialog(
        paste0("Loading of ", returns$chr_name," matrix at ", 
               format(returns$bin_width/1e3, scientific=F, big.mark=","), 
               "kb. (i.e. ", nb_bin, "x",  nb_bin, " bins)."), 
        footer=NULL))
      
      #load matrix
      returns$matrix = cool2matrix(cool.path = returns$mcool.path, chr = returns$chr_name, 
                                   bin.width = returns$bin_width, balance = input$my_balanced,
                                   balancing_name = input$balanced_name)
      
      #close message
      shiny::removeModal()
      
      #update slider
      shiny::updateSliderInput(session, 
                        "start_end", 
                        min = 1,
                        max =  returns$chr_size,
                        value = c(ifelse(input$start_end[1] < returns$chr_size, input$start_end[1], returns$chr_size),  
                                  ifelse(input$start_end[2] <= returns$chr_size, input$start_end[2], returns$chr_size)),
                        step = returns$bin_width)
      
      #window (bins) to read from matrix
      returns$from = input$start_end[1] %/% returns$bin_width + 1
      returns$to = input$start_end[2] %/% returns$bin_width
    })
    ##################################
    ##################################
    ##################################
    ####################################################################
    #update from + to when slider updated
    ##################################
    shiny::observeEvent(input$start_end, {
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
    shiny::observeEvent(list(returns$from, returns$to, returns$matrix), {
      
      #check correlation between chr and chr_size
      shiny::validate(shiny::need(
        input$start_end[1] <= returns$chr_size, "updating chr_size..." #if not wait that chr_size is updated
      ))
      shiny::validate(shiny::need(
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
    shiny::observeEvent(list(input$my_log2, returns$from, returns$to, returns$matrix), {
      
      #for some reason it start this event with returns$subset_matrix = NULL
      shiny::validate(shiny::need(!is.null(returns$subset_matrix), message = "loading matrix..."))
      
      #melt matrix
      upper_mat = Matrix::summary(Matrix::triu(returns$subset_matrix, 1))
      diag_mat = data.frame(i = 1:nrow(returns$subset_matrix), j = 1:nrow(returns$subset_matrix), x = Matrix::diag(returns$subset_matrix))
      lower_mat = data.frame(i = upper_mat$j, j = upper_mat$i, x = upper_mat$x) #Matrix::summary(Matrix::tril(BiocGenerics::t(returns$subset_matrix), -1)) #data.frame(i = upper_mat$j, j = upper_mat$i, x = upper_mat$x)
      melted_mat = rbind(upper_mat, lower_mat, diag_mat)
      
      #add genomic coordinates
      melted_mat$j = (melted_mat$j + returns$from - 1) * returns$bin_width - returns$bin_width / 2
      melted_mat$i = (melted_mat$i + returns$from - 1) * - returns$bin_width + returns$bin_width / 2
      
      #get log2
      if (input$my_log2) {melted_mat$x = log2(melted_mat$x)}
      returns$melted_subset = melted_mat
    })
    ##################################
    ##################################
    ##################################
    ####################################################################
    #upperDom when "upper_Domain" are loaded and chr_name changed
    ##################################  
    shiny::observeEvent(list(input$upper_Domain, returns$chr_name), {
      
      shiny::validate(shiny::need(!is.null(input$upper_Domain), "no upperDomain file"))
      
      input.df = utils::read.table(input$upper_Domain$datapath, h=F, sep="\t")
      
      #check input file
      shiny::validate(
        shiny::need(length(input.df) >= 3, message = "Please upload bed file format (no header, tab separated)"))
      shiny::validate(
        shiny::need(is.numeric(input.df[,2]), message = "Column 2 is not numeric"),
        shiny::need(is.numeric(input.df[,3]), message = "Column 3 is not numeric"))
      
      #prepare datas
      domains.bed = input.df[,1:3]
      names(domains.bed) = c("chr", "s", "e")
      subset_domains.bed = dplyr::filter(domains.bed, chr == returns$chr_name)
      shiny::validate(shiny::need(nrow(subset_domains.bed) >= 1, message = "there is no domains for this chr in upperDomain file"))
      returns$upperDom = subset_domains.bed
    })
    ##################################
    ##################################
    ##################################
    ####################################################################
    #lowerDom when "lower_Domain" are loaded and chr_name changed
    ##################################  
    shiny::observeEvent(list(input$lower_Domain, returns$chr_name), {
      
      shiny::validate(shiny::need(!is.null(input$lower_Domain), "no lowerDomain file"))
      
      input.df = utils::read.table(input$lower_Domain$datapath, h=F, sep="\t")
      
      #check input file
      shiny::validate(
        shiny::need(length(input.df) >= 3, message = "Please upload bed file format (no header, tab separated)"))
      shiny::validate(
        shiny::need(is.numeric(input.df[,2]), message = "Column 2 is not numeric"),
        shiny::need(is.numeric(input.df[,3]), message = "Column 3 is not numeric"))
      
      #prepare datas
      domains.bed = input.df[,1:3]
      names(domains.bed) = c("chr", "s", "e")
      subset_domains.bed = dplyr::filter(domains.bed, chr == returns$chr_name)
      shiny::validate(shiny::need(nrow(subset_domains.bed) >= 1, message = "there is no domains for this chr in lowerDomain file"))
      returns$lowerDom = subset_domains.bed
    })
    ##################################
    ##################################
    ##################################
    ####################################################################
    #Plots if "melted_subset + upperDom + lowerDom" are updated
    ################################## 
    shiny::observeEvent(list(returns$melted_subset, returns$upperDom, returns$lowerDom, input$scale_colors), {
      
      #check
      shiny::validate(shiny::need(!is.null(returns$melted_subset), message = "loading matrix..."))
      
      #MATplot
      returns$MATplot <- ggplot2::ggplot()+ggplot2::geom_tile(data = returns$melted_subset, ggplot2::aes(y = i, x = j, fill = x))+
        viridis::scale_fill_viridis(na.value = "black", option = input$scale_colors)+
        ggplot2::scale_x_continuous(labels = scales::unit_format(unit = "Mb", scale = 1e-6), limits = c(input$start_end[1], input$start_end[2]))+
        ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "Mb", scale = 1e-6), limits = c(-input$start_end[2], -input$start_end[1]))+
        ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), legend.title = ggplot2::element_blank(), legend.position="top")+
        ggplot2::ggtitle(paste0(returns$chr_name, " (", format(returns$bin_width/1e3, scientific=F, big.mark=","),"kb)"))
      
      #upperDom
      if (!is.null(returns$upperDom)) {
        
        #split Domains that are cut through the window
        tad <- dplyr::filter(returns$upperDom, e > input$start_end[1], s < input$start_end[2])
        tad$e2 <- ifelse(tad$e >= input$start_end[2], input$start_end[2], tad$e)
        tad$s2 <- ifelse(tad$s <= input$start_end[1], input$start_end[1], tad$s)
        
        returns$MATplot <- returns$MATplot+
          ggplot2::geom_segment(data = tad, ggplot2::aes(x = s, y = -s, xend = e2, yend = -s2), color = "red", size = 0.5)+
          ggplot2::geom_segment(data = tad, ggplot2::aes(x = e2, y = -s2, xend = e, yend = -e), color = "red", size = 0.5)
      }
      
      #lowerDom
      if (!is.null(returns$lowerDom)) {
        
        #split Domains that are cut through the window
        tad <- dplyr::filter(returns$lowerDom, e > input$start_end[1], s < input$start_end[2])
        tad$e2 <- ifelse(tad$e >= input$start_end[2], input$start_end[2], tad$e)
        tad$s2 <- ifelse(tad$s <= input$start_end[1], input$start_end[1], tad$s)
        
        returns$MATplot <- returns$MATplot+
          ggplot2::geom_segment(data = tad, ggplot2::aes(x = s2, y = -e2, xend = e, yend = -e), color = "red", size = 0.5)+
          ggplot2::geom_segment(data = tad, ggplot2::aes(x = s, y = -s, xend = s2, yend = -e2), color = "red", size = 0.5)
      }
    })
    ##################################
    ##################################
    ##################################
    ####################################################################
    #reset domains files
    ################################## 
    shiny::observeEvent(input$reset, {
      returns$lowerDom = NULL
      returns$upperDom = NULL
    })
    ##################################
    ##################################
    ##################################
    ####################################################################
    #renderPlot
    ################################## 
    output$render_MATplot <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(returns$MATplot), message = "start by uploading a mcool file to plot a symetrical matrix"))
      returns$MATplot}, width = widthSize, height = widthSize) 
    ##################################
    ##################################
    ##################################
    ####################################################################
    #download plot
    ################################## 
    output$download_MATplot <- shiny::downloadHandler(
      filename = function() {
        paste0(returns$chr_name, "_", input$my_res, "_", input$start_end[1], "-", input$start_end[2],".png")},
      content = function(file) {
        ggplot2::ggsave(file, plot = returns$MATplot, device = "png")
      }
    )
    output$download_mMATplot <- shiny::downloadHandler(
      filename = function() {
        paste0(returns$chr_name, "_", input$my_res, "_", input$start_end[1], "-", input$start_end[2],".png")},
      content = function(file) {
        ggplot2::ggsave(file, plot = returns$mMATplot, device = "png")
      }
    )
    ##################################
    ##################################
    ##################################
    ####################################################################
    #mcool2 file path 
    ##################################
    shiny::observe({  
      shinyFiles::shinyFileChoose(input, "Btn_GetFile2", roots = root, defaultPath = returns$mcool.path, session = session, filetypes = c("", "mcool"))
    })
    
    ##################################
    ##################################
    ##################################
    ####################################################################
    #unload second mcool files
    ################################## 
    shiny::observeEvent(input$reload, {
      session$reload()
    })
    ##################################
    ##################################
    ####################################################################
    #load matrix2 
    ##################################
    shiny::observeEvent(list(input$Btn_GetFile2, input$my_balanced, input$my_chr, input$my_res, input$balanced_name), {
      
      
      #mcool path
      file_selected2 = shinyFiles::parseFilePaths(root, input$Btn_GetFile2)
      
      shiny::validate(shiny::need(nrow(file_selected2) > 0, message = "load the second matrix."))
      returns$mcool.path2 = as.character(file_selected2$datapath)
      output$txt_mcoolfile2 <- shiny::renderText(returns$mcool.path2)
      
      #message for loading matrix
      shiny::showModal(shiny::modalDialog(
        paste0("Loading of the second matrix."), 
        footer=NULL))
      
      #load matrix
      returns$matrix2 = cool2matrix(cool.path = returns$mcool.path2, chr = returns$chr_name, 
                                    bin.width = returns$bin_width, balance = input$my_balanced,
                                    balancing_name = input$balanced_name)
      
      #close message
      shiny::removeModal()
    })
    ####################################################################
    #subset & melt of matrix2 when "matrix2 + from + to + log2" are updated
    ##################################
    shiny::observeEvent(list(returns$from, returns$to, returns$matrix2, input$my_log2), {
      
      #check correlation between chr and chr_size
      shiny::validate(shiny::need(
        input$start_end[1] <= returns$chr_size, "updating chr_size..." #if not wait that chr_size is updated
      ))
      shiny::validate(shiny::need(
        input$start_end[2] <= returns$chr_size, "updating chr_size..." #if not wait that chr_size is updated
      ))
      shiny::validate(shiny::need(
        !is.null(returns$matrix2), "waiting mcool..." 
      ))
      shiny::validate(shiny::need(
        returns$chr_name == input$my_chr, "wainting loading matrix2..."
      ))
      
      #filter matrix ranges
      mat = Matrix::triu(returns$matrix2[returns$from:returns$to, returns$from:returns$to]) 
      mat[Matrix::triu(mat == 0)] <- NA
      returns$subset_matrix2 = mat
      
      #melt upper matrix
      upper_mat = Matrix::summary(Matrix::triu(returns$subset_matrix2, 1))
      lower_mat = Matrix::summary(Matrix::tril(BiocGenerics::t(returns$subset_matrix), -1)) 
      
      #merge melted upper and lower mat
      melted_mat2 = rbind(upper_mat, lower_mat)
      
      #add genomic coordinates
      melted_mat2$j = (melted_mat2$j + returns$from - 1) * returns$bin_width - returns$bin_width / 2
      melted_mat2$i = (melted_mat2$i + returns$from - 1) * - returns$bin_width + returns$bin_width / 2
      
      #get log2
      if (input$my_log2) {melted_mat2$x = log2(melted_mat2$x)}
      returns$melted_subset2 = melted_mat2
    })
    ##################################
    ##################################
    ##################################
    ####################################################################
    #Plots if "melted_subset2 + upperDom + lowerDom" are updated
    ################################## 
    shiny::observeEvent(list(returns$melted_subset2, returns$upperDom, returns$lowerDom, input$scale_colors), {
      
      #check
      shiny::validate(shiny::need(!is.null(returns$melted_subset2), message = "loading matrix..."))
      shiny::validate(shiny::need(
        returns$chr_name == input$my_chr, "waiting matrix2..."
      ))
      
      #MATplot
      returns$mMATplot <- ggplot2::ggplot()+ggplot2::geom_tile(data = returns$melted_subset2, ggplot2::aes(y = i, x = j, fill = x))+
        viridis::scale_fill_viridis(na.value = "black", option = input$scale_colors)+
        ggplot2::scale_x_continuous(labels = scales::unit_format(unit = "Mb", scale = 1e-6), limits = c(input$start_end[1], input$start_end[2]))+
        ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "Mb", scale = 1e-6), limits = c(-input$start_end[2], -input$start_end[1]))+
        ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), legend.title = ggplot2::element_blank(), legend.position = "top")+
        ggplot2::ggtitle(paste0(returns$chr_name, " (", format(returns$bin_width/1e3, scientific=F, big.mark=","),"kb)"))
      
      #upperDom
      if (!is.null(returns$upperDom)) {
        
        #split Domains that are cut through the window
        tad <- dplyr::filter(returns$upperDom, e > input$start_end[1], s < input$start_end[2])
        tad$e2 <- ifelse(tad$e >= input$start_end[2], input$start_end[2], tad$e)
        tad$s2 <- ifelse(tad$s <= input$start_end[1], input$start_end[1], tad$s)
        
        returns$mMATplot <- returns$mMATplot+
          ggplot2::geom_segment(data = tad, ggplot2::aes(x = s, y = -s, xend = e2, yend = -s2), color = "red", size = 0.5)+
          ggplot2::geom_segment(data = tad, ggplot2::aes(x = e2, y = -s2, xend = e, yend = -e), color = "red", size = 0.5)
      }
      
      #lowerDom
      if (!is.null(returns$lowerDom)) {
        
        #split Domains that are cut through the window
        tad <- dplyr::filter(returns$lowerDom, e > input$start_end[1], s < input$start_end[2])
        tad$e2 <- ifelse(tad$e >= input$start_end[2], input$start_end[2], tad$e)
        tad$s2 <- ifelse(tad$s <= input$start_end[1], input$start_end[1], tad$s)
        
        returns$mMATplot <- returns$mMATplot+
          ggplot2::geom_segment(data = tad, ggplot2::aes(x = s2, y = -e2, xend = e, yend = -e), color = "red", size = 0.5)+
          ggplot2::geom_segment(data = tad, ggplot2::aes(x = s, y = -s, xend = s2, yend = -e2), color = "red", size = 0.5)
      }
    })
    ##################################
    ##################################
    ##################################
    ####################################################################
    #renderPlot mMATplot
    ################################## 
    output$render_mMATplot <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(returns$mMATplot), message = "upload the second mcool file to plot on the upper part of the matrix"))
      returns$mMATplot}, width = widthSize, height = widthSize)
    ##################################
    ##################################
    ##################################
    ####################################################################
    #bedgraphs set 1
    ################################## 
    shiny::observe({  
      shinyFiles::shinyFileChoose(input, "bedgraphs1", roots = root, defaultPath = returns$mcool.path, session = session)
    })
    
    shiny::observeEvent(list(input$bedgraphs1, returns$chr_name), {
      
      #bedgraphs paths
      file_selected3 = shinyFiles::parseFilePaths(root, input$bedgraphs1)
      bedgraphs1.path = as.character(file_selected3$datapath)
      shiny::validate(shiny::need(nrow(file_selected3) > 0, message = "choose bedgraph file."))
      #shiny::validate(shiny::need(!is.null(input$chr), message = "waiting chr input."))
      
      #read badgraphs and filter chr
      bedgraphs1.df = lapply(bedgraphs1.path, function(x) {
        utils::read.table(x, header = FALSE, sep = "\t")[,1:4] %>% dplyr::filter(V1 == returns$chr_name) #%>% dplyr::filter(V3 >= input$start_end[1] & V2 <= input$start_end[2]) #change 1 by input$chr
        }) 
      
      #check input file
      shiny::validate(
        shiny::need(length(bedgraphs1.df[[1]]) >= 4, message = "Please upload bedgraph file format (no header, tab separated)"))
      shiny::validate(
        shiny::need(is.numeric(bedgraphs1.df[[1]][,2]), message = "Column 2 is not numeric"),
        shiny::need(is.numeric(bedgraphs1.df[[1]][,3]), message = "Column 3 is not numeric"),
        shiny::need(is.numeric(bedgraphs1.df[[1]][,4]), message = "Column 4 is not numeric"))
      
      #add names to bedgraphs list
      names(bedgraphs1.df) = sapply(1:length(bedgraphs1.path), function(i){(strsplit(bedgraphs1.path[[i]], "\\/")[[1]] %>% rev)[1]})
      
      for (c in names(bedgraphs1.df)) {
        names(bedgraphs1.df[[c]]) = c("seqnames", "start", "end", c)
      }
      
      #merge bedgraphs
      bedgraph1.df = bedgraphs1.df %>% 
        Reduce(function(...) dplyr::full_join(..., by = c("seqnames", "start", "end")), .) %>% 
        mutate(bp = start) %>% select(-c(seqnames, start, end)) %>% tidyr::gather("sample", "score",1:length(bedgraphs1.df)) %>% filter(!is.na(score))
        
      returns$bedgraph1.df = bedgraph1.df
    })
    
    ####################################################################
    #Bedgraph Plot if "returns$bedgraph.df", "from", "to" are updated
    ################################## 
    shiny::observeEvent(list(returns$bedgraph1.df, returns$from, returns$to, input$offset_bg1), {
      
      #check
      shiny::validate(shiny::need(!is.null(returns$bedgraph1.df), message = "loading bedgraphs..."))
      
      #BGplot1
      data.plot = returns$bedgraph1.df %>% filter(bp >= input$start_end[1] & bp <= input$start_end[2])
      returns$BGplot1 <- ggplot2::ggplot()+ggplot2::geom_step(data = data.plot, ggplot2::aes(y = score, x = bp, color = sample))+
        ggplot2::scale_x_continuous(labels = scales::unit_format(unit = "Mb", scale = 1e-6), limits = c(input$start_end[1], input$start_end[2]), expand = expansion(mult = c(input$offset_bg1, 0.05)))+ #
        theme(legend.position="bottom")+ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), legend.title = ggplot2::element_blank())
    })
    ####################################################################
    #bedgraphs set 2
    ################################## 
    shiny::observe({  
      shinyFiles::shinyFileChoose(input, "bedgraphs2", roots = root, defaultPath = returns$mcool.path, session = session)
    })
    
    shiny::observeEvent(list(input$bedgraphs2, returns$chr_name), {
      
      #bedgraphs paths
      file_selected4 = shinyFiles::parseFilePaths(root, input$bedgraphs2)
      bedgraphs2.path = as.character(file_selected4$datapath)
      shiny::validate(shiny::need(nrow(file_selected4) > 0, message = "choose bedgraph file."))
      #shiny::validate(shiny::need(!is.null(input$chr), message = "waiting chr input."))
      
      #read badgraphs and filter chr
      bedgraphs2.df = lapply(bedgraphs2.path, function(x) {
        utils::read.table(x, header = FALSE, sep = "\t")[,1:4] %>% dplyr::filter(V1 == returns$chr_name)}) #change 1 by input$chr
      
      #check input file
      shiny::validate(
        shiny::need(length(bedgraphs2.df[[1]]) >= 4, message = "Please upload bedgraph file format (no header, tab separated)"))
      shiny::validate(
        shiny::need(is.numeric(bedgraphs2.df[[1]][,2]), message = "Column 2 is not numeric"),
        shiny::need(is.numeric(bedgraphs2.df[[1]][,3]), message = "Column 3 is not numeric"),
        shiny::need(is.numeric(bedgraphs2.df[[1]][,4]), message = "Column 4 is not numeric"))
      
      #add names to bedgraphs list
      names(bedgraphs2.df) = sapply(1:length(bedgraphs2.path), function(i){(strsplit(bedgraphs2.path[[i]], "\\/")[[1]] %>% rev)[1]})
      
      for (c in names(bedgraphs2.df)) {
        names(bedgraphs2.df[[c]]) = c("seqnames", "start", "end", c)
      }
      
      #merge bedgraphs
      bedgraph2.df = bedgraphs2.df %>% 
        Reduce(function(...) dplyr::full_join(..., by = c("seqnames", "start", "end")), .) %>% 
        mutate(bp = start) %>% select(-c(seqnames, start, end)) %>% tidyr::gather("sample", "score",1:length(bedgraphs2.df)) %>% filter(!is.na(score))
      
      returns$bedgraph2.df = bedgraph2.df
    })
    
    ####################################################################
    #Bedgraph Plot if "returns$bedgraph.df", "from", "to" are updated
    ################################## 
    shiny::observeEvent(list(returns$bedgraph2.df, returns$from, returns$to, input$offset_bg2), {
      
      #check
      shiny::validate(shiny::need(!is.null(returns$bedgraph2.df), message = "loading bedgraphs..."))
      
      #BGplot2
      data.plot = returns$bedgraph2.df %>% filter(bp >= input$start_end[1] & bp <= input$start_end[2])
      returns$BGplot2 <- ggplot2::ggplot()+ggplot2::geom_step(data = data.plot, ggplot2::aes(y = score, x = bp, color = sample))+
        ggplot2::scale_x_continuous(labels = scales::unit_format(unit = "Mb", scale = 1e-6), limits = c(input$start_end[1], input$start_end[2]), expand = expansion(mult = c(input$offset_bg2, 0.05)))+ #
        theme(legend.position="bottom")+ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), legend.title = ggplot2::element_blank())
    })
    
    
    ####################################################################
    #bed
    ################################## 
    shiny::observe({  
      shinyFiles::shinyFileChoose(input, "beds", roots = root, defaultPath = returns$mcool.path, session = session)
    })
    
    shiny::observeEvent(list(input$beds, returns$chr_name), {
      
      #bedgraphs paths
      file_selected4 = shinyFiles::parseFilePaths(root, input$beds)
      beds.path = as.character(file_selected4$datapath)
      shiny::validate(shiny::need(nrow(file_selected4) > 0, message = "choose bedgraph file."))
      #shiny::validate(shiny::need(!is.null(input$chr), message = "waiting chr input."))
      
      #read beds and filter chr
      beds.df = lapply(beds.path, function(x) {
        utils::read.table(x, header = FALSE, sep = "\t") %>% dplyr::filter(V1 == returns$chr_name)}) 
      
      #check input file
      shiny::validate(
        shiny::need(length(beds.df[[1]]) >= 3, message = "Please upload bed file format (no header, tab separated)"))
      shiny::validate(
        shiny::need(is.numeric(beds.df[[1]][,2]), message = "Column 2 is not numeric"),
        shiny::need(is.numeric(beds.df[[1]][,3]), message = "Column 3 is not numeric"))
      
      #add names to bedgraphs list
      names(beds.df) = sapply(1:length(beds.path), function(i){(strsplit(beds.path[[i]], "\\/")[[1]] %>% rev)[1]})
      
      for (c in names(beds.df)) {
        beds.df[[c]]$sample = c
      }
      
      #merge beds
      bed.df = do.call(rbind, beds.df) 
      
      returns$bed.df = bed.df
    })
    
    ####################################################################
    #Bed Plot if "returns$bed.df", "from", "to" are updated
    ################################## 
    shiny::observeEvent(list(returns$bed.df, returns$from, returns$to, input$offset_bed, input$annot.col), {
      
      #check
      shiny::validate(shiny::need(!is.null(returns$bed.df), message = "loading bedgraphs..."))
      
      #BEDplot
      data.plot = returns$bed.df
      if (is.na(input$annot.col)) {data.plot$V4 = NA} else {data.plot$V4 = data.plot[, input$annot.col]}
      returns$BEDplot <- ggplot2::ggplot()+
        ggplot2::geom_segment(data=data.plot, aes(y=sample, yend=sample, x=input$start_end[1], xend=input$start_end[2], color=sample), size=1)+
        geom_segment(data=data.plot, aes(y=sample, yend=sample, x=V2, xend=V3), size=4)+
        ggplot2::scale_x_continuous(labels = scales::unit_format(unit = "Mb", scale = 1e-6), limits = c(input$start_end[1], input$start_end[2]), expand = expansion(mult = c(input$offset_bed, 0.05)))+ #
        theme(legend.position="bottom")+ggplot2::theme(axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(), legend.title = ggplot2::element_blank())+
        ggplot2::geom_text(data=data.plot, aes(y=sample, x=(V2+V3)/2, 
                                                    label=V4), nudge_y = c(-0.25,0.25), check_overlap = TRUE)+
        scale_y_discrete(labels = rep(">", length(unique(data.plot$sample))))
    })
    ################################## 
    #renderPlot
    ################################## 
    
    
    output$render_BGplot1 <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(returns$MATplot), message = ""))
      shiny::validate(shiny::need(!is.null(returns$BGplot1), message = ""))
      returns$BGplot1}, width = widthSize, height = 300) 
    
    output$render_BGplot1B <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(returns$mMATplot), message = ""))
      shiny::validate(shiny::need(!is.null(returns$BGplot1), message = ""))
      returns$BGplot1}, width = widthSize, height = 300)
    
    output$render_BGplot2 <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(returns$MATplot), message = ""))
      shiny::validate(shiny::need(!is.null(returns$BGplot2), message = ""))
      returns$BGplot2}, width = widthSize, height = 300) 
    
    output$render_BGplot2B <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(returns$mMATplot), message = ""))
      shiny::validate(shiny::need(!is.null(returns$BGplot2), message = ""))
      returns$BGplot2}, width = widthSize, height = 300)
    
    output$render_BEDplot <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(returns$MATplot), message = ""))
      shiny::validate(shiny::need(!is.null(returns$BEDplot), message = ""))
      returns$BEDplot}, width = widthSize, height = 300) 
    
    output$render_BEDplotB <- shiny::renderPlot({
      shiny::validate(shiny::need(!is.null(returns$mMATplot), message = ""))
      shiny::validate(shiny::need(!is.null(returns$BEDplot), message = ""))
      returns$BEDplot}, width = widthSize, height = 300)

  }
  
  shiny::shinyApp(ui, server)
}

