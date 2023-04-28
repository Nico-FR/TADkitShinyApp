#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application that draws a histogram
fluidPage(

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          
          #mcool file path
          shinyFiles::shinyFilesButton("Btn_GetFile", "Choose .mcool file" ,
                           title = "Please select a mcool file:", multiple = FALSE,
                           buttonType = "default", class = NULL),
        
          #add horizontal line
          hr(style="height:5px;background:#000000;"),
          
          #balanced box
          checkboxInput("my_balanced", "balanced counts (weight)", value = FALSE),
          
          #log2 box
          checkboxInput("my_log2", "log2(counts)", value = TRUE),
          
          #chr list
          selectInput("my_chr", "chromosome", choices = NULL),
          
          #bin width list
          selectInput("my_res", "bin width", choices = NULL),
          
          #BUTTON to update input$start_end according to chr & bin_width (input$my_chr & input$my_res respectively)
          #actionButton("start_end_update", "Load datas"),
          
          #add horizontal line
          hr(style="height:5px;background:#000000;"),
          
          ###########################
          #Domain files
          ###########################
          #upper Domains
          fileInput("upper_Domain", "Upper Domains File",
                    multiple = FALSE,
                    accept = c(".bed",".txt")),
          
          #lower Domains
          fileInput("lower_Domain", "lower Domains File",
                    multiple = FALSE,
                    accept = c(".bed",".txt", ".csv")),
          
          #reset domains files
          actionButton('reset', 'Reset Domains files'),
          
          #add horizontal line
          hr(style="height:5px;background:#000000;"),
          
          #downloadplot
          downloadButton("downloadPlot", label = "Download plot"),
          
        ),

        ####################################################################
        #MainPanel
        ################################## 
        mainPanel(
          sliderInput("start_end","range:", 
                      min = 1, max =  2, 
                      value = c(1, 2), 
                      width = "100%", sep=",", post="bp"),
          #TABset
          tabsetPanel(type = "tabs",
                      
                      #MATplot
                      tabPanel("MATplot", 
                               
                               # mcool path txt
                               verbatimTextOutput("txt_mcoolfile"),
                               
                               #MATplot
                               plotOutput("render_MATplot", width = "100%", height = "800px") %>% shinycssloaders::withSpinner()
                               ),
                      
                      #mMATplot
                      tabPanel("mMATplot", 
                          
                               #mcool file path
                               shinyFiles::shinyFilesButton("Btn_GetFile2", "Choose second .mcool file (upper)" ,
                                                            title = "Please select a mcool file:", multiple = FALSE,
                                                            buttonType = "default", class = NULL),
                               
                               verbatimTextOutput("txt_mcoolfile2"),
                               
                               #reset domains files
                               actionButton('unload', 'Unload second .mcool files'),
                               
                               plotOutput("render_mMATplot", width = "100%", height = "800px") %>% 
                                 shinycssloaders::withSpinner())
                      )
                      
          
        )
    )
)
