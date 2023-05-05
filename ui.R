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
          
          conditionalPanel(condition = "input.tabselected==1",
                           #mcool file path
                           shinyFiles::shinyFilesButton("Btn_GetFile", "Choose .mcool file" ,
                                                        title = "Please select a mcool file:", multiple = FALSE,
                                                        buttonType = "default", class = NULL)
                           ),
          conditionalPanel(condition = "input.tabselected==2",
                           #mcool file path
                           #mcool file path
                           shinyFiles::shinyFilesButton("Btn_GetFile2", "Choose 2nd .mcool file (upper)" ,
                                                        title = "Please select a mcool file:", multiple = FALSE,
                                                        buttonType = "default", class = NULL)
          ),
        
          #add horizontal line
          hr(style="height:5px;background:#000000;"),
          
          #chr list
          selectInput("my_chr", "chromosome", choices = NULL),
          
          #bin width list
          selectInput("my_res", "bin width", choices = NULL),
          
          #add horizontal line
          hr(style="height:5px;background:#000000;"),
          
          #balanced box
          checkboxInput("my_balanced", "balanced counts", value = FALSE),
          
          #scale colors
          selectInput("balanced_name", "balancing type", choices = NULL),
          
          #log2 box
          checkboxInput("my_log2", "log2(counts)", value = TRUE),
          
          #scale colors
          selectInput("scale_colors", "scale color",
                      choices = c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"), 
                      selected = "turbo"),

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
          downloadButton("download_MATplot", label = "MATplot"),
          downloadButton("download_mMATplot", label = "mMATplot"),
          
          #clear session
          actionButton("reload", "Clear session"),
          
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
                      tabPanel("MATplot", value = 1,
                               
                               # mcool path txt
                               verbatimTextOutput("txt_mcoolfile"),
                               
                               #MATplot
                               plotOutput("render_MATplot", width = "100%", height = "800px") %>% shinycssloaders::withSpinner()
                               ),
                      
                      #mMATplot
                      tabPanel("mMATplot", value = 2,
                          
                               verbatimTextOutput("txt_mcoolfile2"),
                               
                               plotOutput("render_mMATplot", width = "100%", height = "800px") %>% 
                                 shinycssloaders::withSpinner()),
                      id = "tabselected"
                      )
                      
          
        )
    )
)
