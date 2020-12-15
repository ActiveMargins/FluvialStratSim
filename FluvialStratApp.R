#This is a Shiny application that is wirtten in a single file. To run it, click "Run App" that is in the top right hand corner 
#of the RStudio IDE or run the the following command "runApp('path/FulvialStratApp.R')" where path/FulvialStratApp.R (in quotations) 
#is the full path location of this .R file. 

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)


ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        tags$footer(
                tags$p("This application is a simple stratigraphic model designed by Danny Coutts 
                        (contact info below) in a one day coding challenge. The goal was to produce 
                        a simple GUI-based forward stratigraphic model of fluvial deposits that could 
                        be used in an educational setting to help students understand the impact on 
                        stratigraphic architecture due to changes in fluvial sediment routing systems 
                        parameters (e.g., channel belt width, or rate of channel avulsions)."),
                tags$br(),
                tags$p("As this was completed in ~1 day, the model is very simple and operates in a for loop. 
                        Within the for loop the model increments through generating stratigraphy 
                        where each loop represents roughly one year. Channel deposits are represented by polygons 
                        plotted in a R Shiny ggplot. The location of the polygons are either randomly derived from 
                        inputs (e.g., for the initial placement of a channel deposit within a channel belt), or 
                        sequentially incremented (e.g., migration of channel deposits). Because this model runs in a 
                        for loop, it can result in a very long computational process. Please note the number of 
                        iterations/loops that the model will run given the current inputs (listed in the rightmost box). 
                        To shorten the number of iterations either increase the subsidence rate, or decrease the amount 
                        of strata generated.Once the model is started, it cannot be stopped (a neat feature or RShiny 
                        processes), so you can shut the model window or refresh the page to kill it."),
                tags$br(),
                tags$p("In the future this application will have additional pages in this sidebar menu that describe 
                        the model more fully as well as the range of inputs (e.g., plot of channel width/depth ratio). 
                        Additionally, a new algorithm may be coded to decrease the computational time - going from 
                        for loop to vector-based generation."),
                tags$br(),
                tags$p("Contact info:"),
                tags$ol(
                    tags$li("Email: dannycoutts@gmail.com"),
                    tags$li("Twitter: @ActiveMargins"),
                    tags$li("Github: ActiveMargins")
                )#end ordered list
        )#end tags$footer
    ), #end dashboardSidebar
    dashboardBody(
        
        fluidRow(
            column(width=4,
                fluidRow(
                    box(title = "Model controls",
                        sliderInput("model_wd", "Width of modeling space (m):", min=200, max=10000, value=1000, step=200),
                        sliderInput("strat_m", "m of stratigraphy to generate",5,1000,100),
                        width=12
                    )
                ), #end fluidRow
                
                fluidRow(
                    box(title = "Channel belt controls",
                        sliderInput("blt_wd", "Channel belt width (m):", 200, 2000, 1000),
                        sliderInput("blt_avl", "Channel belt avulsion frequency (1/__yr):", 500, 20000, 1000),
                        width=12
                    )
                ),#end fluidRow
            ), #end of first column
            
            column(width=4,
                   box(
                       title = "Channelbody controls",
                       sliderInput("ch_wd", "Channel body width (m):", 1, 200, 50),
                       sliderInput("ch_th", "Channel body thickness (m):", 1, 50, 20),
                       sliderInput("ch_tri", "Triangular-ness of channel:", 0, 1, 0.5),
                       sliderInput("ch_mig", "Channel body migration rate (m/yr):", 0.01, 5, 1),
                       sliderInput("ch_avl", "Channel avulsion frequency (1/__yr):", 50, 500, 100),
                       width=12
                   )
            ),#end of second column
            
            column(width=4,
                fluidRow(
                    box(
                        title = "Basin controls",
                        sliderInput("sub_rate", "Subsidence (m/yr):", 0.0001, 0.25, 0.005),
                        width=12
                    )
                ),#end fluidRow
                
                fluidRow(
                    box(
                        textOutput("model_parameters"),
                        actionButton("model_start",label="Start the model!"),
                        actionButton("model_reset", label="Reset Model?"),
                        width=12     
                    )
                )#end fluidRow
           )#end column 3    
         ),#end upper fluid row with sliders
            
        fluidRow( 
            box(
                plotOutput("strat_plot",width='100%'),
                width=12
            )
        ), #end fluid row with the plot in it
        fluidRow(
            box(
                selectInput("plot_colours",label="Plot colours", choices=c("Channel belts"="blt",
                                                                           "Channel bodies"="ch",
                                                                           "Channel deposits"="ch_depo"))    
            )
        )#end plotting controls fluid row
    )#end dashboard body
)#end dashboard page

server <- function(input, output, session) {
    #Reactive values that are used in the for loop
    val <- reactiveValues(
        strat_df=NULL,
        belt_df=NULL,
        belt=0,
        channel=0,
        deposit=0,
        new_belt=TRUE,
        new_channel=TRUE,
        blt_xpos=0,
        blt_xpos_min=0,
        blt_xpos_max=0,
        ch_xpos=0,
        ch_xpos_min=0,
        ch_xpos_max=0,
        mig_dir=0,
        ch_xpos_min_draw=0,
        ch_xpos_max_draw=0
    )
    
    #Reactive watcher for the colour scheme on the plot
    color <- reactive(
        input$plot_colours 
    )
    
    #Reactive dataframe that allows users to visualize the input values of channel width/depth of the model prior to simulation
    example_ch <- reactive(
        data.frame(
            x=c(-input$ch_wd/2,
                (-input$ch_wd/2)+(input$ch_wd*(input$ch_tri/2)),
                (input$ch_wd/2)-(input$ch_wd*(input$ch_tri/2)),
                input$ch_wd/2),
            y=c(0,
                -input$ch_th,
                -input$ch_th,
                0),
            blt=c(rep(1,4)),
            ch=c(rep(1,4)),
            ch_depo = c(rep(1,4))
        )
    )
    
    #Reactive dataframe that allows users to visualize the input values of channelbelt size prior to simulation
    example_belt <- reactive(
        data.frame(
            blt=c(1),
            xmin=c(-input$blt_wd/2),
            xmax=(input$blt_wd/2),
            ymin=c(-input$ch_th),
            ymax=c(0)
        )
    )
    
    #Reactive dataframe that either uses the example or simulated channel data
    plotdat <- reactive(
        if (is.null(val$strat_df)){
            plotdat <- example_ch()
        } else {
            plotdat <- val$strat_df
        }    
    )
    
    #Reactive dataframe that either uses the example or simulated channelbelt data
    plotbelt <- reactive(
        if (is.null(val$strat_df)){
            plotbelt <- example_belt()
        } else {
            plotbelt <- val$belt_df
        }    
    )

    
#Functions for the model--------------------------------------------------------    
    define_belt <- function(){ #defining the position of a channel belt on the plot
        val$new_belt <- TRUE
        val$blt_xpos <-  sample((-isolate(input$model_wd)/2):((isolate(input$model_wd)/2)-input$blt_wd), 1) #randomly draw a position within the bounds of the modeling space
        val$blt_xpos_min <- val$blt_xpos #set the variable for the min position of the belt
        val$blt_xpos_max <- val$blt_xpos + input$blt_wd #set variable of the max position of the belt
    } #end define_belt function
    
    
    define_channel <- function(){ #defining the position of the channel within the a belt
        val$new_channel<-TRUE 
        val$ch_xpos <- sample((val$blt_xpos_min):(val$blt_xpos_max-input$ch_wd), 1) #randomly draw a position within the belt
        val$ch_xpos_min <- val$ch_xpos #set the minimum position of this channel
        val$ch_xpos_max <- val$ch_xpos_min + input$ch_wd #set the maximum position of this channel
        val$mig_dir <- sample(c(-1,1),size=1,prob=c(0.5,0.5)) # provide it a direction to migrate -1 for left, 1 for right
    } #end define_channel function
    
    
    draw_channel <- function(){ #longer function that does the naming nomenclature of the deposit, the migration of the deposit, and writing the data to the dataframe
        if(val$new_belt==TRUE){ #if its a the first channel in a belt, reset the naming nomenclature
            val$belt <- val$belt+1
            val$channel <- 1 
            val$deposit <- 0
            val$new_belt <- FALSE
        }
        
        if(val$new_channel==TRUE){ #reset numbering nomenclature for new channels
            if(val$new_belt==TRUE){ 
                val$channel<- 1
            } else { 
                val$channel<- val$channel+1
            }
            val$deposit<- 0
            val$new_channel<-FALSE 
        }
        
        val$deposit <- val$deposit+1 #increment the deposit number
        if(val$ch_xpos_min<val$blt_xpos_min | val$ch_xpos_max>val$blt_xpos_max){ #if the channel is out of bounds,
            val$ch_xpos_min_draw <- val$ch_xpos_min + (-val$mig_dir*input$ch_mig) #migrate it the other way,
            val$ch_xpos_max_draw <- val$ch_xpos_max + (-val$mig_dir*input$ch_mig)
            val$mig_dir <<- -val$mig_dir #and flip the migration direction.
        } else {
            val$mig_dir <- sample(c(val$mig_dir,-val$mig_dir),size=1,prob=c(0.8,0.2)) # -1 for left, 1 for right
            val$ch_xpos_min_draw <- val$ch_xpos_min + (val$mig_dir*input$ch_mig)
            val$ch_xpos_max_draw <- val$ch_xpos_max + (val$mig_dir*input$ch_mig)
        }
        
        val$ch_xpos_min <- val$ch_xpos_min_draw
        val$ch_xpos_max <- val$ch_xpos_max_draw
        
        channel_poly <- data.frame( #make a semi-ugly dataframe for the new data, which is returned by the function
            blt=c(rep(paste(val$belt),4)),
            ch=c(rep(paste(val$belt,".",val$channel,sep=""),4)),
            ch_depo=c(rep(paste(val$belt,".",val$channel,".",val$deposit,sep=""),4)),
            x=c(val$ch_xpos_min_draw,
                val$ch_xpos_min_draw+(input$ch_wd*(input$ch_tri/2)),
                val$ch_xpos_max_draw-(input$ch_wd*(input$ch_tri/2)),
                val$ch_xpos_max_draw),
            y=c(0,
                -input$ch_th,
                -input$ch_th,
                0),
            beltmin=c(rep(val$blt_xpos_min,4)),
            beltmax=c(rep(val$blt_xpos_max,4)),
            migration=c(rep(val$mig_dir,4))
        )#end of defining the channel_poly dataframe - this gets returned to as function call.
    } #end draw_channel function
    
    
    subside <- function(){ #function to subside the basin
        val$strat_df <- val$strat_df %>% mutate(y=y-input$sub_rate)
    }#end of subside

    
#Server code to run or reset the model---------------------------------------------------    
    observeEvent(input$model_start,{ #When run model is clicked
        withProgress(message="Generating stratigraphy. This may take a while.", min=0, max=100, value=0, {
            for (t in seq(from=0, to=input$strat_m/input$sub_rate, by=1)){
                setProgress(value=(t/(input$strat_m/input$sub_rate))*100, detail = paste(round((t/(input$strat_m/input$sub_rate))*100,2), "% Complete", sep=""))

                if(t%%input$blt_avl==0){ #every input$blt_avl years there is a channelbelt avulsion
                    define_belt()
                }
                
                if(t%%input$ch_avl==0){ #every input$ch_avl years theres a channel avulsion
                    define_channel()
                    val$define_channel
                }
                
                val$strat_df <- rbind(val$strat_df,draw_channel()) #bind in the new data
                subside() #subside the basin
            }
            
            val$belt_df <- val$strat_df %>% #gather the data channelbelt data for visualisation
                group_by(blt) %>% 
                summarize(xmin=min(beltmin),
                          xmax=max(beltmax),
                          ymin=min(y),
                          ymax=max(y))
        }) #end of the withProgress/for loop
    }) #end of observe event input$model_start
    
    #When reset model is clicked
    observeEvent(input$model_reset,{ 
        val$strat_df <- NULL
    }) #end of observe event input$model_reset

    
#Normal Shiny server code for widgets and such----------------------------------
    output$model_parameters <- renderText({  # Text output warning users about the number of iterations
        paste("This will represent ", round(input$strat_m/input$sub_rate,digits=0)," years/model interations!")
    })
    
    output$strat_plot <- renderPlot({ # Reactive plot of the setup values or the simulated data 
        p <- ggplot() +
            geom_rect(plotbelt(), mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.3)+
            geom_polygon(plotdat(), mapping=aes(x=x, y=y, fill=!!sym(input$plot_colours), group=ch_depo)) +
            coord_cartesian(
                xlim=c((-input$model_wd/2),(input$model_wd/2)),
                ylim=c(-input$strat_m-(input$ch_th),0)) +
            labs(title="Synthetic Fluvial Architecture", 
                 x="Distance (m)", 
                 y="Thickness (m)") +
            theme(legend.position = "none")
        p
    })
} #end of server code


# Run the application 
shinyApp(ui = ui, server = server)
