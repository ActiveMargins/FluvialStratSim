#This is a Shiny application that is wirtten in a single file. To run it, click "Run App" that is in the top right hand corner 
#of the RStudio IDE or run the the following command "runApp('path/FulvialStratApp.R')" where path/FulvialStratApp.R (in quotations) 
#is the full path location of this .R file. 

library(shiny)
library(shinydashboard)
library(shinybusy)
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
                        sliderInput("model_wd", "Width of modeling space (m):", min=200, max=10000, value=5000, step=100),
                        sliderInput("strat_th", "m of stratigraphy to generate",5,1000,100),
                        width=12
                    )
                ), #end fluidRow
                
                fluidRow(
                    box(title = "Channel belt controls",
                        sliderInput("blt_wd", "Channel belt width (m):", 200, 5000, 2000),
                        sliderInput("blt_avl", "Probability of channel belt avulsion, p=(1/frequency):", 500, 20000, 1000),
                        width=12
                    )
                ),#end fluidRow
            ), #end of first column
            
            column(width=4,
                   box(
                       title = "Channelbody controls",
                       sliderInput("ch_wd", "Channel body width (m):", 1, 200, 100),
                       sliderInput("ch_th", "Channel body thickness (m):", 1, 50, 10),
                       sliderInput("ch_tri", "Triangular-ness of channel:", 0, 1, 0.5),
                       sliderInput("ch_mig", "Channel body migration rate (m/yr):", 0.01, 5, 2),
                       sliderInput("ch_avl", "Probabilty of channel avulsion, p=(1/frequency):", 50, 1000, 100),
                       width=12
                   )
            ),#end of second column
            
            column(width=4,
                fluidRow(
                    box(
                        title = "Basin controls",
                        sliderInput("sub_rate", "Subsidence (m/yr):", 0.0001, 0.1, 0.005),
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
                selectInput("plot_colours",label="Plot colours", choices=c("Channel belts"="blt_no",
                                                                           "Channel bodies"="ch_no",
                                                                           "Channel deposits"="dep_no"))    
            )
        )#end plotting controls fluid row
    )#end dashboard body
)#end dashboard page

server <- function(input, output, session) {
    #Reactive values that are used in the for loop
    val <- reactiveValues(
        belt_df=NULL,
        strat_df=NULL,
        ch_temp=NULL,
        strat_list=NULL,
        strata_x1_y1=NULL,
        strata_x2_y2=NULL,
        strata_x3_y3=NULL,
        strata_x4_y4=NULL,
        complete_ch_df=NULL,
        complete_blt_df=NULL,
        years=NULL,
        years_completed=NULL
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
            blt_no=c(rep(1,4)),
            ch_no=c(rep(1,4)),
            dep_no = c(rep(1,4))
        )
    )
    
    #Reactive dataframe that allows users to visualize the input values of channelbelt size prior to simulation
    example_belt <- reactive(
        data.frame(
            blt=c(1),
            x_min=c(-input$blt_wd/2),
            x_max=(input$blt_wd/2),
            y_min=c(-input$ch_th),
            y_max=c(0)
       )
    )
    
    #Reactive dataframe that either uses the example or simulated channel data
    plotdat <- reactive(
        if (is.null(val$complete_ch_df)){
            plotdat <- example_ch()
        } else {
            plotdat <- val$complete_ch_df
        }    
    )
    
    #Reactive dataframe that either uses the example or simulated channelbelt data
    plotbelt <- reactive(
        if (is.null(val$complete_blt_df)){
            plotbelt <- example_belt()
        } else {
            plotbelt <- val$complete_blt_df
        }    
    )

    
#Functions for the model--------------------------------------------------------    
    draw_channel <- function(df){
        #Set up some local varaibles
        ch_df <- NULL #set up new object/dataframe for the new data
        local_mig_dir <- df[[1,11]] #get starting migration direction
        prev_ch_min <- df[[1,9]] #get starting ch_min
        prev_ch_max <- df[[1,10]] #get strating ch_max
        
        #Enter a big for loop within one of the lists
        for (i in 1:nrow(df)){
            if((prev_ch_min+local_mig_dir)<df[[i,7]] | (prev_ch_max+local_mig_dir)>df[[i,8]]){ #if the channel is out of bounds,
                local_mig_dir <- (local_mig_dir*-1) #and flip the migration direction.
                x1 <- prev_ch_min + local_mig_dir #migrate it the other way,
                x4 <- prev_ch_max + local_mig_dir
            } else {
                local_mig_dir <- sample(c(local_mig_dir, (local_mig_dir*-1)),size=1,prob=c(0.8,0.2)) # give local_mig_dir a chance to switch 
                x1 <- prev_ch_min + local_mig_dir
                x4 <- prev_ch_max + local_mig_dir
            }
            
            prev_ch_min <- x1
            prev_ch_max <- x4
            
            ch_df <- rbind(ch_df,data.frame(x1=x1,
                                            y1=df[[i,1]],
                                            
                                            x2=x1+(input$ch_wd*(input$ch_tri/2)),
                                            y2=df[[i,1]]-input$ch_th,
                                            
                                            x3=x4-(input$ch_wd*(input$ch_tri/2)),
                                            y3=df[[i,1]]-input$ch_th,
                                            
                                            x4=x4,
                                            y4=df[[i,1]])
            )
        } #end for loop
        val$years_completed <- nrow(df) + val$years_completed
        setProgress(value=(val$years_completed/val$years)*100, message=paste("Please wait: ", round((val$years_completed/val$years)*100, digits=1),"% complete", sep=""))
        cbind(df,ch_df)
    }#end function

    
#Server code to run or reset the model---------------------------------------------------    
    observeEvent(input$model_start,{ #When run model is clicked
        show_modal_spinner(text="Please wait, processing inputs into stratigraphy. Things are happening if this animation is moving.") # show the modal window
        withProgress(message="Generating stratigraphy", value=0, min=0, max=100, { 
            val$years <- input$strat_th/input$sub_rate
            val$years_completed <- 0
            
            val$strat_df <- data.frame(thickness=seq(from=-1*input$strat_th,to=0, by=input$sub_rate),
                                   blt_avl=sample(c(0,1),size=(input$strat_th/input$sub_rate)+1,prob=c((1-(1/input$blt_avl)),(1/input$blt_avl)),replace=TRUE),
                                   ch_avl=sample(c(0,1),size=(input$strat_th/input$sub_rate)+1,prob=c((1-(1/input$ch_avl)),(1/input$ch_avl)),replace=TRUE)
            )  
            
            print("data frame completed")
            val$strat_df[1,2:3] <- 1 #set the first row of the avulsion columns (blt_avl, ch_avl) to 1
            val$strat_df[which(val$strat_df$blt_avl==1),3] <- 1 #set ch_avl==1 where blt_avl==1
            val$strat_df$blt_no <- as.character(cumsum(val$strat_df$blt_avl)) #create a belt number by cumulative sum
            val$strat_df$ch_no <- cumsum(val$strat_df$ch_avl) #create channel number by cumulative sum
            val$strat_df$dep_no <- paste(val$strat_df$blt_no,".",val$strat_df$ch_no,".",seq.int(nrow(val$strat_df)),sep="") #create unique deposit number by paste
            
            print("first set up done")
            #mutate belt information
            val$strat_df <- val$strat_df %>%
                group_by(blt_no) %>% 
                mutate(blt_min=sample((-1*input$model_wd/2):((input$model_wd/2)+(-1*input$blt_wd)), 1),
                       blt_max=blt_min+input$blt_wd) %>% 
                ungroup()
            print("first mutate done")
            print(unique(val$strat_df$blt_min))
            #mutate channel information
            #filter do rowwise mutation on just channel
            val$ch_temp <- val$strat_df %>% 
                filter(ch_avl==1) %>% 
                select(ch_no,blt_min,blt_max) %>%
                rowwise() %>%
                mutate(ch_min=sample((blt_min):(blt_max+(-1*input$ch_wd)), 1),
                       ch_max=(ch_min+input$ch_wd),
                       mig_dir=sample(c((-1*input$ch_mig),input$ch_mig),size=1,prob=c(0.5,0.5))) %>%
                       select(-blt_min,-blt_max)
            
            #left join this data back into the data and remove
            print("going to do left join")
            val$strat_df <- left_join(val$strat_df,val$ch_temp,by="ch_no")
            #val$strat_df[which(val$strat_df$ch_avl==0),10:12] <- NA 
            
            #split into a list of dataframes/tibbles
            val$strat_list <- split(val$strat_df, f=val$strat_df$ch_no)
            
            #lapply the draw_channel function to each dataframe/tibble in the list
            val$strat_list <- lapply(val$strat_list, function(df)draw_channel(df))
            
            #bind the list back into a single dataframe
            val$strat_df <- do.call(rbind.data.frame, val$strat_list)
            
            #filter into individual dataframes and rename x, y columns - I don't like pivoting... then bind into one data table
            val$strata_x1_y1 <- val$strat_df %>% select(thickness,blt_no,ch_no,dep_no,blt_min,blt_max,x=x1,y=y1)
            val$strata_x2_y2 <- val$strat_df %>% select(thickness,blt_no,ch_no,dep_no,blt_min,blt_max,x=x2,y=y2)
            val$strata_x3_y3 <- val$strat_df %>% select(thickness,blt_no,ch_no,dep_no,blt_min,blt_max,x=x3,y=y3)
            val$strata_x4_y4 <- val$strat_df %>% select(thickness,blt_no,ch_no,dep_no,blt_min,blt_max,x=x4,y=y4)
            
            val$strat_df <- rbind(val$strata_x1_y1, val$strata_x2_y2, val$strata_x3_y3, val$strata_x4_y4)
            val$belt_df <- val$strat_df %>% group_by(blt_no) %>% summarize(x_min=min(blt_min),
                                                                           x_max=max(blt_max),
                                                                           y_min=min(y),
                                                                           y_max=max(y)
            )
            val$complete_ch_df <- rbind(val$complete_ch_df,val$strat_df)
            val$complete_blt_df <- rbind(val$complete_blt_df,val$belt_df)
        })#end with progress
        remove_modal_spinner() # remove the progress spinner/click blocker
    }) #end of observe event input$model_start
    
    
    #When reset model is clicked
    observeEvent(input$model_reset,{ 
        val$complete_ch_df <- NULL
        val$complete_blt_df <- NULL
        val$strat_df <- NULL
    }) #end of observe event input$model_reset

    
#Normal Shiny server code for widgets and such----------------------------------
    output$model_parameters <- renderText({  # Text output warning users about the number of iterations
        paste("This will represent ", round(input$strat_th/input$sub_rate,digits=0)," years/model interations!")
    })
    
    output$strat_plot <- renderPlot({ # Reactive plot of the setup values or the simulated data 
        p <- ggplot() +
            geom_rect(plotbelt(), mapping=aes(xmin=x_min, xmax=x_max, ymin=y_min, ymax=y_max), alpha=0.3)+
            geom_polygon(plotdat(), mapping=aes(x=x, y=y, fill=as.factor(!!sym(input$plot_colours)), group=dep_no)) +
            coord_cartesian(
                xlim=c(((-1*input$model_wd)/2),(input$model_wd/2)),
                ylim=c((-1*input$strat_th)-(input$ch_th),0)) +
            labs(title="Synthetic Fluvial Architecture", 
                 x="Distance (m)", 
                 y="Thickness (m)") +
            theme(legend.position = "none")
        p
    })
} #end of server code


# Run the application 
shinyApp(ui = ui, server = server)
