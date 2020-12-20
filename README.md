# FluvialStratSim

#### A simple forward stratigraphic model of fluvial deposits that runs in a RShiny GUI.

This application is a simple stratigraphic model that I designed in a one day coding challenge. The goal was to produce a simple GUI-based forward stratigraphic model of fluvial deposits that could be used in an educational setting to help students understand the impact of fluvial sediment routing systems parameters (e.g., channel belt width, or rate of channel avulsions) on stratigraphic architecture. 

As the code behind the model and the GUI was completed in ~1 day (with a now a few days of extra tidying and optimization), the model is very simple. It begins by creating a vectors representative of channel or channel belt avulsions. From here the position of channel belts and the initial position of channels are defined. A lapply() loop is used to applied the list of channels to migrate them within their channel belt. This is the most time intensive portion.

Channel deposits are represented by polygons plotted in a R Shiny ggplot. The location of the polygons are either randomly derived from inputs (e.g., for the initial placement of a channel deposit within a channel belt), or sequentially incremented (e.g., migration of channel deposits). Because this model runs partially within a for loop, it can result in a very long computational process. Please note the number of iterations/loops that the model will run given the current inputs (listed in the rightmost box). To shorten the number of iterations either increase the subsidence rate, or decrease the amount of strata generated.Once the model is started, it cannot be stopped (a neat feature of RShiny processes), so you can shut the model window or refresh the page to kill it.

**It is best to download on of the two the R scripts from the repository here and load it from RStudio.** My free shinyapps.io account cannot handle multiple process requests at once (see below).


[Online version of FluvialStratSim](https://activemargins.shinyapps.io/FluvialStratApp/)


<p align="center">
  <img width="808" height="200" src="https://github.com/ActiveMargins/FluvialStratSim/blob/main/images/ModelImage.JPG">
</p>

#### AVAILABLE VERSIONS/FORMATS
##### R Workbook (VectorizedFluvialStratApp.Rmd)
This version contains a workbook that generates fluvial stratigraphy. There are five code chunks within the notebook: 
1) The first loads the required libraries
2) Inputs for numeric variables. The default controls will create 100m of stratigraphy over 20 000 years. 
3) A simple sanity check that prints the number of model iterations (this can be ignored once you get the jist of the model).
4) This is the model. It begins with the main function that is used to draw polygons for channelform deposits. Once that function is loaded, the chunk proceeds into the main body of the model. When the model is complete it will generate a basic plot of the generated stratigraphy to display. The generated stratigraphy is housed in two dataframes:
  a) strat_df - a dataframe containing information for plotting channelform deposits
  b) belt_df - a dataframe containing information for plotting channel belt extents
5) The final code chunk is an additional plotting chunk that can be used to display the three levels of stratigraphic heirarchy used here side by side (i.e., individual deposits, deposits of the same channel, and deposits of similar channel belts).

As the model runs it will print updates when it is finished computing the migration within a channel, listing the total number of years processed and the percentage of completion. 

If you are adapting this for any serious computation, you could look into parallel computing the lapply() for better perfomance. None of the individual lists used by the lapply() do not require previous outputs. I didn't persue this because there are requirements based on operating systems.   

<p align="center">
  <img width="643" height="388" src="https://github.com/ActiveMargins/FluvialStratSim/blob/main/images/NoteBookExample.JPG">
</p>

##### R Shiny App - downloadable (FluvialStratApp.R)
This is a single file R Shiny application. It has the same inputs as the other versions but is launched from R or RStudio. Using this will allow you to not be throttled by ShinyApps.io but still allow you to use the graphical user interface created by all that code in the .R file. 

<p align="center">
  <img width="944.5" height="456" src="https://github.com/ActiveMargins/FluvialStratSim/blob/main/images/ModelExample.JPG">
</p>


##### R Shiny App ([Online version of FluvialStratSim](https://activemargins.shinyapps.io/FluvialStratApp/))
This version of the RShiny app is hosted on shinyapps.io. It is hosted on my free shinyapps account, which has limits on the amount of processing time as well as the number of instances that can be run contemporarily on the servers. See the limitations below. It is best to use one of the other available versions/formats if you are wanting to actually crunch significant stratigraphic models.

#### KNOWN ISSUES
##### Online shinyapps.io limitations
This currently based out of my free shinyapps.io account, which means that it the model will only run processes one at a time. This means that if there is someone running a simulation (which can be quite time consuming) new users will not be able to load onto the app or run simulations. If you cannot connect within 30 seconds, I would try connecting later. Again, it is best download the FluvialStratApp.R file and load it from RStudio. 

##### Processing times
Because the motions of individual channels are computed indivdually at a year scale, loading up a large model (e.g., >500 000 yr) can result in long computation times. I have done my best to do as much of the model in a vectorized format. But there is still a for loop that is run on each channel. On my laptop the R Notebook version takes about between *3 and 5 mins per 100 000 years.*
