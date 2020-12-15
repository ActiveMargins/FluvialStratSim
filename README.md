# FluvialStratSim

##### A simple forward stratigraphic model of fluvial deposits that runs in a RShiny GUI.

This application is a simple stratigraphic model designed that I designed in a one day coding challenge. The goal was to produce a simple GUI-based forward stratigraphic model of fluvial deposits that could be used in an educational setting to help students understand the impact of fluvial sediment routing systems parameters (e.g., channel belt width, or rate of channel avulsions) on stratigraphic architecture. 

As the code behind the model and the GUI was completed in ~1 day, the model is very simple and operates in a for loop. Within the for loop the model generates stratigraphy where each loop represents roughly one year. Channel deposits are represented by polygons plotted in a R Shiny ggplot. The location of the polygons are either randomly derived from inputs (e.g., for the initial placement of a channel deposit within a channel belt), or sequentially incremented (e.g., migration of channel deposits). Because this model runs in a for loop, it can result in a very long computational process. Please note the number of iterations/loops that the model will run given the current inputs (listed in the rightmost box). To shorten the number of iterations either increase the subsidence rate, or decrease the amount of strata generated.Once the model is started, it cannot be stopped (a neat feature or RShiny processes), so you can shut the model window or refresh the page to kill it.

**It is best to download the script from the repository here and load it from RStudio.** My free shinyapps.io account cannot handle multiple process requests at once (see below).

[Online version of FluvialStratSim](https://activemargins.shinyapps.io/FluvialStratApp/)

#

##### KNOWN ISSUES
###### Online shinyapps.io issues
This currently based out of my free shinyapps.io account, which means that it the model will only run processes one at a time. This means that if there is someone running a simulation (which can be quite time consuming) new users will not be able to load onto the app or run simulations. If you cannot connect within 30 seconds, I would try connecting later. Again, it is best download the FluvialStratApp.R file and load it from RStudio. 

