library(plotly)
library(tidyr)
library(plyr)


#-------------------------------------
mydataset <- read.csv("Monthly_rainfall.csv")
tmp3 <- read.csv("C:/Users/PIUS/Desktop/recess2019/FOOD SECURITY ASSURANCE SYSTEM/Code/tmp2.csv")
# #--------------------------------------


data <- spread(Orange, Tree, circumference)
# data <- rename(sub_dataset1, c("1" = "Tree1", "2" = "Tree2", "3" = "Tree3", "4" = "Tree4", "5" = "Tree5"))
 plot_ly(temp3, x = ~month, y = ~year, type = 'scatter', mode = 'lines', name = 'Montyly_rainfall') %>%
  # add_trace(y = ~Tree2, name = 'Tree 2') %>%
  # add_trace(y = ~Tree3, name = 'Tree 3') %>%
  # add_trace(y = ~Tree4, name = 'Tree 4') %>%
  # add_trace(y = ~Tree5, name = 'Tree 5')

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
# chart_link = api_create(p, filename="legend-names")
# chart_link

p