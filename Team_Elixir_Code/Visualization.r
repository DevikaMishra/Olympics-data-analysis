

library(ggplot2)
library(plotly)
library(readxl)
library(readr)


MenWomenComp <- read_excel("/Team_Elixir_Dataset/MenWomenComp.xlsx")
X_tot_per_year_Breakdown <- read_excel("/Team_Elixir_Dataset/(tot per year)Breakdown.xlsx")
med_per_country <- read_excel("/Team_Elixir_Dataset/med per country.xlsx")

ALL_MEDALISTS <- read_csv("/Team_Elixir_Dataset/ALL_MEDALISTS.csv")


#VISUALIZATION 1 - SEX RATIO BAR PLOT

top_labels <- c('Men' , 'Women')
years<-c(unlist(MenWomenComp$Edition))

p <- plot_ly(MenWomenComp , x = ~`Gold Men`, y = ~Edition, type = 'bar', orientation = 'h',
             marker = list(color = 'rgba(38, 24, 74, 0.8)',
                           line = list(color = 'rgb(248, 248, 249)', width = 1))) %>%
  add_trace(x = ~`Gold Women`, marker =  list(color = 'rgba(190, 192, 213, 1)')) %>%
  layout(xaxis = list(title = "Percentage of Gold medals won by  MEN , WOMEN ",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE,
                      domain = c(0.15, 1)),
         yaxis = list(title = "Edition",
                      showgrid = FALSE,
                      showline = FALSE,
                      showticklabels = FALSE,
                      zeroline = FALSE),
         barmode = 'stack',
         paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
         margin = list(l = 10, r = 20, t = 40, b = 20),
         showlegend = FALSE) %>%
  # labeling the y-axis
  add_annotations(xref = 'paper', yref = 'year', x = 0.15, y = years,
                  xanchor = 'right',
                  text = years,
                  font = list(family = 'Arial', size = 14,
                              color = 'rgb(67, 67, 67)'),
                  showarrow = FALSE, align = 'right')%>%
  # labeling the percentages of each bar (x_axis)
  add_annotations(x = (MenWomenComp$`Gold Men`/2) , y = MenWomenComp$Edition,
                  text = paste( round((MenWomenComp$`Gold Men`/(MenWomenComp$`Gold Men`+ MenWomenComp$`Gold Women`) )*100) , '%'),
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(255, 255, 255)'),
                  showarrow = FALSE) %>%
  add_annotations(x = ((2*MenWomenComp$`Gold Men`+ MenWomenComp$`Gold Women`)/2) , y = MenWomenComp$Edition,
                  text = paste( round(MenWomenComp$`Gold Women`/(MenWomenComp$`Gold Men`+MenWomenComp$`Gold Women`)*100), '%'),
                  font = list(family = 'Arial', size = 12,
                              color = 'rgb(0,0,0)'),
                  showarrow = FALSE)

p

######################################################################################################

#VISUALIZATION 2 - Total Medal count per year

Tot_dat<-as.data.frame(X_tot_per_year_Breakdown)
ggplot(Tot_dat, aes(Edition, Grand_Total)) + geom_line() +labs(title="Total Medal count per year")

######################################################################################################

#VISUALIZATION 3

ggplot(med_per_country, aes(med_per_country$Year)) +
  geom_line(aes(y=med_per_country$UK , colour="UK" )) +   # first layer
  geom_line(aes(y=med_per_country$USSR, colour="USSR")) +
  geom_line(aes(y=med_per_country$Russia, colour="Russia")) +
  geom_line(aes(y=med_per_country$USA, colour="USA")) +
  geom_line(aes(y=med_per_country$China , colour="China")) +
  scale_colour_manual("", 
                      breaks = c("UK", "USSR" , "Russia" , "USA" , "China"),
                      values = c( "blue" , "red", "green" , "yellow" , "black")) +
  labs(title="Medals per Country")+
  ylab("Medal count") + xlab("Year(Edition)")

######################################################################################################

#VISUALIZATION 4

sports<-c(unique(ALL_MEDALISTS$Sport))
count_sp<-c(0,0,0)

for(j in (1:42))
  count_sp[j] = 0;

for(i in (1:29216))
{
      count_sp[which(sports == sapply(ALL_MEDALISTS[i, 3], as.character))] = count_sp[which(sports == sapply(ALL_MEDALISTS[i, 3], as.character))] +1;
}

library(plotly)

p <- plot_ly(
  x = sports ,
  y = count_sp,
  type = "bar"
)%>%
  layout(title = "Medals Won - by sports",
         xaxis = list(title = "Sports"),
         yaxis = list(title = "Medal Count") ,
         margin = list(l = 80, r = 30, t = 60, b = 100))

p

############################################################################################################



