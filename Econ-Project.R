#importing the main dataset
library(foreign)
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(plm)
library("mapproj")
library(maps)
library("GGally")
guns <- read.dta("guns.dta")
states <- read.csv("stateid.csv")
guns_s <- merge(x=guns,y=states,all.x=TRUE)

######################################################################################
##########################################VIOLENCE RATE###############################
######################################################################################

guns_s$region <- tolower(guns_s$State)
states <- map_data("state")
map.df <- merge(states,guns_s, by="region", all.x=T)
map.df <- map.df[order(map.df$order),]
ggplot(map.df, aes(x=long,y=lat,group=group))+
  geom_polygon(aes(fill=vio))+
  geom_path()+ 
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
  coord_map()
    
#Except for the Northern region, we can see that vio rate in all the other regions are comparable 
    
ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s, aes(year, log(vio),color=State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#We can see that DC is the one with the highest crime rate. We can see that there is a dip in the violence rate in 
#NY over the past few years starting from 1990.
#Observing the violence rate for NY, taking the log of violance so that the skewness of the data can be controlled
#as some of them have very high violence rate
#Rshiny graph, line chart
ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s[guns_s$State=="New York",], aes(year, log(vio))) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#NY had a vio rate of 7.07 in 1990 which dipped to ~6.4 in the year 1999
#Can we spot the same trend in other states too???

ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s, aes(year, log(vio),color= State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#We can see a similar trend for Rhode Island where there is an increase and then a huge dip, we can also see a similar
#trend in almost all the states where the violence rate has been dipping starting from the 1990's some starting from
#1990 or later 90's
#-----------Graph
#WHY???

###################################INCARC RATE VS VIOLENCE RATE#######################################

#Lets look at the incarcination rate

ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s, aes(year, incarc_rate,color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#We see that the incarcination rate has increased from 1990's for almost all the states. This makes sense ie., if
#there are fewer crimnals on the streets, then crime rate will be less.
#Validate from economic theory : The United States imprisons more people than anywhere else in the world, 
#both in relative and total numbers.

#Is Incarc_rate ENDOGENEOUS --- It might be because, maybe incarcination rate was high due to which crime rate
#started to decrease or due to high crime rate, incarcination rate increased

#ONE OUTLIER === West Virginia(4.8 to 5.8)

ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s[guns_s$State=="West Virginia",], aes(year, log(vio),color= State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)



ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s[guns_s$State=="West Virginia",], aes(year, avginc,color= State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#Increase in Incarcination rate might be a possible reason why the violence rate decreased in 1990's

####################################SHALL LAW VS VIOLENCE RATE######################################

#Did change in law play an important role????

Shall_l<-guns_s %>% filter(shall==1) %>% group_by(State) %>% summarise(Start_year=min(year)) %>% 
  arrange(as.numeric(Start_year))
hist(Shall_l$Start_year, breaks = seq(77,99,1))
#Most of the states have implemented the law in 90's, we can also see a dip in crime rate in the 90's, 
#so we can say that shall law has an effect

#Case 1:
#State having law effective from the begining of the given data
alwayslaw <- guns_s %>% filter(year==77 & shall ==1)
#Indiana, New Hampshire, Vemont and Washington
alwayslaw <- guns_s %>% filter(year==77 & shall ==1)
#There are a total of 4 states where the law was effective from the start of the given data

gun_as <- guns_s %>% filter(State=="Indiana" | State =="Washington" | State=="New Hampshire" | State=="Vermont" )
ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(gun_as, aes(year, log(vio),color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#We see a similar pattern in the 4 states.

#Case 2:
#States that never had the law implemented

y <- guns_s %>% select(State,shall) %>% group_by(State,shall)%>% 
  summarize(n())
z <- y %>% select(State,shall) %>% group_by(State) %>% summarize(shall = sum(shall))
nolaw <- z[z$shall ==0,]
#We can observe that out of 51 sates, 22 states have not implemented the shall law at all.
#Merge with the raw dataset to see the crime over the years
nolaw1 <- merge(x=guns_s,y=nolaw,all.y=TRUE)
#Plot graph
ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(nolaw1, aes(year, log(vio),color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#Even for states that never implemented the law, we see a similar pattern like we saw for the states where the law
#was effective since 1977(start of the data)

#Case 3
w <- guns_s %>% select(State,shall) %>% group_by(State) %>% summarize(shall_t = sum(shall))
guns_st <- w %>% select(State,shall_t) %>% filter(shall_t <23 & shall_t >0)
#There are 25 states that implemented the law after a few years. Most of the states have implemented the law in the
#1990's. 

#Merging with the raw dataset
guns_st <- merge(guns_st,guns_s,all.x=TRUE)
#Plot graph
ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_st, aes(year, log(vio),color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#We see that all the 3 cases have a similar pattern where the crime rate is decreasing mostly in the 1990's'
cor(guns_st$vio,guns_st$shall)
#-5% 
#This might be because we are considering all the years together therefore the correlation is weak.
#Earlier we saw that how the crime rate has decreased from 1990's, implementation of law can have a huge impact 
#on the dip in crime rate


#######################################AVG INC vs VIOLENCE###################################
#Case 1: No law
#Check if avginc has to do anything with the VIOLENECE Rate where the law was never implemented 

plot_ly(nolaw1, x = ~year, y = ~vio, text = ~State, type = 'scatter', mode = 'markers',
        marker = list(size = ~avginc, opacity = 0.5, color = 'rgb(255, 65, 54)')) %>%
  layout(title = 'Violence',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

#LINE CHART OF AVG INC FORL ALL THE STATES OVER THE YEARS
ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s, aes(year, avginc,color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)

#We can see that the avg income have a positive slop wrt year, which makes sense as there has been an economic growth 
#Economic growth can also be a reason why the Violence rate has decreased afet 1990. 

#ECONOMIC  THEORY : From 1990 to 1999, the median American household income grew by 10 percent. The United States 
#economy grew by an average of 4 percent per year between 1992 and 1999. During the '90s, stocks quadrupled in value - 
#the Dow Jones industrial average increased by 309 percent.

#This might be the 3rd possible reason why the crime rate in the US decreased from 1990's


####################################Alaska is an outlier####################
#Avg inc of alaska over the years
ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s[guns_s$State == "Alaska",], aes(year, avginc,color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)

#Violence rate of Alaska over the years
ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s[guns_s$State == "Alaska",], aes(year, vio,color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#The shall law was implememnted in ALaska in 95 and hence the dip. This means that the law played a very important 
#role in controlling the crime rate

#Incarccination rate of Alaska over the years
ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s[guns_s$State == "Alaska",], aes(year, incarc_rate,color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#Even the incarc rate started to increase after the shall law implementation
#We can see an increase in crime rate and a decrease in avg income after 1990's in ALaska

#######################BLACK RACE vs VIOLENT RATE################################

#Case 1: No law
#Check if race has to do anything with the VIOLENECE Rate where the law was never implemented 

plot_ly(nolaw1, x = ~year, y = ~vio, text = ~State, type = 'scatter', mode = 'markers',
        marker = list(size = ~pb1064, opacity = 0.5, color = 'rgb(255, 65, 54)')) %>%
  layout(title = 'Violence',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))

#We can see that the black population in DC is the highest and even the crime rate is very high, but we even have a 
#contradiction that is, Hawaii has one ofthe lowest crime rate but has the 2nd highest black population among the
#states that have not implemented the law ever.

#Case 2: Always had a law

#Check if race has to do anything with the VIOLENECE Rate where the law was always present 

plot_ly(guns_s, x = ~year, y = ~vio, text = ~State, type = 'scatter', mode = 'markers',
        marker = list(size = ~pb1064, opacity = 0.5, color = 'rgb(255, 65, 54)')) %>%
  layout(title = 'Violence',
         xaxis = list(showgrid = FALSE),
         yaxis = list(showgrid = FALSE))
#Even when we look at this graph, we can't say for sure if having higher black population plays an important role
#in the increase/decrease in the violence rate

#LEts remove HAwai and check the correlation:
cor(guns_s$vio,guns_s$pb1064)
guns_ha <- guns_s[guns_s$State !="Hawaii",]
cor(guns_ha$vio,guns_ha$pb1064)
#56% to 77%
#Correlation has increased by 20%

########################################WHITE RACE VS VIOLENCE RATE#######################

#Case 1: No law
#Check if white race has to do anything with the VIOLENECE Rate where the law was never implemented 

ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(nolaw1, aes(pw1064, vio,color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#Most of the states have high white population except for DC and HAwaii
#Case 1: No law
#Check if white race has to do anything with the VIOLENECE Rate where the law was never implemented 

ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(gun_as, aes(pw1064, vio,color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#All the states have high white population.


#########################################MALE POP vs VIOLENCE RATE##########################

ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s, aes(pm1029, vio,color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#Almost all the states have same young male population

############################################POP VS VIOLENCE RATE##########################

ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s, aes(pop, vio,color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#California, Texas and NY have the highest population
ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s[guns_s$State==c("California","Texas","New York","District of Columbia"),], aes(year, vio,color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
#Although the population in these plaaces are the highest the crime rate is not that high
cor(log(guns_s$vio),guns_s$pop)
#Corr is 41%, if we remove the value for DC, we see that the correlation has increased to 49%
guns_dc <- guns_s[guns_s$State !="District of Columbia",]
cor(log(guns_dc$vio),guns_dc$pop)


##########################################DENSITY vs VIOLENCE RATE#########################

ui <- fluidPage(
  plotlyOutput("distPlot")
)
server <- function(input, output) {
  output$distPlot <- renderPlotly({
    ggplot(guns_s, aes(density, vio,color = State)) + 
      geom_line() 
  })
}
shinyApp(ui = ui, server = server)
cor(log(guns_s$vio),guns_s$density)
#38% correlation between vio and density
#Lets remove DC and see 
cor(log(guns_dc$vio),guns_dc$density)
#If we remove DC, density has a 25% correlation with violenece rate


#######################################Lets start with basic lm model########################

m1 <- lm(log(vio)~mur+rob+log(incarc_rate)+pop+avginc+density+pb1064+pm1029+pw1064+stateid
          +shall,data = guns_s)
summary(m1)
AIC(m1)
#511.6131

#This is a simple log linear model, we have not taken the time affect into consideration. We can observe that most
#of the variables have 0 as their coefficient but are very significant.
#We can see that average income,pb1064 and murder are not significant at 10% 
#lets remove all 3 and see the AIC
m2 <- lm(log(vio)~rob+log(incarc_rate)+pop+density+pm1029+pw1064+stateid+
         +shall,data = guns_s)
summary(m2)
AIC(m2)
# 509.615
#The AIC is almost the same and all the coefficients are significant. We can see that the incarc_rate incraeses
#the crime rate by 50%, which doesnt make sense at all
#Similary, shall law decreases the crime rate by 23%, which again doesnt make sense, because, enforcement of law
#can't bring such a huge effect on the crime rate.

m3 <- lm(log(vio)~rob+log(incarc_rate)+pop+density+pm1029+pw1064+stateid+year+
           +shall,data = guns_s)
summary(m3)

AIC(m3)
# 501.0882

m4 <- lm(log(vio)~log(rob)+log(incarc_rate)+pop+density+pm1029+pw1064+stateid+year+pb1064
           +shall,data = guns_s)
summary(m4)
AIC(m4)
#-333.0915
#Everything is significant at 10%
m5 <- lm(log(vio)~log(rob)+mur+log(incarc_rate)+pop+density+pm1029+stateid+year+pb1064
         +shall,data = guns_s)
summary(m5)
AIC(m5)
#-313.7701

#####################LETS CHECK FOR HETEROSKEDASTICITY########################

resid_m5 <- resid(m5)
plot(log(guns_s$incarc_rate),resid_m5, main = "Heteroskedasticity Test")
#There seem to be a pattern, let's see the se to validate
library(sandwich)
library("broom")
#White robust se
tidyg <- function(model,vc=vcov(model),conf.int=FALSE,conf.level=0.95){
  dt <- tidy(model,conf.int=conf.int,conf.level=conf.level)
  dt$std.error <- sqrt(diag(vc))
  dt$statistic <- dt$estimate/dt$std.error
  dt$p.value <- 2*pnorm(-abs(dt$statistic))
  if(conf.int){
    dt$conf.low <- dt$estimate+qnorm((1-conf.level)/2)*dt$std.error
    dt$conf.high <- dt$estimate-qnorm((1-conf.level)/2)*dt$std.error
  }
  return(dt)
}
tidyw <- function(model,...){
  return(tidyg(model,vc=sandwich::vcovHC(model),...))
}
tidyw(m5)
#Heteroskedasticity is not a problem

###################LETS CHECK FOR CORRELATION BETWEEN VARIABLES############

ggcorr(guns_s)

#--------------graph

#We can see that pb1064 and pw1064, are highly correlated. If we have both highly correlated independent variables
#then se wil get inflated, same goes for murder and rob,
#We see that mur and rob are highly correlated with each other and also with other indpendent variables just like vio
#is correlated with other independent variables
#CAN MURDER AND ROBBERY BE ALSO TREATED AS DEPENDENT VARIABLES, we should remove murder and robbery from independent
#variables due to high correlation with the other independent variables and also that its creating simultaneous
#causality bias

#-----------------Write the correlation betwenn various variables

#Since it is a panel data, lets try pooled model

############CASE 1#####POOLED WITH ALL VARIABLES###########################


m6 <- plm(log(vio)~log(rob)+mur+log(incarc_rate)+pop+density+pm1029+pb1064+pw1064
          +shall+avginc,data = guns_s,model="pooling")
summary(m6)
tidyw(m6)
#We can see that pw1064 is insignificant & so is density and pb1064

#Removing PW1064 and checking the significance

m7 <- plm(log(vio)~log(rob)+log(mur)+log(incarc_rate)+pop+log(density)+pm1029+pb1064
          +shall+avginc,data = guns_s,model="pooling")
summary(m7)
tidyw(m7)
#Avg inc becomes highly insignificant
#When we look at the coeff, incr_c is +ve which means every 1% increase in incarc_c rate increases the violence
#rate by 0.33% which should not be the case
#Pooled model is not taking in the fact the time period and the omitted fixed variables, like cultural attitude
#of people. This is making the model biased. Though we see significant results and high R square value, 
#With implementation of shall law, we see a 5% decrease in crime rate but even shall is insignificant when we see 
#cluster Robust se

m8 <-  plm(log(vio)~log(rob)+log(mur)+log(incarc_rate)+pop+log(density)+pm1029+pb1064
           +shall,data = guns_s,model="pooling")
summary(m8)
tidyw(m8)
##The R2 value is 0.9 which is very high this might be due the high corr bw rob and mur with other variables

##########################EXPLAIN THE RELATION BETWEEN VIO murder and rob####################
#REMOVE ROB and MUR, subset of vio

######################CASE FOR SIMULTANEOUS CAUSALITY BIAS#########################
#Lets take log of density as well and remove rob and mur and add others

m9 <-  plm(log(vio)~log(incarc_rate)+pop+log(density)+pm1029+pb1064+pw1064
           +shall+avginc,data = guns_s,model="pooling")

summary(m9)
tidyw(m9)
#We can see that incarc_rate is still positive. This is because pooled takes all the observations as an individual
#observation due to which we don't see correct estimates. Also, pw & pb is insignificant

m10 <- plm(log(vio)~log(incarc_rate)+pop+log(density)+pm1029
           +shall+avginc,data = guns_s,model="pooling")

summary(m10)
#NULL H0 : b5=b6=0
pFtest(m9,m10)
#0.6861, We do not reject the null.
#SOLUTION : FIXED EFFECTS MODEL, IT will take care of both fixed omitted variables and time fixed effects
#White robust se
tidyw(m10)
#If shall law is implemented, the crime rate decreases by 28% which is huge and it doesn't make sense that a law can
#bring such a huge change

########################FIXED EFFECTS WITH ENTITY FIXED EFFECTS#########################
#We use fixed effects because it is immumne to fixed omitted bias like cultural attitude of people. We can take
#care of that in pooled. 

#CASE 1 : WITH ALL THE VARIABLES################

m11 <- plm(log(vio)~log(incarc_rate)+pop+log(density)+pm1029+pb1064+pw1064
           +shall+avginc,data = guns_s,model="within") 
summary(m11)
#Coefficients:
#                 Estimate Std. Error t-value  Pr(>|t|)    
#log(incarc_rate) -0.0672299  0.0282092 -2.3833  0.017327 *  
#pop               0.0243860  0.0092824  2.6271  0.008729 ** 
#log(density)     -0.2518321  0.0859535 -2.9299  0.003460 ** 
#pm1029           -0.0690675  0.0083143 -8.3071 2.821e-16 ***
#pb1064            0.0952893  0.0150322  6.3390 3.352e-10 ***
#pw1064            0.0428067  0.0052073  8.2205 5.591e-16 ***
#shall            -0.0379065  0.0189886 -1.9963  0.046147 *  
#avginc           -0.0041476  0.0057273 -0.7242  0.469107  

#Observations:

#Avg inc is not significant
#Rest everything is significant at 5%
#We see that incarc rate has a negative effect, that is with increase in incar rate by 1%, the vio rate
#decreases by 0.067%. Also with implementation of shall law, the violence rate decreases by 3.8%, which makes sense
#as a  law will not have a huge impact that we saw in pooled

#case 2: AFTER REMOVING AVGINC####################

m12 <- plm(log(vio)~log(incarc_rate)+pop+log(density)+pm1029+pb1064+pw1064
           +shall,data = guns_s,model="within") 
summary(m12)
#All the coefficients are significant.
#With every 1% increase in black pop, the crime rate increases by 0.09%

#################TIME FIXED EFFECTS #######################
#This will take care of the omitted variables that vary over time but are constant across states.

#CASE 1: All VARIABLES
m13 <- plm(log(vio)~log(incarc_rate)+pop+log(density)+pm1029+pb1064+pw1064
           +shall+avginc+as.factor(year),data = guns_s,model="within") 
summary(m13)
#Avg inc , pb1064, pop and pw1064 inc not significant
m14 <- plm(log(vio)~log(incarc_rate)+log(density)+pm1029
           +shall+as.factor(year),data = guns_s,model="within") 
summary(m14)

#All significant 

pFtest(m12,m14)
#We do not reject null, time effects do play a role


#####################CAN WE USE RANDOM EFFECTS MODEL########################
#We know that FE is inefficient when compared to RE, data fixed, can't used RE
#Write reasons in th report
m15 <- plm(log(vio)~log(incarc_rate)+log(density)+pm1029
           +shall+as.factor(year),data = guns_s,model="random") 
summary(m15)
phtest(m14,m15)

#p-value < 2.2e-16, We can see that p-value is very significant, therefore we go for fixed 
#Also the coefficients are different for evry variable, 
#Reason : Endogeneity