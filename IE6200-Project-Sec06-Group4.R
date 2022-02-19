#-------------------------------------------------------------------------------------------------------------------
# IE6200 Project - Sec 06 - Group Number 04
# Samruddhi Kulkarni
# Dev Patel
# Shubham Chopade
#-------------------------------------------------------------------------------------------------------------------
install.packages("leaflet")
install.packages("airportr")
install.packages("rworldmap")
install.packages("sf")
install.packages("geosphere")
install.packages("shiny")
install.packages('fitdistrplus')
install.packages('EnvStats')
install.packages('correlation')
install.packages('corrplot')
install.packages('forcats')
install.packages('date')
install.packages('flightplot')
install.packages('plyr')
install.packages('plotly')
library(fitdistrplus)
library(tidyverse) 
library(e1071) 
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)
library(leaflet)
library(airportr)
library(rworldmap)
library(sf)
library(geosphere)
library(shiny)
library(mapproj)
library(EnvStats)
library(correlation)
library(corrplot)
library(lubridate)
library(forcats)
library(date)
library(flightplot)
library(plotly)

getwd()
setwd('C:/Sam/Sem1/Engineering Probability and Statistics/Project/archive (1)')
working_directory <- getwd()

#-------------------------------------------------------------------------------------------------------------------
# Obtaining the Passengers dataset 
Passengers_df <- read.table('International_Report_Passengers1.csv',  # the dataset to be imported
                        header = TRUE,         # the dataset contains names of its columns called as header names
                        sep = ',')
Passengers_df
head(Passengers_df)
summary(Passengers_df)

# Obtaining the Flights dataset 
Flight_df <- read.table('International_Report_Departures1.csv',  # the dataset to be imported
                        header = TRUE,         # the dataset contains names of its columns called as header names
                        sep = ',')
Flight_df
head(Passengers_df)
summary(Passengers_df)

#------------------- Finding Unique numbers of fields in the Passengers dataset--------------------------------------

# Unique Airports
Unique_US_aiports <- unique(Passengers_df$usg_apt)
Unique_US_aiports
length(Unique_US_aiports)

# Unique Foreign Airports where passengers are going to from US
Unique_foreign_aiports <- unique(Passengers_df$fg_apt)
Unique_foreign_aiports
length(Unique_foreign_aiports)

# Unique Carriers
Unique_carrier <- unique(Passengers_df$carrier)
Unique_carrier
length(Unique_carrier)

# Unique Airline Numbers
Unique_airlineid <- unique(Passengers_df$airlineid)
Unique_airlineid
length(Unique_airlineid)

#-------------------------------Descriptive Statistics and Data Visualization----------------------------------------

# Finding total number of passengers of all the airports per year per month
count_per_year_per_month <- Passengers_df %>% group_by(Year,Month) %>% summarise_at(vars(Total),funs(sum(.)))
count_per_year_per_month$Month <- month.abb[count_per_year_per_month$Month]
count_per_year_per_month
# Finding total number of passengers of all the airports per year
count_per_year <- Passengers_df %>% group_by(Year) %>% summarise_at(vars(Total),funs(sum(.)))
count_per_year

# Mean of number of passengers flying from all airports in US every year from 2000-2019
count_per_year_mean <- mean(count_per_year$Total)
count_per_year_mean
# Variance of number of passengers flying from all airports in US every year from 2000-2019
count_per_year_var <- var(count_per_year$Total)
count_per_year_var
# Range of number of passengers flying from all airports in US every year from 2000-2019
count_per_year_range <- range(count_per_year$Total)
count_per_year_range
# Std dev of number of passengers flying from all airports in US every year from 2000-2019
count_per_year_std <- sqrt(count_per_year_var)
count_per_year_std
# Skewness of number of passengers flying from all airports in US every year from 2000-2019
count_per_year_skewness <- skewness(count_per_year$Total)
count_per_year_skewness
# Kurtosis of number of passengers flying from all airports in US every year from 2000-2019
count_per_year_kurtosis <- kurtosis(count_per_year$Total)
count_per_year_kurtosis

# Appending a column named Year_month to count_per_year_per_month dataframe
count_per_year_per_month$Year_month <- paste(count_per_year_per_month$Year,count_per_year_per_month$Month, sep="_")
count_per_year_per_month

Scheduled_Charter_per_year_per_month <- Passengers_df %>% group_by(Year, Month) %>% summarise_at(vars(Total,Scheduled,Charter),funs(sum(.)))
Scheduled_Charter_per_year_per_month

# Plotting total number of passengers of all the airports per year
ggplot(count_per_year, aes(x = Year, y = Total,size=count_per_year$Total/100000 )) +
  geom_point(shape=18, color="red")+
  labs(title="No of passengers per year from 2000-2019") +
  theme(legend.title = element_blank())

# Plotting total number of passengers of all the airports per year per month
ggplot(data = count_per_year_per_month,aes(x = Year , y = Total,fill = fct_inorder(Month))) +
  geom_point(shape = 23) +scale_fill_brewer(palette = "Spectral")

ggplot(count_per_year_per_month, aes(x = Year_month, y = Total,size=count_per_year_per_month$Total/1000000)) +
  geom_point(shape = 16,color="green")+
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue") +
  labs(title="No of passengers per month per year from 2000-2019") +
  theme(legend.title = element_blank())


# Correlation Coefficient - correlation between number of passengers w.r.t year
cr <- cor(count_per_year)
cr
cor.test(count_per_year$Year,count_per_year$Total)
corrplot(cr, method = "number") 

Flight_per_year <- Flight_df %>% group_by(Year) %>% summarise_at(vars(Total),funs(sum(.)))
Flight_per_year
ggplot(Flight_per_year, aes(x = Year, y = Total,size=Total/1000 )) +
  geom_point(shape=19, color="blue")+
  labs(title="No of flights per year from 2000-2019") +
  theme(legend.title = element_blank())

Flight_passengers <- merge(count_per_year, Flight_per_year, by=c("Year"))
Flight_passengers

# Correlation Coefficient - correlation between number of passengers w.r.t number of flights every year
cr_fp <- cor(Flight_passengers)
cr_fp
cor.test(cr_fp)
corrplot(cr_fp, method = "number") 

# Finding Top busiest airports in US
Passengers_at_airports_count <- Passengers_df %>% group_by(usg_apt) %>% summarise_at(vars(Total),funs(sum(.)))
Passengers_at_airports_count
Passengers_at_airports_count_desc <- Passengers_at_airports_count[with(Passengers_at_airports_count, order(-Total)),]
Passengers_at_airports_count_desc
Passengers_at_airports_count_desc_top <- head(Passengers_at_airports_count_desc,5)
Passengers_at_airports_count_desc_top

# Plotting top 5 busiest airports in US
ggplot(Passengers_at_airports_count_desc_top, aes(y=Total, x=usg_apt, fill = usg_apt)) +
  geom_bar(position="dodge",stat="identity") +  #stat="identity"
  ggtitle("Top 5 busiest Airports in US") +
  scale_fill_brewer(palette = "Set2")

Top_5_airports_df <- dplyr::filter(Passengers_df, (usg_apt == "JFK")|(usg_apt=="LAX")|(usg_apt=="MIA")|(usg_apt=="ORD")|(usg_apt=="EWR"))
Top_5_airports_df <- Top_5_airports_df %>% group_by(Year, Month,usg_apt) %>% summarise_at(vars(Total),funs(sum(.)))
Top_5_airports_df
ggplot(Top_5_airports_df, aes(x = usg_apt, y = Total, col = usg_apt )) +
  geom_violin(trim=FALSE) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2, notch=FALSE) 

# Finding Top foreign airports from airports in US
Passengers_to_foreign_dest_count <- Passengers_df %>% group_by(fg_apt) %>% summarise_at(vars(Total),funs(sum(.)))
Passengers_to_foreign_dest_count
Passengers_to_foreign_dest_count_desc <- Passengers_to_foreign_dest_count[with(Passengers_to_foreign_dest_count, order(-Total)),]
Passengers_to_foreign_dest_count_desc
Passengers_to_foreign_dest_count_desc_top <- head(Passengers_to_foreign_dest_count_desc,5)
Passengers_to_foreign_dest_count_desc_top

# Plotting top 5 foreign airports from airports in US
ggplot(Passengers_to_foreign_dest_count_desc_top, aes(y=Total, x=fg_apt, fill = fg_apt)) +
  geom_bar(position="dodge",stat="identity") +  #stat="identity"
  ggtitle("Top 5 foreign airports from US") +
  scale_fill_brewer(palette = "Paired")

# Finding Top carriers in US
Passengers_by_carrier_count <- Passengers_df %>% group_by(carrier) %>% summarise_at(vars(Total),funs(sum(.)))
Passengers_by_carrier_count
Passengers_by_carrier_count_desc <- Passengers_by_carrier_count[with(Passengers_by_carrier_count, order(-Total)),]
Passengers_by_carrier_count_desc
Passengers_by_carrier_count_desc_top <- head(Passengers_by_carrier_count_desc,10)
Passengers_by_carrier_count_desc_top

# Plotting top 5 Carriers in US
ggplot(Passengers_by_carrier_count_desc_top, aes(y=Total, x=carrier, fill = carrier)) +
  geom_bar(position="dodge",stat="identity") +  #stat="identity"
  ggtitle("Top 5 Carriers") +
  scale_fill_brewer(palette = "PuOr")
  
carrier_pie <- ggplot(Passengers_by_carrier_count_desc_top, aes(x="", y=Total, fill=carrier)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +scale_fill_brewer(palette="Spectral")
carrier_pie

#Filtering passenger dataset to get JFK dataset
JFK_df <- dplyr::filter(Passengers_df, (usg_apt == "JFK"))
JFK_df
JFK_df_sorted2 <- JFK_df %>% group_by(usg_apt,Year,Month) %>% summarise_at(vars(Total),funs(sum(.)))
JFK_df_sorted2
JFK_df_sorted1 <- JFK_df %>% group_by(Year) %>% summarise_at(vars(Total),funs(sum(.)))
JFK_df_sorted1
JFK_df_sorted2$Yearmonth <- paste(JFK_df_sorted2$Year,JFK_df_sorted2$Month, sep="")
JFK_df_sorted2

# Plotting total number of passengers at JFK airport per year
ggplot(JFK_df_sorted1, aes(x = Year, y = Total, size = JFK_df_sorted1$Total )) +
  geom_point(shape=17, color="orange")+
  labs(title="No of passengers per year at JFK Airport") +
  theme(legend.title = element_blank())

# Plotting total number of passengers at JFK airport per year per month
JFK_df_sorted3 <- JFK_df_sorted2
JFK_df_sorted3
JFK_df_sorted3$Month <- month.abb[JFK_df_sorted3$Month]
ggplot(JFK_df_sorted3, aes(x = fct_inorder(Month) , y = Year, fill = Total)) +
  geom_tile(color = "black",
            lwd = 0.5,
            linetype = 1) +
  scale_fill_gradient2(low = "white",
                       mid = "yellow",
                       high = "red") +
  guides(fill = guide_colourbar(title = "Total Passengers"))

cr1 <- cor(JFK_df_sorted1)
cr1
cor.test(JFK_df_sorted1$Year,JFK_df_sorted1$Total)
corrplot(cr1, method = "number")

JFK_df_sorted_YM <- JFK_df %>% group_by(Year, Month) %>% summarise_at(vars(Total,Scheduled,Charter),funs(sum(.)))
JFK_df_sorted_YM

cr2 <- cor(JFK_df_sorted_YM)
cr2
cor.test(JFK_df_sorted_YM)
corrplot(cr2, method = "number")

# Plotting connections of JFK airport to top 5 foreign airports 

Airport_lat_long_df <- data.frame (Airtport_name = c("JFK","LAX","MIA","ORD","EWR"),
                                   Latitude = c(40.6 ,33.9,25.8, 42,40.7),
                                   Longitude = c(-73.8,-118, -80.3,-87.9,-74.2))
Airport_lat_long_df
JFK_Foreign_AP_df <- data.frame (Airtport_name = c("LHR","CDG","SDQ","FRA","MEX"),
                                 Latitude  = c(51.5, 49.0, 18.4, 50.0,19.4),
                                 Longitude = c(-0.462,2.55,-69.7,8.57,-99.1))
JFK_Foreign_AP_df

 worldmap <- getMap(resolution = "coarse")
 # plot world map
 plot(worldmap, xlim = c(-180,180), ylim = c(-90, 90),
      asp = 1, bg = "azure2",border = "lightgrey", col = "wheat1", fill = T)

 points(Airport_lat_long_df$Longitude,Airport_lat_long_df$Latitude,
        col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3), cex = Passengers_at_airports_count_desc_top$Total/400000000,pch = 20)
 text(Airport_lat_long_df$Longitude,Airport_lat_long_df$Latitude, Airport_lat_long_df$Airtport_name, pos = 4, col = "black",
      cex = 0.3)
 points(JFK_Foreign_AP_df$Longitude,JFK_Foreign_AP_df$Latitude,
        col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3), cex = 0.5,pch = 20)
 text(JFK_Foreign_AP_df$Longitude,JFK_Foreign_AP_df$Latitude, Airport_lat_long_df$Airtport_name, pos = 4, col = "black",
      cex = 0.3)
 for (j in 1:length(JFK_Foreign_AP_df$Airtport_name))
 {
   Inter <- gcIntermediate(c(-73.8 , 40.6), c(JFK_Foreign_AP_df$Longitude[j],JFK_Foreign_AP_df$Latitude[j]), n=100, addStartEnd=TRUE)
   lines(Inter, col="orange", lwd=0)
 }

# Mean of number of passengers flying from JFK every year from 2000-2019
JFK_mean <- mean(JFK_df_sorted1$Total)
JFK_mean
# Variance of number of passengers flying from JFK every year from 2000-2019
JFK_var <- var(JFK_df_sorted1$Total)
JFK_var
# Range of number of passengers flying from JFK every year from 2000-2019
JFK_var_range <- range(JFK_df_sorted1$Total)
JFK_var_range
# Std dev of number of passengers flying from JFK every year from 2000-2019
JFK_std_dev <- sqrt(JFK_var)
JFK_std_dev
# Skewness of number of passengers flying from JFK every year from 2000-2019
JFK_skewness <- skewness(JFK_df_sorted1$Total)
JFK_skewness
# Kurtosis of number of passengers flying from JFK every year from 2000-2019
JFK_kurtosis <- kurtosis(JFK_df_sorted1$Total)
JFK_kurtosis

LAX_df <- dplyr::filter(Passengers_df, (usg_apt == "LAX"))
LAX_df
LAX_df_sorted1 <- LAX_df %>% group_by(Year) %>% summarise_at(vars(Total),funs(sum(.)))
LAX_df_sorted1
# Mean of number of passengers flying from LAX every year from 2000-2019
LAX_mean <- mean(LAX_df_sorted1$Total)
LAX_mean
# Variance of number of passengers flying from LAX every year from 2000-2019
LAX_var <- var(LAX_df_sorted1$Total)
LAX_var
# Range of number of passengers flying from LAX every year from 2000-2019
LAX_var_range <- range(LAX_df_sorted1$Total)
LAX_var_range
# Std dev of number of passengers flying from LAX every year from 2000-2019
LAX_std_dev <- sqrt(LAX_var)
LAX_std_dev
# Skewness of number of passengers flying from LAX every year from 2000-2019
LAX_skewness <- skewness(LAX_df_sorted1$Total)
LAX_skewness
# Kurtosis of number of passengers flying from LAX every year from 2000-2019
LAX_kurtosis <- kurtosis(LAX_df_sorted1$Total)
LAX_kurtosis

MIA_df <- dplyr::filter(Passengers_df, (usg_apt == "MIA"))
MIA_df
MIA_df_sorted1 <- MIA_df %>% group_by(Year) %>% summarise_at(vars(Total),funs(sum(.)))
MIA_df_sorted1
# Mean of number of passengers flying from MIA every year from 2000-2019
MIA_mean <- mean(MIA_df_sorted1$Total)
MIA_mean
# Variance of number of passengers flying from MIA every year from 2000-2019
MIA_var <- var(MIA_df_sorted1$Total)
MIA_var
# Range of number of passengers flying from MIA every year from 2000-2019
MIA_var_range <- range(MIA_df_sorted1$Total)
MIA_var_range
# Std dev of number of passengers flying from MIA every year from 2000-2019
MIA_std_dev <- sqrt(MIA_var)
MIA_std_dev
# Skewness of number of passengers flying from MIA every year from 2000-2019
MIA_skewness <- skewness(MIA_df_sorted1$Total)
MIA_skewness
# Kurtosis of number of passengers flying from MIA every year from 2000-2019
MIA_kurtosis <- kurtosis(MIA_df_sorted1$Total)
MIA_kurtosis

#-------------------------------Inferential Analysis-----------------------------------------------------------------

# PMF and CDF of airlines going from JFK to LHR in year 2000
JFK_freq <- Passengers_df %>%
  select(usg_apt, fg_apt, Year,Month ,airlineid,Total)%>%
  dplyr::filter(usg_apt == 'JFK' & fg_apt == 'LHR' & Year == '2000') %>%
  group_by(airlineid) %>%
  summarise(airlineid_count = n()) %>%
  mutate(pmf = airlineid_count/sum(airlineid_count))%>%
  mutate(cdf = cumsum(pmf))
JFK_freq

# Visualizing the 'PMF' table using a scatterplot
ggplot(data = JFK_freq , aes(x = airlineid, y = airlineid_count, color = pmf , size = 5)) +
  geom_point()

#--------------------------Joint Probability Distribution------------------------------------------------------------

#Finding Joint Probability of passengers at top 5 airports taking scheduled flights or chartered flights in the year 2019
Column1 <- dplyr::filter(Passengers_df, ((usg_apt == "JFK") | (usg_apt == "LAX") | (usg_apt == "MIA") | (usg_apt == "ORD") 
                                         | (usg_apt == "EWR")) & (Year == 2019))
Column1 
Column1 <- Column1 %>% group_by(usg_apt) %>% summarise_at(vars(Scheduled, Charter),funs(sum(.)))
Column1
Column1 <- Column1 %>% select(Scheduled, Charter)
Column1
Joint_prob <- round(Column1/sum(Column1),3)
Joint_prob
Joint_prob <- Joint_prob %>%
  add_column(Airport = c("EWR","JFK","LAX","MIA","ORD"))
Joint_prob
ggplot(data = Joint_prob , aes(x = Scheduled, y = Charter, color = Airport , size = 5)) +
  geom_point()

#---------------------Goodness of fit for distribution of passengers count per year for JFK airport------------------

fit_n <- fitdist(JFK_df_sorted1$Total, "norm")
summary(fit_n)

par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(list(fit_n), legendtext = plot.legend, xlab = '#passengers at JFK every year', xlegend = 'topleft')
cdfcomp (list(fit_n), legendtext = plot.legend, xlab = '#passengers at JFK every year')
qqcomp (list(fit_n), legendtext = plot.legend, xlab = '#passengers at JFK every year')
ppcomp (list(fit_n), legendtext = plot.legend, xlab = '#passengers at JFK every year')

# From the above plots we see that our distribution is fairly normal with some outliers at higher and lower end

#---------------------Goodness of fit for distribution of passengers count per month per year for JFK airport--------

fit_n <- fitdist(JFK_df_sorted2$Total, "norm")
summary(fit_n)

par(mfrow=c(2,2))
plot.legend <- c("normal")
denscomp(list(fit_n), legendtext = plot.legend, xlab = '#passengers at JFK every year every month', xlegend = 'topleft')
cdfcomp (list(fit_n), legendtext = plot.legend, xlab = '#passengers at JFK every year every month')
qqcomp (list(fit_n), legendtext = plot.legend, xlab = '#passengers at JFK every year every month')
ppcomp (list(fit_n), legendtext = plot.legend, xlab = '#passengers at JFK every year every month')

# From the above plots we see that our distribution is fairly normal with some outliers at higher and lower end

#--------------------Hypothesis Testing------------------------------------------------------------------------------

# Stratified random samples from JFK dataset
JFK_df_sample <- sample_n(JFK_df_sorted2,3)
JFK_df_sample
JFK_df_sample_mean <- mean(JFK_df_sample$Total)
JFK_df_sample_mean
JFK_df_sample_var <- var(JFK_df_sample$Total)
JFK_df_sample_var
sample_data_length <- nrow(JFK_df_sample)
sample_data_length

# Confidence interval assumed = 95%

# Hypothesis Testing 1: H0: The mean number of passengers per month of a year < 1000000
#                       H1: The mean number of passengers per month of a year > 1000000
# This is a right tailed test

# Mean of number of passengers fying from JFK every year every month from 2000-2019
JFK_mean <- mean(JFK_df_sorted2$Total)
JFK_mean
# Variance of number of passengers fying from JFK every year every month from 2000-2019
JFK_var <- var(JFK_df_sorted2$Total)
JFK_var

z <- (JFK_df_sample_mean - 1000000)/sqrt((JFK_var/(sample_data_length)))
z
df <- data.frame("Z_calc"=z,"P_value"= pnorm(z, lower.tail=FALSE))
df

# Conclusion
# As p<<0.05, we reject the null hypothesis and conclude that the sample mean is greater than 1000000

#-------------------------------------------------------------------------------------------------------------------

# Hypothesis Testing 2: H0: The variance of number of passengers per month of a year > 55x10^10
#                       H1: The variance of number of passengers per month of a year < 55x10^10
# This is a left tailed test

varTest(JFK_df_sample$Total, alternative = "less", conf.level = 0.95, 
        sigma.squared = 550000000000, data.name = NULL) 

# Conclusion
# As p<0.05, we reject the null hypothesis and conclude that the sample variance is less than 55x10^10

#-------------------------------------------------------------------------------------------------------------------

# Hypothesis Testing 3: H0: The proportion of number of passengers going by BA airlines from JFK to LHR in 2008 > 0.41
#                       H1: The proportion of number of passengers going by BA airlines from JFK to LHR in 2008 < 0.41
# This is a left tailed test

JFK_LHR <- Passengers_df%>% filter(usg_apt == 'JFK' & fg_apt == 'LHR' & Year == '2008' )
JFK_LHR
JFK_LHR_BA <- Passengers_df%>% filter(usg_apt == 'JFK' & fg_apt == 'LHR' & carrier == 'BA' & Year == '2008')
JFK_LHR_BA
Num_of_jfk_to_lhr <- JFK_LHR %>% select(Total) %>% summarise_at(vars(Total),funs(sum(.))) %>% arrange(-Total)
Num_of_jfk_to_lhr
Num_of_jfk_to_lhr[1,]

Num_of_jfk_to_lhr_1 <- JFK_LHR_BA %>% select(Total) %>% summarise_at(vars(Total),funs(sum(.))) %>% arrange(-Total)
Num_of_jfk_to_lhr_1

prop.test(x= Num_of_jfk_to_lhr_1[1,], n=Num_of_jfk_to_lhr[1,], p=0.41, correct = TRUE, conf.level = 0.95,
          alternative = "less")

# Conclusion
# As p<<0.05, we reject the null hypothesis and conclude that the proportion of number of passengers going by BA airlines from JFK to LHR in 2008 < 0.41


#-------------------------------Linear Regression---------------------------------------------------------------------
# From Goodness of Fit plots we know that the JFK data(JFK_df_sorted1) is normally distributed.
# From the Correlation plot of JFK dataset(JFK_df_sorted1), we see that Year and Total are highly correlated.
# Therefore, regression is run on Total number of passengers from JFK airport every year as dependent variable and 
# Years as independent variable.

JFK_df_sorted1
linear_model <- lm(Total~Year,data = JFK_df_sorted1)
linear_model
summary(linear_model)
summary(JFK_df_sorted1$Total - linear_model$fitted.values)
Test_year <- data.frame(Year=c(2016,2017,2018,2019))
Test_year
predict_passenger_no <- predict(linear_model,newdata = Test_year, interval = 'confidence')
predict_passenger_no

# Conclusion:
# Looking at the output(Median < Mean) we can say that the distribution is not symmetrical but rightly skewed.
# From Coefficients output we can conclude following points:
# 1. Equation of the model: Total number of passengers for x year = 1.044e+06(x) - 2.074e+09
# From model we get predicted value as 31411645 versus actual value as 32936207 for 2017 year which is very close.
# t-value is 16.37 which means that our Year coefficient is 16.37 standard errors away from 0 which is far and 
# we can say that the year coefficient is away from value 0 which is true naturally as years cannot be 0.
# As p-values in our model are extremely small we can say that there is strong evidence that there is strong relationship 
# between Year and Number of passengers.
# The multiple asterisks indicate that Year is more significant to the model.
# For our model, we can say that on average, the actual values of number of passengers per year at JFK airport would be 
# 1645000 (1M) away from predicted values. As our max actual value is 33M, having all our predicted values off by 1M
# proves that model is a good fit for data. 
# Here, Year explains ~93.71% of the variation within Number of passengers, our dependent variable.
# Thus, we can conclude that our model fits the data very well.

# From Goodness of Fit plots we know that the JFK data(JFK_df_sorted2) is normally distributed.
# From the Correlation plot of JFK dataset(JFK_df_sorted2), we see that Year and Total are highly correlated, 
# scheduled and Charter also seem correlated but it is obvious as Total is addition of those two.
# Therefore, regression is run on Total number of passengers from JFK airport every year every month as dependent variable and Month of year as independent variable.

JFK_df_sorted2
JFK_df_sorted2$Yearmonth <- as.numeric(JFK_df_sorted2$Yearmonth)
linear_model1 <- lm(Total~Yearmonth,data = JFK_df_sorted2)
linear_model1
summary(linear_model1)
summary(JFK_df_sorted2$Total - linear_model1$fitted.values)
Test_year_month <- data.frame(Yearmonth=c(20117,20105))
Test_year_month
predict_passenger_no1 <- predict(linear_model1,newdata = Test_year_month, interval = 'confidence')
predict_passenger_no1

# Conclusion: 
# Looking at the output(Median < Mean) we can say that the distribution is not symmetrical but rightly skewed.
# From Coefficients output we can conclude that as p-value > 0.05 we can say that there is poor relationship 
# between month of year and Number of passengers.

#-------------------------------------------------------------------------------------------------------------------

