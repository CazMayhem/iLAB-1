# -----------------------------------------------------------
# DATA ALGORITMS AND MEANING - AUT 2019
# ASSIGNMENT 1 PART B
# CAROL PAIPA-MYHILL  /  90014679
# EDA R CODE
# -----------------------------------------------------------

# https://cran.r-project.org/web/packages/dlookr/vignettes/EDA.html
# https://datascienceplus.com/visualizations-for-correlation-matrices-in-r/
# https://towardsdatascience.com/simple-fast-exploratory-data-analysis-in-r-with-dataexplorer-package-e055348d9619
# https://cran.r-project.org/web/packages/corrplot/corrplot.pdf
# https://www.guru99.com/r-generalized-linear-model.html

# https://dzone.com/articles/handle-class-imbalance-data-with-r (code next line)
# https://github.com/treselle-systems/handle_class_imbalance_data/blob/master/ImbalanceData.r
# https://machinelearningmastery.com/tactics-to-combat-imbalanced-classes-in-your-machine-learning-dataset/ 

# initial reading of transaction data provided by Sales Manager
setwd("/Users/paipac/Documents/0 DAM/1 DAM AT1/")
repRead<- read.csv("repurchase_training.csv")

# convert the date from string to date
# trRead$date <- as.Date(trRead$date, format = "%d/%m/%y")

# quick view of the contents of the file
str(repRead) 
# temp <- trRead %>% filter (monthly_amount != 0) %>% select(monthly_amount)
# summary(temp)

#Step 1) Check continuous variables
continuous <-select_if(repRead, is.numeric)
#summary(continuous)
str(continuous)

# Plot the distribution - Let's look closer at the distribution of hours.per.week
# Histogram with kernel density curve
library(ggplot2)
ggplot(continuous, aes(x = num_dealers_visited)) + geom_density(alpha = .2, fill = "#FF6666")

top_one_percent <- quantile(repRead$num_dealers_visited, .99)
top_one_percent

repRead_drop <- repRead %>% filter(num_dealers_visited<top_one_percent)
dim(repRead_drop)

repRead_rescale <- repRead_drop %>% mutate_if(is.numeric, funs(as.numeric(scale(.))))
head(repRead_rescale)

# Step 2) Check factor variables
# This step has two objectives - Check the level in each categorical column
#                              - Define new levels
# 1. Select the categorical columns
# 2. Store the bar chart of each column in a list
# 3. Print the graphs

# Check data balance/imbalance
table_1 <- table(repurchData$Target)
table_1
sprintf("%.1f%%",round(prop.table(table_1)*100, 3))

# Select categorical column
factor <- data.frame(select_if(repRead, is.factor))
ncol(factor)
names(factor[1])
# plot a bar chart for each column in the data frame factor. 
# It is more convenient to automatize the process, especially in situation there are lots of columns.

library(ggplot2)
# Create graph for each column
#graph <- lapply(names(factor),
graph <- lapply(names(factor),
                function(x) 
                  ggplot(factor, aes(get(x))) +
                  geom_bar(aes(y = (..count..)/sum(..count..))) + 
                  geom_text(aes( label = percent((..count..)/sum(..count..)),
                                 y= (..count..)/sum(..count..) ), stat= "count", 
                                 vjust = -.2, size=4) +
                  ylab("relative frequencies") +
                  xlab(x) +
                  scale_y_continuous(labels=scales::percent) +
                  theme(axis.text.x = element_text(angle = 45)))


par(cex=0.7,mar=c(5, 12, 2, 3))
graph

# filter out models above model_9 as there is not enough information to analyse
factor %>% filter(as.character(car_model) <= "model_1" | as.character(car_model) >= "model_2") %>% 
  ggplot(aes(x=car_model, fill=gender)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  geom_text(aes( label = percent((..count..)/sum(..count..)),
                 y= (..count..)/sum(..count..) ), stat= "count", 
            vjust = -6, size=3) +
  ylab("relative frequencies") +
  xlab("car model") +
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x = element_text(angle = 45))

# filter out models above model_9 as there is not enough information to analyse
factor %>% filter(as.character(car_model) <= "model_1" | as.character(car_model) >= "model_2") %>% 
  ggplot(aes(x=car_model, fill=gender)) +
  geom_bar() + 
  ylab("frequencies") +
  xlab("car model") +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(angle = 45))

# filter out models above model_9 as there is not enough information to analyse
factor %>% filter(as.character(car_segment) != "Other") %>% 
  ggplot(aes(x=car_segment, fill=gender)) +
  geom_bar() + 
  ylab("frequencies") +
  xlab("car type") +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x = element_text(angle = 45))

names(repRead)
repRead %>% count(Target)
repRead %>% count(age_band)
repRead %>% count(age_of_vehicle_years)
repRead %>% count(annualised_mileage)
repRead %>% count(car_model)
repRead %>% count(total_services)
repRead %>% count(car_model)
repRead %>% count(num_serv_dealer_purchased)

diagnose(repRead)
describe(repRead)

output <- data.frame("Var","Type","isNA","SumIsNa","Count","Unique", stringsAsFactors = FALSE)
colnames(output) <- (c("Var","Type","isNA","SumIsNa","Count","Unique"))

for (i in seq_along(repRead)) {                                    # 2. sequence
  temp <- data.frame("Var" = str_c(names(repRead[i])), 
                     "Type" = typeof(repRead[i]),
                     "isNA" = str_c(any(is.na(repRead[i]))),
                     "SumIsNa" = str_c(sum(is.na(repRead[i]))),
                     "Count" = str_c(count(repRead[i])),
                     "Unique" = group_by(repRead[i]) %>% summarise(U=n_distinct(repRead[i])))           # 3. body    
  output <- bind_rows(output, temp)
}
output$Unique <- output$U
output <- output[-c(1),]
output$U <- NULL
output

# checking the dimension of the input dataset and the time of variables.
plot_str(repRead)
plot_missing(repRead)

plot_histogram(repRead)

#bivariate/multivariate analysis, starting with Correlation analysis.
plot_correlation(repRead, type = 'continuous')

library("corrplot")
repCorr <- repRead %>% subset(select = -c(ID,age_band,gender,car_model,car_segment))
c <- cor(repCorr)
head(round(c,2))

c <- repRead %>% subset(select = -c(ID,age_band,gender,car_model,car_segment)) %>% cor()
corrplot(c, method = "number", tl.cex = 0.7, cl.cex=0.8, number.cex=0.6)

corrplot(c, method = "circle")
corrplot(c, method = "ellipse", col = col1(200), order = "AOE")
corrplot(c, method="pie")

str(repCorr)
corrplot(c, method = "ellipse", tl.cex = 0.8, cl.cex=0.8, number.cex=0.8, order = "AOE")
corrplot(c, method = "number", tl.cex = 0.8, cl.cex=0.8, number.cex=0.8, order = "AOE")
corrplot(c, method = "color", tl.cex = 0.8, cl.cex=0.8, number.cex=0.8, order = "AOE")

pairs(repCorr)
pairs(~ sched_serv_paid + total_paid_services + annualised_mileage, repCorr)

quantile(rep_filt$age_of_vehicle_years, prob = seq(0, 1, length = 11), type = 5)

rep_filt = repCorr[, c("Target","age_of_vehicle_years","sched_serv_warr","non_sched_serv_warr","sched_serv_paid","non_sched_serv_paid")] 
pairs(rep_filt)

ggpairs(repRead, columns=c("age_band","gender","car_segment")
        

ggpairs(repRead, columns=c("age_of_vehicle_years","sched_serv_warr","non_sched_serv_warr","sched_serv_paid"), 
        diag=list(continuous="density", discrete="bar"), axisLabels="show")

cor(credit_filt, method = "pearson")
cor(credit_filt, method = "spearman")

$ ID                       : int  1 2 3 5 6 7 8 9 10 11 ...
$ Target                   : int  0 0 0 0 0 0 0 0 0 0 ...
$ age_band                 : Factor w/ 8 levels "1. <25","2. 25 to 34",..: 3 8 8 8 8 8 1 8 8 8 ...
$ gender                   : Factor w/ 3 levels "Female","Male",..: 2 3 2 3 1 2 2 2 3 3 ...
$ car_model                : Factor w/ 19 levels "model_1","model_10",..: 1 12 13 13 12 15 13 16 14 14 ...
$ car_segment              : Factor w/ 4 levels "Large/SUV","LCV",..: 2 4 1 1 4 1 1 4 4 4 ...

$ age_of_vehicle_years     : int  9 6 9 5 8 7 8 7 1 3 ...
$ sched_serv_warr          : int  2 10 10 8 9 4 2 4 2 1 ...
$ non_sched_serv_warr      : int  10 3 9 5 4 10 8 9 1 1 ...
$ sched_serv_paid          : int  3 10 10 8 10 5 2 6 1 2 ...
$ non_sched_serv_paid      : int  7 4 9 4 7 7 9 9 3 1 ...
$ total_paid_services      : int  5 9 10 5 9 6 9 8 1 2 ...
$ total_services           : int  6 10 10 6 8 8 4 6 2 1 ...
$ mth_since_last_serv      : int  9 6 7 4 5 8 7 9 1 1 ...
$ annualised_mileage       : int  8 10 10 10 4 5 6 5 1 1 ...
$ num_dealers_visited      : int  10 7 6 9 4 10 10 5 2 1 ...
$ num_serv_dealer_purchased: int  4 10 10 7 9 4 4 8 3 1 ...

# Categorical Variables - Barplots
plot_bar(repRead)

#a very nice presentable/shareable rendered markdown in html.
create_report(repRead)

#[1] "ID"                        "Target"                    "age_band"                  "gender"                   
#[5] "car_model"                 "car_segment"               "age_of_vehicle_years"      "sched_serv_warr"          
#[9] "non_sched_serv_warr"       "sched_serv_paid"           "non_sched_serv_paid"       "total_paid_services"      
#[13] "total_services"            "mth_since_last_serv"       "annualised_mileage"        "num_dealers_visited"      
#[17] "num_serv_dealer_purchased"

str(repRead)
repRead %>% count(age_band)
repRead <- repRead %>% mutate(age_band = str_replace(age_band, "NULL", "8. NULL"))
  
dumAge = dummy(repRead$age_band)
dumGender = dummy(repRead$gender)
dumCarModel = dummy(repRead$car_model)
dumCarSeg = dummy(repRead$car_segment)

repRead %>% filter(age_band != "NULL") %>% ggplot() + geom_bar(mapping = aes(x = age_band))

repRead %>% ggplot() + geom_bar(mapping = aes(fct_rev(fct_infreq(factor(age_band))))) + coord_flip()
repRead %>% filter(age_band != "NULL") %>% ggplot() + 
  geom_bar(mapping = aes(fct_rev(fct_infreq(factor(age_band))))) + 
  coord_flip() +
  scale_y_discrete()

repRead %>% ggplot() + geom_bar(mapping = aes(x = gender))+ coord_flip()
repRead %>% ggplot() + geom_bar(mapping = aes(fct_rev(fct_infreq(factor(car_model))))) + coord_flip()
repRead %>% ggplot() + geom_bar(mapping = aes(fct_rev(fct_infreq(factor(car_segment))))) + coord_flip()

repRead %>% 
  count(car_segment) %>% 
  mutate(perc = n / nrow(repRead)) -> repRead2
repRead2 %>% ggplot(aes(x = reorder(car_segment, -perc), y = perc)) + geom_bar(stat = "identity")
repRead %>% ggplot(aes(x = reorder(car_segment, -perc), y = perc)) + geom_bar(stat = "identity")


repRead %>% count(Target)
repRead %>% count(age_band)
repRead %>% count(age_of_vehicle_years)
repRead %>% count(annualised_mileage)
repRead %>% count(car_model)
repRead %>% count(total_services)
repRead %>% count(car_model)
repRead %>% count(num_dealers_visited)
repRead %>% count(num_serv_dealer_purchased)

repRead %>% ggplot() +
  geom_bar(aes(x=age_of_vehicle_years, fill=Target), position = "dodge") +
  scale_y_continuous(breaks=pretty_breaks(), labels = comma) +
  scale_x_continuous(breaks=pretty_breaks())


