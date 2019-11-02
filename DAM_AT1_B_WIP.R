repRead %>% count(Target)
repRead %>% count(age_band)
repRead %>% filter(Target == 1) %>% count(age_band)
repRead %>% count(age_of_vehicle_years)
repRead %>% count(annualised_mileage)
repRead %>% count(car_model)
repRead %>% count(total_services)
repRead %>% count(car_model)
repRead %>% count(num_dealers_visited)
repRead %>% count(num_serv_dealer_purchased)

par(mfrow = c(2,2))

repRead %>% ggplot() +
  geom_bar(aes(x=age_band, fill=as.factor(Target)), position = "dodge") +
  labs(title="# Customer car purchases - age group", subtitle="Single=0, Multipe=1", fill='Target') +
  ylab("# customers")

repRead %>% ggplot() +
  geom_bar(aes(x=gender, fill=as.factor(Target)), position = "dodge") +
  labs(title="# Customer car purchases - gender", subtitle="Single=0, Multipe=1", fill='Target') +
  ylab("# customers")

repRead %>% ggplot() +
  geom_bar(aes(x=car_model, fill=as.factor(Target)), position = "dodge") +
  labs(title="# Customer car purchases - model", subtitle="Single=0, Multipe=1", fill='Target') +
  ylab("# customers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

repRead %>% ggplot() +
  geom_bar(aes(x=age_of_vehicle_years, fill=as.factor(Target)), position = "dodge") +
  labs(fill='Target') +
  ylim(NA,1000)  +
  scale_x_continuous(breaks=pretty_breaks()) 

repRead %>% ggplot() +
  geom_bar(aes(x=annualised_mileage, fill=as.factor(Target)), position = "dodge") +
  labs(fill='Target') +
  ylim(NA,1000) +
  scale_x_continuous(breaks=pretty_breaks()) 


# Scores vector
scores <- c(1, 4, 7, 10, 15, 21, 25, 27, 32, 35,
            49, 60, 75, 23, 45, 86, 26, 38, 34, 67)

# Create deciles based on the values of the vector
decileScores <- decile(vector = scores)
decileScoresDec <- decile(vector = scores, decreasing = TRUE)

ran  <- rnorm(10)
deciles = quantile(ran, seq(0, 1, 0.1))
ran_dec = cut(ran, deciles, include.lowest = TRUE)
dplyr::ntile(rnorm(10), 10)  



mynamestheme <- theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (15)), 
                      legend.title = element_text(colour = "steelblue",  face = "bold.italic", family = "Helvetica"), 
                      legend.text = element_text(face = "italic", colour="steelblue4",family = "Helvetica"), 
                      axis.title = element_text(family = "Helvetica", size = (10), colour = "steelblue4"),
                      axis.text = element_text(family = "Courier", colour = "cornflowerblue", size = (10)))

print(IrisPlot + mynamestheme + labs( title= "Petal and sepal \nlength of iris", y="Petal length (cm)", x = "Sepal length (cm)"))


repRead %>% ggplot() +
  geom_bar(aes(x=age_band, fill=as.factor(Target)), position = "dodge") +
  labs(title="# Customer car purchases", subtitle="Single=0, Multipe=1", fill='Target') +
  ylab("# customers") +
  ylim(NA,5000)





#------------------------------------------------------------------------
recast_data %>% ggplot() +
  geom_bar(aes(x=car_model, fill=as.factor(Target)), position = "dodge") +
  labs(title="# Customer car purchases - model", subtitle="Single=0, Multipe=1", fill='Target') +
  ylab("# customers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

repRead %>% ggplot() +
  geom_bar(aes(x=car_model, fill=as.factor(Target)), position = "dodge") +
  labs(title="# Customer car purchases - model", subtitle="Single=0, Multipe=1", fill='Target') +
  ylab("# customers") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


