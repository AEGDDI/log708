#Mandatory Assignment 1
#Log708
#Group 2: Bendik Hjelen & Sigurd Ness


# Task A: read data (removed my personal path)
cardata <- read.csv("C:/Users/Downloads/el_car_data_1.csv")
head(cardata,10)

# Task B: calculate percentages and create new columns

#Pct Diesel
cardata$pct_diesel <- cardata$diesel / cardata$total * 100

#Pct Gasoline
cardata$pct_gasoline <- cardata$gasoline / cardata$total * 100

#Task C: Order zone column
cardata$zone = ordered(cardata$zone, levels = c("city", "below30", "30to60","above60"))

#Task D: Subset 2012 + 2018
cardata12 <- subset(cardata, year == 2012)
cardata18 <- subset(cardata, year == 2018)

#Task E: Plot of municipality count by zone
count <- table(cardata18$zone)
barplot(count, main = "Muncipality count by zone in year 2018")

#Task F: Min + Max population in municipalities
#min
cardata18[which.min(cardata18$population), c("municip_name", "population")]
#max
cardata18[which.max(cardata18$population), c("municip_name", "population")]

#Task G: The mean adjusted income in 2012 and 2018
with(cardata,tapply(income_med_adj, year, mean))

#Task H: Find top 10 municipality pct elcar 2012
S= order(cardata12$pct_elcar, decreasing = TRUE)
S[1:10]
cardata12[S[1:10],]

#Task I: Find top 10 municipality pct elcar 2018
U= order(cardata18$pct_elcar, decreasing = TRUE)
U[1:10]
cardata18[U[1:10],]

#Task J: Mean value of pct elcar in 2018
with(cardata,tapply(pct_elcar,year,mean))

#Task K: Boxplots
with(cardata18, boxplot(pct_elcar ~ zone))

#Task L: Total national pct elcar in 2012 and 2018
#total in 2012
pct_el_2012 = sum(cardata12$el) / sum(cardata12$total) * 100
paste("In 2012, the total pct of el cars was", round(pct_el_2012, 2),"%")

#total in 2018
pct_el_2018 = sum(cardata18$el) / sum(cardata18$total) * 100
paste("In 2018, the total pct of el cars was", round(pct_el_2018, 2),"%")

#Task M: Correlation between pct expense and pct elcar in 2018, with scatterplot
cor_pct_exp_pct_elcar_18=cor(cardata18$pct_expense, cardata18$pct_elcar)
paste("Correlation between expenses and el cars in 2018 is:", round(cor_pct_exp_pct_elcar_18,2))

plot(cardata18$pct_expense, cardata18$pct_elcar,main="Correlation between pct expenses and pct el cars in 2018")

#Task N: Correlation between central index and pct elcar in 2018
cor_sent_index_pct_elcar_18=cor(cardata18$sent_index, cardata18$pct_elcar)
paste("Correlation between sent index and el cars in 2018 is:", round(cor_sent_index_pct_elcar_18,2))

plot(cardata18$sent_index, cardata18$pct_elcar,main="Correlation between sent index and pct el cars in 2018")

#Task O: Correlation between pct diesel and pct elcar in 2018
cor_pct_diesel_pct_elcar_18=cor(cardata18$pct_diesel, cardata18$pct_elcar)
paste("Correlation between sent index and el cars in 2018 is:", round(cor_pct_diesel_pct_elcar_18,2))

plot(cardata18$pct_diesel, cardata18$pct_elcar,main="Correlation between pct diesel and pct el cars in 2018")
