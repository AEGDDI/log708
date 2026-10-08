el_car_data_1 <- read_csv("~/LOG708/DATA/Data/el_car_data_1.csv")

#a.
cardata <- data.frame(el_car_data_1)

head(cardata,10)
tail(cardata,10)
nrow(cardata)

#b. 
cardata$pct_diesel <- cardata$diesel/cardata$total
cardata$pct_gasoline <- cardata$gasoline/cardata$total
names(cardata)

#c.
cardata$zone = ordered(cardata$zone, levels = c("city",
                                                "below30",
                                                "30to60",
                                                "above60"))
head(cardata$zone)

#d.
cardata12 <- subset(cardata,year<2013)
cardata18 <- subset(cardata,year>2017)

nrow(cardata12)
nrow(cardata18)

#e. 
table(cardata18$zone)

barplot(table(cardata18$zone),main="Zone distribution")

#f.

which.min(cardata18$population)
cardata18[which.min(cardata18$population), ]

which.max(cardata18$population)
cardata18[which.max(cardata18$population), ]

#G.

with(cardata, tapply(income_med_adj, year, mean))

#or
mean(cardata12$income_med_adj)
mean(cardata18$income_med_adj)


#h.

S <- order(cardata12$pct_elcar, decreasing = TRUE)

S[1:10]
cardata12[S[1:10], ]

#i.

S18 <- order(cardata18$pct_elcar, decreasing = TRUE)

S18[1:10]
cardata18[S18[1:10], ]

#j.

with(cardata18, tapply(pct_elcar, zone, mean))

#k.

with(cardata18, boxplot(pct_elcar ~ zone,main="percentage elcar by zone"))

#l.

pct_elcars2018 <- sum(cardata18$el/cardata18$total)
pct_elcars2012 <- sum(cardata12$el/cardata12$total)

print(pct_elcars2018)
print(pct_elcars2012)

#m.

cor(cardata18$pct_expense,cardata18$pct_elcar)

with(cardata18,plot(pct_expense,pct_elcar,main="Scatterplot pct expense and pct elcar"))

#n. 

cor(cardata18$sent_index,cardata18$pct_elcar)

with(cardata18,plot(sent_index,pct_elcar,main="Scatterplot sent_index and pct elcar"))

#o.

cor(cardata18$sent_index,cardata18$pct_diesel)

with(cardata18,plot(sent_index,pct_diesel,main="Scatterplot sent_index and pct diesel"))

#p.



