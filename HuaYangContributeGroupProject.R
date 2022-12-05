#import data
turo_df<-readRDS('turo.data.5140')

#filter data
turo_tx <- turo_df %>%
  filter(car.state=="tx")

#load to csv
turo_tx <- read_csv('turoTX.csv')
print("There have 19 continuouse var and 34 categorical var(8 char,5 num and 21 logi")

#summarise each var and draw hist to continuouse var, bar to categorical var
#1.continuouse var:car.deliver.airport.num
car.deliver.airport.num.summary <- turo_tx %>% 
  summarise_at(vars(car.deliver.airport.num),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.deliver.airport.num.summary
#car.deliver.airport.num Histofram")
hist(turo_tx$car.deliver.airport.num,xlab="car.deliver.airport.num",main="Histogram of car.deliver.airport.num")

#2.continuouse var:car.deliver.hotel.num
car.deliver.hotel.num.summary <- turo_tx %>% 
  summarise_at(vars(car.deliver.hotel.num),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.deliver.hotel.num.summary
#car.deliver.hotel.num Histofram")
hist(turo_tx$car.deliver.hotel.num,xlab="car.deliver.hotel.num",main="Histogram of car.deliver.hotel.num")


#3.continuouse var:car.deliver.train.station.num
car.deliver.train.station.num.summary <- turo_tx %>% 
  summarise_at(vars(car.deliver.train.station.num),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.deliver.train.station.num.summary
#car.deliver.train.station.num Histofram")
hist(turo_tx$car.deliver.train.station.num,xlab="car.deliver.train.station.num",main="Histogram of car.deliver.train.station.num")


#4.continuouse var:car.displayed.turo.review.num
car.displayed.turo.review.num.summary <- turo_tx %>% 
  summarise_at(vars(car.displayed.turo.review.num),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.displayed.turo.review.num.summary
#car.displayed.turo.review.num Histofram")
hist(turo_tx$car.displayed.turo.review.num,xlab="car.displayed.turo.review.num",main="Histogram of car.displayed.turo.review.num")

#5.continuouse var:car.displayed.turo.review.num.past.12m
car.displayed.turo.review.num.past.12m.summary <- turo_tx %>% 
  summarise_at(vars(car.displayed.turo.review.num.past.12m),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.displayed.turo.review.num.past.12m.summary
#car.displayed.turo.review.num.past.12m Histofram")
hist(turo_tx$car.displayed.turo.review.num.past.12m,xlab="car.displayed.turo.review.num.past.12m",main="Histogram of car.displayed.turo.review.num.past.12m")

#6.continuouse var:car.displayed.turo.review.num.past.18m
car.displayed.turo.review.num.past.18m.summary <- turo_tx %>% 
  summarise_at(vars(car.displayed.turo.review.num.past.18m),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.displayed.turo.review.num.past.18m.summary
#car.displayed.turo.review.num.past.18m Histofram")
hist(turo_tx$car.displayed.turo.review.num.past.18m,xlab="car.displayed.turo.review.num.past.18m",main="Histogram of car.displayed.turo.review.num.past.18m")

#7.continuouse var:car.displayed.turo.review.num.past.6m
car.displayed.turo.review.num.past.6m.summary <- turo_tx %>% 
  summarise_at(vars(car.displayed.turo.review.num.past.6m),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.displayed.turo.review.num.past.6m.summary
#car.displayed.turo.review.num.past.6m Histofram")
hist(turo_tx$car.displayed.turo.review.num.past.6m,xlab="car.displayed.turo.review.num.past.6m",main="Histogram of car.displayed.turo.review.num.past.6m")

#8.continuouse var:car.displayed.user.review.num
car.displayed.user.review.num.summary <- turo_tx %>% 
  summarise_at(vars(car.displayed.user.review.num),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.displayed.user.review.num.summary
#car.displayed.user.review.num Histofram")
hist(turo_tx$car.displayed.user.review.num,xlab="car.displayed.user.review.num",main="Histogram of car.displayed.user.review.num")

#9.continuouse var:car.displayed.user.review.num.past.12m
car.displayed.user.review.num.past.12m.summary <- turo_tx %>% 
  summarise_at(vars(car.displayed.user.review.num.past.12m),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.displayed.user.review.num.past.12m.summary
#car.displayed.user.review.num.past.12m Histofram")
hist(turo_tx$car.displayed.user.review.num.past.12m,xlab="car.displayed.user.review.num.past.12m",main="Histogram of car.displayed.user.review.num.past.12m")

#10.continuouse var:car.displayed.turo.review.num.past.18m
car.displayed.turo.review.num.past.18m.summary <- turo_tx %>% 
  summarise_at(vars(car.displayed.turo.review.num.past.18m),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.displayed.turo.review.num.past.18m.summary
#car.displayed.turo.review.num.past.18m Histofram")
hist(turo_tx$car.displayed.turo.review.num.past.18m,xlab="car.displayed.turo.review.num.past.18m",main="Histogram of car.displayed.turo.review.num.past.18m")

#11.continuouse var:car.displayed.user.review.num.past.6m
car.displayed.user.review.num.past.6m.summary <- turo_tx %>% 
  summarise_at(vars(car.displayed.user.review.num.past.6m),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.displayed.user.review.num.past.6m.summary
#car.displayed.user.review.num.past.6m Histofram")
hist(turo_tx$car.displayed.user.review.num.past.6m,xlab="car.displayed.user.review.num.past.6m",main="Histogram of car.displayed.user.review.num.past.6m")

#12.continuouse var:car.extra.num
car.extra.num.summary <- turo_tx %>% 
  summarise_at(vars(car.extra.num),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.extra.num.summary
#car.extra.num Histofram")
hist(turo_tx$car.extra.num,xlab="car.extra.num",main="Histogram of car.extra.num")

#13.continuouse var:car.faq.num
car.faq.num.summary <- turo_tx %>% 
  summarise_at(vars(car.faq.num),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.faq.num.summary
#car.faq.num Histofram")
hist(turo_tx$car.faq.num,xlab="car.faq.num",main="Histogram of car.faq.num")

#14.continuouse var:car.miles.included
car.miles.included.summary <- turo_tx %>% 
  summarise_at(vars(car.miles.included),
               list(min=min, Q1=~quantile(., probs = 0.25,na.rm = TRUE),
                    medium=median, Q3=~quantile(., probs = 0.75,na.rm = TRUE),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.miles.included.summary
#car.miles.included Histofram")
hist(turo_tx$car.miles.included,xlab="car.miles.included",main="Histogram of car.miles.included")

#15.continuouse var:car.photo.num
car.photo.num.summary <- turo_tx %>% 
  summarise_at(vars(car.photo.num),
               list(min=min, Q1=~quantile(., probs = 0.25,na.rm = TRUE),
                    medium=median, Q3=~quantile(., probs = 0.75,na.rm = TRUE),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.photo.num.summary
#car.photo.num Histofram")
hist(turo_tx$car.photo.num,xlab="car.photo.num",main="Histogram of car.photo.num")


#16.continuouse var:car.extra.mile.fee
car.extra.mile.fee.summary <- turo_tx %>% 
  summarise_at(vars(car.extra.mile.fee),
               list(min=min, Q1=~quantile(., probs = 0.25,na.rm = TRUE),
                    medium=median, Q3=~quantile(., probs = 0.75,na.rm = TRUE),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.extra.mile.fee.summary
#car.extra.mile.fee Histofram")
hist(turo_tx$car.extra.mile.fee,xlab="car.extra.mile.fee",main="Histogram of car.extra.mile.fee")


#17.continuouse var:car.trip.price
car.trip.price.summary <- turo_tx %>% 
  summarise_at(vars(car.trip.price),
               list(min=min, Q1=~quantile(., probs = 0.25,na.rm = TRUE),
                    medium=median, Q3=~quantile(., probs = 0.75,na.rm = TRUE),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
car.trip.price.summary
#car.trip.price Histofram")
hist(turo_tx$car.trip.price,xlab="car.trip.price",main="Histogram of car.trip.price")

#18.continuouse var:host.car.num
host.car.num.summary <- turo_tx %>% 
  summarise_at(vars(host.car.num),
               list(min=min, Q1=~quantile(., probs = 0.25),
                    medium=median, Q3=~quantile(., probs = 0.75),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
host.car.num.summary
#host.car.num Histofram")
hist(turo_tx$host.car.num,xlab="host.car.num",main="Histogram of host.car.num")

#19.continuouse var:host.tenure.in.weeks
host.tenure.in.weeks.summary <- turo_tx %>% 
  summarise_at(vars(host.tenure.in.weeks),
               list(min=min, Q1=~quantile(., probs = 0.25,na.rm = TRUE),
                    medium=median, Q3=~quantile(., probs = 0.75,na.rm = TRUE),
                    max=max,mean=mean,sd=sd,ske=skewness),na.rm = TRUE)
host.tenure.in.weeks.summary
#host.tenure.in.weeks Histofram")
hist(turo_tx$host.tenure.in.weeks,xlab="host.tenure.in.weeks",main="Histogram of host.tenure.in.weeks")

#20.categorical var:car.self.pickup.avg.price
car.self.pickup.avg.price.freq <- turo_tx %>%
  group_by(car.self.pickup.avg.price)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.self.pickup.avg.price.freq)
#car.self.pickup.avg.price bar chart
barplot(car.self.pickup.avg.price.freq$n,names.arg=car.self.pickup.avg.price.freq$car.self.pickup.avg.price,
        xlab="car.self.pickup.avg.price",ylab="Frequency",main="Bar Chart of car.self.pickup.avg.price")


#21.categorical var:car.self.pickup.num
car.self.pickup.num.freq <- turo_tx %>%
  group_by(car.self.pickup.num)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.self.pickup.num.freq)
#car.self.pickup.num bar chart
barplot(car.self.pickup.num.freq$n,names.arg=car.self.pickup.num.freq$car.self.pickup.num,
        xlab="car.self.pickup.num",ylab="Frequency",main="Bar Chart of car.self.pickup.num")

#22.categorical var: car.deliver.to.you.num
car.deliver.to.you.num.freq <- turo_tx %>%
  group_by(car.deliver.to.you.num)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.deliver.to.you.num.freq)
#car.deliver.to.you.num bar chart
barplot(car.deliver.to.you.num.freq$n,names.arg=car.deliver.to.you.num.freq$car.deliver.to.you.num,
        xlab="car.deliver.to.you.num",ylab="Frequency",main="Bar Chart of car.deliver.to.you.num")


#23.categorical var:car.doors
car.doors.freq <- turo_tx %>%
  group_by(car.doors)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.doors.freq)
#car.doors bar chart
barplot(car.doors.freq$n,names.arg=car.doors.freq$car.doors,
        xlab="car.doors",ylab="Frequency",main="Bar Chart of car.doors")

#24.categorical var:car.extra.beach.gear
car.extra.beach.gear.freq <- turo_tx %>%
  group_by(car.extra.beach.gear)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.extra.beach.gear.freq)
#car.extra.beach.gear bar chart
barplot(car.extra.beach.gear.freq$n,names.arg=car.extra.beach.gear.freq$car.extra.beach.gear,
        xlab="car.extra.beach.gear",ylab="Frequency",main="Bar Chart of car.extra.beach.gear")

#25.categorical var:car.extra.child.safety.seat
car.extra.child.safety.seat.freq <- turo_tx %>%
  group_by(car.extra.child.safety.seat)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.extra.child.safety.seat.freq)
#car.extra.child.safety.seat bar chart
barplot(car.extra.child.safety.seat.freq$n,names.arg=car.extra.child.safety.seat.freq$car.extra.child.safety.seat,
        xlab="car.extra.child.safety.seat",ylab="Frequency",main="Bar Chart of car.extra.child.safety.seat")

#26.categorical var:car.extra.cooler
car.extra.cooler.freq <- turo_tx %>%
  group_by(car.extra.cooler)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.extra.cooler.freq)
#car.extra.cooler bar chart
barplot(car.extra.cooler.freq$n,names.arg=car.extra.cooler.freq$car.extra.cooler,
        xlab="car.extra.cooler",ylab="Frequency",main="Bar Chart of car.extra.cooler")

#27.categorical var:car.extra.one.way.trip
car.extra.one.way.trip.freq <- turo_tx %>%
  group_by(car.extra.one.way.trip)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.extra.one.way.trip.freq)
#car.extra.one.way.trip bar chart
barplot(car.extra.one.way.trip.freq$n,names.arg=car.extra.one.way.trip.freq$car.extra.one.way.trip,
        xlab="car.extra.one.way.trip",ylab="Frequency",main="Bar Chart of car.extra.one.way.trip")

#28.categorical var:car.extra.pet.fee
car.extra.pet.fee.freq <- turo_tx %>%
  group_by(car.extra.pet.fee)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.extra.pet.fee.freq)
#car.extra.pet.fee bar chart
barplot(car.extra.pet.fee.freq$n,names.arg=car.extra.pet.fee.freq$car.extra.pet.fee,
        xlab="car.extra.pet.fee",ylab="Frequency",main="Bar Chart of car.extra.pet.fee")

#29.categorical var:car.extra.phone.mount
car.extra.phone.mount.freq <- turo_tx %>%
  group_by(car.extra.phone.mount)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.extra.phone.mount.freq)
#car.extra.phone.mount bar chart
barplot(car.extra.phone.mount.freq$n,names.arg=car.extra.phone.mount.freq$car.extra.phone.mount,
        xlab="car.extra.phone.mount",ylab="Frequency",main="Bar Chart of car.extra.phone.mount")

#30.categorical var:car.extra.portable.gps
car.extra.portable.gps.freq <- turo_tx %>%
  group_by(car.extra.portable.gps)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.extra.portable.gps.freq)
#car.extra.portable.gps bar chart
barplot(car.extra.portable.gps.freq$n,names.arg=car.extra.portable.gps.freq$car.extra.portable.gps,
        xlab="car.extra.portable.gps",ylab="Frequency",main="Bar Chart of car.extra.portable.gps")

#31.categorical var:car.extra.post.trip.cleaning
car.extra.post.trip.cleaning.freq <- turo_tx %>%
  group_by(car.extra.post.trip.cleaning)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.extra.post.trip.cleaning.freq)
#car.extra.post.trip.cleaning bar chart
barplot(car.extra.post.trip.cleaning.freq$n,names.arg=car.extra.post.trip.cleaning.freq$car.extra.post.trip.cleaning,
        xlab="car.extra.post.trip.cleaning",ylab="Frequency",main="Bar Chart of car.extra.post.trip.cleaning")

#32.categorical var:car.extra.prepaid.ev.recharge
car.extra.prepaid.ev.recharge.freq <- turo_tx %>%
  group_by(car.extra.prepaid.ev.recharge)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.extra.prepaid.ev.recharge.freq)
#car.extra.prepaid.ev.recharge bar chart
barplot(car.extra.prepaid.ev.recharge.freq$n,names.arg=car.extra.prepaid.ev.recharge.freq$car.extra.prepaid.ev.recharge,
        xlab="car.extra.prepaid.ev.recharge",ylab="Frequency",main="Bar Chart of car.extra.prepaid.ev.recharge")

#33.categorical var:car.extra.prepaid.refuel
car.extra.prepaid.refuel.freq <- turo_tx %>%
  group_by(car.extra.prepaid.refuel)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.extra.prepaid.refuel.freq)
#car.extra.prepaid.refuel bar chart
barplot(car.extra.prepaid.refuel.freq$n,names.arg=car.extra.prepaid.refuel.freq$car.extra.prepaid.refuel,
        xlab="car.extra.prepaid.refuel",ylab="Frequency",main="Bar Chart of car.extra.prepaid.refuel")

#34.categorical var:car.extra.stroller
car.extra.stroller.freq <- turo_tx %>%
  group_by(car.extra.stroller)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.extra.stroller.freq)
#car.extra.stroller bar chart
barplot(car.extra.stroller.freq$n,names.arg=car.extra.stroller.freq$car.extra.stroller,
        xlab="car.extra.stroller",ylab="Frequency",main="Bar Chart of car.extra.stroller")

#35.categorical var:car.extra.unlimited.mileage
car.extra.unlimited.mileage.freq <- turo_tx %>%
  group_by(car.extra.unlimited.mileage)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.extra.unlimited.mileage.freq)
#car.extra.unlimited.mileage bar chart
barplot(car.extra.unlimited.mileage.freq$n,names.arg=car.extra.unlimited.mileage.freq$car.extra.unlimited.mileage,
        xlab="car.extra.unlimited.mileage",ylab="Frequency",main="Bar Chart of car.extra.unlimited.mileage")

#36.categorical var:car.instant.booke
car.instant.book.freq <- turo_tx %>%
  group_by(car.instant.book)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.instant.book.freq)
#car.instant.book bar chart
barplot(car.instant.book.freq$n,names.arg=car.instant.book.freq$car.instant.book,
        xlab="car.instant.book",ylab="Frequency",main="Bar Chart of car.instant.book")

#37.categorical var:car.insurance
car.insurance.freq <- turo_tx %>%
  group_by(car.insurance)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.insurance.freq)
#car.insurance bar chart
barplot(car.insurance.freq$n,names.arg=car.insurance.freq$car.insurance,
        xlab="car.insurance",ylab="Frequency",main="Bar Chart of car.insurance")

#38.categorical var:car.make
car.make.freq <- turo_tx %>%
  group_by(car.make)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.make.freq)
#car.make bar chart
barplot(car.make.freq$n,names.arg=car.make.freq$car.make,
        xlab="car.make",ylab="Frequency",main="Bar Chart of car.make")

#39.categorical var:car.model
car.model.freq <- turo_tx %>%
  group_by(car.model)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.model.freq)
#car.model bar chart
barplot(car.model.freq$n,names.arg=car.model.freq$car.model,
        xlab="car.model",ylab="Frequency",main="Bar Chart of car.model")

#40.categorical var:car.photo.verified
car.photo.verified.freq <- turo_tx %>%
  group_by(car.photo.verified)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.photo.verified.freq)
#car.photo.verified bar chart
barplot(car.photo.verified.freq$n,names.arg=car.photo.verified.freq$car.photo.verified,
        xlab="car.photo.verified",ylab="Frequency",main="Bar Chart of car.photo.verified")

#41.categorical var:car.power
car.power.freq <- turo_tx %>%
  group_by(car.power)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.power.freq)
#car.power bar chart
barplot(car.power.freq$n,names.arg=car.power.freq$car.power,
        xlab="car.power",ylab="Frequency",main="Bar Chart of car.power")

#42.categorical var:car.rental.type
car.rental.type.freq <- turo_tx %>%
  group_by(car.rental.type)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.rental.type.freq)
#car.rental.type bar chart
barplot(car.rental.type.freq$n,names.arg=car.rental.type.freq$car.rental.type,
        xlab="car.rental.type",ylab="Frequency",main="Bar Chart of car.rental.type")

#43.categorical var:car.state
car.state.freq <- turo_tx %>%
  group_by(car.state)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.state.freq)
#car.state bar chart
barplot(car.state.freq$n,names.arg=car.state.freq$car.state,
        xlab="car.state",ylab="Frequency",main="Bar Chart of car.state")

#44.categorical var:car.transmission
car.transmission.freq <- turo_tx %>%
  group_by(car.transmission)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.transmission.freq)
#car.transmission bar chart
barplot(car.transmission.freq$n,names.arg=car.transmission.freq$car.transmission,
        xlab="car.transmission",ylab="Frequency",main="Bar Chart of car.transmission")

#45.categorical var:car.turo.go
car.turo.go.freq <- turo_tx %>%
  group_by(car.turo.go)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.turo.go.freq)
#car.turo.go bar chart
barplot(car.turo.go.freq$n,names.arg=car.turo.go.freq$car.turo.go,
        xlab="car.turo.go",ylab="Frequency",main="Bar Chart of car.turo.go")

#46.categorical var:car.year
car.year.freq <- turo_tx %>%
  group_by(car.year)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.year.freq)
#car.year bar chart
barplot(car.year.freq$n,names.arg=car.year.freq$car.year,
        xlab="car.year",ylab="Frequency",main="Bar Chart of car.year")

#47.categorical var:host.all.star
host.all.star.freq <- turo_tx %>%
  group_by(host.all.star)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(host.all.star.freq)
#host.all.star bar chart
barplot(host.all.star.freq$n,names.arg=host.all.star.freq$host.all.star,
        xlab="host.all.star",ylab="Frequency",main="Bar Chart of host.all.star")

#48.categorical var:host.location.available
host.location.available.freq <- turo_tx %>%
  group_by(host.location.available)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(host.location.available.freq)
#host.location.available bar chart
barplot(host.location.available.freq$n,names.arg=host.location.available.freq$host.location.available,
        xlab="host.location.available",ylab="Frequency",main="Bar Chart of host.location.available")

#49.categorical var:host.verified.approved.to.drive
host.verified.approved.to.drive.freq <- turo_tx %>%
  group_by(host.verified.approved.to.drive)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(host.verified.approved.to.drive.freq)
#host.verified.approved.to.drive bar chart
barplot(host.verified.approved.to.drive.freq$n,names.arg=host.verified.approved.to.drive.freq$host.verified.approved.to.drive,
        xlab="host.verified.approved.to.drive",ylab="Frequency",main="Bar Chart of host.verified.approved.to.drive")

#50.categorical var:host.verified.email
host.verified.email.freq <- turo_tx %>%
  group_by(host.verified.email)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(host.verified.email.freq)
#host.verified.email bar chart
barplot(host.verified.email.freq$n,names.arg=host.verified.email.freq$host.verified.email,
        xlab="host.verified.email",ylab="Frequency",main="Bar Chart of host.verified.email")

#51.categorical var:host.verified.fb
host.verified.fb.freq <- turo_tx %>%
  group_by(host.verified.fb)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(host.verified.fb.freq)
#host.verified.fb bar chart
barplot(host.verified.fb.freq$n,names.arg=host.verified.fb.freq$host.verified.fb,
        xlab="host.verified.fb",ylab="Frequency",main="Bar Chart of host.verified.fb")


#52.categorical var:host.verified.phone
host.verified.phone.freq <- turo_tx %>%
  group_by(host.verified.phone)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(host.verified.phone.freq)
#host.verified.phone bar chart
barplot(host.verified.phone.freq$n,names.arg=host.verified.phone.freq$host.verified.phone,
        xlab="host.verified.phone",ylab="Frequency",main="Bar Chart of host.verified.phone")

#53.categorical var:car.city
car.city.freq <- turo_tx %>%
  group_by(car.city)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.city.freq)
#car.city bar chart
barplot(car.city.freq$n,names.arg=car.city.freq$car.city,
        xlab="car.city",ylab="Frequency",main="Bar Chart of car.city")


#evaluate car.trip.price as continuous variable or categorical varialbe
car.trip.price.freq <- turo_tx %>%
  group_by(car.trip.price)%>%
  summarise(n=n())%>%
  mutate(relative_freq=n / sum(n))
head(car.trip.price.freq)
#car.trip.price bar chart
barplot(car.trip.price.freq$n,names.arg=car.trip.price.freq$car.trip.price,
        xlab="car.trip.price",ylab="Frequency",main="Bar Chart of car.trip.price")


#Use Inter Quartile Range (IQR) method to identify outliers of all continuous variables, then remove all observations containing outliers.
#first, we use boxplot() function for virtual seen 19 continuous variables wether have outlier
#1.car.deliver.airport.num
ggplot(turo_tx)+
  aes(x="",y= car.deliver.airport.num)+
  geom_boxplot(fill="pink")+theme_minimal()

#2.car.deliver.hotel.num
ggplot(turo_tx)+
  aes(x="",y= car.deliver.hotel.num)+
  geom_boxplot(fill="pink")+theme_minimal()

#3.continuouse var:car.deliver.train.station.num
ggplot(turo_tx)+
  aes(x="",y= car.deliver.train.station.num)+
  geom_boxplot(fill="pink")+theme_minimal()

##6.Build a multiple linear regression model using car.trip.price as dependent variable. 
#Select at least five independent variables. Treat each categorical variable as 
#a single variable although it may be broken into multiple dummy variables. 
#Try different models and choose the best one you can find.
#model 2
#train categarical value
turo_train<-dummy_cols(turo_tx,select_columns = NULL)
print(turo_train)
#fix only 1 leve
values_count<-sapply(lapply(turo_train,unique),length)
values_count
#build model
turo_lm <- lm(car.trip.price ~ .,data =turo_train[, values_count>1])

turo_lm_summary<-summary(turo_lm)
turo_lm_summary
#choose 5 independent variables build model again 
turo_model_lm <- lm(car.trip.price ~ car.make+car.model+
                      car.extra.mile.fee+car.city+host.verified.phone,data =turo_tx)

turo_model_lm_summary<-summary(turo_model_lm)
options(max.print = 99999999)
turo_model_lm_summary
#Residual Plot
autoplot(turo_model_lm)
#model 3 
turo_model2_lm <- lm(car.trip.price ~ car.make+car.displayed.user.review.num+
                      car.extra.mile.fee+car.city+host.verified.phone,data =turo_tx)

turo_model2_lm_summary<-summary(turo_model2_lm)
options(max.print = 99999999)
turo_model2_lm_summary
#Residual Plot
autoplot(turo_model2_lm)