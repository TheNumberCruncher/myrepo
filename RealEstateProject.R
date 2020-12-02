red = read.table('/Users/shanemcintyre/Desktop/Github general/Github for Jobs/myrepo/Data/real-estate-data.txt', header = T)
attach(red)
head(red)
red = red[-148,]
Rent = red$RentTotal #Total annual rent of the lease
SqftLease = red$SqftLease #Size of the lease in square feet
#response
RentSqft = Rent/SqftLease
RentSqft
red = red[-c(1,2)]
red$RentSqft = RentSqft
RentSqft = red$RentSqft
Reno = red$Renovation #Number of years since last renovation
Leaselength = red$Leaselength #Length of the lease in years
Age = red$Age #Age of the building in years
DCity = red$DistCity #Distance to the center of the city center in miles
DAir = red$DistAirport #Distance to the airport in miles
DrAir = red$DriveAirp #Distance to the airport in driving time
Location = red$Location #One of three locations:  center of city, old/new suburb
Occ = red$Occupancy #Fraction of offices that are rented
Floors = red$FloorBldg #Number of floors in the building
SqftFloor = red$SqftFloor #Size of a floor in square feet
Elevator = red$Elevator #Number of elevators
Restaurant = as.numeric(red$Restaurant) #Yes, if the building has a restaurant
Wiring = as.numeric(red$Wiring) #Yes, if building has new wiring
Exercise = as.numeric(red$Exercise) #Yes, if building has a health club
DHos = red$DistHosp #Distance to nearest hospital in miles
FirmType = red$FirmType #Majority type of firms in the building (doctors, legal, business, government, other)
FloorLease = red$FloorLease #The (lowest) floor where the lease is located
Renewable = as.numeric(red$Renewable) #Yes, if the lease is renewable
Parking = red$Parking #Number of executive parking spaces included in rental
Restaurant[Restaurant=="1"] = 0
Restaurant[Restaurant=="2"] = 1
Wiring[Wiring=="1"] = 0
Wiring[Wiring=="2"] = 1
Exercise[Exercise=="1"] = 0
Exercise[Exercise=="2"] = 1
Renewable[Renewable=="1"] = 0
Renewable[Renewable=="2"] = 1
#Fixed Costs
#DistCity
DCity = DCity/SqftLease
#DistAirp
DAir = DAir/SqftLease
#DriveAirp
DrAir = DrAir/SqftLease
#DistHosp
DHos = DHos/SqftLease
#Elevator
Elevator = Elevator/SqftLease
#Parking
Parking = Parking/SqftLease

lm1 = lm(RentSqft~log(Reno+1)+log(Leaselength-2)+Age+log(DCity)+log(DAir)+DrAir+Location+Occ+Floors+SqftFloor+Elevator+Restaurant+Wiring+Exercise+log(DHos)+FirmType+FloorLease+Renewable+Parking)
summary(lm1)
summary(lm(RentSqft~Occ))
summary(lm(RentSqft~log(Reno+1)))
summary(lm(RentSqft~log(Leaselength+2)))
summary(lm(RentSqft~log(DAir)))
summary(lm(RentSqft~Location))
summary(lm(RentSqft~Wiring))
summary(lm(RentSqft~Parking))
summary(lm(RentSqft~Age))
summary(lm(RentSqft~log(DCity)))
summary(lm(RentSqft~log(DrAir)))
summary(lm(RentSqft~Floors))
summary(lm(RentSqft~SqftFloor))
summary(lm(RentSqft~Elevator))
summary(lm(RentSqft~Restaurant))
summary(lm(RentSqft~Exercise))
summary(lm(RentSqft~log(DHos)))
summary(lm(RentSqft~FirmType))
summary(lm(RentSqft~FloorLease))
summary(lm(RentSqft~Renewable))
model = lm(RentSqft~Occ+log(Reno+1)+log(Leaselength+2)+log(DAir)+Location+Wiring+Parking+Age+log(DCity)+Floors+SqftFloor+log(DHos)+FloorLease+Renewable)
summary(model)
#F test
Rsqrdfm = 0.6576
dffm = 175
Rsqrdrm = 0.5837
dfrm = 183
Fval = ((Rsqrdfm - Rsqrdrm)/(dfrm - dffm))/((1 - Rsqrdfm)/(dffm))
Fval
p = 1-pf(Fval,(dfrm - dffm),dffm)
p
#prediction intervals
sqft=50000
pdc=predict(model,newdata=data.frame(SqftFloor=50000,Occ=.8,Reno=4,Leaselength=3,DAir=10/sqft,Location="SUBNEW",Wiring=1,Parking=10/sqft,Age=20,DCity=10/sqft,Floors=10,DHos=5/sqft,FloorLease=2,Renewable=1), interval='predict')
pdc
pdc*sqft
pdc=predict(model,newdata=data.frame(SqftFloor=50000,Occ=.8,Reno=4,Leaselength=10,DAir=10/sqft,Location="SUBNEW",Wiring=1,Parking=10/sqft,Age=20,DCity=10/sqft,Floors=10,DHos=5/sqft,FloorLease=2,Renewable=1), interval='predict')
pdc
pdc*sqft
plot(RentSqft~Occ)
plot(RentSqft~Reno)
plot(RentSqft~Leaselength)
plot(RentSqft~DAir)
plot(RentSqft~Location)
plot(RentSqft~Wiring)
plot(RentSqft~Parking)
plot(RentSqft~Age)
plot(RentSqft~DCity)
plot(RentSqft~Floors)
plot(RentSqft~SqftFloor)
plot(RentSqft~DHos)
plot(RentSqft~FloorLease)
plot(RentSqft~Renewable)
#checking assumptions for mlr
plot(model)

