###########################################################################
########  SENSITIVITY CHECKS  ####################################
###########################################################################

########################## STRATIFY BY  AGE  #######################


# Split the sample by child's age
table(data$agechild)

# Create categories of age

data$agechild_cat<-data$agechild
data$agechild_cat[data$agechild<=10 ] <-0 # under ten
data$agechild_cat[data$agechild>=10 ] <-1 # over ten
table(data$agechild_cat)

# Split sample
split<-split(data, f=data$agechild_cat)
under<-split$`0`
over<-split$`1`


m0<- feols(recoz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , under)

m1<- feols(recoz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , over)

m2<- feols(mathz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  ,under)

m3<- feols(mathz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , over)


# Table:

etable_combined<-etable(m0, m1, m2, m3, digits="r2",
                        signif.code = signif_codes)
print(etable_combined)

########################## STRATIFY BY LENGTH OF EXPOSURE  #######################

# Create a variable for long exposure:
data$lag_povstatus<-lag(data$povstatus) #create the lag variable
data$long_exposure<-NA
data$long_exposure[data$povstatus==1 & data$lag_povstatus==1]<-1
data$long_exposure[data$povstatus!=1 | data$lag_povstatus!=1]<-0
table(data$long_exposure)

# Models
m0<- feols(recoz~totalweeksemployed*long_exposure+ famsize + marstatus +year|
             ID  , data)

m1<- feols(mathz~totalweeksemployed*long_exposure + famsize + marstatus +year|
             ID  , data)

m2<- feols(cognz~totalweeksemployed*long_exposure + famsize + marstatus +year|
             ID  , data)

m3<- feols(emotz~totalweeksemployed*long_exposure + famsize + marstatus +year|
             ID  , data)


# Table:

etable_combined<-etable(m0, m1, m2, m3, digits="r2",
                        signif.code = signif_codes)
print(etable_combined)

########################## STRATIFY BY GENDER #######################

# Split the sample by child's gender
split<-split(data, f=data$sex)
boys<-split$`1`
girls<-split$`2`

m0<- feols(recoz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , boys)
etable(m0)

m1<- feols(recoz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , girls)
etable(m1)

m2<- feols(mathz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , boys)
etable(m2)

m3<- feols(mathz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , girls)
etable(m3)

m4<- feols(cognz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , boys)

m5<- feols(cognz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , girls)

m6<- feols(emotz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  ,boys)

m7<- feols(emotz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , girls)

# Table:

etable_combined<-etable(m0, m1, m2, m3, m4, m5, m6, m7, digits="r2",
                        signif.code = signif_codes)
print(etable_combined)


########################## STRATIFY BY RACE  #######################

# Split the sample by child's race
table(data$race)
split<-split(data, f=data$race)
hisp<-split$`1`
black<-split$`2`
white<-split$`3`

# Models
m0<- feols(recoz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , hisp)
etable(m0)

m1<- feols(recoz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , black)
etable(m1)

m2<- feols(recoz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , white)
etable(m2)

m3<- feols(mathz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , hisp)
etable(m3)

m4<- feols(mathz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , black)
etable(m4)

m5<- feols(mathz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , white)
etable(m5)

m6<- feols(cognz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , hisp)
etable(m6)

m7<- feols(cognz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , black)
etable(m7)

m8<- feols(cognz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , white)
etable(m8)

m9<- feols(emotz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , hisp)
etable(m9)

m10<- feols(emotz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , black)
etable(m10)

m11<- feols(emotz~totalweeksemployed*povstatus + famsize + marstatus +year|
             ID  , white)
etable(m11)



# Table:

etable_combined<-etable(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, digits="r2",
                        signif.code = signif_codes)
print(etable_combined)



