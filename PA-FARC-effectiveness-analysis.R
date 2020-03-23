########How effective have been guerrilla occupation and protected areas in avoiding deforestation in Colombia?#######

#Packages
require(raster)
require(sp)

####Creating spatial data frame####
#Raster
elevation <- raster::raster('elevation-colombia.tif')
slope <- raster::raster("slope-colombia.tif")
forest2000 <- raster::raster("floresta-20002.tif")
forest2017 <- raster::raster("bosque-20171.tif")
coca2000 <- raster::raster("coca-2000.tif")
farc2000 <- raster::raster("FARC-2000nov.tif") 
col <- raster::raster("Colombia.tif")
distcit05 <- raster::raster("Dist-Cid05.tif")
distroad05 <- raster::raster("Dist-Est05.tif")
distrivers <- raster::raster("Dist-rios.tif")
salinization <- raster::raster("Salinization.tif")
fertility <- raster::raster("Soil-fertility.tif")
PA <- raster::raster("PAs2000.tif")
buffer.farc <- raster::raster("Buffer-FARC.tif")
buffer.pa <- raster::raster("Buffer-PA.tif")

#Creating stack
colombia<-raster::stack(forest2000, forest2017, coca2000, farc2000, col,
          distcit05, distroad05, distrivers, elevation, slope, fertility,
          salinization, PA, buffer.farc, buffer.pa)

#Forest = 1, no forest = 2, without information = 3

#Creating spatial dataframe
#datafr.col<-rasterToPoints(colombia, spatial=T)
datafr.col <- as.data.frame(colombia, xy=T)
str(datafr.col)
summary(datafr.col)
#Subset without NAs
datafr.col2 <- subset(datafr.col,!is.na(datafr.col$Colombia_category ))

####Dealing with variables and NAs####
##Buffer PA variable
#Factor -> Character
datafr.col2$Buffer.PA_category <- as.character
                                 (datafr.col2$Buffer.PA_category) 
#Variable has NA and text. Here, I replace text to 1
datafr.col2$Buffer.PA_category[datafr.col2$Buffer.PA_category != ""] <- "1" 
#Character -> numeric
datafr.col2$Buffer.PA_category <- as.numeric(datafr.col2$Buffer.PA_category) 
#NA to 0
datafr.col2$Buffer.PA_category[is.na(datafr.col2$Buffer.PA_category[])] <- 0

##Buffer FARC variable
#Factor -> character
datafr.col2$Buffer.FARC_category <- as.character(datafr.col2$
                                                   Buffer.FARC_category) 
#Variable has 0 and NA. Here, I replace 0 to 1
datafr.col2$Buffer.FARC_category[datafr.col2$Buffer.FARC_category == 0]<-1 

#Character -> numeric
datafr.col2$Buffer.FARC_category <- as.numeric(datafr.col2$
                                                 Buffer.FARC_category)
#NA to 0
datafr.col2$Buffer.FARC_category[is.na(datafr.col2$
                                         Buffer.FARC_category[])] <- 0 

##FARC presence variable
#Factor -> character 
datafr.col2$FARC.2000nov_category <- as.character(datafr.col2$
                                                    FARC.2000nov_category)
#NA to 0
datafr.col2$FARC.2000nov_category[is.na(datafr.col2$
                                          FARC.2000nov_category[])] <- 0
#Character -> numeric
datafr.col2$FARC.2000nov_category <- as.numeric(datafr.col2$
                                                  FARC.2000nov_category)

##PA variable
#NA to 0
datafr.col2$PAs2000[is.na(datafr.col2$PAs2000[])] <- 0

##Forest 2000 variable
#NA to 0
datafr.col2$floresta.20002[is.na(datafr.col2$floresta.20002[])] <- 0

##Forest 2017 variable
#NA to 0
datafr.col2$bosque.20171[is.na(datafr.col2$bosque.20171[])] <- 0

##Coca crop variable
#Factor -> Character
datafr.col2$coca.2000_category <- as.character(datafr.col2$
                                                 coca.2000_category)
#Variable has numbers and blank cells. Here, I replace blank cells to 0
datafr.col2$coca.2000_category[datafr.col2$coca.2000_category == ""] <- "0"

#Character -> numeric
datafr.col2$coca.2000_category <- as.numeric(datafr.col2$
                                               coca.2000_category)
#NA to 0
datafr.col2$coca.2000_category[is.na(datafr.col2$
                                       coca.2000_category[])] <- 0

##Soil fertility variable
#Factor -> Character
datafr.col2$Soil.fertility_category <- as.character(datafr.col2$
                                                  Soil.fertility_category)
#NA to 0
datafr.col2$Soil.fertility_category[is.na(datafr.col2$
                                        Soil.fertility_category[])] <- 0

##Soil salinization variable
#NA to 0
datafr.col2$Salinization[is.na(datafr.col2$Salinization[])] <- 0

#Presence/absence variables (FARC, PA, buffer and forest 2017)

#Creating new FARC and PA columns
datafr.col2["is.farc"] <- NA
datafr.col2["is.pa"] <- NA
#Filling with FARC and PA information
datafr.col2["is.farc"] <- datafr.col2$FARC.2000nov_category
datafr.col2["is.pa"] <- datafr.col2$PAs2000
#Replacing to 0 and 1
datafr.col2$is.farc[datafr.col2$is.farc != 0] <- 1
datafr.col2$is.pa[datafr.col2$is.pa != 0] <- 1

#Creating buffer columns
datafr.col2["buffer.farc"] <- NA
datafr.col2["buffer.pa"] <- NA

#Filling buffer columns with sum of FARC/PA and buffer area
datafr.col2["buffer.farc"] <- datafr.col2$is.farc + datafr.col2$Buffer.FARC_category
datafr.col2["buffer.pa"] <- datafr.col2$is.pa + datafr.col2$Buffer.PA_category

#Taking off buffer area overlapped with FARC/PA area. Here, I replace cells with "2" to "0". This is because 2 means overlapped area of PA/FARC and buffer  
datafr.col2$buffer.farc[datafr.col2$buffer.farc == 2] <- 0
datafr.col2$buffer.pa[datafr.col2$buffer.pa == 2] <- 0

#Saving data frame
write.table(datafr.col2, "analysis-col2.txt", sep = ";", na = "0")

#Reading the data frame
col <- read.table("analysis-col2.txt", sep = ";")
head(col)
names(col)
summary(col)

#Replacing column names
names(col)<- c("x", "y", "forest2000", "forest2017", "coca2000", "farc2000", "colombia", "distcit05","distroad05", "distrivers", "elevation", "slope", "soil.fertility", "salinization", "PA", "buffer.farc.fill", "buffer.pa.fill", "is.farc", "is.pa", "buffer.farc", "buffer.pa")

names(col)
dim(col)

#Separating forest and no forest 
#Forest = 1, no forest = 2, without information = 3
#0 were created... Here, I replace 0 for 2 (2 represents no forest)
col$forest2000[col$forest2000 == 0] <- 2 
#Same for 2017
col$forest2017[col$forest2017 == 0] <- 2 
summary(col)

#Choosing forested areas
col.forest2000 <- col[col$forest2000 == 1, ]

#Creating new colum: is.forest2017 = Did the land cover change? 0 means forested area and 1 deforested area
col.forest2000["is.forest2017"] <- col.forest2000$forest2017
col.forest2000$is.forest2017[col.forest2000$is.forest2017 == 1] <- 0  
col.forest2000$is.forest2017[col.forest2000$is.forest2017 != 0] <- 1 


#Saving dataframe 
write.table(col.forest2000, "Forested-areas-2000.txt", sep = ";", na = "0")
col.forest2000<- read.table("Forested-areas-2000.txt", sep = ";")
#Reading forest 2000 dataframe
#col.forest2000 <- read.table("Forested-areas-2000.txt", sep = ";")

#Creating new column = PA + FARC overlap
col.forest2000["pa.farc"] <- NA
col.forest2000["pa.farc"] <- col.forest2000$is.pa + col.forest2000$is.farc

#Subsets 
#Without overlap PA - FARC
for.without.overlap<-subset(col.forest2000,col.forest2000$pa.farc < 2)
#With overlap PA - FARC
for.overlap <- subset(col.forest2000, col.forest2000$pa.farc == 2) #It has not buffer areas

#without buffer
#Taking off FARC buffer
forest.buffer.a <- for.without.overlap[for.without.overlap$buffer.farc == 0,]
#Taking off PA buffer
forest.buffer <- forest.buffer.a[forest.buffer.a$buffer.pa == 0,]

#Saving subset without buffer and without overlap
write.table(forest.buffer, "Forested-areas-without-buffer.txt",  sep = ";", na = "0")
forest.buffer <- read.table("Forested-areas-without-buffer.txt", sep = ";")

####Separating FARC, PA, overlapped and control data frames####
#Now, we will use the forest.buffer data frame to separate FARC areas, PA areas and control areas

#Subset of FARC areas
forest.farc <- forest.buffer[forest.buffer$is.farc == 1,] 
#Subset of PA areas
forest.pa <- forest.buffer[forest.buffer$is.pa == 1,] 
#Subset overlapped areas
for.overlap

#Saving subsets
write.table(forest.farc, "Forested-areas-farc.txt",  sep = ";", na = "0")
write.table(forest.pa, "Forested-areas-pa.txt",  sep = ";", na = "0")
write.table(for.overlap, "Forested-areas-overlapped.txt",  sep = ";", 
            na = "0")

##Control areas
#Total treatment areas = 8879
#% FARC areas (4274/8879) = 48.14%
#% PA areas (3462/8879) = 38.99%
#%Overlapped areas (1143/8879) = 12.87%
#Total control areas = 15101

#Creating a subset for control areas
forest.buffer["control"] <- NA
#Filling column with the sum of PA and FARC areas
forest.buffer["control"] <- forest.buffer$is.farc + forest.buffer$is.pa
#Subset of control areas
forest.control <- forest.buffer[forest.buffer$control == 0,]

#Saving control areas data frame
write.table(forest.control, "Forested-areas-control.txt",  sep = ";", na = "0")
#Now, we selected a percentage of control areas for FARC and for PA. Percentage is proportional to each treatment size

#Selecting 48% of control areas for FARC
control.farc.id <- sample(1:nrow(forest.control), 
                          nrow(forest.control)*0.48)

#Separating FARC control areas
forest.control.farc <- forest.control[control.farc.id,]
forest.farcb<- forest.farc
forest.farcb["control"] <- NA
forest.farcb["control"] <- 2 

#Subset without control farc cells (Here, we taked off the 48% of control cells to made FARC control areas. Then, we subset forest.control without those cells and made the division to PA and overlapped areas. For this, we recalculated the % and made another aleatorization)
forest.controlb <- forest.control[-control.farc.id,]
#Total treatment areas = 4605
#% PA areas (3462/4605) = 75.18%%
#%Overlapped areas (1143/4605) = 24.82%

#Separating PA control areas
control.pa.id <- sample(1:nrow(forest.controlb), nrow(forest.controlb)*0.75)
forest.control.pa <- forest.controlb[control.pa.id,]
#Adding control column to dataframe
forest.pab <- forest.pa
forest.pab["control"] <- NA
forest.pab["control"] <- 2 

#Separating overlapped control areas
forest.control.overlap <- forest.controlb[-control.pa.id,]
#1 = control, 2 = no control
#Adding control column to dataframe
for.overlapb <- for.overlap
for.overlapb["control"] <- NA
for.overlapb["control"] <- 2 


nrow(forest.control.farc)+nrow(forest.control.pa)+
  nrow(forest.control.overlap)

#FARC areas
farc.areas <- rbind(forest.farcb, forest.control.farc) #12579 lines
nrow(forest.farc) #4274 lines
nrow(forest.control.farc) #7797
write.table(farc.areas, "FARC-areas.txt", sep = ";", na = "0")
farc.areas <- read.table("FARC-areas.txt", sep = ";")

#PA areas
pa.areas <- rbind(forest.pab, forest.control.pa) #10258 lines
nrow(forest.pa) #3462 lines
nrow(forest.control.pa) #6335
write.table(pa.areas, "PA-areas.txt", sep = ";", na = "0")
pa.areas <- read.table("PA-areas.txt", sep = ";")

#Overlapped areas
ov.areas <- rbind(for.overlapb, forest.control.overlap) #3255 lines
nrow(for.overlap) #1143 lines
nrow(forest.control.overlap) #2112
write.table(ov.areas, "Overlapped-areas.txt", sep = ";", na = "0")

####Separating by soil fertility####
#Number codes (in parenthesis the object name)
#3 = High (fert.high)
#5 = Low (fert.low)
#11 = Very high (fert.very.high)
#12 = very low (fert.v.low)
#9 = Medium
#15 Waterbodies

#Separating by fertility - FARC areas
fert.high.farc <- farc.areas[farc.areas$soil.fertility == 3, ]
fert.low.farc <- farc.areas[farc.areas$soil.fertility == 5, ]
fert.medium.farc <- farc.areas[farc.areas$soil.fertility == 9, ]
fert.v.high.farc <- farc.areas[farc.areas$soil.fertility == 11, ]
fert.v.low.farc <- farc.areas[farc.areas$soil.fertility == 12, ]


#Separating by fertility - PA areas
fert.high.pa <- pa.areas[pa.areas$soil.fertility == 3, ]
fert.low.pa <- pa.areas[pa.areas$soil.fertility == 5, ]
fert.medium.pa <- pa.areas[pa.areas$soil.fertility == 9, ]
fert.v.high.pa <- pa.areas[pa.areas$soil.fertility == 11, ]
fert.v.low.pa <- pa.areas[pa.areas$soil.fertility == 12, ]

#Separating by fertility - overlapped areas
fert.high.ov <- ov.areas[ov.areas$soil.fertility == 3, ]
fert.low.ov <- ov.areas[ov.areas$soil.fertility == 5, ]
fert.medium.ov <- ov.areas[ov.areas$soil.fertility == 9, ]
fert.v.high.ov <- ov.areas[ov.areas$soil.fertility == 11, ]
fert.v.low.ov <- ov.areas[ov.areas$soil.fertility == 12, ]
#We only have pixels for high, medium, low and very low fertility

####MATCHING####
require(MatchIt)

####Matching - PA####
#High fertility - PA
high.fert.pa <- matchit(is.pa ~ elevation + slope + salinization +
                          distcit05 + distrivers + distroad05, 
                        data = fert.high.pa, method = "nearest", 
                        ratio = 1, calliper = 0.2)

#Criating matrix 
match.m<-cbind(rownames(high.fert.pa$match.matrix),
               high.fert.pa$match.matrix)

#Who are pairs?
match.highf.pa <- match.data(high.fert.pa, distance = "pscore")

head(match.highf.pa)

#Data frame PA high fertility with matched pixels
df.pa.high<-NULL

for (i in 1:nrow(match.m)){
  print(i)
t <- cbind(match.highf.pa[match.m[i,1],],
           match.highf.pa[match.m[i,2],])

df.pa.high<-rbind(df.pa.high,t)
}

#Low fertility - PA
low.fert.pa <- matchit(is.pa ~ elevation + slope + salinization + distcit05
              + distrivers + distroad05, data = fert.low.pa,
              method = "nearest", ratio = 1, calliper = 0.2)

#Criating matrix 
match.m<-cbind(rownames(low.fert.pa$match.matrix),
               low.fert.pa$match.matrix)

#Who are pairs?
match.lowf.pa <- match.data(low.fert.pa, distance = "pscore")
#Dim = 3970 27

head(match.lowf.pa)

#Data frame PA low fertility with matched pixels
df.pa.low<-NULL

for (i in 1:nrow(match.m)){
  print(i)
  t <- cbind(match.lowf.pa[match.m[i,1],],match.lowf.pa[match.m[i,2],])
  
  df.pa.low<-rbind(df.pa.low,t)
}

#Taking off NA caused by unmatched pairs
df.pa.low$forest2000[is.na(df.pa.low[,3][])] <- 0
df.pa.lowf <- subset(df.pa.low, df.pa.low[,3] > 0)

#Medium fertility - PA
medium.fert.pa <- matchit(is.pa ~ elevation + slope + salinization +
                  distcit05 + distrivers + distroad05, 
                  data = fert.medium.pa, method = "nearest", 
                  ratio = 1, calliper = 0.2)

#Criating matrix 
match.m < -cbind(rownames(medium.fert.pa$match.matrix),medium.fert.pa$
                   match.matrix)

#Who are pairs?
match.medf.pa <- match.data(medium.fert.pa, distance = "pscore")
#Dim = 3970 27

#Data frame PA medium fertility with matched pixels
df.pa.medium<-NULL

for (i in 1:nrow(match.m)){
  print(i)
  t<-cbind(match.medf.pa[match.m[i,1],],match.medf.pa[match.m[i,2],])
  
  df.pa.medium<-rbind(df.pa.medium,t)
}
View(df.pa.medium)

#Very low fertility - PA
vlow.fert.pa <- matchit(is.pa ~ elevation + slope + salinization +
                distcit05 + distrivers + distroad05, data = fert.v.low.pa,
                method = "nearest", ratio = 1, calliper = 0.2)

#Criating matrix 
match.m<-cbind(rownames(vlow.fert.pa$match.matrix),vlow.fert.pa$
                 match.matrix)

#Who are pairs?
match.vlowf.pa <- match.data(vlow.fert.pa, distance = "pscore")

#Data frame PA very low fertility with matched pixels
df.pa.vlow<-NULL

for (i in 1:nrow(match.m)){
  print(i)
  t <- cbind(match.vlowf.pa[match.m[i,1],],match.vlowf.pa[match.m[i,2],])
  
  df.pa.vlow<-rbind(df.pa.vlow,t)
}
View(df.pa.vlow)

####Matching FARC####
#We joined high and medium fertility for FARC areas, because we had a small N

#High and medium fertility - FARC
#Joining high and medium fertility
fert.hm.farc <- rbind(fert.high.farc, fert.medium.farc)

#Matching
hm.fert.farc <- matchit(is.farc ~ elevation + slope + salinization +
                distcit05 + distrivers + distroad05 + coca2000, 
                data = fert.hm.farc, method = "nearest", ratio = 1,
                calliper = 0.2)

#Criating matrix 
match.m <- cbind(rownames(hm.fert.farc$match.matrix),hm.fert.farc$
                   match.matrix)

#Who are pairs?
match.hmf.farc <- match.data(hm.fert.farc, distance = "pscore")

#Data frame FARC high fertility with matched pixels
df.farc.hm<-NULL

for (i in 1:nrow(match.m)){
  print(i)
  t <- cbind(match.hmf.farc[match.m[i,1],],match.hmf.farc[match.m[i,2],])
  
  df.farc.hm<-rbind(df.farc.hm,t)
}


#Taking off NAs caused by unmatched pairs
df.farc.hm$forest2000[is.na(df.farc.hm[,3][])] <- 0
df.farc.hmf <- subset(df.farc.hm, df.farc.hm[,3] > 0)

#Low fertility - FARC
low.fert.farc <- matchit(is.farc ~ elevation + slope + salinization +
                 distcit05 + distrivers + distroad05 + coca2000, 
                 data = fert.low.farc, method = "nearest", ratio = 1,
                 calliper = 0.2)

#Criating matrix 
match.m <- cbind(rownames(low.fert.farc$match.matrix),low.fert.farc$
                   match.matrix)

#Who are pairs?
match.lowf.farc <- match.data(low.fert.farc, distance = "pscore")

head(match.lowf.farc)

#Data frame FARC low fertility with matched pixels
df.farc.low<-NULL

for (i in 1:nrow(match.m)){
  print(i)
  t <- cbind(match.lowf.farc[match.m[i,1],],match.lowf.farc[match.m[i,2],])
  
  df.farc.low<-rbind(df.farc.low,t)
}


#Taking off NAs caused by unmatched pairs
df.farc.low$forest2000[is.na(df.farc.low[,3][])] <- 0
df.farc.lowf <- subset(df.farc.low, df.farc.low[,3] > 0)

#Very low fertility - FARC
vlow.fert.farc <- matchit(is.farc ~ elevation + slope + salinization +
                  distcit05 + distrivers + distroad05 + coca2000,
                  data = fert.v.low.farc, method = "nearest", 
                  ratio = 1, calliper = 0.2)

#Criating matrix 
match.m <- cbind(rownames(vlow.fert.farc$match.matrix),vlow.fert.farc$
                   match.matrix)

#Who are pairs?
match.vlowf.farc <- match.data(vlow.fert.farc, distance = "pscore")

#Data frame FARC very low fertility with matched pixels
df.farc.vlow<-NULL

for (i in 1:nrow(match.m)){
  print(i)
  t <- cbind(match.vlowf.farc[match.m[i,1],],
             match.vlowf.farc[match.m[i,2],])
  
  df.farc.vlow<-rbind(df.farc.vlow,t)
}

#Taking off NAs caused by unmatched pairs
df.farc.vlow$forest2000[is.na(df.farc.vlow[,3][])] <- 0
df.farc.vlowf <- subset(df.farc.vlow, df.farc.vlow[,3] > 0)

####Matching overlapped FARC - PA areas####
#We joined all types of fertility for overlapped areas, because we had N too small

#Matching
all.fert.ov <- matchit(is.pa ~ elevation + slope + salinization + 
                         distcit05 + distrivers + distroad05 + coca2000,
                       data = ov.areas, method = "nearest", ratio = 1,
                       calliper = 0.2)

#Criating matrix 
match.m <- cbind(rownames(all.fert.ov$match.matrix),all.fert.ov$
                   match.matrix)

#Who are pairs?
match.ov <- match.data(all.fert.ov, distance = "pscore")

#Data frame OA with matched pixels
df.ov<-NULL

for (i in 1:nrow(match.m)){
  print(i)
  t <- cbind(match.ov[match.m[i,1],],match.ov[match.m[i,2],])
  
  df.ov<-rbind(df.ov,t)
}
table(df.ov$forest2000)


#joining PA, FARC and overlapped (overlapped is already joined)

#PA
#Names are not equal. First, I changed column names
names(df.pa.lowf) <- c("x", "y", "forest2000", "forest2017", "coca2000", "farc2000", "colombia", "distcit05","distroad05", "distrivers", "elevation", "slope", "soil.fertility", "salinization", "PA", "buffer.farc.fill", "buffer.pa.fill", "is.farc", "is.pa", "buffer.farc", "buffer.pa", "is.forest2017", "pa.farc", "control", "pscore", "weights", "x", "y", "forest2000", "forest2017", "coca2000", "farc2000", "colombia", "distcit05","distroad05", "distrivers", "elevation", "slope", "soil.fertility", "salinization", "PA", "buffer.farc.fill", "buffer.pa.fill", "is.farc", "is.pa", "buffer.farc", "buffer.pa", "is.forest2017", "pa.farc", "control", "pscore", "weights")

df.pa <- rbind(df.pa.high, df.pa.lowf, df.pa.medium, df.pa.vlow)
write.table(df.pa, "df.pa.txt", sep = ";")

#FARC
names(df.farc.hmf) <- c("x", "y", "forest2000", "forest2017", "coca2000", "farc2000", "colombia", "distcit05","distroad05", "distrivers", "elevation", "slope", "soil.fertility", "salinization", "PA", "buffer.farc.fill", "buffer.pa.fill", "is.farc", "is.pa", "buffer.farc", "buffer.pa", "is.forest2017", "pa.farc", "control", "pscore", "weights", "x", "y", "forest2000", "forest2017", "coca2000", "farc2000", "colombia", "distcit05","distroad05", "distrivers", "elevation", "slope", "soil.fertility", "salinization", "PA", "buffer.farc.fill", "buffer.pa.fill", "is.farc", "is.pa", "buffer.farc", "buffer.pa", "is.forest2017", "pa.farc", "control", "pscore", "weights")

names(df.farc.lowf) <- c("x", "y", "forest2000", "forest2017", "coca2000", "farc2000", "colombia", "distcit05","distroad05", "distrivers", "elevation", "slope", "soil.fertility", "salinization", "PA", "buffer.farc.fill", "buffer.pa.fill", "is.farc", "is.pa", "buffer.farc", "buffer.pa", "is.forest2017", "pa.farc", "control", "pscore", "weights", "x", "y", "forest2000", "forest2017", "coca2000", "farc2000", "colombia", "distcit05","distroad05", "distrivers", "elevation", "slope", "soil.fertility", "salinization", "PA", "buffer.farc.fill", "buffer.pa.fill", "is.farc", "is.pa", "buffer.farc", "buffer.pa", "is.forest2017", "pa.farc", "control", "pscore", "weights")
names(df.farc.vlowf) <- c("x", "y", "forest2000", "forest2017", "coca2000", "farc2000", "colombia", "distcit05","distroad05", "distrivers", "elevation", "slope", "soil.fertility", "salinization", "PA", "buffer.farc.fill", "buffer.pa.fill", "is.farc", "is.pa", "buffer.farc", "buffer.pa", "is.forest2017", "pa.farc", "control", "pscore", "weights", "x", "y", "forest2000", "forest2017", "coca2000", "farc2000", "colombia", "distcit05","distroad05", "distrivers", "elevation", "slope", "soil.fertility", "salinization", "PA", "buffer.farc.fill", "buffer.pa.fill", "is.farc", "is.pa", "buffer.farc", "buffer.pa", "is.forest2017", "pa.farc", "control", "pscore", "weights")

df.farc <- rbind(df.farc.hmf, df.farc.lowf, df.farc.vlowf)
write.table(df.farc, "df.farc.txt", sep = ";")


t.test(df.pa[,22], df.pa[,48])
t.test(df.farc[,22], df.farc[,48])
t.test(df.ov[,22], df.ov[,48])

####Comparison PAs####
##Creating data frame

#columns
#PA id
pa.id <- c(0, 2, 3, 8, 13, 16, 18, 19, 21, 23, 24, 25, 26, 27, 33, 36, 39,
           40, 41, 45, 46, 48, 50, 52, 54, 58, 103, 104, 105, 107, 125)
#Area
area <- tapply(df.pa[,19], df.pa[,15], FUN = sum) # column 19 is is.pa, column 15 is PA id
#Treatment
tr <- tapply(df.pa[,22], df.pa[,15], FUN = sum) #Column 22 is is.forest.2017 for treatment, column 15 is PA id
#Control
cont <- tapply(df.pa[,48], df.pa[,15], FUN = sum) #Column 48 is is.forest.2017 for treatment, column 15 is PA id

#Joining columns
pa <- cbind(pa.id, area, tr, cont)
pa <- as.data.frame(pa)

#Treatment/area
pa["tr.area"] <- NA
pa["tr.area"] <- pa$tr/pa$area

#Control/area
pa["cnt.area"] <- NA
pa["cnt.area"] <- pa$cont/pa$area

#Impact (c-t/a)
pa["impact"] <- NA
pa["impact"] <- pa$cnt.area - pa$tr.area

View(pa)

pa.comparison <- subset(pa, pa$pa.id>0)
View(pa.comparison)

#Saving
write.table(pa.comparison, "PA-comparison.txt", sep = ";")
pa.comparison <- read.table("PA-comparison.txt", sep = ";")

####Wilcoxon tests - PA####
#Wilcoxon comparing treatment vs. control (control different from treatment?)
# All areas 
wilcox.test(pa.comparison$tr.area, pa.comparison$cnt.area, 
            paired = T,alternative ="less")
boxplot(pa.comparison$tr.area, pa.comparison$cnt.area, main = "PA tr vs. control", xlab = c("Treatment", "Control"), ylab = "Effect")

#Big areas
#Creating subset of big areas
pa.gde<-subset(pa.comparison,pa.comparison$area>10)
write.table(pa.gde, "Big-PA.txt", sep = ";")
pa.gde <- read.table("Big-PA.txt", sep = ";")
#Test
wilcox.test(pa.gde$tr.area, pa.gde$cnt.area ,alternative ="less")
boxplot(pa.gde$tr.area, pa.gde$cnt.area, main = "big PA tr vs. control", xlab = c("Treatment", "Control"), ylab = "Effect")

#Wilcoxon for impact (impact different from 0?)
#All areas
wilcox.test(pa.comparison$impact ,alternative ="greater")
boxplot(pa.comparison$impact, main = "PA impact", xlab = "PA", ylab = "Impact")

#Big areas
wilcox.test(pa.gde$impact ,alternative ="greater")
boxplot(pa.gde$impact, main = "Big PA impact",  xlab = "PA", ylab = "Impact")


####Comparison FARC areas####
##Creating data frame
#columns
#FARC id
farc.id <- c(1, 2, 3, 4, 6, 7, 8, 9, 10, 12, 13, 14, 15, 18, 19, 21, 22, 
             23, 25, 26, 29, 30, 31, 32, 33, 34)
#Area
area <- tapply(df.farc[,18], df.farc[,6], FUN = sum) # column 18 is is.farc, column 6 is FARC id (farc2000)
#Treatment
tr <- tapply(df.farc[,22], df.farc[,6], FUN = sum) #Column 22 is is.forest.2017 for treatment, column 6 is FARC id (farc2000)
#Control
cont <- tapply(df.farc[,48], df.farc[,6], FUN = sum) #Column 48 is is.forest.2017 for treatment, column 6 is FARC id (farc2000)

#Joining columns
farc <- cbind(farc.id, area, tr, cont)
farc <- as.data.frame(farc)

#Treatment/area
farc["tr.area"] <- NA
farc["tr.area"] <- farc$tr/farc$area

#Control/area
farc["cnt.area"] <- NA
farc["cnt.area"] <- farc$cont/farc$area

#Impact (c-t/a)
farc["impact"] <- NA
farc["impact"] <- farc$cnt.area - farc$tr.area

View(farc)

#Saving
write.table(farc, "FARC-comparison.txt", sep = ";")
farc.comparison <- read.table("FARC-comparison.txt", sep = ";")

####Wilcoxon tests - FARC####
#Wilcoxon comparing treatment vs. control (control different from treatment?)
# All areas 
wilcox.test(farc.comparison$tr.area, farc.comparison$cnt.area, 
            paired = T,alternative ="less")
boxplot(farc.comparison$tr.area, farc.comparison$cnt.area, 
        main = "FARC tr vs control",  xlab = c("Treatment", "Control"),
        ylab = "Impact")

#Big areas
#Creating subset of big areas
farc.gde<-subset(farc.comparison,farc.comparison$area>10)
write.table(farc.gde, "Big-FARC.txt", sep = ";")
farc.gde <- read.table("Big-FARC.txt", sep = ";")

#Test
wilcox.test(farc.gde$tr.area, farc.gde$cnt.area ,alternative ="less")
boxplot(farc.gde$tr.area, farc.gde$cnt.area, 
        main = "Big FARC tr vs control", xlab = c("Treatment", "Control"),
        ylab = "Impact")

#Wilcoxon for impact (impact different from 0?)
#All areas
wilcox.test(farc.comparison$impact ,alternative ="greater")
boxplot(farc.comparison$impact, main = "FARC impact", xlab = "FARC areas",
        ylab = "Impact")

#Big areas
wilcox.test(farc.gde$impact ,alternative ="greater")
boxplot(farc.gde$impact, main = "Big FARC impact", xlab = "FARC areas",
        ylab = "Impact" )

####Comparison overlapped FARC - PA areas####
##Creating data frame
#Columns
#PA id
pa.ov.id <- c(2, 3, 9, 10, 15, 17, 21, 22, 23, 26, 28, 31, 32, 33, 35, 37,
              42, 44, 45, 49, 50, 51, 52, 65, 66, 74, 82, 83, 88, 90,91,
              94, 95, 97, 100, 106, 108, 122)
#Area
area <- tapply(df.ov[,23], df.ov[,15], FUN = sum) # column 23 is pa.farc, column 15 is PA id
#Treatment
tr <- tapply(df.ov[,22], df.ov[,15], FUN = sum) #Column 22 is is.forest.2017 for treatment, column 15 is PA id
#Control
cont <- tapply(df.ov[,48], df.ov[,15], FUN = sum) #Column 48 is is.forest.2017 for treatment, column 15 is PA id

#Joining columns
pa.ov <- cbind(pa.ov.id, area, tr, cont)
pa.ov <- as.data.frame(pa.ov)

#Treatment/area
pa.ov["tr.area"] <- NA
pa.ov["tr.area"] <- pa.ov$tr/pa.ov$area

#Control/area
pa.ov["cnt.area"] <- NA
pa.ov["cnt.area"] <- pa.ov$cont/pa.ov$area

#Impact (c-t/a)
pa.ov["impact"] <- NA
pa.ov["impact"] <- pa.ov$cnt.area - pa.ov$tr.area

View(pa.ov)

#Saving
write.table(pa.ov, "PA-overlapped-comparison.txt", sep = ";")
pa.ov <- read.table("PA-overlapped-comparison.txt", sep = ";")

####Wilcoxon tests - overlapped areas####
#Wilcoxon comparing treatment vs. control (control different from treatment?)
# All areas 
wilcox.test(pa.ov$tr.area, pa.ov$cnt.area, paired = T,alternative ="less")
boxplot(pa.ov$tr.area, pa.ov$cnt.area, main = "Overlapped tr vs control",
        xlab = c("Treatment", "Control"), ylab = "Impact")

#Big areas
#Creating subset of big areas
pa.ov.gde<-subset(pa.ov,pa.ov$area>10)
write.table(pa.ov.gde, "Big-overlapped-PA.txt", sep = ";")
pa.ov.gde <- read.table("Big-overlapped-PA.txt", sep = ";")

#Test
wilcox.test(pa.ov.gde$tr.area, pa.ov.gde$cnt.area ,alternative ="less")
boxplot(pa.ov.gde$tr.area, pa.ov.gde$cnt.area,
        main = "Big Overlapped tr vs control", 
        xlab = c("Treatment", "Control"), ylab = "Impact")

#Wilcoxon for impact (impact different from 0?)
#All areas
wilcox.test(pa.ov$impact ,alternative ="greater")
boxplot(pa.ov$impact, main = "Overlapped impact", 
        xlab = "Overlapped areas", ylab = "Impact")

#Big areas
wilcox.test(pa.ov.gde$impact ,alternative ="greater")
boxplot(pa.ov.gde$impact, main = "Big Overlapped impact", 
        xlab = "Overlapped areas", ylab = "Impact")

####Comparison between FARC, PA and overlapped####

#Comparison FARC and PA impact
#All
wilcox.test(pa.comparison$impact, farc.comparison$impact)
boxplot(pa.comparison$impact, farc.comparison$impact, 
        main = "PA vs FARC impact", xlab = c("PA areas", "FARC areas"), 
        ylab = "Impact")

#Big
wilcox.test(pa.gde$impact, farc.gde$impact)
boxplot(pa.gde$impact, farc.gde$impact, 
        main = "PA vs FARC impact - Big areas", 
        xlab = c("PA areas", "FARC areas"), ylab = "Impact")

#Comparison FARC and overlapped impact
#All
wilcox.test(pa.ov$impact, farc.comparison$impact)
boxplot(pa.ov$impact, farc.comparison$impact, 
        main = "overlapped vs FARC impact", xlab = c("Overlapped areas", "FARC areas"), ylab = "Impact")

#Big
wilcox.test(pa.ov.gde$impact, farc.gde$impact)
boxplot(pa.ov.gde$impact, farc.gde$impact, 
        main = "Overlapped vs FARC impact - Big areas", 
        xlab = c("Overlapped areas", "FARC areas"), ylab = "Impact")

#Comparison PA and overlapped impact
#All
wilcox.test(pa.comparison$impact, pa.ov$impact)
boxplot(pa.comparison$impact, pa.ov$impact, 
        main = "PA vs overlapped impact", 
        xlab = c("PA areas", "Overlapped areas"), ylab = "Impact")

#Big
wilcox.test(pa.gde$impact, pa.ov.gde$impact)
boxplot(pa.gde$impact, pa.ov.gde$impact, 
        main = "PA vs overlapped impact - Big areas", 
        xlab = c("PA areas", "Overlapped areas"), ylab = "Impact")