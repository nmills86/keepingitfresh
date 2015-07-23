#  7/21/2015
# R Script for Driven Data Competition, developed using version 3.1.2 
setwd("C:/Users/Nick/Documents/Capstone/")

# I. LOAD LIBRARIES. 

      library("jsonlite")
      library("rpart")
      library("rpart.plot")
      library("caret")
      library("stringr")
      library("reshape2")
      library("dplyr")
      library("Rcpp")


# II. PROCESS BUSINESS CHARACTERISTICS DATA
      
      # READ IN BUSINESS CHARACTERISTICS DATA FROM YELP
            data <- stream_in(file("yelp_academic_dataset_business.json"))
            data2 <- flatten(data, recursive=TRUE)
      
      # COERCE CATEGORIES VARIABLE TO STRING. CATEGORY INCLUDES TYPE OF 
      # RESTAURANT CUISINE. 
      
            data2$categories <- as.character(data2$categories)
            business <- data2
      
      # DEFINE RESTAURANTS BY THEIR FIRST CATEGORY IF THERE ARE SEVERAL. 
            business$cat1 <- substr(business$categories,4,str_locate(business$categories,",")-2)
            
            a <- as.data.frame(table(business$cat1))      
      
      # CREATE INDICATOR VARIABLES FOR EACH OF THE CATEGORIES THAT OCCURS IN
      # MORE THAN 40 BUSINESSES. ELIMINATED AMERICAN, RESTAURANTS, AND FOOD.
      
            a <- a[a$Freq>=40,]
            a <- a[a$Var1!="Food",]
            a <- a[a$Var1!="American (New)",]
            a <- a[a$Var1!="American (Traditional)",]
            a <- a[a$Var1!="Restaurants",]
            vars <- as.vector(a$Var1)
            vars <- c(vars, "American")
      
            for (i in 1:length(vars)) {
                  z <- vars[i]
                  business[,z] <- ifelse(is.na(str_locate(business$categories,z)[,1])!=TRUE,TRUE,FALSE)
            }      
      
      # NOW SUBSET BUSINESS DATA SO IT ONLY HAS RESTAURANT
      # CHARACTERISTICS AND PRICE RANGE
      
            list <-c("business_id","attributes.Price Range",vars)
            
            X <- business[,list]

# III. READ IN TF-IDF MATRIX GENERATED FROM YELP REVIEW DATA
            review <- read.csv("featurestrain_df.csv")
            review_test <- read.csv("featurestest_df.csv")

# IV. PROCESS TRAINING DATA

            data <- read.csv ("training.csv")
            testdata <- read.csv("SubmissionFormat.csv")

# V. MERGE CROSSWALK, CREATE 3 DATASETS, ONE FOR EACH 
      # INFRACTION CATEGORY

            reviewid <- review
            reviewid$restaurant_id <- data$restaurant_id
      
            reviewid$one <- data$X.
            reviewid$two <- data$X..
            reviewid$three <- data$X...
      
            cross <- read.csv("restaurant_ids_to_yelp_ids.csv", stringsAsFactors=FALSE)
            crosslong <- melt(cross, id=c("restaurant_id"),measure=c("yelp_id_0","yelp_id_1","yelp_id_2","yelp_id_3"),value.name="business_id")
            cross_business <- merge(crosslong, X, by="business_id")
      
            set.seed(123)
      # remove duplicates randomly.
      # this needs to be done because restaurant IDs with multiple businesses
      # IDs in YELP will have different types of characteristics. 

            cross_business$number <- runif(1911, min=0, max=1000000)
            min_select <- as.data.frame(summarise(group_by(cross_business,restaurant_id),number=min(number)))
            cross_business_nodup <- merge(min_select, cross_business, by=c("restaurant_id","number"))
      
            data <- merge(reviewid, cross_business_nodup, by="restaurant_id")
            data$Price <- data[,"attributes.Price Range"]
            drop <- c("yelp_id_2","yelp_id_1","yelp_id_0","yelp_id_3","X","X10", "X20", "X30","character(0)","variable","attributes.Price Range","restaurant_id","number", vars)
            DF <- data[,!(names(data) %in% drop)]

      # REPLACE ZERO PRICE IF MISSING
            DF$Price <- ifelse(is.na(DF$Price),0,DF$Price)

            review1 <- DF
            
            review2 <- DF
            
            review3 <- DF

      # Add price variable and Chinese indicator

            drop <- c("oysters","fast","rolls","warm","line","crab","reviews","perfectly","comes","fast","ice","business_id","one","two")
            review3 <- review3[,!(names(review3) %in% drop)]
            review3$Chinese <- DF$Chinese
            review3$Price <- DF$Price
            review3$Price <- as.factor(DF$Price)

            drop <- c("oysters","fast","rolls","warm","line","crab","reviews","perfectly","comes","fast","ice","business_id","one","three")
            review2 <- review2[,!(names(review2) %in% drop)]
            review2$Chinese <- DF$Chinese
            review2$Price <- DF$Price
            review2$Price <- as.factor(DF$Price)

            drop <- c("oysters","fast","rolls","warm","line","crab","reviews","perfectly","comes","fast","ice","business_id","three","two")
            review1 <- review1[,!(names(review1) %in% drop)]
            review1$Chinese <- DF$Chinese
            review1$Price <- DF$Price
            review1$Price <- as.factor(DF$Price)

#VI. CROSS VALIDATION USING CARET
            set.seed(123)
            #fitControl <- trainControl( method = "cv", number=10)
            #cartGrid <- expand.grid( .cp = seq(0,0.2,0.002))
            #train( one ~ ., data=review1, method="rpart", trControl = fitControl, tuneGrid = cartGrid)
            #fitControl <- trainControl( method = "cv", number=10)
            #cartGrid <- expand.grid( .cp = seq(0,0.2,0.002))
            #train( two ~ ., data=review2, method="rpart", trControl = fitControl, tuneGrid = cartGrid)
            #fitControl <- trainControl( method = "cv", number=10)
            #cartGrid <- expand.grid( .cp = seq(0,0.2,0.002))
            #train( three ~ ., data=review3, method="rpart", trControl = fitControl, tuneGrid = cartGrid)

# VII. FIT MODELS USING RPART


      m1 <- rpart(one ~ .,data=review1, cp=0.0)
      m2 <- rpart(two ~ .,data=review2, cp=0.0)
      m3 <- rpart(three ~., data=review3, cp=0.0)


# VIII. NOW MAKE TEST DATASETS.


            data <- read.csv("SubmissionFormat.csv")
            
            
            reviewid <- review_test
            reviewid$restaurant_id <- data$restaurant_id
            reviewid$one <- data$X.
            reviewid$two <- data$X..
            reviewid$three <- data$X...
            
            # MERGE CROSSWALK & INFRACTIONS
            cross <- read.csv("restaurant_ids_to_yelp_ids.csv", stringsAsFactors=FALSE)
            crosslong <- melt(cross, id=c("restaurant_id"),measure=c("yelp_id_0","yelp_id_1","yelp_id_2","yelp_id_3"),value.name="business_id")
            cross_business <- merge(crosslong, X, by="business_id")
            #i <- as.data.frame(summarise(group_by(cross_business,restaurant_id),b=length(restaurant_id)))
            #j <- subset(i,b>1)
            #who <-subset(cross_business, restaurant_id=="ydEj7V3W")
            set.seed(123)
            # remove duplicates randomly
            cross_business$number <- runif(1911, min=0, max=1000000)
            min_select <- as.data.frame(summarise(group_by(cross_business,restaurant_id),number=min(number)))
            cross_business_nodup <- merge(min_select, cross_business, by=c("restaurant_id","number"))

            
            
            data <- merge(reviewid, cross_business_nodup, by="restaurant_id")
            
            data$Price <- data[,"attributes.Price Range"]
            
            drop <- c("yelp_id_2","yelp_id_1","yelp_id_0","yelp_id_3","X","X10", "X20", "X30","character(0)","variable","attributes.Price Range","restaurant_id","number",vars)
            DF <- data[,!(names(data) %in% drop)]
            # REPLACE ZERO PRICE IF MISSING
            DF$Price <- ifelse(is.na(DF$Price),0,DF$Price)
            
            review1 <- DF
            
            review2 <- DF
            
            review3 <- DF
            
            
            drop <- c("oysters","fast","rolls","warm","line","crab","reviews","perfectly","comes","fast","ice","business_id","one","two")
            review3 <- review3[,!(names(review3) %in% drop)]
            review3$Chinese <- DF$Chinese
            review3$Price <- DF$Price
            review3$Price <- as.factor(DF$Price)
            
            
            drop <- c("oysters","fast","rolls","warm","line","crab","reviews","perfectly","comes","fast","ice","business_id","one","three")
            review2 <- review2[,!(names(review2) %in% drop)]
            review2$Chinese <- DF$Chinese
            review2$Price <- DF$Price
            review2$Price <- as.factor(DF$Price)
            
            drop <- c("oysters","fast","rolls","warm","line","crab","reviews","perfectly","comes","fast","ice","business_id","three","two")
            review1 <- review1[,!(names(review1) %in% drop)]
            review1$Chinese <- DF$Chinese
            review1$Price <- DF$Price
            review1$Price <- as.factor(DF$Price)

# IX. MAKE PREDICTIONS


            r3 <- predict(m3, newdata=review3)
            
            r2 <- predict(m2, newdata=review2)
            
            r1 <- predict(m1, newdata=review1)
            
            data <- read.csv("SubmissionFormat.csv")
            keep <- c("id","date","restaurant_id")
            
            keep <- c("id","date","restaurant_id")
            data <- data[,keep]
            data$X. <- round(r1, digits=0)
            data$X.. <- round(r2, digits=0)
            data$X... <- round(r3, digits=0)
            write.csv(data, "submission.csv")

