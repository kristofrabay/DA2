library(tidyverse)
library(dplyr)
library(data.table)

# step 0 download Google Drive directory
# step 1 unzip in R

setwd("../")
getwd()

unzip("team_assignment/da2-2019-shops.zip")
all_dirs <- as.list(list.dirs("team_assignment/da2-2019-shops/", recursive = F))

# step 2 remove our repos to surely randomly select only those that do not belong to us

tanai <- all_dirs[all_dirs %like% "tanai"]
rabay <- all_dirs[all_dirs %like% "rabay"]

index_tanai <- which(all_dirs %like% "tanai")
index_rabay <- which(all_dirs %like% "rabay")

all_dirs[index_tanai] <- NULL
all_dirs[index_rabay] <- NULL

# step 3 randomly select 6 repos, store them in a list

### DO NOT RUN THIS AGAIN, random_dirs will be lost
#random_dirs <- sample(all_dirs, 6)
#saved the list to never lose it (in RDS)

#install.packages('rlist')
library(rlist)

#list.save(random_dirs, "random_list.rds")
check_saved <- readRDS("team_assignment/random_list.rds")

#identical(check_saved,random_dirs)
#from now we use the saved random file (check_saved)

# step 4 create the list that contains all repos (6 random + 2 own)

to_wrangle <- c(tanai,rabay,check_saved)
to_wrangle <- list.dirs(as.character(to_wrangle), recursive = T)
to_wrangle <- list.files(to_wrangle, include.dirs = T, recursive = T, full.names = T)

      
# step 5 select and read the csvs

to_wrangle <- to_wrangle[to_wrangle %like% ".csv"]

#unfortunately cant read all with loop because separation differs (,;)
(data1 <- read.delim(to_wrangle[1], sep = ';'))
(data2 <- read.csv(to_wrangle[2]))
(data3 <- read.csv(to_wrangle[3]))
(data4 <- read.csv(to_wrangle[4]))
(data5 <- read.csv(to_wrangle[5]))
(data6 <- read.delim(to_wrangle[6], sep = ";"))
(data7 <- read.csv(to_wrangle[7]))

# step 6 clean: select (and unselect) columns, concatenate / split where needed, rename columns

colnames(data1)
View(data1)
data1 <- data1 %>% filter(data1$ď.żproduct == "Coke") %>% select(-ď.żproduct, -chain, -open_7_24) %>%  rename(
                                                                 "Coke" = price, 
                                                                 "Address" = shop_address,
                                                                 "Name" = shop_name,
                                                                 "InOut" = district)
data1$InOut <- ifelse(grepl("14", data1$InOut)==T,0,1)

colnames(data2)
View(data2)
data2$Address <- paste(data2$Address_City,data2$Address_ZIP,data2$Address_Street)
data2 <- data2 %>% filter(Product == "Coca Cola") %>% select(-ID, -Product_ID, -Product, -Shop_ID, -(Address_Street:Address_City), -Google.Rating, -Number_of_cashiers) %>% rename("Coke" = Price,
                                                             "Name" = Shop_Name)
data2$InOut <- ifelse(grepl("1111|1114|1117", data2$Address)==T,1,0)


colnames(data3)
View(data3)
data3$Address <- paste(data3$city,data3$zip_code,data3$address)
data3 <- data3 %>% select(-price_redbull, -ď.żid, -address, -zip_code, -observer, -city, -country_of_origin, -type, -opening_times) %>% rename("Name" = shop_name,
                                           "Coke" = price_cocacola,
                                           "InOut" = district,
                                           "Size" = size)
data3$InOut <- ifelse(grepl("IX", data3$InOut)==T,1,0)



colnames(data4)
View(data4)
data4$Address <- paste("Budapest",data4$District,data4$Address)
data4 <- data4 %>% select(-(Coke_Description:Nestle_Aqua_Description), -Variable_ID, -District, -Shop_Type) %>% rename("Name" = Shop_Name,
                                                                          "Coke" = Coke_Price,
                                                                          "InOut" = District_Category,
                                                                          "Size" = Shop_Size)
data4$InOut <- ifelse(grepl("Outer", data4$InOut)==T,0,1)



colnames(data5)
View(data5)
data5$Address <- paste("Budapest",data5$address)
data5 <- data5 %>% select(-orbit_price, -id, -address, -chain) %>% rename("Coke" = cola_price, "Name" = name, "Size" = size) %>% filter(!is.na(Coke))

for (i in 1:nrow(data5)) {
  if (i <= 22) {
    data5$InOut[i] <- 1
  } else {
    data5$InOut[i] <- 0
  }
}


colnames(data6)
View(data6)
data6 <- data6 %>% select(-price_snickers, -ď.żrecord,-hour_opening,-hour_closing, -store_type, -store_private_label, -store_local, -store_24hours) %>% rename("Name" = store_name, "Address" = store_address, "Coke" = price_coke, "Size" = store_size, "InOut" = store_bp)


colnames(data7)
View(data7)
data7$Address <- paste("Budapest",data7$district,data7$street)
data7 <- data7 %>% select(-store_brand, -(discount_rate_Coke:discount_rate_heineken), -obs_ID, -street, -Date, -store_type, -traffic_footfall, -store_feel) %>% rename("Name" = store_name, "Coke" = price_coke, "InOut" = district, "Size" = store_size) %>% filter(!is.na(Coke))
data7$InOut <- ifelse(grepl("3", data7$InOut)==T,0,1)

# step 7: merge dfs together by the identical Name, Address and Price variables

merged <- rbindlist(list(data1,data2,data3,data4,data5,data6,data7), fill = T)
merged <- merged %>% select(-Type)

all_rows_by_length <- nrow(data1) + nrow(data2) + nrow(data3) + nrow(data4) + nrow(data5) + nrow(data6) + nrow(data7)
all_rows_merged <- nrow(merged)

all_rows_by_length == all_rows_merged # great, the merge was successful

View(merged)

saveRDS(merged, "team_assignment/to_analyze.rds")

#remove unneeded objects

list = c()

for (i in 1:7) {
  list <- append(list, paste0("data",i))
}

rm(list = list)


# step 8: deal with different names for sizes

unique(merged$Size)

merged$Size <- tolower(trimws(merged$Size))

merged$Size[merged$Size == "big"] <- "large"
merged$Size[merged$Size == "very small"] <- "small"
merged$Size[merged$Size == "extra large"] <- "large"

unique(merged$Size)

merged %>% group_by(Size) %>% summarize(count = n()) %>% arrange(desc(count))


# step 9: deal with NA in size: will have to go half automated half manual

#1: match names of stores that already have a matching size

to_match <- merged %>% filter(!is.na(Size)) %>% select(Name,Size) 
to_match$Name <- tolower(to_match$Name)
to_match <- to_match %>% distinct(Name, .keep_all = T) #distinct store names with sizes
View(to_match)

merged$Name <- tolower(merged$Name)

merged <- left_join(x = merged,
          y = to_match,
          by = "Name") %>% select(-Size.x) %>% rename(Size = Size.y)


View(merged)

saveRDS(merged, "team_assignment/to_analyze.rds")

#2: see what is still missing a size, fill up with data

merged %>% filter(is.na(Size)) #52 records - not so bad
merged %>% filter(is.na(Size)) %>% distinct(Name) #33 distinct names - even better

#everything is small except:
#medium if contains: tesco, cba, spar

merged$Size[is.na(merged$Size) & (merged$Name %like% "spar" | merged$Name %like% "tesco" | merged$Name %like% "cba")] <- "medium"
merged$Size[is.na(merged$Size)] <- "small"

unique(merged$Size)

merged %>% group_by(Size) %>% summarize(count = n()) %>% arrange(desc(count))

View(merged)

names(merged)[1] <- "price_coke"
names(merged)[2] <- "address"
names(merged)[3] <- "name"
names(merged)[5] <- "store_size"

# add districts for further analysis 

merged$ID <- seq.int(nrow(merged))

merged$district <- ifelse(merged$ID < 21, 7,
                             ifelse(merged$ID < 41, 14,
                                    ifelse(merged$ID < 61, 11,
                                           ifelse(merged$ID < 82, 17,
                                                  ifelse(merged$ID < 102, 9,
                                                         ifelse(merged$ID < 122, 10,
                                                                ifelse(merged$ID < 145, 8,
                                                                       ifelse(merged$ID < 165, 10,
                                                                              ifelse(merged$ID < 187, 9,
                                                                                     ifelse(merged$ID < 207, 14,
                                                                                            ifelse(merged$ID < 224, 16,
                                                                                                   ifelse(merged$ID < 227, 10,
                                                                                                          ifelse(merged$ID < 247, 5,
                                                                                                                 ifelse(merged$ID < 266, 3,
                                                                                                                        9))))))))))))))


View(merged)

# step 10: finally, save the datatable ready for analysis

saveRDS(merged, "team_assignment/rabay_tanai_cleaned.rds")
write_csv(merged, "team_assignment/rabay_tanai_cleaned.csv")


##################################################################
