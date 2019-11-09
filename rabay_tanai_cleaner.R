library(tidyverse)
library(dplyr)
library(data.table)

# step 0 download Google Drive directory
# step 1 unzip in R

unzip("da2-2019-shops.zip")
all_dirs <- as.list(list.dirs("da2-2019-shops/", recursive = F))

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
check_saved <- readRDS("random_list.rds")

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
(data3 <- read.delim(to_wrangle[3], sep = ';'))
(data4 <- read.csv(to_wrangle[4]))
(data5 <- read.csv(to_wrangle[5]))
(data6 <- read.csv(to_wrangle[6]))
(data7 <- read.csv(to_wrangle[7]))

# step 6 clean: select (and unselect) columns, concatenate / split where needed, rename columns

colnames(data1)
data1 <- data1 %>% filter(data1$ď.żproduct == "Coke") %>% select(-ď.żproduct, -chain) %>%  rename(
                                                                 "Coke" = price, 
                                                                 "Address" = shop_address,
                                                                 "Name" = shop_name,
                                                                 "InOut" = district)

colnames(data2)
data2$Address <- paste(data2$Address_City,data2$Address_ZIP,data2$Address_Street)
data2 <- data2 %>% filter(Product == "Coca Cola") %>% select(-ID, -Product_ID, -Product, -Shop_ID, -(Address_Street:Address_City), -Google.Rating) %>% rename("Coke" = Price,
                                                             "Name" = Shop_Name)

colnames(data3)
data3 <- data3 %>% select(-price_snickers, -ď.żrecord,-hour_opening,-hour_closing, -store_type, -store_private_label, -store_local) %>% rename("Name" = store_name, "Address" = store_address, "Coke" = price_coke, "Size" = store_size, "InOut" = store_bp)

colnames(data4)
data4$Address <- paste(data4$city,data4$zip_code,data4$address)
data4 <- data4 %>% select(-price_redbull, -ď.żid, -address, -zip_code, -observer, -city, -country_of_origin, -type) %>% rename("Name" = shop_name,
                                           "Coke" = price_cocacola,
                                           "InOut" = district,
                                           "Size" = size)

colnames(data5)
data5$Address <- paste("Budapest",data5$district,data5$street)
data5 <- data5 %>% select(-store_brand, -(discount_rate_Coke:discount_rate_heineken), -obs_ID, -street, -Date, -store_type, -traffic_footfall, -store_feel) %>% rename("Name" = store_name, "Coke" = price_coke, "InOut" = district, "Size" = store_size) %>% filter(!is.na(Coke))


colnames(data6)
data6$Address <- paste("Budapest",data6$District,data6$Address)
data6 <- data6 %>% select(-(Coke_Description:Nestle_Aqua_Description), -Variable_ID, -District, -Shop_Type) %>% rename("Name" = Shop_Name,
                                                                          "Coke" = Coke_Price,
                                                                          "InOut" = District_Category,
                                                                          "Size" = Shop_Size)

colnames(data7)
data7$Address <- paste("Budapest",data7$address)
data7 <- data7 %>% select(-orbit_price, -id, -address, -chain) %>% rename("Coke" = cola_price, "Name" = name, "Size" = size) %>% filter(!is.na(Coke))

# step 7: merge dfs together by the identical Name, Address and Price variables

merged <- rbindlist(list(data1,data2,data3,data4,data5,data6,data7), fill = T)

all_rows_by_length <- nrow(data1) + nrow(data2) + nrow(data3) + nrow(data4) + nrow(data5) + nrow(data6) + nrow(data7)
all_rows_merged <- nrow(merged)

all_rows_by_length == all_rows_merged # great, the merge was successful

View(merged)

saveRDS(merged, "to_analyze.rds")

