library(vroom)
library(magrittr)
library(data.table)
library(tidyverse) 
library(rvest)     
library(xopen)     
library(jsonlite)  
library(glue)      
library(stringi)
library(tictoc)

##patents
col_types <- list(
  patent_id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "Patent_data_reduced/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)


## patent_assignee
col_types_1 <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "Patent_data_reduced/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_1,
  na         = c("", "NA", "NULL")
)

# assignee
col_types_2 <- list(
  assignee_id = col_character(),
  type = col_double(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "Patent_data_reduced/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types_2,
  na         = c("", "NA", "NULL")
)

# uspc
col_types_3 <- list(
  patent_id = col_character(),
  mainclass_id = col_double(),
  num_claims = col_double()
)

uspc_tbl <- vroom(
  file       = "Patent_data_reduced/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types_3,
  na         = c("", "NA", "NULL")
)

setDT(patent_tbl)

setDT(patent_assignee_tbl)

setDT(assignee_tbl)

setDT(uspc_tbl)

combined_data <- patent_tbl %>% 
  left_join(uspc_tbl, by = "patent_id") %>%
  left_join(patent_assignee_tbl, by = "patent_id") %>% 
  left_join(assignee_tbl, by = "assignee_id") ## Combining data

setDT(combined_data)
setkey(combined_data, "patent_id")
setorderv(combined_data, c("patent_id", "assignee_id"))


keep_cols <- c("patent_id", 
               "date", 
               "num_claims",
               "mainclass_id", 
               "sequence", 
               "assignee_id", 
               "type", 
               "organization")
combined_data <- combined_data[, ..keep_cols]

## Up to here, I'm creating a data table including all variables

Q_1 <- combined_data [type == 2]%>%
  filter(!is.na(organization)) %>%
  count(organization) %>% 
  arrange(desc(n)) ## Sorting the most repeated organization in the data table (US companies/ type 2)

QR_1 <- combined_data [type == 2]%>%
  filter(!is.na(assignee_id)) %>%
  count(assignee_id) %>% 
  arrange(desc(n)) ## Most repeated assignee_id in the data table

Q_11 <- Q_1[1:10]
QR_11 <- QR_1 [1:10] ## Extracting first 10 (top ten) entries
Qp_1 <- Q_11 %>% left_join(QR_11) 
names(Qp_1)[2] <- "no. of patents"
first_Q <- Qp_1[, c(1, 3, 2)] ## Rearranging the data table
first_Q

Q_2 <- combined_data [type == 2] %>% 
  mutate(date = as.character(date)) %>%
  
  separate(col  = date,
           into = c("year", "month", "day"),
           sep  = "-", remove = FALSE) %>%
  
  mutate(
    year  = as.numeric(year),
    month = as.numeric(month),
    day   = as.numeric(day)
  ) ## Extracting US based companies (type 2)

Q_22 <- Q_2[month==8] %>%
  filter(!is.na(organization)) %>%
  count(organization) %>%
  arrange(desc(n)) ## Extracting top ten organizations with most patents in the US in August (month 8)

Qt_2 <- cbind(Q_2, Q_22)

Qt_22 <- Qt_2[,-11]
Qt_23 <- Qt_22[month == 8]
Qt_23 <- Qt_23[,-3]
Qt_23 <- Qt_23[,-3]
Qt_23 <- Qt_23[,-3]
Qt_23 <- Qt_23[,-4]
Qt_23 <- Qt_23[,-4]
Qt_23 <- Qt_23[,-5]
names(Qt_23)[6] <- "Number of patents" ## Renaming the column
Second_Q <- Qt_23[1:10]
Second_Q

Q_3 <- combined_data %>%
  filter(!is.na(organization)) %>%
  count(organization) %>% 
  arrange(desc(n)) ## Extracting top ten organizations with most patents worldwide

QR_3 <- combined_data %>%
  filter(!is.na(assignee_id)) %>%
  count(assignee_id) %>% 
  arrange(desc(n)) 

Q_33 <- Q_3[1:10]
QR_33 <- QR_3 [1:10]
Qp_3 <- Q_33 %>% left_join(QR_33)
names(Qp_3)[2] <- "no. of patents"
Qt_3 <- cbind(Qp_3, combined_data)


Qt_33 <- Qt_3[1:10]
Qt_34 <- Qt_33[,-11]
Qt_34 <- Qt_34[,-9]
Qt_34 <- Qt_34[,-5] ##Top ten Organizations worldwide

#1
M_1 <- combined_data[organization == "International Business Machines Corporation"] %>%
  filter(!is.na(mainclass_id)) %>%
  count(mainclass_id) %>% 
  arrange(desc(n))
M_11 <- M_1[1:5]

#2
M_2 <- combined_data[organization == "Samsung Electronics Co., Ltd."] %>%
  filter(!is.na(mainclass_id)) %>%
  count(mainclass_id) %>% 
  arrange(desc(n))
M_22 <- M_2[1:5]

#3
M_3 <- combined_data[organization == "Canon Kabushiki Kaisha"] %>%
  filter(!is.na(mainclass_id)) %>%
  count(mainclass_id) %>% 
  arrange(desc(n))
M_33 <- M_3[1:5]

#4
M_4 <- combined_data[organization == "Sony COrporation"] %>%
  filter(!is.na(mainclass_id)) %>%
  count(mainclass_id) %>% 
  arrange(desc(n))
M_44 <- M_4[1:5]

#5
M_5 <- combined_data[organization == "Microsoft Corporation"] %>%
  filter(!is.na(mainclass_id)) %>%
  count(mainclass_id) %>% 
  arrange(desc(n))
M_55 <- M_5[1:5]

#6
M_6 <- combined_data[organization == "QUALCOMM Incorporated"] %>%
  filter(!is.na(mainclass_id)) %>%
  count(mainclass_id) %>% 
  arrange(desc(n))
M_66 <- M_6[1:5]

#7
M_7 <- combined_data[organization == "Kabushiki Kaisha Toshiba"] %>%
  filter(!is.na(mainclass_id)) %>%
  count(mainclass_id) %>% 
  arrange(desc(n))
M_77 <- M_7[1:5]

#8
M_8 <- combined_data[organization == "Google Inc."] %>%
  filter(!is.na(mainclass_id)) %>%
  count(mainclass_id) %>% 
  arrange(desc(n))
M_88 <- M_8[1:5]

#9
M_9 <- combined_data[organization == "LG Electronics Inc."] %>%
  filter(!is.na(mainclass_id)) %>%
  count(mainclass_id) %>% 
  arrange(desc(n))
M_99 <- M_9[1:5]

#10
M_10 <- combined_data[organization == "Panasonic Corporation"] %>%
  filter(!is.na(mainclass_id)) %>%
  count(mainclass_id) %>% 
  arrange(desc(n))
M_100 <- M_10[1:5]