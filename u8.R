library(vroom)
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

patent_tbl %>% glimpse

##patent_assignee
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

patent_assignee_tbl %>% glimpse

#assignee
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

assignee_tbl %>% glimpse

#uspc
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

uspc_tbl %>% glimpse

setDT(patent_tbl)
patent_tbl %>% glimpse()

setDT(patent_assignee_tbl)
patent_assignee_tbl %>% glimpse()

setDT(assignee_tbl)
assignee_tbl %>% glimpse()

setDT(uspc_tbl)
uspc_tbl %>% glimpse()

###

combined_data <- patent_tbl %>% 
  left_join(uspc_tbl, by = "patent_id") %>%
  left_join(patent_assignee_tbl, by = "patent_id") %>% 
  left_join(assignee_tbl, by = "assignee_id") 
setDT(combined_data)
combined_data %>% glimpse()

setkey(combined_data, "patent_id")
key(combined_data)
setorderv(combined_data, c("patent_id", "assignee_id"))
combined_data %>% glimpse()

tic()
combined_data %>%
  filter(!is.na(patent_id)) %>%
  count(patent_id) 
toc()

combined_data %>% dim() 
keep_cols <- c("patent_id", 
                                       "date", 
                                       "num_claims",
                                       "mainclass_id", 
                                       "sequence", 
                                       "assignee_id", 
                                       "type", 
                                       "organization")
combined_data <- combined_data[, ..keep_cols]
combined_data %>% dim()

#####First Question

Q_1 <- combined_data %>%
            filter(!is.na(organization)) %>%
            count(organization) %>% 
            arrange(desc(n)) 

QR_1 <- combined_data %>%
  filter(!is.na(assignee_id)) %>%
  count(assignee_id) %>% 
  arrange(desc(n)) 

Q_11 <- Q_1[1:10]
QR_11 <- QR_1 [1:10]
Qp_1 <- Q_11 %>% left_join(QR_11)
names(Qp_1)[2] <- "no. of patents"
first_Q <- Qp_1[, c(1, 3, 2)]
first_Q

####Second Question

Q_2 <- combined_data %>% 
  mutate(date = as.character(date)) %>%
  
  separate(col  = date,
           into = c("year", "month", "day"),
           sep  = "-", remove = FALSE) %>%
  mutate(
    year  = as.numeric(year),
    month = as.numeric(month),
    day   = as.numeric(day)
  )

Qp_2 <- Q_2[month==8] %>% 
  filter(!is.na(organization)) %>% 
  count(organization) %>%
  arrange(desc(n)) 

QR_2 <- Q_2[month==8] %>% 
  filter(!is.na(assignee_id)) %>% 
  count(assignee_id) %>% 
  arrange(desc(n))

W <- Qp_2 %>% left_join(QR_2)
  
Qt_2 <- Q_2[month==8] %>% 
  cbind(W)
  
QR_T<- patent_tbl[month==8] 