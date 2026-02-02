library(AER)
library(tidyverse)
library(mosaic)
library(plm)
library(sandwich)
library(lmtest)
library(GGally)
library(wooldridge)
library(margins)
library(tidyverse)
library(ggformula)
library(mosaic)
library(stringr)
library(tidyr)

df <- read_csv("/Users/annawtong/Desktop/data assistant/Anna Copy of Homepage data.xlsx - Sheet1 (2).csv", col_types = cols(.default = "?", col47 = "c"))
problems(df)
dim(df)

View(df)

df1<- df%>% filter(state=="paid")
scraps<- df%>%filter(state!="paid")

dim(df1)
dim(scraps)
# use df 1 from here on out.
#Dropped rows(failure to pay) = 139. success = 29733


#splitting rows where couples are recorded. 115 new entries have been made.
df2 <- df1 %>%
  separate_rows(constituent_identifier, sep = ";")
dim(df2)
print(29848-29733)

df2 <- df2 %>%
  mutate(
    gift_size_category = case_when(
      donation_amount < 20 ~ "<$20",
      donation_amount >= 20 & donation_amount < 50 ~ "$20-$50",
      donation_amount >= 50 & donation_amount < 100 ~ "$50-$100", 
      donation_amount >= 100 & donation_amount < 200 ~ "$100-$200",
      donation_amount >= 200 & donation_amount < 500 ~ "$200-$500",
      donation_amount >= 500 & donation_amount < 1000 ~ "$500-$1,000",
      donation_amount >= 1000 & donation_amount < 5000 ~ "Leadership ($1,000-$5,000)",
      donation_amount >= 5000 ~ "Leadership ($5,000+)",
      TRUE ~ "Unknown"
    ),
    gift_size_category = factor(gift_size_category,
                                levels = c("<$20", "$20-$50", "$50-$100", "$100-$200",
                                           "$200-$500", "$500-$1,000", "Leadership ($1,000-$5,000)","Leadership ($5,000+)", "Unknown"))
  )

dim(df2)

#quick counts
df2 %>%count(gift_size_category)
df2 %>% gf_bar(~gift_size_category)
df2<-df2%>% mutate()

library(dplyr)
library(stringr)

df2 <- df2 %>%
  mutate(
      affiliation1 = case_when(
      str_detect(affiliation, "alumni") ~ "alumni",
      str_detect(affiliation, "parent") ~ "parent",
      str_detect(affiliation, "student") ~ "student",
      str_detect(affiliation, "friend|employee") ~ "friend"
    )
  )

df2 <- df2 %>%
  drop_na(affiliation1)

df2 %>% gf_bar(~affiliation1)
df2 %>%gf_bar(~recur)

#date fixing
library(lubridate)
library(dplyr)

df2 <- df2 %>%
  mutate(
    capture_date = as.Date(mdy_hm(datetime_of_capture))
  )

summary(df2$capture_date)

#fiscal year grabbing
df2 <- df2 %>%
  mutate(
    calender_month= month(capture_date),
    calendar_year = year(capture_date),
    fiscal_year = case_when(
      month(capture_date) >= 10 ~ calendar_year + 1,
      TRUE ~ calendar_year
    )
  )

favstats(~calendar_year, data = df2)
favstats(~calender_month, data = df2)
favstats(~fiscal_year, data = df2)


?case_when
#lookup ID fixing

# Ultra-conservative approach - only match when very confident
#we only want to make sure its unique donors within the same fiscal year
df2 <- df2 %>%
  mutate(
    clean_name = str_to_lower(str_trim(paid_name)),
    clean_email = str_to_lower(str_trim(payer_email)),
    clean_address = str_to_lower(str_squish(mail_address)),
    
    across(c(clean_name, clean_email, clean_address), 
           ~ ifelse(. == "" | is.na(.), NA, .))
  ) %>%
  
  # Start with constituent IDs
  mutate(donor_id = ifelse(!is.na(constituent_identifier) & constituent_identifier != "", 
                           constituent_identifier, NA)) %>%
  
  # ONLY match if we have the same email AND same name (very conservative)
  group_by(clean_email, clean_name) %>%
  mutate(
    # Only propagate constituent IDs within exact matches
    donor_id = ifelse(is.na(donor_id) & !is.na(clean_email) & !is.na(clean_name),
                      first(na.omit(donor_id)), donor_id)
  ) %>%
  ungroup() %>%
  
  # For everything else, use email if available, otherwise generate unique ID
  mutate(
    donor_id = case_when(
      !is.na(donor_id) ~ donor_id,
      !is.na(clean_email) ~ clean_email,
      TRUE ~ paste0("donor_", row_number())
    )
  )

cat("Conservative approach - Unique donors:", n_distinct(df2$donor_id), "\n")
dim(df2)

write.csv(df2, "homepagedatav1.csv", row.names = FALSE, na = "")

head(df2$donor_id)
head(df2$gift_size_category)
head(df2$designation)
