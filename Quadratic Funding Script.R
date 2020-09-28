library(tidyverse)
library(lubridate)

data <- read.csv("donation_data.csv", encoding = "latin1")
individual_donations <- data %>% 
  select(donor_id, candidate_name, candidate_id, NM_PARTIDO, 
         date_donation_reported,donation_amount)%>%
  filter(!is.na(donor_id), donor_id != -1, !is.na(donation_amount)) %>%
  

ggplot(individual_donations, mapping = aes(x=date_donation_reported)) +
       geom_bar()
    


match <- sum(individual_donations$donation_amount)

donor_summary <- individual_donations %>%
  group_by(donor_id, candidate_name) %>%
  summarize(
    overall_donation_amount = sum(donation_amount),
    sqrt_donation_amount = sqrt(overall_donation_amount)
  )

candidate_summary <- donor_summary %>%
  group_by(candidate_name) %>%
  summarize(
    summed_squares = sum(sqrt_donation_amount)^2,
    funded_amount = sum(overall_donation_amount),
    count_of_donors = n()
  )

final_table <- candidate_summary %>%
  mutate(final_match = summed_squares * match / sum(summed_squares)) %>%
  select(candidate_name, count_of_donors, funded_amount, final_match)

final_table
