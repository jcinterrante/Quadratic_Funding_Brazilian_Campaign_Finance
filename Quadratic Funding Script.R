library(tidyverse)
library(lubridate)

data <- read.csv("donation_data.csv", encoding = "latin1")
ideology <- read.csv("2018_party_ideology_scores.csv", encoding = "latin1", sep = ";")

individual_donations <- data %>% 
  filter(CD_CNAE_DOADOR == -1)%>%
  mutate(period = floor_date(dmy(donation_date),unit = "month")) %>%
  select(office = "DS_CARGO", candidate_id, candidate_name,"party_id" = NR_PARTIDO, 
         donor_id, donor_name = "NM_DOADOR",period,donation_amount)%>%
  filter(!is.na(donor_id), donor_id != -1, 
         !is.na(donation_amount))

ideology <- ideology %>% 
  filter(BLS_YEAR == 2018)%>%
  select(party_id = "NUM_PART", party_name = "NOME_PARTIDO", 
         ideology = "IDEO_IMPUTED")


donor_summary <- individual_donations %>%
  group_by(office, period, donor_id, candidate_id) %>%
  summarize(
    party_id = first(party_id),
    overall_donation_amount = sum(donation_amount),
    sqrt_donation_amount = sqrt(overall_donation_amount)
  )

candidate_summary <- donor_summary %>%
  group_by(office, period,candidate_id) %>%
  summarize(
    party_id = first(party_id),
    summed_squares = sum(sqrt_donation_amount)^2,
    funded_amount = sum(overall_donation_amount),
    count_of_donors = n()
  )

qf_table <- candidate_summary %>%
  group_by(office, period)%>%
  mutate(final_match = summed_squares * sum(funded_amount) / sum(summed_squares),
         total_funding = funded_amount+final_match) %>%
  select(candidate_id, party_id, count_of_donors, funded_amount, final_match,
         total_funding)

candidate_dictionary <-data%>%
  select(candidate_id, candidate_name)%>%
  distinct()


qf_table <- left_join(qf_table,candidate_dictionary, by = "candidate_id")
qf_table <- left_join(qf_table, ideology, by = "party_id")

qf_table <- qf_table %>%select(office, period, candidate_id, candidate_name, party_id,
                   party_name,ideology,count_of_donors,funded_amount,final_match,
                   total_funding)

qf_totals <- qf_table %>% group_by(candidate_id)%>%
  summarize(
    office = first(office),
    candidate_name = first(candidate_name),
    party_id = first(party_id),
    party_name = first(party_name),
    ideology = first(ideology),
    donor_count = sum(count_of_donors),
    funded_amount = sum(funded_amount),
    final_match = sum(final_match),
    total_funding = sum(total_funding)
  )

ggplot(qf_totals) +
  geom_point(aes(x=ideology, y = final_match,jitter = TRUE, alpha = 0.5))+
  geom_point(aes(x=ideology, y = funded_amount, color = "red", jitter = TRUE, alpha = 0.5))

qf_totals_long = qf_totals %>% pivot_longer(c(funded_amount, final_match),names_to = "source", values_to = "amount")

levels(qf_totals_long$office[2])


offices<-unique(qf_totals_long$office)
offices
vector(offices)
length(offices)

filter(qf_totals_long, office == offices[[3]])

for (i in seq_along(offices)){
  print(
    ggplot(head(arrange(filter(qf_totals_long, office == offices[[i]]), -total_funding), n = 20)) +
      geom_col(aes(x = reorder(candidate_name,total_funding), y = amount, fill = source)) +
      labs(title = paste(offices[[i]], "by Candidate"),
           x = "Candidate")+
      coord_flip()
  )
  ggsave(paste(offices[[i]], "_QF_bar_chart"), device = "png", path = "graphs")
  print(
    ggplot(filter(qf_totals, office == offices[[i]]),
           aes(x=ideology, y = final_match/total_funding))+
      geom_point()+
      geom_smooth(method = "lm") +
      labs(title = offices[[i]],
           y="% funds raised through QF")
  )
  
  print(
    ggplot(head(arrange(filter(qf_totals_long, office == offices[[i]]), -total_funding), n = 20)) +
      geom_col(aes(x = reorder(party_name,total_funding), y = amount, fill = source)) +
      labs(title = paste(offices[[i]], " by Party"),
           x = "Candidate")+
      coord_flip()
  )
  ggsave(paste(offices[[i]], "_QF_bar_chart_parties"), device = "png", path = "graphs")
}

ggplot(filter(qf_table, office == "Governador")) +
  geom_boxplot(aes(x=factor(ideology), y = final_match))
ggplot(filter(qf_table, office == "Governador")) +
  geom_boxplot(aes(x=factor(ideology), y = funded_amount))

ggplot(filter(qf_table, office == "Senador")) +
  geom_boxplot(aes(x=factor(ideology), y = final_match))
ggplot(filter(qf_table, office == "Senador")) +
  geom_boxplot(aes(x=factor(ideology), y = funded_amount))
