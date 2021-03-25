


library(charlatan)
library(randomNames)
library(tidyverse)
cases <-
  tibble(
    id_case = 1:20,
    name_case = ch_name(n = 20),
    classif_case = "Confirmed",
    date_symp_onset_case = paste("2020",
                               sample(5:12, size = 20, replace = TRUE),
                               sample(1:19, size = 20, replace = TRUE),
                               sep = "-")) %>%
  mutate(date_symp_onset_case = as.Date(date_symp_onset_case,
                                      format = "%Y-%m-%d")) %>%
  slice(rep(1:n(), each = 10))  %>%
  arrange(id_case)



contacts_male <-
  tibble(last_name_contact = randomNames(n = 100, which.names = "last", gender = 0),
         first_name_contact = randomNames(n = 100, which.names = "first", gender = 0),
         sex_contact = "Male"
  )

contacts_female <-
  tibble(last_name_contact = randomNames(n = 100, which.names = "last", gender = 1),
         first_name_contact = randomNames(n = 100, which.names = "first", gender = 1),
         sex_contact = "Female")

contacts_init <-
  contacts_male %>%
  bind_rows(contacts_female) %>%
  mutate(id_contact_end = sample(1:200, 200)) %>%
  arrange(id_contact_end)


sample_contacts_df_raw <-
  cases %>%
  bind_cols(contacts_init) %>%
  mutate(id_contact = paste(id_case, 1:10, sep = "-"),
         relation_contact_case = sample(c("family member", "co-worker", "community-member", "patient"),
                                        size = n(),
                                        replace = TRUE),
         age_contact = sample(0:100, size = n(), replace = TRUE),
         phone_contact = ch_phone_number(n = n()),
         hcw_status_contact = sample(c("Yes", "No"), size = n(), replace = TRUE),
         admin1_contact = "Lagos",
         admin2_contact = sample(c("Agege", "Ajeromi-Ifelodun", "Alimosho", "Amuwo-Odofin",
                                   "Apapa", "Badagry", "Epe", "Eti-Osa", "Ibeju/Lekki",
                                   "Ifako-Ijaye", "Ikeja", "Ikorodu", "Kosofe",
                                   "Lagos Island", "Lagos Mainland", "Mushin", "Ojo",
                                   "Oshodi-Isolo", "Shomolu", "Surulere"),
                                 size = n(),
                                 replace = TRUE),
         admin3_contact = NA_character_,
         hhld_name_contact = NA_character_,
         type_contact = sample(c("High-risk", "Low-risk"), size = n(), replace = TRUE),
         vital_state_contact = sample(c("Healthy", "Ill", "Deceased"), size = n(), replace = TRUE),
         date_death_or_illness_contact = date_symp_onset_case + sample(1:50, size = n(), replace = TRUE),
         # should be after date of symptom onset but before date of death or illness
         date_last_interaction = date_symp_onset_case + sample(1:50, size = n(), replace = TRUE),
         # if data of last interaction is after date of death or illness, set it as 10 days before date of death
         date_last_interaction = ifelse(vital_state_contact == "Ill" | vital_state_contact == "Deceased",
                                      date_death_or_illness_contact - 10,
                                      date_last_interaction),
         date_last_interaction = as.Date(date_last_interaction, origin = as.Date("1970-01-01")), 
         date_death_or_illness_contact = as.Date(date_death_or_illness_contact, origin = as.Date("1970-01-01")),
         follow_up_status = "Inactive",
         loc_death_contact = ifelse(vital_state_contact == "Deceased",
                                    sample(admin2_contact),
                                    NA_character_),
         # everyday lose 10 people to followup
         follow_up_day_1 = c(rep(1, each = 190),
                         rep(0, each = 10)),
         follow_up_day_2 = c(rep(1, each = 180),
                         rep(0, each = 20)),
         follow_up_day_3 = c(rep(1, each = 170),
                         rep(0, each = 30)),
         follow_up_day_4 = c(rep(1, each = 160),
                         rep(0, each = 40)),
         follow_up_day_5 = c(rep(1, each = 150),
                         rep(0, each = 50)),
         follow_up_day_6 = c(rep(1, each = 140),
                         rep(0, each = 60)),
         follow_up_day_7 = c(rep(1, each = 130),
                         rep(0, each = 70)),
         follow_up_day_8 = c(rep(1, each = 120),
                         rep(0, each = 80)),
         follow_up_day_9 = c(rep(1, each = 110),
                         rep(0, each = 90)),
         follow_up_day_10 = c(rep(1, each = 100),
                          rep(0, each = 100)),
         follow_up_day_11 = c(rep(1, each = 90),
                          rep(0, each = 110)),
         follow_up_day_12 = c(rep(1, each = 80),
                          rep(0, each = 120)),
         follow_up_day_13 = c(rep(1, each = 70),
                          rep(0, each = 130)),
         follow_up_day_14 = c(rep(1, each = 60),
                          rep(0, each = 140)),
         reasons_end_follow_up = ifelse(follow_up_day_14 == 0, 
                                        "Lost to follow-up",
                                        "Completed"),
         date_death_or_illness_contact = ifelse(vital_state_contact == "Healthy",
                                              NA_character_,
                                              date_death_or_illness_contact))


openxlsx::write.xlsx(sample_contacts_df_raw, here("data/sample_contacts_df_raw.xlsx"))

