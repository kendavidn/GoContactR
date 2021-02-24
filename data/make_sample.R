# 
# cases <- 
#   tibble(
#     id_case = 1:20,
#     name_case = ch_name(n = 20),
#     classif_case = "Confirmed", 
#     dt_symp_onset_case = paste("2020", 
#                                sample(5:12, size = 20, replace = TRUE), 
#                                sample(1:19, size = 20, replace = TRUE), 
#                                sep = "-")) %>% 
#   mutate(dt_symp_onset_case = as.Date(dt_symp_onset_case, 
#                                       format = "%Y-%m-%d")) %>% 
#   slice(rep(1:n(), each = 10))  %>% 
#   arrange(id_case)
# 
# 
# 
# contacts_male <- 
#   tibble(last_name_contact = randomNames(n = 100, which.names = "last", gender = 0), 
#          first_name_contact = randomNames(n = 100, which.names = "first", gender = 0), 
#          sex_contact = "Male"
#   )
# 
# contacts_female <- 
#   tibble(last_name_contact = randomNames(n = 100, which.names = "last", gender = 1), 
#          first_name_contact = randomNames(n = 100, which.names = "first", gender = 1), 
#          sex_contact = "Female")
# 
# contacts_init <- 
#   contacts_male %>% 
#   bind_rows(contacts_female) %>% 
#   mutate(id_contact_end = sample(1:200, 200)) %>% 
#   arrange(id_contact_end)
# 
# 
# sample_contacts_df <- 
#   cases %>% 
#   bind_cols(contacts_init) %>% 
#   mutate(id_contact = paste(id_case, 1:10, sep = "-"),
#          relation_contact_case = sample(c("family member", "co-worker", "community-member", "patient"), 
#                                         size = n(),
#                                         replace = TRUE),
#          age_contact = sample(0:100, size = n(), replace = TRUE), 
#          phone_contact = ch_phone_number(n = n()), 
#          hcw_status_contact = sample(c("Yes", "No"), size = n(), replace = TRUE),
#          admin1_contact = "Lagos",
#          admin2_contact = sample(c("Agege", "Ajeromi-Ifelodun", "Alimosho", "Amuwo-Odofin",
#                                    "Apapa", "Badagry", "Epe", "Eti-Osa", "Ibeju/Lekki",
#                                    "Ifako-Ijaye", "Ikeja", "Ikorodu", "Kosofe",
#                                    "Lagos Island", "Lagos Mainland", "Mushin", "Ojo",
#                                    "Oshodi-Isolo", "Shomolu", "Surulere"), 
#                                  size = n(), 
#                                  replace = TRUE), 
#          admin3_contact = NA_character_, 
#          hhld_name_contact = NA_character_, 
#          type_contact = sample(c("High-risk", "Low-risk"), size = n(), replace = TRUE),
#          vital_state_contact = sample(c("Healthy", "Ill", "Deceased"), size = n(), replace = TRUE),
#          dt_death_or_illness_contact = dt_symp_onset_case + sample(1:50, size = n(), replace = TRUE),
#          # should be after date of symptom onset but before date of death or illness
#          dt_last_interaction = dt_symp_onset_case + sample(1:50, size = n(), replace = TRUE),
#          # if data of last interaction is after date of death or illness, set it as 10 days before date of death
#          dt_last_interaction = ifelse(vital_state_contact == "Ill" | vital_state_contact == "Deceased", 
#                                       dt_death_or_illness_contact - 10, 
#                                       dt_last_interaction), 
#          follow_up_status = "Inactive",
#          loc_death_contact = ifelse(vital_state_contact == "Deceased", 
#                                     sample(admin2_contact), 
#                                     NA_character_),
#          # everyday lose 10 people to followup
#          follow_up_1 = c(rep(1, each = 190), 
#                          rep(0, each = 10)), 
#          follow_up_2 = c(rep(1, each = 180), 
#                          rep(0, each = 20)),
#          follow_up_3 = c(rep(1, each = 170), 
#                          rep(0, each = 30)),
#          follow_up_4 = c(rep(1, each = 160), 
#                          rep(0, each = 40)),
#          follow_up_5 = c(rep(1, each = 150), 
#                          rep(0, each = 50)),
#          follow_up_6 = c(rep(1, each = 140), 
#                          rep(0, each = 60)),
#          follow_up_7 = c(rep(1, each = 130), 
#                          rep(0, each = 70)),
#          follow_up_8 = c(rep(1, each = 120), 
#                          rep(0, each = 80)),
#          follow_up_9 = c(rep(1, each = 110), 
#                          rep(0, each = 90)),
#          follow_up_10 = c(rep(1, each = 100), 
#                           rep(0, each = 100)),
#          follow_up_11 = c(rep(1, each = 90), 
#                           rep(0, each = 110)),
#          follow_up_12 = c(rep(1, each = 80), 
#                           rep(0, each = 120)),
#          follow_up_13 = c(rep(1, each = 70), 
#                           rep(0, each = 130)),
#          follow_up_14 = c(rep(1, each = 60), 
#                           rep(0, each = 140)), 
#          dt_death_or_illness_contact = ifelse(vital_state_contact == "Healthy", 
#                                               NA_character_, 
#                                               dt_death_or_illness_contact))
# 
# 
# 
# write_rds(sample_contacts_df, here("data/sample_contacts_df.RDS"))
# 
