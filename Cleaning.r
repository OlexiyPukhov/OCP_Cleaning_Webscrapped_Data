if (!require("pacman")) install.packages("pacman")
pacman::p_load(pacman, rio, tidyverse, ggplot2, rmarkdown, lubridate,
               skimr, doParallel, patchwork, tidyquant, plotly, DataExplorer,
               rvest, RSelenium, netstat, stringr, tidyr, seleniumPipes,
               webshot, strex, GGally, trelliscopejs)

pharmacists_data_raw = readRDS("data.rds")
pharmacists_data_cleaned = pharmacists_data_raw

# General Info ----

#Separate gen_txt into gen1 dividing the string by gender (I work on it in chunks)
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
        mutate(gen1 = str_before_nth(gen_txt, "Gender:", n = 1)) %>% 
        mutate(gen_txt = str_after_nth(gen_txt, "Gender:", n = 1))

#Separate gen1 by the first pattern of | and get the name
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(name = str_before_nth(gen1, "\\|", n = 1)) %>% 
    mutate(gen1 = str_after_nth(gen1, "\\|", n = 1))

##Separate gen1 by the second pattern of | and get the status
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(status = str_before_nth(gen1, "\\|", n = 1)) %>% 
    mutate(gen1 = str_after_nth(gen1, "\\|", n = 1))

#Separate gen1 by the third pattern of | and get the role
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(role = str_before_nth(gen1, "\\|", n = 1)) %>% 
    mutate(gen1 = str_after_nth(gen1, "\\|", n = 1))

#Separate gen1 by the pattern of general information and removed the string 
#that is was got from
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(geninfo = str_before_nth(gen1, "General Information", n = 1)) %>% 
    mutate(gen1 = str_after_nth(gen1, "General Information", n = 1)) %>% 
    select(-geninfo)

#Get the registration number from the 2nd pattern of | (the data uses || for new pages of info)
#then make it numeric
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(registration_number = str_before_nth(gen1, "\\|", n = 2)) %>% 
    mutate(gen1 = str_after_nth(gen1, "\\|", n = 2)) %>% 
    mutate(registration_number = str_first_number(registration_number)) %>% 
    mutate(registration_number = registration_number %>% as.numeric())

#Separate gen1 dividing by the first pattern of | (we just keep dividing by the first pattern to get new variables) to get part
#and removing the first pattern from the parent string. If there is no data (the parent string is empty) replace with no data string.
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(part = ifelse(gen1 != "", str_before_nth(gen1, "\\|", n = 1), "No Data")) %>% 
    select(-gen1)

#Separate gen_txt dividing the string by the first pattern of | and remove the pattern from parent string, getting gender in the process
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(gender = str_before_nth(gen_txt, "\\|", n = 1)) %>% 
    mutate(gen_txt = str_after_nth(gen_txt, "\\|", n = 1))

#Work with a new chunk of text by separating by the first pattern of workplaces and remove the pattern from parent string
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(gen1 = str_before_nth(gen_txt, "Workplaces", n = 1)) %>% 
    mutate(gen_txt = str_after_nth(gen_txt, "Workplaces", n = 1))

#Remove the preferred name section from the split off chunk
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(gen1 = ifelse(
        (str_before_nth(gen1, "\\|", n = 1)) %>% 
            str_detect("Preferred name") == TRUE,
        str_after_nth(gen1, "\\|", n = 1),
        gen1))
#Remove the previous name section from the split off chunk
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(gen1 = ifelse(
        (str_before_nth(gen1, "\\|", n = 1)) %>% 
            str_detect("Previous name") == TRUE,
        str_after_nth(gen1, "\\|", n = 1),
        gen1))

#if the string mentions being able to immunize, set that column to yes otherwise no and cut the string by the pattern again
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(trained_to_immunize = ifelse(
        (str_before_nth(gen1, "\\|", n = 1) %>% 
             str_detect("injections")) == TRUE,
        "Yes", "No")) %>% 
    mutate(gen1 = ifelse(
    (str_before_nth(gen1, "\\|", n = 1)) %>% 
        str_detect("injections") == TRUE,
    str_after_nth(gen1, "\\|", n = 1),
    gen1))

#Get the last part from the chunk which is the languages, after the pattern of provides pharmacy services.
#Remove unneeded strings with blank spaces and replace N/A (no information) with no data string
#then remove the chunk i was working on.
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(languages = str_after_nth(gen1, "Provides pharmacy services in:", n = 1)) %>% 
    mutate(languages = str_replace_all(languages, "\\|", "" )) %>% 
    mutate(languages = str_replace_all(languages, " , ", ", " )) %>% 
    mutate(languages = ifelse(is.na(languages), yes = "No Data", no = languages)) %>% 
    mutate(trained_to_immunize = ifelse(is.na(trained_to_immunize), yes = "No Data", no = trained_to_immunize)) %>% 
    select(-gen1)

#Separate the people who have not reported a workplace
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces = ifelse(str_detect(gen_txt, "This pharmacy professional has not reported a workplace."),
                               "This pharmacy professional has not reported a workplace.", "Pending")) %>% 
    mutate(gen_txt = str_remove(gen_txt, "This pharmacy professional has not reported a workplace.")) 

pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces = ifelse(str_detect(gen_txt, "This person is not entitled to practice and does not have a workplace."),
                           "This person is not entitled to practice and does not have a workplace.", workplaces)) %>% 
    mutate(gen_txt = str_remove(gen_txt, "This person is not entitled to practice and does not have a workplace."))
    
#Find the people working in USA and add to stores working in counter
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces = ifelse(str_detect(gen_txt, "\\|USA\\|"),
                                   str_before_nth(gen_txt, "\\|USA\\|", n = 1),
                                   workplaces)) %>% 
    mutate(countries_working_in = ifelse(str_detect(gen_txt, "\\|USA\\|"),
                               "USA", "Pending")) %>% 
    mutate(stores_working_at = ifelse(str_detect(gen_txt, "\\|USA\\|"),
                                         1, 0)) %>% 
    mutate(gen_txt = ifelse(str_detect(gen_txt, "\\|USA\\|"),
                                str_after_nth(gen_txt, "\\|USA\\|", n = 1),
                                gen_txt)) %>% 
    mutate(workplaces = str_remove(workplaces, "\\|"))

#Find the people working in USA and Canada, add to countries_working_in and stores working at counter
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces2 = ifelse(str_detect(workplaces, "\\|Canada\\|"),
                               str_before_nth(workplaces, "\\|Canada\\|", n = 1),
                               "")) %>% 
    mutate(stores_working_at = ifelse(str_detect(workplaces, "\\|Canada\\|"),
                                      stores_working_at + 1, stores_working_at)) %>% 
    mutate(countries_working_in = ifelse(str_detect(workplaces, "\\|Canada\\|"),
                                         str_c(countries_working_in, ", Canada"),
                                         countries_working_in)) %>% 
    mutate(workplaces = ifelse(str_detect(workplaces, "\\|Canada\\|"),
                            str_after_nth(workplaces, "\\|Canada\\|", n = 1),
                            workplaces))

#Keep just the name of the store their working at, dropping the location, address, postal code, and city/province
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces = ifelse(!workplaces %in% c("Pending", "This pharmacy professional has not reported a workplace.", "This person is not entitled to practice and does not have a workplace."),
                               str_before_nth(workplaces, "\\|", n = 1),
                               workplaces)) %>% 
    mutate(workplaces2 = ifelse(workplaces2 != "",
                               str_before_nth(workplaces2, "\\|", n = 1),
                               workplaces2))

#Put USA and Canada in the same workplace column, unite them, and remove workplace2
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces = ifelse(workplaces != "Pending" & workplaces2 != "" & workplaces != "This pharmacy professional has not reported a workplace.",
                               yes = str_c(workplaces, ", ", workplaces2),
                               no = workplaces)) %>% 
    select(-workplaces2)


#There's an extra | pattern, remove it
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(gen_txt = str_remove(gen_txt, "\\|"))

#Remove next | pattern from chunk. Update counters. Fix counters. Get Canada working locations
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces2 = ifelse(str_detect(gen_txt, "\\|Canada\\|"),
                                str_before_nth(gen_txt, "\\|Canada\\|", n = 1),
                                "")) %>% 
    mutate(stores_working_at = ifelse(str_detect(gen_txt, "\\|Canada\\|"),
                                      stores_working_at + 1, stores_working_at)) %>% 
    mutate(countries_working_in = ifelse(str_detect(gen_txt, "\\|Canada\\|"),
                                         str_c(countries_working_in, ", Canada"),
                                         countries_working_in)) %>% 
    mutate(gen_txt = ifelse(str_detect(gen_txt, "\\|Canada\\|"),
                               str_after_nth(gen_txt, "\\|Canada\\|", n = 1),
                               gen_txt)) %>% 
    mutate(countries_working_in = ifelse(countries_working_in == "Pending, Canada",
                                     yes = "Canada",
                                     no = countries_working_in)) %>% 
    mutate(countries_working_in = ifelse(countries_working_in == "USA, Canada, Canada",
                                         yes = "USA, Canada",
                                         no = countries_working_in))

#Keep just the name of the store their working at, dropping the location, address, postal code, and city/province
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces2 = ifelse(workplaces2 != "",
                                str_before_nth(workplaces2, "\\|", n = 1),
                                workplaces2))
#Replace pending with workplace2
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces = ifelse((workplaces2 != "" & workplaces == "Pending"),
                                workplaces2,
                                workplaces))

#Isolate workplace2 that do not work in just 1 location
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces2 = ifelse(workplaces2 == workplaces,
                                "",
                                workplaces2))

#People who work in the USA and Canada and Canada follow different string pasterns.
#So getting their work name only by a numeric pattern
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces2 = ifelse(workplaces2 != "",
                                str_first_non_numeric(workplaces2),
                                workplaces2))

#Fix cases not separating correctly
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces2 = ifelse(str_detect(workplaces2, "Windsor Regional Hospital"),
                                "Windsor Regional Hospital",
                                workplaces2))

#Fix another case not separating correctly
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces2 = ifelse(str_detect(workplaces2, "Brantford Commons PharmacyH"),
                                "Brantford Commons Pharmacy",
                                workplaces2))

#Add to workplaces from working var workplaces2
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces = ifelse((workplaces != "Pending" & 
                                    workplaces2 != "" & 
                                    workplaces != "This pharmacy professional has not reported a workplace." & 
                                    workplaces != "This person is not entitled to practice and does not have a workplace."),
                               yes = str_c(workplaces, ", ", workplaces2),
                               no = workplaces)) %>% 
    select(-workplaces2)

pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces2 = ifelse(str_detect(gen_txt, "\\|Canada\\|"),
                                str_before_nth(gen_txt, "\\|Canada\\|", n = 1),
                                "")) %>% 
    mutate(stores_working_at = ifelse(str_detect(gen_txt, "\\|Canada\\|"),
                                      stores_working_at + 1, stores_working_at)) %>% 
    mutate(countries_working_in = ifelse(str_detect(gen_txt, "\\|Canada\\|") & str_detect(countries_working_in, "Canada") == FALSE,
                                         str_c(countries_working_in, ", Canada"),
                                         countries_working_in)) %>% 
    mutate(gen_txt = ifelse(str_detect(gen_txt, "\\|Canada\\|"),
                            str_after_nth(gen_txt, "\\|Canada\\|", n = 1),
                            gen_txt))

#Removing the Designated Manager from the data because it is complicating trying to parse the data.
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(gen_txt = str_remove_all(gen_txt, "Designated Manager\\|")) %>% 
    mutate(gen_txt = str_remove_all(gen_txt, "Authorized to order/receive controlled substances\\|")) %>% 
    mutate(workplaces2 = str_remove_all(workplaces2, "Designated Manager\\|")) %>% 
    mutate(workplaces2 = str_remove_all(workplaces2, "Authorized to order/receive controlled substances\\|"))

#Keep just the name of the store their working at, dropping the location, address, postal code, and city/province
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces2 = ifelse(workplaces2 != "",
                                str_before_nth(workplaces2, "\\|", n = 1),
                                workplaces2))

#Add working workplace2 to workplace
pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(workplaces = ifelse((workplaces != "Pending" & 
                                    workplaces2 != "" & 
                                    workplaces != "This pharmacy professional has not reported a workplace." & 
                                    workplaces != "This person is not entitled to practice and does not have a workplace."),
                               yes = str_c(workplaces, ", ", workplaces2),
                               no = workplaces)) %>% 
    select(-workplaces2)

#Repeating what I did above for all of the rest of the Canadian working locations.
# Note: Different locations will appear twice. For example, if someone works at 2 shoppers locations, it will show up in the workplaces
# column aS "Shoppers drug mart, shoppers drug mart". 
canadas = TRUE

while (canadas != FALSE) {
    canadas = TRUE %in% (pharmacists_data_cleaned$gen_txt %>% str_detect("\\|Canada\\|"))
    if (canadas == FALSE) break
    
    pharmacists_data_cleaned = pharmacists_data_cleaned %>%
        mutate(workplaces2 = ifelse(str_detect(gen_txt, "\\|Canada\\|"),
                                    str_before_nth(gen_txt, "\\|Canada\\|", n = 1),
                                    "")) %>% 
        mutate(stores_working_at = ifelse(str_detect(gen_txt, "\\|Canada\\|"),
                                          stores_working_at + 1, stores_working_at)) %>% 
        mutate(countries_working_in = ifelse(str_detect(gen_txt, "\\|Canada\\|") & str_detect(countries_working_in, "Canada") == FALSE,
                                             str_c(countries_working_in, ", Canada"),
                                             countries_working_in)) %>% 
        mutate(gen_txt = ifelse(str_detect(gen_txt, "\\|Canada\\|"),
                                str_after_nth(gen_txt, "\\|Canada\\|", n = 1),
                                gen_txt))
    
    #Keep just the name of the store their working at, dropping the location, address, postal code, and city/province
    pharmacists_data_cleaned = pharmacists_data_cleaned %>%
        mutate(workplaces2 = ifelse(workplaces2 != "",
                                    str_before_nth(workplaces2, "\\|", n = 1),
                                    workplaces2))
    
    pharmacists_data_cleaned = pharmacists_data_cleaned %>%
        mutate(workplaces = ifelse((workplaces != "Pending" & 
                                        workplaces2 != "" & 
                                        workplaces != "This pharmacy professional has not reported a workplace." & 
                                        workplaces != "This person is not entitled to practice and does not have a workplace."),
                                   yes = str_c(workplaces, ", ", workplaces2),
                                   no = workplaces)) %>% 
        select(-workplaces2)

}

# Cleaning up countries_working_in

pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(countries_working_in = ifelse(workplaces == "This pharmacy professional has not reported a workplace.",
                                         "Unknown", countries_working_in))

pharmacists_data_cleaned = pharmacists_data_cleaned %>%
    mutate(countries_working_in = ifelse(workplaces == "This person is not entitled to practice and does not have a workplace.",
                                         "Can't Practice", countries_working_in))
                                
# There is still information on other corporations and health profession corporations info to be parsed in gen_txt.
# If you require this info, I can parse it too. However at the time of making this script I did not think it would add
# sufficient value to the analysis, so I dropped it from the rest of the script.

pharmacists_data_cleaned = pharmacists_data_cleaned %>% select(-gen_txt)

# Registration History ----

# All of the dates are currently registered times. They are the most recent time on OCP. 
# For example, if a field is pharmacist_registration_date is 1970, they are currently a registered pharmacist initially registered in 1970.
# Likewise, if a field is student_registration_date is 2021, they are currently a student initially registered in 2021.
# Further, if a field is pharmacist_current_initial_resignation_date is that they were a pharmacist that resigned at this date.

#Every field has the same information at the top. This was parsed the first time. Remove the unneeded info.
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(reg_history_txt = str_after_nth(reg_history_txt, "Member Type", n = 1))

#All of the info on registration history
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(reg_history_txt = str_before_nth(reg_history_txt, "\\|", n = 7))

#Different case when scenarios and adding info to dates
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(pharmacist_current_initial_registration_date = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Current\\|"),
                                                         yes = str_after_nth(reg_history_txt, "Pharmacist\\|Current\\|", n = 1),
                                                         no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Current\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(pharmacist_current_initial_registration_date = pharmacist_current_initial_registration_date  %>% mdy())

    
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(pharmacist_current_initial_resignation_date = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Resigned\\|"),
                                                         yes = str_after_nth(reg_history_txt, "Pharmacist\\|Resigned\\|", n = 1),
                                                         no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Resigned\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(pharmacist_current_initial_resignation_date = pharmacist_current_initial_resignation_date  %>% mdy())

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(pharmacist_current_cancelled_or_suspended_for_nonpayment_date = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Cancelled for Non-payment\\|"),
                                                yes = str_after_nth(reg_history_txt, "Pharmacist\\|Cancelled for Non-payment\\|", n = 1),
                                                no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Cancelled for Non-payment\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(pharmacist_current_cancelled_or_suspended_for_nonpayment_date = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Suspended for Non - Payment\\|"),
                                                                                  yes = str_after_nth(reg_history_txt, "Pharmacist\\|Suspended for Non - Payment\\|", n = 1),
                                                                                  no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Suspended for Non - Payment\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(pharmacist_current_cancelled_or_suspended_for_nonpayment_date = pharmacist_current_cancelled_or_suspended_for_nonpayment_date  %>% mdy())

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(pt_current_cancelled_or_suspended_for_nonpayment_date = ifelse(str_detect(reg_history_txt, "Pharmacy Technician\\|Cancelled for Non-payment\\|"),
                                                                                  yes = str_after_nth(reg_history_txt, "Pharmacy Technician\\|Cancelled for Non-payment\\|", n = 1),
                                                                                  no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Pharmacy Technician\\|Cancelled for Non-payment\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(pt_current_cancelled_or_suspended_for_nonpayment_date = ifelse(str_detect(reg_history_txt, "Pharmacy Technician\\|Suspended for Non - Payment\\|"),
                                                                                  yes = str_after_nth(reg_history_txt, "Pharmacy Technician\\|Suspended for Non - Payment\\|", n = 1),
                                                                                  no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Pharmacy Technician\\|Suspended for Non - Payment\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(pt_current_cancelled_or_suspended_for_nonpayment_date = pt_current_cancelled_or_suspended_for_nonpayment_date  %>% mdy())

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(intern_current_initial_registration_date = ifelse(str_detect(reg_history_txt, "Intern\\|Current\\|"),
                                                             yes = str_after_nth(reg_history_txt, "Intern\\|Current\\|", n = 1),
                                                             no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Intern\\|Current\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(intern_current_initial_registration_date = intern_current_initial_registration_date  %>% mdy())

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(student_current_initial_registration_date = ifelse(str_detect(reg_history_txt, "Student\\|Current\\|"),
                                                     yes = str_after_nth(reg_history_txt, "Student\\|Current\\|", n = 1),
                                                     no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Student\\|Current\\|"),
                                                              yes = "",
                                                              no = reg_history_txt)) %>% 
    mutate(student_current_initial_registration_date = student_current_initial_registration_date  %>% mdy())

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(student_current_initial_expired_date = ifelse(str_detect(reg_history_txt, "Student\\|Expired\\|"),
                                                              yes = str_after_nth(reg_history_txt, "Student\\|Expired\\|", n = 1),
                                                              no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Student\\|Expired\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(student_current_initial_expired_date = student_current_initial_expired_date  %>% mdy())

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(pt_current_initial_reg_date = ifelse(str_detect(reg_history_txt, "Pharmacy Technician\\|Current\\|"),
                                                         yes = str_after_nth(reg_history_txt, "Pharmacy Technician\\|Current\\|", n = 1),
                                                         no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Pharmacy Technician\\|Current\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(pt_current_initial_reg_date = pt_current_initial_reg_date  %>% mdy())

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(pt_current_initial_resigned_date = ifelse(str_detect(reg_history_txt, "Pharmacy Technician\\|Resigned\\|"),
                                                yes = str_after_nth(reg_history_txt, "Pharmacy Technician\\|Resigned\\|", n = 1),
                                                no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Pharmacy Technician\\|Resigned\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(pt_current_initial_resigned_date = pt_current_initial_resigned_date  %>% mdy())

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(pt_or_pharmacist_deceased_date = ifelse(str_detect(reg_history_txt, "Deceased\\|"),
                                                     yes = str_after_nth(reg_history_txt, "\\|", n = 6),
                                                     no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Deceased"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(pt_or_pharmacist_deceased_date = pt_or_pharmacist_deceased_date  %>% mdy())

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(pharmacist_current_suspended_or_revoked_date = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Suspended for Discipline\\|"),
                                                                          yes = str_after_nth(reg_history_txt, "Pharmacist\\|Suspended for Discipline\\|", n = 1),
                                                                          no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Suspended for Discipline\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(pharmacist_current_suspended_or_revoked_date = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Revoked\\|"),
                                                                          yes = str_after_nth(reg_history_txt, "Pharmacist\\|Revoked\\|", n = 1),
                                                                          no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Revoked\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(pharmacist_current_suspended_or_revoked_date = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Suspended - Other\\|"),
                                                                 yes = str_after_nth(reg_history_txt, "Pharmacist\\|Suspended - Other\\|", n = 1),
                                                                 no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Suspended - Other\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(pharmacist_current_suspended_or_revoked_date = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Interim Suspension\\|"),
                                                                 yes = str_after_nth(reg_history_txt, "Pharmacist\\|Interim Suspension\\|", n = 1),
                                                                 no = "")) %>% 
    mutate(reg_history_txt = ifelse(str_detect(reg_history_txt, "Pharmacist\\|Interim Suspension\\|"),
                                    yes = "",
                                    no = reg_history_txt)) %>% 
    mutate(pharmacist_current_suspended_or_revoked_date = pharmacist_current_suspended_or_revoked_date  %>% mdy())

#In total, there are 107 more records that can be parsed. However with 27 of these, they are missing a status so it's hard to make sense of
#them. The rest (80) are interns that are expired. I can get this info if you need it, but I thought it's unnecessary for further analysis.

#Finish with the reg history; remove it

pharmacists_data_cleaned = pharmacists_data_cleaned %>% select(-reg_history_txt)

# Academic History ----

#Every field has the same information at the top. This was parsed the first time. Remove the unneeded info.
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(aca_his_txt = str_after_nth(aca_his_txt, "Qualifying Education\\|", n = 1))

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(education_site = str_before_nth(aca_his_txt, "\\|", n = 1)) %>% 
    mutate(aca_his_txt = str_after_nth(aca_his_txt, "\\|", n = 1))

#If the pharmacy professional has no education site, you get a weird pattern result by splitting by pattern. Detect this
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(education_site = ifelse(str_detect(education_site, "Other Registered Training") | is.na(education_site),
                                              "This pharmacy professional has not reported a education site.",
                                              education_site))

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(education_country = str_before_nth(aca_his_txt, "\\|", n = 1)) %>% 
    mutate(aca_his_txt = str_after_nth(aca_his_txt, "\\|", n = 1))

#Same weird pattern
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(education_country = ifelse(str_detect(education_country, "Injection Training") | is.na(education_country),
                                   "This pharmacy professional has not reported a education site.",
                                   education_country))

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(aca_his_txt = ifelse(str_detect(education_country, "Graduated"),
                                      str_c(education_country, "\\|", aca_his_txt),
                                      aca_his_txt))

#Get graduation date info and convert into a datetime using lubridate
pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(education_graduation_date = str_before_nth(aca_his_txt, "\\|", n = 1)) %>% 
    mutate(aca_his_txt = str_after_nth(aca_his_txt, "\\|", n = 1)) %>% 
    mutate(education_graduation_date = str_after_nth(education_graduation_date, "Graduated\\: ", n = 1)) %>% 
    mutate(education_graduation_date = education_graduation_date %>% my())

#Make a column if that person is an international grad outside of Canada

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(international_grad = ifelse(
        str_detect(education_country, "Canada") & !str_detect(education_site, "This pharmacy professional has not reported a education site."), "No", "Yes")) %>% 
    mutate(international_grad = ifelse(
        str_detect(education_country, "This pharmacy professional has not reported a education site."), "No Data", international_grad))


#Finished with aca_his. Remove it

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    select(-aca_his_txt)

# Concerns info ----

# This does not get the number of times they did the action. For example, it someone did professional misconduct twice,
# under the concerns column it would show up as "professional misconduct", not "professional misconduct, professional misconduct".

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(concerns = ifelse(str_detect(concerns_txt, "The College is not aware of any concerns that are relevant to this practitioner’s suitability to practice."),
                             "No concerns", "")) %>% 
    mutate(concerns_txt = ifelse(str_detect(concerns_txt, "The College is not aware of any concerns that are relevant to this practitioner’s suitability to practice."),
                             "", concerns_txt))

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(concerns = ifelse(str_detect(concerns_txt, "Professional Misconduct"),
                             "Professional Misconduct", concerns)) %>% 
    mutate(concerns = ifelse(str_detect(concerns_txt, "Federal or Provincial Findings of Guilt"),
                         str_c(concerns, ", ", "Federal or Provincial Findings of Guilt"), concerns)) %>% 
    mutate(concerns = ifelse(str_detect(concerns_txt, "Proprietary Misconduct"),
                         str_c(concerns, ", ", "Proprietary Misconduct"), concerns)) %>% 
    mutate(concerns = ifelse(str_detect(concerns_txt, "Caution and Remedial Training"),
                             str_c(concerns, ", ", "Caution and Remedial Training"), concerns)) %>% 
    mutate(concerns = ifelse(str_detect(concerns_txt, "Federal or Provincial Charges"),
                             str_c(concerns, ", ", "Federal or Provincial Charges"), concerns)) %>%
    mutate(concerns = ifelse(str_detect(concerns_txt, "Application for Reinstatement"),
                             str_c(concerns, ", ", "Application for Reinstatement"), concerns)) %>% 
    mutate(concerns = ifelse(str_detect(concerns_txt, "Outcome: Caution"),
                             str_c(concerns, ", ", "Caution from OCP"), concerns)) %>% 
    mutate(concerns = ifelse(concerns == "", "Pending trial", concerns))

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    mutate(concerns = ifelse(str_starts(concerns, ", "), str_remove(concerns, ", "), concerns))

# Finish with concerns_txt, remove it

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    select(-concerns_txt)

# There are duplicates in the data set. This is caused by database updates and quirks with the web scraping.
# Overall I managed to get 26905 unique records, with 200 duplicates.

pharmacists_data_cleaned = pharmacists_data_cleaned %>% distinct()

# Reformatting the order of the columns of the data set

pharmacists_data_cleaned = pharmacists_data_cleaned %>% 
    select(name, role, status, registration_number,
           part, gender, workplaces, languages,
           trained_to_immunize, countries_working_in, stores_working_at,
           education_site, education_country, education_graduation_date,
           international_grad, concerns, everything())

write.csv(pharmacists_data_cleaned, "OCP_Pharmacists_2022.csv")
