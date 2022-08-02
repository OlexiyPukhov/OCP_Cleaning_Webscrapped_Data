    if (!require("pacman")) install.packages("pacman")
    pacman::p_load(pacman, rio, tidyverse, ggplot2, rmarkdown, lubridate,
                   skimr, doParallel, patchwork, tidyquant, plotly, DataExplorer,
                   rvest, RSelenium, netstat, stringr, tidyr, seleniumPipes,
                   webshot, strex, GGally, trelliscopejs, tidygeocoder, mapview, sf, optbin, data.table)

    library(rio)
    options(scipen=999)
    
    pharmacies_data_raw = readRDS("Pharmacies_2022_V5.0_Complete.rds")
    pharmacies_data_cleaned = pharmacies_data_raw
    pharmacists = import("OCP_Pharmacists_2022_V12.rds")
    
    # General Info ----
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>% distinct()
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
        mutate(name = str_before_nth(gen_txt, "\\|", n = 1)) %>% 
        mutate(gen_txt = str_after_nth(gen_txt, "\\|", n = 1))
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
        mutate(status = str_before_nth(gen_txt, "\\|", n = 1)) %>% 
        mutate(gen_txt = str_after_nth(gen_txt, "\\|", n = 1))
    
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
        mutate(type = str_before_nth(gen_txt, "\\|", n = 1)) %>% 
        mutate(gen_txt = str_after_nth(gen_txt, "\\|", n = 1)) %>% 
        mutate(type = ifelse(str_detect(type, "General Information"),
                             yes = "Non-accredited Workplace",
                             no = type))
    
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>%
        mutate(accreditation_number = ifelse(str_detect(status, "Accreditation number"),
                                             str_after_nth(gen_txt, "Accreditation number:", n = 1),
                                            NA)) %>% 
        mutate(gen_txt = ifelse(str_detect(gen_txt, "Accreditation number:"),
                                str_after_nth(gen_txt, "Accreditation number:", n = 1),
                                gen_txt)) %>% 
        mutate(accreditation_number = ifelse(type != "Non-accredited Workplace", str_before_nth(gen_txt, "\\|", n = 1), accreditation_number)) %>% 
        mutate(gen_txt = ifelse(type != "Non-accredited Workplace", str_after_nth(gen_txt, "\\|", n = 1), gen_txt))
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>%
        mutate(address =  ifelse(type != "Non-accredited Workplace", str_before_nth(gen_txt, "Phone:", n = 1), "")) %>% 
        mutate(gen_txt =  ifelse(type != "Non-accredited Workplace", str_after_nth(gen_txt, "Phone:", n = 1), gen_txt)) %>% 
        mutate(address =  ifelse(type == "Non-accredited Workplace", str_before_nth(gen_txt, "\\|", n = 1), address)) %>% 
        mutate(gen_txt =  ifelse(type == "Non-accredited Workplace", str_after_nth(gen_txt, "\\|", n = 1), gen_txt))

    pharmacies_data_cleaned = pharmacies_data_cleaned %>%
        mutate(postal_code = ifelse(type != "Non-accredited Workplace", str_after_nth(address, "\\|", n = -2), "")) %>% 
        mutate(postal_code = str_remove(postal_code, "\\|")) %>% 
        mutate(address = ifelse(type != "Non-accredited Workplace", str_before_nth(address, "\\|", n = -2), address))
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>%
        mutate(address_city_province = ifelse(type != "Non-accredited Workplace", str_after_nth(address, "\\|", n = -1), "")) %>% 
        mutate(address = ifelse(type != "Non-accredited Workplace", str_before_nth(address, "\\|", n = -1), address)) %>% 
        mutate(address_city_province = ifelse(type == "Non-accredited Workplace", str_before_nth(gen_txt, "\\|", n = 1), address_city_province)) %>% 
        mutate(gen_txt = ifelse(type == "Non-accredited Workplace", str_after_nth(gen_txt, "\\|", n = 1), gen_txt)) %>% 
        mutate(postal_code = ifelse(type == "Non-accredited Workplace", str_before_nth(gen_txt, "\\|", n = 1), postal_code)) %>% 
        mutate(gen_txt = ifelse(type == "Non-accredited Workplace", str_after_nth(gen_txt, "\\|", n = 1), gen_txt)) %>% 
        mutate(gen_txt = ifelse(type == "Non-accredited Workplace", str_after_nth(gen_txt, "Phone:", n = 1), gen_txt))
    
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>%
        mutate(phone_num = str_before_nth(gen_txt, "\\|", n = 1)) %>% 
        mutate(gen_txt = str_after_nth(gen_txt, "\\|", n = 1)) 
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>%
        mutate(fax_num = str_before_nth(gen_txt, "\\|", n = 1)) %>% 
        mutate(gen_txt = str_after_nth(gen_txt, "\\|", n = 1)) 
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>%
        mutate(date_issued = str_before_nth(gen_txt, "\\|", n = 1)) %>% 
        mutate(gen_txt = str_after_nth(gen_txt, "\\|", n = 1)) 
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
        mutate(owner_or_corporation = ifelse(date_issued == "Owner/Corporation", gen_txt, "pending"))
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>%
        mutate(gen_txt = str_after_nth(gen_txt, "Owner/Corporation", n = 1))
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
        mutate(owner_or_corporation = ifelse(!owner_or_corporation == "pending", str_before_nth(owner_or_corporation, "\\|", n = 1), owner_or_corporation))
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>%
        mutate(owner_or_corporation = ifelse(owner_or_corporation == "pending", str_before_nth(gen_txt, "\\|", n = 2), owner_or_corporation)) %>% 
        mutate(gen_txt = str_after_nth(gen_txt, "\\|", n = 2)) %>% 
        mutate(owner_or_corporation = str_remove(owner_or_corporation, "\\|"))
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
        mutate(date_issued = ifelse(str_detect(date_issued, "Closed Date"), "NA", date_issued)) %>% 
        mutate(date_issued = ifelse(str_detect(fax_num, "Date"), fax_num, date_issued)) %>% 
        mutate(date_issued = str_remove(date_issued, "Date issued: ")) %>% 
        mutate(date_issued = date_issued %>% mdy())
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>% select(-gen_txt)
    
    # Pharmacy Staff ----
    # POSSIBLE IMPROVEMENT
    pharmacies_data_cleaned = pharmacies_data_cleaned %>%
        mutate(pharm_staff_txt = str_remove(pharm_staff_txt, "Designated Manager\\|")) %>% 
        mutate(pharm_staff_txt = str_remove(pharm_staff_txt, "Back to search"))
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>%
        mutate(pharm_staff_txt = ifelse(str_detect(pharm_staff_txt, "Pharmacy Staff"),
                                        str_after_nth(pharm_staff_txt, "Pharmacy Staff\\|", n = 1),
                                        pharm_staff_txt))
    
    pharmacies_data_cleaned = pharmacies_data_cleaned %>%
        mutate(pharmacy_staff = "") %>% 
        mutate(employees_employed = 0) %>% 
        mutate(international_grad_employees_employed = 0)
    
    





pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    drop_na(pharm_staff_txt)

errors = 0
for (i in 1:nrow(pharmacies_data_cleaned)) {
    if (pharmacies_data_cleaned$pharm_staff_txt[i] != "" & pharmacies_data_cleaned$status[i] == "Entitled to operate") {
        num_staff = str_count(pharmacies_data_cleaned$pharmacy_staffs_primary_keys_url[i],  "\\,")
        
        for (a in 1:num_staff) {
            profname <- str_before_nth(pharmacies_data_cleaned$pharm_staff_txt[i], "\\|", n = 1)
            occupation <- str_before_nth(pharmacies_data_cleaned$pharm_staff_txt[i], "\\|", n = 2) %>% str_after_nth("\\|", n = 1) 
            primary_key_pharmacist <- pharmacies_data_cleaned %>% 
                slice(i) %>% 
                select(pharmacy_staffs_primary_keys_url) %>% 
                pull() %>% 
                str_after_nth("\\,", a)
            
            if (!str_before_nth(primary_key_pharmacist, "\\,", 1) %>% is.na()) {primary_key_pharmacist = primary_key_pharmacist %>% str_before_nth("\\,", 1)}
            
            searched_pharmacist <- pharmacists %>% filter(primary_key_pharmacist == pharmacist_url)
            #searched_pharmacist <- pharmacists %>% filter(name == profname, occupation == role, primary_key_pharmacist == primary_key_url)
            if (nrow(searched_pharmacist) > 0) {
                reg_num <- searched_pharmacist %>% select(registration_number) %>% pull()
                is_international <- searched_pharmacist %>% select(international_grad) %>% pull()
                
                if (is_international == "Yes") {pharmacies_data_cleaned$international_grad_employees_employed[i] = pharmacies_data_cleaned$international_grad_employees_employed[i] + 1}
                pharmacies_data_cleaned$employees_employed[i] = pharmacies_data_cleaned$employees_employed[i] + 1
                
                pharmacies_data_cleaned$pharmacy_staff[i] = str_c(pharmacies_data_cleaned$pharmacy_staff[i], ", ", profname, " * ", reg_num, " - ", occupation)
                
                
            } else {
                
                
                searched_pharmacist <- pharmacists %>% filter(name == profname, occupation == role)
                if (nrow(searched_pharmacist) > 0) {
                    reg_num <- searched_pharmacist %>% select(registration_number) %>% pull()
                    is_international <- searched_pharmacist %>% select(international_grad) %>% pull()
                    
                    if (is_international == "Yes") {pharmacies_data_cleaned$international_grad_employees_employed[i] = pharmacies_data_cleaned$international_grad_employees_employed[i] + 1}
                    pharmacies_data_cleaned$employees_employed[i] = pharmacies_data_cleaned$employees_employed[i] + 1
                    
                    pharmacies_data_cleaned$pharmacy_staff[i] = str_c(pharmacies_data_cleaned$pharmacy_staff[i], ", ", profname, " * ", reg_num, " - ", occupation)
                
                } else {
                    
                errors = errors + 1
                pharmacies_data_cleaned$employees_employed[i] = pharmacies_data_cleaned$employees_employed[i] + 1
                pharmacies_data_cleaned$pharmacy_staff[i] = str_c(pharmacies_data_cleaned$pharmacy_staff[i], ", ", profname, " * ", "error", " - ", occupation)
                }
            }
           
            print(str_glue("Pharmacy {i} Employee {a} / {num_staff}"))
            
            pharmacies_data_cleaned$pharm_staff_txt[i] = ifelse(!is.na(pharmacies_data_cleaned$pharm_staff_txt[i]) | !pharmacies_data_cleaned$pharm_staff_txt[i] == "", str_after_nth(pharmacies_data_cleaned$pharm_staff_txt[i], "\\|", n = 2), pharmacies_data_cleaned$pharm_staff_txt[i])
        }
    }
}

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(international_grad_employees_employed_percentage = ifelse(status == "Entitled to operate", (international_grad_employees_employed / employees_employed), 0))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% select(-assess_his_txt, -concerns_txt, -pharm_staff_txt)

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(pharmacy_staffs_primary_keys_url = ifelse(str_starts(pharmacy_staffs_primary_keys_url, "\\,"), str_remove(pharmacy_staffs_primary_keys_url, "\\,"), pharmacy_staffs_primary_keys_url)) %>% 
    mutate(pharmacy_staff = ifelse(str_starts(pharmacy_staff, "\\, "), str_remove(pharmacy_staff, "\\, "), pharmacy_staff))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    select(name, status, type, accreditation_number, address, address_city_province, postal_code, phone_num, fax_num, date_issued, owner_or_corporation, pharmacy_staff, employees_employed, international_grad_employees_employed, international_grad_employees_employed_percentage, everything())

write_rds(pharmacies_data_cleaned, "pending.rds")

pharmacies_data_cleaned = import("pending.rds")


#==



# PAUL'S CLEANING TRANSLATED INTO R

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = "")

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse((str_detect(name, "Shoppers") | str_detect(name, "Shopper's") | str_detect(name, "Glebe Apothecary")),
                        "Shoppers Drug Mart", wpg)) %>% 
    mutate(wpg = ifelse((str_detect(name, "Rexall")),
                        "Rexall", wpg)) %>% 
    mutate(wpg = ifelse((str_detect(name, "Medicine Shop")),
                        "Medicine Shoppe", wpg)) %>% 
    mutate(wpg = ifelse((str_detect(name, "Jean Coutu")),
                        "Jean Coutu", wpg)) %>% 
    mutate(wpg = ifelse((str_detect(name, "Costco")),
                        "Costco", wpg)) %>%
    mutate(wpg = ifelse((str_detect(name, "Wal-Mart") | str_detect(name, "Walmart")),
                        "Wal-Mart", wpg)) %>%
    mutate(wpg = ifelse((str_detect(name, "Sobey") | str_detect(name, "Lawton")),
                        "Sobeys", wpg)) %>%
    mutate(wpg = ifelse((str_detect(name, "Loblaw")),
                        "Loblaw Pharmacy", wpg)) %>% 
    mutate(wpg = ifelse((str_detect(name, "Morelli")),
                        "Morelli's Pharmacy", wpg)) %>%
    mutate(wpg = ifelse((str_detect(name, "Food Basics")),
                        "Food Basics", wpg)) %>%
    mutate(wpg = ifelse((str_detect(name, "FreshCo") | str_detect(name, "Metro:")),
                        "FreshCo", wpg)) %>%
    mutate(wpg = ifelse((str_detect(name, "Metro Pharmacy") | str_detect(name, "Freshco")),
                        "Metro Pharmacy", wpg)) %>%
    mutate(wpg = ifelse((str_detect(name, "Safeway")),
                        "Safeway", wpg)) %>%
    mutate(wpg = ifelse((str_detect(name, "Drugstore Pharmacy") | str_detect(name, "No Frills") | str_detect(name, "DrugStore Pharmacy")),
                        "Drugstore Pharmacy", wpg)) %>%
    mutate(wpg = ifelse((str_detect(name, "Pharmasave") | str_detect(name, "PharmaSave") | str_detect(name, "PHARMASAVE") | str_detect(name, "Phamasave Michael's Pharmacy") | str_detect(name, "Phamasave")),
                        "Pharmasave", wpg)) %>% 
    mutate(wpg = ifelse((str_detect(name, "Marshall Park")),
                        "Pharmasave", wpg)) %>% 
    mutate(wpg = ifelse((str_detect(name, "IDA") | str_detect(name, "I.D.A")  | str_detect(name, "North End Farmacia")  | str_detect(name, "Guardian") ),
                        "Guardian/IDA", wpg)) %>% 
    mutate(wpg = ifelse((str_detect(name, "Remedy")),
                        "Remedy's Rx", wpg)) %>% 
    mutate(wpg = ifelse((str_detect(name, "Whole Health")),
                        "Whole Health", wpg)) %>%
    mutate(wpg = ifelse((str_detect(name, "Total Health Pharmacy")),
                        "Total Health Pharmacy", wpg)) %>%
    mutate(wpg = ifelse((str_detect(name, "Pharma Plus") | str_detect(name, "Pharmaplus")),
                        "Pharma Plus", wpg)) %>%
    mutate(wpg = ifelse((str_detect(name, "Pharmachoice") | str_detect(name, "PHARMACHOICE") | str_detect(name, "PharmaChoice")),
                        "Pharma Plus", wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(type == "Hospital pharmacy",
                        yes = "Hospital", no = wpg)) %>% 
    mutate(wpg = ifelse(str_detect(name, "Hospital") | str_detect(name, "Hopital") | str_detect(name, "Cancer") | str_detect(name, "CAMH"),
                        yes = "Hospital", no = wpg)) %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Outpatient") | 
            str_detect(name, "University Health Network")
    )),
    yes = "Hospital Outpatient", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "HealthPro") | 
            str_detect(name, "Medbuy") | 
            str_detect(name, "Health Pro")
    )),
    yes = "Hospital Drug Procurement", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Pharmacy") | 
            str_detect(name, "Drug") | 
            str_detect(name, "Store") | 
            str_detect(name, "Chemist") |
            str_detect(name, "Apothecary") | 
            str_detect(name, "Pharmacie") | 
            str_detect(name, "Rx") |
            str_detect(name, "PHARMACY") | 
            str_detect(name, "Cabinet") |  
            str_detect(name, "Mart") |
            str_detect(name, "Shop") | 
            str_detect(name, "shop") | 
            str_detect(name, "Pharmacentre") | 
            str_detect(name, "Saveon") |
            str_detect(name, "Rx") | 
            str_detect(name, "Pharma ") |
            str_detect(name, "Mister Pharmacist") | 
            str_detect(name, "shoppe") | 
            str_detect(name, "Prescriptions") |
            str_detect(name, "Pharmacia") | 
            str_detect(name, "London Medical Pharmacy") |  
            str_detect(name, "pharmacy") |
            str_detect(name, "Disp ") | 
            str_detect(name, "Dispensary") | 
            str_detect(name, "House of Medicine") |
            str_detect(name, "RAZI") | 
            str_detect(name, "Wellness Shield") |  
            str_detect(name, "Medical Place Health Solutions")
    )) & wpg == "",
    yes = "Independent", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Compound") | 
            str_detect(name, "Mortar + Pestle") | 
            str_detect(name, "compound") | 
            str_detect(name, "Hybrid Pharm") |
            str_detect(name, "Quiick Medicine") | 
            str_detect(name, "Restore Pharmaceuticals")
    )),
    yes = "Compounding", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Health Team") | 
            str_detect(name, "FHT") | 
            str_detect(name, "F.H.T.") | 
            str_detect(name, "CCAC") |
            str_detect(name, "FHG") | 
            str_detect(name, "MMT Downtown") | 
            str_detect(name, "Public Health") |
            str_detect(name, "Region") | 
            str_detect(name, "Health Service") |  
            str_detect(name, "Health System") |
            str_detect(name, "Authority") | 
            str_detect(name, "Rehab") | 
            str_detect(name, "Sante") | 
            str_detect(name, "Physician") |
            str_detect(name, "Family Practice") | 
            str_detect(name, "Cardio") |
            str_detect(name, "Infusion") | 
            str_detect(name, "Hospice") | 
            str_detect(name, "Ornge") |
            str_detect(name, "Equipe") | 
            str_detect(name, "Professional Corporation") |  
            str_detect(name, "Pain") |
            str_detect(name, "Epilepsy") | 
            str_detect(name, "Wellmedica") | 
            str_to_upper(name) %>% str_detect("LTC")
    )),
    yes = "Health Team", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Health Network") | 
            str_detect(name, "Medical Center") | 
            str_detect(name, "Healthcare") | 
            str_detect(name, "Hotel Dieu") |
            str_detect(name, "Integration") | 
            str_detect(name, "Local") | 
            str_detect(name, "Health Science") |
            str_detect(name, "Community") | 
            str_detect(name, "community") |  
            str_detect(name, "Care") |
            str_detect(name, "Dr ") | 
            str_detect(name, "Dr. ") | 
            str_detect(name, "Clinic") | 
            str_detect(name, "Medical Centre") |
            str_detect(name, "Alliance") | 
            str_detect(name, "Urgent") |
            str_detect(name, "Geriatric") | 
            str_detect(name, "Centre") | 
            str_detect(name, "Kadri Medical") |
            str_detect(name, "Richmond Hill Urology") | 
            str_detect(name, "Lanark Lifestyles") |  
            str_detect(name, "Harvest Medicine") |
            str_detect(name, "D & V Pharmcare") | 
            str_detect(name, "Retirement") | 
            str_detect(name, "Simplicity Wellness") |
            str_detect(name, " site") | 
            str_detect(name, "Site")
    )) & wpg == "",
    yes = "Health Team", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Management") | 
            str_detect(name, "Research") | 
            str_detect(name, "Cubic Health") | 
            str_detect(name, "RSJ Health") |
            str_detect(name, "Deloitte") | 
            str_detect(name, "Editorial") | 
            str_detect(name, "Smart & Biggar") |
            str_detect(name, "BD Canada") | 
            str_detect(name, "COMPASS") |  
            str_detect(name, "Consult") |
            str_detect(name, "Bend In The Road") | 
            str_detect(name, "Agro Health") | 
            str_detect(name, "Med-I-Well Services") | 
            str_detect(name, "PointClickCare") |
            str_detect(name, "Medtronic") | 
            str_detect(name, "Dextra Group Inc") |
            str_detect(name, "Medication Use Mgnt Services") | 
            str_detect(name, "Ontario Ltd") | 
            str_detect(name, "Meds") |
            str_detect(name, "Mums Inc") | 
            str_detect(name, "A&E ITP Services") |  
            str_detect(name, "Mercer") |
            str_detect(name, "Klick Health Pharma") | 
            str_detect(name, "Medication Use") | 
            str_detect(name, "Contract") |
            str_detect(name, "GeriMedRisk") | 
            str_detect(name, "GeneYouIn") |
            str_detect(name, "Strateg") | 
            str_detect(name, "Canadian Cancer Trials Group") |
            str_detect(name, "Extendicare") | 
            str_detect(name, "Communications") |  
            str_detect(name, "Parallon")
    )),
    yes = "Consulting", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Solutions") | 
            str_detect(name, "Inc")
    )) & wpg == "",
    yes = "Consulting", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Janssen") | 
            str_detect(name, "Hoffmann") | 
            str_detect(name, "Baxter") | 
            str_detect(name, "Vertex") |
            str_detect(name, "Amgen") | 
            str_detect(name, "Pfizer") | 
            str_detect(name, "Astrazeneca") |
            str_detect(name, "Sanofi") | 
            str_detect(name, "Lilly") |  
            str_detect(name, "Bayer") |
            str_detect(name, "Takeda") | 
            str_detect(name, "Celgene") | 
            str_detect(name, "Nordisk") | 
            str_detect(name, "Astellas") |
            str_detect(name, "LEO Pharma") | 
            str_detect(name, "Apotex") |
            str_detect(name, "Ipsen Biopharmaceuticals") | 
            str_detect(name, "Boehringer") | 
            str_detect(name, "Johnson & Johnson") |
            str_detect(name, "Steripharm Laboratories") | 
            str_detect(name, "Allergan") |  
            str_detect(name, "Aventis") |
            str_detect(name, "Merck") | 
            str_detect(name, "ApoPharma") | 
            str_detect(name, "Genzyme") |
            str_detect(name, "Teva") | 
            str_detect(name, "Aventis") |
            str_detect(name, "Nycomed") | 
            str_detect(name, "Sterimax") |
            str_detect(name, "Biogen Canada") | 
            str_detect(name, "UCB Canada") |  
            str_detect(name, "Patheon") |
            str_detect(name, "CSL Behring") | 
            str_detect(name, "Servier") | 
            str_detect(name, "ProPharma Group") |
            str_detect(name, "Purdue") | 
            str_detect(name, "BioAdvance") |
            str_detect(name, "Bioadvance") | 
            str_detect(name, "Fresenius Kabi") | 
            str_detect(name, "Glaxo") |
            str_detect(name, "Galderma") | 
            str_detect(name, "Eisai Limited") |  
            str_detect(name, "Elsai Limited") |
            str_detect(name, "AbbVie") | 
            str_detect(name, "Abbott Laboratories") | 
            str_detect(name, "Alexion") |
            str_detect(name, "Bristol Myers Squibb") | 
            str_detect(name, "Genentech") |
            str_detect(name, "Gilead") | 
            str_detect(name, "Ferring") |
            str_detect(name, "Pendopharm") | 
            str_detect(name, "LEO") |  
            str_detect(name, "Bio Pharma Services") |
            str_detect(name, "Reve Pharma")
    )),
    yes = "Drug Company", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Forces") | 
            str_detect(name, "CFB") | 
            str_detect(name, "Central Medical Equipment Depot") | 
            str_detect(name, "Defense") |
            str_detect(name, "Petawawa") | 
            str_detect(name, "Field Hospital") | 
            str_detect(name, "CF Base") |
            str_detect(name, "Defence") | 
            str_detect(name, "Base Hospital")
    )),
    yes = "Military", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Association of Faculties of Pharmacy") | 
            str_detect(name, "University of Toronto") | 
            str_detect(name, "University of Waterloo") | 
            str_detect(name, "School") |
            str_detect(name, "Career") | 
            str_detect(name, "Discovery Pharmacy") | 
            str_detect(name, "Collegiale")
    )),
    yes = "Academic", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "College") | 
            str_detect(name, "Academy") | 
            str_detect(name, "University") | 
            str_detect(name, "Institute")
    )) & wpg == "",
    yes = "Academic", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Ontario Pharmacists Assoc") | 
            str_detect(name, "Canadian Pharmacists Association") | 
            str_detect(name, "Drug Info And Resource Centre") | 
            str_detect(name, "National Academy of Health & Business") |
            str_detect(name, "Drug Info And Resource Centre")
    )),
    yes = "Professional Association", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Ontario College Of Pharmacists") | 
            str_detect(name, "Pharmacy Exam Board Of Canada") | 
            str_detect(name, "Pharmaceutical Advertising Advisory Board") | 
            str_detect(name, "Pharm Advert Adv Board") |
            str_detect(name, "NAPRA") |
            str_detect(name, "College of ") | 
            str_detect(name, "PharmAchieve") | 
            str_detect(name, "Institute for Safe Medication")
    )),
    yes = "Regulatory", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Ontario") | 
            str_detect(name, "Canada")
    )) & wpg == "",
    yes = "Government", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "CADTH") | 
            str_detect(name, "Blood") | 
            str_detect(name, "Government") | 
            str_detect(name, "MOH") |
            str_detect(name, "Board ") | 
            str_detect(name, "Ministry") | 
            str_detect(name, "Municipality") |
            str_detect(name, "Health Canada") | 
            str_detect(name, "Insurance") |  
            str_detect(name, "Canada Health Infoway") |
            str_detect(name, "CADTH") | 
            str_detect(name, "Ontario Agency") | 
            str_detect(name, "First Nations") | 
            str_detect(name, "Public Health Agency") |
            str_detect(name, "Information Technology Services") | 
            str_detect(name, "Pan-Canadian Pharmaceutical Alliance") |
            str_detect(name, "Ontario Health") | 
            str_detect(name, "Canadian Institute for Health Info") | 
            str_detect(name, "Citizenship") |
            str_detect(name, "CMHA")
    )),
    yes = "Government", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Veterinary") | 
            str_detect(name, "Donald A Organ")
    )),
    yes = "Veterinary", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Omnicell") | 
            str_detect(name, "GMD Pharma Solutions") | 
            str_detect(name, "Drug Trading Co") | 
            str_detect(name, "Pack4U") |
            str_detect(name, "Pharmaceutical Services") | 
            str_detect(name, "Pharmacy Practice Magazine") | 
            str_detect(name, "Pharma-Insight") |
            str_detect(name, "MedMe Health") | 
            str_detect(name, "McCann Pharmacy Initiative") |  
            str_detect(name, "McCANN Health") |
            str_detect(name, "MedEssist") | 
            str_detect(name, "BCE Pharma") | 
            str_detect(name, "Healthmark Services") | 
            str_detect(name, "SRX Enterprises") |
            str_detect(name, "Westwing Pharma") | 
            str_detect(name, "Pharma Support") |
            str_detect(name, "OnPharm-United")
    )),
    yes = "Services for Pharmacy", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Relief") | 
            str_detect(name, "locum") | 
            str_detect(name, "McCANN Health") | 
            str_detect(name, "MedEssist") |
            str_detect(name, "RPI Consulting Group") | 
            str_detect(name, "TAL Group") | 
            str_detect(name, "QRP") |
            str_detect(name, "Milo Pharmaco Inc") | 
            str_detect(name, "Tal Group") |  
            str_detect(name, "Youanna") |
            str_detect(name, "Lauber Group Canada") | 
            str_detect(name, "Personnel")
    )),
    yes = "Temp Agency", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Telepharm") | 
            str_detect(name, "Pocket") | 
            str_detect(name, "Pharmex Direct") | 
            str_detect(name, "Simpill") |
            str_detect(name, "Pharmacy.Ca") | 
            str_detect(name, "Telepharm") | 
            str_detect(name, "MEDMAIL") |
            str_detect(name, "PharmAssist") | 
            str_detect(name, "Well.ca") |  
            str_detect(name, "The Health Depot") |
            str_detect(name, "Pillway") | 
            str_detect(name, "PointClickCare") | 
            str_detect(name, "PharmAssist") | 
            str_detect(name, "Well.ca") |
            str_detect(name, "Express Scripts Canada Pharmacy") | 
            str_detect(name, "Telepharm") |
            str_detect(name, "MEDMAIL") |
            str_detect(name, "Pharmacy.Ca") | 
            str_detect(name, "Telepharm") | 
            str_detect(name, "MEDMAIL") |
            str_detect(name, "PharmAssist") | 
            str_detect(name, "Postscriptions") |  
            str_detect(name, "Xpresspill") |
            str_detect(name, "Silver Scripts") | 
            str_detect(name, "North West") | 
            str_detect(name, "Felix") | 
            str_detect(name, "Virtual")
    )),
    yes = "Telepharmacy", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Correction") | 
            str_detect(name, "Detention")
    )),
    yes = "Corrections", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Rite Aid") | 
            str_detect(name, "Proxim") | 
            str_detect(name, "Walgreen") | 
            str_detect(name, "prix") |
            str_detect(name, "Kaiser") | 
            str_detect(name, "USA") | 
            str_detect(name, "Familiprix") |
            str_detect(name, "Kroger") | 
            str_detect(name, "Blue Shield") |  
            str_detect(name, "International") |
            str_detect(name, "Cvs") | 
            str_detect(name, "Target") | 
            str_detect(name, "CVS") | 
            str_detect(name, "NYGH")
    )),
    yes = "Outside Ontario", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "New Day") | 
            str_detect(name, "Specialty") | 
            str_detect(name, "CATP") | 
            str_detect(name, "Specialties") |
            str_detect(name, "Shoppers Drug Mart 6900 Shared Services Pharmacy") | 
            str_detect(name, "Calea") | 
            str_detect(name, "BioScript Pharmacy") |
            str_detect(name, "Infusion") | 
            str_detect(name, "Advanced Care Specialty") |  
            str_detect(name, "Fresenius Kabi Compounding Solutions") |
            str_detect(name, "Specialty") | 
            str_detect(name, "New Day") | 
            str_detect(name, "Infusion") | 
            str_detect(name, "Bio Pharma Services") |
            str_detect(name, "Aved") | 
            str_detect(name, "Charlton Health Inc") |
            str_detect(name, "NKS") | 
            str_detect(name, "N.K.S") | 
            str_detect(name, "Biorex") |
            str_detect(name, "The PrEP Clinic") | 
            str_detect(name, "The Hamilton Clinic") |  
            str_detect(name, "Fertility")
    )),
    yes = "Specialty Pharmacy", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Cancer")
    )) & str_detect(name, "Pharmacy"),
    yes = "Specialty Pharmacy", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Medisystem") | 
            str_detect(name, "Classic Care") | 
            str_detect(name, "Medical Pharmacies Group") | 
            str_detect(name, "Medical Pharmacy:2 - 2735 Matheson Blvd") |
            str_detect(name, "Medical Pharmacy:30 Fulton Way") | 
            str_detect(name, "Central Fill") | 
            str_detect(name, "Hogan Pharmacy Partners") |
            str_detect(name, "Medical Pharmacy:100 - 2447 Kaladar Ave") | 
            str_detect(name, "Smartmeds Pharmacy:1252 Northside") |  
            str_detect(name, "Medical Pharmacy:1 - 1351 G Kelly Lake") |
            str_detect(name, "Medical Pharmacy:1100 Algoma Rd") | 
            str_detect(name, "GeriatRx") | 
            str_detect(name, "Yurek Specialties") | 
            str_detect(name, "LTC") |
            str_detect(name, "Pack4U") | 
            str_detect(name, "Synpharm") |
            str_detect(name, "Silver Fox Pharmacy") | 
            str_detect(name, "Steripharm Laboratories") | 
            str_detect(name, "Baxter Pharmacy Services") |
            str_detect(name, "CareRx")
    )),
    yes = "Central Fill", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Sun Life") | 
            str_detect(name, "Sunlife") | 
            str_detect(name, "Desjardins") | 
            str_detect(name, "Medavie Blue Cross") |
            str_detect(name, "Green Shield") | 
            str_detect(name, "Manulife") | 
            str_detect(name, "ClaimSecure") |
            str_detect(name, "Telus") | 
            str_detect(name, "Express Scripts")
    )) & wpg == "",
    yes = "Private drug plan", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Health") | 
            str_detect(name, " Medical")
    )) & wpg == "",
    yes = "Health Team", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        str_detect(name, "Pharmacare")
    )) & wpg == "",
    yes = "Independent", no = wpg))

# pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
#     mutate(wpg = ifelse(((
#         str_detect(name, "no workplaces")
#     )),
#     yes = "No workplaces", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg = ifelse(((
        wpg == ""
    )),
    yes = "Other", no = wpg))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = "")

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = ifelse(((
        str_detect(wpg, "Independent") | 
            str_detect(wpg, "Hospital Outpatient")
    )),
    yes = "Independent CP", no = wpg2))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = ifelse(((
        str_detect(wpg, "Shoppers") | 
            str_detect(wpg, "Rexall") |
            str_detect(wpg, "Pharma Plus") | 
            str_detect(wpg, "Jean Coutu") |
            str_detect(wpg, "Medicine Shoppe")
    )),
    yes = "Corporate/Franchise CP", no = wpg2))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = ifelse(((
        str_detect(wpg, "Pharmasave") | 
            str_detect(wpg, "Guardian") |
            str_detect(wpg, "Whole Health") | 
            str_detect(wpg, "Remedy") |
            str_detect(wpg, "Total Health") |
            str_detect(wpg, "Drugstore Pharmacy") |
            str_detect(wpg, "Pharmachoice")
    )),
    yes = "Banner CP", no = wpg2))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = ifelse(((
        str_detect(wpg, "Costco") | 
            str_detect(wpg, "Wal-Mart")
    )),
    yes = "Mass merchandiser CP", no = wpg2))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = ifelse(((
        str_detect(wpg, "Food Basics") | 
            str_detect(wpg, "Morelli") |
            str_detect(wpg, "Sobeys") | 
            str_detect(wpg, "Metro Pharmacy") |
            str_detect(wpg, "FreshCo") |
            str_detect(wpg, "Loblaw Pharmacy") |
            str_detect(wpg, "Safeway")
        
    )),
    yes = "Grocery CP", no = wpg2))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = ifelse(((
        str_detect(wpg, "Compounding") | 
            str_detect(wpg, "Specialty") |
            str_detect(wpg, "Central Fill") | 
            str_detect(wpg, "Veterinary") |
            str_detect(wpg, "Telepharmacy")
    )),
    yes = "Specialty pharmacy", no = wpg2))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = ifelse(((
        str_detect(wpg, "Hospital")
    )),
    yes = "Hospital", no = wpg2))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = ifelse(((
        str_detect(wpg, "Health Team")
    )),
    yes = "Health Team", no = wpg2))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = ifelse(((
        str_detect(wpg, "Other")
    )),
    yes = "Other", no = wpg2))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = ifelse(((
        str_detect(wpg, "Corrections") |
            str_detect(wpg, "Military")
    )),
    yes = "Military/Corrections", no = wpg2))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = ifelse(((
        str_detect(wpg, "Outside Ontario")
    )),
    yes = "Outside Ontario", no = wpg2))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = ifelse(((
        str_detect(wpg, "Academic") | 
            str_detect(wpg, "Government") |
            str_detect(wpg, "Regulatory") | 
            str_detect(wpg, "Professional Association") |
            str_detect(wpg, "Hospital Drug Procurement")
    )),
    yes = "Non-clinical Public Sector", no = wpg2))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg2 = ifelse(((
        str_detect(wpg, "Drug Company") | 
            str_detect(wpg, "Private drug plan") |
            str_detect(wpg, "Services for Pharmacy") | 
            str_detect(wpg, "Temp Agency") | 
            str_detect(wpg, "Consulting")
    )),
    yes = "Non-clinical Private Sector", no = wpg2))

# pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
#     mutate(wpg2 = ifelse(((
#         str_detect(wpg, "No workplaces")
#     )),
#     yes = "No workplaces", no = wpg2))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg3 = "")

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg3 = ifelse(str_detect(wpg2, "Sector"), "Non-clinical", wpg3))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg3 = ifelse(((
        str_detect(wpg2, "Outside Ontario") |
            str_detect(wpg2, "Other")
    )),
    yes = "Other", no = wpg3))

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(wpg3 = ifelse(((
        str_detect(wpg2, "Independent CP") | 
            str_detect(wpg2, "Corporate/Franchise CP") |
            str_detect(wpg2, "Banner CP") | 
            str_detect(wpg2, "Grocery CP") |
            str_detect(wpg2, "Health Team") |
            str_detect(wpg2, "Specialty pharmacy") |
            str_detect(wpg2, "Hospital") |
            str_detect(wpg2, "Mass merchandiser CP") |
            str_detect(wpg2, "Military/Corrections")
    )),
    yes = "Clinical", no = wpg3))

# pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
#     mutate(wpg3 = ifelse(str_detect(wpg2, "No workplaces"), "No workplaces", wpg3))


pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    mutate(accreditation_number = str_trim(accreditation_number, side = "both")) %>% 
    mutate(type = str_trim(type, side = "both")) 

for (i in 1:nrow(pharmacies_data_cleaned)) {
    if (pharmacies_data_cleaned$type[i] == "Non-accredited Workplace") {
        pharmacies_data_cleaned$accreditation_number[i] = i ^ 2
    } else {}
}

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    distinct(accreditation_number, .keep_all = TRUE)

pharmacies_data_cleaned = pharmacies_data_cleaned %>% 
    select(name, wpg, wpg2, wpg3, status, type, accreditation_number, address, address_city_province, postal_code, phone_num, fax_num, date_issued, owner_or_corporation, pharmacy_staff, employees_employed, international_grad_employees_employed, international_grad_employees_employed_percentage, everything())



write_rds(pharmacies_data_cleaned, "OCP_Pharmacies_Completed_2022_V12.rds")

