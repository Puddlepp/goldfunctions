# Serum Gamma Glutamyl Transferase (GGT)
#'
#' This function extracts gamma glutamyl transferase records from CPRD GOLD.
#' It assumes that your cohort contains a column named patid.
#'
#' Codes:
#'
#' \tabular{lllll}{
#' Medcode \tab  \tab Readcode \tab  \tab Description\cr
#' 5214 \tab  \tab 44G4.00 \tab  \tab Gamma - G.T. level\cr
#' 5891 \tab  \tab 44G4100 \tab  \tab Gamma glutamyl transferase level abnormal\cr
#' 13752 \tab  \tab 44G4000 \tab  \tab Gamma glutamyl transferase level normal\cr
#' 13753 \tab  \tab 44G9.00 \tab  \tab Serum gamma-glutamyl transferase level\cr
#' 13754 \tab  \tab 44G7.00 \tab  \tab Plasma gamma-glutamyl transferase level\cr
#' }
#'
#' @param cohort A dataframe containing at least one column called patid
#' @return A dataframe containing all gamma glutamyl transferase records
#' @export
ggt <- function(cohort) {

  # Draw out the cohort patids.

  cohort_patid <-
    cohort %>%
    pull(patid)

  # Retrieve medical table entries for the relevant readcodes.

  gamma_gold <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("medical"))) %>%
    filter(readcode %in% c("44G4.00",
                           "44G4000",
                           "44G4100",
                           "44G7.00",
                           "44G9.00")) %>%
    collect()

  # Draw out the medcodes.

  gamma_gold_medcode <-
    gamma_gold %>%
    pull(medcode)

  # Identify relevant enttypes and units of measurement (on separate script).

  # Retrieve all test table entries for the cohort.

  gamma_cohort <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("test"))) %>%
    filter(patid %in% cohort_patid,
           medcode %in% gamma_gold_medcode,
           enttype %in% c("172", "288", "480"),
           data3 %in% c("0", "61", "127", "138", "142", "164", "277")) %>%
    collect()

  # Retrieve entity table records for use below.

  gamma_entity <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("entity"))) %>%
    filter(enttype %in% c("172", "288", "480")) %>%
    collect()

  # Retrieve lookup_xxx table entries and prepare them for lookups below.

  lookup <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("lookup_xxx"))) %>%
    collect() %>%
    mutate(together = paste0(reference, code))

  # Complete all lookups for our cohort table.

  gamma_cohort_1 <-
    gamma_cohort %>%
    mutate(data1_lkup = VLOOKUP(gamma_cohort$enttype,
                                gamma_entity,
                                enttype,
                                data1_lkup),
           data2_lkup = VLOOKUP(gamma_cohort$enttype,
                                gamma_entity,
                                enttype,
                                data2_lkup),
           data3_lkup = VLOOKUP(gamma_cohort$enttype,
                                gamma_entity,
                                enttype,
                                data3_lkup),
           data4_lkup = VLOOKUP(gamma_cohort$enttype,
                                gamma_entity,
                                enttype,
                                data4_lkup),
           data5_lkup = VLOOKUP(gamma_cohort$enttype,
                                gamma_entity,
                                enttype,
                                data5_lkup),
           data6_lkup = VLOOKUP(gamma_cohort$enttype,
                                gamma_entity,
                                enttype,
                                data6_lkup),
           data7_lkup = VLOOKUP(gamma_cohort$enttype,
                                gamma_entity,
                                enttype,
                                data7_lkup),
           operator_data1 = VLOOKUP(paste0(data1_lkup, data1),
                                    lookup,
                                    together,
                                    text),
           value_data2 = "Value",
           unit_of_measure_data3 = VLOOKUP(paste0(data3_lkup, data3),
                                           lookup,
                                           together,
                                           text),
           qualifier_data4 = VLOOKUP(paste0(data4_lkup, data4),
                                     lookup,
                                     together,
                                     text),
           normal_range_from_data5 = "Normal range from",
           normal_range_to_data6 =  "Normal range to",
           normal_range_basis_data7 = VLOOKUP(paste0(data7_lkup, data7),
                                              lookup,
                                              together,
                                              text),
           consultation_type = VLOOKUP(paste0("SED", constype),
                                       lookup,
                                       together,
                                       text),
           medical_type = VLOOKUP(medcode,
                                  gamma_gold,
                                  medcode,
                                  desc),
           entity_type = VLOOKUP(enttype,
                                 gamma_entity,
                                 enttype,
                                 description)) %>%
    select(patid, eventdate, sysdate, constype, consultation_type, consid, staffid,
           medcode, medical_type, enttype, entity_type, data1, operator_data1, data2, value_data2,
           data3, unit_of_measure_data3, data4, qualifier_data4, data5, normal_range_from_data5,
           data6, normal_range_to_data6)

  return(gamma_cohort_1)

}
