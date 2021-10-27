#' Blood Pressure
#'
#' This function extracts systolic and diastolic test results from CPRD GOLD.
#' It assumes that your cohort contains a column named patid.
#'
#' @param cohort A dataframe containing at least one column called patid
#' @return A dataframe containing all systolic and diastolic blood pressure records
#' @export
bp <- function(cohort) {

  # Draw out the cohort patids.

  cohort_patid <-
    cohort %>%
    pull(patid)

  # Retrieve medical table entries for the relevant readcodes.

  bp_gold <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("medical"))) %>%
    filter(readcode %in% c( "246..00", "246..11", "246..12", "2461.00", "662L.00",
                            "246V.00", "G20..11", "R1y2.00", "68B1.00", "2464.00",
                            "9OD..11", "2465.00", "2466.00", "2462.00", "ZV70B00",
                            "R1y3.00", "662Q.00", "G2...11", "R1y4.00", "315B.00",
                            "2468.00", "246C.00", "246D.00", "246E.00", "2467.00",
                            "6623.00", "9OD7.00", "9OD2.00", "9OD..12", "246Q.00",
                            "246J.00", "662V.00", "662j.00", "246A.00", "2469.00",
                            "246G.00", "246B.00", "246Z.00", "246F.00", "2463.00",
                            "662C.00", "246d.00", "662B.00", "246c.00", "246K.00",
                            "9OD5.00", "9OD6.00", "9OD1.00", "9OD3.00", "246Y.00",
                            "246N.00", "246P.00", "2460.00", "246W.00", "246b.00",
                            "246e.00", "246X.00", "246R.00", "246S.00", "246a.00",
                            "9OD9.00", "9ODA.00", "246f.00", "246L.00", "9ODB.00",
                            "246T.00", "246g.00")) %>%
    collect()

  # Draw out the medcodes.

  bp_gold_medcode <-
    bp_gold %>%
    pull(medcode)

  # Identify relevant enttypes and units of measurement (on separate script).

  # Retrieve all clinical table entries for the cohort.

  bp_cohort <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("clinical"))) %>%
    filter(patid %in% cohort_patid,
           medcode %in% bp_gold_medcode,
           enttype %in% c("1")) %>%
    collect()

  # Draw out the adids.

  bp_cohort_adid <-
    bp_cohort %>%
    pull(adid)

  # Retrieve all additional table entries for the cohort.

  bp_additional <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("additional"))) %>%
    filter(adid %in% bp_cohort_adid) %>%
    collect()

  # Combine the clinical and additional tables

  bp_cohort_1 <-
    full_join(x = bp_cohort,
              y = bp_additional,
              by = c("patid",
                     "eventdate",
                     "sysdate",
                     "enttype",
                     "adid"))

  # Retrieve entity table records for use below.

  bp_entity <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("entity"))) %>%
    filter(enttype %in% c("1")) %>%
    collect()

  # Retrieve lookup_xxx table entries and prepare them for lookups below.

  lookup <-
    tbl(con,
        in_schema(schema = sql("cprd0121.dbo"),
                  table = sql("lookup_xxx"))) %>%
    collect() %>%
    mutate(together = paste0(reference, code))

  # Complete all lookups for our cohort table.

  bp_cohort_2 <-
    bp_cohort_1 %>%
    mutate(data5_lkup = VLOOKUP(bp_cohort_1$enttype,
                                bp_entity,
                                enttype,
                                data5_lkup),
           data6_lkup = VLOOKUP(bp_cohort_1$enttype,
                                bp_entity,
                                enttype,
                                data6_lkup),
           data7_lkup = VLOOKUP(bp_cohort_1$enttype,
                                bp_entity,
                                enttype,
                                data7_lkup),
           diastolic_data1 = "Diastolic",
           systolic_data2 = "Systolic",
           korotkoff_data3 = "Korotkoff",
           event_time_data4 = "Event time",
           laterality_data5 = VLOOKUP(paste0(data5_lkup, data5),
                                      lookup,
                                      together,
                                      text),
           posture_data6 = VLOOKUP(paste0(data6_lkup, data6),
                                   lookup,
                                   together,
                                   text),
           cuff_data7 = VLOOKUP(paste0(data7_lkup, data7),
                                lookup,
                                together,
                                text),
           consultation_type = VLOOKUP(paste0("SED", constype),
                                       lookup,
                                       together,
                                       text),
           medical_type = VLOOKUP(medcode,
                                  bp_gold,
                                  medcode,
                                  desc),
           entity_type = VLOOKUP(enttype,
                                 bp_entity,
                                 enttype,
                                 description)) %>%
    select(patid, eventdate, sysdate, constype, consultation_type, consid, staffid,
           medcode, medical_type, enttype, entity_type, data1, diastolic_data1,
           data2, systolic_data2, data3, korotkoff_data3, data4, event_time_data4,
           data5, laterality_data5, data6, posture_data6, data7, cuff_data7)

  return(bp_cohort_2)

}
