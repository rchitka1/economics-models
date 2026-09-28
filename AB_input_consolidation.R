# BUILD INPUTS — Bundle Excel sheets + kr_eligible into one RDS ----
#
#   Project : Equity in Childhood Vaccination Under Ayushman Bharat
#   Author  : Rohan Chitkara
#
# PURPOSE:
#   Compile every input that AB_Equity_ECEA_Wealth_Final.R consumes into
#   a single .rds bundle. The bundle is a named list whose elements are:
#     - every sheet of AB_Equity_Inputs_Final_v2.xlsx as a tibble, with
#       skip applied for sheets whose headers do not sit on row 1
#     - kr_eligible, the 12-23 month child microdata produced by
#       DHS_NFHS_Analysis.R
#
#   Run this script whenever the Excel master input or kr_eligible is
#   updated. The master ECEA script reads only the resulting bundle.
#

if (TRUE) {


  ## LIBRARIES ----
  
  library(openxlsx)
  library(readxl)


  ## INPUT AND OUTPUT PATHS ----
  
  excel_path <- "AB_Equity_Inputs_Final_v2.xlsx"
  kr_path    <- "DHS_files/kr_eligible.rds"
  out_path   <- "AB_equity_inputs.rds"


  ## NON-STANDARD HEADER SHEETS ----
  # Most sheets carry the header on row 1. Four IMI / metadata sheets have
  # title and parameter rows above the header.
  
  custom_skips <- list(
    R_imi_exposure       = 4,
    R_imi_costs          = 4,
    R_metadata           = 4,
    R_imi_phaseweighted  = 5
  )


  ## READ EVERY SHEET ----
  
  if (!file.exists(excel_path))
    stop("Excel master not found at ", excel_path)
  
  cat("Reading ", excel_path, "...\n", sep = "")
  sheets <- getSheetNames(excel_path)
  
  inputs <- lapply(sheets, function(s) {
    skip_n <- if (s %in% names(custom_skips)) custom_skips[[s]] else 0
    read_excel(excel_path, sheet = s, skip = skip_n)
  })
  names(inputs) <- sheets
  
  cat("  Loaded ", length(inputs), " sheets:\n", sep = "")
  for (s in sheets) {
    cat(sprintf("    %-30s %5d rows x %3d cols\n",
                s, nrow(inputs[[s]]), ncol(inputs[[s]])))
  }


  ## ATTACH ELIGIBLE-CHILDREN MICRODATA ----
  
  if (!file.exists(kr_path))
    stop("kr_eligible.rds not found at ", kr_path,
         " — run DHS_NFHS_Analysis.R first.")
  
  inputs$kr_eligible <- readRDS(kr_path)
  cat(sprintf("    %-30s %5d rows x %3d cols\n",
              "kr_eligible (microdata)",
              nrow(inputs$kr_eligible), ncol(inputs$kr_eligible)))


  ## SAVE BUNDLE ----
  
  saveRDS(inputs, out_path)
  
  bundle_size_mb <- round(file.info(out_path)$size / 1024^2, 1)
  cat("\nWrote ", out_path, " (", bundle_size_mb, " MB, ",
      length(inputs), " elements).\n", sep = "")
  cat("Master ECEA can now read from this single file.\n")

}
