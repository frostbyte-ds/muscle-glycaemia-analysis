# ------------------------------------------- #
# Saving assets required to render the report
# ------------------------------------------- #

final_objects_to_save <- c(
  "descriptive.tbl.kbl",
  "onset.summary.table.kbl",
  "prog.summary.table.kbl",
  "almi_forest_plot",
  "gluc.dens",
  "musc.dens",
  "gluc.musc.scat",
  "pred.plot",
  "cv_summary_tbl_kbl",
  "roc_plot"
)

# Save these specific objects to a file named "report_assets.RData"
save(list = final_objects_to_save, file = "report_assets.RData")