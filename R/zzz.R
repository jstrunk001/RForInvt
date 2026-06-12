.datatable.aware <- TRUE

# Declare symbols used in data.table / sf / dplyr non-standard evaluation so
# R CMD check ("checking R code for possible problems") does not flag them as
# undefined globals. These are NSE column names and data.table operators, not
# real bindings.
utils::globalVariables(c(
  # data.table NSE operators / special symbols
  ":=", ".I", ".SD", "..keep_cols",
  # data.table column names referenced in j-expressions
  "TreeIdx", "WFGRN_LBSCFT", "WFDRY_LBSCFT", "TR_TCFV_ALL",
  "model_class", "predictor_spec_path", "registry_file", "vcov_path",
  # sf geometry column referenced inside dplyr::summarise()
  "geometry"
))
