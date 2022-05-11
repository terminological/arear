utils::globalVariables(c(".id",".interp","A_U_m_k","D_V_n","R","S_P_m","altCode","area.in","as_label","code","count",
"delta_A_U_m_k","demandCode","end","fracInput","from","from_tmp_id","geometry",
"intersectionArea","intersectionValue","k","m","n","name","start","supplyCode",
"tmp_code","tmp_id","tmp_input_id","tmp_name","tmp_output_id","to","to_tmp_id"))

# paste0(unname(unlist(sapply(c("dplyr","tidyr"), function(p) {
#   functions = ls(asNamespace("sf"))[ls(asNamespace("sf")) %in% paste0(ls(asNamespace(p)),".sf")]
#   paste0("#' @importFrom sf ",functions)
# }))),collapse="\n") %>% clipr::write_clip()

#' @import sf
NULL
