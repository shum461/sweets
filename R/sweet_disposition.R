#' Make a disposition table
#'
#' @param data a data frame that must include deletion flags called DELFN.
#' @param subjid Subject Identifier such as USUBJID, or ID.
#' @param group_vars grouping variables useful in disposition tables such as STUDY or STUDYID. If none are provided a default group="1" will be set
#' @param cnt_n_keeps Deletion Flags DELFN that should be included in disposition table but do NOT remove subjects or rows.
#' @return A disposition table.
#' @examples
#' sweet_disposition(subjid = USUBJID,
#' group_vars = c(STUDY),
#' cnt_n_keeps = c(16,24)
#' )




sweet_disposition <- function(data, subjid, group_vars, cnt_n_keeps=NULL,
                                init_desc = "Concentration Records Received"){

  # Not providing grouping variables will pool the disposition
  if (missing(group_vars)) {
    data <-  data %>%
      dplyr::ungroup() %>%
      dplyr::mutate(pooled_group = "1")

    group_vars <- "pooled_group"

    cli::cli_alert_warning("{.arg group_vars} is missing. No grouping variables found")
  }

  # ERROR if DDELFN missing
  if(!"DELFN" %in% names(data)){
    cli::cli_abort("Deletion flags variable named {.var DELFN} must be present in data")}

  # Error if DELFN is not numeric
  if(!is.numeric({{data$DELFN}})){
    cli::cli_abort(c(
      "{.var del_flags} must be a {.cls {class(5)}} vector",
      "x" = "You tried {.arg {del_flags}} which is a {.cls {class(del_flags)}} vector."
    ))}

  # Warning: if any DELFN are NA, they will be set to 0 (i.e. no deletion)

  DELFN_is_NA <- length(data$DELFN[is.na(data$DELFN)])

  if(DELFN_is_NA>0){
    cli::cli_alert_warning(c(
      "{.val {DELFN_is_NA}} rows where {.var DELFN} was NA. ",
      "{.var DELFN} for these rows was set to 0"
    ))}

  # set NAs to 0 if they exist
  data <- data %>%
    dplyr::ungroup() %>%
    dplyr::mutate(DELFN=dplyr::if_else(is.na(DELFN),0,DELFN))

  # Data with no DELFN filtered out
  nothing_removed <- data %>%
    dmcognigen::cnt(Flag=0,dplyr::across({{group_vars}}),
                    n_distinct_vars = {{subjid}},prop = FALSE, pct = FALSE)

  # Name of the n distinct var, usually n_ID or n_USUBJID
  n_name <-  dmcognigen::cnt(data, dplyr::across({{group_vars}}),
                             n_distinct_vars = {{subjid}},prop = FALSE, pct = FALSE) %>%
    dplyr::select(starts_with("n_"),-n_cumulative) %>%
    names()

  # Names of group vars
  grp_vars <- data %>%
    dplyr::select({{group_vars}}) %>%
    names()


  # Make sure 'count and keeps' are valid DELFNs

  if(!missing(cnt_n_keeps) & length(setdiff(cnt_n_keeps,data$DELFN))>0){

    diff_len <- length(setdiff(cnt_n_keeps,data$DELFN))

    cli::cli_warn(c("{diff_len} invalid DELFN{?s} provided to {.arg cnt_n_keeps}",
                    "{.val {setdiff(cnt_n_keeps,data$DELFN)}} must be in {.val {unique(data$DELFN)}}"))

  }

  # Keep and counts
  if (!is.null(cnt_n_keeps)) {
    keeps <- cnt_n_keeps
  } else{
    keeps <- -Inf
  }



  #Allows for Reason (DELFNC) to be in data or added later
  #Replace 'Analysis Record' with Concentration Records Received
  if("DELFNC" %in% names(data)){

    reasons <- data %>%
      dplyr::distinct(DELFN, DELFNC) %>%
      dplyr::mutate(DELFNC = dplyr::case_when(DELFN == 0 ~ init_desc,
                                              TRUE ~ DELFNC)) %>%
      dplyr::rename('Reason for Deletion'= DELFNC)

    unique_DELFNC_check <- data %>%
      dplyr::group_by(DELFN) %>%
      dplyr::summarise(N_DELFNC=n_distinct(DELFNC),
                       DELFNC=DELFNC)
  }
  else {
    reasons <- data %>%
    dplyr::distinct(DELFN)
    cli::cli_alert_info("Deletion Reason (DELFNC) was not provided")

    unique_DELFNC_check=NULL

  }

  if(any(unique_DELFNC_check$N_DELFNC > 1)){

    count_of_DELFNC <- length(unique(unique_DELFNC_check$DELFN[unique_DELFNC_check$N_DELFNC>1]))

    cli::cli_alert_danger(c("{cli::qty(count_of_DELFNC)} DELFN{?s} {.val {unique_DELFNC_check %>% filter(N_DELFNC>1) %>% distinct(DELFN)}}",
                            "{cli::qty(count_of_DELFNC)} {?has/have} non unique DELFNC{?s}"))

  }

  # Counts of subjects
  affected_Subjects <- data %>%
    dmcognigen::cnt(dplyr::across({{group_vars}}),DELFN,n_distinct_vars = {{subjid}},
                    prop = FALSE, pct = FALSE) %>%
    dplyr::select({{group_vars}},DELFN,SubjAffected=n_name)%>%
    dplyr::mutate(SubjAffected=ifelse(DELFN==0,0,SubjAffected))

  # Unique aff subjects
  unique_aff_subjects <-  data %>%
    dplyr::filter(DELFN>0) %>%
    dmcognigen::cnt(n_distinct_vars = {{subjid}},prop = FALSE, pct = FALSE)%>%
    dplyr::pull(1)

  # DELFNs with USUBJID in list col
  nested_subjects <-
    data %>%
    dplyr::distinct(DELFN,{{subjid}}) %>%
    dplyr::group_by(DELFN) %>%
    tidyr::nest() %>%
    dplyr::rename(subjects_in_flag=data)

  #list(affected_Subjects,grp_vars,n_name )

  deletion_flags <- sort(
    unique(data$DELFN) %>% setdiff(keeps)
  )

  # Just actual deletions
  df_deletions <- purrr::map_dfr(deletion_flags,~
                                   data %>%
                                   dplyr::filter(DELFN==0| DELFN>.x | DELFN %in% keeps) %>%
                                   dmcognigen::cnt(DELFN=.x,dplyr::across({{group_vars}}), n_distinct_vars = {{subjid}}),
                                 prop = FALSE, pct = FALSE) %>%
    dplyr::group_by(dplyr::across({{group_vars}}))
  # n_name is usually n_USUBJID or n_ID from cnt()
  # n is the number of rows from cnt()

  # Just count and keeps
  df_keeps <- purrr::map_dfr(keeps,~
    data %>%
    dplyr::filter(DELFN==0|DELFN>.x) %>%
    dmcognigen::cnt(DELFN=.x,dplyr::across({{group_vars}}),
        n_distinct_vars = {{subjid}}),prop = FALSE, pct = FALSE) %>%
    dplyr::group_by(dplyr::across({{group_vars}}))


  df_both <- df_deletions %>%
    dplyr::bind_rows(df_keeps) %>%
    dplyr::arrange(DELFN) %>%
    dplyr::mutate(KEEPFLG=ifelse(DELFN %in% keeps,1,0))

  # Last real DELFN. Values to use if count and keep DELFN is encountered
  find_flag <- function(df,current_flag){
    if (current_flag==0) {
      0
    } else{
      max(df$DELFN[df$DELFN < current_flag & df$KEEPFLG == 0])
    }}



  df2 <- df_both %>%
    dplyr::mutate(across(c(!!!n_name,n,n_cumulative),~ifelse(KEEPFLG==0,.x,NA_real_))) %>%
    tidyr::fill(!!!n_name,n,n_cumulative,.direction="down") %>%
    dplyr::mutate(Remaining_subj=eval(dplyr::sym(n_name)),
           SubjExcluded=lag(Remaining_subj,n=1)-Remaining_subj,
           SamplesExcluded=lag(n,n=1)-n)

  df3 <- df2 %>%
    dplyr::full_join(affected_Subjects,by=c(grp_vars,"DELFN"))%>%
    dplyr::select(starts_with("DELF"),
                  grp_vars,
                  SubjAffected,
                  SamplesExcluded,
                  SubjExcluded,
                  Remaining_subj,
                  Remaining_Samp=n)


  df4 <- df3 %>%
    dplyr::filter(!DELFN %in% -Inf)  %>%
    dplyr::arrange(DELFN,dplyr::across({{group_vars}}))%>%
    dplyr::mutate(
      SubjAffected = tidyr::replace_na(SubjAffected, 0L),
      # check for deletions where the remainder of some group is completely removed.
      SamplesExcluded = dplyr::case_when(
        !is.na(SamplesExcluded) ~ SamplesExcluded,
        dplyr::row_number() == 1 ~ 0,
        TRUE ~ dplyr::lag(Remaining_Samp)
      ),
      SubjExcluded = dplyr::case_when(
        !is.na(SubjExcluded) ~ SubjExcluded,
        dplyr::row_number() == 1 ~ 0,
        TRUE ~ dplyr::lag(Remaining_subj)
      ),
      Remaining_Samp = tidyr::replace_na(Remaining_Samp, 0),
      Remaining_subj = tidyr::replace_na(Remaining_subj, 0)
    )


  df5 <- df4 %>%
    dplyr::left_join(reasons,by="DELFN") %>%
    dplyr::arrange(DELFN, dplyr::across({{group_vars}})) %>%
    dplyr::select('Flag #'= DELFN,
                  dplyr::starts_with('Reason'),
                  dplyr::any_of(grp_vars),
                  'Samples Excluded'= SamplesExcluded,
                  'Subjects Affected' = SubjAffected,
                  'Subjects Excluded' = SubjExcluded,
                  'Remaining Samples' = Remaining_Samp,
                  'Remaining Subjects' = Remaining_subj
    )


  #class(df) <- append(class(df),"disposition_table")

  attr(df5, "disposition_table") <- TRUE
  attr(df5, "unique_aff_subjects") <- unique_aff_subjects
  attr(df5, "subjects_in_flag") <- nested_subjects
  return(df5)

}
