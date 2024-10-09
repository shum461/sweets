# multiplication works

    Code
      waldo::compare(
        fd_old, fd_new)
    Output
      class(old) vs class(new)
      - "data.frame"
      + "grouped_df"
      + "tbl_df"
      + "tbl"
      + "data.frame"
      
      names(old) vs names(new)
      - "Flag.."
      + "Flag #"
        "pooled_group"
      - "Samples.Excluded"
      + "Samples Excluded"
      - "Subjects.Affected"
      + "Subjects Affected"
      - "Subjects.Excluded"
      + "Subjects Excluded"
      - "Remaining.Samples"
      + "Remaining Samples"
      - "Remaining.Subjects"
      + "Remaining Subjects"
      
      `attr(old, 'disposition_table')` is absent
      `attr(new, 'disposition_table')` is a logical vector (TRUE)
      
      `attr(old, 'groups')` is absent
      `attr(new, 'groups')` is an S3 object of class <tbl_df/tbl/data.frame>, a list
      
      `attr(old, 'subjects_in_flag')` is absent
      `attr(new, 'subjects_in_flag')` is an S3 object of class <grouped_df/tbl_df/tbl/data.frame>, a list
      
      `attr(old, 'unique_aff_subjects')` is absent
      `attr(new, 'unique_aff_subjects')` is an integer vector (19)
      
      `old$Flag..` is an integer vector (0, 4, 5, 6, 7, ...)
      `new$Flag..` is absent
      
      `old$pooled_group` is an integer vector (1, 1, 1, 1, 1, ...)
      `new$pooled_group` is a character vector ('1', '1', '1', '1', '1', ...)
      
      `old$Samples.Excluded` is an integer vector (0, 0, 0, 0, 0, ...)
      `new$Samples.Excluded` is absent
      
      `old$Subjects.Affected` is an integer vector (0, 3, 13, 13, 7, ...)
      `new$Subjects.Affected` is absent
      
      And 9 more differences ...

