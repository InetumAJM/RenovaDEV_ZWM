constants: on   value 'X',
           off  value ' '.

*---------------------------------------------------------------------*
*       FORM DYNPRO                                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(DYNBEGIN)                                               *
*  -->  VALUE(NAME)                                                   *
*  -->  VALUE(VALUE)                                                  *
*---------------------------------------------------------------------*
form dynpro tables bdc_tab structure bdcdata
            using value(dynbegin) value(name) value(value).

  clear bdc_tab.
  if dynbegin = 'X'.
    bdc_tab-program  = name.
    bdc_tab-dynpro   = value.
    bdc_tab-dynbegin = 'X'.
  else .
    bdc_tab-fnam = name.
    bdc_tab-fval = value.
  endif.
  append bdc_tab.

endform.                               " DYNPRO

*---------------------------------------------------------------------*
*       FORM BDC_OPEN                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  GROUP                                                         *
*---------------------------------------------------------------------*
form bdc_open using value(group) subrc.

  if not group is initial.
    call function 'BDC_OPEN_GROUP'
         exporting
              client              = sy-mandt
              group               = group
              user                = sy-uname
         exceptions
              client_invalid      = 1
              destination_invalid = 2
              group_invalid       = 3
              holddate_invalid    = 4
              internal_error      = 5
              queue_error         = 6
              running             = 7
              user_invalid        = 8.

    subrc = sy-subrc.

  else.

    subrc = 999.

  endif.

endform.                               " BDC_OPEN

*---------------------------------------------------------------------*
*       FORM BDC_INSERT                                               *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  TCODE                                                         *
*---------------------------------------------------------------------*
form bdc_insert tables bdc_tab structure bdcdata
                using value(tcode) value(online) value(debug).

  if online is initial and debug = off.
    call function 'BDC_INSERT'
         exporting
              tcode          = tcode
         tables
              dynprotab      = bdc_tab
         exceptions
              internal_error = 1
              not_open       = 2
              queue_error    = 3
              tcode_invalid  = 4.
  elseif debug = on.
    call transaction tcode using bdc_tab mode 'A'.
  else.
    call transaction tcode using bdc_tab mode 'E'.
  endif.

  refresh bdc_tab.

endform.                               " BDC_INSERT

*---------------------------------------------------------------------*
*       FORM BDC_CLOSE                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form bdc_close using value(online) value(debug).

  if online = off and debug = off.
    call function 'BDC_CLOSE_GROUP'
         exceptions
              not_open    = 1
              queue_error = 2.
  endif.

endform.                               " BDC_CLOSE

*---------------------------------------------------------------------*
*       FORM PROCESSA_BI                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  VALUE(BI_NAME)                                                *
*  -->  F_STRIM                                                       *
*---------------------------------------------------------------------*
form processa_bi using value(bi_name) f_strim.

  tables: apqi.
  data: jobcount like tbtcjob-jobcount,
        jobname  like tbtcjob-jobname.

  constants: k_spc1(8) value '        ',
             k_spc2    value ' '.

  jobname = bi_name.
  select * from apqi where destsys = k_spc1
                       and destapp = k_spc1
                       and datatyp = 'BDC'
                       and mandant = sy-mandt
                       and groupid = bi_name
                       and progid  = k_spc1
                       and qattrib = k_spc2
                       and qstate  = k_spc2.
    exit.
  endselect.

  if sy-subrc = 0.
    call function 'JOB_OPEN'
         exporting
              jobgroup = 'BATCH-INPUT'
              jobname  = jobname
         importing
              jobcount = jobcount.

    submit rsbdcbtc user sy-uname
          via job jobname number jobcount
          with queue-id eq apqi-qid
          with mappe    eq apqi-groupid
          with modus    eq 'N'
          and return.

    call function 'JOB_CLOSE'
         exporting
              jobcount  = jobcount
              jobname   = jobname
              strtimmed = f_strim.

  endif.

endform.

*---------------------------------------------------------------------*
*       FORM LOAD                                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  DATA_TAB                                                      *
*  -->  VALUE(ORIGEM)                                                 *
*  -->  FILE                                                          *
*---------------------------------------------------------------------*
*form load tables data_tab using value(origem) file.
*
*
*  translate origem to upper case.
*  if origem ne 'DOS'.
*    open dataset file for input in text mode.
*    if sy-subrc <> 0. sy-subrc = 2. exit. endif.
*    do.
*      read dataset file into data_tab.
*      if sy-subrc <> 0.
*        exit.
*      endif.
*      append data_tab.
*    enddo.
*    if sy-subrc = 8. sy-subrc = 3. exit. endif.
*    close dataset file.
*  else.
*    call function 'WS_UPLOAD'
*         exporting
*              codepage            = 'IBM'
*              filename            = file
*              filetype            = 'ASC'
*              headlen             = space
*              line_exit           = space
*              trunclen            = space
*              user_form           = space
*              user_prog           = space
*         tables
*              data_tab            = data_tab
*         exceptions
*              conversion_error    = 1
*              file_open_error     = 2
*              file_read_error     = 3
*              invalid_table_width = 4
*              invalid_type        = 5
*              no_batch            = 6
*              unknown_error       = 7.
*  endif.
*
*endform.

*---------------------------------------------------------------------*
*       FORM SAVE                                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  DATA_TAB                                                      *
*  -->  VALUE(DESTINO)                                                *
*  -->  FILE                                                          *
*---------------------------------------------------------------------*
*form save tables data_tab
*          using value(destino) file zzact_filename.
*
*
*  translate destino to upper case.
*  if destino ne 'DOS'.                 " FILE CS '/'.
*    open dataset file for output in text mode.
*    if sy-subrc <> 0. sy-subrc = 2. exit. endif.
*    loop at data_tab.
*      transfer data_tab to file.
*      if sy-subrc <> 0. exit. endif.
*    endloop.
*    if sy-subrc = 8. sy-subrc = 3. exit. endif.
*    close dataset file.
*  else.
*    call function 'DOWNLOAD'
*         exporting
*              codepage                = 'IBM'
*              filename                = file
*              filetype                = 'ASC'
*         importing
*              act_filename            = zzact_filename
*         tables
*              data_tab                = data_tab
*         exceptions
*              file_open_error         = 1
*              file_write_error        = 2
*              invalid_filesize        = 3
*              invalid_table_width     = 4
*              invalid_type            = 5
*              no_batch                = 6
*              unknown_error           = 7
*              gui_refuse_filetransfer = 8
*              customer_error          = 9.
*  endif.
*
*endform.
*
*---------------------------------------------------------------------*
*       FORM LOAD1                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_TAB                                                         *
*  -->  VALUE(F_ORIGEM)                                               *
*  -->  F_FILE                                                        *
*  -->  F_TYPE                                                        *
*---------------------------------------------------------------------*
*form load1 tables f_tab using value(f_origem) f_file f_type.
*
*  if f_type is initial. f_type = 'ASC'. endif.
*
*  translate: f_origem to upper case,
*             f_type   to upper case.
*  if f_origem ne 'DOS'.
*    open dataset f_file for input in text mode.
*    if sy-subrc <> 0. sy-subrc = 2. exit. endif.
*    do.
*      read dataset f_file into f_tab.
*      if sy-subrc <> 0. exit. endif.
*      append f_tab.
*    enddo.
*    if sy-subrc = 8. sy-subrc = 3. exit. endif.
*    close dataset f_file.
*  else.
*    call function 'WS_UPLOAD'
*         exporting
*              codepage            = 'IBM'
*              filename            = f_file
*              filetype            = f_type
*              headlen             = space
*              line_exit           = space
*              trunclen            = space
*              user_form           = space
*              user_prog           = space
*         tables
*              data_tab            = f_tab
*         exceptions
*              conversion_error    = 1
*              file_open_error     = 2
*              file_read_error     = 3
*              invalid_table_width = 4
*              invalid_type        = 5
*              no_batch            = 6
*              unknown_error       = 7.
*  endif.
*
*endform.
*
*---------------------------------------------------------------------*
*       FORM LOAD2                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_TAB                                                         *
*  -->  VALUE(F_ORIGEM)                                               *
*  -->  F_FILE                                                        *
*  -->  F_TYPE                                                        *
*---------------------------------------------------------------------*
*form load2 tables f_tab using value(f_origem) f_file f_type.
*
*  if f_type is initial. f_type = 'ASC'. endif.
*
*  translate: f_origem to upper case,
*             f_type   to upper case.
*  if f_origem ne 'DOS'.
*    open dataset f_file for input in text mode.
*    if sy-subrc <> 0. sy-subrc = 2. exit. endif.
*    do.
*      read dataset f_file into f_tab.
*      if sy-subrc <> 0. exit. endif.
*      append f_tab.
*    enddo.
*    if sy-subrc = 8. sy-subrc = 3. exit. endif.
*    close dataset f_file.
*  else.
*    call function 'WS_UPLOAD'
*         exporting
**             codepage            = 'IBM'
*              filename            = f_file
*              filetype            = f_type
*              headlen             = space
*              line_exit           = space
*              trunclen            = space
*              user_form           = space
*              user_prog           = space
*         tables
*              data_tab            = f_tab
*         exceptions
*              conversion_error    = 1
*              file_open_error     = 2
*              file_read_error     = 3
*              invalid_table_width = 4
*              invalid_type        = 5
*              no_batch            = 6
*              unknown_error       = 7.
*  endif.
*
*endform.
*---------------------------------------------------------------------*
*       FORM SAVE1                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_TAB                                                         *
*  -->  VALUE(F_DESTINO)                                               *
*  -->  F_FILE                                                        *
*  -->  F_TYPE                                                        *
*---------------------------------------------------------------------*
*form save1 tables f_tab using value(f_destino) f_file f_type.
*  if f_type is initial. f_type = 'ASC'. endif.
*
*  translate: f_destino to upper case,
*             f_type    to upper case.
*  if f_destino ne 'DOS'.
*    open dataset f_file for output in text mode.
*    if sy-subrc <> 0. sy-subrc = 2. exit. endif.
*    loop at f_tab.
*      transfer f_tab to f_file.
*      if sy-subrc <> 0. exit. endif.
*    endloop.
*    if sy-subrc = 8. sy-subrc = 3. exit. endif.
*    close dataset f_file.
*  else.
*    call function 'WS_DOWNLOAD'
*         exporting
*              codepage            = 'IBM'
*              filename            = f_file
*              filetype            = f_type
*         tables
*              data_tab            = f_tab
*         exceptions
*              file_open_error     = 1
*              file_write_error    = 2
*              invalid_filesize    = 3
*              invalid_table_width = 4
*              invalid_type        = 5
*              no_batch            = 6
*              unknown_error       = 7.
*  endif.
*
*endform.
*
