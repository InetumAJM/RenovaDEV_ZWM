FUNCTION ZWM_EXIT_AUT_SPLIT_PRM .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_LGNUM) TYPE  LGNUM OPTIONAL
*"     VALUE(I_TANUM) TYPE  TANUM OPTIONAL
*"     VALUE(IS_LTAK_VB) TYPE  LTAK_VB OPTIONAL
*"     VALUE(IT_LTAP_VB) TYPE  TT_LTAP_VB OPTIONAL
*"----------------------------------------------------------------------

  DATA: lt_ltap_vb         TYPE tt_ltap_vb,
        lt_ltap_vb_aut_out TYPE tt_ltap_vb,
        lt_return_msg      TYPE TABLE OF bdcmsgcoll,
        lt_ltap_create     TYPE TABLE OF ltap_creat,
        lt_ltap_lenum      TYPE TABLE OF ltap.

  DATA: ls_ltap        TYPE ltap,
        ls_ltak_vb     TYPE ltak_vb,
        ls_ltap_vb     TYPE ltap_vb,
        ls_ltap_create TYPE ltap_creat.

  DATA: lv_tanum  TYPE tanum,
        lv_2step  TYPE flag,
        lv_2spart TYPE flag,
        lv_exit   TYPE flag.

  CLEAR: ls_ltak_vb, lt_ltap_vb.


*  DO.
*    IF lv_exit EQ 'X'.
*      EXIT.
*    ENDIF.
*  ENDDO.

** Process
***********************************************************************
  IF NOT i_lgnum IS INITIAL AND
     NOT i_tanum IS INITIAL.
    SELECT SINGLE * FROM ltak
                    INTO CORRESPONDING FIELDS OF ls_ltak_vb
                    WHERE lgnum = i_lgnum AND
                          tanum = i_tanum.


    SELECT * FROM ltap
             INTO CORRESPONDING FIELDS OF TABLE lt_ltap_vb
             WHERE lgnum = i_lgnum AND
                   tanum = i_tanum.
  ELSE.
    SELECT SINGLE * FROM ltak
                    INTO CORRESPONDING FIELDS OF ls_ltak_vb
                    WHERE lgnum = is_ltak_vb-lgnum AND
                          tanum = is_ltak_vb-tanum.

    SELECT * FROM ltap
             INTO CORRESPONDING FIELDS OF TABLE lt_ltap_vb
             WHERE lgnum = is_ltak_vb-lgnum AND
                   tanum = is_ltak_vb-tanum.
  ENDIF.

  SORT lt_ltap_vb BY tanum tapos.



** Split de Tarefas por ação
***********************************************************************
  LOOP AT lt_ltap_vb INTO ls_ltap_vb WHERE "vltyp = 'AUT' AND
                                           nltyp = 'PRM' AND
                                           pquit = abap_true.
    APPEND ls_ltap_vb TO lt_ltap_vb_aut_out.
    IF NOT ls_ltap_vb-orpos IS INITIAL.
      DELETE lt_ltap_vb_aut_out WHERE tanum = ls_ltap_vb-tanum AND
                                      tapos = ls_ltap_vb-orpos.
    ENDIF.
  ENDLOOP.

  DO 1 TIMES.
    CHECK NOT lt_ltap_vb_aut_out IS INITIAL.

    SELECT * FROM ltap
             INTO TABLE lt_ltap_lenum
             FOR ALL ENTRIES IN lt_ltap_vb_aut_out
             WHERE lgnum = lt_ltap_vb_aut_out-lgnum AND
                   vlenr = lt_ltap_vb_aut_out-nlenr AND
                   vltyp = lt_ltap_vb_aut_out-nltyp AND
                   vlpla = lt_ltap_vb_aut_out-nlpla AND
                   nltyp IN ( '815', '916' ).

    CHECK NOT lt_ltap_lenum IS INITIAL.

    DELETE lt_ltap_lenum WHERE vorga EQ 'ST'.

    LOOP AT lt_ltap_lenum INTO ls_ltap.
      DELETE lt_ltap_vb_aut_out WHERE vlenr = ls_ltap-vlenr.
    ENDLOOP.
  ENDDO.


** Saida de Remontadas
***********************************************************************
  IF z_wm_cl_management=>is_split_remontada( it_ltap = lt_ltap_vb_aut_out ) EQ abap_true.
    LOOP AT lt_ltap_vb_aut_out INTO ls_ltap_vb.
      CHECK z_wm_cl_management=>is_remontada( is_data = ls_ltap_vb ) EQ abap_true.

      CHECK ls_ltap_vb-pquit EQ abap_true.

      IF ls_ltak_vb-vbeln IS INITIAL AND
         ( ls_ltak_vb-betyp EQ 'L' OR ls_ltak_vb-betyp EQ 'I' ) AND
         NOT ls_ltak_vb-benum IS INITIAL.
        ls_ltak_vb-vbeln = ls_ltak_vb-benum.
      ENDIF.

      IF ls_ltak_vb-refnr IS INITIAL AND
         NOT ls_ltak_vb-vbeln IS INITIAL.
        SELECT SINGLE refnr FROM t311a
                            INTO ls_ltak_vb-refnr
                            WHERE lgnum = ls_ltak_vb-lgnum AND
                                  rbtyp = 'L' AND
                                  rbnum = ls_ltak_vb-vbeln.
      ENDIF.

      CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
        EXPORTING
          i_lgnum  = ls_ltak_vb-lgnum
          i_refnr  = ls_ltak_vb-refnr
          i_vbeln  = ls_ltak_vb-vbeln
        IMPORTING
          e_2step  = lv_2step
          e_2spart = lv_2spart
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF lv_2spart EQ abap_true.
        ls_ltap_create-werks = ls_ltap_vb-werks.
        ls_ltap_create-lgort = ls_ltap_vb-lgort.
        ls_ltap_create-vltyp = ls_ltap_vb-nltyp.
        ls_ltap_create-vlpla = ls_ltap_vb-nlpla.
        ls_ltap_create-nlenr = ls_ltap_vb-nlenr.
        ls_ltap_create-matnr = ls_ltap_vb-matnr.
        ls_ltap_create-charg = ls_ltap_vb-charg.
        ls_ltap_create-anfme = ls_ltap_vb-vsolm.
        ls_ltap_create-altme = ls_ltap_vb-altme.
        APPEND ls_ltap_create TO lt_ltap_create.


        CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
          EXPORTING
            i_lgnum       = ls_ltak_vb-lgnum
            i_bwlvs       = '850'
            i_betyp       = 'L'
            i_benum       = ls_ltak_vb-vbeln
            i_commit_work = abap_true
            i_refnr       = ls_ltak_vb-refnr
            i_l2ska       = '1'
            i_kompl       = abap_false
          TABLES
            t_ltap_creat  = lt_ltap_create
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.

      ELSEIF lv_2step EQ abap_true.
        CALL FUNCTION 'ZWM_TO_CREATE_2_STEP_PICKING'
          EXPORTING
            i_lgnum      = ls_ltak_vb-lgnum
            i_refnr      = ls_ltak_vb-refnr
            i_up_grp_anl = ''
          EXCEPTIONS
            error        = 1
            OTHERS       = 2.
        CHECK sy-subrc EQ 0.
      ELSE.
        CALL FUNCTION 'ZWM_CREATE_TO_DELIVERY'
          EXPORTING
            warehouse     = ls_ltap_vb-lgnum
            refnr         = ls_ltak_vb-refnr
            vbeln         = ls_ltak_vb-vbeln
            posnr         = ls_ltap_vb-posnr
            vsola         = ls_ltap_vb-vsolm
            su            = ls_ltap_vb-nlenr
          IMPORTING
            to            = lv_tanum
          TABLES
            return_msg    = lt_return_msg
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.
        CHECK sy-subrc EQ 0.

      ENDIF.

      DELETE lt_ltap_vb_aut_out WHERE tanum = ls_ltap_vb-tanum.
    ENDLOOP.
  ENDIF.



ENDFUNCTION.
