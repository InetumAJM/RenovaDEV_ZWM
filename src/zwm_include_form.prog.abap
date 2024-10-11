************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWM_INCLUDE_FORM                                         *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Include FORMS para Pool de Módulos                       *
* Criado por: Luís Rocha                                               *
* Criado em.: 06/12/2004                                               *
* Tipo PRG..: Include                                                  *
************************************************************************

*---------------------------------------------------------------------*
*       FORM ok_code_exit                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form ok_code_exit using f_changes f_tcode f_ecran.

  data: flg_resposta.

  if not ( f_changes is initial ).
    perform confirm_step using text-p18 text-p10 text-p05
                      changing flg_resposta.
    if flg_resposta = 'J'.
      if f_tcode is initial.
        set screen f_ecran.
        leave screen.
      else.
        leave to transaction f_tcode.
      endif.
    endif.
  else.
    if f_tcode is initial.
      set screen f_ecran.
      leave screen.
    else.
      leave to transaction f_tcode.
    endif.
  endif.
  clear sy-ucomm.
endform.

*---------------------------------------------------------------------*
*       FORM ok_code_back                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form ok_code_back using f_changes f_tcode f_ecran.
  data: flg_resposta.

  if not ( f_changes is initial ).
    perform confirm_step using text-p18 text-p11 text-p04
                      changing flg_resposta.
    if flg_resposta = 'J'.
      if f_tcode is initial.
        set screen f_ecran.
        leave screen.
      else.
        leave to transaction f_tcode.
      endif.
    endif.
  else.
    if f_tcode is initial.
      set screen f_ecran.
      leave screen.
    else.
      leave to transaction f_tcode.
    endif.
  endif.
  clear sy-ucomm.
endform.

*---------------------------------------------------------------------*
*       FORM OK_CODE_CANCEL                                           *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form ok_code_cancel using f_changes f_tcode f_ecran.

  data: flg_resposta.

  if not ( f_changes is initial ).
    call function 'POPUP_TO_CONFIRM_LOSS_OF_DATA'
         exporting
              textline1 = text-p18
              textline2 = text-p12
              titel     = text-p06
         importing
              answer    = flg_resposta
         exceptions
              others    = 1.
    if flg_resposta = 'J'.
      if f_tcode is initial.
        set screen f_ecran.
        leave screen.
      else.
        leave to transaction f_tcode.
      endif.
    endif.
  else.
    if f_tcode is initial.
      set screen f_ecran.
      leave screen.
    else.
      leave to transaction f_tcode.
    endif.
  endif.
  clear sy-ucomm.

endform.

*---------------------------------------------------------------------*
*       FORM SCROLLING_IN_WORK_TABLE                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_FIRSTLINE                                                   *
*  -->  F_START                                                       *
*  -->  F_MAXLINES                                                    *
*  -->  F_LAST_PAGE_FULL                                              *
*  -->  F_LOOPS                                                       *
*  -->  F_CODE                                                        *
*  -->  F_OVERLAPPING                                                 *
*  -->  F_ENTRIES_SUM                                                 *
*  -->  F_PAGE_SUM                                                    *
*---------------------------------------------------------------------*
form scrolling_in_work_table using
*                          Importing
                           f_firstline
                           f_start
                           f_maxlines
                           f_last_page_full
                           f_loops
                           f_code
                           f_overlapping
*                          Exporting
                           f_entries_sum
                           f_page_sum.
  call function 'SCROLLING_IN_TABLE'
       exporting
           entry_act             = f_firstline
           entry_from            = f_start
           entry_to              = f_maxlines
           last_page_full        = f_last_page_full
           loops                 = f_loops
           ok_code               = f_code
           overlapping           = f_overlapping
*          page_act              = 0
*          page_go               = 0
      importing
           entries_sum           = f_entries_sum
           entry_new             = f_firstline
           pages_sum             = f_page_sum
*          page_new              = 0
       exceptions
            no_maxlines_or_page_act  = 1
            no_maxlines_to           = 2
            no_ok_code_or_page_go = 3
            others                = 4.
endform.

*---------------------------------------------------------------------*
*       FORM refresh                                                  *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form refresh using f_changes f_initial.

  data: flg_resposta.

  if not ( f_changes is initial ).
    perform confirm_step using ' ' text-p19 text-p17
                         changing flg_resposta.
    if flg_resposta = 'J'.
      clear: f_initial.
    endif.
  else.
    clear: f_initial.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM altera_dados                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form
altera_dados using f_changes f_initial.

  data: flg_resposta,
        flg_subrc like sy-subrc.

  if not ( f_changes is initial ).
    perform confirm_step using ' ' text-p13 text-p17
                         changing flg_resposta.
    if flg_resposta = 'J'.
      perform modifica_db_tab using flg_subrc.
      check flg_subrc is initial.
      clear: f_changes, f_initial.
      message s002(sy) with text-001.
    endif.
  else.
    if sy-binpt is initial.
      message i002(sy) with text-002.
    else.
      message s002(sy) with text-002.
    endif.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM apaga_dados                                              *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form apaga_dados using f_changes f_initial.

  data: flg_resposta.

  if not ( f_changes is initial ).
    perform confirm_step using ' ' text-p20 text-p17
                         changing flg_resposta.
    if flg_resposta = 'J'.
*     perform apaga_db_regs.
      clear f_initial.
      message s002(sy) with text-001.
    endif.
  else.
    if sy-binpt is initial.
      message i002(sy) with text-002.
    else.
      message s002(sy) with text-002.
    endif.
  endif.
endform.

*---------------------------------------------------------------------*
*       FORM popup                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form popup using resposta f_frete.
  call function 'POPUP_TO_CONFIRM_WITH_MESSAGE'
       exporting
            defaultoption  = ' '
            diagnosetext1  = text-p14
            diagnosetext2  = f_frete
            diagnosetext3  = space
            textline1      = text-p15
            textline2      = space
            titel          = text-p16
       importing
            answer         = resposta
       exceptions
            titel_too_long = 01.
endform.

*---------------------------------------------------------------------*
*       FORM pergunta                                                 *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
form pergunta using txt_oper txt_mens resposta.
  call function 'POPUP_TO_CONFIRM_WITH_MESSAGE'
       exporting
            defaultoption  = ' '
            diagnosetext1  = space
            diagnosetext2  = txt_mens
            diagnosetext3  = space
            textline1      = text-p18
            textline2      = txt_oper
            titel          = text-p16
       importing
            answer         = resposta
       exceptions
            titel_too_long = 01.
endform.

*---------------------------------------------------------------------*
*       FORM CONFIRM_STEP                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_LINE1                                                       *
*  -->  F_LINE2                                                       *
*  -->  F_TITLE                                                       *
*  -->  F_ANSWER                                                      *
*---------------------------------------------------------------------*
form confirm_step using f_line1 f_line2 f_title f_answer.
  call function 'POPUP_TO_CONFIRM_STEP'
       exporting
*           defaultoption  = ' '
            textline1 = f_line1
            textline2 = f_line2
            titel     = f_title
       importing
            answer    = f_answer
       exceptions
            others    = 1.
endform.
