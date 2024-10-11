*&---------------------------------------------------------------------*
*& Report  ZWMREP0043                                                  *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*

report  zwmrep0043 message-id zwmmsg001.

include: rlmobinc.

tables: tsp03l.

data: itab_sscc like zwm_ean128 occurs 0 with header line.

** Dados gerais
data: ok_code_0043 like sy-ucomm,
      l_error      like bdcmsgcoll-msgv1,
      printer(10),
      sscc like zwm_ean128-sscc,
      cursorfield(20).

start-of-selection.

  message_lang = sy-langu.
  perform user_own_data.

  clear: printer, sscc.

  if lrf_wkqu-devty(5) = '16X20'.
    call screen '0001'.
  else.
    call screen '0002'.
  endif.

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module status_0043 output.

  if printer is initial.
    move 'PRINTER' to cursorfield.
  elseif sscc is initial.
    move 'SSCC' to cursorfield.
  endif.

  set pf-status 'ZRF_IMP'.
  set cursor field cursorfield.

endmodule.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module exit input.

  clear: printer, sscc, ok_code_0043, itab_sscc.
  free: itab_sscc.

  set screen '0000'.
  leave screen.

endmodule.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0043 input.

  data: x_printer like nast-ldest,
        n_copies type itcpo-tdcopies.

  case ok_code_0043.
    when 'IMP'.
      if not sscc    is initial and
         not printer is initial.

        x_printer = printer.

        itab_sscc-sscc = sscc.
        append itab_sscc.

        call function 'ZWM_IMPRIME_EAN128'
          exporting
            printer                  = x_printer
          tables
            sscc                     = itab_sscc
          exceptions
            impressora_nao_existe    = 1
            sscc_nao_existe          = 2
            sscc_com_impressao_grupo = 3
            others                   = 4.

        if sy-subrc <> 0.
          call function 'YWM_MESSAGE_SCREEN'
            exporting
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '172'.

          clear: sscc.
          if lrf_wkqu-devty(5) = '16X20'.
            set screen '0001'.
            leave screen.
          else.
            set screen '0002'.
            leave screen.
          endif.

        endif.

        free:  itab_sscc.
        clear: itab_sscc.

        n_copies = 1.

        select * from zwm061
         into corresponding fields of table itab_sscc
         where lgnum = '100'
           and sscc_master = sscc.

        call function 'ZWM_IMPRIME_EAN128'
          exporting
            printer                  = x_printer
            copies                   = n_copies
          tables
            sscc                     = itab_sscc
          exceptions
            impressora_nao_existe    = 1
            sscc_nao_existe          = 2
            sscc_com_impressao_grupo = 3
            others                   = 4.

        if sy-subrc <> 0.
          call function 'YWM_MESSAGE_SCREEN'
            exporting
              message_id     = 'ZWMMSG001'
              message_lang   = sy-langu
              message_type   = 'E'
              message_number = '172'.

          clear: sscc.
          if lrf_wkqu-devty(5) = '16X20'.
            set screen '0001'.
            leave screen.
          else.
            set screen '0002'.
            leave screen.
          endif.

        endif.


      endif.

      free:  itab_sscc.
      clear: itab_sscc, sscc, ok_code_0043, l_error.

    when 'CLR'.
      free:  itab_sscc.
      clear: itab_sscc, sscc, printer, ok_code_0043, l_error.

  endcase.

endmodule.                 " USER_COMMAND_0043  INPUT
*&---------------------------------------------------------------------*
*&      Module  CHECK_sscc  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module check_sscc input.

  data: l_sscc like vekp-exidv.

  check not sscc is initial.

  clear: l_sscc, l_error.

  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = sscc
    importing
      output = l_sscc.

  clear: vekp.
  select * from vekp
  where exidv eq l_sscc.
    exit.
  endselect.

  if sy-subrc ne 0.
    l_error = sscc.
    call function 'YWM_MESSAGE_SCREEN'
      exporting
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '113'
        message_var1   = l_error.

    move 'SSCC' to cursorfield.
    clear sscc.
    exit.
  endif.

endmodule.                 " CHECK_sscc  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_printer  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module check_printer input.

  data: l_printer like tsp03l-padest.
  data: l_count type i.

  check not printer is initial.

  clear: tsp03l, l_error, l_count.
*-----------------------------------------------Ins 16jun
  l_count = strlen( printer ).
  if l_count gt 4.
    concatenate 'Nome da Impressora só pode ter 4 Caracteres'(001)
                '-' printer '-' 'Erro'(002)
           into l_error separated by space.

    call function 'YWM_MESSAGE_SCREEN'
      exporting
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '000'
        message_var1   = l_error.

    move 'PRINTER' to cursorfield.
    clear: printer, l_error.
    exit.

  endif.
*-----------------------------------------------End
  call function 'CONVERSION_EXIT_ALPHA_INPUT'
    exporting
      input  = printer
    importing
      output = l_printer.

  select * from tsp03l
  where padest eq l_printer.
    exit.
  endselect.

  if sy-subrc ne 0.
    l_error = printer.
    call function 'YWM_MESSAGE_SCREEN'
      exporting
        message_id     = 'ZWMMSG001'
        message_lang   = sy-langu
        message_type   = 'E'
        message_number = '171'
        message_var1   = l_error.

    move 'PRINTER' to cursorfield.
    clear printer.
    exit.
  else.
    translate printer to upper case.
    move 'SSCC' to cursorfield.
  endif.

endmodule.                 " check_printer  INPUT
