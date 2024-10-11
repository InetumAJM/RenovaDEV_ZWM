*&---------------------------------------------------------------------*
*& Report  ZWMFR0006
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zwmfr0006.

TYPE-POOLS: truxs.

TYPES: BEGIN OF gty_exel,
data TYPE c LENGTH 200,
hora TYPE c LENGTH 200,
processo TYPE c LENGTH 200,
simulacao TYPE c LENGTH 200,
codigo TYPE c LENGTH 200,
altura TYPE c LENGTH 200,
quantidade TYPE c LENGTH 200,
cor TYPE c LENGTH 200,
linha TYPE c LENGTH 200,
aufnr TYPE c LENGTH 200,
pal_completa TYPE c LENGTH 200,
actualiza TYPE c LENGTH 200,
in_string TYPE c LENGTH 200,
out_string TYPE c LENGTH 200,
retorno TYPE c LENGTH 200,
sscc TYPE c LENGTH 200,
mesa TYPE c LENGTH 200,
out_producao TYPE c LENGTH 200,
simula TYPE c LENGTH 200,
posto TYPE c LENGTH 200,
out_cb_tx TYPE c LENGTH 200,
msg TYPE c LENGTH 200,
sscc1 TYPE c LENGTH 200,
matnr TYPE c LENGTH 200,
matnr2 TYPE c LENGTH 200,
matnr_pal TYPE c LENGTH 200,
matnr_pal2 TYPE c LENGTH 200,
pal_remontada TYPE c LENGTH 200,
ordem TYPE c LENGTH 200,
meins TYPE c LENGTH 200,
utilizador TYPE c LENGTH 200,
doc_entrada TYPE c LENGTH 200,
ano_entrada TYPE c LENGTH 200,
doc_transf TYPE c LENGTH 200,
ano_transf TYPE c LENGTH 200,
doc_estorno TYPE c LENGTH 200,
ano_estorno TYPE c LENGTH 200,
doc_palete TYPE c LENGTH 200,
ano_palete TYPE c LENGTH 200,
doc_estr_pal TYPE c LENGTH 200,
ano_estr_pal TYPE c LENGTH 200,
out_string_cb TYPE c LENGTH 200,
out_string_tx TYPE c LENGTH 200,
peso_pal TYPE c LENGTH 200,
uni_peso TYPE c LENGTH 200,
equipa TYPE c LENGTH 200,
turno TYPE c LENGTH 200,
pernr TYPE c LENGTH 200,
erdat_peso TYPE c LENGTH 200,
erzet_peso TYPE c LENGTH 200,
       END OF gty_exel.

DATA: gt_zwm_log_efacec TYPE TABLE OF zwm_log_efacec,
      gt_raw            TYPE truxs_t_text_data,
      gt_exel           TYPE TABLE OF gty_exel.

DATA: gs_exel           TYPE gty_exel,
      gs_zwm_log_efacec TYPE zwm_log_efacec.

DATA: gv_index TYPE syindex,
      gv_type  TYPE c.

FIELD-SYMBOLS: <lv_src> TYPE ANY,
               <lv_dst> TYPE ANY.

PARAMETERS: p_exel TYPE localfile.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_exel.
  PERFORM get_file_exel.


START-OF-SELECTION.

  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
      i_tab_raw_data       = gt_raw
      i_filename           = p_exel
    TABLES
      i_tab_converted_data = gt_exel
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
            DISPLAY LIKE 'E' WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.

  DELETE gt_exel INDEX 1.

  LOOP AT gt_exel INTO gs_exel.
    CLEAR: gs_zwm_log_efacec.

    DO.
      UNASSIGN:  <lv_src>, <lv_dst>.

      gv_index = sy-index.

      UNASSIGN:  <lv_src>, <lv_dst>.
      ASSIGN COMPONENT gv_index OF STRUCTURE gs_exel TO <lv_src>.
      IF NOT <lv_src> IS ASSIGNED.
        EXIT.
      ENDIF.

      gv_index = gv_index + 1.
      ASSIGN COMPONENT gv_index OF STRUCTURE gs_zwm_log_efacec TO <lv_dst>.
      IF NOT <lv_dst> IS ASSIGNED.
        EXIT.
      ENDIF.


      DESCRIBE FIELD <lv_dst> TYPE gv_type.

      CASE gv_type.
        WHEN 'D'.
          REPLACE ALL OCCURRENCES OF '-' IN <lv_src> WITH ''.
          CONDENSE <lv_src> NO-GAPS.
          CONCATENATE <lv_src>+4(4) <lv_src>+2(2) <lv_src>(2) INTO <lv_src>.
        WHEN 'T'.
          REPLACE ALL OCCURRENCES OF ':' IN <lv_src> WITH ''.
          CONDENSE <lv_src> NO-GAPS.
      ENDCASE.

      CALL FUNCTION 'Z_WM_GENERAL_CONVERSION'
        EXPORTING
          i_input_value  = <lv_src>
          i_input_output = 'I'
        IMPORTING
          e_output_value = <lv_dst>.
    ENDDO.

    APPEND gs_zwm_log_efacec TO gt_zwm_log_efacec.
  ENDLOOP.

  LOOP AT gt_zwm_log_efacec INTO gs_zwm_log_efacec.
    MODIFY zwm_log_efacec FROM gs_zwm_log_efacec.
  ENDLOOP.

  COMMIT WORK.
*&---------------------------------------------------------------------*
*&      Form  GET_FILE_EXEL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_file_exel .
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_EXEL'
    IMPORTING
      file_name  = p_exel.
ENDFORM.                    " GET_FILE_EXEL
