*&---------------------------------------------------------------------*
*&  Include           ZWMREPF080                                       *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  reg_t2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_2_ZMATNR  text
*      -->P_2_ZERDAT  text
*      -->P_2_ZERUHR  text
*      -->P_2_ZEXIDV  text
*      -->P_2_ZVBELN  text
*      -->P_2_ZKUNNR  text
*----------------------------------------------------------------------*
FORM reg_t2  USING    p_2_zmatnr
                      p_2_zerdat
                      p_2_zeruhr
                      p_2_zexidv
                      p_2_zvbeln
                      p_2_zkunnr.
ENDFORM.                                                    " reg_t2
*&---------------------------------------------------------------------*
*&      Form  get_event
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_EVENTS  text
*----------------------------------------------------------------------*
FORM get_event  CHANGING p_events TYPE slis_t_event.
  wa_events-name = slis_ev_user_command.
  wa_events-form = 'USER_COMMAND'.
  APPEND wa_events TO p_events.
ENDFORM.                    " get_event
*&---------------------------------------------------------------------*
*&      Form  f_header
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_header .
  CLEAR header.
  header-typ  = 'H'.
  header-info = 'Rastreabilidade de Produtos - Lista de Movimentos'.
  APPEND header TO it_header.

*  CLEAR header.
*  header-typ  = 'A'.
*  header-key  = ' '.
*  header-info = 'COES'.
*  APPEND header TO it_header.

  CLEAR header.
  header-typ  = 'A'.
  header-key  = ' '.
  header-info = ' '.
  APPEND header TO it_header.
ENDFORM.                    " f_header
*&---------------------------------------------------------------------*
*&      Form  get_campos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_IT_FIELDCAT  text
*----------------------------------------------------------------------*
FORM get_campos  CHANGING p_it_fieldcat TYPE
                                slis_t_fieldcat_alv.

  DATA l_fieldcat TYPE slis_fieldcat_alv.

  MOVE sy-repid TO repid.

  SET TITLEBAR 'Rastreabilidade de Produtos - Lista de Movimentos' OF
  PROGRAM repid.

  CLEAR : p_it_fieldcat.
  REFRESH : p_it_fieldcat.

  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = repid
      i_internal_tabname     = 'TABI'
      i_inclname             = repid
    CHANGING
      ct_fieldcat            = p_it_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.


  LOOP AT p_it_fieldcat INTO l_fieldcat.
*    l_fieldcat-col_pos = sy-tabix.
    CASE l_fieldcat-fieldname.

      WHEN 'TIPO'.
        l_fieldcat-outputlen = 1.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = ' '.
        l_fieldcat-do_sum = 'X '.
        l_fieldcat-key = 'X'.
        l_fieldcat-seltext_l = 'T'.
        l_fieldcat-seltext_m = 'T'.
        l_fieldcat-seltext_s = 'T'.
        l_fieldcat-reptext_ddic = 'T'.
        l_fieldcat-col_pos = '1'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.


      WHEN 'MATNR'.
        l_fieldcat-outputlen = 18.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = 'X'.
        l_fieldcat-seltext_l = 'Material'.
        l_fieldcat-seltext_m = 'Material'.
        l_fieldcat-seltext_s = 'Material'.
        l_fieldcat-reptext_ddic = 'Material'.
        l_fieldcat-col_pos = '2'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'CHARG'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = 'X'.
        l_fieldcat-seltext_l = 'Lote'.
        l_fieldcat-seltext_m = 'Lote'.
        l_fieldcat-seltext_s = 'Lote'.
        l_fieldcat-reptext_ddic = 'Lote'.
        l_fieldcat-col_pos = '3'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.


      WHEN 'EXIDV'.
        l_fieldcat-outputlen = 20.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = 'X'.
        l_fieldcat-seltext_l = 'SSCC - Prod.'.
        l_fieldcat-seltext_m = 'SSCC - Prod.'.
        l_fieldcat-seltext_s = 'SSCC - Prod.'.
        l_fieldcat-reptext_ddic = 'SSCC - Prod.'.
        l_fieldcat-col_pos = '4'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'EXIDV_PIK'.
        l_fieldcat-outputlen = 20.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = 'X'.
        l_fieldcat-seltext_l = 'SSCC - Pick.'.
        l_fieldcat-seltext_m = 'SSCC - Pick.'.
        l_fieldcat-seltext_s = 'SSCC - Pick.'.
        l_fieldcat-reptext_ddic = 'SSCC - Pick.'.
        l_fieldcat-col_pos = '5'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'TEXTO'.
        l_fieldcat-outputlen = 5.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Texto'.
        l_fieldcat-seltext_m = 'Texto'.
        l_fieldcat-seltext_s = 'Texto'.
        l_fieldcat-reptext_ddic = 'Texto'.
        l_fieldcat-col_pos = '6'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'ERDAT_SSCC'.
        l_fieldcat-outputlen = 8.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Data Ent.SSCC'.
        l_fieldcat-seltext_m = 'Data Ent.SSCC'.
        l_fieldcat-seltext_s = 'Data Ent.SSCC'.
        l_fieldcat-reptext_ddic = 'Data Ent.SSCC'.
        l_fieldcat-col_pos = '7'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'ERUHR_SSCC'.
        l_fieldcat-outputlen = 8.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Hora Ent.SSCC'.
        l_fieldcat-seltext_m = 'Hora Ent.SSCC'.
        l_fieldcat-seltext_s = 'Hora Ent.SSCC'.
        l_fieldcat-reptext_ddic = 'Hora Ent.SSCC'.
        l_fieldcat-col_pos = '8'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'LGTYP'.
        l_fieldcat-outputlen = 3.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Tipo Dep.'.
        l_fieldcat-seltext_m = 'Tipo Dep.'.
        l_fieldcat-seltext_s = 'Tipo Dep.'.
        l_fieldcat-reptext_ddic = 'Tipo Dep.'.
        l_fieldcat-col_pos = '9'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'LGPLA'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Posição'.
        l_fieldcat-seltext_m = 'Posição'.
        l_fieldcat-seltext_s = 'Posição'.
        l_fieldcat-reptext_ddic = 'Posição'.
        l_fieldcat-col_pos = '10'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'BDATU_SAI'.
        l_fieldcat-outputlen = 8.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Data Saida'.
        l_fieldcat-seltext_m = 'Data Saida'.
        l_fieldcat-seltext_s = 'Data Saida'.
        l_fieldcat-reptext_ddic = 'Data Saida'.
        l_fieldcat-col_pos = '11'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

*      WHEN 'BZEIT_SAI'.
*        l_fieldcat-outputlen = 8.
*        l_fieldcat-just = 'L'.
*        l_fieldcat-no_sum = 'X'.
*        l_fieldcat-do_sum = ' '.
*        l_fieldcat-key = ' '.
*        l_fieldcat-seltext_l = 'Hora Saida'.
*        l_fieldcat-seltext_m = 'Hora Saida'.
*        l_fieldcat-seltext_s = 'Hora Saida'.
*        l_fieldcat-reptext_ddic = 'Hora Saida'.
*        l_fieldcat-col_pos = '12'.
*        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'VBELN'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Guia'.
        l_fieldcat-seltext_m = 'Guia'.
        l_fieldcat-seltext_s = 'Guia'.
        l_fieldcat-reptext_ddic = 'Guia'.
        l_fieldcat-col_pos = '12'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'VEMNG'.
        l_fieldcat-outputlen = 15.
        l_fieldcat-just = 'R'.
        l_fieldcat-no_sum = ' '.
        l_fieldcat-do_sum = 'X'.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Quant.Pal.'.
        l_fieldcat-seltext_m = 'Quant.Pal.'.
        l_fieldcat-seltext_s = 'Quant.Pal.'.
        l_fieldcat-reptext_ddic = 'Quant.Pal.'.
        l_fieldcat-col_pos = '13'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'KUNNR'.
        l_fieldcat-outputlen = 10.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Recebedor'.
        l_fieldcat-seltext_m = 'Recebedor'.
        l_fieldcat-seltext_s = 'Recebedor'.
        l_fieldcat-reptext_ddic = 'Recebedor'.
        l_fieldcat-col_pos = '14'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN 'NOME'.
        l_fieldcat-outputlen = 35.
        l_fieldcat-just = 'L'.
        l_fieldcat-no_sum = 'X'.
        l_fieldcat-do_sum = ' '.
        l_fieldcat-key = ' '.
        l_fieldcat-seltext_l = 'Nome Recebedor'.
        l_fieldcat-seltext_m = 'Nome Recebedor'.
        l_fieldcat-seltext_s = 'Nome Recebedor'.
        l_fieldcat-reptext_ddic = 'Nome Recebedor'.
        l_fieldcat-col_pos = '15'.
        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.
*
*      WHEN 'ERUHR_PIK'.
*        l_fieldcat-outputlen = 8.
*        l_fieldcat-just = 'L'.
*        l_fieldcat-no_sum = 'X'.
*        l_fieldcat-do_sum = ' '.
*        l_fieldcat-key = ' '.
*        l_fieldcat-seltext_l = 'Hora Picking'.
*        l_fieldcat-seltext_m = 'Hora Picking'.
*        l_fieldcat-seltext_s = 'Hora Picking'.
*        l_fieldcat-reptext_ddic = 'Hora Picking'.
*        l_fieldcat-col_pos = '16'.
*        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

*      WHEN 'VEMNG'.
*        l_fieldcat-outputlen = 15.
*        l_fieldcat-just = 'R'.
*        l_fieldcat-no_sum = ' '.
*        l_fieldcat-do_sum = 'X'.
*        l_fieldcat-key = ' '.
*        l_fieldcat-seltext_l = 'Quant.Pal.'.
*        l_fieldcat-seltext_m = 'Quant.Pal.'.
*        l_fieldcat-seltext_s = 'Quant.Pal.'.
*        l_fieldcat-reptext_ddic = 'Quant.Pal.'.
*        l_fieldcat-col_pos = '15'.
*        MODIFY p_it_fieldcat FROM l_fieldcat INDEX sy-tabix.

      WHEN OTHERS.
        DELETE p_it_fieldcat INDEX sy-tabix.

    ENDCASE.
  ENDLOOP.

ENDFORM.                    " get_campos
*&---------------------------------------------------------------------*
*&      Form  get_layout_sort
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_LAYOUT  text
*      <--P_SORT  text
*----------------------------------------------------------------------*
FORM get_layout_sort  CHANGING p_layout TYPE slis_layout_alv
                                  p_sort TYPE slis_t_sortinfo_alv.

  DATA l_sort TYPE slis_sortinfo_alv.

* Layout
  p_layout-get_selinfos = 'X'.
  p_layout-key_hotspot = 'X'.
  p_layout-totals_before_items = ' '.
  p_layout-group_change_edit = 'X'.
  p_layout-detail_popup = 'X'.
  p_layout-zebra = 'X'.

* Ordenação

  REFRESH p_sort.

  l_sort-spos = 1.
  l_sort-fieldname = 'TIPO'.
  l_sort-up = 'X'.
  l_sort-subtot = 'X'.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

*  l_sort-spos = 2.
*  l_sort-fieldname = 'EXIDV'.
*  l_sort-up = 'X'.
*  l_sort-subtot = 'X'.
*  l_sort-comp = ' '.
*  l_sort-expa = ' '.
*  APPEND l_sort TO p_sort.


  l_sort-spos = 2.
  l_sort-fieldname = 'ERDAT_SSCC'.
  l_sort-up = 'X'.
  l_sort-subtot = ' '.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.

  l_sort-spos = 3.
  l_sort-fieldname = 'ERDAT_SSCC'.
  l_sort-up = 'X'.
  l_sort-subtot = ' '.
  l_sort-comp = ' '.
  l_sort-expa = ' '.
  APPEND l_sort TO p_sort.



ENDFORM.                    " get_layout_sort
*&---------------------------------------------------------------------*
*&      Form  imprime_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM imprime_dados .

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
  EXPORTING
*         I_INTERFACE_CHECK        = ' '
          i_callback_program       = repid
*            I_CALLBACK_PF_STATUS_SET = 'STATUS'         "Nome SUBROTINA
*          i_callback_user_command  = 'USER_COMMAND'   "Nome SUBROTINA
          i_callback_top_of_page  = 'TOP_OF_PAGE'
*         I_STRUCTURE_NAME         =
          is_layout                = layout
          it_fieldcat              = it_fieldcat       " esta
          i_background_id         = 'ALV_BACKGROUND'
*         IT_EXCLUDING             =
*         IT_SPECIAL_GROUPS        =
            it_sort                  = sort
*         IT_FILTER                =
*         IS_SEL_HIDE              =
*         I_DEFAULT                = 'X'
*         I_SAVE                   = ' '
*         IS_VARIANT               = VARIANT
***            it_events                = events
*         IT_EVENT_EXIT            =
*         IS_PRINT                 =
*         IS_REPREP_ID             =
*         I_SCREEN_START_COLUMN    = 0
*          i_screen_start_line      = 2
*         I_SCREEN_END_COLUMN      = 0
*         I_SCREEN_END_LINE        = 0
*    IMPORTING
*         E_EXIT_CAUSED_BY_CALLER  =
*         ES_EXIT_CAUSED_BY_USER   =
  TABLES
            t_outtab                 =  tabi[]
  EXCEPTIONS
            program_error            = 1
            OTHERS                   = 2.

ENDFORM.                    " imprime_dados
*&---------------------------------------------------------------------*
*&      Form  nome_recebedor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TABI_KUNNR  text
*      -->P_TABI_NOME  text
*----------------------------------------------------------------------*
FORM nome_recebedor  USING    p_vkorg
                              p_vbeln
                              p_kunnr
                              p_nome.

  IF p_vkorg = 'RP00' OR p_vkorg = 'RP05' OR
     p_vkorg = 'RPB0'.

    SELECT SINGLE adrnr kunnr INTO (vbpa-adrnr, vbpa-kunnr)
                 FROM vbpa WHERE vbeln = p_vbeln
                           AND   posnr = '000000'
                           AND   parvw = 'W1'.
  ELSE.
    SELECT SINGLE adrnr kunnr INTO (vbpa-adrnr, vbpa-kunnr)
                 FROM vbpa WHERE vbeln = p_vbeln
                           AND   posnr = '000000'
                           AND   parvw = 'WE'.
  ENDIF.

  SELECT SINGLE name1 INTO (adrc-name1) FROM adrc
         WHERE addrnumber = vbpa-adrnr.
  IF sy-subrc = 0.
    p_nome = adrc-name1.
  ENDIF.

ENDFORM.                    " nome_recebedor
*&---------------------------------------------------------------------*
*&       FORM TOP_OF_PAGE                                              *
*&---------------------------------------------------------------------*
FORM top_of_page.
* Para criar um logotipo, deve-se entrar na transação 0FPM002 e
* preencher:
* - Classe = PICTURES
* - Objeto = OT
* - Item   = Nome do ID da figura

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = it_header
      i_logo             = 'RENOVA_LOGO'.

*  V_LINNO = SY-LINNO.
*
*  SKIP TO LINE 1.
*  WRITE: AT 70  'Página:', SY-PAGNO.
*
*  SKIP TO LINE V_LINNO.
ENDFORM.                    "top_of_page
*&---------------------------------------------------------------------*
*&      Form  REGISTO_ABERTO_GRUPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM registo_aberto_grupo USING p_zmatnr.

  CLEAR zmaispik.

  pi_tabi-tipo = '3'.
  pi_tabi-texto = 'PICK'.

  SELECT zwm026~armazem zwm026~estado zwm026~grupo zwm026~material
         zwm026~pack_material zwm026~pal_destino zwm026~quantidade
        zwm026~remessa zwm026~sscc zwm026~to_number zwm026~unidade
         zwm026~user_name zwm026~lote ltap~edatu ltap~ezeit
         ltap~lgnum ltap~matnr ltap~tanum vekp~erdat vekp~eruhr
         vekp~exidv
      INTO (zwm026-armazem , zwm026-estado , zwm026-grupo ,
            zwm026-material ,zwm026-pack_material ,
            zwm026-pal_destino ,
            zwm026-quantidade , zwm026-remessa , zwm026-sscc ,
            zwm026-to_number , zwm026-unidade , zwm026-user_name ,
            zwm026-lote , ltap-edatu, ltap-ezeit , ltap-lgnum ,
            ltap-matnr , ltap-tanum , vekp-erdat ,vekp-eruhr ,
            vekp-exidv )
     FROM ( zwm026
            INNER JOIN ltap
            ON ltap~lgnum = zwm026~armazem
            AND ltap~tanum = zwm026~to_number
            INNER JOIN vekp
            ON vekp~exidv = zwm026~sscc )
            WHERE
                  zwm026~grupo = tabi-benum
              AND zwm026~material = p_zmatnr
             ORDER BY vekp~erdat vekp~eruhr.

****
* verifica se existe já algum sscc de pik e remessa, então exclui
* pedido goncalo guterres 22/10/08
    READ TABLE px_tabi WITH KEY
                                matnr = zwm026-material
                                exidv_pik = zwm026-sscc
                                vbeln = zwm026-remessa.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
****
    ztotpik = ztotpik + zwm026-quantidade.
    IF ztotpik >= ztotrep.
      zmaispik = zmaispik + 1.
      IF zmaispik > 1.
        EXIT.
      ENDIF.
    ENDIF.
****

    pi_tabi-matnr = zwm026-material.
    pi_tabi-vemng = zwm026-quantidade.
*     coloca numero do sscc de produção
*      retirado a pedido do Goncalo Guterres em 21/10/08
*    pi_tabi-exidv = tabi-exidv.
    pi_tabi-exidv = ' '.
*
    pi_tabi-exidv_pik = zwm026-sscc.
    pi_tabi-charg = zwm026-lote.
    pi_tabi-vbeln = zwm026-remessa.
    pi_tabi-nome = ' '.
    pi_tabi-erdat_sscc = vekp-erdat.
    pi_tabi-eruhr_sscc = vekp-eruhr.
*    pi_tabi-erdat_sscc = ltap-edatu.
*    pi_tabi-eruhr_sscc = ltap-ezeit.
    SELECT SINGLE * FROM likp WHERE vbeln = zwm026-remessa.
    IF sy-subrc = 0.
      pi_tabi-kunnr = likp-kunnr.
      pi_tabi-bdatu_sai = likp-wadat_ist.
      PERFORM nome_recebedor USING likp-vkorg
                                   pi_tabi-vbeln
                                   pi_tabi-kunnr
                                   pi_tabi-nome.
    ENDIF.
*********
*     fazer somatorio por chave exidv exidv_pik,remessa
*********
    READ TABLE pi_tabi WITH KEY exidv = tabi-exidv
                                matnr = zwm026-material
                                exidv_pik = zwm026-sscc
                                vbeln = zwm026-remessa.
    IF sy-subrc = 0.
      pi_tabi-vemng = pi_tabi-vemng + zwm026-quantidade.
      MODIFY pi_tabi INDEX sy-tabix.
    ELSE.
      APPEND pi_tabi.
    ENDIF.
*    ztotpik = ztotpik + zwm026-quantidade.
  ENDSELECT.
  IF sy-subrc = 0.
    IF pi_tabi[] IS NOT INITIAL.
      zeruhr_sscc = pi_tabi-eruhr_sscc.
      zerdat_sscc = pi_tabi-erdat_sscc.
    ENDIF.
  else.
****  quando são restos de produção e vem directamente dos tapetes
****   da produção
    IF tabi-benum = '000-000-01' OR tabi-benum IS INITIAL.
      zeruhr_sscc = tabi-eruhr_sscc.
      zerdat_sscc = tabi-erdat_sscc.
*      p_fl_sai = 0.
*    ELSE.
*      p_fl_sai = sy-subrc.
    ENDIF.
  ENDIF.

*****  SORT pi_tabi BY erdat_sscc eruhr_sscc.

  APPEND LINES OF pi_tabi TO px_tabi.
  REFRESH pi_tabi.
  CLEAR pi_tabi.
ENDFORM.                    " REGISTO_ABERTO_GRUPO
*&---------------------------------------------------------------------*
*&      Form  LE_HISTORICO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM le_historico USING p_fl_sai
                        p_zmatnr.

  SELECT zwm026h~armazem zwm026h~estado zwm026h~grupo zwm026h~material
         zwm026h~pack_material zwm026h~pal_destino zwm026h~quantidade
        zwm026h~remessa zwm026h~sscc zwm026h~to_number zwm026h~unidade
         zwm026h~user_name zwm026h~lote ltap~edatu ltap~ezeit
         ltap~lgnum ltap~matnr ltap~tanum vekp~erdat vekp~eruhr
         vekp~exidv
      INTO (zwm026h-armazem , zwm026h-estado , zwm026h-grupo ,
            zwm026h-material ,zwm026h-pack_material ,
            zwm026h-pal_destino ,
            zwm026h-quantidade , zwm026h-remessa , zwm026h-sscc ,
            zwm026h-to_number , zwm026h-unidade , zwm026h-user_name ,
            zwm026h-lote , ltap-edatu, ltap-ezeit , ltap-lgnum ,
            ltap-matnr , ltap-tanum , vekp-erdat ,vekp-eruhr ,
            vekp-exidv )
     FROM ( zwm026h
            INNER JOIN ltap
            ON ltap~lgnum = zwm026h~armazem
            AND ltap~tanum = zwm026h~to_number
            INNER JOIN vekp
            ON vekp~exidv = zwm026h~sscc )
            WHERE
                  zwm026h~grupo = tabi-benum
              AND zwm026h~material = p_zmatnr
             ORDER BY vekp~erdat vekp~eruhr.


****
* verifica se existe já algum sscc de pik e remessa, então exclui
* pedido goncalo guterres 22/10/08
    READ TABLE px_tabi WITH KEY
                                matnr = zwm026h-material
                                exidv_pik = zwm026h-sscc
                                vbeln = zwm026h-remessa.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
****
    ztotpik = ztotpik + zwm026h-quantidade.
    IF ztotpik >= ztotrep.
      zmaispik = zmaispik + 1.
      IF zmaispik > 1.
        EXIT.
      ENDIF.
    ENDIF.

    pi_tabi-matnr = zwm026h-material.
    pi_tabi-vemng = zwm026h-quantidade.
*     coloca numero do sscc de produção
*      retirado a pedido do Goncalo Guterres em 21/10/08
*    pi_tabi-exidv = tabi-exidv.
    pi_tabi-exidv = ' '.
*
    pi_tabi-exidv_pik = zwm026h-sscc.
    pi_tabi-charg = zwm026h-lote.
    pi_tabi-vbeln = zwm026h-remessa.
    pi_tabi-nome = ' '.
*    pi_tabi-erdat_sscc = ltap-edatu.
*    pi_tabi-eruhr_sscc = ltap-ezeit.
    pi_tabi-erdat_sscc = vekp-erdat.
    pi_tabi-eruhr_sscc = vekp-eruhr.
    SELECT SINGLE * FROM likp WHERE vbeln = zwm026h-remessa.
    IF sy-subrc = 0.
      pi_tabi-kunnr = likp-kunnr.
      pi_tabi-bdatu_sai = likp-wadat_ist.
      PERFORM nome_recebedor USING likp-vkorg
                                   pi_tabi-vbeln
                                   pi_tabi-kunnr
                                   pi_tabi-nome.
    ENDIF.
*********
*     fazer somatorio por chave exidv exidv_pik,remessa
*********
    READ TABLE pi_tabi WITH KEY exidv = tabi-exidv
                                matnr = zwm026h-material
                                exidv_pik = zwm026h-sscc
                                vbeln = zwm026h-remessa.
    IF sy-subrc = 0.
      pi_tabi-vemng = pi_tabi-vemng + zwm026h-quantidade.
      MODIFY pi_tabi INDEX sy-tabix.
    ELSE.
      APPEND pi_tabi.
    ENDIF.
*    ztotpik = ztotpik + zwm026h-quantidade.
  ENDSELECT.
  IF sy-subrc NE 0.
****  quando são restos de produção e vem directamente dos tapetes
****   da produção
    IF tabi-benum = '000-000-01' OR tabi-benum IS INITIAL.
      zeruhr_sscc = tabi-eruhr_sscc.
      zerdat_sscc = tabi-erdat_sscc.
      p_fl_sai = 0.
    ELSE.
      p_fl_sai = sy-subrc.
    ENDIF.
  ELSE.
*     guarda o dia e a hora do ultimo sscc de Picking
    IF pi_tabi-erdat_sscc IS INITIAL.
      zeruhr_sscc = tabi-eruhr_sscc.
      zerdat_sscc = tabi-erdat_sscc.
    ELSE.
      zeruhr_sscc = pi_tabi-eruhr_sscc.
      zerdat_sscc = pi_tabi-erdat_sscc.
    ENDIF.
    APPEND LINES OF pi_tabi TO px_tabi.

  ENDIF.

ENDFORM.                    " LE_HISTORICO
*&---------------------------------------------------------------------*
*&      Form  LE_HISTORICO2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM le_historico2 .

  REFRESH p_tabi.
  CLEAR: p_tabi,
         zmaispik.

  SELECT zwm026h~armazem zwm026h~material zwm026h~lote
         zwm026h~quantidade
         zwm026h~remessa zwm026h~sscc zwm026h~to_number
         ltap~edatu ltap~ezeit
         vekp~erdat vekp~eruhr vekp~exidv
  INTO (zwm026h-armazem , zwm026h-material , zwm026h-lote ,
        zwm026h-quantidade , zwm026h-remessa , zwm026h-sscc ,
        zwm026h-to_number ,ltap-edatu, ltap-ezeit ,
        vekp-erdat , vekp-eruhr ,
        vekp-exidv )
  FROM ( zwm026h
       INNER JOIN ltap
       ON ltap~lgnum = zwm026h~armazem
       AND ltap~tanum = zwm026h~to_number
         INNER JOIN vekp
         ON vekp~exidv = zwm026h~sscc )
         WHERE zwm026h~material = tabi-matnr
             AND  zwm026h~sscc NE tabi-exidv_pik
                 AND   vekp~erdat >= zerdat_sscc
*             AND   ltap~edatu >= zerdat_sscc
             ORDER BY vekp~erdat vekp~eruhr.
*         ORDER BY ltap~edatu ltap~ezeit.

*     excluir horas anteriores á hora do ultimo sscc
*        IF vekp-erdat = tabi-erdat_sscc.
*    IF ltap-edatu = zerdat_sscc.
    IF vekp-erdat = zerdat_sscc.
*          IF vekp-eruhr < tabi-eruhr_sscc.
*      IF ltap-ezeit < zeruhr_sscc.
      IF vekp-eruhr < zeruhr_sscc.
        CONTINUE.
      ENDIF.
    ENDIF.

*     verifica se anteriormente já apanhou o mesmo registo
    READ TABLE px_tabi WITH KEY matnr = zwm026h-material
                            exidv_pik = zwm026h-sscc
                                vbeln = zwm026h-remessa.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
*
    ztotpik = ztotpik + zwm026h-quantidade.
    IF ztotpik >= ztotrep.
      zmaispik = zmaispik + 1.
      IF zmaispik > 1.
        EXIT.
      ENDIF.
    ENDIF.
*        ELSE.
    p_tabi-tipo = '3'.
    p_tabi-texto = 'PICK'.
    p_tabi-matnr = zwm026h-material.
    p_tabi-vemng = zwm026h-quantidade.
*      retirado a pedido do Goncalo Guterres em 21/10/08
*    p_tabi-exidv = tabi-exidv.
    p_tabi-exidv = ' '.
*
    p_tabi-exidv_pik = zwm026h-sscc.
    p_tabi-charg = zwm026h-lote.
    p_tabi-vbeln = zwm026h-remessa.
    p_tabi-erdat_sscc = vekp-erdat.
    p_tabi-eruhr_sscc = vekp-eruhr.
*    p_tabi-erdat_sscc = ltap-edatu.
*    p_tabi-eruhr_sscc = ltap-ezeit.
    p_tabi-nome = ' '.
    SELECT SINGLE * FROM likp WHERE vbeln = p_tabi-vbeln.
    IF sy-subrc = 0.
      p_tabi-kunnr = likp-kunnr.
      p_tabi-bdatu_sai = likp-wadat_ist.
      PERFORM nome_recebedor USING likp-vkorg
                                   p_tabi-vbeln
                                   p_tabi-kunnr
                                   p_tabi-nome.
    ENDIF.
*********
*     fazer somatorio por chave matnr exidv_pik,remessa
*********
    READ TABLE p_tabi WITH KEY matnr = zwm026h-material
                                exidv_pik = zwm026h-sscc
                                vbeln = zwm026h-remessa.
    IF sy-subrc = 0.
      p_tabi-vemng = p_tabi-vemng + zwm026h-quantidade.
      MODIFY p_tabi INDEX sy-tabix.
    ELSE.
      APPEND p_tabi.
    ENDIF.

  ENDSELECT.
  IF sy-subrc = 0.
    IF p_tabi-erdat_sscc IS INITIAL.
      zeruhr_sscc = tabi-eruhr_sscc.
      zerdat_sscc = tabi-erdat_sscc.
    ELSE.
*     guarda o dia e a hora do ultimo sscc de Picking
      zeruhr_sscc = p_tabi-eruhr_sscc.
      zerdat_sscc = p_tabi-erdat_sscc.
    ENDIF.
  ENDIF.

*******  SORT p_tabi BY erdat_sscc eruhr_sscc.

  APPEND LINES OF p_tabi TO px_tabi.
  REFRESH p_tabi.
  CLEAR p_tabi.
ENDFORM.                    " LE_HISTORICO2
*&---------------------------------------------------------------------*
*&      Form  REGISTO_ABERTO_SEMGRUPO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM registo_aberto_semgrupo .

  REFRESH p_tabi.
  CLEAR: p_tabi,
        zmaispik.

  SELECT zwm026~armazem zwm026~material zwm026~lote
         zwm026~quantidade
         zwm026~remessa zwm026~sscc zwm026~to_number
         ltap~edatu ltap~ezeit
         vekp~erdat vekp~eruhr vekp~exidv
  INTO (zwm026-armazem , zwm026-material , zwm026-lote ,
        zwm026-quantidade , zwm026-remessa , zwm026-sscc ,
        zwm026-to_number ,ltap-edatu, ltap-ezeit ,
        vekp-erdat , vekp-eruhr ,
        vekp-exidv )
  FROM ( zwm026
       INNER JOIN ltap
       ON ltap~lgnum = zwm026~armazem
       AND ltap~tanum = zwm026~to_number
         INNER JOIN vekp
         ON vekp~exidv = zwm026~sscc )
         WHERE zwm026~material = tabi-matnr
             AND  zwm026~sscc NE tabi-exidv_pik
             AND   vekp~erdat >= zerdat_sscc
*             AND   ltap~edatu >= zerdat_sscc
             ORDER BY vekp~erdat vekp~eruhr.
*         ORDER BY ltap~edatu ltap~ezeit.

*     excluir horas anteriores á hora do ultimo sscc
*        IF vekp-erdat = tabi-erdat_sscc.
*    IF ltap-edatu = zerdat_sscc.
    IF vekp-erdat = zerdat_sscc.
*          IF vekp-eruhr < tabi-eruhr_sscc.
*      IF ltap-ezeit < zeruhr_sscc.
      IF vekp-eruhr < zeruhr_sscc.
        CONTINUE.
      ENDIF.
    ENDIF.
*     verifica se anteriormente já apanhou o mesmo registo
    READ TABLE px_tabi WITH KEY matnr = zwm026-material
                            exidv_pik = zwm026-sscc
                                vbeln = zwm026-remessa.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
*
    ztotpik = ztotpik + zwm026-quantidade.
    IF ztotpik >= ztotrep.
      zmaispik = zmaispik + 1.
      IF zmaispik > 1.
        EXIT.
      ENDIF.
    ENDIF.

    p_tabi-tipo = '3'.
    p_tabi-texto = 'PICK'.
    p_tabi-matnr = zwm026-material.
    p_tabi-vemng = zwm026-quantidade.
*      retirado a pedido do Goncalo Guterres em 21/10/08
*    p_tabi-exidv = tabi-exidv.
    p_tabi-exidv = ' '.
*
    p_tabi-exidv_pik = zwm026-sscc.
    p_tabi-charg = zwm026-lote.
    p_tabi-vbeln = zwm026-remessa.
    p_tabi-erdat_sscc = vekp-erdat.
    p_tabi-eruhr_sscc = vekp-eruhr.
*    p_tabi-erdat_sscc = ltap-edatu.
*    p_tabi-eruhr_sscc = ltap-ezeit.
    p_tabi-nome = ' '.
    SELECT SINGLE * FROM likp WHERE vbeln = p_tabi-vbeln.
    IF sy-subrc = 0.
      p_tabi-kunnr = likp-kunnr.
      p_tabi-bdatu_sai = likp-wadat_ist.
      PERFORM nome_recebedor USING likp-vkorg
                                   p_tabi-vbeln
                                   p_tabi-kunnr
                                   p_tabi-nome.
    ENDIF.
*********
*     fazer somatorio por chave matnr exidv_pik,remessa
*********
    READ TABLE p_tabi WITH KEY matnr = zwm026-material
                                exidv_pik = zwm026-sscc
                                vbeln = zwm026-remessa.
    IF sy-subrc = 0.
      p_tabi-vemng = p_tabi-vemng + zwm026-quantidade.
      MODIFY p_tabi INDEX sy-tabix.
    ELSE.
      APPEND p_tabi.
    ENDIF.
  ENDSELECT.
  IF sy-subrc = 0.
*     guarda o dia e a hora do ultimo sscc de Picking
    zeruhr_sscc = p_tabi-eruhr_sscc.
    zerdat_sscc = p_tabi-erdat_sscc.
  ENDIF.

*******  SORT p_tabi BY erdat_sscc eruhr_sscc.

  APPEND LINES OF p_tabi TO px_tabi.
  REFRESH p_tabi.
  CLEAR p_tabi.

ENDFORM.                    " REGISTO_ABERTO_SEMGRUPO
*&---------------------------------------------------------------------*
*&      Form  REGISTO_ABERTO2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM registo_aberto2 .
  REFRESH p_tabi.
  CLEAR: p_tabi,
         zmaispik.

  SELECT zwm026~armazem zwm026~material zwm026~lote
         zwm026~quantidade
         zwm026~remessa zwm026~sscc zwm026~to_number
         ltap~edatu ltap~ezeit
         vekp~erdat vekp~eruhr vekp~exidv
  INTO (zwm026-armazem , zwm026-material , zwm026-lote ,
        zwm026-quantidade , zwm026-remessa , zwm026-sscc ,
        zwm026-to_number ,ltap-edatu, ltap-ezeit ,
        vekp-erdat , vekp-eruhr ,
        vekp-exidv )
  FROM ( zwm026
       INNER JOIN ltap
       ON ltap~lgnum = zwm026~armazem
       AND ltap~tanum = zwm026~to_number
         INNER JOIN vekp
         ON vekp~exidv = zwm026~sscc )
         WHERE zwm026~material = tabi-matnr
             AND  zwm026~sscc NE tabi-exidv_pik
             AND   vekp~erdat >= zerdat_sscc
*             AND   ltap~edatu >= zerdat_sscc
             ORDER BY vekp~erdat vekp~eruhr.
*         ORDER BY ltap~edatu ltap~ezeit.

*     excluir horas anteriores á hora do ultimo sscc
*        IF vekp-erdat = tabi-erdat_sscc.
*    IF ltap-edatu = zerdat_sscc.
    IF vekp-erdat = zerdat_sscc.
*          IF vekp-eruhr < tabi-eruhr_sscc.
*      IF ltap-ezeit < zeruhr_sscc.
      IF vekp-eruhr < zeruhr_sscc.
        CONTINUE.
      ENDIF.
    ENDIF.
*     verifica se anteriormente já apanhou o mesmo registo
    READ TABLE px_tabi WITH KEY matnr = zwm026-material
                            exidv_pik = zwm026-sscc
                                vbeln = zwm026-remessa.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.
*
    ztotpik = ztotpik + zwm026-quantidade.
    IF ztotpik >= ztotrep.
      zmaispik = zmaispik + 1.
      IF zmaispik > 1.
        EXIT.
      ENDIF.
    ENDIF.
*        ELSE.
    p_tabi-tipo = '3'.
    p_tabi-texto = 'PICK'.
    p_tabi-matnr = zwm026-material.
    p_tabi-vemng = zwm026-quantidade.
*      retirado a pedido do Goncalo Guterres em 21/10/08
*    p_tabi-exidv = tabi-exidv.
    p_tabi-exidv = ' '.
*
    p_tabi-exidv_pik = zwm026-sscc.
    p_tabi-charg = zwm026-lote.
    p_tabi-vbeln = zwm026-remessa.
    p_tabi-erdat_sscc = vekp-erdat.
    p_tabi-eruhr_sscc = vekp-eruhr.
*    p_tabi-erdat_sscc = ltap-edatu.
*    p_tabi-eruhr_sscc = ltap-ezeit.
    p_tabi-nome = ' '.
    SELECT SINGLE * FROM likp WHERE vbeln = p_tabi-vbeln.
    IF sy-subrc = 0.
      p_tabi-kunnr = likp-kunnr.
      p_tabi-bdatu_sai = likp-wadat_ist.
      PERFORM nome_recebedor USING likp-vkorg
                                   p_tabi-vbeln
                                   p_tabi-kunnr
                                   p_tabi-nome.
    ENDIF.
*********
*     fazer somatorio por chave matnr exidv_pik,remessa
*********
    READ TABLE p_tabi WITH KEY matnr = zwm026-material
                                exidv_pik = zwm026-sscc
                                vbeln = zwm026-remessa.
    IF sy-subrc = 0.
      p_tabi-vemng = p_tabi-vemng + zwm026-quantidade.
      MODIFY p_tabi INDEX sy-tabix.
    ELSE.
      APPEND p_tabi.
    ENDIF.

  ENDSELECT.
  IF sy-subrc = 0.
*     guarda o dia e a hora do ultimo sscc de Picking
    zeruhr_sscc = p_tabi-eruhr_sscc.
    zerdat_sscc = p_tabi-erdat_sscc.
  ENDIF.

*****  SORT p_tabi BY erdat_sscc eruhr_sscc.

  APPEND LINES OF p_tabi TO px_tabi.
  REFRESH p_tabi.
  CLEAR p_tabi.
ENDFORM.                    " REGISTO_ABERTO2
