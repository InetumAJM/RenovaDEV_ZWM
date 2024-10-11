
*-----------------------------------------------------------------------
*      Report-Header / Tabellen / Daten
*-----------------------------------------------------------------------

REPORT rlsamm10 MESSAGE-ID l5
     NO STANDARD PAGE HEADING.

TYPE-POOLS: abap.

INCLUDE zwm_constants.

*-----------------------------------------------------------------------
*      Tabellen
*-----------------------------------------------------------------------
TABLES: t311,                   "Referenznummern
        t311a,                 "Belege zu Referenznummern
        ltak,                  "Transportauftrag Kopf
        ltap,                  "Transportauftrag Position
        lqua,
        mlgt,
        mlgn,
        mara,
        lcoms,                 "Kommunikationsstruktur Sammelgang
        rl05s,                 "E/A-Tabelle
        zwm001,
        zwm007,
        zwm020,
        zwm026,
        zwm028,
        marm,
        lagp,
        vbss,
        vbuk.

TABLES: t300t,                  "Lagernummertext
        t342.                  "FCODE-Tabelle


*......Tabelle der selektierten Referenznummern........................
DATA: BEGIN OF it311 OCCURS 1.
        INCLUDE STRUCTURE t311.
        DATA:   kreuz    TYPE c,                       "Selektionsparameter
        liefn,                              "Ref.nummer zu Tranp.bedarf
        tbedn,                              "Ref.nummer zu Lieferung
        k_ers(1),                           "KZ Refnr erstellt
        k_off(1),                           "KZ Refnr offen/fehlerhaft
        k_erl(1),                           "KZ Refnr erl. (TA kompl)
        k_fre(1),                           "KZ Refnr freigegeben
        k_tao    LIKE rl05s-offta,             "KZ Transportaufträge offen
        linno    LIKE sy-linno,
        pagno    LIKE sy-pagno.
DATA: END OF it311.

DATA: BEGIN OF sav_it311 OCCURS 0.
        INCLUDE STRUCTURE t311.
        DATA:   kreuz    TYPE c,                       "Selektionsparameter
        liefn,                              "Ref.nummer zu Tranp.bedarf
        tbedn,                              "Ref.nummer zu Lieferung
        k_ers(1),                           "KZ Refnr erstellt
        k_off(1),                           "KZ Refnr offen/fehlerhaft
        k_erl(1),                           "KZ Refnr erl. (TA kompl)
        k_fre(1),                           "KZ Refnr freigegeben
        k_tao(1),                           "KZ Transportaufträge offen
        linno    LIKE sy-linno,
        pagno    LIKE sy-pagno.
DATA: END OF sav_it311.

*......Tabelle der Belege zur Referenznummer...........................
DATA: BEGIN OF it311a OCCURS 0.
        INCLUDE STRUCTURE t311a.
        DATA:   kreuz TYPE c,                       "Selektionsparameter
        tanum LIKE ltak-tanum,              "TA-Nummer
        linno LIKE sy-linno,
        pagno LIKE sy-pagno.
DATA: END OF it311a.

DATA: BEGIN OF sav_it311a OCCURS 0.
        INCLUDE STRUCTURE t311a.
        DATA:   kreuz TYPE c,                       "Selektionsparameter
        tanum LIKE ltak-tanum,              "TA-Nummer
        linno LIKE sy-linno,
        pagno LIKE sy-pagno.
DATA: END OF sav_it311a.

*......Interne Tabelle zur Summierung über die Belegtypen..............
DATA: BEGIN OF sum00 OCCURS 0,
        sort(1)   TYPE c,                   "SORTIERKENNZEICHEN
        rtext(18),                          "Belegtyptext(->Textelement)
        rbtyp     LIKE t311-rbtyp,              "Referenzbelegtyp
        r_ers(4)  TYPE p,                   "Refnr erstellt
        r_off(4)  TYPE p,                   "Refnr offen/fehlerhaft
        r_erl(4)  TYPE p,                   "Refnr erledigt (TA kompl)
        r_fre(4)  TYPE p,                   "Refnr freigegeben
        r_lin(4)  TYPE p,                   "Zeilensumme (line)
        r_tao(4)  TYPE p.                   "Transportaufträge offen
DATA: END OF sum00.

DATA: BEGIN OF sum01,
        sort(1)   TYPE c,                   "SORTIERKENNZEICHEN
        rtext(18),                          "Belegtyptext(->Textelement)
        rbtyp     LIKE t311-rbtyp,              "Referenzbelegtyp
        r_ers(4)  TYPE p,                   "Refnr erstellt
        r_off(4)  TYPE p,                   "Refnr offen/fehlerhaft
        r_erl(4)  TYPE p,                   "Refnr erledigt (TA kompl)
        r_fre(4)  TYPE p,                   "Refnr freigegeben
        r_lin(4)  TYPE p,                   "Zeilensumme (line)
        r_tao(4)  TYPE p.                   "Transportaufträge offen
DATA: END OF sum01.

*........Tabelle Transportauftragsköpfe.................................
DATA: BEGIN OF iltak OCCURS 0,
        lgnum LIKE ltak-lgnum,
        refnr LIKE ltak-refnr,
        tanum LIKE ltak-tanum,
        kquit LIKE ltak-kquit,
      END OF iltak.

*........Tabelle zur Übergabe der Referenzbelege an Fkt.baustein........
DATA: BEGIN OF t_t311a OCCURS 0.
        INCLUDE STRUCTURE t311a.
      DATA: END OF t_t311a.

*........Tabelle zur Übergabe der Referennummer  an Fkt.baustein........
DATA: BEGIN OF t_t311 OCCURS 0.
        INCLUDE STRUCTURE t311.
      DATA: END OF t_t311.

*........Text-Zeilen in SAPscript-Format...............................*

DATA: BEGIN OF llines OCCURS 100.
        INCLUDE STRUCTURE tline.
      DATA: END OF llines.

*........Struktur des Dokuobjektes für Langtext........................*

DATA: BEGIN OF doku_na,
        msgid LIKE sy-msgid,
        msgno LIKE sy-msgno,
      END   OF doku_na.

DATA: con_doku_na     LIKE dsysh-dokclass VALUE 'NA'.

DATA: r_ers(5) TYPE c    VALUE  'R_ERS', "Refnr erstellt
      r_off(5) TYPE c    VALUE  'R_OFF', "Refnr offen/fehlerhaft
      r_erl(5) TYPE c    VALUE  'R_ERL', "Refnr erledigt (TA kompl)
      r_fre(5) TYPE c    VALUE  'R_FRE', "Refnr freigegeben
      r_lin(5) TYPE c    VALUE  'R_LIN', "Zeilensumme (line)
      r_tao(5) TYPE c    VALUE  'R_TAO'. "Transportaufträge offen

DATA: erstellte(11)    TYPE c    VALUE  'SUM00-R_ERS',
      offene(11)       TYPE c    VALUE  'SUM00-R_OFF',
      erledigte(11)    TYPE c    VALUE  'SUM00-R_ERL',
      freigegebene(11) TYPE c    VALUE  'SUM00-R_FRE',
      alle(11)         TYPE c    VALUE  'SUM00-R_LIN',
      offene_ta(11)    TYPE c    VALUE  'SUM00-R_TAO'.


**************************ALTERAÇÔES SAPCONSOLE******************
DATA : confirmado VALUE '0'.
DATA  ok_code_0001(5).
DATA  pulmao LIKE lagp-lgtyp.
DATA  bin_pulmao LIKE lagp-lgpla.
DATA  porta_pul LIKE lagp-lgtyp.
DATA  bin_porta_pul LIKE lagp-lgpla.
DATA  pick_pre LIKE lagp-lgtyp.
DATA  bin_pick_pre LIKE lagp-lgpla.
DATA : posicoes LIKE zwm027 OCCURS 0 WITH HEADER LINE.
DATA index TYPE sy-tabix.
DATA: text1 TYPE  bdcmsgcoll-msgv1.
DATA : iit311 LIKE it311 OCCURS 1.
RANGES: range_refnr FOR t311-refnr.
DATA flag(1).
DATA to LIKE ltap-tanum.

DATA t_ltap_cancl LIKE ltap_cancl OCCURS 0 WITH HEADER LINE.

DATA: t_ltak      LIKE ltak OCCURS 0 WITH HEADER LINE,
      t_ltap      LIKE ltap OCCURS 0 WITH HEADER LINE,
      c_ltap      LIKE ltap OCCURS 0 WITH HEADER LINE,
      return_msg  LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE,
      lt_messages TYPE tab_bdcmsgcoll,
      t_sscc      LIKE zwm_sscc OCCURS 0 WITH HEADER LINE,
      ls_return   TYPE bdcmsgcoll.

DATA: to_prm,
      qtd_total    LIKE ltap-vsola,
      to_remontada LIKE ltak-tanum.
**************************ALTERAÇÔES SAPCONSOLE******************

DATA: gv_out.

*-----------------------------------------------------------------------
*      Daten
*-----------------------------------------------------------------------
INCLUDE mllvsfld.

*-Zeiger---------------------------------------------------------------*
FIELD-SYMBOLS: <feldname>.

*-Hilfsvariablen-------------------------------------------------------*
DATA: cnt            TYPE i,        "Zähler für Einzelsatzanzeige
      hlp_gefunden,      "Selektion mit Datenauswahl
      hlp_fname(30),     "Feldname Cursorposition
      sav_fname(30),     "Feldname Cursorposition
      sav_rbtyp      LIKE t311-rbtyp,
      sav_linno      LIKE sy-linno,
      sav_pagno      LIKE sy-pagno,
*     HLP_FNUMM          LIKE LDK00-FNUMM,
      hlp_text(60)   TYPE c,     "Feldname Cursorposition
      high_value     LIKE t311-rbtyp VALUE 'Z', "Summe zuletzt
      hlp_fvalue(30),    "Wert in Feldname Cursorposition
      hlp_lines      TYPE i.

DATA: such_rbtyp    LIKE t311-rbtyp, "Referenzbelegtyp
*     SUCH_KZAKT LIKE T311-KZAKT, "Status 'Bereits aktiv gewesen'
*     SUCH_KZERL LIKE T311-KZERL, "Status 'Ref.nummer komplett erledigt'
*     SUCH_KZDRU LIKE T311-KZDRU. "Status 'Ref.nummer freigegeben'
      such_k_ers(1) TYPE c,       "Status Refnr erstellt
      such_k_off(1) TYPE c,       "Status Refnr offen/fehlerhaft
      such_k_erl(1) TYPE c,       "Status Refnr erl. (TA kompl)
      such_k_fre(1) TYPE c,       "Status Refnr freigegeben
      such_k_tao(1) TYPE c.       "Status Transportaufträge offen

DATA: liste1_titel1(50),           "Überschrift Detailliste1 Teil1
      liste1_titel2(55).           "Überschrift Detailliste1 Teil2
"(->Textelemente)

DATA: return_subrc LIKE sy-subrc.  "Returncode

DATA: low_refnr  LIKE t311-refnr.  "die erste selektierte Ref.nummer

*-Konstanten-----------------------------------------------------------*
INCLUDE mllvskon.

DATA: sort_x TYPE c      VALUE 'X',
      sort_y TYPE c      VALUE 'Y',
      sort_z TYPE c      VALUE 'Z'.

*.........Funktionscodes..............................................*

*     FCODE_LGRA(4)          TYPE C    VALUE 'LGRA',
*     FCODE_MARF(4)          TYPE C    VALUE 'MARF',
DATA: fcode_sammelgang(4)    TYPE c    VALUE 'SAMM',
      fcode_freigabe(4)      TYPE c    VALUE 'FREI',
      fcode_aktualisieren(4) TYPE c    VALUE 'AKTU',
      fcode_anz_refbeleg(4)  TYPE c    VALUE 'ANZR',
      fcode_anz_ta(4)        TYPE c    VALUE 'ANZT',
      fcode_anz_liste(4)     TYPE c    VALUE 'ANZL',
      fcode_anz_einzeln(4)   TYPE c    VALUE 'ANZK',
      fcode_back(4)          TYPE c    VALUE 'BAKK',
      fcode_zurueck_liste(4) TYPE c    VALUE 'ZURU',
      fcode_beenden(4)       TYPE c    VALUE 'RET ',
      fcode_abbrechen(4)     TYPE c    VALUE 'ESC ',
      fcode_langtext(4)      TYPE c    VALUE 'LANG',
      fcode_loeschen(4)      TYPE c    VALUE 'LOEB',
      fcode_mark(4)          TYPE c    VALUE 'MARK',
      fcode_mara(4)          TYPE c    VALUE 'MARA',
      fcode_mare(4)          TYPE c    VALUE 'MARE',
      fcode_2stufig(4)       TYPE c    VALUE '2STK',
      fcode_kommfort(4)      TYPE c    VALUE 'PKPR',

*.........FCODE fremder Transaktionen.................................*

      fcode_strt(4)          TYPE c    VALUE 'STRT',
      fcode_frei(4)          TYPE c    VALUE 'FREI',

*.........Reportnamen..................................................*

      con_report_rllt2900    LIKE sy-repid  VALUE 'RLLT2900',

*.........PF-Status...................................................*

      pfstat_liste1(4)       TYPE c    VALUE 'PICK',
      pfstat_liste2(4)       TYPE c    VALUE 'PIC1',
      pfstat_liste3(4)       TYPE c    VALUE 'PIC2',
      pfstat_liste4(4)       TYPE c    VALUE 'PIC3',

*.........Titel.......................................................*

      titel_liste1(3)        TYPE c    VALUE '01',
      titel_liste2(3)        TYPE c    VALUE '02',
      titel_liste3(3)        TYPE c    VALUE '03',

*.........Flags.......................................................*

      flg_intens             TYPE c,
      flg_return             TYPE c,
      markier_return         TYPE c,

*.........Selektiontabellen...........................................*

      con_sign_i(1)          TYPE c    VALUE 'I',
      con_sign_e(1)          TYPE c    VALUE 'E',
      con_option_ge(2)       TYPE c    VALUE 'GE',
      con_option_le(2)       TYPE c    VALUE 'LE',
      con_option_bt(2)       TYPE c    VALUE 'BT',
      con_option_eq(2)       TYPE c    VALUE 'EQ',
      con_option_cp(2)       TYPE c    VALUE 'CP'.

RANGES: r_refnr FOR t311-refnr.

DATA: gv_subrc TYPE sysubrc.

*-SELEKTIONSBILD-------------------------------------------------------*
PARAMETERS:     lgnum LIKE t311-lgnum OBLIGATORY MEMORY ID lgn.
SELECT-OPTIONS: refnr FOR t311-refnr.
PARAMETERS:     rbtyp LIKE t311-rbtyp.
SELECTION-SCREEN SKIP 1.

SELECT-OPTIONS: datum FOR t311-datum
                      DEFAULT sy-datlo TO sy-datlo.
PARAMETERS:     offta LIKE rl05s-offta.

SELECTION-SCREEN SKIP 1.
PARAMETERS: gf_selv LIKE lvs_rllt2900_struct-lvs_sel_variant_rllt2900
                    MEMORY ID lva.

*----------------------------------------------------------------------*
*        AT SELECTION-SCREEN                                           *
*----------------------------------------------------------------------*

*------------------------------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR gf_selv.
*------------------------------------------------
*  value help for selection variant of report picking progress
  PERFORM f4_for_sel_variant.

*-------------------
AT SELECTION-SCREEN.
*-------------------
* check, whether selection variant exists
  PERFORM check_sel_variant_existence.

*----------------------------------------------------------------------*
*        START-OF-SELECTION                                            *
*----------------------------------------------------------------------*
START-OF-SELECTION.


  CLEAR: gv_out.
  PERFORM check_whs.
  IF gv_out = 'X'.
    EXIT.
  ENDIF.


  SET PF-STATUS pfstat_liste1.
  SET TITLEBAR titel_liste1.

  PERFORM t311_lesen.

  IF offta NE space.
*......Ausgabe der Summen mit TA-Informationen.........................
    PERFORM t311_sum_ausgeben_ta.
  ELSE.
*......Ausgabe der Summen ohne TA-Informationen........................
    PERFORM t311_sum_ausgeben.
  ENDIF.

END-OF-SELECTION.

*----------------------------------------------------------------------*
*        TOP-OF-PAGE                                                   *
*----------------------------------------------------------------------*
TOP-OF-PAGE.

  PERFORM kopf_liste1_ausgeben.

*----------------------------------------------------------------------*
*        AT USER-COMMAND                                               *
*----------------------------------------------------------------------*
AT USER-COMMAND.

*........Direkt angekreuzte Felder müssen in der internen Tabelle
  PERFORM pai.                        "gekreuzt werden.

  CLEAR hlp_fname.
*  iit311[] = it311[].
  CASE sy-ucomm.
*---------------------------------------------------------------------
*       Zurück zur vorigen Liststufe
*---------------------------------------------------------------------
    WHEN fcode_back.

      PERFORM zurueck_aus_liste.

      sy-lsind = sy-lsind - 1.
      SET SCREEN 0.
      LEAVE SCREEN.

*---------------------------------------------------------------------*
*       Übersichtsliste aktualisieren                                 *
*---------------------------------------------------------------------*
    WHEN fcode_aktualisieren.
      SET PF-STATUS pfstat_liste1.
      SET TITLEBAR titel_liste1.

      PERFORM refnr_aktualisieren.
      PERFORM t311_lesen.

      sy-lsind = sy-lsind - 1.
      NEW-PAGE.

      IF offta NE space.
*......Ausgabe der Summen mit TA-Informationen.........................
        PERFORM t311_sum_ausgeben_ta.
      ELSE.
*......Ausgabe der Summen ohne TA-Informationen........................
        PERFORM t311_sum_ausgeben.
      ENDIF.

*---------------------------------------------------------------------*
*       Sammelgang für Referenznummer starten.                        *
*---------------------------------------------------------------------*
    WHEN fcode_sammelgang.

*      CLEAR flag.
*      LOOP AT it311 WHERE kreuz = con_x.
*
*        SELECT SINGLE *
*            FROM zwm028
*                WHERE lgnum = it311-lgnum AND
*                      refnr = it311-refnr.
*        IF sy-subrc <> 0.
*          flag = 'X'.
*          MESSAGE ID 'ZWMMSG001' TYPE 'I'
*                NUMBER '131' WITH it311-refnr.
*
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*      IF NOT flag IS INITIAL.
*        EXIT.
*      ENDIF.

      PERFORM markier_konsistenz.
      CHECK markier_return = space.
      PERFORM sammelgang CHANGING gv_subrc.
      CHECK gv_subrc EQ 0.

*---------------------------------------------------------------------*
*       Anzeigen Transportaufträge zur Referenznummer.                *
*---------------------------------------------------------------------*
    WHEN fcode_anz_ta.
      CLEAR r_refnr.
      REFRESH r_refnr.
      LOOP AT it311 WHERE kreuz = con_x.
*........Füllen Rangestabelle der Referenznummern.......................
        CLEAR r_refnr.
        MOVE:
              it311-refnr   TO r_refnr-low,
              con_sign_i    TO r_refnr-sign,
              con_option_eq TO r_refnr-option.
        APPEND r_refnr.
      ENDLOOP.
      IF sy-subrc NE 0.
*........Keine Einträge zum Löschen selektiert..........................
        MESSAGE w090.
        sy-ucomm = space.
        EXIT.
      ELSE.
        SUBMIT rllt2500 WITH t5_lgnum EQ lgnum
                        WITH t5_refnr IN r_refnr
                        WITH t5_offta EQ space
                        WITH t5_quita EQ space
                        WITH t5_allta EQ con_x
                        WITH dunkl EQ space
                        WITH subst EQ con_x
                        AND RETURN.
        LOOP AT it311 WHERE kreuz = con_x.
          it311-kreuz = space.
          MODIFY it311.
        ENDLOOP.
        MOVE sav_fname TO hlp_fname.
        MOVE sav_rbtyp TO sum00-rbtyp.

        SET PF-STATUS pfstat_liste2.
        SET TITLEBAR titel_liste2.
        sy-lsind = sy-lsind - 1.
        PERFORM detailliste1.
      ENDIF.

*---------------------------------------------------------------------*
*       Anzeigen Referenzbeleg (Transportbedarf bzw. Lieferung)       *
*---------------------------------------------------------------------*
    WHEN fcode_anz_refbeleg.

      IF it311a-rbnum IS INITIAL.
        LOOP AT it311a WHERE kreuz = con_x.
          EXIT.
        ENDLOOP.
        IF sy-subrc NE 0.
*........Keine Einträge zum Löschen selektiert..........................
          MESSAGE w090.
          sy-ucomm = space.
          EXIT.
        ENDIF.
      ENDIF.
      IF it311-rbtyp EQ con_refnrbelegtyp_b.
*........Parameter entsprechend setzen und TB-Anzeige aufrufen..........
        SET PARAMETER ID parid_lgnum FIELD it311a-lgnum.
        SET PARAMETER ID parid_tbnum FIELD it311a-rbnum.
        CALL TRANSACTION 'LB03' AND SKIP FIRST SCREEN.
      ENDIF.
      IF it311-rbtyp EQ con_refnrbelegtyp_l.
*........Parameter entsprechend setzen und LF-Anzeige aufrufen..........
        SET PARAMETER ID parid_vbeln FIELD it311a-rbnum.
        CALL TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
      ENDIF.

*---------------------------------------------------------------------*
*       Referenznummer anzeigen                                       *
*---------------------------------------------------------------------*
    WHEN fcode_anz_einzeln.
      IF NOT it311-refnr IS INITIAL.
        SET PF-STATUS pfstat_liste3.
        SET TITLEBAR titel_liste3.
        REFRESH it311a.
        SELECT * FROM t311a INTO TABLE it311a
                            WHERE lgnum = lgnum
                            AND   refnr = it311-refnr.
        PERFORM detailliste2.
      ELSE.
        CLEAR cnt.
        LOOP AT it311 WHERE kreuz = con_x.
          cnt = cnt + 1.
          sav_it311 = it311.
        ENDLOOP.
        IF cnt = 1.
          it311 = sav_it311.
          SET PF-STATUS pfstat_liste3.
          SET TITLEBAR titel_liste3.
          REFRESH it311a.
          SELECT * FROM t311a INTO TABLE it311a
                              WHERE lgnum = lgnum
                              AND   refnr = it311-refnr.
          PERFORM detailliste2.
        ELSE.
          MESSAGE i088.
        ENDIF.
      ENDIF.

*---------------------------------------------------------------------*
*       Übersicht über die selektierten Referenznummern               *
*---------------------------------------------------------------------*
    WHEN fcode_anz_liste.
      GET CURSOR FIELD hlp_fname VALUE hlp_fvalue.
      sav_fname = hlp_fname.
      sav_rbtyp = sum00-rbtyp.
      IF hlp_fname+6(5) = r_ers OR
         hlp_fname+6(5) = r_off OR
         hlp_fname+6(5) = r_erl OR
         hlp_fname+6(5) = r_fre OR
         hlp_fname+6(5) = r_lin OR
         hlp_fname+6(5) = r_tao.
        CHECK hlp_fvalue NE '      0'.      "CHECK HLP_FVALUE NE 0
        IF lgnum = '100'.
          SET PF-STATUS pfstat_liste4.
        ELSE.
          SET PF-STATUS pfstat_liste2.
        ENDIF.
        SET TITLEBAR titel_liste2.
        PERFORM detailliste1.
      ENDIF.

*---------------------------------------------------------------------*
*       Markieren alle Belege                                         *
*---------------------------------------------------------------------*
    WHEN fcode_mara.
      CASE sy-pfkey.
        WHEN pfstat_liste1.
        WHEN pfstat_liste2.
          PERFORM markier_konsistenz.
          CHECK markier_return = space.
          PERFORM markier_selektion USING fcode_mara.
          IF flg_return = space.
*           MESSAGE I088.
          ENDIF.
          PERFORM ausgabe_nach_markieren.
        WHEN pfstat_liste3.
          PERFORM markier_konsistenz_detail2.
          CHECK markier_return = space.
          PERFORM markier_selektion_detail2 USING fcode_mara.
          IF flg_return = space.
*           MESSAGE I088.
          ENDIF.
          PERFORM ausgabe_nach_markieren_detail2.
        WHEN OTHERS.
      ENDCASE.

*---------------------------------------------------------------------*
*       Entmarkieren Belege                                           *
*---------------------------------------------------------------------*
    WHEN fcode_mare.
      CASE sy-pfkey.
        WHEN pfstat_liste1.
        WHEN pfstat_liste2.
          PERFORM markier_konsistenz.
          CHECK markier_return = space.
          PERFORM markier_selektion USING fcode_mare.
          IF flg_return = space.
*           MESSAGE I088.
          ENDIF.
          PERFORM ausgabe_nach_markieren.
        WHEN pfstat_liste3.
          PERFORM markier_konsistenz_detail2.
          CHECK markier_return = space.
          PERFORM markier_selektion_detail2 USING fcode_mare.
          IF flg_return = space.
*           MESSAGE I088.
          ENDIF.
          PERFORM ausgabe_nach_markieren_detail2.
        WHEN OTHERS.
      ENDCASE.

*---------------------------------------------------------------------*
*       Markieren Beleg einzeln                                       *
*---------------------------------------------------------------------*
    WHEN fcode_mark.
      CASE sy-pfkey.
        WHEN pfstat_liste1.
        WHEN pfstat_liste2.
          PERFORM markier_konsistenz.
          CHECK markier_return = space.
          PERFORM markier_selektion USING fcode_mark.
          IF flg_return = space.
            MESSAGE i089.
          ENDIF.
          PERFORM ausgabe_nach_markieren.
        WHEN pfstat_liste3.
          PERFORM markier_konsistenz_detail2.
          CHECK markier_return = space.
          PERFORM markier_selektion_detail2 USING fcode_mark.
          IF flg_return = space.
*           MESSAGE I088.
          ENDIF.
          PERFORM ausgabe_nach_markieren_detail2.
        WHEN OTHERS.
      ENDCASE.

*---------------------------------------------------------------------*
*       Referenznummer freigeben                                      *
*---------------------------------------------------------------------*
    WHEN fcode_freigabe.
      CLEAR cnt.
      LOOP AT it311.
        CHECK it311-kreuz = con_x.
        CASE sav_fname.
          WHEN erstellte.
            MESSAGE e100. EXIT.
          WHEN offene.
            sav_it311 = it311.           "Merken für Einzel-Starten
            cnt = cnt + 1.
            MESSAGE e101. EXIT.
          WHEN erledigte.
            sav_it311 = it311.
            cnt = cnt + 1.
          WHEN freigegebene.
            sav_it311 = it311.           "Merken für Einzel-Starten
            cnt = cnt + 1.
*SAPCONSOLE
            confirmado = '1'.
            MESSAGE i102. EXIT.
          WHEN OTHERS.
            MESSAGE e108. EXIT.
        ENDCASE.
      ENDLOOP.

      CASE cnt.
        WHEN 0.
          MESSAGE i099.
        WHEN 1.
          it311 = sav_it311.
          PERFORM sammelgang_freigeben.
        WHEN OTHERS.
          MESSAGE i099.
      ENDCASE.

*---------------------------------------------------------------------*
*       Verarbeitung von PF12 Abbrechen                               *
*---------------------------------------------------------------------*
    WHEN fcode_abbrechen.
      LEAVE.
      LEAVE TO TRANSACTION sy-tcode.

*---------------------------------------------------------------------*
*       Verarbeitung von PF15 Beenden                                 *
*---------------------------------------------------------------------*
    WHEN fcode_beenden.
      LEAVE.
      LEAVE TO TRANSACTION sy-tcode.

*---------------------------------------------------------------------*
*       Langtext zu Fehlermeldungen anzeigen                          *
*---------------------------------------------------------------------*
    WHEN fcode_langtext.

*     IF TDK00-FNUMM IS INITIAL.
*       MESSAGE S132.
*     ENDIF.

**    CHECK NOT TDK00-MSGID IS INITIAL AND
**          NOT TDK00-FNUMM IS INITIAL.

*     MOVE:
*         TDK00-MSGID TO DOKU_NA-MSGID,
*         TDK00-FNUMM TO DOKU_NA-MSGNO.
*     WRITE:
*         TDK00-FTEXT TO HLP_TEXT.

*     REFRESH LLINES.
*     CALL FUNCTION 'DOKU_OBJECT_SHOW'
*          EXPORTING
*                   DOKCLASS         = CON_DOKU_NA
*                   DOKLANGU         = SY-LANGU
*                    DOKNAME          = DOKU_NA
*                    DOKTITLE         = HLP_TEXT
*          TABLES
*                    LINKS            = LLINES
*          EXCEPTIONS
*                    OBJECT_NOT_FOUND = 1
*                    SAPSCRIPT_ERROR  = 2.
*
*         CASE SY-SUBRC.
*            WHEN 1.
*              MESSAGE S130.    "There is no Long-text
*            WHEN 2.
*              MESSAGE S131.    "Error while SAPSCRIPTing
*         ENDCASE.

*---------------------------------------------------------------------*
*       Löschen Beleg aus Referenznummer  (analog FC053 in ML05SFFC)  *
*---------------------------------------------------------------------*
    WHEN fcode_loeschen.
      CLEAR t_t311a.
      REFRESH t_t311a.
      LOOP AT it311a
              WHERE kreuz = con_x.
*........Füllen interne Tabelle der Referenzbelege......................
        CLEAR t_t311a.
        MOVE:
             lgnum        TO t_t311a-lgnum,
             it311-refnr  TO t_t311a-refnr,
             it311-rbtyp  TO t_t311a-rbtyp,
             it311a-rbnum TO t_t311a-rbnum.

        APPEND t_t311a.
      ENDLOOP.
      IF sy-subrc NE 0.
*........Keine Einträge zum Löschen selektiert..........................
        MESSAGE w083.
        sy-ucomm = space.
        EXIT.
      ELSE.
*........Aufruf POPUP zum Bestätigen des Löschens.......................
        PERFORM popup_bei_loeschen.
        CASE antwort.
          WHEN antwort_ja.
*........JA -> Löschen..................................................
            return_subrc = 0.
            PERFORM rbnum_loeschen USING lgnum
                                         it311-refnr
                                         it311-rbtyp
                                         return_subrc.
            IF return_subrc NE 0.
              MESSAGE w084.
              CLEAR sy-ucomm.
              EXIT.
            ELSE.
              LOOP AT it311a WHERE kreuz = con_x.
                DELETE it311a.
              ENDLOOP.
              COMMIT WORK.
              SET PF-STATUS pfstat_liste3.
              SET TITLEBAR titel_liste3.
              sy-lsind = sy-lsind - 1.
              NEW-PAGE.
              PERFORM detailliste2.
            ENDIF.
          WHEN OTHERS.
*........NEIN -> Nicht Löschen..........................................
            CLEAR sy-ucomm.
            EXIT.
        ENDCASE.
      ENDIF.

*---------------------------------------------------------------------*
*       Anzeigen Referenznummer für 2-stufige Kommi                   *
*---------------------------------------------------------------------*
    WHEN fcode_2stufig.
*........Füllen interne Tabelle der Referenznummern.....................
      REFRESH t_t311.
      LOOP AT it311
              WHERE kreuz = con_x.
        CLEAR t_t311.
        MOVE-CORRESPONDING it311 TO t_t311.

        APPEND t_t311.
      ENDLOOP.
      IF sy-subrc NE 0.

*........Keine Einträge für 2-st. Kommi selektiert.....................
        MESSAGE w109.
        sy-ucomm = space.
        EXIT.
      ELSE.
        DESCRIBE TABLE t_t311 LINES hlp_lines.
        IF hlp_lines EQ 1.

*........Nur eine Referenznummer selektiert.............................
          IF it311-l2skr EQ con_refnr_no_rel.
            MESSAGE e201.
          ELSE.
            CALL FUNCTION 'L_2_STEP_PICKING_DISP_VIEW_REF'
              EXPORTING
                i_lgnum                       = lgnum
                i_refnr                       = it311-refnr
*               I_VARIANT                     =
              EXCEPTIONS
                refnr_no_found                = 1
                refnr_documents_no_found      = 2
                no_relevant_for_2step_picking = 3
                OTHERS                        = 4.
            IF sy-subrc NE 0.
              MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          ENDIF.
        ELSE.
*........Mehrere Referenznummer selektiert..............................

          CALL FUNCTION 'L_2_STEP_PICKING_DISP_VIEW_ALL'
            EXPORTING
              i_lgnum                       = lgnum
*             I_VARIANT                     =
            TABLES
              t_t311                        = t_t311
            EXCEPTIONS
              refnr_no_found                = 1
              refnr_documents_no_found      = 2
              no_relevant_for_2step_picking = 3
              OTHERS                        = 4.
          IF sy-subrc NE 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.
      ENDIF.
*---------------------------------------------------------------------*
*       Kommissionierfortschritt                                      *
*---------------------------------------------------------------------*
    WHEN fcode_kommfort.
      cnt = 0.
      IF NOT it311-refnr IS INITIAL.
        sav_it311 = it311.
      ENDIF.

      LOOP AT it311
              WHERE kreuz = con_x.
        cnt = cnt + 1.
        sav_it311 = it311.
      ENDLOOP.

      IF  cnt = 0
      AND sav_it311-refnr IS INITIAL.
        MESSAGE i090.           "Es wurde keine Gruppe selektiert.
      ELSE.
        IF cnt > 1.
          CLEAR it311.
          MESSAGE i088.         "Bitte nur eine Gruppe selektieren.
        ELSE.
          IF gf_selv IS INITIAL.  "without selection variant
            SET PARAMETER ID parid_lgnum FIELD lgnum.
            SUBMIT ('RLLT2900') VIA SELECTION-SCREEN
                                AND RETURN
                                WITH pa_sammg = sav_it311-refnr.
          ELSE.             "with selection variant
            SUBMIT ('RLLT2900') USING SELECTION-SET gf_selv
                                AND RETURN
                                WITH pa_sammg = sav_it311-refnr.
          ENDIF.
        ENDIF.
      ENDIF.
*---------------------------------------------------------------------
*      Sapconsole - Visualizar Paletes de picking
*---------------------------------------------------------------------
    WHEN 'PPICK'.
*      REFRESH range_refnr.
*      LOOP AT it311 WHERE kreuz = con_x.
*        CLEAR range_refnr.
*        MOVE:
*              it311-refnr   TO range_refnr-low,
*              con_sign_i    TO range_refnr-sign,
*              con_option_eq TO range_refnr-option.
*        APPEND range_refnr.
*      ENDLOOP.
*
*      IF sy-subrc NE 0.
*        MESSAGE w090.
*        sy-ucomm = space.
*        EXIT.
*      ELSE.
*        SUBMIT zwmrep0017 WITH s_grupo IN range_refnr AND RETURN.
*      ENDIF.
*---------------------------------------------------------------------
*      Sapconsole - Criar Recorridos de picking
*---------------------------------------------------------------------
    WHEN 'ROTA'.

      CLEAR flag.
      REFRESH range_refnr.
      LOOP AT it311 WHERE kreuz = con_x.
        CLEAR range_refnr.
        MOVE:
              it311-refnr   TO range_refnr-low,
              con_sign_i    TO range_refnr-sign,
              con_option_eq TO range_refnr-option.
        APPEND range_refnr.
      ENDLOOP.
      IF sy-subrc NE 0.
        MESSAGE w090.
        sy-ucomm = space.
        EXIT.
      ELSE.
*      Verificar se ja executou a rota de picking para o grupo
        LOOP AT it311 WHERE kreuz = con_x.
          SELECT SINGLE *
              FROM zwm026
                  WHERE armazem = it311-lgnum AND
                        grupo = it311-refnr.
          IF sy-subrc <> 0.
            flag = 'X'.
            EXIT.

          ELSEIF sy-subrc = 0.

            IF zwm026-num_recorrido <> '                    '.

              MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '116' WITH
              it311-refnr.
              flag = 'X'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.
        IF flag = 'X'.
          EXIT.
        ENDIF.

** RL -> MOD 13.05.2005
** Não permitir a criação de rotas de picking para mais do que 1 grupo
*        SUBMIT zwmrep0018
*                        WITH s_grupo IN range_refnr
*                        AND RETURN.

        PERFORM sammelgang_new.
** RL <- MOD 13.05.2005

        LOOP AT it311 WHERE kreuz = con_x.
          it311-kreuz = space.
          MODIFY it311.
        ENDLOOP.
        MOVE sav_fname TO hlp_fname.
        MOVE sav_rbtyp TO sum00-rbtyp.

        SET PF-STATUS pfstat_liste2.
        SET TITLEBAR titel_liste2.
        sy-lsind = sy-lsind - 1.
        PERFORM detailliste1.
      ENDIF.

*........Ende AT USER-COMMAND.........................................*
  ENDCASE.

*----------------------------------------------------------------------*
*        TOP-OF-PAGE DURING LINE-SELECTION                             *
*----------------------------------------------------------------------*
TOP-OF-PAGE DURING LINE-SELECTION.

  CASE sy-pfkey.
    WHEN pfstat_liste1.
      PERFORM kopf_liste1_ausgeben.

    WHEN pfstat_liste2.
      PERFORM kopf_liste2_ausgeben.

    WHEN pfstat_liste3.
      PERFORM kopf_liste3_ausgeben.

    WHEN pfstat_liste4.
      PERFORM kopf_liste2_ausgeben.
  ENDCASE.


*----------------------------------------------------------------------*
*        Unterroutinen                                                 *
*        -------------                                                 *
*                                                                      *
*        FORM AUSGABE_NACH_MARKIEREN                                   *
*        FORM T311_LESEN                                               *
*        FORM T311_SELECT                                              *
*                                                                      *
*        FORM SAMMELGANG                                               *
*        FORM SAMMELGANG_STARTEN                                       *
*        FORM SAMMELGANG_FREIGEBEN                                     *
*        FORM DETAILLISTE1                                             *
*        FORM DETAILLISTE2                                             *
*                                                                      *
*        FORM KOPF_LISTE1_AUSGEBEN                                     *
*        FORM KOPF_LISTE2_AUSGEBEN                                     *
*        FORM KOPF_LISTE3_AUSGEBEN                                     *
*        FORM T311_SUM_AUSGEBEN                                        *
*        FORM T311_SUM_AUSGEBEN_TA                                     *
*        FORM DETAILLISTE1_AUSGABE                                     *
*        FORM DETAILLISTE2_AUSGABE                                     *
*                                                                      *
*----------------------------------------------------------------------*

*----------------------------------------------------------------------*
*        AUSGABE_NACH_MARKIEREN                                        *
*----------------------------------------------------------------------*
FORM ausgabe_nach_markieren.
  MOVE sav_fname TO hlp_fname.
  MOVE sav_rbtyp TO sum00-rbtyp.

  SET PF-STATUS pfstat_liste2.
  SET TITLEBAR titel_liste2.
  sy-lsind = sy-lsind - 1.
  PERFORM detailliste1.
ENDFORM.                    "ausgabe_nach_markieren
*----------------------------------------------------------------------*
*        AUSGABE_NACH_MARKIEREN_DETAIL2                                *
*----------------------------------------------------------------------*
FORM ausgabe_nach_markieren_detail2.

  SET PF-STATUS pfstat_liste3.
  SET TITLEBAR titel_liste3.
  sy-lsind = sy-lsind - 1.
  PERFORM detailliste2.

ENDFORM.                    "ausgabe_nach_markieren_detail2
*----------------------------------------------------------------------*
*        FORM LTAK_LESEN                                               *
*----------------------------------------------------------------------*
*        Lesen Transportauftragskopf zum Ermitteln von abgeschlossenen *
*        und offenen TAs zur Referenznummer.                           *
*----------------------------------------------------------------------*
FORM ltak_lesen USING p_lgnum p_refnr.

  CHECK offta NE space.

  CLEAR iltak.
  REFRESH iltak.
  SELECT * FROM ltak WHERE lgnum EQ p_lgnum
                     AND   refnr EQ p_refnr.

    MOVE-CORRESPONDING ltak TO iltak.
    APPEND iltak.

  ENDSELECT.

ENDFORM.                    "ltak_lesen

*----------------------------------------------------------------------*
*        FORM T311_LESEN                                               *
*----------------------------------------------------------------------*
*        Lesen der Tabelle T311 gemäß Selektionkriterien mit Summen-   *
*        bildung für 'erstellt', 'offen', 'erledigt' und 'frei-   *
*        gegeben pro Referenzbelegtyp.                                 *
*----------------------------------------------------------------------*
FORM t311_lesen.

*-Löschen der internen Tabellen/Hilfsvariablen-------------------------*
  REFRESH it311.
  REFRESH sum00.
  CLEAR   hlp_gefunden.
  CLEAR   low_refnr.


*-Selektion gemäß Kriterium--------------------------------------------*
  SELECT * FROM t311 WHERE lgnum EQ lgnum
                     AND   datum IN datum.

    IF NOT refnr-low IS INITIAL OR NOT refnr-high IS INITIAL.
      CHECK refnr.
    ENDIF.

    IF NOT rbtyp IS INITIAL.
      CHECK rbtyp EQ t311-rbtyp.
    ENDIF.

    MOVE t311 TO it311.
    CLEAR: it311-linno, it311-pagno,
           it311-liefn, it311-tbedn, it311-k_ers, it311-k_off,
           it311-k_erl, it311-k_fre, it311-k_tao.
    PERFORM t311_select.
    APPEND it311.

*........Den ersten selektierten Satz merken..........................
    IF low_refnr IS INITIAL.
      MOVE t311-refnr TO low_refnr.
    ENDIF.

  ENDSELECT.

  SORT it311 BY refnr.

ENDFORM.                                                    "t311_lesen

*----------------------------------------------------------------------*
*        FORM T311_SELECT                                              *
*----------------------------------------------------------------------*
*        Unterroutine für T311_LESEN.                                  *
*----------------------------------------------------------------------*
FORM t311_select.

  hlp_gefunden = con_x.            "Mindestens ein Datensatz gefunden

*-Füllen der Kopfleiste der Tabelle SUM00 mit Status-------------------*
  CLEAR sum00.

  IF offta NE space.
    PERFORM ltak_lesen USING t311-lgnum t311-refnr.
    LOOP AT iltak.
      IF iltak-kquit = space.
        sum00-r_tao = 1.
        it311-k_tao = con_x.
      ENDIF.
    ENDLOOP.
  ENDIF.

  IF it311-kzdru = space.
    IF it311-kzerl = space.
      IF it311-kzakt = con_x.
        sum00-r_off = 1.           "-> Status ist offen
        it311-k_off = con_x.
      ELSE.
        sum00-r_ers = 1.           "-> Status ist erstellt
        it311-k_ers = con_x.
      ENDIF.
    ELSE.
      sum00-r_erl = 1.             "-> Status ist erledigt
      it311-k_erl = con_x.
    ENDIF.
  ELSE.
    sum00-r_fre = 1.               "-> Status ist freigegeben
    it311-k_fre = con_x.
  ENDIF.

  sum00-r_lin = 1.                 "Zeilensumme

*-Summenbildung über die beteiligten Referenzbelegtypen----------------*
  CASE it311-rbtyp.
    WHEN con_refnrbelegtyp_b.
      it311-tbedn = con_x.             "Transportbedarf
      sum00-rbtyp = con_refnrbelegtyp_b.
      sum00-rtext = text-a30.
      COLLECT sum00.
      sum00-rbtyp = high_value.        "Gesamtsumme fortschreiben.
      sum00-sort = sort_z.
      sum00-rtext = text-a32.
      COLLECT sum00.
    WHEN con_refnrbelegtyp_l.
      it311-liefn = con_x.             "Lieferung
      sum00-rbtyp = con_refnrbelegtyp_l.
      sum00-rtext = text-a31.
      COLLECT sum00.
      sum00-rbtyp = high_value.        "Gesamtsumme fortschreiben.
      sum00-sort = sort_z.
      sum00-rtext = text-a32.
      COLLECT sum00.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    "t311_select

*----------------------------------------------------------------------*
*        FORM SAMMELGANG                                               *
*----------------------------------------------------------------------*
*        Sammelgang für Referenznummern starten.                       *
*----------------------------------------------------------------------*
FORM sammelgang CHANGING cv_subrc TYPE sysubrc.
  DATA: lt_paletes TYPE TABLE OF zpalete_picking.

  DATA:   cnt_selekt   TYPE i.

  CLEAR: cnt_selekt, gv_subrc.

  LOOP AT it311.
    CHECK it311-kreuz = con_x.
    CASE sav_fname.
      WHEN erstellte.
      WHEN offene.
      WHEN erledigte.
        MESSAGE e085. EXIT.
      WHEN freigegebene.
        MESSAGE e085. EXIT.
      WHEN OTHERS.
        MESSAGE e108. EXIT.
*       CHECK 1 = 2.
    ENDCASE.
    sav_it311 = it311.                "Merken für Einzel-Starten
    cnt_selekt = cnt_selekt + 1.
  ENDLOOP.

  CASE cnt_selekt.
    WHEN 0.
      MESSAGE i086.
    WHEN 1.
*{   INSERT         DEVK901482                                        1
* Verificar se o Grupo não tem a cadeia de caracteres 'XX' na
*  descrição. Caso tenha dá um alerta ao utilizador

      DATA: result_tab TYPE match_result_tab,
            lv_rsp.
      CLEAR: result_tab, lv_rsp. REFRESH: result_tab.

      LOOP AT it311 WHERE kreuz = con_x.
        FIND FIRST OCCURRENCE OF 'XX' IN it311-refnt
             IGNORING CASE RESULTS result_tab.
      ENDLOOP.
      IF NOT result_tab[] IS INITIAL.
        CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
          EXPORTING
            defaultoption  = 'N'
            textline1      = text-t01
            titel          = text-t02
            cancel_display = ' '
          IMPORTING
            answer         = lv_rsp.

        IF lv_rsp NE 'J'.
          EXIT.
        ENDIF.
      ENDIF.
*     BREAK-POINT.
*}   INSERT

***  Criar Paletes de Picking
      REFRESH range_refnr.
      LOOP AT it311 WHERE kreuz = con_x.
        CLEAR range_refnr.
        MOVE:
              it311-refnr   TO range_refnr-low,
              con_sign_i    TO range_refnr-sign,
              con_option_eq TO range_refnr-option.
        APPEND range_refnr.
      ENDLOOP.

      IF sy-subrc NE 0.
        MESSAGE w090.
        sy-ucomm = space.
        EXIT.
      ELSE.
        SUBMIT zwmrep0017 WITH s_grupo IN range_refnr AND RETURN.
      ENDIF.

      PERFORM update_picking CHANGING cv_subrc.
      CHECK cv_subrc EQ 0.
      CLEAR flag.
      PERFORM verify_picking_quantity CHANGING lt_paletes.
      IF flag IS INITIAL.
        PERFORM sammelgang_starten CHANGING lt_paletes.
      ENDIF.
    WHEN OTHERS.
      MESSAGE i087.
  ENDCASE.

ENDFORM.                    "sammelgang
*&---------------------------------------------------------------------*
*&      Form  verify_paletes_remontadas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verify_paletes_remontadas.

  DATA: valor1     LIKE zwm001-valor,
        um_pal     LIKE marm-meinh,
        l_tanum    LIKE ltap-tanum,
        quantidade TYPE int2,
        resto      TYPE int2,
        n_pal      TYPE i,
        aux_benum  LIKE ltak-benum.

  DATA: lt_ltap_sum TYPE TABLE OF ltap,
        lt_ltap_par TYPE SORTED TABLE OF ltap WITH UNIQUE KEY tanum tapos,
        ls_ltap     TYPE ltap.

  DATA: lt_ltak  TYPE SORTED TABLE OF ltak WITH UNIQUE KEY tanum,
        lt_t311a TYPE TABLE OF t311a,
        lt_ltap  TYPE TABLE OF ltap.

  DATA: ls_ltak     TYPE ltak,
        ls_t311a    TYPE t311a,
        ls_ltak_sum TYPE ltak.

  DATA t_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

  DATA: lv_2step  TYPE flag,
        lv_2spart TYPE flag,
        lv_tabix  TYPE sytabix.

  DATA: ls_return_msg	TYPE bdcmsgcoll.

  FIELD-SYMBOLS: <ls_ltap> TYPE ltap,
                 <ls_ltak> TYPE ltak.

  CLEAR: t_ltak, t_ltap, c_ltap, return_msg, t_sscc,
  to_prm, to, qtd_total, to_remontada, n_pal.

  REFRESH: t_ltak, t_ltap, c_ltap, return_msg, t_sscc.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

  READ TABLE it311 WITH KEY kreuz = 'X'.

  CHECK sy-subrc = 0.

  SELECT * INTO TABLE t_ltak
      FROM ltak
          WHERE lgnum = it311-lgnum AND
                 refnr = it311-refnr.

  CHECK NOT t_ltak[] IS INITIAL.

  SELECT * FROM ltap INTO TABLE t_ltap
      FOR ALL ENTRIES IN t_ltak
          WHERE lgnum = t_ltak-lgnum AND
                tanum = t_ltak-tanum.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 27.06.2012 14:02:10
*  Motivo: Picking em 2 passos
*--------------------------------------------------------------------*
  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = it311-lgnum
      i_refnr  = it311-refnr
    IMPORTING
      e_2step  = lv_2step
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  IF lv_2step EQ abap_true AND
     lv_2spart EQ abap_false.
    LOOP AT t_ltap ASSIGNING <ls_ltap>.
      <ls_ltap>-vbeln = gc_vbeln_2step_dummy.
    ENDLOOP.
  ELSEIF lv_2spart EQ abap_true.

    LOOP AT t_ltap ASSIGNING <ls_ltap>.
      READ TABLE t_ltak
        ASSIGNING <ls_ltak>
        WITH KEY tanum = <ls_ltap>-tanum.
      CHECK sy-subrc EQ 0.
      CHECK <ls_ltak>-betyp = 'L'.
      CHECK NOT <ls_ltak>-benum IS INITIAL.

      <ls_ltap>-vbeln = <ls_ltak>-benum.
      <ls_ltak>-vbeln = <ls_ltak>-benum.
    ENDLOOP.
  ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*



  DELETE t_ltap WHERE vorga = 'ST'
                         OR vbeln = ' '.

  DELETE t_ltap WHERE letyp <> 'P2'
                  AND letyp <> 'P5'.

*  READ TABLE it311 WITH KEY kreuz = 'X'.
*
*  SELECT * INTO TABLE t_ltak
*      FROM ltak
*          WHERE lgnum = it311-lgnum AND
*                refnr = it311-refnr.
*
*  LOOP AT t_ltak.
*
*    SELECT * APPENDING TABLE t_ltap
*        FROM ltap
*            WHERE lgnum = t_ltak-lgnum AND
*                  tanum = t_ltak-tanum AND
*                  vorga <> 'ST'.
*
*  ENDLOOP.

  CLEAR: valor1, um_pal, l_tanum.
  SELECT SINGLE valor INTO valor1
      FROM zwm001
          WHERE armazem = it311-lgnum AND
                processo = 'PALETIZACAO' AND
                parametro = 'PALETE'.

  um_pal = valor1.

  lt_ltap_sum = t_ltap[].

  LOOP AT t_ltap
  WHERE ( vltyp = 'DRI' OR vltyp = 'BLK' ) AND
  ( letyp = 'P2' OR letyp = 'P5' ) AND
  vbeln <> ' '.

    CLEAR: resto, quantidade, marm, t_sscc.
    FREE: t_sscc.

    SELECT SINGLE * FROM marm
    WHERE matnr EQ t_ltap-matnr
    AND meinh EQ um_pal.

    IF lv_2spart EQ abap_true.
      CLEAR: ls_ltap.
      DELETE lt_ltap_sum WHERE tanum = t_ltap-tanum AND
                               tapos = t_ltap-tapos.

      READ TABLE lt_ltap_par
           WITH TABLE KEY tanum = t_ltap-tanum
                          tapos = t_ltap-tapos
           TRANSPORTING NO FIELDS.
      IF sy-subrc EQ 0.
        CONTINUE.
      ENDIF.



      LOOP AT lt_ltap_sum INTO ls_ltap WHERE vltyp = t_ltap-vltyp AND
                                             vlpla = t_ltap-vlpla AND
                                             nltyp = t_ltap-nltyp AND
                                             nlpla = t_ltap-nlpla AND
                                             matnr = t_ltap-matnr AND
                                             charg = t_ltap-charg AND
                                             vbeln = t_ltap-vbeln.
        lv_tabix = sy-tabix.

        DELETE lt_ltap_sum INDEX lv_tabix.
        t_ltap-vsolm = t_ltap-vsolm + ls_ltap-vsolm.
        INSERT ls_ltap INTO TABLE lt_ltap_par.
        EXIT.
      ENDLOOP.
    ENDIF.

    quantidade = t_ltap-vsolm DIV marm-umrez.

    resto = quantidade MOD 2.
    IF NOT resto IS INITIAL.
** Tem de Criar uma to com a palete de cima para a zona PRM

*      t_sscc-sscc = t_ltap-lgnum.
*      t_sscc-tipo_su = t_ltap-letyp.
      t_sscc-material = t_ltap-matnr.
      t_sscc-quantidade = '1'.
      t_sscc-uni = um_pal.
      t_sscc-lote_producao = t_ltap-charg.
      APPEND t_sscc.
      CLEAR t_sscc.

*      CLEAR ltak.
*      SELECT SINGLE *
*          FROM ltak
*              WHERE lgnum = t_ltap-lgnum AND
*                    tanum = t_ltap-tanum.
*
*
*      DATA aux_benum LIKE ltak-benum.
*      CLEAR aux_benum.

*      aux_benum = ltak-refnr.

      CLEAR aux_benum.

      aux_benum = it311-refnr.
      IF lv_2spart EQ abap_true.
        aux_benum = t_ltap-vbeln.
      ENDIF.

      CLEAR: l_tanum.
      DO 30 TIMES.
        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
          EXPORTING
            warehouse  = t_ltap-lgnum
            mov_type   = '940'
            st_type_o  = t_ltap-vltyp
            bin_origem = t_ltap-vlpla
            plant      = t_ltap-werks
            s_loc      = t_ltap-lgort
            req_number = aux_benum
            req_type   = 'I'
            origem     = 'X'
          IMPORTING
            to         = l_tanum
          TABLES
            return_msg = return_msg
            sscc       = t_sscc
          EXCEPTIONS
            error      = 1
            OTHERS     = 2.
        IF sy-subrc <> 0.
          WAIT UP TO 1 SECONDS.
          PERFORM complete_log_message USING 'REMONTADA_001'
                                             aux_benum
                                             '' ''
                                             t_ltap-matnr
                                       CHANGING return_msg[].
          CALL FUNCTION 'ZWM_LOG_MESSAGE'
            EXPORTING
              i_master    = t_ltap-lgnum
              i_object    = 'ZWM001'
              i_subobject = 'ZWM006'
              i_extnumber = '1'
              i_commit    = 'X'
              it_messages = return_msg[].
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

**      IF l_tanum IS INITIAL.
**        CALL FUNCTION 'ZWM_TO_CANCEL'
**          EXPORTING
**            i_lgnum     = t_ltap-lgnum
**            i_tanum     = t_ltap-tanum
**            i_tapos     = t_ltap-tapos
**            i_commit    = 'X'
**          IMPORTING
**            et_messages = lt_messages
**          EXCEPTIONS
**            error       = 1
**            OTHERS      = 2.
**        IF sy-subrc <> 0.
**          CALL FUNCTION 'ZWM_LOG_MESSAGE'
**            EXPORTING
**              i_master    = t_ltap-lgnum
**              i_object    = 'ZWM001'
**              i_subobject = 'ZWM006'
**              i_extnumber = '1'
**              i_commit    = 'X'
**              it_messages = lt_messages.
**        ELSE.
**          CLEAR: lt_messages, ls_return.
**          ls_return-msgid = 'ZWM001'.
**          ls_return-msgnr = '000'.
**          ls_return-msgtyp = 'W'.
**          ls_return-msgv1 = 'Tarefa Estornada'.
**          ls_return-msgv2 = t_ltap-tanum.
**          ls_return-msgv2 = t_ltap-tapos.
**          APPEND ls_return to lt_messages.
**
**          CALL FUNCTION 'ZWM_LOG_MESSAGE'
**            EXPORTING
**              i_master    = t_ltap-lgnum
**              i_object    = 'ZWM001'
**              i_subobject = 'ZWM006'
**              i_extnumber = '1'
**              i_commit    = 'X'
**              it_messages = lt_messages.
**        ENDIF.
**
**
**      ENDIF.

    ENDIF.

  ENDLOOP.

  CLEAR c_ltap.
  REFRESH c_ltap.
** Verifcar se tem paletes remontadas vindas dos trilaterais
** para a remessa
  SORT t_ltap BY vlenr.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

  LOOP AT t_ltap
*  WHERE vltyp = 'TRI' AND " << DEL ROFF(SDF):TMGP:11.01.2016 14:17:10
  WHERE ( vltyp = 'TRI' OR vltyp = 'AUT' ) AND " << INS ROFF(SDF):TMGP:11.01.2016 14:17:11
  ( letyp = 'P2' OR letyp = 'P5' ) AND
  vbeln <> ' '.

    CLEAR zwm020.
    SELECT SINGLE * FROM zwm020
    WHERE armazem = t_ltap-lgnum AND
    ( p1 = t_ltap-vlenr OR p2 = t_ltap-vlenr ).

    IF sy-subrc = 0.
      CLEAR to_prm.
      CLEAR c_ltap.
      REFRESH c_ltap.
      MOVE-CORRESPONDING t_ltap TO c_ltap.
      APPEND c_ltap.
      CLEAR c_ltap.

      IF zwm020-p1 = t_ltap-vlenr.
** Verificar se existem duas to´s para as duas paletes remontadas

** Se tiverem as duas paletes tem de se estornar as duas tos e voltar a
** criar as duas to's sendo a primeira a da to de baixo

** Senão estorna a to e cria a de baixo para a remessa e a de cima para
** a zona PRM

        READ TABLE t_ltap WITH KEY vlenr = zwm020-p2.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING t_ltap TO c_ltap.
          APPEND c_ltap.
          CLEAR c_ltap.
        ELSE.
          to_prm = 'X'.
        ENDIF.

      ELSEIF zwm020-p2 = t_ltap-vlenr.

        READ TABLE t_ltap WITH KEY vlenr = zwm020-p1.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING t_ltap TO c_ltap.
          APPEND c_ltap.
          CLEAR c_ltap.
        ELSE.
          to_prm = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

** Verificar se estao as duas paletes remontadas para a saida.
** Se for as duas paletes para saida verificar se ja estao criadas pela
** ordem correcta

    CLEAR n_pal.

    DESCRIBE TABLE c_ltap LINES n_pal.
    IF n_pal = 2.
      SORT c_ltap BY lgnum tanum tapos.
      READ TABLE c_ltap INDEX 1.
      IF c_ltap-vlenr = zwm020-p1.
** Validar se as duas paletes são para a mesma remessa
        CLEAR n_pal.
        LOOP AT c_ltap WHERE vbeln = t_ltap-vbeln.
          n_pal = n_pal + 1.
        ENDLOOP.
        IF n_pal = 1.
** As duas paletes são para remessas diferentes

**        No caso de ser grupo em 2 passos, nunca vai ser chamado este PERFORM pois todas as
**        paletes são para o grupo e a remessa é igual (remessa dummy)
          PERFORM split_paletes_remontadas.
          CONTINUE.
        ELSEIF n_pal = 2.
** As duas paletes são para a mesma remessa
          CONTINUE.
        ENDIF.

      ENDIF.
    ENDIF.

    LOOP AT c_ltap.

*      IF n_pal = 1 AND c_ltap-vlenr = zwm020-p1.
*
*      ELSE.

      CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
        EXPORTING
          warehouse     = c_ltap-lgnum
          tanum         = c_ltap-tanum
          tapos         = c_ltap-tapos
        TABLES
          return_msg    = return_msg
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

*      ENDIF.

    ENDLOOP.

    READ TABLE c_ltap INDEX 1.

    READ TABLE t_ltak WITH KEY tanum = c_ltap-tanum.

    IF to_prm IS INITIAL.

**    No caso de ser grupo em 2 passos, nunca vai ser chamado este IF pois todas as
**    paletes são para o grupo e a remessa é igual (remessa dummy)

      WAIT UP TO 10 SECONDS.

      DATA flag(1).
      CLEAR flag.

      DO 60 TIMES. "WHILE flag IS INITIAL.
        CALL FUNCTION 'ZWM_CREATE_TO_DELIVERY'
          EXPORTING
            warehouse     = c_ltap-lgnum
            refnr         = t_ltak-refnr
            vbeln         = t_ltak-vbeln
            posnr         = c_ltap-posnr
            vsola         = c_ltap-vsola
            su            = zwm020-p1
            su2           = zwm020-p2
          IMPORTING
            to            = to
          TABLES
            return_msg    = return_msg
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.
        IF sy-subrc EQ 0.
          flag = 'X'.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
          CLEAR flag.
        ENDIF.
      ENDDO. "ENDWHILE.

    ELSE.

      WAIT UP TO 10 SECONDS.

      CLEAR flag.

*      IF n_pal = 1 AND c_ltap-vlenr = zwm020-p1.
*
*      ELSE.
      DO 60 TIMES. "WHILE flag IS INITIAL.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 27.06.2012 15:21:49
*  Motivo: Fornecer remessa para pck2 passos e normal
*--------------------------------------------------------------------*
        CALL FUNCTION 'ZWM_TO_CREATE_OUT'
          EXPORTING
            warehouse     = c_ltap-lgnum
            refnr         = t_ltak-refnr
            vbeln         = t_ltak-vbeln
            posnr         = c_ltap-posnr
            vsola         = c_ltap-vsola
            meins         = c_ltap-altme
            su            = zwm020-p1
*           SU2           =
*           VLTYP         =
*           VLPLA         =
*           BACKGROUND    =
            werks         = c_ltap-werks
            lgort         = c_ltap-lgort
            matnr         = c_ltap-matnr
          IMPORTING
            to            = to
          TABLES
            return_msg    = return_msg
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.


*        CALL FUNCTION 'ZWM_CREATE_TO_DELIVERY'
*          EXPORTING
*            warehouse     = c_ltap-lgnum
*            refnr         = t_ltak-refnr
*            vbeln         = t_ltak-vbeln
*            posnr         = c_ltap-posnr
*            vsola         = c_ltap-vsola
*            su            = zwm020-p1
*          IMPORTING
*            to            = to
*          TABLES
*            return_msg    = return_msg
*          EXCEPTIONS
*            error_message = 1
*            OTHERS        = 2.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
        IF sy-subrc EQ 0.
          flag = 'X'.
          EXIT.
        ELSE.
          WAIT UP TO 1 SECONDS.
          CLEAR flag.
        ENDIF.
      ENDDO. "ENDWHILE.

*      ENDIF.
      t_sscc-sscc = zwm020-p2.
      t_sscc-tipo_su = c_ltap-letyp.
      t_sscc-material = c_ltap-matnr.
      t_sscc-quantidade = 1.
      t_sscc-uni = um_pal.
      t_sscc-lote_producao = c_ltap-charg.
      APPEND t_sscc.
      CLEAR t_sscc.

*      CLEAR ltak.
*      SELECT SINGLE *
*          FROM ltak
*              WHERE lgnum = c_ltap-lgnum AND
*                    tanum = c_ltap-tanum.
*
*      CLEAR aux_benum.
*      aux_benum = ltak-refnr.

      CLEAR aux_benum.
      aux_benum = it311-refnr.
      IF lv_2spart EQ abap_true.
        aux_benum = t_ltap-vbeln.
      ENDIF.

      CLEAR flag.

      DO 10 TIMES. "WHILE flag IS INITIAL.

        CALL FUNCTION 'ZWM_CREATE_TO'
          EXPORTING
            warehouse  = c_ltap-lgnum
            mov_type   = '940'
            material   = c_ltap-matnr
            quantity   = '1'
            unit       = 'PAL'
            plant      = c_ltap-werks
            s_loc      = c_ltap-lgort
            lote       = c_ltap-charg
            source_sty = c_ltap-vltyp
            source_bin = c_ltap-vlpla
            req_type   = 'I'
            req_number = aux_benum
            su         = zwm020-p2
          IMPORTING
            to         = to_remontada
          TABLES
            return_msg = return_msg
          EXCEPTIONS
            error      = 1
            OTHERS     = 2.

        IF sy-subrc = 0.
          flag = 'X'.
          EXIT.
        ELSE.
          CLEAR flag.
          WAIT UP TO 1 SECONDS.

          PERFORM complete_log_message USING 'REMONTADA_002'
                                             aux_benum
                                             zwm020-p1
                                             zwm020-p2
                                             c_ltap-matnr
                                       CHANGING return_msg[].

          CALL FUNCTION 'ZWM_LOG_MESSAGE'
            EXPORTING
              i_master    = c_ltap-lgnum
              i_object    = 'ZWM001'
              i_subobject = 'ZWM006'
              i_extnumber = '2'
              i_commit    = 'X'
              it_messages = return_msg[].
        ENDIF.

      ENDDO. "ENDWHILE.
    ENDIF.

    DELETE t_ltap WHERE vlenr = zwm020-p1 OR vlenr = zwm020-p2.
    CLEAR c_ltap.
    REFRESH c_ltap.

  ENDLOOP.

  READ TABLE it311 WITH KEY kreuz = 'X'.

  REFRESH: t_ltak, t_ltap.
  CLEAR: t_ltak, t_ltap.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = it311-lgnum
      i_refnr  = it311-refnr
    IMPORTING
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  DO 1 TIMES.
    CHECK lv_2spart EQ abap_true.

    SELECT * FROM t311a
             INTO TABLE lt_t311a
             WHERE lgnum = it311-lgnum AND
                   refnr = it311-refnr.

    CHECK sy-subrc EQ 0.

    SELECT * FROM ltak
             INTO TABLE lt_ltak
             FOR ALL ENTRIES IN lt_t311a
             WHERE lgnum = lt_t311a-lgnum AND
                   benum = lt_t311a-rbnum.

    CHECK sy-subrc EQ 0.
    DELETE lt_ltak WHERE kquit = abap_true.
    CHECK NOT lt_ltak IS INITIAL.

    SELECT * FROM ltap
             INTO TABLE lt_ltap
             FOR ALL ENTRIES IN lt_ltak
             WHERE lgnum = lt_ltak-lgnum AND
                   tanum = lt_ltak-tanum.

    CHECK sy-subrc EQ 0.
    DELETE lt_ltap WHERE pquit EQ abap_true.
    CHECK NOT lt_ltap IS INITIAL.

    CLEAR: lt_ltap_par.
    lt_ltap_sum = lt_ltap.

    LOOP AT lt_t311a INTO ls_t311a.
      LOOP AT lt_ltak INTO ls_ltak WHERE benum = ls_t311a-rbnum.
        LOOP AT lt_ltap INTO ls_ltap WHERE tanum = ls_ltak-tanum.
          CHECK ls_ltap-vltyp EQ 'DRI' OR
                ls_ltap-vltyp EQ 'BLK' OR
                ls_ltap-vltyp EQ 'TRI'.

          CHECK ls_ltap-letyp EQ 'P2' OR
                ls_ltap-letyp EQ 'P5'.

          READ TABLE lt_ltap_par
               WITH TABLE KEY tanum = ls_ltap-tanum
                              tapos = ls_ltap-tapos
               TRANSPORTING NO FIELDS.
          IF sy-subrc EQ 0.
            CONTINUE.
          ENDIF.

          CLEAR: n_pal.
          LOOP AT lt_ltap_sum INTO ls_ltap WHERE vltyp = ls_ltap-vltyp AND
                                                 vlpla = ls_ltap-vlpla AND
                                                 matnr = ls_ltap-matnr AND
                                                 charg = ls_ltap-charg.
            lv_tabix = sy-tabix.

            CLEAR: ls_ltak_sum.
            READ TABLE lt_ltak
                  INTO ls_ltak_sum
                  WITH TABLE KEY tanum = ls_ltap-tanum.
            CHECK sy-subrc EQ 0.

            IF ls_ltak_sum-benum <> ls_ltak-benum.
              CONTINUE.
            ENDIF.


            DELETE lt_ltap_sum INDEX lv_tabix.
            INSERT ls_ltap INTO TABLE lt_ltap_par.

            n_pal = n_pal + 1.
            IF n_pal EQ 2.
              EXIT.
            ENDIF.
          ENDLOOP.

          IF n_pal EQ 1.
            CALL FUNCTION 'ZWM_CREATE_TO'
              EXPORTING
                warehouse  = ls_ltap-lgnum
                mov_type   = '940'
                material   = ls_ltap-matnr
                quantity   = '1'
                unit       = 'PAL'
                plant      = ls_ltap-werks
                s_loc      = ls_ltap-lgort
                lote       = ls_ltap-charg
                source_sty = ls_ltap-vltyp
                source_bin = ls_ltap-vlpla
                req_type   = 'I'
                req_number = ls_ltak-benum
*               su         = zwm020-p2
*                IMPORTING
*               to         = to_remontada
              TABLES
                return_msg = return_msg
              EXCEPTIONS
                error      = 1
                OTHERS     = 2.

            IF sy-subrc <> 0.
              PERFORM complete_log_message USING 'REMONTADA_003'
                                                 ls_ltak-benum
                                                 '' ''
                                                 ls_ltap-matnr
                                           CHANGING return_msg[].

              CALL FUNCTION 'ZWM_LOG_MESSAGE'
                EXPORTING
                  i_master    = ls_ltap-lgnum
                  i_object    = 'ZWM001'
                  i_subobject = 'ZWM006'
                  i_extnumber = '3'
                  i_commit    = 'X'
                  it_messages = return_msg[].
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
  ENDDO.




  SELECT * INTO TABLE t_ltak
  FROM ltak
  WHERE lgnum = it311-lgnum AND
  benum = it311-refnr AND
  betyp = 'Z'.

  CHECK NOT t_ltak[] IS INITIAL.

  SELECT * FROM ltap INTO TABLE t_ltap
  FOR ALL ENTRIES IN t_ltak
  WHERE lgnum = t_ltak-lgnum AND
  tanum = t_ltak-tanum.

  DELETE t_ltap WHERE vorga = 'ST'.

*  SELECT * INTO TABLE t_ltak
*      FROM ltak
*          WHERE lgnum = it311-lgnum AND
*                benum = it311-refnr AND
*                betyp = 'Z'.
*
*  LOOP AT t_ltak.
*    SELECT * APPENDING TABLE t_ltap
*        FROM ltap
*            WHERE lgnum = t_ltak-lgnum AND
*                  tanum = t_ltak-tanum AND
*                  vorga <> 'ST'.
*  ENDLOOP.

  CLEAR c_ltap.
  REFRESH c_ltap.
** Verifcar se tem paletes remontadas vindas dos trilaterais
** para o reabastecimento
  LOOP AT t_ltap
  WHERE vltyp = 'TRI' AND
  ( letyp = 'P2' OR letyp = 'P5' ) AND
  vbeln = ' '.

    CLEAR zwm020.
    SELECT SINGLE * FROM zwm020
    WHERE armazem = t_ltap-lgnum AND
    ( p1 = t_ltap-vlenr OR p2 = t_ltap-vlenr ).

    IF sy-subrc = 0.
      CLEAR to_prm.
      CLEAR c_ltap.
      REFRESH c_ltap.
      MOVE-CORRESPONDING t_ltap TO c_ltap.
      APPEND c_ltap.
      CLEAR c_ltap.

      IF zwm020-p1 = t_ltap-vlenr.
** Verificar se existem duas to´s para as duas paletes remontadas

** Se tiverem as duas paletes tem de se estornar as duas tos e voltar a
** criar as duas to's sendo a primeira a da to de baixo

        READ TABLE t_ltap WITH KEY vlenr = zwm020-p2.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING t_ltap TO c_ltap.
          APPEND c_ltap.
          CLEAR c_ltap.
        ENDIF.

      ELSEIF zwm020-p2 = t_ltap-vlenr.

        READ TABLE t_ltap WITH KEY vlenr = zwm020-p1.
        IF sy-subrc = 0.
          MOVE-CORRESPONDING t_ltap TO c_ltap.
          APPEND c_ltap.
          CLEAR c_ltap.
        ENDIF.
      ENDIF.
    ENDIF.

** Verificar se estao as duas paletes remontadas para a saida.
** Se for as duas paletes para saida verificar se ja estao criadas pela
** ordem correcta
    CLEAR n_pal.

    DESCRIBE TABLE c_ltap LINES n_pal.

    IF n_pal = 2.
      SORT c_ltap BY lgnum tanum tapos.
      READ TABLE c_ltap INDEX 1.
      IF c_ltap-vlenr = zwm020-p1.
        CONTINUE.
      ENDIF.
    ENDIF.

    CLEAR t_sscc.
    REFRESH t_sscc.
    LOOP AT c_ltap.

**************************
**   Cancelar as OT
**************************

      CLEAR t_ltap_cancl.
      REFRESH t_ltap_cancl.

      t_ltap_cancl-tanum = c_ltap-tanum.
      t_ltap_cancl-tapos = c_ltap-tapos.
      APPEND t_ltap_cancl.
      CLEAR t_ltap_cancl.

      CALL FUNCTION 'ZWM_CANCEL_TO'
        EXPORTING
          armazem      = c_ltap-lgnum
        TABLES
          t_ltap_cancl = t_ltap_cancl
        EXCEPTIONS
          error        = 1
          OTHERS       = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    ENDLOOP.

**************************
**   Tabela para Criar Novas OT´s
**************************
    READ TABLE c_ltap WITH KEY vlenr = zwm020-p1.
    IF sy-subrc = 0.
      t_sscc-sscc = c_ltap-vlenr.
      t_sscc-tipo_su = c_ltap-letyp.
      t_sscc-material = c_ltap-matnr.
      t_sscc-quantidade = c_ltap-vsola.
      t_sscc-uni = c_ltap-altme.
      t_sscc-lote_producao = c_ltap-charg.
      APPEND t_sscc.
      CLEAR t_sscc.
    ENDIF.

    READ TABLE t_ltak WITH KEY lgnum = c_ltap-lgnum
    tanum = c_ltap-tanum.

    CLEAR c_ltap.
    READ TABLE c_ltap WITH KEY vlenr = zwm020-p2.
    IF sy-subrc = 0.
      t_sscc-sscc = c_ltap-vlenr.
      t_sscc-tipo_su = c_ltap-letyp.
      t_sscc-material = c_ltap-matnr.
      t_sscc-quantidade = c_ltap-vsola.
      t_sscc-uni = c_ltap-altme.
      t_sscc-lote_producao = c_ltap-charg.
      APPEND t_sscc.
      CLEAR t_sscc.
    ENDIF.

    CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse      = c_ltap-lgnum
        mov_type       = '972'
        st_type_o      = c_ltap-vltyp
        bin_origem     = c_ltap-vlpla
        st_type_d      = c_ltap-nltyp
        bin_destino    = c_ltap-nlpla
        plant          = c_ltap-werks
        s_loc          = c_ltap-lgort
        origem         = 'X'
        req_number     = t_ltak-benum
        req_type       = t_ltak-betyp
        sscc_adicional = t_ltak-lznum
      IMPORTING
        to             = to
      TABLES
        return_msg     = return_msg
        sscc           = t_sscc
      EXCEPTIONS
        error          = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

    CLEAR t_sscc.
    REFRESH t_sscc.

*    ENDLOOP.

    DELETE t_ltap WHERE vlenr = zwm020-p1 OR vlenr = zwm020-p2.
    CLEAR c_ltap.
    REFRESH c_ltap.

  ENDLOOP.

ENDFORM.                    " verify_paletes_remontadas
*&---------------------------------------------------------------------*
*&      Form  verify_volum_pal_picking
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verify_volum_pal_picking .

***********************************************************************
** DESATIVADO
***********************************************************************

**  DATA w_zwm026 LIKE zwm026.
**
**  DATA : volume_maximo_aux  LIKE zwm001-valor,
**         volume_maximo      LIKE mara-volum,
**         num_unidades       TYPE i,
**         num_unidades_resto TYPE i.
**
**  CLEAR: t_ltak, t_ltap, volume_maximo, volume_maximo_aux,
**         num_unidades, num_unidades_resto.
**
**  REFRESH: t_ltak, t_ltap.
**
**  SELECT SINGLE valor INTO volume_maximo_aux
**      FROM zwm001
**          WHERE armazem = it311-lgnum AND
**                processo = 'PICKING' AND
**                parametro = 'VOLUME_PAL_PICKING'.
**
**** Retirar ao volume maximo o volume da palete
**  SELECT SINGLE * FROM mara WHERE matnr = 'PCHEP'.
**
**  volume_maximo_aux = volume_maximo_aux - mara-volum.
**
**  MOVE volume_maximo_aux TO volume_maximo.
**
**  READ TABLE it311 WITH KEY kreuz = 'X'.
**
**  SELECT * INTO TABLE t_ltak
**      FROM ltak
**          WHERE lgnum = it311-lgnum AND
**                refnr = it311-refnr AND
**                kquit = ' '.
**
**  CHECK NOT t_ltak[] IS INITIAL.
**
**  SELECT * FROM ltap INTO TABLE t_ltap
**      FOR ALL ENTRIES IN t_ltak
**            WHERE lgnum = t_ltak-lgnum AND
**                  tanum = t_ltak-tanum.
**
**  DELETE t_ltap WHERE vltyp <> 'PCK' AND vltyp <> 'PKB'.
**
**  LOOP AT t_ltap WHERE volum > volume_maximo.
**
**    CLEAR: mara,
**           num_unidades,
**           num_unidades_resto,
**           return_msg.
**
**    REFRESH return_msg.
**
**    SELECT SINGLE *
**        FROM mara
**            WHERE matnr = t_ltap-matnr.
**
**    num_unidades = volume_maximo DIV mara-volum.
**    num_unidades_resto = t_ltap-vsola - num_unidades.
**
**** Estornar a OT
**    CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
**      EXPORTING
**        warehouse     = t_ltap-lgnum
**        tanum         = t_ltap-tanum
**        tapos         = t_ltap-tapos
**      TABLES
**        return_msg    = return_msg
**      EXCEPTIONS
**        error_message = 1
**        OTHERS        = 2.
**    IF sy-subrc <> 0.
**      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**    ENDIF.
**
**** Apagar a entrada na tabela ZWM026 que tem a OT que foi estornada
**    CLEAR w_zwm026.
**    SELECT SINGLE * INTO w_zwm026
**        FROM zwm026
**            WHERE armazem = t_ltap-lgnum AND
**                  to_number = t_ltap-tanum.
**
**    IF sy-subrc = 0.
**      CLEAR w_zwm026-to_number.
**      w_zwm026-quantidade = num_unidades.
**      UPDATE zwm026 FROM w_zwm026.
**      COMMIT WORK.
**    ENDIF.
**
**** Criar duas OT´s uma com o num_unidades e outra com num_unidades_resto
**    SORT t_ltak BY tanum.
**    READ TABLE t_ltak WITH KEY lgnum = t_ltap-lgnum
**                               tanum = t_ltap-tanum.
**
**    t_ltap-vsola = num_unidades.
**
***    CALL FUNCTION 'ZWM_CREATE_TO_DELIVERY'
**    CALL FUNCTION 'ZWM_TO_CREATE_OUT'
**      EXPORTING
**        warehouse     = t_ltap-lgnum
**        refnr         = t_ltak-refnr
**        vbeln         = t_ltak-vbeln
**        posnr         = t_ltap-posnr
**        vsola         = t_ltap-vsola
**        meins         = t_ltap-altme
**        werks         = t_ltap-werks
**        lgort         = t_ltap-lgort
**        matnr         = t_ltap-matnr
**      IMPORTING
**        to            = to
**      TABLES
**        return_msg    = return_msg
**      EXCEPTIONS
**        error_message = 1
**        OTHERS        = 2.
**    IF sy-subrc <> 0.
**      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**    ENDIF.
**
**    t_ltap-vsola = num_unidades_resto.
**
***    CALL FUNCTION 'ZWM_CREATE_TO_DELIVERY'
**    CALL FUNCTION 'ZWM_TO_CREATE_OUT'
**      EXPORTING
**        warehouse     = t_ltap-lgnum
**        refnr         = t_ltak-refnr
**        vbeln         = t_ltak-vbeln
**        posnr         = t_ltap-posnr
**        vsola         = t_ltap-vsola
**        meins         = t_ltap-altme
**        werks         = t_ltap-werks
**        lgort         = t_ltap-lgort
**        matnr         = t_ltap-matnr
**      IMPORTING
**        to            = to
**      TABLES
**        return_msg    = return_msg
**      EXCEPTIONS
**        error_message = 1
**        OTHERS        = 2.
**    IF sy-subrc <> 0.
**      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
**    ENDIF.
**
**  ENDLOOP.

ENDFORM.                    " verify_volum_pal_picking
*&--------------------------------------------------------------------*
*&      Form  verify_picking_quantity
*&--------------------------------------------------------------------*
*       text
*---------------------------------------------------------------------*
FORM verify_picking_quantity CHANGING ct_paletes TYPE zpalete_picking_tt.

  DATA: lt_lips LIKE lips OCCURS 0 WITH HEADER LINE,
        lt_vbss LIKE vbss OCCURS 0 WITH HEADER LINE,
        lt_ltak LIKE ltak OCCURS 0 WITH HEADER LINE,
        lt_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.

  DATA: lt_paletes LIKE zpalete_picking OCCURS 10 WITH HEADER LINE.
  DATA: lt_paletes_pkl LIKE zpalete_picking OCCURS 10 WITH HEADER LINE.
*  DATA: ls_zwm033 LIKE zwm033.

  DATA: BEGIN OF lt_grupos OCCURS 0,
          sammg LIKE vbss-sammg,
        END OF lt_grupos,
        ls_grupos LIKE lt_grupos.

  DATA: BEGIN OF lt_totais OCCURS 0,
          matnr LIKE lips-matnr,
          werks LIKE lips-werks,
          lgort LIKE lips-lgort,
          quant LIKE zpalete_picking-uni_incompleta,
        END OF lt_totais,
        ls_totais LIKE lt_totais,
        BEGIN OF lk_totais,
          vbeln LIKE lips-vbeln,
          matnr LIKE lips-matnr,
        END OF lk_totais.

  DATA: BEGIN OF lt_totais_lote OCCURS 0,
          matnr LIKE ltap-matnr,
          werks LIKE ltap-werks,
          lgort LIKE ltap-lgort,
          charg LIKE ltap-charg,
          vsola LIKE ltap-vsola,
        END OF lt_totais_lote,
        ls_totais_lote LIKE lt_totais_lote.

  DATA: BEGIN OF lt_totais_lo OCCURS 0,
          matnr LIKE ltap-matnr,
          charg LIKE ltap-charg,
          vsola LIKE ltap-vsola,
        END OF lt_totais_lo,
        ls_totais_lo LIKE lt_totais_lo.

  DATA: l_tanum LIKE ltak-tanum,
*        l_werks LIKE lqua-werks,
*        l_lgort LIKE lqua-lgort,
        l_meins LIKE lqua-meins.

  DATA: lt_return_msg LIKE bdcmsgcoll OCCURS 0 WITH HEADER LINE.

  DATA: lt_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

  DATA: lt_zwm001 LIKE zwm001 OCCURS 0 WITH HEADER LINE.

  DATA: l_quant     LIKE zpalete_picking-uni_incompleta,
        l_quant_pkr LIKE zpalete_picking-uni_incompleta,
        l_quant_pkl LIKE zpalete_picking-uni_incom_p,
        lv_qtd      TYPE menge_d,
        lv_verme    TYPE menge_d.

  DATA: l_quant_aux TYPE i.

  DATA: l_mov_type  LIKE ltak-bwlvs,
        l_mov_type1 LIKE ltak-bwlvs,
        l_mov_type2 LIKE ltak-bwlvs,
        l_mov       LIKE ltak-bwlvs,
        l_mov11     LIKE ltak-bwlvs,
        l_mov12     LIKE ltak-bwlvs,
        l_mov21     LIKE ltak-bwlvs,
        l_mov22     LIKE ltak-bwlvs,
        l_movpkl    LIKE ltak-bwlvs,
        l_lgtyp     LIKE mlgt-lgtyp,
        l_plkpt     LIKE mlgn-plkpt,
        l_betyp     LIKE ltak-betyp,
        l_pstyv1    LIKE lips-pstyv,
        l_pstyv2    LIKE lips-pstyv.

  DATA: lv_lines TYPE sytabix.

  DATA: lv_refnr LIKE ltak-refnr.

  DATA: ls_marm TYPE marm.

  REFRESH: lt_vbss,   lt_lips,
           lt_grupos, lt_totais, lt_paletes.

  LOOP AT it311.
    CHECK it311-kreuz = con_x.
    ls_grupos-sammg = it311-sammg.
    COLLECT ls_grupos INTO lt_grupos.
  ENDLOOP.

  SELECT * FROM zwm001 INTO TABLE lt_zwm001
                 WHERE armazem   = lgnum
                   AND processo  = 'REABASTECIMENTO'.
  IF lt_zwm001[] IS INITIAL.
*    Não existem parametros definidos para este processo (tabela &)
    MESSAGE i156(zwmmsg001) WITH 'ZWM001'.
    EXIT.
  ENDIF.

  CLEAR lt_zwm001.
  LOOP AT lt_zwm001.
    CASE lt_zwm001-parametro.
      WHEN 'ST_PCK'.
        l_lgtyp = lt_zwm001-valor.
      WHEN 'ST_BET'.
        l_betyp = lt_zwm001-valor.
      WHEN 'ST_PKB'.
        l_plkpt = lt_zwm001-valor.
      WHEN 'MOV1'.
        l_movpkl = lt_zwm001-valor.
      WHEN 'ST_MOV'.
        l_mov = lt_zwm001-valor.
      WHEN 'ST_MOV11'.
        l_mov11 = lt_zwm001-valor.
      WHEN 'ST_MOV12'.
        l_mov12 = lt_zwm001-valor.
      WHEN 'ST_MOV21'.
        l_mov21 = lt_zwm001-valor.
      WHEN 'ST_MOV22'.
        l_mov22 = lt_zwm001-valor.
      WHEN 'ST_PST1'.
        l_pstyv1 = lt_zwm001-valor.
      WHEN 'ST_PST2'.
        l_pstyv2 = lt_zwm001-valor.
    ENDCASE.
  ENDLOOP.

*  CHECK NOT lt_grupos[] IS INITIAL.
*  SORT lt_grupos.
*
*  SELECT * FROM vbss INTO TABLE lt_vbss
*              FOR ALL ENTRIES IN lt_grupos
*                WHERE sammg = lt_grupos-sammg.
*
*  REFRESH: lt_grupos.

* Só haverá 1 grupo selecionado
  READ TABLE lt_grupos INDEX 1.
  CHECK sy-subrc = 0.

  SELECT * FROM vbss INTO TABLE lt_vbss
                WHERE sammg = lt_grupos-sammg.

  CHECK NOT lt_vbss[] IS INITIAL.

* Eliminar as remessas que ja têm to´s criadas
  CLEAR vbuk.
  LOOP AT lt_vbss.
    SELECT SINGLE *
        FROM vbuk
            WHERE vbeln = lt_vbss-vbeln.
    IF vbuk-lvstk = 'B'.
*    IF vbuk-lvstk = 'C'.
      DELETE lt_vbss WHERE vbeln = vbuk-vbeln.
      CLEAR lt_vbss.
    ENDIF.
  ENDLOOP.

** Garantir que existem remessas a selecionar
  IF lt_vbss[] IS INITIAL.
* As remessas do grupo & já tem todas as to´s criadas.
    MESSAGE i210(zwmmsg001) WITH lt_grupos-sammg.
  ENDIF.

  CHECK NOT lt_vbss[] IS INITIAL.

  SELECT * FROM lips INTO TABLE lt_lips
              FOR ALL ENTRIES IN lt_vbss
                WHERE vbeln = lt_vbss-vbeln.

***************************************************
* Eliminar itens que não sejam do CD - Inicio
***************************************************
*  DELETE lt_lips WHERE lgort <> 'CD'.
  CALL FUNCTION 'Z_WM_FILTER_TABLE_TO_WM'
    EXPORTING
      i_lgnum  = lgnum
    CHANGING
      ct_table = lt_lips[].


* PSTYV = ZPAL or ZPAS
  DELETE lt_lips WHERE ( pstyv = l_pstyv1 OR pstyv = l_pstyv2 ).

  REFRESH: lt_vbss.

  LOOP AT lt_lips.
    CLEAR lt_paletes.
    MOVE-CORRESPONDING lt_lips TO lt_paletes.
    lt_paletes-refnr = lt_grupos-sammg.
    APPEND lt_paletes.
  ENDLOOP.

  REFRESH: lt_lips.

**   Cálculo das paletes
  CALL FUNCTION 'ZWM_PAL_PICKING'
    EXPORTING
      armazem         = lgnum
**      it_vbeln_vl     = lt_lips[]
      actualiza       = ' '
    TABLES
      zpalete_picking = lt_paletes[].

  REFRESH: lt_lips.

  DESCRIBE TABLE lt_paletes LINES lv_lines.

  LOOP AT lt_paletes.

    IF sy-tabix EQ lv_lines AND
       lt_paletes-pal_picking EQ 0 AND
       lt_paletes-pal_completa EQ 0.
      MESSAGE i000 DISPLAY LIKE 'E' WITH 'Contactar ROFF. Erro: Palete não terminada, OT Pendente'.
    ENDIF.

    CLEAR ls_totais.
    ls_totais-matnr = lt_paletes-matnr.
    ls_totais-werks = lt_paletes-werks.
    ls_totais-lgort = lt_paletes-lgort.
    ls_totais-quant = lt_paletes-uni_incompleta - lt_paletes-uni_incom_p.
    COLLECT ls_totais INTO lt_totais.
  ENDLOOP.

*  REFRESH lt_paletes.

** Picking Loja Online
*  PERFORM verify_picking_pkl.
  DATA lt_lqua LIKE lqua OCCURS 0 WITH HEADER LINE.

*& Begin of Modification by Tiago Pateiro - ROFF @ 28.12.2015 17:12:04
**  SELECT werks UP TO 1 ROWS
**    FROM t320 INTO l_werks
**    WHERE lgort EQ 'CD'
**      AND lgnum EQ lgnum.
**  ENDSELECT.
**  IF sy-subrc NE 0.
**    l_werks = 'RENV'.
**  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 28.12.2015 17:12:05

  CLEAR lt_paletes_pkl.
  REFRESH lt_paletes_pkl.

  lt_paletes_pkl[] = lt_paletes[].
*  LOOP AT lt_paletes WHERE uni_incom_p IS NOT INITIAL AND werks = 'RENV' AND lgort = 'CD'. " << DEL ROFF(SDF):TMGP:28.12.2015 17:17:14
  LOOP AT lt_paletes WHERE uni_incom_p IS NOT INITIAL. " << INS ROFF(SDF):TMGP:28.12.2015 17:17:16


    PERFORM get_plant_data USING lt_paletes-refnr CHANGING lt_paletes-werks lt_paletes-lgort.
    CHECK lt_paletes-lgort EQ lt_paletes-lgort.

    CLEAR lt_paletes-uni_incom_p.
    LOOP AT lt_paletes_pkl WHERE uni_incom_p IS NOT INITIAL
                           AND werks = lt_paletes-werks
                           AND lgort = lt_paletes-lgort
                           AND matnr = lt_paletes-matnr.
      lt_paletes-uni_incom_p = lt_paletes-uni_incom_p + lt_paletes_pkl-uni_incom_p.
    ENDLOOP.

    REFRESH: lt_return_msg, lt_sscc, lt_lqua.
    CLEAR: lt_return_msg, lt_sscc, lt_lqua.
    CLEAR l_tanum.


    SELECT * FROM lqua INTO TABLE lt_lqua
            WHERE lgnum = lgnum
              AND lgtyp = 'PKL'
              AND matnr = lt_paletes-matnr
              AND werks = lt_paletes-werks
              AND lgort = lt_paletes-lgort
              AND lenum = ' '.

    IF lt_paletes-charg IS NOT INITIAL.
      DELETE lt_lqua WHERE charg <> lt_paletes-charg.
    ENDIF.

    DATA l_quant_aux_pkl LIKE zpalete_picking-uni_incom_p.

    CLEAR l_quant_pkl.
    IF NOT lt_lqua[] IS INITIAL.

      LOOP AT lt_lqua.

        CLEAR ls_marm.
        SELECT SINGLE * FROM marm
                  INTO ls_marm
                  WHERE matnr = lt_lqua-matnr AND
                        meinh = lt_paletes-vrkme.

        lv_qtd = ( lt_lqua-verme * ls_marm-umren ) / ls_marm-umrez.

        CALL FUNCTION 'ROUND'
          EXPORTING
            input         = lv_qtd
            sign          = '-'
          IMPORTING
            output        = lv_verme
          EXCEPTIONS
            input_invalid = 1
            overflow      = 2
            type_invalid  = 3
            OTHERS        = 4.

        IF lv_verme <= 1.
          CONTINUE.
        ENDIF.

        CLEAR l_quant_aux_pkl.
        l_quant_aux_pkl = lt_lqua-verme.
        l_quant_pkl = l_quant_pkl + l_quant_aux_pkl.
      ENDLOOP.
    ENDIF.

    CHECK l_quant_pkl <= lt_paletes-uni_incom_p.

    lt_sscc-material      = lt_paletes-matnr.
    lt_sscc-quantidade    = 1.
    lt_sscc-uni           = l_meins.
    APPEND lt_sscc.

    CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse   = lgnum
        mov_type    = l_movpkl                          "979
        plant       = lt_paletes-werks
        s_loc       = lt_paletes-lgort
        certificado = 'X'
        req_number  = lt_paletes-refnr
        req_type    = l_betyp
      IMPORTING
        to          = l_tanum
      TABLES
        return_msg  = lt_return_msg
        sscc        = lt_sscc
      EXCEPTIONS
        error       = 1
        OTHERS      = 2.

    IF l_tanum IS INITIAL.
      CLEAR ls_totais.
      READ TABLE lt_totais INTO ls_totais WITH KEY matnr = lt_paletes-matnr.
      IF sy-subrc = 0.
        ls_totais-quant = lt_paletes-uni_incom_p.
        COLLECT ls_totais INTO lt_totais.
      ENDIF.
    ELSE.
      DO 10 TIMES.

        CLEAR: l_quant_pkl, lt_lqua.
        REFRESH lt_lqua.

        SELECT * FROM lqua INTO TABLE lt_lqua
        WHERE lgnum = lgnum
          AND lgtyp = 'PKL'
          AND matnr = lt_paletes-matnr
          AND werks = lt_paletes-werks
          AND lgort = lt_paletes-lgort
          AND lenum = ' '.

        IF lt_paletes-charg IS NOT INITIAL.
          DELETE lt_lqua WHERE charg <> lt_paletes-charg.
        ENDIF.

        IF NOT lt_lqua[] IS INITIAL.
          LOOP AT lt_lqua.

            CLEAR ls_marm.
            SELECT SINGLE * FROM marm
                      INTO ls_marm
                      WHERE matnr = lt_lqua-matnr AND
                            meinh = lt_paletes-vrkme.

            lv_qtd = ( lt_lqua-verme * ls_marm-umren ) / ls_marm-umrez.

            CALL FUNCTION 'ROUND'
              EXPORTING
                input         = lv_qtd
                sign          = '-'
              IMPORTING
                output        = lv_verme
              EXCEPTIONS
                input_invalid = 1
                overflow      = 2
                type_invalid  = 3
                OTHERS        = 4.

            IF lv_verme <= 1.
              CONTINUE.
            ENDIF.

            CLEAR l_quant_aux_pkl.
            l_quant_aux_pkl = lt_lqua-verme.
            l_quant_pkl = l_quant_pkl + l_quant_aux_pkl.
          ENDLOOP.
        ENDIF.

        IF l_quant_pkl < lt_paletes-uni_incom_p.
          WAIT UP TO 1 SECONDS.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.
  ENDLOOP.

  IF NOT lt_totais[] IS INITIAL.
**  Verificar quantas posicoes vazias existem no rep

    SORT lt_totais BY quant DESCENDING.

** So fazer esta validação no caso de existir quantidades incompletas
    READ TABLE lt_totais INDEX 1.
    CHECK NOT lt_totais-quant IS INITIAL.

    DATA n_posi TYPE i.
    CLEAR n_posi.

*& Begin of Modification by Tiago Pateiro - ROFF @ 07.01.2016 17:42:58
    SELECT COUNT(*)
      FROM lagp INTO n_posi
      WHERE lgnum EQ lgnum
        AND lgtyp EQ 'REP'.
    IF n_posi NE 1.
      CLEAR n_posi.
*& End of Modification by Tiago Pateiro - ROFF @ 07.01.2016 17:42:58

      SELECT COUNT(*) FROM lagp INTO n_posi
          WHERE lgnum = lgnum AND
                lgtyp = 'REP' AND
                kzler = 'X' AND
                kzvol = ' ' AND
                anzqu = 0   AND
                skzue = ' ' AND
                skzsi = ' ' AND
                skzse = ' '.


      IF n_posi <= 15.
        flag = 'X'.
        MESSAGE ID 'ZWMMSG001' TYPE 'I'
              NUMBER '206' WITH it311-refnr.
        EXIT.
      ENDIF.
*& Begin of Modification by Tiago Pateiro - ROFF @ 07.01.2016 17:47:41
    ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 07.01.2016 17:47:41
  ENDIF.

  SORT lt_totais BY matnr.

  LOOP AT lt_totais.

    PERFORM get_plant_data USING lt_grupos-sammg CHANGING lt_totais-werks lt_totais-lgort.

    CHECK NOT lt_totais-quant IS INITIAL.
    CLEAR l_quant.


*& Begin of Modification by Tiago Pateiro - ROFF @ 28.12.2015 17:12:04
**    SELECT werks UP TO 1 ROWS
**      FROM t320 INTO l_werks
**      WHERE lgort EQ l_lgort
**        AND lgnum EQ lgnum.
**    ENDSELECT.
*& End of Modification by Tiago Pateiro - ROFF @ 28.12.2015 17:12:05

    CLEAR mara.
    SELECT SINGLE * FROM mara WHERE matnr = lt_totais-matnr.
    CHECK sy-subrc = 0.

    CLEAR l_meins.
    CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
      EXPORTING
        input          = mara-meins
      IMPORTING
        output         = l_meins
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      l_meins = mara-meins.
    ENDIF.

    l_mov_type = l_mov.  "972

    CLEAR: mlgn, mlgt, lqua.
    SELECT SINGLE * FROM mlgn
            WHERE matnr = lt_totais-matnr
              AND lgnum = lgnum.
    IF mlgn-plkpt = l_plkpt.
      l_mov_type1 = l_mov21.  "976
      l_mov_type2 = l_mov22.  "975
    ELSE.
      l_mov_type1 = l_mov11.  "971
      l_mov_type2 = l_mov12.  "970
    ENDIF.
    SELECT SINGLE * FROM mlgt
            WHERE matnr = lt_totais-matnr
              AND lgnum = lgnum
              AND lgtyp = l_lgtyp.
    CHECK sy-subrc = 0.
    IF mlgn-plkpt IS INITIAL.
      CHECK NOT mlgt-lgpla IS INITIAL.
    ENDIF.

    DATA t_lqua LIKE lqua OCCURS 0 WITH HEADER LINE.

** Verificar stock no picking fixo
    REFRESH t_lqua.
    CLEAR: t_lqua, l_quant.

    IF mlgn-plkpt IS INITIAL.
      SELECT * FROM lqua INTO TABLE t_lqua
              WHERE lgnum = mlgt-lgnum
                AND lgtyp = mlgt-lgtyp
                AND lgpla = mlgt-lgpla
                AND werks = lt_totais-werks
                AND lgort = lt_totais-lgort
                AND lenum = ' '.

      IF NOT t_lqua[] IS INITIAL.
        LOOP AT t_lqua.
          CLEAR l_quant_aux.
          l_quant_aux = trunc( t_lqua-verme ).
          l_quant = l_quant + l_quant_aux.
        ENDLOOP.
      ENDIF.

    ELSE.
** Verificar stock no picking variavel
      SELECT * FROM lqua INTO TABLE t_lqua
              WHERE lgnum = mlgt-lgnum
                AND lgtyp = 'PKB'
                AND matnr = lt_totais-matnr
                AND werks = lt_totais-werks
                AND lgort = lt_totais-lgort
*                AND lgtyp = mlgt-lgtyp
*                AND lgpla = mlgt-lgpla
                AND lenum = ' '.

      IF NOT t_lqua[] IS INITIAL.
        LOOP AT t_lqua.
          CLEAR l_quant_aux.
          l_quant_aux = trunc( t_lqua-verme ).
          l_quant = l_quant + l_quant_aux.
        ENDLOOP.
      ENDIF.
    ENDIF.

    CHECK l_quant < lt_totais-quant.

* Verificar se já foi efectuado algum reabastecimento
    DATA: t_ltak LIKE ltak OCCURS 0 WITH HEADER LINE,
          t_ltap LIKE ltap OCCURS 0 WITH HEADER LINE.

    DATA qtd_reab LIKE ltap-vsolm.
    CLEAR qtd_reab.

    IF mlgn-plkpt IS INITIAL.
      SELECT * INTO TABLE t_ltak
          FROM ltak
              WHERE lgnum = lgnum AND
                    betyp = 'Z' AND
                    benum = lt_grupos-sammg.

      LOOP AT t_ltak.
        CLEAR: ltap.
        SELECT *
            FROM ltap
                WHERE lgnum = t_ltak-lgnum AND
                      tanum = t_ltak-tanum AND
                      matnr = lt_totais-matnr AND
                      werks = lt_totais-werks AND
                      lgort = lt_totais-lgort AND
                      vorga <> 'ST' AND
                      pquit <> 'X'.

          qtd_reab = qtd_reab + ltap-vsolm.
        ENDSELECT.
      ENDLOOP.

    ELSE.

      CLEAR: t_ltak, t_ltap.
      REFRESH:  t_ltak, t_ltap.
      SELECT * INTO TABLE t_ltak
               FROM ltak
                   WHERE lgnum = lgnum AND
                         betyp = 'Z' AND
                         kquit = ' '.
*                         benum = lt_grupos-sammg.

      DELETE t_ltak WHERE bwlvs = '976'.

      IF NOT t_ltak[] IS INITIAL.

        SELECT * FROM ltap INTO TABLE t_ltap
            FOR ALL ENTRIES IN t_ltak
                  WHERE lgnum = t_ltak-lgnum AND
                        tanum = t_ltak-tanum.

        DELETE t_ltap WHERE matnr <> lt_totais-matnr
                         OR vorga = 'ST'
                         OR pquit = 'X'.

      ENDIF.
*      LOOP AT t_ltak.
*        CLEAR: ltap.
*        SELECT * APPENDING CORRESPONDING FIELDS OF TABLE t_ltap
*            FROM ltap
*                WHERE lgnum = t_ltak-lgnum AND
*                      tanum = t_ltak-tanum AND
*                      matnr = lt_totais-matnr AND
*                      vorga <> 'ST' AND
*                      pquit <> 'X'.
*
*      ENDLOOP.
      IF lgnum <> '150'.
        IF NOT t_ltap[] IS INITIAL.
          flag = 'X'.
          SORT: t_ltak, t_ltap.
          LOOP AT t_ltap.
            READ TABLE t_ltak WITH KEY lgnum = t_ltap-lgnum
                                       tanum = t_ltap-tanum.
            MESSAGE ID 'ZWMMSG001' TYPE 'I'
                  NUMBER '266' WITH t_ltak-benum t_ltap-matnr.
          ENDLOOP.
          EXIT.
        ENDIF.
      ENDIF.

    ENDIF.

    CHECK qtd_reab < lt_totais-quant.
******************************************************
** Verfificar quantidade na material na reserva de picking (PKR)

    REFRESH t_lqua.
    CLEAR: t_lqua, l_quant_pkr, l_quant_aux.

    IF mlgn-plkpt IS INITIAL.
      SELECT * FROM lqua INTO TABLE t_lqua
              WHERE lgnum = mlgt-lgnum
                AND lgtyp = 'PKR'
                AND matnr = lt_totais-matnr
                AND werks = lt_totais-werks
                AND lgort = lt_totais-lgort.

      IF NOT t_lqua[] IS INITIAL.
        LOOP AT t_lqua.
          CLEAR l_quant_aux.
          l_quant_aux = trunc( t_lqua-verme ).
          l_quant_pkr = l_quant_pkr + l_quant_aux.
        ENDLOOP.
      ENDIF.

    ELSE.
** Verificar stock na reserva do picking variavel
      SELECT * FROM lqua INTO TABLE t_lqua
              WHERE lgnum = mlgt-lgnum
                AND lgtyp = 'PRB'
                AND matnr = lt_totais-matnr
                AND werks = lt_totais-werks
                AND lgort = lt_totais-lgort.

      IF NOT t_lqua[] IS INITIAL.
        LOOP AT t_lqua.
          CLEAR l_quant_aux.
          l_quant_aux = trunc( t_lqua-verme ).
          l_quant_pkr = l_quant_pkr + l_quant_aux.
        ENDLOOP.
      ENDIF.
    ENDIF.

    REFRESH: lt_return_msg, lt_sscc.
    CLEAR: lt_return_msg, lt_sscc.
    CLEAR l_tanum.

*   lt_sscc-sscc          = ' '.
*   lt_sscc-tipo_su       = ' '.
    lt_sscc-material      = lt_totais-matnr.
*   lt_sscc-variante      = ' '.

** so vou pedir a diferenca do que quero com o que esta no picking
** no maximo o que estiver no PKR
    lt_sscc-quantidade    = lt_totais-quant - l_quant.

    IF lt_sscc-quantidade > l_quant_pkr.
      lt_sscc-quantidade  = l_quant_pkr.
    ENDIF.

    lt_sscc-uni           = l_meins.
*   lt_sscc-altura        = ' '.
*   lt_sscc-lote_producao = ' '.
*    APPEND lt_sscc.

    CLEAR mlgn.
    SELECT SINGLE *
        FROM mlgn
            WHERE matnr = lt_totais-matnr AND
                  lgnum = lgnum.
    IF sy-subrc = 0.
      IF mlgn-lety1 = 'P2' OR mlgn-lety1 = 'P5'.
        IF l_mov_type1 = '971'.
          lt_sscc-quantidade    = 2.
          lt_sscc-uni           = 'PAL'.
        ENDIF.
      ENDIF.
    ENDIF.
    APPEND lt_sscc.

    CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
      EXPORTING
        warehouse  = lgnum
        mov_type   = l_mov_type1                            "971 ou 976
        plant      = lt_totais-werks
        s_loc      = lt_totais-lgort
        req_number = lt_grupos-sammg
        req_type   = l_betyp
      IMPORTING
        to         = l_tanum
      TABLES
        return_msg = lt_return_msg
        sscc       = lt_sscc
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    IF NOT l_tanum IS INITIAL.
      CLEAR ltak.
      SELECT * FROM ltak INTO TABLE lt_ltak
                          WHERE lgnum = lgnum
                            AND betyp = l_betyp
                            AND benum = lt_grupos-sammg
                            AND kquit = ' '.

      IF NOT lt_ltak[] IS INITIAL.

        SELECT * FROM ltap INTO TABLE lt_ltap
                    FOR ALL ENTRIES IN lt_ltak
                      WHERE lgnum = lt_ltak-lgnum
                        AND tanum = lt_ltak-tanum.

        DELETE lt_ltap WHERE matnr <> lt_totais-matnr.
        DELETE lt_ltap WHERE pquit = 'X'.

        IF NOT lt_ltap[] IS INITIAL.
          LOOP AT lt_ltap.
*            AT NEW tanum.
*              IF l_mov_type = l_mov11.  "971
*                ls_zwm033-lgnum = lgnum.
*                ls_zwm033-tanum = lt_ltap-tanum.
*                ls_zwm033-tapos = lt_ltap-tapos.
*                ls_zwm033-matnr = lt_ltap-matnr.
*                ls_zwm033-refnr = lt_grupos-sammg.
*                ls_zwm033-vbeln = lt_ltap-vbeln.
*                ls_zwm033-posnr = lt_ltap-posnr.
*                INSERT zwm033 FROM ls_zwm033.
*              ENDIF.
*            ENDAT.
            IF lt_ltap-letyp = 'P2' OR lt_ltap-letyp = 'P5'.
              IF lt_ltap-tapos = '0001'.
                l_quant_aux = trunc( lt_ltap-vsola ).
                l_quant = l_quant + l_quant_aux.
              ENDIF.
            ELSE.
              l_quant_aux = trunc( lt_ltap-vsola ).
              l_quant = l_quant + l_quant_aux.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
    ELSE.
      CLEAR l_quant.
    ENDIF.

    IF l_quant < lt_totais-quant.

      l_quant = lt_totais-quant - l_quant.

      REFRESH: lt_return_msg, lt_sscc.
      CLEAR: lt_return_msg, lt_sscc.
      CLEAR l_tanum.

      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_INPUT'
        EXPORTING
          input          = l_meins
          language       = sy-langu
        IMPORTING
          output         = l_meins
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.


*     lt_sscc-sscc          = ' '.
*     lt_sscc-tipo_su       = ' '.
      lt_sscc-material      = lt_totais-matnr.
*     lt_sscc-variante      = ' '.
      lt_sscc-quantidade    = l_quant.
      lt_sscc-uni           = l_meins.
*     lt_sscc-altura        = ' '.
*     lt_sscc-lote_producao = ' '.
*      APPEND lt_sscc.


      DATA: adicional   TYPE lvs_lznum,
            qt_aux      LIKE lt_sscc-quantidade,
            qt_pal      LIKE mlgn-lhmg1,
            n_pal       TYPE i,
            qt_resto    TYPE i,
            bin_destino TYPE lgpla.

      CLEAR bin_destino.

      IF lgnum = '150' AND mlgn-plkpt = 'PKB'.
        CLEAR t_lqua.
        REFRESH t_lqua.
        SELECT * FROM lqua INTO TABLE t_lqua
        WHERE lgnum = mlgt-lgnum
          AND lgtyp = 'PKB'
          AND matnr = lt_totais-matnr
          AND werks = lt_totais-werks
          AND lgort = lt_totais-lgort
          AND lenum = ' '.

        SORT t_lqua BY verme.
        READ TABLE t_lqua INDEX 1.
        bin_destino = t_lqua-lgpla.
      ENDIF.

      CLEAR mlgn.
      SELECT SINGLE *
          FROM mlgn
              WHERE matnr = lt_totais-matnr AND
                    lgnum = lgnum.
      IF sy-subrc = 0.
        IF mlgn-lety1 = 'P2' OR mlgn-lety1 = 'P5'.
          lt_sscc-quantidade    = 1.
          lt_sscc-uni           = 'PAL'.
          APPEND lt_sscc.
          lt_sscc-quantidade    = 1.
          lt_sscc-uni           = 'PAL'.
          n_pal = 2.
        ELSE.
          n_pal = 1.
        ENDIF.
      ENDIF.
      APPEND lt_sscc.

      CLEAR: qt_aux, qt_pal.
      IF mlgn-plkpt = 'PKB' AND lgnum <> '150'.
        adicional = l_quant.
      ELSE.
        CLEAR adicional.
      ENDIF.

      qt_pal = mlgn-lhmg1 * n_pal.

      IF l_quant > qt_pal.
        CLEAR lt_sscc.
        REFRESH lt_sscc.

        qt_aux = l_quant DIV qt_pal.
        qt_resto = l_quant MOD qt_pal.

        IF NOT qt_resto IS INITIAL.
          IF mlgn-plkpt = 'PKB' AND lgnum <> '150'.
            adicional = qt_resto.
          ELSE.
            CLEAR adicional.
          ENDIF.

          IF mlgn-lety1 = 'P2' OR mlgn-lety1 = 'P5'.
            lt_sscc-material      = lt_totais-matnr.
            lt_sscc-quantidade    = 1.
            lt_sscc-uni           = 'PAL'.
            APPEND lt_sscc.
            lt_sscc-material      = lt_totais-matnr.
            lt_sscc-quantidade    = 1.
            lt_sscc-uni           = 'PAL'.
          ELSE.
            lt_sscc-material      = lt_totais-matnr.
            lt_sscc-quantidade    = 1.
            lt_sscc-uni           = 'PAL'.
          ENDIF.
          APPEND lt_sscc.

          IF adicional IS NOT INITIAL.
            adicional = ceil( adicional ).
          ENDIF.

          CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
            EXPORTING
              warehouse      = lgnum
              mov_type       = l_mov_type "972
              bin_destino    = bin_destino
              plant          = lt_totais-werks
              s_loc          = lt_totais-lgort
              req_number     = lt_grupos-sammg
              req_type       = l_betyp
              sscc_adicional = adicional
            IMPORTING
              to             = l_tanum
            TABLES
              return_msg     = lt_return_msg
              sscc           = lt_sscc
            EXCEPTIONS
              error          = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            IF sy-msgid = 'L3' AND sy-msgno = '008'. " Msg não diz o Material
              MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '278' WITH lt_totais-matnr.

            ELSE.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.

            ok_code_0001 = 'CANCE'.
            EXIT.
          ENDIF.

          CLEAR lt_sscc.
          REFRESH lt_sscc.
        ENDIF.

        DO qt_aux TIMES.

          IF mlgn-plkpt = 'PKB' AND lgnum <> '150'.
            adicional = mlgn-lhmg1.
          ELSE.
            CLEAR adicional.
          ENDIF.

          IF mlgn-lety1 = 'P2' OR mlgn-lety1 = 'P5'.
            lt_sscc-material      = lt_totais-matnr.
            lt_sscc-quantidade    = 1.
            lt_sscc-uni           = 'PAL'.
            APPEND lt_sscc.
            lt_sscc-material      = lt_totais-matnr.
            lt_sscc-quantidade    = 1.
            lt_sscc-uni           = 'PAL'.
          ELSE.
            lt_sscc-material      = lt_totais-matnr.
            lt_sscc-quantidade    = 1.
            lt_sscc-uni           = 'PAL'.
          ENDIF.
          APPEND lt_sscc.

          IF adicional IS NOT INITIAL.
            adicional = ceil( adicional ).
          ENDIF.

          CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
            EXPORTING
              warehouse      = lgnum
              mov_type       = l_mov_type "972
              bin_destino    = bin_destino
              plant          = lt_totais-werks
              s_loc          = lt_totais-lgort
              req_number     = lt_grupos-sammg
              req_type       = l_betyp
              sscc_adicional = adicional
            IMPORTING
              to             = l_tanum
            TABLES
              return_msg     = lt_return_msg
              sscc           = lt_sscc
            EXCEPTIONS
              error          = 1
              OTHERS         = 2.
          IF sy-subrc <> 0.
            IF sy-msgid = 'L3' AND sy-msgno = '008'. " Msg não diz o Material
              MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '278' WITH lt_totais-matnr.

            ELSE.
              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            ENDIF.
          ENDIF.

          CLEAR lt_sscc.
          REFRESH lt_sscc.

        ENDDO.
*      ENDIF.


      ELSE.
        IF adicional IS NOT INITIAL.
          adicional = ceil( adicional ).
        ENDIF.

        CALL FUNCTION 'ZWM_CREATE_MULTIPLE_TO'
          EXPORTING
            warehouse      = lgnum
            mov_type       = l_mov_type "972
            bin_destino    = bin_destino
            plant          = lt_totais-werks
            s_loc          = lt_totais-lgort
            req_number     = lt_grupos-sammg
            req_type       = l_betyp
            sscc_adicional = adicional
          IMPORTING
            to             = l_tanum
          TABLES
            return_msg     = lt_return_msg
            sscc           = lt_sscc
          EXCEPTIONS
            error          = 1
            OTHERS         = 2.
        IF sy-subrc <> 0.
          IF sy-msgid = 'L3' AND sy-msgno = '008'. " Msg não diz o Material
            MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '278' WITH lt_totais-matnr.

          ELSE.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.
        ENDIF.

      ENDIF.

    ENDIF.

    CLEAR ltak.
    SELECT * FROM ltak INTO TABLE lt_ltak
                        WHERE lgnum = lgnum
                          AND betyp = l_betyp
                          AND benum = lt_grupos-sammg
                          AND kquit = ' '.

    CHECK NOT lt_ltak[] IS INITIAL.

    SELECT * FROM ltap INTO TABLE lt_ltap
                FOR ALL ENTRIES IN lt_ltak
                  WHERE lgnum = lt_ltak-lgnum
                    AND tanum = lt_ltak-tanum.

    DELETE lt_ltap WHERE matnr <> lt_totais-matnr.
    DELETE lt_ltap WHERE werks <> lt_totais-werks.
    DELETE lt_ltap WHERE lgort <> lt_totais-lgort.
    DELETE lt_ltap WHERE pquit = 'X'.
    DELETE lt_ltap WHERE zeugn = 'X'.

    CHECK NOT lt_ltap[] IS INITIAL.

    REFRESH lt_totais_lote.
    SORT lt_ltak.
    LOOP AT lt_ltap.
      CLEAR ls_totais_lote.

      IF lt_ltap-letyp = 'P2' OR lt_ltap-letyp = 'P5'.
        IF lt_ltap-tapos = '0001'.
          READ TABLE lt_ltak WITH KEY lgnum = lt_ltap-lgnum
                                      tanum = lt_ltap-tanum
                                      BINARY SEARCH.
          IF sy-subrc = 0.
            IF NOT lt_ltak-lznum IS INITIAL.
              MOVE lt_ltak-lznum TO ls_totais_lote-vsola.
            ELSE.
              ls_totais_lote-vsola = lt_ltap-vsolm.
            ENDIF.
          ENDIF.

          ls_totais_lote-matnr = lt_ltap-matnr.
          ls_totais_lote-werks = lt_ltap-werks.
          ls_totais_lote-lgort = lt_ltap-lgort.
          ls_totais_lote-charg = lt_ltap-charg.
          COLLECT ls_totais_lote INTO lt_totais_lote.
        ENDIF.
      ELSE.
        READ TABLE lt_ltak WITH KEY lgnum = lt_ltap-lgnum
                                              tanum = lt_ltap-tanum
                                              BINARY SEARCH.
        IF sy-subrc = 0.
          IF NOT lt_ltak-lznum IS INITIAL.
            MOVE lt_ltak-lznum TO ls_totais_lote-vsola.
          ELSE.
            ls_totais_lote-vsola = lt_ltap-vsolm.
          ENDIF.
        ENDIF.

        ls_totais_lote-matnr = lt_ltap-matnr.
        ls_totais_lote-werks = lt_ltap-werks.
        ls_totais_lote-lgort = lt_ltap-lgort.
        ls_totais_lote-charg = lt_ltap-charg.
        COLLECT ls_totais_lote INTO lt_totais_lote.

      ENDIF.
    ENDLOOP.

    LOOP AT lt_totais_lote.
      REFRESH: lt_return_msg, lt_sscc.
      CLEAR: lt_return_msg, lt_sscc.
      CLEAR l_tanum.


      CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
        EXPORTING
          input          = l_meins
          language       = sy-langu
        IMPORTING
          output         = l_meins
        EXCEPTIONS
          unit_not_found = 1
          OTHERS         = 2.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.

*     lt_sscc-sscc          = ' '.
*     lt_sscc-tipo_su       = ' '.
      lt_sscc-material      = lt_totais_lote-matnr.
*     lt_sscc-variante      = ' '.
      lt_sscc-quantidade    = trunc( lt_totais_lote-vsola ) .
      lt_sscc-uni           = l_meins.
*     lt_sscc-altura        = ' '.
      lt_sscc-lote_producao = lt_totais_lote-charg.
      APPEND lt_sscc.

      CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
        EXPORTING
          warehouse  = lgnum
          mov_type   = l_mov_type2                          "970 ou 975
          plant      = lt_totais_lote-werks
          s_loc      = lt_totais_lote-lgort
          req_number = lt_grupos-sammg
          req_type   = l_betyp
        IMPORTING
          to         = l_tanum
        TABLES
          return_msg = lt_return_msg
          sscc       = lt_sscc
        EXCEPTIONS
          error      = 1
          OTHERS     = 2.

      DATA uni_incom_p TYPE zpalete_picking-uni_incom_p.
      uni_incom_p = lt_totais_lote-vsola MOD 1.

      IF uni_incom_p IS NOT INITIAL.

        CLEAR lt_sscc.
        REFRESH lt_sscc.
        lt_sscc-material      = lt_totais_lote-matnr.
        lt_sscc-quantidade    = 1.
        lt_sscc-uni           = l_meins.
        lt_sscc-lote_producao = lt_totais_lote-charg.
        APPEND lt_sscc.

        DO 20 TIMES.
          CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
            EXPORTING
              warehouse  = lgnum
              mov_type   = '983'
              plant      = lt_totais_lote-werks
              s_loc      = lt_totais_lote-lgort
              req_number = lt_grupos-sammg
              req_type   = l_betyp
            IMPORTING
              to         = l_tanum
            TABLES
              return_msg = lt_return_msg
              sscc       = lt_sscc
            EXCEPTIONS
              error      = 1
              OTHERS     = 2.

          IF l_tanum IS INITIAL.
            WAIT UP TO 1 SECONDS.
          ELSE.
            EXIT.
          ENDIF.
        ENDDO.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  REFRESH lt_totais.

*& Begin of Modification by Tiago Pateiro - ROFF @ 28.12.2015 17:12:04
*  SELECT werks UP TO 1 ROWS
*    FROM t320 INTO l_werks
*    WHERE lgort EQ 'CD'
*      AND lgnum EQ lgnum.
*  ENDSELECT.
*  IF sy-subrc NE 0.
*    l_werks = 'RENV'.
*  ENDIF.
*& End of Modification by Tiago Pateiro - ROFF @ 28.12.2015 17:12:05

*  LOOP AT lt_paletes WHERE uni_incom_p IS NOT INITIAL AND werks = 'RENV' AND lgort = 'CD'. " << DEL ROFF(SDF):TMGP:28.12.2015 17:17:44
  LOOP AT lt_paletes WHERE uni_incom_p IS NOT INITIAL. " << INS ROFF(SDF):TMGP:28.12.2015 17:17:46

    REFRESH: lt_return_msg, lt_sscc, lt_lqua.
    CLEAR: lt_return_msg, lt_sscc, lt_lqua, l_quant_pkl, l_quant_aux_pkl.
    CLEAR l_tanum.

    PERFORM get_plant_data USING lt_grupos-sammg CHANGING lt_paletes-werks lt_paletes-lgort.
    CHECK lt_paletes-lgort EQ lt_paletes-lgort.

    SELECT * FROM lqua INTO TABLE lt_lqua
            WHERE lgnum = lgnum
              AND lgtyp = 'PKL'
              AND matnr = lt_paletes-matnr
              AND werks = lt_paletes-werks
              AND lgort = lt_paletes-lgort
              AND lenum = ' '.

    IF lt_paletes-charg IS NOT INITIAL.
      DELETE lt_lqua WHERE charg <> lt_paletes-charg.
      DELETE lt_lqua WHERE werks <> lt_paletes-werks.
      DELETE lt_lqua WHERE lgort <> lt_paletes-lgort.
    ENDIF.

    IF NOT lt_lqua[] IS INITIAL.
      LOOP AT lt_lqua.
        CLEAR l_quant_aux_pkl.
        l_quant_aux_pkl = lt_lqua-verme.
        l_quant_pkl = l_quant_pkl + l_quant_aux_pkl.
      ENDLOOP.
    ENDIF.

    CHECK l_quant_pkl < lt_paletes-uni_incom_p.

    lt_sscc-material      = lt_paletes-matnr.
    lt_sscc-quantidade    = 1.
    lt_sscc-uni           = l_meins.
    APPEND lt_sscc.

    DO 20 TIMES.
      CLEAR:   lt_return_msg, flag.
      REFRESH: lt_return_msg.
      CALL FUNCTION 'ZWM_BI_CREATE_MULTIPLE_TO'
        EXPORTING
          warehouse   = lgnum
          mov_type    = l_movpkl                          "979
          plant       = lt_paletes-werks
          s_loc       = lt_paletes-lgort
          certificado = 'X'
          req_number  = lt_paletes-refnr
          req_type    = l_betyp
        IMPORTING
          to          = l_tanum
        TABLES
          return_msg  = lt_return_msg
          sscc        = lt_sscc
        EXCEPTIONS
          error       = 1
          OTHERS      = 2.

      DELETE lt_return_msg WHERE msgtyp <> 'E'.
      IF lt_return_msg[] IS NOT INITIAL.
        flag = 'X'.
        WAIT UP TO 1 SECONDS.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.
  ENDLOOP.

  IF flag IS NOT INITIAL.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '301' WITH lt_paletes-matnr.
  ENDIF.

  REFRESH lt_paletes.
  IF flag IS INITIAL.
    WAIT UP TO 30 SECONDS.

    READ TABLE lt_grupos INDEX 1.

    CLEAR lv_refnr.
    lv_refnr = lt_grupos-sammg.
    CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
      EXPORTING
        i_lgnum = lgnum
        i_refnr = lv_refnr
        i_step  = 1.

  ENDIF.

ENDFORM.                    "verify_picking_quantity
*----------------------------------------------------------------------*
*        FORM SAMMELGANG_FREIGEBEN                                     *
*----------------------------------------------------------------------*
*        Sammelgang für selektierte Referenznummer freigeben.          *
*----------------------------------------------------------------------*
FORM sammelgang_freigeben.
  DATA: lt_total  TYPE TABLE OF l2sktotal.

  DATA: lv_error TYPE flag,
        lv_2step TYPE flag.

  DATA: ls_t311 TYPE t311.

  DATA qtd_pal LIKE ltap-vsolm.

  IF NOT it311-tbedn IS INITIAL OR
     NOT it311-liefn IS INITIAL.

*SAPCONSOLE
    IF confirmado = '0'.
      CLEAR ok_code_0001.
** Verificar se tem ot´s partidas
      CLEAR: t_ltak, t_ltap.
      REFRESH: t_ltak, t_ltap.

      SELECT * INTO TABLE t_ltak
            FROM ltak
                WHERE lgnum = it311-lgnum AND
                      refnr = it311-refnr.

      CHECK NOT t_ltak[] IS INITIAL.

      SELECT * FROM ltap INTO TABLE t_ltap
          FOR ALL ENTRIES IN t_ltak
                WHERE lgnum = t_ltak-lgnum AND
                      tanum = t_ltak-tanum.

      DELETE t_ltap WHERE vorga = 'ST'.

      LOOP AT t_ltap.
        IF t_ltap-vltyp = 'DRI' OR
           t_ltap-vltyp = 'BLK' OR
           t_ltap-vltyp = 'TRI' OR
           t_ltap-vltyp = 'PRM' OR
           t_ltap-vltyp = 'AUT'.

          CLEAR qtd_pal.
          SELECT SINGLE *
              FROM marm
                  WHERE matnr = t_ltap-matnr AND
                        meinh = 'PAL'.
          IF sy-subrc = 0.

            qtd_pal = marm-umrez / marm-umren.
            qtd_pal = t_ltap-nsolm MOD qtd_pal.
            IF qtd_pal NE 0.
              MESSAGE ID 'ZWMMSG001' TYPE 'I'
                      NUMBER '243' WITH it311-refnr.
              ok_code_0001 = 'CANCE'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.

        IF t_ltap-vltyp = 'PCK' OR
           t_ltap-vltyp = 'PKB'.

          IF t_ltap-nsolm < 1.
**          Error, existem OT's com quantidades inferiores à UMB!
            MESSAGE ID 'ZWMMSG001' TYPE 'I'
                    NUMBER '349' WITH it311-refnr.
            ok_code_0001 = 'CANCE'.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF ok_code_0001 = 'CANCE'.
*        it311[] = iit311[].
        LOOP AT it311 WHERE kreuz = con_x.
          it311-kreuz = space.
          MODIFY it311.
        ENDLOOP.
        MOVE sav_fname TO hlp_fname.
        MOVE sav_rbtyp TO sum00-rbtyp.

        SET PF-STATUS pfstat_liste2.
        SET TITLEBAR titel_liste2.
        sy-lsind = sy-lsind - 1.
        PERFORM detailliste1.
        EXIT.
      ENDIF.

*************************************

      DATA: i_vbss LIKE vbss OCCURS 0 WITH HEADER LINE.
      SELECT SINGLE *
          FROM t311
              WHERE lgnum = it311-lgnum AND
                    refnr = it311-refnr.
      IF NOT t311-kzdru IS INITIAL.
        MESSAGE ID 'ZWMMSG001' TYPE 'I'
               NUMBER '218' WITH it311-refnr.
        ok_code_0001 = 'CANCE'.
      ELSE.
        SELECT SINGLE *
          FROM zwm028
              WHERE lgnum = it311-lgnum AND
                    refnr = it311-refnr.
        IF sy-subrc <> 0.
          MESSAGE ID 'ZWMMSG001' TYPE 'I'
              NUMBER '099' WITH it311-refnr.
          ok_code_0001 = 'CANCE'.
*        EXIT.
        ELSE.
          IF zwm028-transporte IS INITIAL.
            MESSAGE ID 'ZWMMSG001' TYPE 'I'
                NUMBER '212' WITH it311-refnr.
            ok_code_0001 = 'CANCE'.
*          EXIT.
          ENDIF.
        ENDIF.
      ENDIF.

      IF ok_code_0001 = 'CANCE'.
*        it311[] = iit311[].
        LOOP AT it311 WHERE kreuz = con_x.
          it311-kreuz = space.
          MODIFY it311.
        ENDLOOP.
        MOVE sav_fname TO hlp_fname.
        MOVE sav_rbtyp TO sum00-rbtyp.

        SET PF-STATUS pfstat_liste2.
        SET TITLEBAR titel_liste2.
        sy-lsind = sy-lsind - 1.
        PERFORM detailliste1.
        EXIT.
      ENDIF.


*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 22.05.2012 10:11:22
*  Motivo: Picking em 2 Passos
*--------------------------------------------------------------------*
      ls_t311 = it311.

      CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
        EXPORTING
          is_t311 = ls_t311
        IMPORTING
          e_2step = lv_2step
        EXCEPTIONS
          error   = 1
          OTHERS  = 2.

      IF lv_2step EQ 'X'.
        CALL FUNCTION 'L_2_STEP_QUANTITY_REMOVAL'
          EXPORTING
            i_lgnum                       = it311-lgnum
            i_refnr                       = it311-refnr
          TABLES
            t_total                       = lt_total
          EXCEPTIONS
            refnr_no_found                = 1
            refnr_documents_no_found      = 2
            no_relevant_for_2step_picking = 3
            item_for_removal_not_found    = 4
            OTHERS                        = 5.

        IF sy-subrc <> 0.
          lv_error = 'X'.
        ELSE.
          DELETE lt_total WHERE ofmng = 0.

          IF lt_total IS INITIAL.
            CLEAR lv_error.
          ELSE.
            lv_error = 'X'.
          ENDIF.
        ENDIF.

        IF lv_error EQ 'X'.
** 011  Atenção, ainda existem OT's por criar para o Grupo &

          MESSAGE i011(zwm001) WITH it311-refnr.
          EXIT.
        ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
      ELSE.
**********************************************************************
** RS- 24-01-2011 @ Valida se todas as remessas têm o picking efectuado
**********************************************************************
        DATA: lv_tabix  LIKE sy-tabix.
        DATA: aux_benum LIKE ltak-benum,
              pal_p1    LIKE zwm020-p1,
              pal_p2    LIKE zwm020-p2.

        DATA: lv_matnr  TYPE matnr.

        DATA: lt_t311a  TYPE t311a  OCCURS 0 WITH HEADER LINE.
        DATA: lt_vbup   TYPE vbup   OCCURS 0 WITH HEADER LINE.
        DATA: lt_zwm047 TYPE zwm047 OCCURS 0 WITH HEADER LINE.
        DATA: lv_text1  TYPE char255.
        DATA: lv_text2  TYPE char70.
        DATA: lv_rsp    TYPE char1.

** Obter remessas
        SELECT *
          FROM t311a INTO TABLE lt_t311a
           WHERE lgnum = it311-lgnum AND
                 refnr = it311-refnr.

** Obter status das remessas
        IF lt_t311a[] IS NOT INITIAL.

          SELECT *
            FROM vbup INTO TABLE lt_vbup
            FOR ALL ENTRIES IN lt_t311a
            WHERE vbeln = lt_t311a-rbnum.

** Validar se todas items das remessas do grupo têm Status de picking completo
          DELETE lt_vbup WHERE kosta = 'C'.

**      Obter as OTs do grupo
          READ TABLE lt_vbup INDEX 1.
          IF sy-subrc = 0.

*          SELECT *
*            FROM zwm047 INTO TABLE lt_zwm047
*            FOR ALL ENTRIES IN lt_vbup
*            WHERE vbeln = lt_vbup-vbeln AND
*                  posnr = lt_vbup-posnr.
*        ENDIF.
*
*        LOOP AT lt_vbup.
*          LOOP AT lt_zwm047 WHERE vbeln = lt_vbup-vbeln AND
*                                  posnr = lt_vbup-posnr.
*
*            CLEAR to.
*            DO 10 TIMES.
*              CLEAR return_msg. REFRESH return_msg.
*
*              CALL FUNCTION 'ZWM_CREATE_TO_DELIVERY'
*                EXPORTING
*                  warehouse     = it311-lgnum
*                  refnr         = it311-refnr
*                  vbeln         = lt_zwm047-vbeln
*                  posnr         = lt_zwm047-posnr
*                  vsola         = lt_zwm047-vsola
**        su            = zwm020-p1
*                IMPORTING
*                  to            = to
*                TABLES
*                  return_msg    = return_msg
*                EXCEPTIONS
*                  error_message = 1
*                  OTHERS        = 2.
*
*              IF NOT to IS INITIAL.
*                EXIT.
*              ELSE.
*                WAIT UP TO 1 SECONDS.
*              ENDIF.
*            ENDDO.
*
***          Se for a palete P1 criar a outra ot para o PRM com a P2
*            CLEAR ltap.
*            SELECT SINGLE *
*                FROM ltap
*                    WHERE lgnum = it311-lgnum
*                      AND tanum = to.
*            IF sy-subrc = 0.
*
*              DELETE FROM zwm047 WHERE vbeln = lt_zwm047-vbeln AND
*                                       posnr = lt_zwm047-posnr AND
*                                       tanum = lt_zwm047-tanum AND
*                                       tapos = lt_zwm047-tapos.
*              IF sy-subrc = 0.
*                COMMIT WORK.
*              ENDIF.
*
*              CLEAR zwm020.
*              SELECT SINGLE p1 p2 INTO (pal_p1, pal_p2)
*                  FROM zwm020
*                      WHERE armazem = it311-lgnum
*                        AND ( p1 = ltap-vlenr OR
*                              p2 = ltap-vlenr ).
*
*              IF sy-subrc = 0.
*                WAIT UP TO 1 SECONDS.
*
***              CC estornar a ot com a P2 e criar para a remessa com a P1
*                IF ltap-vlenr = pal_p2.
*
*                  CLEAR return_msg. REFRESH: return_msg.
*                  CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
*                    EXPORTING
*                      warehouse     = ltap-lgnum
*                      tanum         = ltap-tanum
*                      tapos         = ltap-tapos
*                    TABLES
*                      return_msg    = return_msg
*                    EXCEPTIONS
*                      error_message = 1
*                      OTHERS        = 2.
*                  IF sy-subrc <> 0.
*                    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*                  ENDIF.
*
*                  WAIT UP TO 1 SECONDS.
*
*                  CLEAR: flag, to.
*                  WHILE flag IS INITIAL.
*                    CALL FUNCTION 'ZWM_CREATE_TO_DELIVERY'
*                      EXPORTING
*                        warehouse     = it311-lgnum
*                        refnr         = it311-refnr
*                        vbeln         = lt_zwm047-vbeln
*                        posnr         = lt_zwm047-posnr
*                        vsola         = lt_zwm047-vsola
*                        su            = pal_p1
*                      IMPORTING
*                        to            = to
*                      TABLES
*                        return_msg    = return_msg
*                      EXCEPTIONS
*                        error_message = 1
*                        OTHERS        = 2.
*                    IF NOT to IS INITIAL.
*                      flag = 'X'.
*                    ELSE.
*                      CLEAR flag.
*                    ENDIF.
*                  ENDWHILE.
*
*                ENDIF.
*
*                WAIT UP TO 1 SECONDS.
*
*                CLEAR aux_benum.
*                aux_benum = it311-refnr.
*                CLEAR flag.
*                WHILE flag IS INITIAL.
*
*                  CALL FUNCTION 'ZWM_CREATE_TO'
*                    EXPORTING
*                      warehouse  = ltap-lgnum
*                      mov_type   = '940'
*                      material   = ltap-matnr
*                      quantity   = '1'
*                      unit       = 'PAL'
*                      plant      = ltap-werks
*                      s_loc      = ltap-lgort
*                      lote       = ltap-charg
*                      source_sty = ltap-vltyp
*                      source_bin = ltap-vlpla
*                      req_type   = 'I'
*                      req_number = aux_benum
*                      su         = pal_p2
*                    IMPORTING
*                      to         = to_remontada
*                    TABLES
*                      return_msg = return_msg
*                    EXCEPTIONS
*                      error      = 1
*                      OTHERS     = 2.
*
*                  IF sy-subrc = 0.
*                    flag = 'X'.
*                  ELSE.
*                    CLEAR flag.
*                  ENDIF.
*                ENDWHILE.
*              ENDIF.
*
*            ELSE.
**         O Grupo & não pode ser liberado. Rem &/item & sem st de picking completo
*              MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '277'
*              WITH it311-refnr lt_vbup-vbeln lt_vbup-posnr.
*              ok_code_0001 = 'CANCE'.
*              EXIT.
*            ENDIF.
*
*          ENDLOOP.
*          IF sy-subrc <> 0.
**         O Grupo & não pode ser liberado. Rem &/item & sem st de picking completo
*            MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '277'
*            WITH it311-refnr lt_vbup-vbeln lt_vbup-posnr.
*            ok_code_0001 = 'CANCE'.
*            EXIT.
*          ENDIF.
*        ENDLOOP.

**        O Grupo & não pode ser liberado. Rem &/item &, falta criar OT para material &!
            CLEAR lv_matnr.
            SELECT SINGLE matnr
              FROM lips INTO lv_matnr
              WHERE vbeln = lt_vbup-vbeln  AND
                    posnr = lt_vbup-posnr.

            MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '277' INTO lv_text1
            WITH lv_matnr lt_vbup-vbeln lt_vbup-posnr it311-refnr.

            lv_text2 = lv_text1+70(70).
            lv_text1 = lv_text1(70).

            CLEAR lv_rsp.
            CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
              EXPORTING
                defaultoption  = 'N'
                textline1      = lv_text1
                textline2      = lv_text2
                titel          = 'Erro na liberação'
                cancel_display = ' '
              IMPORTING
                answer         = lv_rsp.

            IF lv_rsp NE 'J'.
              ok_code_0001 = 'CANCE'.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.

        IF ok_code_0001 = 'CANCE'.
*        it311[] = iit311[].
          LOOP AT it311 WHERE kreuz = con_x.
            it311-kreuz = space.
            MODIFY it311.
          ENDLOOP.
          MOVE sav_fname TO hlp_fname.
          MOVE sav_rbtyp TO sum00-rbtyp.

          SET PF-STATUS pfstat_liste2.
          SET TITLEBAR titel_liste2.
          sy-lsind = sy-lsind - 1.
          PERFORM detailliste1.
          EXIT.
        ENDIF.
**********************************************************************
** RS- 24-01-2011 @ END
**********************************************************************
      ENDIF.

      DATA lt_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE.

      CLEAR lt_zwm028.
      REFRESH lt_zwm028.

      SELECT SINGLE * FROM zwm026
      WHERE armazem   EQ it311-lgnum
        AND grupo     EQ it311-refnr
        AND to_number NE space.
** RL <- MOD 26.05.2005
      IF sy-subrc = 0.
        SELECT * INTO TABLE lt_zwm028
            FROM zwm028
                WHERE lgnum = it311-lgnum AND
                      refnr = it311-refnr.

        LOOP AT lt_zwm028.
          CLEAR: lt_zwm028-st_pul,
                 lt_zwm028-st_dck,
                 lt_zwm028-st_ppk.
          MODIFY lt_zwm028 INDEX sy-tabix.
        ENDLOOP.

        MODIFY zwm028 FROM TABLE lt_zwm028.
        COMMIT WORK AND WAIT.
        DELETE FROM zwm028  WHERE lgnum = it311-lgnum AND
                                   refnr = it311-refnr AND
                                   remessa = ' '.
        COMMIT WORK AND WAIT.

        PERFORM actualiza_zwm028.

*      se tem paletes de picking
*        CALL SCREEN 0001 STARTING AT 10 1 ENDING AT 61  7.

      ELSE.
        SELECT * INTO TABLE lt_zwm028
             FROM zwm028
                 WHERE lgnum = it311-lgnum AND
                       refnr = it311-refnr.

        LOOP AT lt_zwm028.
          CLEAR: lt_zwm028-st_pul,
                 lt_zwm028-st_dck,
                 lt_zwm028-st_ppk.
          MODIFY lt_zwm028 INDEX sy-tabix.
        ENDLOOP.

        MODIFY zwm028 FROM TABLE lt_zwm028.
        COMMIT WORK AND WAIT.
        DELETE FROM zwm028  WHERE lgnum = it311-lgnum AND
                                   refnr = it311-refnr AND
                                   remessa = ' '.
        COMMIT WORK AND WAIT.

        PERFORM actualiza_zwm028.

*      senao tem paletes de picking
*        CALL SCREEN 0003 STARTING AT 10 1 ENDING AT 61 7.
      ENDIF.
    ENDIF.

    IF ok_code_0001 = 'CANCE'.
*      it311[] = iit311[].
      LOOP AT it311 WHERE kreuz = con_x.
        it311-kreuz = space.
        MODIFY it311.
      ENDLOOP.
      MOVE sav_fname TO hlp_fname.
      MOVE sav_rbtyp TO sum00-rbtyp.

      SET PF-STATUS pfstat_liste2.
      SET TITLEBAR titel_liste2.
      sy-lsind = sy-lsind - 1.
      PERFORM detailliste1.

      EXIT.
    ENDIF.

    CLEAR lcoms.
    MOVE:
          it311-lgnum    TO lcoms-lgnum,
          it311-refnr    TO lcoms-refnr,
          it311-rbtyp    TO lcoms-rbtyp,
          con_dunkel     TO lcoms-dunkl,
          fcode_frei     TO lcoms-fcode.
    CLEAR lcoms-subrc.

    IF lv_2step <> 'X'.
      CALL FUNCTION 'Z_WMFR_IDOC_FREE_WORK'
        EXPORTING
          i_lgnum = lcoms-lgnum
          i_refnr = lcoms-refnr
          i_step  = 1.



      EXPORT  lcoms TO MEMORY ID 'LT42A'.
      CALL TRANSACTION 'LT44' AND
                SKIP FIRST SCREEN.
    ENDIF.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 22.05.2012 10:27:48
*  Motivo: Update de Tabela
*--------------------------------------------------------------------*
    IF lv_2step EQ 'X'.
      UPDATE t311 SET kzdru = 'X'
                  WHERE lgnum = it311-lgnum AND
                        refnr = it311-refnr.
      COMMIT WORK AND WAIT.
    ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

**  Confirma as rotas automaticamente
    PERFORM confirma_rotas USING lcoms-lgnum
                                 lcoms-refnr.

    LOOP AT it311 WHERE refnr = lcoms-refnr.
      it311-kreuz = space.

*      IF lv_2step EQ 'X'.
      it311-kzdru = 'X'.
*      ENDIF.

      MODIFY it311.
    ENDLOOP.

    MOVE sav_fname TO hlp_fname.
    MOVE sav_rbtyp TO sum00-rbtyp.

    SET PF-STATUS pfstat_liste2.
    SET TITLEBAR titel_liste2.
    sy-lsind = sy-lsind - 1.
    PERFORM detailliste1.

  ENDIF.

ENDFORM.                    "sammelgang_freigeben

*----------------------------------------------------------------------*
*        FORM REFNR_AKTUALISIEREN                                      *
*----------------------------------------------------------------------*
*        Bei Aktualisiern der Summenanzeige wird die Selektions-       *
*        tabelle REFNR auch entsprechend aktualisiert                  *
*----------------------------------------------------------------------*
FORM refnr_aktualisieren.

*........Keine Selektion fuer Ref.nummer im Anforderungsdynpro .........

  CLEAR refnr.
  LOOP AT refnr.
    EXIT.
  ENDLOOP.
  IF sy-subrc NE 0.
    MOVE con_sign_i    TO refnr-sign.
    MOVE con_option_ge TO refnr-option.
    MOVE low_refnr     TO refnr-low.
  ELSE.

*........Nur eine Selektion für Referenznummer im Anforderungsdynpro....

    IF sy-tfill EQ 1.
      IF refnr-sign EQ con_sign_i.
        CASE refnr-option.
*........Bei Selektion GE die VON-Referenznummer modifizieren...........
          WHEN con_option_ge.
            MOVE low_refnr TO refnr-low.
            MODIFY refnr INDEX 1.
*........Bei Selektion BT die VON-Referenznummer modifizieren...........
          WHEN con_option_bt.
            MOVE low_refnr TO refnr-low.
            MODIFY refnr INDEX 1.
*........Bei Selektion LT die VON-Referenznummer modifizieren...........
          WHEN con_option_le.
            MOVE low_refnr TO refnr-low.
            MODIFY refnr INDEX 1.
        ENDCASE.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "refnr_aktualisieren

*----------------------------------------------------------------------*
*        FORM DETAILLISTE1                                             *
*----------------------------------------------------------------------*
*        Bildschirmausgabe der Referenznummern des gemäß Cursor-       *
*        position ausgewählten Belegtypes.                             *
*----------------------------------------------------------------------*
FORM detailliste1.

  such_k_ers = ' '.
  such_k_off = ' '.
  such_k_erl = ' '.
  such_k_fre = ' '.
  such_k_tao = ' '.
*-- Suchfelder zuweisen -----------------------------------------------*
  CASE hlp_fname.
    WHEN erstellte.
      such_k_ers = 'X'.
      liste1_titel2 = text-a80.
    WHEN offene.
      such_k_off = 'X'.
      liste1_titel2 = text-a81.
    WHEN erledigte.
      such_k_erl = 'X'.
      liste1_titel2 = text-a82.
    WHEN freigegebene.
      such_k_fre = 'X'.
      liste1_titel2 = text-a83.
    WHEN alle.                         "->Zeilensumme, dh. kein Status
      liste1_titel2 = text-a84.
    WHEN offene_ta.
      such_k_tao = 'X'.
      liste1_titel2 = text-a85.
  ENDCASE.

  CASE sum00-rbtyp.
    WHEN con_refnrbelegtyp_b.
      ASSIGN it311-tbedn TO <feldname>.
      liste1_titel1 = text-a30.
    WHEN con_refnrbelegtyp_l.
      ASSIGN it311-liefn TO <feldname>.
      liste1_titel1 = text-a31.
    WHEN high_value(1).
      liste1_titel1 = text-a32.
  ENDCASE.

  such_rbtyp = sum00-rbtyp.

*-- IT311 gemäß der Suchfelder durchsuchen und ausgeben----------------*
  LOOP AT it311.

    CLEAR: it311-linno, it311-pagno.

    IF hlp_fname = alle.                      "Zeilensumme?
      IF such_rbtyp(1) = high_value(1).    "Gesamtsumme?

        IF it311-rbtyp = con_refnrbelegtyp_b OR "Selektion aller Sätze
           it311-rbtyp = con_refnrbelegtyp_l.         "Doppelsumme
          PERFORM detailliste1_ausgabe.
        ENDIF.

      ELSE.                                   "Keine Gesamtsumme!

        IF <feldname> = con_x.             "Selektion der Sätze in Zeile
          PERFORM detailliste1_ausgabe.
        ENDIF.

      ENDIF.
    ELSE.
      IF such_rbtyp(1) = high_value(1). "Gesamtsumme?

        IF hlp_fname = offene_ta.             "offene TA ?           "
          IF it311-k_tao = such_k_tao.                               "
            IF it311-rbtyp = con_refnrbelegtyp_b OR                  "
               it311-rbtyp = con_refnrbelegtyp_l.                    "
              PERFORM detailliste1_ausgabe.                          "
            ENDIF.                                                   "
          ENDIF.                                                     "
        ELSE.                                                        "
          IF it311-k_ers = such_k_ers AND       "Selektion der Sätze
             it311-k_off = such_k_off AND       "Gesamtsumme statusabh.
             it311-k_erl = such_k_erl AND
             it311-k_fre = such_k_fre.
            IF it311-rbtyp = con_refnrbelegtyp_b OR
               it311-rbtyp = con_refnrbelegtyp_l.
              PERFORM detailliste1_ausgabe.
            ENDIF.
          ENDIF.
        ENDIF.

      ELSE.

        IF hlp_fname = offene_ta.             "offene TA ?           "
          IF it311-k_tao = such_k_tao.                               "
            IF <feldname> = con_x.            "Selektion der Sätze   "
              PERFORM detailliste1_ausgabe.                          "
            ENDIF.                                                   "
          ENDIF.                                                     "
        ELSE.                                                        "
          IF it311-k_ers = such_k_ers AND     "Selektion der Sätze
             it311-k_off = such_k_off AND     "Einzelsumme
             it311-k_erl = such_k_erl AND
             it311-k_fre = such_k_fre.
            IF <feldname> = con_x.            "Selektion der Sätze
              PERFORM detailliste1_ausgabe.
            ENDIF.
          ENDIF.
        ENDIF.

      ENDIF.
    ENDIF.

    MODIFY it311.
    CLEAR it311.
  ENDLOOP.

  IF offta EQ space.
    WRITE: /1(83) sy-uline.
  ELSE.
    WRITE: /1(87) sy-uline.
  ENDIF.

ENDFORM.                    "detailliste1
*----------------------------------------------------------------------*
*        FORM SAMMELGANG_STARTEN                                       *
*----------------------------------------------------------------------*
*        Sammelgang für selektierte Referenznummer starten.            *
*----------------------------------------------------------------------*
FORM sammelgang_starten CHANGING ct_paletes TYPE zpalete_picking_tt.
  DATA: lt_messages	  TYPE tab_bdcmsgcoll,
        lt_messages_b TYPE tab_bdcmsgcoll,
        lt_total      TYPE TABLE OF l2sktotal,
        lt_ltak       TYPE TABLE OF ltak,
        lt_ltap       TYPE TABLE OF ltap,
        lt_ltak_sum   TYPE SORTED TABLE OF ltak WITH NON-UNIQUE KEY benum,
        lt_ltap_sum   TYPE SORTED TABLE OF ltap WITH NON-UNIQUE KEY tanum,
        lt_queue      TYPE TABLE OF lrf_queue,
        lt_lgpla      TYPE TABLE OF lgpla,
        lt_mlgn       TYPE SORTED TABLE OF mlgn WITH UNIQUE KEY matnr.

  DATA: ls_message       TYPE bdcmsgcoll,
        ls_total         TYPE l2sktotal,
        ls_t311          TYPE t311,
        lt_t311a_2s      TYPE TABLE OF t311a,
        ls_t311a_2s      TYPE t311a,
        lt_lips          TYPE TABLE OF lips,
        ls_lips          TYPE lips,
        lt_likp          TYPE TABLE OF likp,
        ls_likp          TYPE likp,
        ls_ltak          TYPE ltak,
        ls_ltap          TYPE ltap,
        ls_ltap_last     TYPE ltap,
        lt_vbuk          LIKE vbuk OCCURS 0 WITH HEADER LINE,
        lt_t311a         LIKE t311a OCCURS 0 WITH HEADER LINE,
        lt_ltap_create   TYPE TABLE OF ltap_creat,
        lt_ltap_create_c TYPE TABLE OF ltap_creat,
        ls_ltap_create   TYPE ltap_creat,
        ls_ltap_create_c TYPE ltap_creat,
        ls_mlgn          TYPE mlgn,
        lt_ltap_vb       TYPE TABLE OF ltap_vb,
        ls_ltap_vb       TYPE ltap_vb.

  DATA: lv_valor1         LIKE zwm001-valor,
        lv_kzakt_kzerl    TYPE flag,
        lv_2step          TYPE flag,
        lv_success        TYPE flag,
        lv_material_error TYPE flag,
        lv_tabix          TYPE sytabix,
        lv_lgpla          TYPE lgpla,
        lv_activo         TYPE flag,
        lv_2spart         TYPE flag,
        lv_2step_vb       TYPE flag,
        lv_normal_to      TYPE flag,
        lv_normal_to_ok   TYPE flag,
        lv_um_pal         TYPE meins,
        lv_quantity       TYPE menge_d,
        lv_quantity_pal   TYPE menge_d.

  DATA: lr_lgpla TYPE RANGE OF lgpla.

  DATA:lr_s_lgpla LIKE LINE OF lr_lgpla.

  FIELD-SYMBOLS: <lv_matnr>       TYPE matnr,
                 <ls_ltap_create> TYPE ltap_creat.

  it311 = sav_it311.
  IF NOT it311-tbedn IS INITIAL OR
     NOT it311-liefn IS INITIAL.

    CLEAR lcoms.
    MOVE:
          it311-lgnum    TO lcoms-lgnum ,
          it311-refnr    TO lcoms-refnr,
          it311-rbtyp    TO lcoms-rbtyp,
          con_dunkel     TO lcoms-dunkl,
          fcode_strt     TO lcoms-fcode.
    CLEAR lcoms-subrc.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 21.05.2012 14:20:00
*  Motivo: Picking 2 Passos
*--------------------------------------------------------------------*
    ls_t311 = it311.

    lv_normal_to = abap_true. "faz TO normal de origem

    CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
      EXPORTING
        is_t311  = ls_t311
      IMPORTING
        e_2step  = lv_2step
        e_2spart = lv_2spart
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF lv_2spart EQ abap_true.

      DO 1 TIMES.
        SELECT * FROM t311a
                 INTO TABLE lt_t311a_2s
                 WHERE lgnum = ls_t311-lgnum AND
                       refnr = ls_t311-refnr.
        CHECK sy-subrc EQ 0.

        SELECT * FROM likp
                 INTO TABLE lt_likp
                 FOR ALL ENTRIES IN lt_t311a_2s
                 WHERE vbeln = lt_t311a_2s-rbnum.

        SELECT * FROM lips
                 INTO TABLE lt_lips
                 FOR ALL ENTRIES IN lt_likp
                 WHERE vbeln = lt_likp-vbeln.

        CHECK sy-subrc EQ 0.
*        DELETE lt_lips WHERE lgort <> 'CD'.

        CALL FUNCTION 'Z_WM_FILTER_TABLE_TO_WM'
          EXPORTING
            i_lgnum  = ls_t311-lgnum
          CHANGING
            ct_table = lt_lips.

        SELECT * FROM ltak
                 INTO TABLE lt_ltak_sum
                 FOR ALL ENTRIES IN lt_likp
                 WHERE lgnum = ls_t311-lgnum AND
                       benum = lt_likp-vbeln AND
                       betyp = 'L' AND
                       refnr = ls_t311-refnr AND
                       kquit = abap_false.

        IF NOT lt_ltak_sum IS INITIAL.
          SELECT * FROM ltap
                   INTO TABLE lt_ltap_sum
                   FOR ALL ENTRIES IN lt_ltak_sum
                   WHERE lgnum = lt_ltak_sum-lgnum AND
                         tanum = lt_ltak_sum-tanum.
        ENDIF.


        SELECT * FROM mlgn
                 INTO TABLE lt_mlgn
                 FOR ALL ENTRIES IN lt_lips
                 WHERE matnr = lt_lips-matnr AND
                       lgnum = ls_t311-lgnum.


        LOOP AT lt_likp INTO ls_likp.
          CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
            EXPORTING
              i_vbeln = ls_likp-vbeln
            IMPORTING
              e_2step = lv_2step_vb
            EXCEPTIONS
              error   = 1
              OTHERS  = 2.

          IF lv_2step_vb <> abap_true.
            lv_normal_to_ok = abap_true.
            CONTINUE.
          ENDIF.

          CLEAR: lt_ltap_create.
          LOOP AT lt_lips INTO ls_lips WHERE vbeln = ls_likp-vbeln.
            CLEAR: ls_ltap_create.
            ls_ltap_create-matnr = ls_lips-matnr.
            ls_ltap_create-werks = ls_lips-werks.
            ls_ltap_create-lgort = ls_lips-lgort.
            ls_ltap_create-anfme = ls_lips-lfimg.
            ls_ltap_create-altme = ls_lips-vrkme.
            COLLECT ls_ltap_create INTO lt_ltap_create.
          ENDLOOP.

          LOOP AT lt_ltak_sum INTO ls_ltak WHERE benum = ls_likp-vbeln.
            LOOP AT lt_ltap_sum INTO ls_ltap WHERE tanum = ls_ltak-tanum AND
                                                          vsola > 0.
              LOOP AT lt_ltap_create ASSIGNING <ls_ltap_create> WHERE werks = ls_ltap-werks AND
                                                                      lgort = ls_ltap-lgort AND
                                                                      matnr = ls_ltap-matnr AND
                                                                      anfme > 0.
                IF ls_ltap-vsola > <ls_ltap_create>-anfme.
                  <ls_ltap_create>-anfme = 0.
                ELSE.
                  <ls_ltap_create>-anfme = <ls_ltap_create>-anfme - ls_ltap-vsola.
                ENDIF.
              ENDLOOP.
            ENDLOOP.
          ENDLOOP.

          DELETE lt_ltap_create  WHERE anfme <= 0.

          LOOP AT lt_ltap_create INTO ls_ltap_create.
            CLEAR: ls_mlgn.
            READ TABLE lt_mlgn
                  INTO ls_mlgn
                  WITH TABLE KEY matnr = ls_ltap_create-matnr.
            CHECK sy-subrc EQ 0.

            WHILE ls_ltap_create-anfme > 0.
              CLEAR: lt_ltap_create_c, ls_ltap_create_c, lv_quantity, lt_ltap_vb.

              lv_quantity = ls_ltap_create-anfme / ls_mlgn-lhmg1.


*              IF ls_mlgn-lety1 EQ 'P2' OR
*                 ls_mlgn-lety1 EQ 'P5'.
*
*                lv_quantity_pal = 2.
*              ELSE.
              lv_quantity_pal = 1.
*              ENDIF.


              IF lv_quantity < lv_quantity_pal.
                lv_quantity = ls_ltap_create-anfme.
              ELSE.
                lv_quantity = ls_mlgn-lhmg1 * lv_quantity_pal.
              ENDIF.

              ls_ltap_create-anfme = ls_ltap_create-anfme - lv_quantity.

              ls_ltap_create_c = ls_ltap_create.
              ls_ltap_create_c-anfme = lv_quantity.

              IF ls_ltap_create-anfme > 0.
                IF ls_mlgn-lety1 EQ 'P2' OR
                   ls_mlgn-lety1 EQ 'P5'.
                  ls_ltap_create_c-posnr = '99999'.
                ENDIF.
              ENDIF.

              APPEND ls_ltap_create_c TO lt_ltap_create_c.

              IF ls_ltap_create-anfme > 0.
                IF ls_mlgn-lety1 EQ 'P2' OR
                   ls_mlgn-lety1 EQ 'P5'.
                  APPEND ls_ltap_create_c TO lt_ltap_create_c.
                  ls_ltap_create-anfme = ls_ltap_create-anfme - lv_quantity.
                ENDIF.
              ENDIF.

              CALL FUNCTION 'L_TO_CREATE_MULTIPLE'
                EXPORTING
                  i_lgnum       = ls_t311-lgnum
                  i_bwlvs       = '850'
                  i_betyp       = 'L'
                  i_benum       = ls_likp-vbeln
                  i_commit_work = abap_true
                  i_refnr       = ls_t311-refnr
                  i_l2ska       = '1'
                  i_kompl       = abap_false
                TABLES
                  t_ltap_creat  = lt_ltap_create_c
                  t_ltap_vb     = lt_ltap_vb
                EXCEPTIONS
                  error_message = 1
                  OTHERS        = 2.

              IF sy-subrc <> 0.
*               Não foi possivel criar OT para o material &!
                IF sy-msgid = 'L3' AND
                   ( sy-msgno = '008' OR sy-msgno = '332' ).
                  ASSIGN ('(SAPLL03B)LTAP-MATNR') TO <lv_matnr>.
                  IF <lv_matnr> IS ASSIGNED.
                    MESSAGE ID 'ZWMMSG001' TYPE 'E' NUMBER '278' WITH <lv_matnr>.
                  ENDIF.
                ENDIF.

                MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
                        WITH sy-msgv1
                             sy-msgv2
                             sy-msgv3
                             sy-msgv4.
                CONTINUE.
              ENDIF.

              CLEAR: ls_ltap_vb.
              READ TABLE lt_ltap_vb
                    INTO ls_ltap_vb
                    INDEX 1.

              IF NOT ls_ltap_vb-vsolm IS INITIAL AND
                 ls_ltap_vb-vsolm <> ls_ltap_create_c-anfme AND
                 ls_ltap_create_c-anfme > ls_ltap_vb-vsolm.
                ls_ltap_create-anfme = ls_ltap_create-anfme + ( ls_ltap_create_c-anfme - ls_ltap_vb-vsolm ).
              ENDIF.

            ENDWHILE.
          ENDLOOP.
        ENDLOOP.
        CHECK sy-subrc EQ 0.

        IF lv_normal_to_ok <> abap_true.
          CLEAR: lv_normal_to.
        ENDIF.
      ENDDO.


    ELSEIF lv_2step EQ abap_true AND
           lv_2spart EQ abap_false.

      CLEAR: lv_normal_to. "em 2 step total não precisa de fazer OT's normais
      CALL FUNCTION 'ZWM_TO_CREATE_2_STEP_PICKING'
        EXPORTING
          i_lgnum          = it311-lgnum
          i_refnr          = it311-refnr
          i_up_grp_anl     = ''
        IMPORTING
          e_material_error = lv_material_error
          et_messages      = lt_messages
        EXCEPTIONS
          error            = 1
          OTHERS           = 2.

      IF lv_material_error EQ abap_true.
*-->    Tenta retirar stock de picking
        CALL FUNCTION 'ZWM_TO_VALIDATE_PICKING'
          EXPORTING
            i_lgnum   = it311-lgnum
            i_refnr   = it311-refnr
          IMPORTING
            e_success = lv_success
          EXCEPTIONS
            error     = 1
            OTHERS    = 2.
      ENDIF.

      IF lv_success IS INITIAL.
        DELETE lt_messages WHERE msgid = 'L3' AND
                                 msgnr = '726'.

        IF NOT lt_messages IS INITIAL.
          READ TABLE lt_messages
                INTO ls_message
                INDEX 1.
          MESSAGE ID ls_message-msgid TYPE ls_message-msgtyp NUMBER ls_message-msgnr
                  WITH ls_message-msgv1
                       ls_message-msgv2
                       ls_message-msgv3
                       ls_message-msgv4.
        ENDIF.
      ENDIF.
    ENDIF.

*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
    DO 1 TIMES.
      SELECT SINGLE valor FROM zwm001
                          INTO lv_activo
                          WHERE armazem   = it311-lgnum AND
                                processo  = 'LIBERACAO_VIA_IDOC' AND
                                parametro = 'ACTIVAR'.
      CHECK lv_activo EQ abap_true.

      SELECT valor FROM zwm001
                   INTO TABLE lt_queue
                   WHERE armazem   = it311-lgnum AND
                         processo  = 'GESTAO_FILAR' AND
                         parametro IN ('FILA_AUT_REP','FILA_AUT_SD','FILA_AUT_PRM').
      CHECK NOT lt_queue IS INITIAL.

      SELECT * FROM ltak
               INTO TABLE lt_ltak
               FOR ALL ENTRIES IN lt_queue
               WHERE lgnum = it311-lgnum AND
                     refnr = it311-refnr AND
                     kquit = abap_false AND
                     queue = lt_queue-table_line.

      SELECT * FROM ltak
               APPENDING TABLE lt_ltak
               FOR ALL ENTRIES IN lt_queue
               WHERE lgnum = it311-lgnum AND
                     betyp = 'Z' AND
                     benum = it311-refnr AND
                     kquit = abap_false AND
                     queue = lt_queue-table_line.
      CHECK NOT lt_ltak IS INITIAL.

      SELECT * FROM ltap
               INTO TABLE lt_ltap
               FOR ALL ENTRIES IN lt_ltak
               WHERE lgnum = lt_ltak-lgnum AND
                     tanum = lt_ltak-tanum AND
                     pvqui = abap_false.
      CHECK sy-subrc EQ 0.

      LOOP AT lt_ltap INTO ls_ltap.
        CONCATENATE ls_ltap-vlpla(8) '*' INTO lv_lgpla.
        CLEAR: lr_s_lgpla.
        lr_s_lgpla-low = lv_lgpla.
        lr_s_lgpla-option = 'CP'.
        lr_s_lgpla-sign   = 'I'.
        APPEND lr_s_lgpla TO lr_lgpla.
      ENDLOOP.
      CHECK NOT lr_lgpla IS INITIAL.
      SORT lr_lgpla.
      DELETE ADJACENT DUPLICATES FROM lr_lgpla.

      SELECT * FROM ltap
               INTO TABLE lt_ltap
               WHERE lgnum = it311-lgnum AND
                     pvqui = abap_false AND
                     vlpla IN lr_lgpla.
      CHECK sy-subrc EQ 0.

      SELECT * FROM ltak
               INTO TABLE lt_ltak
               FOR ALL ENTRIES IN lt_ltap
               WHERE lgnum = lt_ltap-lgnum AND
                     tanum = lt_ltap-tanum.

      LOOP AT lt_ltak INTO ls_ltak.
        lv_tabix = sy-tabix.
        READ TABLE lt_queue
          WITH KEY table_line = ls_ltak-queue
          TRANSPORTING NO FIELDS.
        CHECK sy-subrc <> 0.
        DELETE lt_ltak WHERE tanum = ls_ltak-tanum.
        DELETE lt_ltap WHERE tanum = ls_ltak-tanum.
      ENDLOOP.

      CHECK NOT lt_ltak IS INITIAL.

      SORT lt_ltap BY vlpla DESCENDING.
      DELETE ADJACENT DUPLICATES FROM lt_ltap COMPARING vlpla.

      LOOP AT lt_ltap INTO ls_ltap.
        IF ls_ltap_last IS INITIAL.
          ls_ltap_last = ls_ltap.
          CONTINUE.
        ENDIF.

        IF ls_ltap-vlpla(8) EQ ls_ltap_last-vlpla(8).
          MESSAGE s296(zwmmsg001).
          RETURN.
        ENDIF.


        ls_ltap_last = ls_ltap.
      ENDLOOP.
    ENDDO.


    EXPORT lcoms TO MEMORY ID 'LT42A'.
    CALL TRANSACTION 'LT42' AND
              SKIP FIRST SCREEN.


    PERFORM verify_volum_pal_picking.

    PERFORM delete_lines_zwm026.

    PERFORM split_to_dri.

    PERFORM corrige_to_aut.

    PERFORM verify_paletes_remontadas.


*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 22.05.2012 18:25:58
*  Motivo: Update de Status de Grupo
*--------------------------------------------------------------------*
    CALL FUNCTION 'ZWM_UPDATE_ANALISE_GRUPOS'
      EXPORTING
        i_lgnum = it311-lgnum
        i_refnr = it311-refnr.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 22.05.2012 09:24:58
*  Motivo: Valida se Picking de Grupo está Completo
*--------------------------------------------------------------------*
    CLEAR: ls_t311.

    IF lv_2step EQ 'X'.
      CALL FUNCTION 'L_2_STEP_QUANTITY_REMOVAL'
        EXPORTING
          i_lgnum                       = lcoms-lgnum
          i_refnr                       = lcoms-refnr
        TABLES
          t_total                       = lt_total
        EXCEPTIONS
          refnr_no_found                = 1
          refnr_documents_no_found      = 2
          no_relevant_for_2step_picking = 3
          item_for_removal_not_found    = 4
          OTHERS                        = 5.

      IF sy-subrc <> 0.
        CLEAR lv_kzakt_kzerl.
      ELSE.
        DELETE lt_total WHERE ofmng = 0.

        IF lt_total IS INITIAL.
          lv_kzakt_kzerl = 'X'.
        ENDIF.
      ENDIF.
    ENDIF.

*    IF lv_2step EQ abap_true OR
*       lv_2spart EQ abap_true.
    CLEAR ls_t311.
    SELECT SINGLE * FROM t311
                    INTO ls_t311
                    WHERE lgnum = lcoms-lgnum AND
                          refnr = lcoms-refnr.
*    ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
    LOOP AT it311 WHERE refnr = lcoms-refnr.
**      IF lv_2step EQ 'X'.
**        it311-kzakt = lv_kzakt_kzerl.
**        it311-kzerl = lv_kzakt_kzerl.
**      ENDIF.

      IF NOT ls_t311 IS INITIAL.

        IF ls_t311-kzakt = 'X'.
          SELECT * FROM t311a INTO TABLE lt_t311a
              WHERE lgnum = lgnum
                AND refnr = lcoms-refnr.
          IF lt_t311a[] IS NOT INITIAL.
            SELECT * FROM vbuk INTO TABLE lt_vbuk
                FOR ALL ENTRIES IN lt_t311a
                    WHERE vbeln = lt_t311a-rbnum.

            "remove remessas em 2 passos
            LOOP AT lt_vbuk.
              lv_tabix = sy-tabix.

              CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
                EXPORTING
                  i_lgnum = ls_t311-lgnum
                  i_refnr = ls_t311-refnr
                  i_vbeln = lt_vbuk-vbeln
                IMPORTING
                  e_2step = lv_2step_vb
                EXCEPTIONS
                  error   = 1
                  OTHERS  = 2.

              IF lv_2step_vb EQ abap_true.
                DELETE lt_vbuk INDEX lv_tabix.
              ENDIF.
            ENDLOOP.

            DELETE lt_vbuk WHERE kostk = 'C'.
          ENDIF.
          IF lt_vbuk[] IS INITIAL.
            lv_kzakt_kzerl = abap_true.

*            it311-kzakt = 'X'.
*            it311-kzerl = 'X'.
**            UPDATE  t311 SET kzakt = 'X'
**                             kzerl = 'X'
**                 WHERE lgnum = lcoms-lgnum AND
**                       refnr = lcoms-refnr.
          ELSE.
            lv_kzakt_kzerl = abap_false.
          ENDIF.
        ENDIF.
*            it311-kzakt = 'X'.
*            it311-kzerl = 'X'.

*        MOVE-CORRESPONDING ls_t311 TO it311.
      ENDIF.

      it311-kreuz = space.

      MODIFY it311.
    ENDLOOP.

    IF lv_kzakt_kzerl EQ abap_true.
      UPDATE  t311 SET kzakt = lv_kzakt_kzerl
                       kzerl = lv_kzakt_kzerl
                  WHERE lgnum = lcoms-lgnum AND
                        refnr = lcoms-refnr.
      COMMIT WORK.
    ENDIF.


**    IF lv_2step EQ 'X'.
**      UPDATE  t311 SET kzakt = lv_kzakt_kzerl
**                       kzerl = lv_kzakt_kzerl
**                  WHERE lgnum = lcoms-lgnum AND
**                        refnr = lcoms-refnr.
**    ENDIF.


    MOVE sav_fname TO hlp_fname.
    MOVE sav_rbtyp TO sum00-rbtyp.

    SET PF-STATUS pfstat_liste2.
    SET TITLEBAR titel_liste2.
    sy-lsind = sy-lsind - 1.
    PERFORM detailliste1.

  ENDIF.

ENDFORM.                    "sammelgang_starten
*----------------------------------------------------------------------*
*        FORM DETAILLISTE2                                             *
*----------------------------------------------------------------------*
*        Bildschirmausgabe der Referenzbelege zu einer Referenznummer. *
*----------------------------------------------------------------------*
FORM detailliste2.

  LOOP AT it311a.
    PERFORM detailliste2_ausgabe.
    MODIFY it311a.
    CLEAR it311a.
  ENDLOOP.

  WRITE: /1(82) sy-uline.

ENDFORM.                    "detailliste2

*----------------------------------------------------------------------*
*        FORM KOPF_LISTE1_AUSGEBEN                                     *
*----------------------------------------------------------------------*
*        Kopf und Überschriften für die aggregierten LDK00-Daten.      *
*                                                                      *
*----------------------------------------------------------------------*
FORM kopf_liste1_ausgeben.

  WRITE:
*.........1.Zeile Kopf ...............................................
       /3(22) text-a14  COLOR COL_BACKGROUND INTENSIFIED,
       25     lgnum     COLOR COL_BACKGROUND INTENSIFIED OFF,
*.........2.Zeile Kopf ...............................................
       /3(22) text-a12  COLOR COL_BACKGROUND INTENSIFIED,
       25     datum-low COLOR COL_BACKGROUND INTENSIFIED OFF,
*.........3.Zeile Kopf ...............................................
       /3(22) text-a13  COLOR COL_BACKGROUND INTENSIFIED,
       25     datum-high COLOR COL_BACKGROUND INTENSIFIED OFF.
  SKIP 1.

ENDFORM.                    "kopf_liste1_ausgeben

*----------------------------------------------------------------------*
*        FORM KOPF_LISTE2_AUSGEBEN                                     *
*----------------------------------------------------------------------*
*        Kopf und Überschriften für Detailliste der T311 Daten.        *
*                                                                      *
*----------------------------------------------------------------------*
FORM kopf_liste2_ausgeben.

  WRITE: /2(22) text-a14    COLOR COL_BACKGROUND INTENSIFIED,
         25     lgnum       COLOR COL_BACKGROUND INTENSIFIED OFF,
         /2(22) text-a90  COLOR COL_BACKGROUND INTENSIFIED,
         25(18) liste1_titel1 COLOR COL_BACKGROUND INTENSIFIED OFF,
         /2(22) text-a91 COLOR COL_BACKGROUND INTENSIFIED,
         25(55) liste1_titel2 COLOR COL_BACKGROUND INTENSIFIED OFF.
  SKIP 1.
  IF offta EQ space.
    WRITE: /1(83) sy-uline.              "Unterstrich
  ELSE.
    WRITE: /1(87) sy-uline.              "Unterstrich
  ENDIF.
  WRITE: /1(83)  space COLOR COL_HEADING INTENSIFIED ON ,
          1      sy-vline,
          2(1)   text-a59 COLOR COL_HEADING INTENSIFIED ON ,
          3      sy-vline,
          4(10)  text-a60 COLOR COL_HEADING INTENSIFIED ON ,
          14     sy-vline,
          15(6)  text-a61 COLOR COL_HEADING INTENSIFIED ON ,
          21     sy-vline,
          22(40) text-a62 COLOR COL_HEADING INTENSIFIED ON ,
          62     sy-vline,
          63(8)  text-a63 COLOR COL_HEADING INTENSIFIED ON ,
          71     sy-vline,
          72(2)  text-a66 COLOR COL_HEADING INTENSIFIED ON ,
          74     sy-vline,
          75(9)  text-a64 COLOR COL_HEADING INTENSIFIED ON ,
          83     sy-vline.
  IF offta NE space.
    WRITE:  84(3)  text-a65 COLOR COL_HEADING INTENSIFIED ON ,
            87     sy-vline.
  ENDIF.
  IF offta EQ space.
    WRITE: /1(83)  sy-uline.              "Unterstrich
  ELSE.
    WRITE: /1(87)  sy-uline.              "Unterstrich
  ENDIF.

ENDFORM.                    "kopf_liste2_ausgeben

*----------------------------------------------------------------------*
*        FORM KOPF_LISTE3_AUSGEBEN                                     *
*----------------------------------------------------------------------*
*        Kopf und Überschriften für Detailliste der T311A Daten.       *
*                                                                      *
*----------------------------------------------------------------------*
FORM kopf_liste3_ausgeben.

  WRITE: /2(22) text-a14    COLOR COL_BACKGROUND INTENSIFIED,
         25     lgnum       COLOR COL_BACKGROUND INTENSIFIED OFF,
         /2(22) text-a92    COLOR COL_BACKGROUND INTENSIFIED,
         25     it311-refnr COLOR COL_BACKGROUND INTENSIFIED OFF,
         /2(22) text-a93    COLOR COL_BACKGROUND INTENSIFIED,
         25     it311-refnt COLOR COL_BACKGROUND INTENSIFIED OFF.

  IF it311-rbtyp = con_refnrbelegtyp_b.
    WRITE: /2(22) text-a90 COLOR COL_BACKGROUND INTENSIFIED,
           25(18) text-a30 COLOR COL_BACKGROUND INTENSIFIED OFF.
  ENDIF.
  IF it311-rbtyp = con_refnrbelegtyp_l.
    WRITE: /2(22) text-a90 COLOR COL_BACKGROUND INTENSIFIED,
           25(18) text-a31 COLOR COL_BACKGROUND INTENSIFIED OFF.
  ENDIF.
  WRITE: 50     it311-kzakt AS CHECKBOX INPUT OFF,
         52(20) text-b01   COLOR COL_BACKGROUND INTENSIFIED,
         /50    it311-kzerl AS CHECKBOX INPUT OFF,
         52(20) text-b02   COLOR COL_BACKGROUND INTENSIFIED,
         /50    it311-kzdru AS CHECKBOX INPUT OFF,
         52(20) text-b03   COLOR COL_BACKGROUND INTENSIFIED.

  SKIP 1.
  WRITE: /1(82) sy-uline.              "Unterstrich
  WRITE: /1(82)  space COLOR COL_HEADING INTENSIFIED ON ,
          1      sy-vline,
          2(1)   text-a59 COLOR COL_HEADING INTENSIFIED ON ,
          3      sy-vline,
          4(10)  text-b04 COLOR COL_HEADING INTENSIFIED ON ,
          14     sy-vline,
          15(6)  text-b05 COLOR COL_HEADING INTENSIFIED ON ,
          21     sy-vline,
          22(60) text-b07 COLOR COL_HEADING INTENSIFIED ON ,
          82     sy-vline.
  WRITE: /1(82)  sy-uline.              "Unterstrich

ENDFORM.                    "kopf_liste3_ausgeben

*----------------------------------------------------------------------*
*        FORM T311_SUM_AUSGEBEN                                        *
*----------------------------------------------------------------------*
*        Bildschirmausgabe der aggregierten T311-Daten.                *
*                                                                      *
*----------------------------------------------------------------------*
FORM t311_sum_ausgeben.

  SORT sum00 BY sort.

*........Überschriften...............................................
  WRITE: /2(70)  sy-uline.
  WRITE: /2      sy-vline,
          3(18)  text-a50 COLOR COL_HEADING INTENSIFIED,
          21     sy-vline,
          22(9)  text-a51 COLOR COL_HEADING INTENSIFIED,
          31     sy-vline,
          32(9)  text-a52 COLOR COL_HEADING INTENSIFIED,
          41     sy-vline,
          42(9)  text-a53 COLOR COL_HEADING INTENSIFIED,
          51     sy-vline,
          52(9)  text-a54 COLOR COL_HEADING INTENSIFIED,
          61     sy-vline,
          62(9)  text-a55 COLOR COL_HEADING INTENSIFIED,
          71     sy-vline.
  WRITE: /2(70)  sy-uline.
  LOOP AT sum00.

*---Gesamtsumme zum Schluß---------------------------------------------*
    IF sum00-rbtyp = high_value.
      sum01 = sum00.
    ELSE.
*........Summenzeile....................................................
      PERFORM write_zeile.
    ENDIF.

  ENDLOOP.

  WRITE: /2(70)  sy-uline.
  SKIP 1.
  WRITE: /2(70)  sy-uline.
  sum00 = sum01.
  WRITE: /2      sy-vline,
          3      sum00-rtext COLOR COL_TOTAL,
          21     sy-vline,
          22     sum00-r_ers COLOR COL_TOTAL,
          31     sy-vline,
          32     sum00-r_off COLOR COL_TOTAL,
          41     sy-vline,
          42     sum00-r_erl COLOR COL_TOTAL,
          51     sy-vline,
          52     sum00-r_fre COLOR COL_TOTAL,
          61     sy-vline,
          62     sum00-r_lin COLOR COL_TOTAL,
          71     sy-vline.
  HIDE:   sum00-rbtyp,
          sum00-r_ers,
          sum00-r_off,
          sum00-r_erl,
          sum00-r_fre,
          sum00-r_lin.
  WRITE: /2(70)  sy-uline.

  IF hlp_gefunden IS INITIAL.
    SKIP 5.
    WRITE: /30 text-a40.            "Keine Daten selektiert
  ENDIF.

*-------Kopfzeile initialisieren---------------------------------------*
  CLEAR sum00.

ENDFORM.                    "t311_sum_ausgeben

*----------------------------------------------------------------------*
*        FORM WRITE_ZEILE                                              *
*----------------------------------------------------------------------*
*        Summen-Einzelzeilen.                                          *
*----------------------------------------------------------------------*
FORM write_zeile.
  WRITE: /2  sy-vline,
          3  sum00-rtext COLOR COL_KEY,
          21 sy-vline,
          22 sum00-r_ers COLOR COL_NORMAL INTENSIFIED OFF,
          31 sy-vline,
          32 sum00-r_off COLOR COL_NORMAL INTENSIFIED OFF,
          41 sy-vline,
          42 sum00-r_erl COLOR COL_NORMAL INTENSIFIED OFF,
          51  sy-vline,
          52 sum00-r_fre COLOR COL_NORMAL INTENSIFIED OFF,
          61 sy-vline,
          62 sum00-r_lin COLOR COL_NORMAL INTENSIFIED OFF,
          71 sy-vline.
  HIDE:   sum00-rbtyp,
          sum00-r_ers,
          sum00-r_off,
          sum00-r_erl,
          sum00-r_fre,
          sum00-r_lin.

ENDFORM.                    "write_zeile

*----------------------------------------------------------------------*
*        FORM T311_SUM_AUSGEBEN_TA                                     *
*----------------------------------------------------------------------*
*        Bildschirmausgabe der aggregierten T311-Daten mit Informa-    *
*        tionen über offene Transportaufträge.                         *
*----------------------------------------------------------------------*
FORM t311_sum_ausgeben_ta.

  SORT sum00 BY sort.

*........Überschriften...............................................
  WRITE: /2(70)  sy-uline,
          73(11) sy-uline.
  WRITE: /2      sy-vline,
          3(18)  text-a50 COLOR COL_HEADING INTENSIFIED,
          21     sy-vline,
          22(9)  text-a51 COLOR COL_HEADING INTENSIFIED,
          31     sy-vline,
          32(9)  text-a52 COLOR COL_HEADING INTENSIFIED,
          41     sy-vline,
          42(9)  text-a53 COLOR COL_HEADING INTENSIFIED,
          51     sy-vline,
          52(9)  text-a54 COLOR COL_HEADING INTENSIFIED,
          61     sy-vline,
          62(9)  text-a55 COLOR COL_HEADING INTENSIFIED,
          71     sy-vline.
  WRITE:  73     sy-vline,
          74(9)  text-a56 COLOR COL_HEADING INTENSIFIED,
          83     sy-vline.
  WRITE: /2(70)  sy-uline,
          73(11) sy-uline.
  LOOP AT sum00.

*---Gesamtsumme zum Schluß---------------------------------------------*
    IF sum00-rbtyp = high_value.
      sum01 = sum00.
    ELSE.
*........Summenzeile....................................................
      PERFORM write_zeile_ta.
    ENDIF.

  ENDLOOP.

  WRITE: /2(70)  sy-uline,
          73(11) sy-uline.
  SKIP 1.
  WRITE: /2(70)  sy-uline,
          73(11) sy-uline.
  sum00 = sum01.
  WRITE: /2      sy-vline,
          3      sum00-rtext COLOR COL_TOTAL,
          21     sy-vline,
          22     sum00-r_ers COLOR COL_TOTAL,
          31     sy-vline,
          32     sum00-r_off COLOR COL_TOTAL,
          41     sy-vline,
          42     sum00-r_erl COLOR COL_TOTAL,
          51     sy-vline,
          52     sum00-r_fre COLOR COL_TOTAL,
          61     sy-vline,
          62     sum00-r_lin COLOR COL_TOTAL,
          71     sy-vline.
  WRITE:  73     sy-vline,
          74     sum00-r_tao COLOR COL_TOTAL,
          83     sy-vline.
  HIDE:   sum00-rbtyp,
          sum00-r_ers,
          sum00-r_off,
          sum00-r_erl,
          sum00-r_fre,
          sum00-r_lin,
          sum00-r_tao.
  WRITE: /2(70)  sy-uline,
          73(11) sy-uline.

  IF hlp_gefunden IS INITIAL.
    SKIP 5.
    WRITE: /30 text-a40.            "Keine Daten selektiert
  ENDIF.

*-------Kopfzeile initialisieren---------------------------------------*
  CLEAR sum00.

ENDFORM.                    "t311_sum_ausgeben_ta

*----------------------------------------------------------------------*
*        FORM WRITE_ZEILE_TA                                           *
*----------------------------------------------------------------------*
*        Summen-Einzelzeilen bei Anforderung mit Info über TA.         *
*----------------------------------------------------------------------*
FORM write_zeile_ta.
  WRITE: /2  sy-vline,
          3  sum00-rtext COLOR COL_KEY,
          21 sy-vline,
          22 sum00-r_ers COLOR COL_NORMAL INTENSIFIED OFF,
          31 sy-vline,
          32 sum00-r_off COLOR COL_NORMAL INTENSIFIED OFF,
          41 sy-vline,
          42 sum00-r_erl COLOR COL_NORMAL INTENSIFIED OFF,
          51  sy-vline,
          52 sum00-r_fre COLOR COL_NORMAL INTENSIFIED OFF,
          61 sy-vline,
          62 sum00-r_lin COLOR COL_NORMAL INTENSIFIED OFF,
          71 sy-vline.
  WRITE:  73 sy-vline,
          74 sum00-r_tao COLOR COL_NORMAL INTENSIFIED OFF,
          83 sy-vline.
  HIDE:   sum00-rbtyp,
          sum00-r_ers,
          sum00-r_off,
          sum00-r_erl,
          sum00-r_fre,
          sum00-r_lin,
          sum00-r_tao.

ENDFORM.                    "write_zeile_ta

*---------------------------------------------------------------------*
*        FORM CHANGE_INTENSIFIED                                      *
*---------------------------------------------------------------------*
FORM change_intensified.
  IF flg_intens = 0.
    FORMAT INTENSIFIED ON.
    flg_intens = 1.
  ELSE.
    FORMAT INTENSIFIED OFF.
    flg_intens = 0.
  ENDIF.
ENDFORM.                    "change_intensified


*----------------------------------------------------------------------*
*        FORM DETAILLISTE1_AUSGABE                                     *
*----------------------------------------------------------------------*
*        Write-Anweisungen zu DETAILLISTE1.                            *
*----------------------------------------------------------------------*
FORM detailliste1_ausgabe.

  PERFORM change_intensified.
  WRITE: /1 sy-vline.
  it311-linno = sy-linno.
  it311-pagno = sy-pagno.
  IF offta EQ space.
    WRITE:  3(81) space  COLOR COL_NORMAL.
  ELSE.
    WRITE:  3(85) space  COLOR COL_NORMAL.
  ENDIF.
  WRITE:  2 it311-kreuz AS CHECKBOX INPUT,
          3 sy-vline,
          4 it311-refnr COLOR COL_NORMAL,
         14 sy-vline,
         15 it311-tbedn COLOR COL_NORMAL,
         18 it311-liefn COLOR COL_NORMAL,
         21 sy-vline,
         22 it311-refnt COLOR COL_NORMAL,
         62 sy-vline,
         63 it311-datum DD/MM/YY COLOR COL_NORMAL,
         71 sy-vline,
         72 it311-l2skr COLOR COL_NORMAL,
         74 sy-vline,
         76 it311-kzakt COLOR COL_NORMAL,
         79 it311-kzerl COLOR COL_NORMAL,
         82 it311-kzdru COLOR COL_NORMAL,
         83 sy-vline.
  IF offta NE space.
    WRITE: 84 it311-k_tao COLOR COL_NORMAL,
           87 sy-vline.
  ENDIF.

  HIDE:     hlp_fname,
            it311-linno,
            it311-pagno,
            it311-refnr,
            it311-tbedn,
            it311-liefn,
            it311-refnt,
            it311-datum,
            it311-l2skr,
            it311-kzakt,
            it311-kzerl,
            it311-kzdru,
            it311-rbtyp,
            it311-k_tao,
            sy-tabix.

*-------Kopfzeile initialisieren---------------------------------------*

ENDFORM.                    "detailliste1_ausgabe
*----------------------------------------------------------------------*
*        FORM DETAILLISTE2_AUSGABE                                     *
*----------------------------------------------------------------------*
*        Write-Anweisungen zu DETAILLISTE2.                            *
*----------------------------------------------------------------------*
FORM detailliste2_ausgabe.
  DATA: ls_t311 TYPE t311.

  PERFORM change_intensified.
*........Wenn Liste: Offene oder Fehlerhafte, dann mit Checkboxen.....
  WRITE: /1 sy-vline.
  it311a-linno = sy-linno.
  it311a-pagno = sy-pagno.
  WRITE:  2     it311a-kreuz AS CHECKBOX INPUT,
          3(79) space  COLOR COL_NORMAL,
          3     sy-vline,
          4     it311a-rbnum COLOR COL_NORMAL,
         14     sy-vline,
         16     it311a-bstat COLOR COL_NORMAL,
         19     it311a-vstat COLOR COL_NORMAL,
         21     sy-vline,
         22(60) it311a-ftext COLOR COL_NORMAL,
         82     sy-vline.

** Valida se Tem Todas as OT's Criadas
***********************************************************************
  SELECT SINGLE * FROM t311
                  INTO ls_t311
                  WHERE lgnum = it311a-lgnum AND
                        refnr = it311a-refnr.

  IF ls_t311-kzerl EQ abap_true.
    CLEAR: it311a-ftext.
  ENDIF.
**********************************************************************



  HIDE:     it311a-linno,
            it311a-pagno,
            it311a-lgnum,
            it311a-refnr,
            it311a-rbtyp,
            it311a-rbnum,
            it311a-bstat,
            it311a-vstat,
            it311a-tanum,
            it311a-ftext,
            sy-tabix.

*-------Kopfzeile initialisieren---------------------------------------*

ENDFORM.                    "detailliste2_ausgabe
*----------------------------------------------------------------------*
*        FORM MARKIER_KONSISTENZ                                       *
*----------------------------------------------------------------------*
FORM markier_konsistenz.
  CLEAR markier_return.
* IF SAV_FNAME <> OFFENE AND SAV_FNAME <> FEHLER.    "!!!!!!!
*   MARKIER_RETURN = 1.                              "!!!!!!!
*   MESSAGE I087.                                    "!!!!!!!
* ENDIF.                                             "!!!!!!!
ENDFORM.                    "markier_konsistenz

*----------------------------------------------------------------------*
*        FORM MARKIER_KONSISTENZ_DETAIL2                               *
*----------------------------------------------------------------------*
FORM markier_konsistenz_detail2.
  CLEAR markier_return.
ENDFORM.                    "markier_konsistenz_detail2

*----------------------------------------------------------------------*
*        FORM MARKIER_SELEKTION                                        *
*----------------------------------------------------------------------*
FORM markier_selektion USING p_fcode.

  DATA: hlp_refnr LIKE t311-refnr.

  flg_return = space.
*........Einzelverarbeitung...........................................
  IF p_fcode = fcode_mark.       "Einzel
    hlp_refnr = it311-refnr.
    LOOP AT it311 WHERE refnr = hlp_refnr.
      IF it311-kreuz EQ space.
        MOVE con_x TO it311-kreuz.
      ELSE.
        MOVE space TO it311-kreuz.
      ENDIF.
      MODIFY it311.
      flg_return = 1.
    ENDLOOP.
  ELSE.
*........Viele markieren..............................................
    such_k_ers = ' '.
    such_k_off = ' '.
    such_k_erl = ' '.
    such_k_fre = ' '.
*   SUCH_K_TAO = ' '.
*-- Suchfelder zuweisen -----------------------------------------------*
    CASE sav_fname.
      WHEN erstellte.
        such_k_ers = 'X'.
      WHEN offene.
        such_k_off = 'X'.
      WHEN erledigte.
        such_k_erl = 'X'.
      WHEN freigegebene.
        such_k_fre = 'X'.
      WHEN alle.                       "->Zeilensumme, dh. kein Status
    ENDCASE.

    such_rbtyp = sav_rbtyp.

*-- IT311 gemäß der Suchfelder durchsuchen und ausgeben----------------*
    LOOP AT it311.
      CASE sav_rbtyp.
        WHEN con_refnrbelegtyp_b.
          ASSIGN it311-tbedn TO <feldname>.
        WHEN con_refnrbelegtyp_l.
          ASSIGN it311-liefn TO <feldname>.
        WHEN high_value(1).
      ENDCASE.
      IF sav_fname = alle.                      "Zeilensumme?
        IF such_rbtyp(1) = high_value(1).    "Gesamtsumme?
          IF it311-rbtyp = con_refnrbelegtyp_b OR "Selektion alles
             it311-rbtyp = con_refnrbelegtyp_l.
            PERFORM markier_selektion_alle USING p_fcode.
          ENDIF.
        ELSE.                                   "Keine Gesamtsumme!
          IF <feldname> = con_x.                "Selektion der Sätze
            PERFORM markier_selektion_alle USING p_fcode.
          ENDIF.
        ENDIF.
      ELSE.
        IF such_rbtyp(1) = high_value(1). "Gesamtsumme?
          IF it311-k_ers = such_k_ers AND       "Selektion der Sätze
             it311-k_off = such_k_off AND       "Gesamtsumme statusabh.
             it311-k_erl = such_k_erl AND
             it311-k_fre = such_k_fre.
            IF it311-rbtyp = con_refnrbelegtyp_b OR
                  it311-rbtyp = con_refnrbelegtyp_l.
              PERFORM markier_selektion_alle USING p_fcode.
            ENDIF.
          ENDIF.
        ELSE.
          IF it311-k_ers = such_k_ers AND       "Selektion der Sätze
             it311-k_off = such_k_off AND       "Einzelsumme
             it311-k_erl = such_k_erl AND
             it311-k_fre = such_k_fre.
            IF <feldname> = con_x.              "Selektion der Sätze
              PERFORM markier_selektion_alle USING p_fcode.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.
  ENDIF.

ENDFORM.                    "markier_selektion

*----------------------------------------------------------------------*
*        FORM MARKIER_SELEKTION_ALLE                                   *
*----------------------------------------------------------------------*
FORM markier_selektion_alle USING p_fcode.

*........Aktion nach erfolgreicher Selektion............................
  CASE p_fcode.
    WHEN fcode_mara.                       "alle markieren
      MOVE con_x TO it311-kreuz.
    WHEN fcode_mare.                       "alle entmarkieren
      MOVE space TO it311-kreuz.
  ENDCASE.

  MODIFY it311.
  CLEAR it311.
  flg_return = 1.

ENDFORM.                    "markier_selektion_alle
*----------------------------------------------------------------------*
*        FORM MARKIER_SELEKTION_DETAIL2                                *
*----------------------------------------------------------------------*
FORM markier_selektion_detail2 USING p_fcode.

  DATA: hlp_rbnum LIKE t311a-rbnum.

  flg_return = space.
*........Einzelverarbeitung...........................................
  IF p_fcode = fcode_mark.       "Einzel
    hlp_rbnum = it311a-rbnum.
    LOOP AT it311a WHERE rbnum = hlp_rbnum.
      IF it311a-kreuz EQ space.
        MOVE con_x TO it311a-kreuz.
      ELSE.
        MOVE space TO it311a-kreuz.
      ENDIF.
      MODIFY it311a.
      flg_return = 1.
    ENDLOOP.
  ELSE.
*........alle markieren...............................................
    LOOP AT it311a.
      CASE p_fcode.
        WHEN fcode_mara.                       "alle markieren
          MOVE con_x TO it311a-kreuz.
        WHEN fcode_mare.                       "alle entmarkieren
          MOVE space TO it311a-kreuz.
      ENDCASE.
      MODIFY it311a.
      CLEAR it311a.
      flg_return = 1.
    ENDLOOP.
  ENDIF.

ENDFORM.                    "markier_selektion_detail2

*---------------------------------------------------------------------*
*       FORM PAI                                                      *
*---------------------------------------------------------------------*
*       Markieren in Liste                                            *
*---------------------------------------------------------------------*
FORM pai.
  DATA:  sav_lisel   LIKE sy-lisel.
  DATA:  sav_rbtyp   LIKE t311-rbtyp.

  IF sy-lsind = 1 OR sy-lsind = 2.
*........Retten eventuell benötigter Daten..............................
    sav_lisel = sy-lisel.
    sav_it311 = it311.
    sav_rbtyp = sum00-rbtyp.
*........Schleife .....................................................
    LOOP AT it311.
      READ LINE it311-linno OF PAGE it311-pagno.
      IF sy-subrc <> 0.
        CHECK 1 = 2.
      ENDIF.
      IF sy-lisel+1(1) = space OR sy-lisel+1(1) = con_x.
        MOVE sy-lisel+1(1) TO it311-kreuz.
        MODIFY it311.
      ENDIF.
    ENDLOOP.
    sum00-rbtyp     = sav_rbtyp.
    it311           = sav_it311.
    sy-lisel        = sav_lisel.
    CLEAR sav_lisel.
    CLEAR sav_it311.
  ENDIF.
  IF sy-lsind = 3.
*........Retten eventuell benötigter Daten..............................
    sav_lisel = sy-lisel.
    sav_it311a = it311a.
    sav_rbtyp = sum00-rbtyp.
*........Schleife .....................................................
    LOOP AT it311a.
      READ LINE it311a-linno OF PAGE it311a-pagno.
      IF sy-subrc <> 0.
        CHECK 1 = 2.
      ENDIF.
      IF sy-lisel+1(1) = space OR sy-lisel+1(1) = con_x.
        MOVE sy-lisel+1(1) TO it311a-kreuz.
        MODIFY it311a.
      ENDIF.
    ENDLOOP.
    sum00-rbtyp     = sav_rbtyp.
    sy-lisel        = sav_lisel.
    it311a          = sav_it311a.
    CLEAR sav_lisel.
    CLEAR sav_it311a.
  ENDIF.
ENDFORM.                    "pai

*---------------------------------------------------------------------*
*       FORM ZURUECK_AUS_LISTE                                        *
*---------------------------------------------------------------------*
*       Entmarkieren in interner Tabelle bei Funktionscode Zurück     *
*---------------------------------------------------------------------*
FORM zurueck_aus_liste.

  IF sy-lsind = 1 OR sy-lsind = 2.
    LOOP AT it311 WHERE kreuz EQ con_x.
      MOVE space TO it311-kreuz.
      MODIFY it311.
    ENDLOOP.
  ENDIF.
  IF sy-lsind = 3.
    LOOP AT it311a WHERE kreuz EQ con_x.
      MOVE space TO it311a-kreuz.
      MODIFY it311a.
    ENDLOOP.
  ENDIF.
ENDFORM.                    "zurueck_aus_liste

*----------------------------------------------------------------------*
*        FORM RBNUM_LOESCHEN  (analog RBNUM_LOESCHEN_NACH in ML05SFFC) *
*----------------------------------------------------------------------*
FORM rbnum_loeschen USING p_lgnum p_refnr p_rbtyp p_subrc.

  CALL FUNCTION 'L_REF_DELETE_DOC'
    EXPORTING
      i_lgnum = p_lgnum
      i_refnr = p_refnr
      i_rbtyp = p_rbtyp
    TABLES
      t_rbnum = t_t311a.
*          EXCEPTIONS
*               REFNR_BLOCKED        = 1
*               STATUS_BLOCKED       = 2
*               STATUS_ACTIV         = 3
*               STATUS_TO_CREATED    = 4
*               STATUS_RELEASED      = 5
*               REFNR_DOES_NOT_EXIST = 6.

  CASE sy-subrc.
    WHEN 1.
      MESSAGE i019 WITH p_refnr.
      p_subrc = 4.
      EXIT.
    WHEN 2.
      p_subrc = 4.
      EXIT.
    WHEN 3.
      p_subrc = 4.
      EXIT.
    WHEN 4.
      p_subrc = 4.
      EXIT.
    WHEN 5.
      p_subrc = 4.
      EXIT.
    WHEN 6.
      p_subrc = 4.
      MESSAGE e006 WITH p_refnr.
  ENDCASE.

ENDFORM.                    "rbnum_loeschen
*----------------------------------------------------------------------*
*        FORM POPUP_BEI_LOESCHEN  (analog FC912 in ML05SFFC)           *
*----------------------------------------------------------------------*
FORM popup_bei_loeschen.

  CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
    EXPORTING
      titel         = text-b40
      textline1     = text-b41
      textline2     = text-b42
      defaultoption = antwort_nein
    IMPORTING
      answer        = antwort.

ENDFORM.                    "popup_bei_loeschen
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_SEL_VARIANT
*&---------------------------------------------------------------------*
*       value help for selection variant of report picking progress
*----------------------------------------------------------------------*
FORM f4_for_sel_variant.

  DATA: lf_variant  LIKE rsvar-variant,
        lf_var_text LIKE rsvar-vtext.

  CALL FUNCTION 'RS_VARIANT_CATALOG'
    EXPORTING
      report              = con_report_rllt2900
*     NEW_TITLE           = ' '
*     DYNNR               =
*     INTERNAL_CALL       = ' '
*     MASKED              = 'X'
*     VARIANT             = ' '
    IMPORTING
      sel_variant         = lf_variant
      sel_variant_text    = lf_var_text
*    TABLES
*     BELONGING_DYNNR     =
    EXCEPTIONS
      no_report           = 1
      report_not_existent = 2
      report_not_supplied = 3
      no_variants         = 4
      error_message       = 5
      OTHERS              = 6.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  gf_selv = lf_variant.

ENDFORM.                    " F4_FOR_SEL_VARIANT
*&---------------------------------------------------------------------*
*&      Form  CHECK_SEL_VARIANT_EXISTENCE
*&---------------------------------------------------------------------*
*       check, whether selection variant exists
*----------------------------------------------------------------------*
FORM check_sel_variant_existence.

  DATA: lf_subrc  LIKE sy-subrc.

  CHECK NOT gf_selv IS INITIAL.

  CALL FUNCTION 'RS_VARIANT_EXISTS'
    EXPORTING
      report              = con_report_rllt2900
      variant             = gf_selv
    IMPORTING
      r_c                 = lf_subrc
    EXCEPTIONS
      not_authorized      = 1
      no_report           = 2
      report_not_existent = 3
      report_not_supplied = 4
      OTHERS              = 5.

  IF lf_subrc <> 0
  OR sy-subrc <> 0.
    MESSAGE e065(vo).         "Selektionsvariante existiert nicht
  ENDIF.

ENDFORM.                    " CHECK_SEL_VARIANT_EXISTENCE


*****************SAPCONSOLE*****************

*&---------------------------------------------------------------------*
*&      Module  STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0001 OUTPUT.

  SET PF-STATUS 'PORTA'.
  CLEAR: pulmao, pick_pre.

ENDMODULE.                 " STATUS_0001  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  user_exit  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_exit INPUT.

  IF ok_code_0001 EQ 'CANCE'.
    SET SCREEN '0000'.
    LEAVE SCREEN.
  ENDIF.

ENDMODULE.                 " user_exit  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0001  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0001 INPUT.

*  DATA: i_zwm007 LIKE zwm007 OCCURS 0 WITH HEADER LINE,
*        i_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE,
*        i_lagp LIKE lagp OCCURS 0 WITH HEADER LINE.
*
*  DATA: num_paletes(2), pulmao_aux(3), pulmao_ LIKE lagp-lgpla,
*        encontrou_pulmao(1), num_quantos TYPE i.
*
**  DATA: t_vbss LIKE vbss OCCURS 0 WITH HEADER LINE.
*
*  DATA: t_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE.
*
*  CHECK NOT pulmao IS INITIAL.
*
*** RL -> INS 23.05.2005
*** Nova actualização da tabela ZWM028 tendo em conta eliminação
*** de items nas remessas
*
*  CALL FUNCTION 'ZWM_ACTUALIZA_PAL_GRUPO'
*    EXPORTING
*      lgnum                = it311-lgnum
*      refnr                = it311-refnr
*    EXCEPTIONS
*      actualizacao_zwm0028 = 1
*      OTHERS               = 2.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*** RL <- INS 23.05.2005
*
*** Actualizar o total de paletes a nivel do grupo
*
*  CLEAR t_zwm028.
*  REFRESH t_zwm028.
*
*  SELECT * INTO TABLE t_zwm028 FROM zwm028 WHERE refnr = it311-refnr.
*
*  CLEAR num_paletes.
*
*  LOOP AT t_zwm028.
*    CLEAR zwm028.
*    SELECT SINGLE *
*      FROM zwm028
*          WHERE lgnum = t_zwm028-lgnum AND
*                refnr = t_zwm028-refnr AND
*                remessa = t_zwm028-remessa.
*    IF sy-subrc = 0.
*      num_paletes = num_paletes + zwm028-total_paletes.
*    ENDIF.
*  ENDLOOP.
*
**  CLEAR t_vbss.
**  REFRESH t_vbss.
**
**  CLEAR num_paletes.
**  SELECT * INTO TABLE t_vbss FROM vbss WHERE sammg = it311-refnr.
**
**  LOOP AT t_vbss.
**    CLEAR zwm028.
**    SELECT SINGLE *
**      FROM zwm028
**          WHERE lgnum = it311-lgnum AND
**                refnr = t_vbss-sammg AND
**                remessa = t_vbss-vbeln.
**    IF sy-subrc = 0.
**      num_paletes = num_paletes + zwm028-total_paletes.
**    ENDIF.
**  ENDLOOP.
*
*
*  CLEAR i_zwm028.
*  REFRESH i_zwm028.
*
*  SELECT * INTO TABLE i_zwm028
*      FROM zwm028
*          WHERE lgnum = it311-lgnum AND
*                refnr = it311-refnr.
**** LOG ZWM028
**
**  LOOP AT i_zwm028.
**    CLEAR: zwm028_log, wa_log.
**    GET TIME.
**    MOVE-CORRESPONDING i_zwm028 TO wa_log.
**    wa_log-data = sy-datum.
**    wa_log-hora = sy-uzeit.
**    wa_log-ini_fim = 'INI'.
**    wa_log-user_tarefa = sy-uname.
**    wa_log-programa = sy-repid.
**
**    MODIFY zwm028_log FROM wa_log.
**    COMMIT WORK AND WAIT.
**    WAIT UP TO 2 SECONDS.
**  ENDLOOP.
**** LOG ZWM028
*
*  READ TABLE i_zwm028 INDEX 1.
*  CLEAR i_zwm028-remessa.
*  CLEAR i_zwm028-ordem.
*  i_zwm028-refnr = it311-refnr.
*  i_zwm028-total_paletes = num_paletes.
*** RL -> INS 14.04.2005 ----------------------------------------------
*** Servisan
*  CLEAR: i_zwm028-servisan, i_zwm028-emissor.
*** RL <- INS 14.04.2005 ----------------------------------------------
*
*  INSERT INTO zwm028 VALUES i_zwm028.
*  IF sy-subrc = 0.
*    COMMIT WORK AND WAIT.
*  ELSE.
*    ROLLBACK WORK.
*    ok_code_0001 = 'CANCE'.
*    CLEAR: pulmao, ok_code_0001.
*    SET  SCREEN '0000'.LEAVE SCREEN.
*  ENDIF.
*
*  WHILE 1 = 1.
*    SELECT SINGLE *
*        FROM zwm028
*            WHERE lgnum = it311-lgnum AND
*                  refnr = it311-refnr AND
*                  remessa = ' '.
*    IF sy-subrc = 0.
*      EXIT.
*    ELSE.
*      WAIT UP TO 1 SECONDS.
*    ENDIF.
*  ENDWHILE.
*
*  DATA: pos TYPE i, aux_pos.
*  CLEAR aux_pos.
*  pos = 1.
*
*  DELETE i_zwm028 WHERE remessa IS INITIAL.
*
*  SORT i_zwm028 BY ordem ASCENDING.
*
*  LOOP AT i_zwm028.
*
*    i_zwm028-posicao_ini_pul = pos.
*
*    MODIFY i_zwm028 INDEX sy-tabix.
*    pos = pos + i_zwm028-total_paletes.
*
*  ENDLOOP.
*
*  MODIFY zwm028 FROM TABLE i_zwm028.
*  COMMIT WORK AND WAIT.
*
**** LOG ZWM028
**  CLEAR i_zwm028.
**  REFRESH i_zwm028.
**
**  SELECT * INTO TABLE i_zwm028
**    FROM zwm028
**        WHERE lgnum = it311-lgnum AND
**              refnr = it311-refnr.
**
**  LOOP AT i_zwm028.
**    CLEAR: zwm028_log, wa_log.
**    GET TIME.
**    MOVE-CORRESPONDING i_zwm028 TO wa_log.
**    wa_log-data = sy-datum.
**    wa_log-hora = sy-uzeit.
**    wa_log-ini_fim = 'FIM'.
**    wa_log-user_tarefa = sy-uname.
**    wa_log-programa = sy-repid.
**
**    MODIFY zwm028_log FROM wa_log.
**    COMMIT WORK AND WAIT.
**    WAIT UP TO 2 SECONDS.
**  ENDLOOP.
**** LOG ZWM028
*
*  REFRESH i_zwm007.
*  CASE pulmao.
*
*    WHEN 'PUL'.
*
*      CLEAR zwm028.
*      IF pick_pre IS INITIAL.
*        UPDATE zwm028
*              SET st_pul = pulmao
*                  WHERE lgnum = it311-lgnum AND
*                        refnr = it311-refnr." AND
**                        remessa EQ ' '.
*      ELSE.
*        UPDATE zwm028
*              SET st_pul = pulmao
*                  st_ppk = pick_pre
*                  WHERE lgnum = it311-lgnum AND
*                        refnr = it311-refnr. " AND
**                        remessa EQ ' '.
*      ENDIF.
*
*      COMMIT WORK AND WAIT.
*
*  ENDCASE.
*
*  CLEAR: pulmao, pick_pre, ok_code_0001.
*  SET  SCREEN '0000'.LEAVE SCREEN.
ENDMODULE.                 " USER_COMMAND_0001  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0003 OUTPUT.
  SET PF-STATUS 'PORTA'.
  CLEAR: porta_pul, pick_pre.

ENDMODULE.                 " STATUS_0003  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0003  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0003 INPUT.

*  REFRESH: i_zwm007,
*           i_zwm028 ,
*           i_lagp .
*
*  CLEAR: num_paletes, pulmao_aux, pulmao_ ,
*        encontrou_pulmao, num_quantos.
************************************
*  CHECK NOT porta_pul IS INITIAL.
*
*** RL -> INS 23.05.2005
*** Nova actualização da tabela ZWM028 tendo em conta eliminação
*** de items nas remessas
*  CALL FUNCTION 'ZWM_ACTUALIZA_PAL_GRUPO'
*    EXPORTING
*      lgnum                = it311-lgnum
*      refnr                = it311-refnr
*    EXCEPTIONS
*      actualizacao_zwm0028 = 1
*      OTHERS               = 2.
*  IF sy-subrc <> 0.
** MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
*** RL <- INS 23.05.2005
*
*** Actualizar o total de paletes a nivel do grupo
*  CLEAR t_zwm028.
*  REFRESH t_zwm028.
*
*  SELECT * INTO TABLE t_zwm028 FROM zwm028 WHERE refnr = it311-refnr.
*
*  CLEAR num_paletes.
*
*  LOOP AT t_zwm028.
*    CLEAR zwm028.
*    SELECT SINGLE *
*      FROM zwm028
*          WHERE lgnum = t_zwm028-lgnum AND
*                refnr = t_zwm028-refnr AND
*                remessa = t_zwm028-remessa.
*    IF sy-subrc = 0.
*      num_paletes = num_paletes + zwm028-total_paletes.
*    ENDIF.
*  ENDLOOP.
*
**  CLEAR t_vbss.
**  REFRESH t_vbss.
**
**  CLEAR num_paletes.
**  SELECT * INTO TABLE t_vbss FROM vbss WHERE sammg = it311-refnr.
**
**  LOOP AT t_vbss.
**    CLEAR zwm028.
**    SELECT SINGLE *
**      FROM zwm028
**          WHERE lgnum = it311-lgnum AND
**                refnr = t_vbss-sammg AND
**                remessa = t_vbss-vbeln.
**    IF sy-subrc = 0.
**      num_paletes = num_paletes + zwm028-total_paletes.
**    ENDIF.
**  ENDLOOP.
*
*  CLEAR i_zwm028.
*  REFRESH i_zwm028.
*  SELECT * INTO TABLE i_zwm028
*      FROM zwm028
*          WHERE lgnum = it311-lgnum AND
*                refnr = it311-refnr.
*
*  READ TABLE i_zwm028 INDEX 1.
*  CLEAR i_zwm028-remessa.
*  CLEAR i_zwm028-ordem.
*  i_zwm028-refnr = it311-refnr.
*  i_zwm028-total_paletes = num_paletes.
*
*** RL -> INS 14.04.2005 ----------------------------------------------
*** Servisan
*  CLEAR: i_zwm028-servisan, i_zwm028-emissor.
*** RL <- INS 14.04.2005 ----------------------------------------------
*
*  INSERT INTO zwm028 VALUES i_zwm028.
*  IF sy-subrc = 0.
*    COMMIT WORK AND WAIT.
*  ELSE.
*    ROLLBACK WORK.
*    ok_code_0001 = 'CANCE'.
*    CLEAR: porta_pul, ok_code_0001.
*    SET  SCREEN '0000'.LEAVE SCREEN.
*  ENDIF.
*
*  WHILE 1 = 1.
*    SELECT SINGLE *
*        FROM zwm028
*            WHERE lgnum = it311-lgnum AND
*                  refnr = it311-refnr AND
*                  remessa = ' '.
*    IF sy-subrc = 0.
*      EXIT.
*    ELSE.
*      WAIT UP TO 1 SECONDS.
*    ENDIF.
*  ENDWHILE.
*
*  CLEAR: aux_pos,pos.
*  pos = 1.
*
*  DELETE i_zwm028 WHERE remessa IS INITIAL.
*  COMMIT WORK AND WAIT.
*
*  SORT i_zwm028 BY ordem ASCENDING.
*
*  LOOP AT i_zwm028.
*    i_zwm028-posicao_ini_pul = pos.
*    MODIFY i_zwm028 INDEX sy-tabix.
*    pos = pos + i_zwm028-total_paletes.
*  ENDLOOP.
*
*  MODIFY zwm028 FROM TABLE i_zwm028.
*  COMMIT WORK AND WAIT.
*
*  REFRESH i_zwm007.
*  CLEAR zwm028.
*  CASE porta_pul.
*    WHEN 'DCK'.
*
*      UPDATE zwm028
*       SET st_dck = porta_pul
*           WHERE lgnum = it311-lgnum AND
*                 refnr = it311-refnr.
*
*
*    WHEN 'PUL'.
*
*      UPDATE zwm028
*       SET st_pul = porta_pul
*           WHERE lgnum = it311-lgnum AND
*                 refnr = it311-refnr.
*
*  ENDCASE.
*  SET  SCREEN '0000'.LEAVE SCREEN.
*  CLEAR porta_pul.
ENDMODULE.                 " USER_COMMAND_0003  INPUT
*&---------------------------------------------------------------------*
*&      Module  help_pulmao  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_pulmao INPUT.

  REFRESH posicoes.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE posicoes
      FROM lagp WHERE lgnum = it311-lgnum AND lgtyp = 'PUL'.


  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
    EXPORTING
*     CUCOL                        = 20
      curow                        = 10
*     DISPLAY                      = ' '
      selectfield                  = 'LGNUM'
      tablename                    = 'ZWM027'
      given_value                  = ' '
*     SHOW_ALL_VALUES_AT_FIRST_TIME       = ' '
      titel                        = 'Posicoes'
    IMPORTING
      ind                          = index
    TABLES
      full_table                   = posicoes
    EXCEPTIONS
      no_tablefields_in_dictionary = 1
      no_tablestructure_given      = 2
      more_then_one_selectfield    = 3
      no_selectfield               = 4
      OTHERS                       = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE posicoes INDEX index.
    IF sy-subrc = 0.
      pulmao = posicoes-lgtyp.
    ENDIF.
  ENDIF.

ENDMODULE.                 " help_pulmao  INPUT
*&---------------------------------------------------------------------*
*&      Module  help_porta_pul  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_porta_pul INPUT.

  REFRESH posicoes.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE posicoes
      FROM lagp WHERE lgnum = it311-lgnum AND
                      ( lgtyp = 'DCK' OR lgtyp = 'PUL' ).


  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
    EXPORTING
*     CUCOL                        = 20
      curow                        = 10
*     DISPLAY                      = ' '
      selectfield                  = 'LGNUM'
      tablename                    = 'ZWM027'
      given_value                  = ' '
*     SHOW_ALL_VALUES_AT_FIRST_TIME       = ' '
      titel                        = 'Posicoes'
    IMPORTING
      ind                          = index
    TABLES
      full_table                   = posicoes
    EXCEPTIONS
      no_tablefields_in_dictionary = 1
      no_tablestructure_given      = 2
      more_then_one_selectfield    = 3
      no_selectfield               = 4
      OTHERS                       = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE posicoes INDEX index.
    IF sy-subrc = 0.
      porta_pul = posicoes-lgtyp.
    ENDIF.
  ENDIF.

ENDMODULE.                 " help_porta_pul  INPUT
*&---------------------------------------------------------------------*
*&      Module  help_pick_pre  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE help_pick_pre INPUT.

  REFRESH posicoes.
  SELECT * APPENDING CORRESPONDING FIELDS OF TABLE posicoes
      FROM lagp WHERE lgnum = it311-lgnum AND lgtyp = 'PPK'.

  CALL FUNCTION 'HELP_VALUES_GET_GIVEN_VALUE'
    EXPORTING
*     CUCOL                        = 20
      curow                        = 10
*     DISPLAY                      = ' '
      selectfield                  = 'LGNUM'
      tablename                    = 'ZWM027'
      given_value                  = ' '
*     SHOW_ALL_VALUES_AT_FIRST_TIME       = ' '
      titel                        = 'Posicoes'
    IMPORTING
      ind                          = index
    TABLES
      full_table                   = posicoes
    EXCEPTIONS
      no_tablefields_in_dictionary = 1
      no_tablestructure_given      = 2
      more_then_one_selectfield    = 3
      no_selectfield               = 4
      OTHERS                       = 5.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    READ TABLE posicoes INDEX index.
    IF sy-subrc = 0.
      pick_pre = posicoes-lgtyp.
    ENDIF.
  ENDIF.


ENDMODULE.                 " help_pick_pre  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_pulmao  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pulmao INPUT.
  CHECK NOT pulmao IS INITIAL.

  IF pulmao <> 'PUL'.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '118' WITH pulmao.
    CLEAR: pulmao.
  ENDIF.
ENDMODULE.                 " check_pulmao  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_pick_pre  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_pick_pre INPUT.

  CLEAR: t_ltak, t_ltap.
  REFRESH: t_ltak, t_ltap.

  CHECK NOT pick_pre IS INITIAL.

  IF pick_pre <> 'PPK'.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '118' WITH  pick_pre.
    CLEAR: pick_pre.
  ENDIF.

** Verficar se tem paletes completas

  SELECT * INTO TABLE t_ltak
      FROM ltak
          WHERE lgnum = it311-lgnum AND
                refnr = it311-refnr AND
                kquit = ' '.

  LOOP AT t_ltak.

    SELECT * APPENDING CORRESPONDING FIELDS OF TABLE t_ltap
        FROM ltap
            WHERE lgnum = t_ltak-lgnum AND
                  tanum = t_ltak-tanum AND
                  vorga <> 'ST' AND
                  pquit = ' ' AND
                  ( vltyp = 'TRI' OR
                    vltyp = 'DRI' OR
                    vltyp = 'BLK' OR
                    vltyp = 'PRM' ).

  ENDLOOP.

  IF t_ltap[] IS INITIAL.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '208' .
    CLEAR: pick_pre.
  ENDIF.

ENDMODULE.                 " check_pick_pre  INPUT
*&---------------------------------------------------------------------*
*&      Module  check_porta_pul  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_porta_pul INPUT.
  CHECK NOT porta_pul IS INITIAL.

  IF porta_pul = 'PUL' OR porta_pul = 'DCK'.

  ELSE.
    MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '118' WITH  porta_pul.
    CLEAR: porta_pul.
  ENDIF.

ENDMODULE.                 " check_porta_pul  INPUT
*&---------------------------------------------------------------------*
*&      Form  delete_lines_zwm026
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM delete_lines_zwm026 .

  DATA: l_lips   LIKE lips OCCURS 0 WITH HEADER LINE,
** RL -> DEL 23.05.2005
** Deixar ficar os registos na tabela ZWM026 de modo
** a garantir o histórico de modificações
*        l_zwm026 LIKE zwm026 OCCURS 0 WITH HEADER LINE,
** RL <- DEL 23.05.2005
        l_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE,
        l_zwm040 LIKE zwm040 OCCURS 0 WITH HEADER LINE.

** RL -> MOD 23.05.2005
** Deixar ficar os registos na tabela ZWM026 de modo
** a garantir o histórico de modificações
*  CLEAR: l_lips, l_zwm026, l_zwm028.
*  REFRESH: l_lips, l_zwm026, l_zwm028.

  DATA: lt_t311a TYPE TABLE OF t311a.

  DATA: ls_t311  TYPE t311,
        ls_t311a TYPE t311a.

  DATA: lv_2step TYPE flag.

  CLEAR: l_lips, l_zwm028, l_zwm040.
  REFRESH: l_lips, l_zwm028, l_zwm040.
** RL <- MOD 23.05.2005

  READ TABLE it311 WITH KEY kreuz = 'X'.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 22.05.2012 15:07:00
*  Motivo: Valida Tipo de Picking
*--------------------------------------------------------------------*
  ls_t311 = it311.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      is_t311 = ls_t311
    IMPORTING
      e_2step = lv_2step
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*  SELECT * INTO CORRESPONDING FIELDS OF TABLE l_lips BYPASSING BUFFER
*      FROM lips AS s INNER JOIN vbss AS p
*           ON s~vbeln = p~vbeln
*           WHERE p~sammg = it311-refnr AND
*               ( s~pstyv <> 'ZPAS' AND
*                 s~pstyv <> 'ZPAL' ).

  SELECT * INTO CORRESPONDING FIELDS OF TABLE l_lips BYPASSING BUFFER
      FROM lips AS s INNER JOIN t311a AS p
           ON s~vbeln = p~rbnum
           WHERE p~lgnum = it311-lgnum AND
                 p~refnr = it311-refnr AND
               ( s~pstyv <> 'ZPAS' AND
                 s~pstyv <> 'ZPAL' ).

  SORT l_lips BY vbeln posnr.

** RL -> DEL 23.05.2005
** Deixar ficar os registos na tabela ZWM026 de modo
** a garantir o histórico de modificações
*  SELECT * INTO TABLE l_zwm026
*      FROM zwm026
*          WHERE armazem = it311-lgnum AND
*                grupo = it311-refnr.
*
*  SORT l_zwm026 BY grupo remessa posnr.
** RL <- DEL 23.05.2005

  SELECT * INTO TABLE l_zwm028
      FROM zwm028
          WHERE lgnum = it311-lgnum AND
                refnr = it311-refnr.

  SORT l_zwm028 BY refnr remessa.

  SELECT * INTO TABLE l_zwm040
      FROM zwm040
          WHERE lgnum = it311-lgnum AND
                refnr = it311-refnr.

  SORT l_zwm040 BY refnr remessa.

** RL -> DEL 23.05.2005
** Deixar ficar os registos na tabela ZWM026 de modo
** a garantir o histórico de modificações
*  LOOP AT l_zwm026.
*    READ TABLE l_lips WITH KEY vbeln = l_zwm026-remessa
*                               posnr = l_zwm026-posnr
*                               matnr = l_zwm026-material.
*    IF sy-subrc <> 0.
*      DELETE FROM zwm026
*          WHERE remessa = l_zwm026-remessa AND
*                posnr = l_zwm026-posnr AND
*                material = l_zwm026-material.
*
*      COMMIT WORK.
*    ENDIF.
*  ENDLOOP.
** RL <- DEL 23.05.2005

  LOOP AT l_zwm028.
    READ TABLE l_zwm040 WITH KEY id_servisan = l_zwm028-remessa.
    IF sy-subrc <> 0.
      READ TABLE l_lips WITH KEY vbeln = l_zwm028-remessa.
      IF sy-subrc <> 0.
        DELETE FROM zwm028
            WHERE refnr = l_zwm028-refnr AND
                  remessa = l_zwm028-remessa.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDLOOP.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 22.05.2012 15:09:25
*  Motivo: Se Picking em 2 passos apaga por grupo
*--------------------------------------------------------------------*
  IF lv_2step EQ 'X'.
    SELECT * FROM t311a
       INTO TABLE lt_t311a
       WHERE lgnum = ls_t311-lgnum AND
             refnr = ls_t311-refnr.

    LOOP AT lt_t311a INTO ls_t311a.
      READ TABLE l_lips WITH KEY vbeln = ls_t311a-rbnum.
      IF sy-subrc <> 0.
        DELETE FROM zwm040
            WHERE refnr = l_zwm040-refnr.
        COMMIT WORK.
      ENDIF.
    ENDLOOP.

  ELSE.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
    LOOP AT l_zwm040.
      READ TABLE l_lips WITH KEY vbeln = l_zwm040-remessa.
      IF sy-subrc <> 0.
        DELETE FROM zwm040
            WHERE refnr = l_zwm040-refnr AND
                  remessa = l_zwm040-remessa.
        COMMIT WORK.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.                    " delete_lines_zwm026
*&---------------------------------------------------------------------*
*&      Form  sammelgang_new
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM sammelgang_new .

  DATA:  cnt_selekt TYPE i.
  CLEAR: cnt_selekt.

  LOOP AT it311 WHERE kreuz = con_x.
*    CHECK it311-kreuz = con_x.
    sav_it311 = it311.                "Merken für Einzel-Starten
    cnt_selekt = cnt_selekt + 1.
  ENDLOOP.

  CASE cnt_selekt.
    WHEN 0.
      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '000'
      WITH 'Para criar Rotas selecionar 1 grupo'(001).
    WHEN 1.
      SUBMIT zwmrep0018 WITH s_grupo IN range_refnr
                        WITH p_back = 'X'
                        AND RETURN.
    WHEN OTHERS.
      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '000'
      WITH 'Para criar Rotas selecionar apenas 1 grupo'(002).
  ENDCASE.

ENDFORM.                    " sammelgang_new
*&---------------------------------------------------------------------*
*&      Form  split_to_dri
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM split_to_dri .
  DATA: valor1     LIKE zwm001-valor,
        um_pal     LIKE marm-meinh,
        l_tanum    LIKE ltap-tanum,
        quantidade TYPE int2,
        resto      TYPE int2,
        n_pal      TYPE i,
        aux_benum  LIKE ltak-benum,
        l_vsola    LIKE ltap-vsola.

  DATA t_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

  DATA: lv_2step TYPE flag.

  CLEAR: t_ltak, t_ltap, c_ltap, return_msg, t_sscc,
         to_prm, to, qtd_total, to_remontada, n_pal.

  REFRESH: t_ltak, t_ltap, c_ltap, return_msg, t_sscc.

  READ TABLE it311 WITH KEY kreuz = 'X'.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 27.06.2012 12:26:50
*  Motivo: Picking em 2 passos
*--------------------------------------------------------------------*
  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum = it311-lgnum
      i_refnr = it311-refnr
    IMPORTING
      e_2step = lv_2step
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

  SELECT * INTO TABLE t_ltak
        FROM ltak
            WHERE lgnum = it311-lgnum AND
                  refnr = it311-refnr.

  CHECK NOT t_ltak[] IS INITIAL.

  SELECT * FROM ltap INTO TABLE t_ltap
      FOR ALL ENTRIES IN t_ltak
            WHERE lgnum = t_ltak-lgnum AND
                  tanum = t_ltak-tanum.

  IF lv_2step EQ abap_false.
    DELETE t_ltap WHERE vorga = 'ST'
                     OR vbeln = ' '
                     OR ( vltyp <> 'DRI' AND vltyp <> 'BLK').
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 27.06.2012 12:29:10
*  Motivo: Picking em 2 passos
*--------------------------------------------------------------------*
  ELSE.
    DELETE t_ltap WHERE vorga = 'ST'
                     OR ( vltyp <> 'DRI' AND vltyp <> 'BLK').
  ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*  DELETE t_ltap WHERE letyp = 'P2'
*                  AND letyp = 'P5'.

  CLEAR: valor1, um_pal, l_tanum.
  SELECT SINGLE valor INTO valor1
      FROM zwm001
          WHERE armazem = it311-lgnum AND
                processo = 'PALETIZACAO' AND
                parametro = 'PALETE'.

  um_pal = valor1.

  LOOP AT t_ltap.
    READ TABLE t_ltak
      WITH KEY tanum = t_ltap-tanum.


    CLEAR: resto, quantidade, marm, t_sscc.
    REFRESH: t_sscc.

    SELECT SINGLE * FROM marm
    WHERE matnr EQ t_ltap-matnr
      AND meinh EQ um_pal.

    quantidade = t_ltap-vsolm DIV marm-umrez.

    resto = t_ltap-vsolm MOD marm-umrez.

    IF quantidade > 1 OR resto IS NOT INITIAL.

** To´s do Drive-in para cancelar
      CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
        EXPORTING
          warehouse     = t_ltap-lgnum
          tanum         = t_ltap-tanum
          tapos         = t_ltap-tapos
        TABLES
          return_msg    = return_msg
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.

** Criar to´s do drive mas com a partição por paletes
        WHILE quantidade > 0.
          CLEAR l_vsola.
          IF t_ltap-letyp = 'P2' OR t_ltap-letyp = 'P5'.
            IF quantidade >= 2.
              l_vsola = marm-umrez * 2.
              quantidade = quantidade - 2.
            ELSE.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 02.07.2012 17:07:35
*  Motivo: Passa Quantidade de Paletezação para ultima Palete
*--------------------------------------------------------------------*
              IF lv_2step EQ abap_true.
                l_vsola = marm-umrez.
              ENDIF.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*
              CLEAR quantidade.
            ENDIF.
          ELSE.
            l_vsola = marm-umrez.
            quantidade = quantidade - 1.
          ENDIF.

          DO 30 TIMES.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 27.06.2012 12:31:22
*  Motivo: Abastecimento de Remessa
*--------------------------------------------------------------------*
            CALL FUNCTION 'ZWM_TO_CREATE_OUT'
              EXPORTING
                warehouse     = lgnum
                refnr         = it311-refnr
                vbeln         = t_ltap-vbeln
                posnr         = t_ltap-posnr
                vsola         = l_vsola
                meins         = t_ltap-altme
                vltyp         = t_ltap-vltyp
                vlpla         = t_ltap-vlpla
                werks         = t_ltap-werks
                lgort         = t_ltap-lgort
                matnr         = t_ltap-matnr
                benum         = t_ltak-benum
              IMPORTING
                to            = to
              TABLES
                return_msg    = return_msg
              EXCEPTIONS
                error_message = 1
                OTHERS        = 2.
*--------------------------------------------------------------------*
* FIM MODIFICAÇÃO - Diogo Silva <<ROFF>>
*--------------------------------------------------------------------*

*            CALL FUNCTION 'ZWM_CREATE_TO_DELIVERY'
*              EXPORTING
*                warehouse     = lgnum
*                refnr         = it311-refnr
*                vbeln         = t_ltap-vbeln
*                posnr         = t_ltap-posnr
*                vsola         = l_vsola
*                vltyp         = t_ltap-vltyp
*                vlpla         = t_ltap-vlpla
**                su            = su
**                su2           = su2
*              IMPORTING
*                to            = to
*              TABLES
*                return_msg    = return_msg
*              EXCEPTIONS
*                error_message = 1
*                OTHERS        = 2.

            IF sy-subrc <> 0.
              CLEAR to.
              WAIT UP TO 1 SECONDS.
            ELSE.

              EXIT.
            ENDIF.

          ENDDO.
        ENDWHILE.

      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " split_to_dri
*&---------------------------------------------------------------------*
*&      Form  confirma_rotas
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM confirma_rotas USING pu_lgnum
                          pu_refnr.

  REFRESH range_refnr.
  CLEAR   range_refnr.

  MOVE: pu_refnr      TO range_refnr-low,
        con_sign_i    TO range_refnr-sign,
        con_option_eq TO range_refnr-option.
  APPEND range_refnr.

** Verificar se ja executou a rota de picking para o grupo
  SELECT SINGLE * FROM zwm026
          WHERE armazem = pu_lgnum AND
                  grupo = pu_refnr.

  IF sy-subrc <> 0.
    EXIT.

  ELSE.
    IF zwm026-num_recorrido <> '                    '.
      MESSAGE ID 'ZWMMSG001' TYPE 'I' NUMBER '116'
            WITH pu_refnr.
      EXIT.
    ENDIF.
  ENDIF.

  SUBMIT zwmrep0018 WITH s_grupo IN range_refnr
                    WITH p_back = 'X'
                    AND RETURN.

ENDFORM.                    " confirma_rotas
*&---------------------------------------------------------------------*
*&      Form  ACTUALIZA_ZWM028
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM actualiza_zwm028 .

  DATA: i_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE,
        i_lagp   LIKE lagp   OCCURS 0 WITH HEADER LINE,
        t_zwm028 LIKE zwm028 OCCURS 0 WITH HEADER LINE..

  DATA: num_paletes(2),
        pos            TYPE i,
        aux_pos.

  CALL FUNCTION 'ZWM_ACTUALIZA_PAL_GRUPO'
    EXPORTING
      lgnum                = it311-lgnum
      refnr                = it311-refnr
    EXCEPTIONS
      actualizacao_zwm0028 = 1
      OTHERS               = 2.

** Actualizar o total de paletes a nivel do grupo
  CLEAR t_zwm028.
  REFRESH t_zwm028.

  SELECT * INTO TABLE t_zwm028
      FROM zwm028
          WHERE refnr = it311-refnr.

  CLEAR num_paletes.

  LOOP AT t_zwm028.
    CLEAR zwm028.
    SELECT SINGLE *
      FROM zwm028
          WHERE lgnum = t_zwm028-lgnum AND
                refnr = t_zwm028-refnr AND
                remessa = t_zwm028-remessa.
    IF sy-subrc = 0.
      num_paletes = num_paletes + zwm028-total_paletes.
    ENDIF.
  ENDLOOP.

  CLEAR i_zwm028.
  REFRESH i_zwm028.

  SELECT * INTO TABLE i_zwm028
      FROM zwm028
          WHERE lgnum = it311-lgnum AND
                refnr = it311-refnr.

  READ TABLE i_zwm028 INDEX 1.
  CLEAR i_zwm028-remessa.
  CLEAR i_zwm028-ordem.
  i_zwm028-refnr = it311-refnr.
  i_zwm028-total_paletes = num_paletes.

  CLEAR: i_zwm028-servisan, i_zwm028-emissor.

  INSERT INTO zwm028 VALUES i_zwm028.
  IF sy-subrc = 0.
    COMMIT WORK AND WAIT.
  ELSE.
*    ROLLBACK WORK.
*    ok_code_0001 = 'CANCE'.
*    CLEAR: pulmao, ok_code_0001.
*    SET  SCREEN '0000'.LEAVE SCREEN.
  ENDIF.

  WHILE 1 = 1.
    SELECT SINGLE *
        FROM zwm028
            WHERE lgnum = it311-lgnum AND
                  refnr = it311-refnr AND
                  remessa = ' '.
    IF sy-subrc = 0.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDWHILE.

  CLEAR aux_pos.
  pos = 1.

  DELETE i_zwm028 WHERE remessa IS INITIAL.

  SORT i_zwm028 BY ordem ASCENDING.

  LOOP AT i_zwm028.

    i_zwm028-posicao_ini_pul = pos.

    MODIFY i_zwm028 INDEX sy-tabix.
    pos = pos + i_zwm028-total_paletes.

  ENDLOOP.

  MODIFY zwm028 FROM TABLE i_zwm028.
  COMMIT WORK AND WAIT.

ENDFORM.                    " ACTUALIZA_ZWM028
*&---------------------------------------------------------------------*
*&      Form  SPLIT_PALETES_REMONTADAS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM split_paletes_remontadas .

  DATA: um_pal    LIKE marm-meinh,
        aux_benum LIKE ltak-benum,
        aux_betyp TYPE lvs_betyp,
        pal_p1    LIKE zwm020-p1,
        pal_p2    LIKE zwm020-p2.

  DATA: ls_zwm047     TYPE zwm047.
  DATA: lt_ltap_cancl LIKE ltap_cancl OCCURS 0 WITH HEADER LINE.
  DATA: lt_ltap       LIKE ltap       OCCURS 0 WITH HEADER LINE.

  DATA: flag_nao_estornar.
  DATA: flag_add_ot.

  DATA: lv_2spart TYPE flag.

  CLEAR: um_pal, aux_benum, pal_p1, pal_p2.

** Estornar as duas paletes
  LOOP AT c_ltap.

    CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
      EXPORTING
        warehouse     = c_ltap-lgnum
        tanum         = c_ltap-tanum
        tapos         = c_ltap-tapos
      TABLES
        return_msg    = return_msg
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDLOOP.

  WAIT UP TO 1 SECONDS.
** Criar para a remessa a ot com a palete P1
  READ TABLE c_ltap INDEX 1.
  READ TABLE t_ltak WITH KEY tanum = c_ltap-tanum.

  CLEAR flag.
  DO 60 TIMES. "WHILE flag IS INITIAL.
    CALL FUNCTION 'ZWM_TO_CREATE_OUT'
      EXPORTING
        warehouse     = c_ltap-lgnum
        refnr         = t_ltak-refnr
        vbeln         = t_ltak-vbeln
        posnr         = c_ltap-posnr
        vsola         = c_ltap-vsola
        meins         = c_ltap-altme
        su            = zwm020-p1
        werks         = c_ltap-werks
        lgort         = c_ltap-lgort
        matnr         = c_ltap-matnr
      IMPORTING
        to            = to
      TABLES
        return_msg    = return_msg
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

    IF sy-subrc EQ 0.
      flag = 'X'.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
      CLEAR flag.
    ENDIF.
  ENDDO. "ENDWHILE.

** Criar para o PRM a ot com a palete P2
  t_sscc-sscc = zwm020-p2.
  t_sscc-tipo_su = c_ltap-letyp.
  t_sscc-material = c_ltap-matnr.
  t_sscc-quantidade = 1.
  t_sscc-uni = um_pal.
  t_sscc-lote_producao = c_ltap-charg.
  APPEND t_sscc.
  CLEAR t_sscc.

  WAIT UP TO 1 SECONDS.

  CLEAR: aux_benum, aux_betyp.
  aux_benum = it311-refnr.
  aux_betyp = 'I'.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum  = c_ltap-lgnum
      i_refnr  = it311-refnr
    IMPORTING
      e_2spart = lv_2spart
    EXCEPTIONS
      error    = 1
      OTHERS   = 2.

  IF lv_2spart EQ abap_true.
    aux_benum = t_ltak-vbeln.
  ENDIF.


  CLEAR flag.

  DO 60 TIMES.
    CALL FUNCTION 'ZWM_CREATE_TO'
      EXPORTING
        warehouse  = c_ltap-lgnum
        mov_type   = '940'
        material   = c_ltap-matnr
        quantity   = '1'
        unit       = 'PAL'
        plant      = c_ltap-werks
        s_loc      = c_ltap-lgort
        lote       = c_ltap-charg
        source_sty = c_ltap-vltyp
        source_bin = c_ltap-vlpla
        req_type   = aux_betyp
        req_number = aux_benum
        su         = zwm020-p2
      IMPORTING
        to         = to_remontada
      TABLES
        return_msg = return_msg
      EXCEPTIONS
        error      = 1
        OTHERS     = 2.

    IF sy-subrc = 0.
      flag = 'X'.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
      CLEAR flag.

      PERFORM complete_log_message USING 'REMONTADA_004'
                                         aux_benum
                                         zwm020-p1
                                         zwm020-p2
                                         c_ltap-matnr
                                   CHANGING return_msg[].

      CALL FUNCTION 'ZWM_LOG_MESSAGE'
        EXPORTING
          i_master    = c_ltap-lgnum
          i_object    = 'ZWM001'
          i_subobject = 'ZWM006'
          i_extnumber = '4'
          i_commit    = 'X'
          it_messages = return_msg[].
    ENDIF.
  ENDDO.

** Saber se não faz sentido remover da tabela zwm020,
** Visto que estas paletes já não estão juntas ????
*  DELETE FROM zwm020 WHERE armazem = c_ltap-lgnum AND
*                           p1      = zwm020-p1    AND
*                           p2      = zwm020-p2.
*  IF sy-subrc = 0.
*    COMMIT WORK AND WAIT.
*  ENDIF.

  WAIT UP TO 1 SECONDS.

** Criar para a segunda delivery a ot segundo estratégia e
** verificar a palete que foi determinada
  READ TABLE c_ltap INDEX 2.
  READ TABLE t_ltak WITH KEY tanum = c_ltap-tanum.

  CLEAR: flag, to.
  DO 60 TIMES.
    CLEAR return_msg. REFRESH return_msg.

    CALL FUNCTION 'ZWM_TO_CREATE_OUT'
      EXPORTING
        warehouse     = c_ltap-lgnum
        refnr         = t_ltak-refnr
        vbeln         = t_ltak-vbeln
        posnr         = c_ltap-posnr
        vsola         = c_ltap-vsola
        meins         = c_ltap-altme
        werks         = c_ltap-werks
        lgort         = c_ltap-lgort
        matnr         = c_ltap-matnr
      IMPORTING
        to            = to
      TABLES
        return_msg    = return_msg
      EXCEPTIONS
        error_message = 1
        OTHERS        = 2.

    IF sy-subrc EQ 0.
      flag = 'X'.
      EXIT.
    ELSE.
      WAIT UP TO 1 SECONDS.
    ENDIF.
  ENDDO.
**********************************************************************
** RS @ 26-01-2011 - Não existe mais stock, é única palete disponível
**********************************************************************
  IF to IS INITIAL.

    READ TABLE return_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      IF return_msg-msgid = 'L3' AND return_msg-msgnr = '008'.

*       Estornar OT que foi criada para PRM
        IF to_remontada IS NOT INITIAL.
          SELECT *
            FROM ltap INTO TABLE lt_ltap
            WHERE lgnum = c_ltap-lgnum AND
                  tanum = to_remontada.

          LOOP AT lt_ltap.
            CLEAR lt_ltap_cancl.
            lt_ltap_cancl-tanum = lt_ltap-tanum.
            lt_ltap_cancl-tapos = lt_ltap-tapos.
            APPEND lt_ltap_cancl.
          ENDLOOP.

          CALL FUNCTION 'L_TO_CANCEL_SU'
            EXPORTING
              i_lenum       = zwm020-p2
            TABLES
              t_ltap_cancl  = lt_ltap_cancl
            EXCEPTIONS
              error_message = 99.

*         Criar uma nova OT, para outra remessa
          IF sy-subrc = 0.
            CLEAR to.
            DO 10 TIMES.
              CLEAR return_msg. REFRESH return_msg.

              CALL FUNCTION 'ZWM_TO_CREATE_OUT'
                EXPORTING
                  warehouse     = c_ltap-lgnum
                  refnr         = t_ltak-refnr
                  vbeln         = c_ltap-vbeln
                  posnr         = c_ltap-posnr
                  vsola         = c_ltap-vsola
                  meins         = c_ltap-altme
                  werks         = c_ltap-werks
                  lgort         = c_ltap-lgort
                  matnr         = c_ltap-matnr
                  su            = zwm020-p2
                IMPORTING
                  to            = to
                TABLES
                  return_msg    = return_msg
                EXCEPTIONS
                  error_message = 1
                  OTHERS        = 2.



              IF sy-subrc EQ 0.
                flag = 'X'.
                EXIT.
              ELSE.
                WAIT UP TO 1 SECONDS.
              ENDIF.
            ENDDO.
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

*    CLEAR ls_zwm047.
*    ls_zwm047-vbeln = c_ltap-vbeln.
*    ls_zwm047-posnr = c_ltap-posnr.
*    ls_zwm047-tanum = c_ltap-tanum.
*    ls_zwm047-tapos = c_ltap-tapos.
*    ls_zwm047-vsola = c_ltap-vsola.
*
*    MODIFY zwm047 FROM ls_zwm047.
*    IF sy-subrc = 0.
*      COMMIT WORK AND WAIT.
*    ENDIF.
**********************************************************************
  ELSE.

** Se for a palete P1 criar a outra ot para o PRM com a P2
    CLEAR ltap.
    SELECT SINGLE *
        FROM ltap
            WHERE lgnum = c_ltap-lgnum
              AND tanum = to.

    CLEAR zwm020.
    SELECT SINGLE p1 p2 INTO (pal_p1, pal_p2)
        FROM zwm020
            WHERE armazem = c_ltap-lgnum
              AND ( p1 = ltap-vlenr OR
                    p2 = ltap-vlenr ).

    IF sy-subrc = 0.

**    Validar se outra palete remontada tem já uma OT para o grupo
      CLEAR flag_nao_estornar.
      IF ltap-vlenr = pal_p1.
        READ TABLE t_ltap WITH KEY vlenr = pal_p2.
        IF sy-subrc = 0.
          flag_nao_estornar = 'X'.
          flag_add_ot = 'X'.
        ENDIF.

      ELSEIF ltap-vlenr = pal_p2.
        READ TABLE t_ltap WITH KEY vlenr = pal_p1.
        IF sy-subrc = 0.
          flag_nao_estornar = 'X'.
          flag_add_ot = 'X'.
        ENDIF.
      ENDIF.

      IF flag_nao_estornar IS INITIAL." Palete já tem OT para o grupo

        WAIT UP TO 1 SECONDS.

** CC estornar a ot com a P2 e criar para a remessa com a P1
        IF ltap-vlenr = pal_p2.
          CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
            EXPORTING
              warehouse     = ltap-lgnum
              tanum         = ltap-tanum
              tapos         = ltap-tapos
            TABLES
              return_msg    = return_msg
            EXCEPTIONS
              error_message = 1
              OTHERS        = 2.
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          CLEAR: flag, to.
          DO 60 TIMES.
            CALL FUNCTION 'ZWM_TO_CREATE_OUT'
              EXPORTING
                warehouse     = c_ltap-lgnum
                refnr         = t_ltak-refnr
                vbeln         = t_ltak-vbeln
                posnr         = c_ltap-posnr
                vsola         = c_ltap-vsola
                meins         = c_ltap-altme
                werks         = c_ltap-werks
                lgort         = c_ltap-lgort
                matnr         = c_ltap-matnr
                su            = pal_p1
              IMPORTING
                to            = to
              TABLES
                return_msg    = return_msg
              EXCEPTIONS
                error_message = 1
                OTHERS        = 2.


            IF sy-subrc EQ 0..
              flag = 'X'.
              EXIT.
            ELSE.
              CLEAR flag.
              WAIT UP TO 1 SECONDS.
            ENDIF.
          ENDDO.

        ENDIF.

        WAIT UP TO 1 SECONDS.

**        CLEAR aux_benum.
**        aux_benum = it311-refnr.
        CLEAR flag.
        DO 60 TIMES. "WHILE flag IS INITIAL.

          CALL FUNCTION 'ZWM_CREATE_TO'
            EXPORTING
              warehouse  = ltap-lgnum
              mov_type   = '940'
              material   = ltap-matnr
              quantity   = '1'
              unit       = 'PAL'
              plant      = ltap-werks
              s_loc      = ltap-lgort
              lote       = ltap-charg
              source_sty = ltap-vltyp
              source_bin = ltap-vlpla
              req_type   = aux_betyp
              req_number = aux_benum
              su         = pal_p2
            IMPORTING
              to         = to_remontada
            TABLES
              return_msg = return_msg
            EXCEPTIONS
              error      = 1
              OTHERS     = 2.

          IF sy-subrc = 0.
            flag = 'X'.
            EXIT.
          ELSE.
            WAIT UP TO 1 SECONDS.
            CLEAR flag.

            PERFORM complete_log_message USING 'REMONTADA_005'
                                               aux_benum
                                               pal_p1
                                               pal_p2
                                               ltap-matnr
                                         CHANGING return_msg[].

            CALL FUNCTION 'ZWM_LOG_MESSAGE'
              EXPORTING
                i_master    = ltap-lgnum
                i_object    = 'ZWM001'
                i_subobject = 'ZWM006'
                i_extnumber = '3'
                i_commit    = 'X'
                it_messages = return_msg[].
          ENDIF.
        ENDDO. "ENDWHILE.

      ENDIF.
    ENDIF.
  ENDIF.

  LOOP AT c_ltap.
    DELETE t_ltap WHERE vlenr = c_ltap-vlenr.
  ENDLOOP.

  IF flag_add_ot = 'X'.
    APPEND ltap TO t_ltap.
  ENDIF.

  CLEAR c_ltap.
  REFRESH c_ltap.

ENDFORM.                    " SPLIT_PALETES_REMONTADAS
*&---------------------------------------------------------------------*
*&      Form  CHECK_WHS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_whs .

  DATA: BEGIN OF l_user OCCURS 0.
          INCLUDE STRUCTURE lrf_wkqu.
        DATA: END OF l_user.

  CALL FUNCTION 'L_USER_DATA_GET'
    EXPORTING
      i_uname        = sy-uname
    TABLES
      t_xuser        = l_user
    EXCEPTIONS
      no_entry_found = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
*   ERRO: Utilizador não tem armazem atribuído!
    MESSAGE ID 'ZWMMPMSG' TYPE 'I' NUMBER '001'.
    gv_out = 'X'.
  ELSE.
    READ TABLE l_user WITH KEY statu = 'X'. "Util. Atrib. Arm.
    IF sy-subrc <> 0 OR
       l_user-lgnum <> '100' AND
       l_user-lgnum NE '150'. " >> INS ROFF(SDF):TMGP:16.12.2015 15:54:24
      MESSAGE ID 'ZWMMPMSG' TYPE 'I' NUMBER '077' WITH l_user-lgnum.
      gv_out = 'X'.
    ENDIF.
  ENDIF.

*  IF l_user-lgnum = '100' AND sy-uname <> 'GGUTERRES'.
*    MESSAGE i000 WITH 'Transação ZWM025 obsoleta, usar nova transação ZWM031B'.
*    gv_out = 'X'.
*  ENDIF.

ENDFORM.                    " CHECK_WHS
*&---------------------------------------------------------------------*
*&      Form  VERIFY_PICKING_PKL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM verify_picking_pkl .

ENDFORM.                    " VERIFY_PICKING_PKL
*&---------------------------------------------------------------------*
*&      Form  UPDATE_PICKING
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM update_picking CHANGING cv_subrc TYPE sysubrc.

  DATA: lt_lips LIKE lips OCCURS 0 WITH HEADER LINE,
        lt_vbss LIKE vbss OCCURS 0 WITH HEADER LINE.

  DATA: ls_lips           TYPE lips,
        ls_zwm001         TYPE zwm001,
        ls_unpaired_stock TYPE lqua.

  DATA: lt_unpaired_stock TYPE TABLE OF lqua.

  CLEAR: cv_subrc.

  DATA l_qtd(17).
  LOOP AT it311 WHERE kreuz = con_x.
    EXIT.
  ENDLOOP.

  CHECK sy-subrc = 0.

  SELECT * FROM vbss INTO TABLE lt_vbss
                WHERE sammg = it311-sammg.

  CHECK NOT lt_vbss[] IS INITIAL.

  SELECT * FROM lips INTO TABLE lt_lips
              FOR ALL ENTRIES IN lt_vbss
                WHERE vbeln = lt_vbss-vbeln.

** Valida Remontadas
***********************************************************************
  SELECT SINGLE * FROM zwm001
                  INTO ls_zwm001
                  WHERE armazem = lgnum AND
                        processo = 'REMONTADA' AND
                        parametro = 'CHECK'.

  IF ls_zwm001-valor EQ abap_true.

    LOOP AT lt_lips INTO ls_lips.
      CALL FUNCTION 'ZWM_CHECK_REMONTADAS'
        EXPORTING
          i_lgnum           = lgnum
          i_matnr           = ls_lips-matnr
        IMPORTING
          et_unpaired_stock = lt_unpaired_stock.

      LOOP AT lt_unpaired_stock INTO ls_unpaired_stock.
        MESSAGE ID zwm001
              TYPE 'I'
              NUMBER 079
              DISPLAY LIKE 'E'
              WITH ls_unpaired_stock-lenum.
      ENDLOOP.
      IF sy-subrc EQ 0.
        cv_subrc = 4.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDIF.


**********************************************************************

* Eliminar itens que sejam do CD

*  DELETE lt_lips WHERE lgort = 'CD'.
  CALL FUNCTION 'Z_WM_FILTER_TABLE_TO_WM'
    EXPORTING
      i_lgnum  = lgnum
    CHANGING
      ct_table = lt_lips[].

  CHECK lt_lips[] IS NOT INITIAL.


  LOOP AT lt_lips.

    CALL FUNCTION 'ZWM_UPDATE_PICKING'
      EXPORTING
        i_lfimg = lt_lips-lfimg
        i_vbeln = lt_lips-vbeln
        i_posnr = lt_lips-posnr.

  ENDLOOP.
ENDFORM.                    " UPDATE_PICKING

*&---------------------------------------------------------------------*
*&      Form  corrige_to_aut
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM corrige_to_aut .
  DATA: valor1     LIKE zwm001-valor,
        um_pal     LIKE marm-meinh,
        l_tanum    LIKE ltap-tanum,
        quantidade TYPE int2,
        resto      TYPE int2,
        n_pal      TYPE i,
        aux_benum  LIKE ltak-benum,
        l_vsola    LIKE ltap-vsola.

  DATA t_sscc LIKE zwm_sscc OCCURS 0 WITH HEADER LINE.

  DATA: lv_2step TYPE flag.

  CLEAR: t_ltak, t_ltap, c_ltap, return_msg, t_sscc,
         to_prm, to, qtd_total, to_remontada, n_pal.

  REFRESH: t_ltak, t_ltap, c_ltap, return_msg, t_sscc.

  READ TABLE it311 WITH KEY kreuz = 'X'.

  CHECK sy-subrc = 0.

  CALL FUNCTION 'ZWM_DEBUG_ONLINE'.

  CALL FUNCTION 'ZWM_GROUP_CHECK_2STEP'
    EXPORTING
      i_lgnum = it311-lgnum
      i_refnr = it311-refnr
    IMPORTING
      e_2step = lv_2step
    EXCEPTIONS
      error   = 1
      OTHERS  = 2.

  SELECT * INTO TABLE t_ltak
        FROM ltak
            WHERE lgnum = it311-lgnum AND
                  refnr = it311-refnr.

  CHECK NOT t_ltak[] IS INITIAL.

  SELECT * FROM ltap INTO TABLE t_ltap
      FOR ALL ENTRIES IN t_ltak
            WHERE lgnum = t_ltak-lgnum AND
                  tanum = t_ltak-tanum.

  IF lv_2step EQ abap_false.
    DELETE t_ltap WHERE vorga = 'ST'
                     OR vbeln = ' '
                     OR vltyp <> 'AUT'.

  ELSE.
    DELETE t_ltap WHERE vorga = 'ST'
                     OR vltyp <> 'AUT'.
  ENDIF.

  CLEAR: valor1, um_pal, l_tanum.
  SELECT SINGLE valor INTO valor1
      FROM zwm001
          WHERE armazem = it311-lgnum AND
                processo = 'PALETIZACAO' AND
                parametro = 'PALETE'.

  um_pal = valor1.

  LOOP AT t_ltap.
    READ TABLE t_ltak
      WITH KEY tanum = t_ltap-tanum.

    CLEAR: resto, quantidade, marm, t_sscc.
    REFRESH: t_sscc.

    SELECT SINGLE * FROM marm
    WHERE matnr EQ t_ltap-matnr
      AND meinh EQ um_pal.

    quantidade = t_ltap-vsolm DIV marm-umrez.

    resto = t_ltap-vsolm MOD marm-umrez.

    IF quantidade > 1 OR resto IS NOT INITIAL.

      CALL FUNCTION 'ZWM_CANCEL_TO_ITEM_DELIVERY'
        EXPORTING
          warehouse     = t_ltap-lgnum
          tanum         = t_ltap-tanum
          tapos         = t_ltap-tapos
        TABLES
          return_msg    = return_msg
        EXCEPTIONS
          error_message = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.

        l_vsola = marm-umrez.

        DO 30 TIMES.
*--------------------------------------------------------------------*
*  INICIO MODIFICAÇÃO - Diogo Silva <<ROFF>>
*  Data: 27.06.2012 12:31:22
*  Motivo: Abastecimento de Remessa
*--------------------------------------------------------------------*
          CALL FUNCTION 'ZWM_TO_CREATE_OUT'
            EXPORTING
              warehouse     = lgnum
              refnr         = it311-refnr
              vbeln         = t_ltap-vbeln
              posnr         = t_ltap-posnr
              vsola         = l_vsola
              meins         = t_ltap-altme
              vltyp         = t_ltap-vltyp
              vlpla         = t_ltap-vlpla
              werks         = t_ltap-werks
              lgort         = t_ltap-lgort
              matnr         = t_ltap-matnr
              benum         = t_ltak-benum
            IMPORTING
              to            = to
            TABLES
              return_msg    = return_msg
            EXCEPTIONS
              error_message = 1
              OTHERS        = 2.

          IF sy-subrc <> 0.
            CLEAR to.
            WAIT UP TO 1 SECONDS.
          ELSE.
            EXIT.
          ENDIF.

        ENDDO.

      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " corrige_to_aut
*&---------------------------------------------------------------------*
*&      Form  GET_PLANT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_L_WERKS  text
*      <--P_L_LGORT  text
*----------------------------------------------------------------------*
FORM get_plant_data  USING u_refnr TYPE lvs_refnr
                     CHANGING c_werks TYPE werks_d
                              c_lgort TYPE lgort_d.

  CALL FUNCTION 'Z_WM_WAREHOUSE_COMPLETE_INFO'
    EXPORTING
      i_user      = sy-uname
      i_refnr     = u_refnr
      i_recall    = 'X'
      i_usewm     = 'X'
      i_usemm     = 'X'
      i_useaut    = 'X'
      i_get_lgnum = 'X'
      i_get_werks = 'X'
      i_get_lgort = 'X'
**   IMPORTING
**     ET_MESSAGES         =
    CHANGING
      c_lgnum     = lgnum
      c_werks     = c_werks
      c_lgort     = c_lgort
    EXCEPTIONS
      error       = 1
      user_back   = 2
      OTHERS      = 3.


ENDFORM.                    " GET_PLANT_DATA

FORM complete_log_message  USING uv_log_zone
                                 uv_ref
                                 uv_lenum_v1
                                 uv_lenum_v2
                                 uv_matnr
                           CHANGING ct_return_msg TYPE tab_bdcmsgcoll.

  DATA: ls_return TYPE bdcmsgcoll.

  IF NOT uv_matnr IS INITIAL.
    ls_return-msgid = 'ZWM001'.
    ls_return-msgnr = '000'.
    ls_return-msgtyp = 'I'.
    ls_return-msgv1 = 'Material'.
    ls_return-msgv2 = uv_matnr.
    INSERT ls_return INTO ct_return_msg INDEX 1.
  ENDIF.

  IF NOT uv_lenum_v2 IS INITIAL.
    ls_return-msgid = 'ZWM001'.
    ls_return-msgnr = '000'.
    ls_return-msgtyp = 'I'.
    ls_return-msgv1 = 'Palete 2'.
    ls_return-msgv2 = uv_lenum_v2.
    INSERT ls_return INTO ct_return_msg INDEX 1.
  ENDIF.

  IF NOT uv_lenum_v1 IS INITIAL.
    ls_return-msgid = 'ZWM001'.
    ls_return-msgnr = '000'.
    ls_return-msgtyp = 'I'.
    ls_return-msgv1 = 'Palete 1'.
    ls_return-msgv2 = uv_lenum_v1.
    INSERT ls_return INTO ct_return_msg INDEX 1.
  ENDIF.

  IF NOT uv_ref IS INITIAL.
    ls_return-msgid = 'ZWM001'.
    ls_return-msgnr = '000'.
    ls_return-msgtyp = 'I'.
    ls_return-msgv1 = 'Referencia'.
    ls_return-msgv2 = uv_ref.
    INSERT ls_return INTO ct_return_msg INDEX 1.
  ENDIF.

  IF NOT uv_log_zone IS INITIAL.
    ls_return-msgid = 'ZWM001'.
    ls_return-msgnr = '000'.
    ls_return-msgtyp = 'I'.
    ls_return-msgv1 = 'Zona de Log'.
    ls_return-msgv2 = uv_log_zone.
    INSERT ls_return INTO ct_return_msg INDEX 1.
  ENDIF.


ENDFORM.
