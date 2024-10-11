*&---------------------------------------------------------------------*
*&  Include           LZWMFUNC12D01
*&---------------------------------------------------------------------*

** Waves de Picking (REVER)
***********************************************************************

INCLUDE zwm_constants.

*-----------------------------------------------------------------------
*      Tabellen
*-----------------------------------------------------------------------
DATA: gv_lgnum TYPE lgnum,
      gv_offta TYPE flag.


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

DATA : confirmado VALUE '0'.
DATA  pulmao LIKE lagp-lgtyp.
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
