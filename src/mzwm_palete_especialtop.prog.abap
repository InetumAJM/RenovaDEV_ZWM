************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: MZWM_PALETE_ESPECIALTOP                                  *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Manutenção da Tabela de Paletização Especial (TOP)       *
* Criado por: Luís Rocha                                               *
* Criado em.: 06/12/2004                                               *
* Tipo PRG..: Include                                                  *
************************************************************************

PROGRAM sapmzwm_palete_especial
        MESSAGE-ID zv NO STANDARD PAGE HEADING LINE-SIZE 80 .

TABLES: mara,
        makt,
        kna1,
        t001w,
        t300,
        t300t,
        zwm031,
       *zwm031,
        zwm_scr,
        bhdgd,
        zwm049 .

CONTROLS ctrl_tab TYPE TABLEVIEW USING SCREEN 200.

DATA: tab_zwm031  LIKE zwm031  OCCURS 10 WITH HEADER LINE.

DATA: BEGIN OF w_tab_0100 OCCURS 100,
          kunnr     LIKE zwm031-kunnr,
          matnr     LIKE zwm031-matnr,
          niveis    LIKE zwm031-niveis,
          lastro    LIKE zwm031-lastro,
          unporpal  LIKE zwm031-unporpal,
          hoehe     LIKE zwm031-hoehe,
          meabm     LIKE zwm031-meabm,
          remontada LIKE zwm031-remontada,
          name1     LIKE kna1-name1,
          maktx     LIKE makt-maktx,
          flg_del,
          flg_ins,
          flg_mod,
          flg_exist,
      END OF w_tab_0100,
      w_tab_0200 LIKE w_tab_0100 OCCURS 100 WITH HEADER LINE.

DATA: BEGIN OF w_key,
          kunnr     LIKE zwm031-kunnr,
          matnr     LIKE zwm031-matnr,
      END OF w_key.

DATA: w_retorno  LIKE sy-subrc,
      w_subrc    LIKE sy-subrc.

DATA: w_zwm031_lock.
data: wa_zwm049 type zwm049.
data: SCR200_PALTXT(80).
data: scr200_rfshow TYPE flag.
