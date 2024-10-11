*&---------------------------------------------------------------------*
*& Include Z02RP_RP_REPDITA_TOP                                        *
*&                                                                     *
*&---------------------------------------------------------------------*

REPORT   z02rp_rp_repdita NO STANDARD PAGE HEADING LINE-SIZE 121.

CONSTANTS: col TYPE i VALUE 121  .
*CONSTANTS: COL TYPE I VALUE 104.

TABLES: z02rpreqpapel,
        z02rpconsprod,
        *z02rpconsprod,
        z02rpconsprodl,
        z02rpconsprodh,
        z02rpconslt,
        z02rptmarchale,
        z02rpsessao,
        z02rpsessaodita,
        lein, lqua, afru,
        resb, afko, mara.

* INETUM - NR - 03.10.2023 - RENPRJ00041 - Inicio
DATA xsessaodita LIKE z02rpsessaodita OCCURS 0 WITH HEADER LINE.

***TYPES: BEGIN OF ty_xsessaodita.
***         INCLUDE STRUCTURE z02rpsessaodita.
***         TYPES: tempos_par_sem_prod_flg TYPE flag,
***         sessao                  TYPE z02rpsessao-sessao,
***       END OF ty_xsessaodita.
***
***DATA xsessaodita TYPE ty_xsessaodita OCCURS 0 WITH HEADER LINE.
***
***DATA: xsessaodita_sem_prod_aux TYPE STANDARD TABLE OF ty_xsessaodita.
* INETUM - NR - 03.10.2023 - RENPRJ00041 - Fim

* Roff - NR - 16.09.2019 - Inicio
***DATA: ls_xsessaodita_aux TYPE z02rpsessaodita.
* Roff - NR - 16.09.2019 - Fim

DATA: xreqpapel LIKE z02rpreqpapel OCCURS 0 WITH HEADER LINE.
DATA: BEGIN OF xconsprod OCCURS 0.
        INCLUDE STRUCTURE z02rpconsprodl.
        DATA:   mblnr_p LIKE z02rpconsprod-mblnr,
        mjahr_p LIKE z02rpconsprod-mjahr,
        prodp   LIKE z02rpconsprod-prodp,
        meins_p LIKE z02rpconsprod-meins_p,
        codeb   LIKE z02rpconsprod-codeb,
      END OF xconsprod.
DATA xconsprodh LIKE z02rpconsprodh OCCURS 0 WITH HEADER LINE.
DATA xconslt LIKE z02rpconslt OCCURS 0 WITH HEADER LINE.
DATA xtempos LIKE z02rptmarchale OCCURS 0 WITH HEADER LINE.

DATA: BEGIN OF xbobinas_i OCCURS 0,
        lenum LIKE lqua-lenum,
        lgpla LIKE lqua-lgpla,
      END OF xbobinas_i.

DATA: consprod        LIKE z02rpconsprod-consprod,
      divisao         LIKE z02rpconsprod-divisao,
      area            LIKE z02rpsessao-area,
      linha           LIKE z02rpsessao-linha,
* INETUM - NR - 04.10.2023 - RENPRJ00041 - Inicio
***      aufnr           TYPE z02rpsessao-aufnr1,
***      sessao_sem_prod TYPE z02rpsessao-sessao,
* INETUM - NR - 04.10.2023 - RENPRJ00041 - Fim
      erro.

DATA operacao LIKE z02rplogoper-operacao.
DATA okcode LIKE sy-ucomm.

* BEGIN ROFF JCO - 09.04.2019 - RENPRM00002
TYPES: BEGIN OF ty_s_charg_alt,
         aufnr TYPE z02rpconslt-aufnr,
         matnr TYPE z02rpconslt-aufnr,
         maktx TYPE maktx,
         charg TYPE z02rpconslt-charg,
         menge TYPE z02rpconslt-menge,
         meins TYPE z02rpconslt-meins,
       END OF ty_s_charg_alt.

TYPES:
  BEGIN OF ty_excpt,
    divisao TYPE z02divisao,
    werks	  TYPE werks_d,
    area    TYPE co_dispo,
    linha	  TYPE fevor,
    consprd TYPE flag,
    reqs    TYPE flag,
  END OF ty_excpt.

DATA:
  gt_charg_alt TYPE STANDARD TABLE OF ty_s_charg_alt,
  gt_excpt     TYPE STANDARD TABLE OF ty_excpt.
* END ROFF JCO - 09.04.2019 - RENPRM00002

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-z01.
* Roff - NR - 30.09.2019 - Inicio
***SELECT-OPTIONS: so_werks FOR z02rpsessao-werks,
SELECT-OPTIONS: so_werks FOR z02rpsessao-werks DEFAULT 'RENV',
* Roff - NR - 30.09.2019 - Fim
                so_area FOR z02rpsessao-area,
                so_linha FOR z02rpsessao-linha,
                so_data FOR z02rpsessao-data,
                so_turno FOR z02rpsessao-turno.
SELECTION-SCREEN END   OF BLOCK b1.

*JCaetano - 5/7/2005
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-050.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: p_ger RADIOBUTTON GROUP rd1 DEFAULT 'X'.
SELECTION-SCREEN COMMENT (15) text-051.
PARAMETERS: p_ok  RADIOBUTTON GROUP rd1.
SELECTION-SCREEN COMMENT (18) text-052.
PARAMETERS: p_erro RADIOBUTTON GROUP rd1.
SELECTION-SCREEN COMMENT (15) text-053.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b3.
*

INCLUDE <icon>.

DEFINE icon_ok_ko1.
  if &1 = '0'.
    write: '  ', icon_incomplete as icon, '  '.
  elseif not &1 is initial.
    write: '  ', icon_checked as icon, '  '.
  else.
    write: '  ', icon_led_inactive as icon, '  '.
  endif.
END-OF-DEFINITION.

DEFINE icon_ok_ko2.
  if not &1 is initial.
    write: '  ', icon_checked as icon, '  '.
  else.
    write: '  ', icon_incomplete as icon, '  '.
  endif.
END-OF-DEFINITION.
