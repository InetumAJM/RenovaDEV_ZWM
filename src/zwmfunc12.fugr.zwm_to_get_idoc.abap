FUNCTION zwm_to_get_idoc.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_LGNUM) TYPE  LGNUM
*"     REFERENCE(I_TANUM) TYPE  TANUM
*"  EXPORTING
*"     REFERENCE(E_IDOCNUM) TYPE  EDI_DOCNUM
*"----------------------------------------------------------------------


  DATA: lv_objkey     TYPE swo_typeid.
  DATA: lv_objtype    TYPE swo_objtyp.
  DATA: ls_srrelroles TYPE srrelroles.
  DATA: ls_idocrel    TYPE idocrel.

  CLEAR e_idocnum.

** obter numero de idoc
**********************************************************************
  MOVE i_lgnum TO lv_objkey.
  MOVE i_tanum TO lv_objkey+3.

  CONDENSE lv_objkey NO-GAPS.

  lv_objtype = 'BUS2018'.

  DO 1 TIMES.


    SELECT * UP TO 1 ROWS
    FROM srrelroles INTO ls_srrelroles
    WHERE objkey  = lv_objkey
    AND   objtype = lv_objtype
    AND   roletype = 'OUTBELEG'
    ORDER BY PRIMARY KEY.
    ENDSELECT.


    CHECK sy-subrc = 0.



    SELECT * UP TO 1 ROWS FROM idocrel INTO ls_idocrel
                          WHERE role_a = ls_srrelroles-roleid
    ORDER BY utctime DESCENDING.
    ENDSELECT.



    CHECK sy-subrc = 0.




    SELECT * UP TO 1 ROWS
    FROM srrelroles INTO ls_srrelroles
    WHERE roleid  = ls_idocrel-role_b
    ORDER BY PRIMARY KEY.
    ENDSELECT.

    CHECK sy-subrc = 0.

    e_idocnum = ls_srrelroles-objkey.

  ENDDO.


ENDFUNCTION.
