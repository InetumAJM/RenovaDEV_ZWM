************************************************************************
*                                                                      *
*     *************************************************************    *
*     *  ROFF - Consultoria em Tecnologia de Informação           *    *
*     *                                                           *    *
*     *                          SAP                              *    *
*     *************************************************************    *
*                                                                      *
************************************************************************
* Nome ABAP.: ZWM_INCLUDE_TOP                                          *
* Nm.Cliente: RENOVA                                                   *
* Descrição.: Include Top para Pool de Módulos                         *
* Criado por: Luís Rocha                                               *
* Criado em.: 06/01/2004                                               *
* Tipo PRG..: Include                                                  *
************************************************************************

data: ctrl_maxlines  like sy-tabix,
      ctrl_firstline like sy-tabix,
      ctrl_line      like sy-tabix,
      old_ctrl_line  like sy-tabix,
      w_indice       like sy-tabix,
      x_ucomm        like sy-ucomm,
      w_ucomm        like sy-ucomm.

data: w_flag_del     value ' ',
      flag_del       value ' ',
      w_flag         value ' ',
      w_len          type i,
      x_len          type i.

data: txt_oper(30),
      txt_mens(30),
      flg_initial.

constants:
           k_on      value 'X',
           k_off     value ' ',
           k_um      value '1',
           k_zero    value '0'.

data: begin of ihelp_fields occurs 20.
        include structure help_value.
data: end of ihelp_fields.

data: tnam like help_info-tabname,
      fnam like help_info-fieldname.

data: begin of scroll_0100,
        firstline       like sy-tabix, " First line issued .............
        start           like sy-tabix, " Beginning of table substructure
        maxlines        like sy-tabix, " Total no. of output lines .....
        loops           like sy-tabix, " No. of lines of the STEP loop .
        page_number     like sy-tabix, " Current page no. ..............
        entries_sum     like sy-tabix, " Total no. of entries ..........
        page_sum        like sy-tabix, " Total no. of pages ............
        new_page_number like sy-tabix, " Target page number ............
        cucol           like sy-cucol, " Cursor column .................
        curow           like sy-curow, " Cursor row ....................
        initial,                       " Flag inicio ...................
        changes,                       " Flag alteraäes ...............
      end of scroll_0100,
      scroll_0150 like scroll_0100,
      scroll_0200 like scroll_0100,
      scroll_0300 like scroll_0100,
      scroll_0400 like scroll_0100.
