*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZWM001..........................................*
DATA:  BEGIN OF STATUS_ZWM001                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM001                        .
CONTROLS: TCTRL_ZWM001
            TYPE TABLEVIEW USING SCREEN '0001'.
*...processing: ZWM002..........................................*
DATA:  BEGIN OF STATUS_ZWM002                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM002                        .
CONTROLS: TCTRL_ZWM002
            TYPE TABLEVIEW USING SCREEN '0007'.
*...processing: ZWM007..........................................*
DATA:  BEGIN OF STATUS_ZWM007                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM007                        .
CONTROLS: TCTRL_ZWM007
            TYPE TABLEVIEW USING SCREEN '0002'.
*...processing: ZWM009..........................................*
DATA:  BEGIN OF STATUS_ZWM009                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM009                        .
CONTROLS: TCTRL_ZWM009
            TYPE TABLEVIEW USING SCREEN '0003'.
*...processing: ZWM010..........................................*
DATA:  BEGIN OF STATUS_ZWM010                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM010                        .
CONTROLS: TCTRL_ZWM010
            TYPE TABLEVIEW USING SCREEN '0004'.
*...processing: ZWM022..........................................*
DATA:  BEGIN OF STATUS_ZWM022                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM022                        .
CONTROLS: TCTRL_ZWM022
            TYPE TABLEVIEW USING SCREEN '0005'.
*...processing: ZWM024..........................................*
DATA:  BEGIN OF STATUS_ZWM024                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM024                        .
CONTROLS: TCTRL_ZWM024
            TYPE TABLEVIEW USING SCREEN '0006'.
*...processing: ZWM062..........................................*
DATA:  BEGIN OF STATUS_ZWM062                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM062                        .
CONTROLS: TCTRL_ZWM062
            TYPE TABLEVIEW USING SCREEN '0008'.
*...processing: ZWM075..........................................*
DATA:  BEGIN OF STATUS_ZWM075                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM075                        .
CONTROLS: TCTRL_ZWM075
            TYPE TABLEVIEW USING SCREEN '0009'.
*...processing: ZWM079..........................................*
DATA:  BEGIN OF STATUS_ZWM079                        .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZWM079                        .
CONTROLS: TCTRL_ZWM079
            TYPE TABLEVIEW USING SCREEN '0010'.
*.........table declarations:.................................*
TABLES: *ZWM001                        .
TABLES: *ZWM002                        .
TABLES: *ZWM007                        .
TABLES: *ZWM009                        .
TABLES: *ZWM010                        .
TABLES: *ZWM022                        .
TABLES: *ZWM024                        .
TABLES: *ZWM062                        .
TABLES: *ZWM075                        .
TABLES: *ZWM079                        .
TABLES: ZWM001                         .
TABLES: ZWM002                         .
TABLES: ZWM007                         .
TABLES: ZWM009                         .
TABLES: ZWM010                         .
TABLES: ZWM022                         .
TABLES: ZWM024                         .
TABLES: ZWM062                         .
TABLES: ZWM075                         .
TABLES: ZWM079                         .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
