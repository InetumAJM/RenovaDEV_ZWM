FUNCTION-POOL zwm082                     MESSAGE-ID sv.

* INCLUDE LZWM082D...                        " Local class definition
INCLUDE lsvimdat                                . "general data decl.
INCLUDE lzwm082t00                              . "view rel. data dcl.


DATA: BEGIN OF gs_9500,
        weekday_name TYPE t246-kurzt,
      END OF gs_9500.
