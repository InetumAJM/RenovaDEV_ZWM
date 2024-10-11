*******************************************************************
*   System-defined Include-files.                                 *
*******************************************************************
  include lzwm50qtop.                    " Global Data
  include lzwm50quxx.                    " Function Modules

*******************************************************************
*   User-defined Include-files (if necessary).                    *
*******************************************************************
* INCLUDE LV50QF...                          " Subprograms
* INCLUDE LV50QO...                          " PBO-Modules
* INCLUDE LV50QI...                          " PAI-Modules

* Help routines
  include lzwm50qf0a.
* Routines for initialization of list output
  include lzwm50qf0i.
* Routines for list output
  include lzwm50qf0l.
* Routines for processing of subsequent functions
  include lzwm50qf0p.
* Routines for data reduction
  include lzwm50qf0r.
* Routines for selection of additional data
  include lzwm50qf0s.
* Routines for processing of user commands
  include lzwm50qf0u.

* PBO modules for dialog pop-ups
  include lzwm50qo01.
* PAI modules for dialog pop-ups
  include lzwm50qi01.

* Include for user-defined forms
  include lzwm50qfz1.

* New includes as of release 4.70
  include lzwm50qf3v.
  include lzwm50qf3w.
  include lzwm50qf3x.
  include lzwm50qf3y.
