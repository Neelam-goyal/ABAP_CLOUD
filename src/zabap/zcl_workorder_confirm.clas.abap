CLASS zcl_workorder_confirm DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_co_bd_workorder_confirm .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WORKORDER_CONFIRM IMPLEMENTATION.


  METHOD if_co_bd_workorder_confirm~save_confirmation.
*  if 1 = 2.
**    errormessagetext  = 'Invalid Data'.
*  ENDIF.
  ENDMETHOD.
ENDCLASS.
