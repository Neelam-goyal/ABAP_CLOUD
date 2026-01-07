CLASS zcl_workorder_goodsmvt DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES IF_CO_BD_WORKORDER_CONFIRM.
    INTERFACES if_workorder_goodsmovt_bkf_gdr .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WORKORDER_GOODSMVT IMPLEMENTATION.


  METHOD if_workorder_goodsmovt_bkf_gdr~modify_goods_movement.

  ENDMETHOD.
ENDCLASS.
