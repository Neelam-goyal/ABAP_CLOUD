CLASS zcl_slin_badi_dbtab_access DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_slin_badi_dbtab_access .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SLIN_BADI_DBTAB_ACCESS IMPLEMENTATION.


  METHOD if_slin_badi_dbtab_access~add_dbtab_access.
    ir->accept_all_dbtabs( ).
  ENDMETHOD.
ENDCLASS.
