CLASS ycl_token_process DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      gt_data TYPE TABLE OF zmm_token_data,
      gs_data TYPE zmm_token_data.

    DATA:
      lv_char10  TYPE c LENGTH 10,
      lv_char120 TYPE c LENGTH 120,
      lv_char4   TYPE c LENGTH 4.

    METHODS:
      save_data_get_tokennum
        IMPORTING
                  it_data             LIKE gt_data
                  im_action           LIKE lv_char10
        RETURNING VALUE(rv_token_num) LIKE lv_char120,

      get_token_change_data
        IMPORTING
                  im_token_num   LIKE lv_char10
        RETURNING VALUE(et_data) LIKE gt_data.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_TOKEN_PROCESS IMPLEMENTATION.


  METHOD get_token_change_data.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20,
      lv_token_num TYPE c LENGTH 10.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT * FROM zmm_token_data WHERE tokennum  = @im_token_num AND tokendeleted = @abap_false
             INTO TABLE @DATA(ct_token_data).

    IF ct_token_data[] IS NOT INITIAL.

      et_data[] = CORRESPONDING #( ct_token_data[] ).

    ENDIF.

  ENDMETHOD.


  METHOD save_data_get_tokennum.

    DATA:
      lv_material  TYPE C LENGTH 18, "i_inspectionlot-material,
      lv_plant     TYPE i_inspectionlot-plant,
      lv_lottype   TYPE i_inspectionlot-inspectionlottype,
      lv_lotorigin TYPE i_inspectionlot-inspectionlotorigin,
      lv_supplier  TYPE i_inspectionlot-supplier,
      lv_lottext   TYPE i_inspectionlot-inspectionlottext,
      lv_lotqty    TYPE i_inspectionlot-inspectionlotquantity.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20.

    DATA:
      xt_data TYPE TABLE OF zmm_token_data,
      xs_data TYPE zmm_token_data.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    IF im_action = 'create'.

      IF it_data[] IS NOT INITIAL.

        READ TABLE it_data INTO DATA(ls_data_new) INDEX 1.
        TRY.

            IF ls_data_new-plant = '1001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '16'
                  object      = 'ZNR_TOKEN'
                IMPORTING
                  number      = DATA(lv_token_num)
                  returncode  = DATA(rcode).

            ELSEIF ls_data_new-plant = '1002'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '12'
                  object      = 'ZNR_TOKEN'
                IMPORTING
                  number      = lv_token_num
                  returncode  = rcode.

            ELSEIF ls_data_new-plant = '2001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '13'
                  object      = 'ZNR_TOKEN'
                IMPORTING
                  number      = lv_token_num
                  returncode  = rcode.

            ELSEIF ls_data_new-plant = '3001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '14'
                  object      = 'ZNR_TOKEN'
                IMPORTING
                  number      = lv_token_num
                  returncode  = rcode.

            ELSEIF ls_data_new-plant = '4001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '15'
                  object      = 'ZNR_TOKEN'
                IMPORTING
                  number      = lv_token_num
                  returncode  = rcode.

            ENDIF.

          CATCH cx_nr_object_not_found ##NO_HANDLER.
          CATCH cx_number_ranges ##NO_HANDLER.
        ENDTRY.
      ENDIF.

    ENDIF.

    xt_data[] = it_data[].
    LOOP AT xt_data ASSIGNING FIELD-SYMBOL(<lfs_data>).

      IF im_action = 'create'.

*        lv_material   = <lfs_data>-material. "'AFGI076180AA'.
*        lv_material = |{ lv_material ALPHA = IN }| .
*        lv_plant      = <lfs_data>-plant.    "'1001'.
*        lv_lottype    = '89'.
*        "*lv_lotorigin  = '89'.
*        "*lv_supplier   = <lfs_data>-supplier. "'A055'.
*        lv_lottext    = 'Lot Creation From Token'.
*        lv_lotqty     = 1.
*
*        MODIFY ENTITY i_inspectionlottp_2
*
*            CREATE FIELDS ( material plant inspectionlottype inspectionlotquantity )
*                WITH VALUE #( (
*                    %cid                    = 'CID_001'
*                    material                = lv_material
*                    plant                   = lv_plant
*                    inspectionlottype       = lv_lottype
*                    "*inspectionlotorigin   = lv_lotorigin
*                    "*supplier              = lv_supplier
*                    inspectionlottext       = lv_lottext
*                    inspectionlotquantity   = lv_lotqty ) )
*            MAPPED DATA(mapped)
*            REPORTED DATA(reported)
*            FAILED DATA(failed).
*
*        IF sy-subrc EQ 0.
*          DATA(lt_lot_num) = mapped-inspectionlot.
*          READ TABLE lt_lot_num INTO DATA(ls_lot_num) INDEX 1.
*          DATA(lv_lot_num) = ls_lot_num-inspectionlot.
*          COMMIT WORK.
*        ENDIF.

        <lfs_data>-token_createdby   = sys_uname.
        <lfs_data>-token_createdon   = sys_date.
        <lfs_data>-token_createdtime = sys_time.

      ELSEIF im_action = 'change'.

        lv_token_num                 = <lfs_data>-tokennum.
        <lfs_data>-tokenchanged      = abap_true.
        <lfs_data>-token_changedby   = sys_uname.
        <lfs_data>-token_changedon   = sys_date.
        <lfs_data>-token_changedtime = sys_time.

      ELSEIF im_action = 'delete'.

        lv_token_num = <lfs_data>-tokennum.
        <lfs_data>-tokendeleted       = abap_true.
        <lfs_data>-token_deletedby    = sys_uname.
        <lfs_data>-token_deletedon    = sys_date.
        <lfs_data>-token_deletedtime  = sys_time.

      ENDIF.

      SHIFT lv_token_num LEFT DELETING LEADING '0'.
      CONDENSE lv_token_num.
      <lfs_data>-tokennum          = lv_token_num.

    ENDLOOP.

    IF im_action = 'create'.

      INSERT zmm_token_data FROM TABLE @xt_data.
      IF sy-subrc EQ 0.
        CONCATENATE 'Token number' lv_token_num 'generated successfully' INTO rv_token_num SEPARATED BY space ##NO_TEXT.
      ENDIF.

    ELSEIF im_action = 'change'.

      MODIFY zmm_token_data FROM TABLE @xt_data.

      IF sy-subrc EQ 0.
        CONCATENATE 'Token number' lv_token_num 'updated successfully' INTO rv_token_num SEPARATED BY space ##NO_TEXT.
      ENDIF.

    ELSEIF im_action = 'delete'.

      MODIFY zmm_token_data FROM TABLE @xt_data.
      IF sy-subrc EQ 0.
        CONCATENATE 'Token number' lv_token_num 'deleted successfully' INTO rv_token_num SEPARATED BY space ##NO_TEXT.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
