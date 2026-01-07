CLASS ycl_pack_adv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      gt_item  TYPE TABLE OF zstr_pack_adv_itm,
      gs_item  TYPE zstr_pack_adv_itm,
      gt_pack  TYPE TABLE OF zstr_pack_adv_hdr,
      gs_pack  TYPE zstr_pack_adv_hdr,
      gt_final TYPE TABLE OF zsd_pack_adv,
      gs_final TYPE zsd_pack_adv,
      lv_vbeln TYPE c LENGTH 10.

    DATA : lv_char10 TYPE c LENGTH 10 .
    DATA : lv_char20 TYPE c LENGTH 20 .
    DATA : lv_char120 TYPE c LENGTH 120 .

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sy_uname     TYPE c LENGTH 20.

    METHODS:
      get_sales_data
        IMPORTING
                  im_vbeln       LIKE lv_char10
        RETURNING VALUE(et_pack) LIKE gt_pack,

      save_data_get_packnum
        IMPORTING
                  xt_pack            LIKE gt_pack
                  im_action          LIKE lv_char10
        RETURNING VALUE(rv_pack_num) LIKE lv_char120,

      get_pack_change_data
        IMPORTING
                  im_packnum     LIKE lv_char10
        RETURNING VALUE(et_pack) LIKE gt_pack.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_PACK_ADV IMPLEMENTATION.


  METHOD get_sales_data.

    lv_vbeln = im_vbeln.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT * FROM i_salesdocumentitem "#EC CI_ALL_FIELDS_NEEDED
    WHERE salesdocument = @lv_vbeln
    INTO TABLE @DATA(lt_sales_item).

    gs_pack-saleorderno     = lv_vbeln.
    gs_pack-vehicleno       = ''.
    gs_pack-mobileno        = ''.
    gs_pack-entrydate       = sys_date.
    gs_pack-destination     = ''.
    gs_pack-dimclaim        = ''.
    gs_pack-brokername      = ''.

    LOOP AT lt_sales_item INTO DATA(ls_item).
      gs_item-saleorderno   = ls_item-salesdocument.
      gs_item-saleorderitem   = ls_item-salesdocumentitem.
      gs_item-material        = ls_item-material.
      gs_item-materialdesc    = ls_item-salesdocumentitemtext.
      gs_item-custmateril     = ls_item-materialbycustomer.
      gs_item-quantity        = ls_item-orderquantity.
      IF ls_item-committeddelivqtyinordqtyunit IS NOT INITIAL.
        gs_item-unitweight      = ls_item-itemnetweight / ls_item-committeddelivqtyinordqtyunit.
      ENDIF.
      gs_item-unit            = ls_item-itemweightunit.
      gs_item-bookwt          = gs_item-quantity * gs_item-unitweight.
      gs_item-rate            = ls_item-netpriceamount.
      CONDENSE gs_item-quantity.
      CONDENSE gs_item-rate.
      CONDENSE gs_item-bookwt.
      CONDENSE gs_item-unitweight.
      APPEND gs_item TO gt_item.

      CLEAR: ls_item.
    ENDLOOP.

    INSERT LINES OF gt_item INTO TABLE gs_pack-items.
    APPEND gs_pack TO et_pack.

    "et_pack[] = gt_pack[].

  ENDMETHOD.


  METHOD get_pack_change_data.

    DATA:
      gt_final  TYPE TABLE OF zstr_pack_adv_hdr,
      gs_final  TYPE zstr_pack_adv_hdr,
      gs_item   TYPE zstr_pack_adv_itm,
      pack_item TYPE TABLE OF zstr_pack_adv_itm.

    SELECT * FROM zsd_pack_adv WHERE packadvnum = @im_packnum
             INTO TABLE @DATA(gt_pack).

    DATA(gt_pitem) = gt_pack[].
    SORT gt_pack BY packadvnum.
    DELETE ADJACENT DUPLICATES FROM gt_pack COMPARING packadvnum.

    LOOP AT gt_pack INTO DATA(gs_pack).

      MOVE-CORRESPONDING gs_pack TO gs_final.

      LOOP AT gt_pitem INTO DATA(gs_pitem) WHERE packadvnum = gs_pack-packadvnum.

        MOVE-CORRESPONDING gs_pitem TO gs_item.
        APPEND gs_item TO pack_item.

      ENDLOOP.

      INSERT LINES OF pack_item INTO TABLE gs_final-items.
      APPEND gs_final TO et_pack.

    ENDLOOP.

  ENDMETHOD.


  METHOD save_data_get_packnum.

    DATA:
      lv_index TYPE sy-tabix,
      lv_year  TYPE n LENGTH 4.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    IF xt_pack[] IS NOT INITIAL.

      IF im_action = 'create'.

        TRY.

            lv_year = sys_date+0(4).
            CALL METHOD cl_numberrange_runtime=>number_get
              EXPORTING
                nr_range_nr = '01'
                object      = 'ZNR_PAKADV'
                toyear      = lv_year
              IMPORTING
                number      = DATA(pack_num)
                returncode  = DATA(rcode).

          CATCH cx_nr_object_not_found ##NO_HANDLER.
          CATCH cx_number_ranges ##NO_HANDLER.

        ENDTRY.

      ENDIF.

      LOOP AT xt_pack INTO DATA(xs_pack).

        MOVE-CORRESPONDING xs_pack TO gs_final.
        IF im_action = 'change'.
          pack_num = gs_final-packadvnum.
        ENDIF.

        SHIFT pack_num LEFT DELETING LEADING '0'.
        gs_final-packadvnum   = pack_num.
        gs_final-erdate   = sys_date . """ CL_ABAP_CONTEXT_INFO=>get_system_date( ) .
        gs_final-uzeit    = sys_time . """ CL_ABAP_CONTEXT_INFO=>get_system_time( ) .
        gs_final-uname    = sy-uname.

        LOOP AT xs_pack-items INTO DATA(xs_pack_item).
          lv_index = lv_index + 1.
          MOVE-CORRESPONDING xs_pack_item TO gs_final.
          gs_final-bookwt          =  gs_final-quantity *  gs_final-unitweight.
          gs_final-saleorderno    = xs_pack-saleorderno.
          "*gs_final-pack_posnr = lv_index.
          APPEND gs_final TO gt_final.
        ENDLOOP.

      ENDLOOP.

      IF gt_final[] IS NOT INITIAL.

        IF im_action = 'create'.

          INSERT zsd_pack_adv FROM TABLE @gt_final.
          IF sy-subrc EQ 0.
            CONCATENATE 'Packing number' pack_num 'generated successfully' INTO rv_pack_num SEPARATED BY space ##NO_TEXT.
          ENDIF.

        ELSEIF im_action = 'change'.

          MODIFY zsd_pack_adv FROM TABLE @gt_final.
          IF sy-subrc EQ 0.
            CONCATENATE 'Packing number' pack_num 'updated successfully' INTO rv_pack_num SEPARATED BY space ##NO_TEXT.
          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
