CLASS ycl_gate_out_process DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      xt_final TYPE TABLE OF zstr_ge_billing_f4,
      xt_data  TYPE TABLE OF zstr_ge_data,
      gt_final TYPE TABLE OF zsd_gout_data,
      gs_final TYPE zsd_gout_data.

    DATA:
      lv_char10  TYPE c LENGTH 10,
      lv_char120 TYPE c LENGTH 120,
      lv_char4   TYPE c LENGTH 4.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20.

    METHODS:
      get_so_f4_data
        IMPORTING
                  im_sonum       LIKE lv_char10
        RETURNING VALUE(et_data) LIKE xt_final,

      save_data_get_genum
        IMPORTING
                  xt_gedata        LIKE xt_data
                  im_action        LIKE lv_char10
        RETURNING VALUE(rv_ge_num) LIKE lv_char120,

      get_ge_change_data
        IMPORTING
                  im_genum       LIKE lv_char10
        RETURNING VALUE(et_data) LIKE xt_data,

      delete_data_genum
        IMPORTING
                  xt_gedata        LIKE xt_data
                  im_action        LIKE lv_char10
        RETURNING VALUE(rv_ge_num) LIKE lv_char120.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_GATE_OUT_PROCESS IMPLEMENTATION.


  METHOD delete_data_genum.

    IF im_action = 'delete'.

      READ TABLE xt_gedata INTO DATA(xs_gedata) INDEX 1.

      IF xs_gedata-gentry_num IS NOT INITIAL.

        UPDATE zsd_gout_data SET gedeleted = @abap_true
                             WHERE gentry_num = @xs_gedata-gentry_num.
        IF sy-subrc EQ 0.
          COMMIT WORK.
          rv_ge_num = |Gate entry number - | && xs_gedata-gentry_num && | deleted successfully| ##NO_TEXT.
        ENDIF.

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_ge_change_data.

    DATA:
      gs_change   TYPE zstr_ge_data,
      gt_item     TYPE TABLE OF zstr_ge_item,
      gs_item     TYPE zstr_ge_item,
      lv_open_qty TYPE p DECIMALS 2,
      lv_val_neg  TYPE c LENGTH 20.

    SELECT * FROM zsd_gout_data WHERE gentry_num = @im_genum AND gedeleted = @abap_false
             INTO TABLE @DATA(gt_gedata).

    DATA(gt_pitem) = gt_gedata[].
    SORT gt_gedata BY gentry_num.
    DELETE ADJACENT DUPLICATES FROM gt_gedata COMPARING gentry_num.

    LOOP AT gt_gedata INTO DATA(gs_gedata).

      MOVE-CORRESPONDING gs_gedata TO gs_change.

      IF gs_change-check_rc EQ 'X'.
        gs_change-check_rc = 'true'.
      ENDIF.

      IF gs_change-check_pollt EQ 'X'.
        gs_change-check_pollt = 'true'.
      ENDIF.

      IF gs_change-check_tripal EQ 'X'.
        gs_change-check_tripal = 'true'.
      ENDIF.

      IF gs_change-check_insur EQ 'X'.
        gs_change-check_insur = 'true'.
      ENDIF.

      IF gs_change-check_dl EQ 'X'.
        gs_change-check_dl = 'true'.
      ENDIF.


      LOOP AT gt_pitem INTO DATA(gs_pitem) WHERE gentry_num = gs_gedata-gentry_num.

        MOVE-CORRESPONDING gs_pitem TO gs_item.

        SELECT SINGLE
               invoicenum,
               invoiceitem,
               SUM( itemqty ) AS itemqty
               FROM zsd_gout_data
               WHERE invoicenum = @gs_pitem-invoicenum AND invoiceitem = @gs_pitem-invoiceitem
                   AND gedeleted = @abap_false
               GROUP BY invoicenum, invoiceitem
               INTO @DATA(ls_ge).                           "#EC WARNOK

        lv_open_qty = gs_pitem-itemqty.
        lv_open_qty = lv_open_qty - ls_ge-itemqty.
        lv_val_neg  = lv_open_qty.
        CONDENSE lv_val_neg.
        IF lv_val_neg CA '-'.
          lv_open_qty = 0.
        ENDIF.

        gs_item-openqty = lv_open_qty.
        gs_item-invoicenum   = gs_item-invoicenum.
        gs_item-invoiceitem   = gs_item-invoiceitem.
        APPEND gs_item TO gt_item.

        CLEAR: lv_open_qty, ls_ge.
      ENDLOOP.

      SORT gt_item BY ebeln ebelp.
      INSERT LINES OF gt_item INTO TABLE gs_change-ge_item.
      APPEND gs_change TO et_data.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_so_f4_data.

    DATA:
      gt_data     TYPE TABLE OF zstr_ge_billing_f4,
      gs_data     TYPE zstr_ge_billing_f4,
      lv_open_qty TYPE p DECIMALS 2.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT
        billingdocument,
        billingdocumentitem,
        salesdocument,
        salesdocumentitem,
        organizationdivision,
        division,
        salesoffice,
        product,
        batch,
        plant,
        billingdocumentitemtext,
        billingquantity,
        billingquantityunit,
        itemgrossweight,
        itemnetweight,
        itemweightunit,
        netamount,
        transactioncurrency,
        grossamount,
        pricedetnexchangerate,
        soldtoparty,
        payerparty,
        billingdocumentdate
    FROM i_billingdocumentitem
    WHERE salesdocument = @im_sonum
    INTO TABLE @DATA(lt_bdoc).

    LOOP AT lt_bdoc INTO DATA(ls_bdoc).

      SELECT SINGLE
             saledocument,
             invoicenum,
             invoiceitem,
             SUM( itemqty )   AS itemqty,
             SUM( gross_wgt ) AS gross_wgt,
             SUM( net_wgt )   AS net_wgt
             FROM zsd_gout_data
             WHERE saledocument = @ls_bdoc-salesdocument AND
                   invoicenum   = @ls_bdoc-billingdocument AND
                   invoiceitem  = @ls_bdoc-billingdocumentitem AND
                   gedeleted    = @abap_false
             GROUP BY saledocument, invoicenum, invoiceitem
             INTO @DATA(ls_ge).                             "#EC WARNOK

      lv_open_qty = ls_bdoc-billingquantity.
      lv_open_qty = lv_open_qty - ls_ge-itemqty.

*      IF lv_open_qty GT 0.

      CLEAR: gs_data.
      gs_data-invoicenum   = ls_bdoc-billingdocument.
      gs_data-invoiceitem  = ls_bdoc-billingdocumentitem.
      gs_data-matnr        = ls_bdoc-Product.
      gs_data-maktx        = ls_bdoc-billingdocumentitemtext.
      gs_data-hsncode      = ''.
      gs_data-custcode     = ls_bdoc-soldtoparty.
      "*gs_data-doc_date     = ''.
      gs_data-itemqty      = ls_bdoc-billingquantity.
      gs_data-netprice     = ls_bdoc-grossamount.
      gs_data-itemgrosswt  = ls_bdoc-grossamount.
      gs_data-itemnetwt    = ls_bdoc-itemnetweight.
      gs_data-uom          = ls_bdoc-billingquantityunit.
      gs_data-currency     = ls_bdoc-transactioncurrency.
      APPEND gs_data TO gt_data.

*      ENDIF.

      SORT gt_data BY invoicenum invoiceitem.
      et_data[] = gt_data[].

      CLEAR: ls_bdoc,lv_open_qty, ls_ge.
    ENDLOOP.

  ENDMETHOD.


  METHOD save_data_get_genum.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear,
          lv_doc_month(2) TYPE n.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    IF xt_gedata[] IS NOT INITIAL.

      lv_doc_date  = sys_date.
      lv_doc_month = lv_doc_date+4(2).
      lv_fis_year  = lv_doc_date+0(4).

      READ TABLE xt_gedata INTO DATA(xs_gedata_new) INDEX 1.

      IF im_action = 'create'.

        TRY.

            IF xs_gedata_new-werks = '1001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '01'
                  object      = 'ZGATE_NUM'
                IMPORTING
                  number      = DATA(ge_num)
                  returncode  = DATA(rcode).



                   """ADD BY SB 06/01/2026""""""""
               ELSEIF xs_gedata_new-werks = '1002'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '02'
                  object      = 'ZGATE_NUM'
                IMPORTING
                  number      = ge_num
                  returncode  = rcode.

                 """ADD BY SB 06/01/2026""""""""

             """"""""ADD BY SB 25/12/2025"""""
               ELSEIF xs_gedata_new-werks = '3001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '03'
                  object      = 'ZGATE_NUM'
                IMPORTING
                  number      = ge_num
                  returncode  = rcode.

             """"""""ADD BY SB 25/12/2025"""""

            ELSEIF xs_gedata_new-werks = '4001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '04'
                  object      = 'ZGATE_NUM'
                IMPORTING
                  number      = ge_num
                  returncode  = rcode.

            ELSEIF xs_gedata_new-werks = '1005'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '12'
                  object      = 'ZGATE_NUM'
                IMPORTING
                  number      = ge_num
                  returncode  = rcode.

            ENDIF.

          CATCH cx_nr_object_not_found ##NO_HANDLER.
          CATCH cx_number_ranges ##NO_HANDLER.
        ENDTRY.

      ENDIF.

      LOOP AT xt_gedata INTO DATA(xs_gedata).

        MOVE-CORRESPONDING xs_gedata TO gs_final.
        IF im_action = 'change'.
          ge_num = gs_final-gentry_num.
        ENDIF.

        SHIFT ge_num LEFT DELETING LEADING '0'.
        gs_final-gentry_num  = ge_num.
        gs_final-gentry_year = lv_fis_year. "sys_date+0(4).
        gs_final-erdat       = sys_date.
        gs_final-uzeit       = sys_time.
        gs_final-uname       = sys_uname.
        gs_final-created_on   = sys_date.
        gs_final-created_time = sys_time.
        gs_final-out_date     = sys_date.
        gs_final-out_time     = sys_time.

        DELETE xs_gedata-ge_item WHERE invoicenum = ''.
        IF xs_gedata-ge_item[] IS NOT INITIAL.

          LOOP AT xs_gedata-ge_item INTO DATA(xs_ge_item).
            MOVE-CORRESPONDING xs_ge_item TO gs_final.
            gs_final-invoicenum  = xs_ge_item-invoicenum.
            gs_final-invoiceitem = xs_ge_item-invoiceitem.
            APPEND gs_final TO gt_final.
          ENDLOOP.

        ELSE.

          APPEND gs_final TO gt_final.

        ENDIF.

      ENDLOOP.

      IF gt_final[] IS NOT INITIAL.

        IF im_action = 'create'.

          INSERT zsd_gout_data FROM TABLE @gt_final.
          IF sy-subrc EQ 0.
            CONCATENATE 'Gate entry number' ge_num 'generated successfully' INTO rv_ge_num SEPARATED BY space ##NO_TEXT.
          ENDIF.

        ELSEIF im_action = 'change'.

          MODIFY zsd_gout_data FROM TABLE @gt_final.
          IF sy-subrc EQ 0.
            CONCATENATE 'Gate entry number' ge_num 'updated successfully' INTO rv_ge_num SEPARATED BY space ##NO_TEXT.
          ENDIF.

        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
