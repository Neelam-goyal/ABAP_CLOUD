CLASS zcl_http_post_goods_movement DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_POST_GOODS_MOVEMENT IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    TYPES: BEGIN OF gty_msg,
             message TYPE string,
           END OF gty_msg.

    DATA: gt_msg TYPE TABLE OF gty_msg,
          gs_msg TYPE gty_msg.

    TYPES: BEGIN OF gty_item,
             ponum           TYPE c LENGTH 40,
             poitem          TYPE c LENGTH 40,
             ebeln           TYPE c LENGTH 40,
             ebelp           TYPE c LENGTH 40,
             supplier        TYPE c LENGTH 10,
             matnr           TYPE c LENGTH 40,
             maktx           TYPE c LENGTH 40,
             docdate         TYPE c LENGTH 40,
             poqty           TYPE c LENGTH 40,
             uom             TYPE c LENGTH 40,
             netprice        TYPE c LENGTH 40,
             challnqty       TYPE c LENGTH 40,
             migoqty         TYPE c LENGTH 40,
             storagelocation TYPE c LENGTH 40,
             delnoteqty      TYPE c LENGTH 40,
           END OF gty_item.

    DATA: xt_item TYPE TABLE OF gty_item,
          xs_item TYPE gty_item.

    TYPES: BEGIN OF gty_hdr,
             gentrynum  TYPE c LENGTH 40,
             docdate    TYPE c LENGTH 40,
             postdate   TYPE c LENGTH 40,
             headertext TYPE c LENGTH 40,
             invoiceref TYPE c LENGTH 40,
             billofladd TYPE c LENGTH 40,
             token_type TYPE c LENGTH 10,
             geitem     LIKE xt_item,
           END OF gty_hdr.

    DATA: xt_data TYPE TABLE OF gty_hdr,
          xs_data TYPE gty_hdr.

    DATA:
      lv_action  TYPE c LENGTH 10,
      miw_string TYPE string,
      lo_migo    TYPE REF TO zcl_goods_movement_create,
      gt_header  TYPE TABLE OF zstr_migo_header,
      gs_header  TYPE zstr_migo_header,
      gt_item    TYPE TABLE OF zstr_migo_item,
      gs_item    TYPE zstr_migo_item,
      gt_serial  TYPE TABLE OF zstr_migo_serial,
      lv_gm_code TYPE c LENGTH 2.

    CREATE OBJECT lo_migo.

    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).
    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname'.
    IF sy-subrc EQ 0.
      lv_action = ls_action-value.
    ENDIF.

    "Get inbound data
    DATA(lv_request_body) = request->get_text( ).

    /ui2/cl_json=>deserialize(
                    EXPORTING json = lv_request_body
                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                       CHANGING data = xt_data
                       ).

    READ TABLE xt_data INTO DATA(xs_data_new) INDEX 1.

    IF xs_data_new-token_type = 'NORMAL'.

      ""****Start: Validation for duplicate invoice no*************************"""""
      IF xt_data[] IS NOT INITIAL.

        DATA: lv_inv_no        TYPE zi_grn_detail-referencedocument,
              lv_doc_date      TYPE d,
              lv_fis_year      TYPE zi_dc_note-fiscalyear,
              lv_doc_month(2)  TYPE n,
              lv_fis_oyear     TYPE zi_dc_note-fiscalyear,
              lv_po_item       TYPE zmm_ge_data-poitem,
              lv_doc_omonth(2) TYPE n,
              lv_supplier      TYPE zi_grn_detail-supplier,
              lv_dup_bill      TYPE c.

        DATA:
          sys_date     TYPE d.

        sys_date = cl_abap_context_info=>get_system_date( ).

        lv_inv_no = xs_data_new-invoiceref.

        lv_doc_date  = sys_date.
        lv_doc_month = lv_doc_date+4(2).
        lv_fis_year  = lv_doc_date+0(4).

        IF lv_doc_month LT 4. "( v_month = '01' OR v_month = '02' OR v_month = '03' ).
          lv_fis_year = lv_fis_year - 1.
        ENDIF.

        SELECT SINGLE lifnr FROM zmm_ge_data
                            WHERE gentry_num = @xs_data_new-gentrynum
                            INTO @lv_supplier.              "#EC WARNOK

        lv_supplier = |{ lv_supplier ALPHA = IN }| .

        SELECT * FROM zi_grn_detail
                 WHERE referencedocument = @lv_inv_no AND
                       goodsmovementtype IN ( '101', '102' ) AND
                       supplier = @lv_supplier
                 INTO TABLE @DATA(gt_mdoc).   "#EC CI_ALL_FIELDS_NEEDED

        LOOP AT gt_mdoc ASSIGNING FIELD-SYMBOL(<lfs_mdoc>).

          CLEAR: lv_fis_oyear, lv_doc_omonth.
          lv_doc_omonth = <lfs_mdoc>-postingdate+4(2).
          lv_fis_oyear  = <lfs_mdoc>-postingdate+0(4).
          IF lv_doc_omonth LT 4.
            lv_fis_oyear = lv_fis_oyear - 1.
          ENDIF.
          <lfs_mdoc>-materialdocumentyear = lv_fis_oyear.

        ENDLOOP.

        LOOP AT gt_mdoc INTO DATA(wa_mkpf) WHERE goodsmovementtype = '101'.

          READ TABLE gt_mdoc INTO DATA(wa_mkpf1)
                         WITH KEY reversedmaterialdocument = <lfs_mdoc>-materialdocument
                                                     goodsmovementtype = '102'. "#EC CI_NOORDER
          IF sy-subrc = 0.
            CONTINUE.
          ELSE.
            IF wa_mkpf-materialdocumentyear = lv_fis_year.
              miw_string  =  'This Bill No. has already been posted' ##NO_TEXT.
              lv_dup_bill = abap_true.
              EXIT.                                     "#EC CI_NOORDER
            ENDIF.
          ENDIF.

          CLEAR: wa_mkpf.
        ENDLOOP.

      ENDIF.
      ""****End: Validation for duplicate invoice no***************************"""""

      IF lv_dup_bill = abap_false.

        IF xt_data[] IS NOT INITIAL.

          READ TABLE xt_data INTO xs_data INDEX 1.

          CLEAR: gs_header.
          gs_header-genumber                       = xs_data-gentrynum.
          gs_header-documentdate                   = xs_data-docdate.
          gs_header-postingdate                    = xs_data-postdate.
          gs_header-materialdocumentheadertext     = xs_data-headertext.
          gs_header-referencedocument              = xs_data-invoiceref.
          gs_header-goodsmovementcode              = '01'.
          APPEND gs_header TO gt_header.

          SELECT SINGLE werks FROM zmm_ge_data
                              WHERE gentry_num = @xs_data_new-gentrynum
                              INTO @DATA(lv_werks).         "#EC WARNOK

          LOOP AT xs_data-geitem INTO xs_item.

            gs_item-material                     = xs_item-matnr.
            gs_item-plant                        = lv_werks.
            gs_item-storagelocation              = xs_item-storagelocation.
            gs_item-goodsmovementtype            = '101'.
            gs_item-purchaseorder                = xs_item-ponum.
            SHIFT xs_item-poitem LEFT DELETING LEADING space.
            lv_po_item = xs_item-poitem.
            gs_item-purchaseorderitem            = lv_po_item.
            gs_item-goodsmovementrefdoctype      = ''.
            gs_item-quantityinentryunit          = xs_item-migoqty. "challnqty.
            gs_item-delnoteqty                   = xs_item-delnoteqty.
            gs_item-batch                        = ''.
            gs_item-issgorrcvgmaterial           = ''.
            gs_item-issgorrcvgbatch              = ''.
            gs_item-issuingorreceivingplant      = ''.
            gs_item-issuingorreceivingstorageloc = ''.
            APPEND gs_item TO gt_item.

          ENDLOOP.

          lv_gm_code = '01'.
          lo_migo->post_goods_mvt(
            EXPORTING
              im_header  = gt_header
              im_item    = gt_item
              im_serial  = gt_serial
              im_action  = lv_action
              im_gm_code = lv_gm_code
            RECEIVING
              es_result  = miw_string
          ).

        ENDIF.

        ""**Setting response/pdf in base64 format to UI5
        response->set_text(
          EXPORTING
            i_text = miw_string ).

      ELSE.

        response->set_text(
          EXPORTING
            i_text = miw_string ).


      ENDIF.


    ELSEIF xs_data_new-token_type = 'AARTI'.

      """*****Incase of multiple vendor************"
      IF xt_data[] IS NOT INITIAL.

        READ TABLE xt_data INTO xs_data INDEX 1.

        DATA(ct_geitem) = xs_data-geitem[].
        SORT ct_geitem BY supplier.
        DELETE ADJACENT DUPLICATES FROM ct_geitem COMPARING supplier.

        LOOP AT ct_geitem INTO DATA(cs_geitem).

          CLEAR: gs_header.
          gs_header-genumber                       = xs_data-gentrynum.
          gs_header-documentdate                   = xs_data-docdate.
          gs_header-postingdate                    = xs_data-postdate.
          gs_header-materialdocumentheadertext     = xs_data-headertext.
          gs_header-referencedocument              = xs_data-invoiceref.
          gs_header-goodsmovementcode              = '01'.
          APPEND gs_header TO gt_header.

          SELECT SINGLE werks FROM zmm_ge_data
                              WHERE gentry_num = @xs_data_new-gentrynum
                              INTO @lv_werks.               "#EC WARNOK

          LOOP AT xs_data-geitem INTO xs_item WHERE supplier = cs_geitem-supplier.

            gs_item-material                     = xs_item-matnr.
            gs_item-plant                        = lv_werks.
            gs_item-storagelocation              = xs_item-storagelocation.
            gs_item-goodsmovementtype            = '101'.
            gs_item-purchaseorder                = xs_item-ponum.
            SHIFT xs_item-poitem LEFT DELETING LEADING space.
            lv_po_item = xs_item-poitem.
            gs_item-purchaseorderitem            = lv_po_item.
            gs_item-goodsmovementrefdoctype      = ''.
            gs_item-quantityinentryunit          = xs_item-migoqty. "challnqty.
            gs_item-delnoteqty                   = xs_item-delnoteqty.
            gs_item-batch                        = ''.
            gs_item-issgorrcvgmaterial           = ''.
            gs_item-issgorrcvgbatch              = ''.
            gs_item-issuingorreceivingplant      = ''.
            gs_item-issuingorreceivingstorageloc = ''.
            APPEND gs_item TO gt_item.

            CLEAR: xs_item, gs_item.
          ENDLOOP.

          lv_gm_code = '01'.
          lo_migo->post_goods_mvt(
            EXPORTING
              im_header  = gt_header
              im_item    = gt_item
              im_serial  = gt_serial
              im_action  = lv_action
              im_gm_code = lv_gm_code
            RECEIVING
              es_result  = miw_string
          ).

          gs_msg-message = miw_string.
          APPEND gs_msg TO gt_msg.

          CLEAR: cs_geitem, gt_header, gt_item, gt_serial.
        ENDLOOP.

        IF gt_msg[] IS NOT INITIAL.

          DATA(lv_json)  = /ui2/cl_json=>serialize(
            data             = gt_msg[]
            pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
            ).

        ENDIF.

        miw_string = lv_json.

      ENDIF.

      ""**Setting response/pdf in base64 format to UI5
      response->set_text(
        EXPORTING
          i_text = miw_string ).

    ENDIF.

  ENDMETHOD.
ENDCLASS.
