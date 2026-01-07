CLASS zcl_ge_process DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      xt_final TYPE TABLE OF zstr_ge_po_f4_data,
      xt_data  TYPE TABLE OF zstr_ge_data,
      gt_final TYPE TABLE OF zmm_ge_data,
      gs_final TYPE zmm_ge_data.

    DATA:
      lv_char10  TYPE c LENGTH 10,
      lv_char120 TYPE c LENGTH 120,
      lv_char4   TYPE c LENGTH 4.

    METHODS:
      get_po_f4_data
        IMPORTING
                  iv_lifnr       LIKE lv_char10
                  iv_werks       LIKE lv_char4
                  iv_ponum       LIKE lv_char10
        RETURNING VALUE(et_data) LIKE xt_final,

      get_ge_change_data
        IMPORTING
                  iv_genum       LIKE lv_char10
        RETURNING VALUE(et_data) LIKE xt_data,

      save_data_get_genum
        IMPORTING
                  xt_gedata        LIKE xt_data
                  im_action        LIKE lv_char10
        RETURNING VALUE(rv_ge_num) LIKE lv_char120,

      delete_data_genum
        IMPORTING
                  xt_gedata        LIKE xt_data
                  im_action        LIKE lv_char10
        RETURNING VALUE(rv_ge_num) LIKE lv_char120.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_GE_PROCESS IMPLEMENTATION.


  METHOD delete_data_genum.
    IF im_action = 'delete'.
      READ TABLE xt_gedata INTO DATA(xs_gedata) INDEX 1.

      IF xs_gedata-gentry_num IS NOT INITIAL.

        UPDATE zmm_ge_data SET gedeleted = @abap_true
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
      gs_change TYPE zstr_ge_data,
      gt_item   TYPE TABLE OF zstr_ge_item,
      gs_item   TYPE zstr_ge_item.

    SELECT * FROM zmm_ge_data WHERE gentry_num = @iv_genum AND gedeleted = @abap_false
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
        gs_item-ebeln    = gs_item-ponum.
        gs_item-ebelp    = gs_item-poitem.
        gs_item-supplier = gs_pitem-supplier.
        APPEND gs_item TO gt_item.

      ENDLOOP.

      SORT gt_item BY ebeln ebelp.
      INSERT LINES OF gt_item INTO TABLE gs_change-ge_item.
      APPEND gs_change TO et_data.

    ENDLOOP.


  ENDMETHOD.


  METHOD get_po_f4_data.

    DATA:
      gt_data            TYPE TABLE OF zstr_ge_po_f4_data,
      gs_data            TYPE zstr_ge_po_f4_data,
      lv_open_qty        TYPE p DECIMALS 2,
      gv_werks           TYPE c LENGTH 4,
      gv_supplying_plant TYPE c LENGTH 4,
      gv_lifnr           TYPE c LENGTH 10,
      gv_ponum           TYPE c LENGTH 10,
      lv_tolr_qty        TYPE zi_po_data-orderquantity,
      lv_mandi_item      TYPE zi_po_data-PurchaseOrderItem.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sy_uname     TYPE c LENGTH 20.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    gv_werks = iv_werks.
    gv_lifnr = iv_lifnr.
    gv_ponum = iv_ponum.
    gv_supplying_plant = iv_lifnr+6(4).

    IF gv_ponum IS INITIAL.

      SELECT * FROM zi_po_data WHERE supplier = @gv_lifnr AND
                                        plant = @gv_werks
*                                        purchasingprocessingstatus = '05'
                                    INTO TABLE @DATA(lt_po).

      SELECT * FROM zi_po_data WHERE supplyingplant = @gv_supplying_plant AND
                                        plant = @gv_werks
*                                        AND purchasingprocessingstatus = '05'
                                    APPENDING TABLE @lt_po.

    ELSE.

      SELECT * FROM zi_po_data WHERE purchaseorder = @gv_ponum
*      AND purchasingprocessingstatus = '05'
                                    INTO TABLE @DATA(lt_po_org).

      IF lt_po_org[] IS NOT INITIAL.

        loop at lt_po_org ASSIGNING FIELD-SYMBOL(<lfs_po_org>).
        if <lfs_po_org> is ASSIGNED.
         CONDENSE <lfs_po_org>-RequisitionerName.
         lv_mandi_item = <lfs_po_org>-RequisitionerName.
         lv_mandi_item = |{ lv_mandi_item ALPHA = IN }| .
         <lfs_po_org>-mandipoitem = lv_mandi_item.
        ENDIF.
        ENDLOOP.

        SELECT
          purchaseorder,
          purchaseorderitem,
          orderquantity,
          overdelivtolrtdlmtratioinpct,
          material,
          yy1_sampleinspno_pdi,
          plant,
          companycode,
          suppliermaterialnumber,
          requirementtracking,
          RequisitionerName,
          mandipoitem,
          productgroup,
          producttype,
          documentcurrency,
          referencedeliveryaddressid,
          subcontractor,
          yy1_item_remark_pdi,
          customer,
          supplier,
          supplyingplant,
          validitystartdate,
          validityenddate,
          purchaseorderdate,
          purchasingprocessingstatus,
          purchaseordertype,
          paymentterms,
          incotermsclassification,
          incotermslocation1,
          yy1_bardana_deduction_pdh,
          yy1_policy_no_pdh,
          yy1_shipto_pdh,
          productdescription,
          taxcode,
          orderpriceunit,
          netamount,
          netpriceamount,
          baseunit,
          purchaseorderquantityunit,
          subtotal1amount,
          subtotal6amount
        FROM zi_po_data
        FOR ALL ENTRIES IN @lt_po_org
        WHERE purchaseorder     = @lt_po_org-requirementtracking
              AND purchaseorderitem = @lt_po_org-mandipoitem
        INTO TABLE @lt_po.                         "#EC CI_NO_TRANSFORM

      ENDIF.

    ENDIF.

    SELECT * FROM zi_schedgagrmt_po WHERE supplier = @gv_lifnr
                                  AND plant = @gv_werks
                                  INTO TABLE @DATA(lt_po_schd).

    IF lt_po[] IS NOT INITIAL.

      SELECT
      purchaseorder,
      purchaseorderitem,
      goodsmovementtype,
      quantity
      FROM zi_po_hist FOR ALL ENTRIES IN @lt_po
      WHERE purchaseorder = @lt_po-purchaseorder AND purchaseorderitem = @lt_po-purchaseorderitem
      INTO TABLE @DATA(lt_hist).                   "#EC CI_NO_TRANSFORM

    ENDIF.

*    IF lt_po[] IS NOT INITIAL.

    LOOP AT lt_po INTO DATA(ls_po).

      READ TABLE lt_hist INTO DATA(ls_hist_201) WITH KEY purchaseorder     = ls_po-purchaseorder
                                                         purchaseorderitem = ls_po-purchaseorderitem
                                                         goodsmovementtype = '101'.

      READ TABLE lt_hist INTO DATA(ls_hist_202) WITH KEY purchaseorder     = ls_po-purchaseorder
                                                         purchaseorderitem = ls_po-purchaseorderitem
                                                         goodsmovementtype = '102'.

      SELECT SINGLE
             ponum,
             poitem,
             SUM( challnqty ) AS challnqty
             FROM zmm_ge_data
             WHERE ponum = @ls_po-purchaseorder AND poitem = @ls_po-purchaseorderitem
                 AND gedeleted = @abap_false
             GROUP BY ponum, poitem
             INTO @DATA(ls_ge).                             "#EC WARNOK

      lv_open_qty = ls_po-orderquantity. "- ( ls_hist_201-Quantity - ls_hist_202-Quantity ).
      lv_open_qty = lv_open_qty - ls_ge-challnqty.

      IF lv_open_qty GT 0.

        CLEAR: gs_data, lv_tolr_qty.

        IF ls_po-overdelivtolrtdlmtratioinpct IS NOT INITIAL AND ls_po-overdelivtolrtdlmtratioinpct NE 0.
          lv_tolr_qty = ( ls_po-orderquantity * ls_po-overdelivtolrtdlmtratioinpct ) / 100.
        ENDIF.

        gs_data-ebeln    =  ls_po-purchaseorder.
        gs_data-ebelp    =  ls_po-purchaseorderitem.
        gs_data-matnr    =  ls_po-material.
        gs_data-maktx    =  ls_po-productdescription.
        gs_data-supplier =  ls_po-supplier.
        gs_data-doc_date =  ls_po-purchaseorderdate.

        IF lt_po_org[] IS NOT INITIAL.

        DATA:
          lv_req_track TYPE zi_po_data-RequirementTracking,
          lv_req_manitm TYPE zi_po_data-mandipoitem.

      lv_req_track  = ls_po-PurchaseOrder.
      lv_req_manitm = ls_po-PurchaseOrderItem.


        READ TABLE lt_po_org INTO DATA(lvs_po_org)
        with key requirementtracking = lv_req_track
                 mandipoitem         = lv_req_manitm.

         if sy-subrc eq 0.
          gs_data-poqty   = lvs_po_org-OrderQuantity.
         ENDIF.

        else.
         gs_data-poqty    =  ls_po-orderquantity.
        endif.

        gs_data-opqty    =  lv_open_qty + lv_tolr_qty.
        gs_data-uom      =  ls_po-baseunit.
        gs_data-overtol  =  ls_po-overdelivtolrtdlmtratioinpct.
        gs_data-netprice =  ls_po-netpriceamount.
        gs_data-currency =  ls_po-orderpriceunit.
        gs_data-per      =  ''.
        gs_data-productgroup = ls_po-productgroup.
        gs_data-producttype  = ls_po-producttype.
        APPEND gs_data TO gt_data.

      ENDIF.

      CLEAR: ls_po,  ls_hist_201, ls_hist_202, lv_open_qty, ls_ge.
      clear: lv_req_track, lv_req_manitm.
    ENDLOOP.

    """***Data for scheduling agreement "5500000077
    DATA(ct_po_schd) = lt_po_schd[].

    DATA:
       lv_days15 TYPE d.

    IF lt_po_schd[] IS NOT INITIAL.

      SORT lt_po_schd BY schedulingagreement schedulingagreementitem.
      DELETE ADJACENT DUPLICATES FROM lt_po_schd COMPARING schedulingagreement schedulingagreementitem.

      lv_days15 = sys_date + 15.

      LOOP AT lt_po_schd INTO DATA(ls_po_schd).

        CLEAR: ls_po_schd-targetquantity.
        LOOP AT ct_po_schd INTO DATA(cs_po_schd) WHERE schedulingagreement = ls_po_schd-schedulingagreement AND
                                                       schedulingagreementitem = ls_po_schd-schedulingagreementitem AND
                                                       schedulelinedeliverydate LE lv_days15.

          ls_po_schd-targetquantity = ls_po_schd-targetquantity + cs_po_schd-schedulelineorderquantity.

          CLEAR: cs_po_schd.
        ENDLOOP.

        SELECT SINGLE
               ponum,
               poitem,
               SUM( challnqty ) AS challnqty
               FROM zmm_ge_data
               WHERE ponum = @ls_po_schd-schedulingagreement AND poitem = @ls_po_schd-schedulingagreementitem
                   AND gedeleted = @abap_false
               GROUP BY ponum, poitem
               INTO @ls_ge.                             "#EC CI_NOORDER


        lv_open_qty = ls_po_schd-targetquantity.
        lv_open_qty = lv_open_qty - ls_ge-challnqty.

        IF lv_open_qty GT 0.

          CLEAR: gs_data.
          gs_data-ebeln    = ls_po_schd-schedulingagreement.
          gs_data-ebelp    = ls_po_schd-schedulingagreementitem.
          gs_data-matnr    = ls_po_schd-material.
          gs_data-maktx    = ls_po_schd-productdescription.
          gs_data-doc_date = ls_po_schd-schedulelinedeliverydate.
          gs_data-poqty    = ls_po_schd-targetquantity.
          gs_data-opqty    = lv_open_qty.
          gs_data-uom      = ls_po_schd-orderquantityunit.
          gs_data-overtol  = ls_po_schd-overdelivtolrtdlmtratioinpct.
          gs_data-netprice = ls_po_schd-netpriceamount.
          gs_data-currency = ls_po_schd-orderpriceunit.
          gs_data-per      = ''.
          gs_data-hsncode          = ls_po_schd-consumptiontaxctrlcode.
          gs_data-poitemcategory   = ls_po_schd-purchasingdocumentcategory.
          gs_data-productgroup = ls_po_schd-productgroup.
          gs_data-producttype  = ls_po_schd-producttype.
          APPEND gs_data TO gt_data.

        ENDIF.

        CLEAR: ls_ge, ls_po_schd.
      ENDLOOP.
    ENDIF.

    SORT gt_data BY ebeln ebelp.
    et_data[] = gt_data[].

*    ENDIF.

  ENDMETHOD.


  METHOD save_data_get_genum.
    DATA:
      lv_billnum TYPE zmm_ge_data-billnum,
      lv_dupbill TYPE c.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20.

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

      IF lv_doc_month LT 4. "( v_month = '01' OR v_month = '02' OR v_month = '03' ).
        lv_fis_year = lv_fis_year - 1.
      ENDIF.

      READ TABLE xt_gedata INTO DATA(xs_gedata_new) INDEX 1.
      lv_billnum = xs_gedata_new-billnum.

      IF lv_billnum IS NOT INITIAL.

        SELECT * FROM zmm_ge_data WHERE billnum     = @lv_billnum AND
                                        lifnr       = @xs_gedata_new-lifnr AND
                                        gentry_year = @lv_fis_year AND
                                        gedeleted   = @abap_false
                                        INTO TABLE @DATA(bt_data).

        IF bt_data[] IS NOT INITIAL.
          lv_dupbill = abap_true.
        ENDIF.

      ENDIF.

      IF lv_dupbill EQ abap_true AND im_action = 'create'.
        CONCATENATE 'Gate entry already posted against bill number' lv_billnum INTO rv_ge_num SEPARATED BY space ##NO_TEXT.
      ELSE.

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

            ELSEIF xs_gedata_new-werks = '1002'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '02'
                  object      = 'ZGATE_NUM'
                IMPORTING
                  number      = ge_num
                  returncode  = rcode.

            ELSEIF xs_gedata_new-werks = '2001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '03'
                  object      = 'ZGATE_NUM'
                IMPORTING
                  number      = ge_num
                  returncode  = rcode.

            ELSEIF xs_gedata_new-werks = '3001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '04'
                  object      = 'ZGATE_NUM'
                IMPORTING
                  number      = ge_num
                  returncode  = rcode.

            ELSEIF xs_gedata_new-werks = '4001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '05'
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
          gs_final-token_type   = 'NORMAL'.

          LOOP AT xs_gedata-ge_item INTO DATA(xs_ge_item).
            MOVE-CORRESPONDING xs_ge_item TO gs_final.
            gs_final-ponum  = xs_ge_item-ebeln.
            gs_final-poitem = xs_ge_item-ebelp.
            APPEND gs_final TO gt_final.
          ENDLOOP.

        ENDLOOP.

        IF gt_final[] IS NOT INITIAL.

          IF im_action = 'create'.

            INSERT zmm_ge_data FROM TABLE @gt_final.
            IF sy-subrc EQ 0.
              CONCATENATE 'Gate entry number' ge_num 'generated successfully' INTO rv_ge_num SEPARATED BY space ##NO_TEXT.
            ENDIF.

          ELSEIF im_action = 'change'.

            DATA:
              lv_index TYPE sy-tabix.

            SELECT * FROM zmm_ge_data
                     WHERE gentry_num = @gs_final-gentry_num AND gedeleted = @abap_false AND mblnr NE ''
                     INTO TABLE @DATA(lt_ge_data). "#EC CI_ALL_FIELDS_NEEDED

            IF lt_ge_data[] IS NOT INITIAL.

              SELECT
        materialdocument,
        materialdocumentyear,
        documentdate,
        postingdate,
        materialdocumentheadertext,
        deliverydocument,
        referencedocument,
        billoflading,
        plant,
        materialdocumentitem,
        goodsmovementtype,
        supplier,
        purchaseorder,
        purchaseorderitem,
        material,
        entryunit,
        quantityinentryunit,
        totalgoodsmvtamtincccrcy,
        inventoryspecialstocktype,
        inventorystocktype,
        reversedmaterialdocument,
        reversedmaterialdocumentitem,
        reversedmaterialdocumentyear,
        batch,
        goodsmovementiscancelled,
        goodsrecipientname,
        unloadingpointname,
        isautomaticallycreated,
        manufacturingorder,
        reservation,
        reservationitem,
        storagelocation,
        storagebin,
        issgorrcvgbatch,
        issuingorreceivingstorageloc,
        ewmstoragebin,
        purchaseorderdate,
        orderquantity,
        netpriceamount,
        quantityindeliveryqtyunit,
        suppliername,
        country,
        addressid,
        inspectionlot,
        insplotqtytoblocked,
        insplotqtytofree,
        matldoclatestpostgdate,
        inspectionlottype,
        inspectionlotusagedecidedby,
        inspectionlotusagedecidedon,
        streetprefixname1,
        streetprefixname2,
        streetname,
        streetsuffixname1,
        districtname,
        cityname,
        postalcode,
        addressrepresentationcode,
        addresspersonid,
        region,
        supll_email,
        productdescription
              FROM zi_grn_detail
                       FOR ALL ENTRIES IN @lt_ge_data
                       WHERE reversedmaterialdocument = @lt_ge_data-mblnr AND
                             goodsmovementtype = '102'
                       INTO TABLE @DATA(gt_mdoc).  "#EC CI_NO_TRANSFORM

              LOOP AT lt_ge_data INTO DATA(ls_ge_data).
                lv_index = sy-tabix.

                READ TABLE gt_mdoc INTO DATA(wa_mkpf1)
                               WITH KEY reversedmaterialdocument = ls_ge_data-mblnr
                                               goodsmovementtype = '102'.
                IF sy-subrc EQ 0.
                  DELETE lt_ge_data INDEX lv_index.
                ENDIF.

                CLEAR: ls_ge_data.
              ENDLOOP.

            ENDIF.

            IF lt_ge_data[] IS INITIAL.

              MODIFY zmm_ge_data FROM TABLE @gt_final.
              IF sy-subrc EQ 0.
                CONCATENATE 'Gate entry number' ge_num 'updated successfully' INTO rv_ge_num SEPARATED BY space ##NO_TEXT.
              ENDIF.

            ELSE.

              CONCATENATE 'Changes not allowed, MIGO alreday posted against gate entry' ge_num INTO rv_ge_num SEPARATED BY space ##NO_TEXT.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
