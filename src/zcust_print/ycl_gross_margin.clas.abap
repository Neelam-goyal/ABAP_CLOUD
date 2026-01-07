CLASS ycl_gross_margin DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      gt_data TYPE TABLE OF zstr_gross_margin_report,
      gs_data TYPE zstr_gross_margin_report.

    METHODS:
      get_report_data
        IMPORTING
                  im_input_str   TYPE string
        RETURNING VALUE(et_data) LIKE gt_data.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_GROSS_MARGIN IMPLEMENTATION.


  METHOD get_report_data.

    DATA:
      lv_net_value       TYPE p LENGTH 16 DECIMALS 2,
      lv_sale_unit_price TYPE p LENGTH 16 DECIMALS 2,
      lv_value_foh       TYPE p LENGTH 16 DECIMALS 2,
      lv_value_voh       TYPE p LENGTH 16 DECIMALS 2,
      lv_gm_perct        TYPE p LENGTH 16 DECIMALS 2,
      lv_gm_value        TYPE p LENGTH 16 DECIMALS 2.

    TYPES:
      BEGIN OF gty_kunnr,
        kunnr_low TYPE c LENGTH 10,
      END OF gty_kunnr,

      BEGIN OF gty_matnr,
        matnr_low TYPE c LENGTH 40,
      END OF gty_matnr,

      BEGIN OF gty_werks,
        werks_low TYPE c LENGTH 4,
      END OF gty_werks.

*           BEGIN OF gty_budat,
*             budat_low TYPE c LENGTH 10,
*           END OF gty_budat,

    DATA:
      gt_kunnr TYPE TABLE OF gty_kunnr,
      gt_matnr TYPE TABLE OF gty_matnr,
      gt_werks TYPE TABLE OF gty_werks,
      gs_kunnr TYPE gty_kunnr,
      gs_matnr TYPE gty_matnr,
      gs_werks TYPE gty_werks.

*      gt_budat TYPE TABLE OF gty_budat,
*      gs_budat TYPE gty_budat,

    TYPES: BEGIN OF gty_input,
             budat_low  TYPE c LENGTH 10, "LIKE gt_budat,
             budat_high TYPE c LENGTH 10,
             kunnr_low  LIKE gt_kunnr,
             matnr_low  LIKE gt_matnr,
             werks_low  LIKE gt_werks,
           END OF gty_input.

    DATA:
      gt_input TYPE TABLE OF gty_input,
      gs_input TYPE gty_input.

    TYPES: BEGIN OF gty_obd,
             OutboundDelivery TYPE I_JournalEntry-DocumentReferenceID,
           END OF gty_obd.

    DATA:
      gt_obd TYPE TABLE OF gty_obd,
      gs_obd TYPE gty_obd.


    DATA :
      r_budat   TYPE RANGE OF zi_dc_note-PostingDate,
      rs_budat  LIKE LINE OF  r_budat,

      r_kunnr   TYPE RANGE OF I_Customer-Customer,
      rs_kunnr  LIKE LINE OF r_kunnr,

      r_matnr   TYPE RANGE OF I_Product-Product,
      rs_matnr  LIKE LINE OF r_matnr,

      r_werks   TYPE RANGE OF zi_dc_note-Plant,
      rs_werks  LIKE LINE OF r_werks,

      r_gl_acc  TYPE RANGE OF I_JournalEntryItem-GLAccount,
      rs_gl_acc LIKE LINE OF r_gl_acc.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sy_uname     TYPE c LENGTH 20.

    DATA:
      lv_gl_cost     TYPE I_JournalEntryItem-AmountInCompanyCodeCurrency,
      lv_gl_cost_sum TYPE I_JournalEntryItem-AmountInCompanyCodeCurrency,
      sum_gl_cost    TYPE I_JournalEntryItem-AmountInCompanyCodeCurrency.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gt_input
                                 ).

    READ TABLE gt_input INTO gs_input INDEX 1.
    CLEAR: r_budat, r_kunnr, r_werks, r_matnr.

    ""***Preparing range for posting date
    IF gs_input-budat_high IS INITIAL.

      rs_budat-low     = gs_input-budat_low+6(4) && gs_input-budat_low+3(2)  && gs_input-budat_low+0(2).
      "*rs_budat-high    = '' .
      rs_budat-option  = 'EQ' .
      rs_budat-sign    = 'I' .
      APPEND rs_budat TO r_budat.

    ELSE.

      rs_budat-low     = gs_input-budat_low+6(4) && gs_input-budat_low+3(2)  && gs_input-budat_low+0(2).
      rs_budat-high    = gs_input-budat_high+6(4) && gs_input-budat_high+3(2)  && gs_input-budat_high+0(2).
      rs_budat-option  = 'BT'.
      rs_budat-sign    = 'I'.
      APPEND rs_budat TO r_budat.

    ENDIF.

    ""***Preparing range for customer code
    LOOP AT gs_input-kunnr_low INTO DATA(ls_kunnr).

      rs_kunnr-low     = ls_kunnr-kunnr_low.
      rs_kunnr-high    = '' .
      rs_kunnr-option  = 'EQ' .
      rs_kunnr-sign    = 'I' .
      APPEND rs_kunnr TO r_kunnr.

      CLEAR: ls_kunnr.
    ENDLOOP.

    ""***Preparing range for material code
    LOOP AT gs_input-matnr_low INTO DATA(ls_matnr).

      rs_matnr-low     = ls_matnr-matnr_low.
      rs_matnr-high    = '' .
      rs_matnr-option  = 'EQ' .
      rs_matnr-sign    = 'I' .
      APPEND rs_matnr TO r_matnr.

      CLEAR: ls_matnr.
    ENDLOOP.

    ""***Preparing range for plant
    LOOP AT gs_input-werks_low INTO DATA(ls_werks).

      rs_werks-low     = ls_werks-werks_low.
      rs_werks-high    = '' .
      rs_werks-option  = 'EQ' .
      rs_werks-sign    = 'I' .
      APPEND rs_werks TO r_werks.

      CLEAR: ls_werks.
    ENDLOOP.

    SELECT * FROM zi_sale_reg  WHERE BillingDocumentDate IN @r_budat
                                AND SoldToParty IN @r_kunnr
                                AND Plant IN @r_werks
                                AND product IN @r_matnr
*                                AND SalesDocument IN ( '0010000077' )
*                                AND BillingDocument IN ( '3241000014' )
                                INTO TABLE @DATA(it_sale).

    IF it_sale[] IS NOT INITIAL.

      SELECT
      OutboundDelivery,
      OutboundDeliveryItem,
      DeliveryDocumentItemCategory,
      Material,
      MaterialTypePrimary,
      MaterialGroup,
      ProductGroup,
      Batch,
      HigherLvlItmOfBatSpltItm,
      ActualDeliveryQuantity,
      OriginalDeliveryQuantity,
      DeliveryQuantityUnit
      FROM I_OutboundDeliveryItem
      FOR ALL ENTRIES IN @it_sale
      WHERE OutboundDelivery = @it_sale-DeliveryDocument
      INTO TABLE @DATA(lt_obdel). "#EC CI_NO_TRANSFORM

if lt_obdel[] is NOT INITIAL.

      SELECT
      GoodsMovement,
      GoodsMovementYear,
      GoodsMovementItem,
      ManufacturingOrder,
      Batch,
      GoodsMovementType,
      BaseUnit,
      GoodsMovementRefDocType,
      PostingDate,
      QuantityInBaseUnit
      FROM I_MfgOrderDocdGoodsMovement
      FOR ALL ENTRIES IN @lt_obdel
      WHERE Batch = @lt_obdel-Batch AND
            GoodsMovementType = '101' AND
            GoodsMovementRefDocType = 'F'
      INTO TABLE @DATA(it_mfg).  "#EC CI_NO_TRANSFORM

endif.

if it_mfg[] is NOT INITIAL.

      SELECT
      MfgOrderConfirmationGroup,
      MfgOrderConfirmation,
      ManufacturingOrder,
      MaterialDocument,
      MaterialDocumentYear,
      PostingDate
      FROM I_MfgOrderConfirmation
      FOR ALL ENTRIES IN @it_mfg
      WHERE ManufacturingOrder = @it_mfg-ManufacturingOrder AND
            PostingDate        = @it_mfg-PostingDate
      INTO TABLE @DATA(lt_conf). "#EC CI_NO_TRANSFORM

endif.

*      SELECT
*      mfg~GoodsMovement,
*      mfg~GoodsMovementYear,
*      mfg~GoodsMovementItem,
*      mfg~ManufacturingOrder,
*      mfg~Batch,
*      mfg~GoodsMovementType,
*      mfg~BaseUnit,
*      mfg~GoodsMovementRefDocType,
*      mfg~PostingDate,
*      mfg~QuantityInBaseUnit,
*      conf~MfgOrderConfirmationGroup,
*      conf~MfgOrderConfirmation,
*      conf~MaterialDocument,
*      conf~MaterialDocumentYear
*      FROM I_MfgOrderDocdGoodsMovement AS mfg
*      INNER JOIN I_MfgOrderConfirmation AS conf
*      ON conf~ManufacturingOrder = mfg~ManufacturingOrder AND
*      conf~postingDate           = mfg~PostingDate
*      "conf~milestoneconfirmationtype = @abap_true
*      FOR ALL ENTRIES IN @lt_obdel
*      WHERE mfg~GoodsMovementType = '101' AND
*            mfg~GoodsMovementRefDocType = 'F' AND
*            mfg~Batch = @lt_obdel-Batch
*      INTO TABLE @DATA(it_mfg_conf).            "#EC CI_FAE_NO_LINES_OK

*      IF it_mfg_conf[] IS NOT INITIAL.
*
*        SORT it_mfg_conf BY GoodsMovement
*                            GoodsMovementYear
*                            Batch
*                            ManufacturingOrder
*                            MfgOrderConfirmationGroup
*                            MfgOrderConfirmation.
*
*        DELETE ADJACENT DUPLICATES FROM it_mfg_conf COMPARING
*                                                    GoodsMovement
*                                                    GoodsMovementYear
*                                                    Batch
*                                                    ManufacturingOrder
*                                                    MfgOrderConfirmationGroup
*                                                    MfgOrderConfirmation.
*
*      ENDIF.

*      SELECT
*      ConfirmationGroup,
*      ConfirmationCount,
*      MaterialDocument,
*      MaterialDocumentYear,
*      MaterialDocumentItem
*      FROM I_OrderConfMatlDocItemAssgmt
*      FOR ALL ENTRIES IN @lt_conf
*      WHERE ConfirmationGroup = @lt_conf-MfgOrderConfirmationGroup AND
*            ConfirmationCount = @lt_conf-MfgOrderConfirmation
*      INTO TABLE @DATA(gt_conf).

if lt_conf[] is NOT INITIAL.

      SELECT
      OrderID,
      CompanyCode,
      FiscalYear,
      AccountingDocument,
      LedgerGLLineItem,
      AmountInCompanyCodeCurrency,
      PostingDate,
      DocumentDate,
      GLAccount,
      Product,
      Quantity,
      ReferenceDocumentType,
      ReferenceDocumentContext,
      ReferenceDocument,
      ReferenceDocumentItem,
      ReferenceDocumentItemGroup
      FROM I_JournalEntryItem
      FOR ALL ENTRIES IN @lt_conf
      WHERE OrderID           = @lt_conf-ManufacturingOrder AND
            PostingDate       = @lt_conf-PostingDate AND
            Ledger  = '0L'
      INTO TABLE @DATA(lt_gl) . "#EC CI_NO_TRANSFORM

ENDIF.

      IF lt_obdel[] IS NOT INITIAL.
        gt_obd[] = CORRESPONDING #( lt_obdel[] ).
      ENDIF.

if gt_obd[] is NOT INITIAL.

      SELECT
      CompanyCode,
      FiscalYear,
      AccountingDocument,
      DocumentReferenceID
      FROM I_JournalEntry
      FOR ALL ENTRIES IN @gt_obd
      WHERE DocumentReferenceID = @gt_obd-OutboundDelivery
      INTO TABLE @DATA(lt_bkpf) . "#EC CI_NO_TRANSFORM

endif.

if lt_obdel[] is NOT INITIAL.

      SELECT
      Product,
      ProductGroup,
      ExternalProductGroup
      FROM I_product
      FOR ALL ENTRIES IN @lt_obdel
      WHERE Product = @lt_obdel-Material
      INTO TABLE @DATA(lt_product). "#EC CI_NO_TRANSFORM

endif.

    ENDIF.

    DATA:
      lv_mfg_rder     TYPE I_JournalEntryItem-OrderID,
      lv_conf_group   TYPE I_JournalEntryItem-ReferenceDocument,
      lv_conf_countr  TYPE I_JournalEntryItem-ReferenceDocumentContext,
      lv_qty          TYPE I_OutboundDeliveryItem-ActualDeliveryQuantity,
      lv_conf_group1  TYPE I_JournalEntryItem-ReferenceDocument,
      lv_conf_countr1 TYPE I_JournalEntryItem-ReferenceDocumentContext,
      sum_mfgodr_qty  TYPE I_MfgOrderDocdGoodsMovement-QuantityInBaseUnit,
      lv_amount_neg   TYPE c LENGTH 20,
      lv_amt_neg      TYPE p LENGTH 16 DECIMALS 2,
      lv_dc_ref_id    TYPE I_JournalEntry-DocumentReferenceID.


    CLEAR: r_gl_acc.
    rs_gl_acc-low     = '0004000003'.
    rs_gl_acc-high    = '0004000021'.
    rs_gl_acc-option  = 'BT'.
    rs_gl_acc-sign    = 'I'.
    APPEND rs_gl_acc TO r_gl_acc.

    LOOP AT it_sale INTO DATA(ls_sale).

      CLEAR: lv_net_value, lv_sale_unit_price.

      gs_data-sap_inv_num       = ls_sale-BillingDocument.
      gs_data-tax_inv_num       = ls_sale-DocumentReferenceID.

      gs_data-tax_inv_date      = ls_sale-BillingDocumentDate+6(2) && '.' &&
                                  ls_sale-BillingDocumentDate+4(2) && '.' &&
                                  ls_sale-BillingDocumentDate+0(4).

      gs_data-plant             = ls_sale-Plant.
      gs_data-sold_to_party     = ls_sale-sold_to_party.
      gs_data-sold_to_name      = ls_sale-ag_name.
      gs_data-saled_ocument     = ls_sale-SalesDocument.
      gs_data-dist_channel      = ls_sale-DistributionChannel.
      gs_data-product           = ls_sale-product.
      gs_data-item_desc         = ls_sale-BillingDocumentItemText.

*      gs_data-latest_deliv_date = ls_sale-F2BillingDocumentDate+6(2) && '.' &&
*                                  ls_sale-F2BillingDocumentDate+4(2) && '.' &&
*                                  ls_sale-F2BillingDocumentDate+0(4).

      gs_data-sale_qty          = ls_sale-BillingQuantity.

      lv_net_value              = ls_sale-item_assessableamount_inr.
      lv_sale_unit_price        = ls_sale-item_unitprice.
      "*gs_data-net_value         = lv_net_value.
      gs_data-sale_unit_price   = lv_sale_unit_price.

      gs_data-currency          = ls_sale-TransactionCurrency.
      gs_data-shop_sale_value   = ''.
      gs_data-profit_center     = ls_sale-ProfitCenter.
      gs_data-delivery_no       = ls_sale-DeliveryDocument.
      gs_data-Delivery_no_item  = ls_sale-DeliveryDocumentItem.
      gs_data-gl_accout         = ''.

      LOOP AT lt_obdel INTO DATA(ls_obdel) WHERE OutboundDelivery         = ls_sale-DeliveryDocument AND
                                                 HigherLvlItmOfBatSpltItm = ls_sale-DeliveryDocumentItem.

        lv_qty                    = ls_obdel-ActualDeliveryQuantity.
        gs_data-BatchQty          = lv_qty.

        if ls_sale-BillingQuantity gt 0.
         gs_data-net_value         = ( lv_net_value / ls_sale-BillingQuantity ) * lv_qty.
         gs_data-net_value_foreign = ( ls_sale-item_assessableamount / ls_sale-BillingQuantity ) * lv_qty.
        ENDIF.

        gs_data-product_type      = ls_obdel-MaterialTypePrimary.
        gs_data-batch_no          = ls_obdel-Batch.

        READ TABLE lt_product INTO DATA(ls_product) WITH KEY product = ls_obdel-Material.
        IF sy-subrc EQ 0.
          gs_data-product_family    = ls_product-ExternalProductGroup. "ls_obdel-ProductGroup.
        ENDIF.

        CLEAR:
          lv_mfg_rder,
          lv_conf_group,
          lv_conf_countr.

        READ TABLE it_mfg INTO DATA(ls_mfg) WITH KEY batch = ls_obdel-Batch.
        IF sy-subrc EQ 0.

          gs_data-sho_ord_qty       = ls_mfg-QuantityInBaseUnit.
          gs_data-shop_ord_qty_uom  = ls_mfg-BaseUnit.
          gs_data-shop_order_no     = ls_mfg-ManufacturingOrder.

          READ TABLE it_mfg INTO DATA(ls_conf) WITH KEY ManufacturingOrder     = ls_mfg-ManufacturingOrder
                                                                 PostingDate   = ls_mfg-PostingDate
                                                                 batch         = ls_obdel-Batch.

          IF sy-subrc EQ 0.
            lv_mfg_rder      = ls_conf-ManufacturingOrder.
            lv_conf_group1   = ls_conf-GoodsMovement.
          ENDIF.

        ELSE.

          ls_mfg-QuantityInBaseUnit = 1.

        ENDIF.

        CLEAR: lv_dc_ref_id.
        lv_dc_ref_id = ls_obdel-OutboundDelivery.

        READ TABLE lt_bkpf INTO DATA(ls_bkpf) WITH KEY DocumentReferenceID = lv_dc_ref_id.
        IF sy-subrc EQ 0.
          gs_data-compcode           = ls_bkpf-CompanyCode.
          gs_data-fiscalyear         = ls_bkpf-FiscalYear.
          gs_data-accountingdocument = ls_bkpf-AccountingDocument.
        ENDIF.


        CLEAR: sum_mfgodr_qty.
        LOOP AT it_mfg INTO DATA(ls_mfg_news) WHERE ManufacturingOrder  = ls_mfg-ManufacturingOrder.
          sum_mfgodr_qty = sum_mfgodr_qty + ls_mfg_news-QuantityInBaseUnit.
          CLEAR: ls_mfg_news.
        ENDLOOP.

        LOOP AT lt_gl INTO DATA(ls_gl) WHERE OrderID = ls_mfg-ManufacturingOrder.


          """*******************Case-2
          IF ls_gl-GLAccount = '0004000001'.

            IF sum_mfgodr_qty GT 0.
              lv_gl_cost = ( ls_gl-AmountInCompanyCodeCurrency / sum_mfgodr_qty ) * lv_qty.
            ENDIF.
            gs_data-raw_material_cost = gs_data-raw_material_cost + lv_gl_cost.
            sum_gl_cost = sum_gl_cost + lv_gl_cost.

          ELSEIF ls_gl-GLAccount = '0004000002'.

            IF sum_mfgodr_qty GT 0.
              lv_gl_cost = ( ls_gl-AmountInCompanyCodeCurrency / sum_mfgodr_qty ) * lv_qty.
            ENDIF.
            gs_data-bop_cost = gs_data-bop_cost + lv_gl_cost.
            sum_gl_cost = sum_gl_cost + lv_gl_cost.

          ELSEIF  ls_gl-GLAccount = '0004020001'.

            IF sum_mfgodr_qty GT 0.
              lv_gl_cost = ( ls_gl-AmountInCompanyCodeCurrency / sum_mfgodr_qty ) * lv_qty.
            ENDIF.
            gs_data-job_work_cost = gs_data-job_work_cost + lv_gl_cost.
            sum_gl_cost = sum_gl_cost + lv_gl_cost.

          ELSEIF ls_gl-GLAccount = '0004743002'.

            IF sum_mfgodr_qty GT 0.
              lv_gl_cost = ( ls_gl-AmountInCompanyCodeCurrency / sum_mfgodr_qty ) * lv_qty.
            ENDIF.
            gs_data-labour_cost = gs_data-labour_cost + lv_gl_cost.
            sum_gl_cost = sum_gl_cost + lv_gl_cost.

          ELSEIF  ls_gl-GLAccount = '0004743001'.

            IF sum_mfgodr_qty GT 0.
              lv_gl_cost = ( ls_gl-AmountInCompanyCodeCurrency / sum_mfgodr_qty ) * lv_qty.
            ENDIF.
            gs_data-reapir_manit = gs_data-reapir_manit + lv_gl_cost.
            sum_gl_cost = sum_gl_cost + lv_gl_cost.

          ELSEIF  ls_gl-GLAccount = '0004743003'.

            IF sum_mfgodr_qty GT 0.
              lv_gl_cost = ( ls_gl-AmountInCompanyCodeCurrency / sum_mfgodr_qty ) * lv_qty.
            ENDIF.
            gs_data-power_cost = gs_data-power_cost + lv_gl_cost.
            sum_gl_cost = sum_gl_cost + lv_gl_cost.

          ELSEIF  ls_gl-GLAccount = '0004743004'.

            IF sum_mfgodr_qty GT 0.
              lv_gl_cost = ( ls_gl-AmountInCompanyCodeCurrency / sum_mfgodr_qty ) * lv_qty.
            ENDIF.
            gs_data-consumable_cost = gs_data-consumable_cost + lv_gl_cost.
            sum_gl_cost = sum_gl_cost + lv_gl_cost.

          ELSEIF  ls_gl-GLAccount = '0004743005'.

            IF sum_mfgodr_qty GT 0.
              lv_gl_cost = ( ls_gl-AmountInCompanyCodeCurrency / sum_mfgodr_qty ) * lv_qty.
            ENDIF.
            gs_data-depriciation_cost = gs_data-depriciation_cost + lv_gl_cost.
            sum_gl_cost = sum_gl_cost + lv_gl_cost.

          ELSEIF  ls_gl-GLAccount = '0004743006'.

            IF sum_mfgodr_qty GT 0.
              lv_gl_cost = ( ls_gl-AmountInCompanyCodeCurrency / sum_mfgodr_qty ) * lv_qty.
            ENDIF.
            gs_data-setup_cost = gs_data-setup_cost + lv_gl_cost.
            sum_gl_cost = sum_gl_cost + lv_gl_cost.

          ELSEIF  ls_gl-GLAccount = '0004741002'.

            IF sum_mfgodr_qty GT 0.
              lv_gl_cost = ( ls_gl-AmountInCompanyCodeCurrency / sum_mfgodr_qty ) * lv_qty.
            ENDIF.
            gs_data-warranty_voh = gs_data-warranty_voh + lv_gl_cost.
            sum_gl_cost = sum_gl_cost + lv_gl_cost.

          ELSEIF  ls_gl-GLAccount = '0004741001'.

            IF sum_mfgodr_qty GT 0.
              lv_gl_cost = ( ls_gl-AmountInCompanyCodeCurrency / sum_mfgodr_qty ) * lv_qty.
            ENDIF.
            gs_data-foh = gs_data-foh + lv_gl_cost.
            sum_gl_cost = sum_gl_cost + lv_gl_cost.

          ENDIF.

          """*******************Case-2
          IF ls_gl-Product = ls_obdel-Material.
            "ls_gl-OrderID             = lv_mfg_rder AND
            "ls_gl-ReferenceDocument   = lv_conf_group1 AND

            CLEAR: lv_amount_neg, lv_amt_neg.
            lv_amount_neg = ls_gl-AmountInCompanyCodeCurrency.
            CONDENSE lv_amount_neg.
            IF lv_amount_neg CA '-'.
              lv_amt_neg = ls_gl-AmountInCompanyCodeCurrency * -1.
            ELSE.
              lv_amt_neg = ls_gl-AmountInCompanyCodeCurrency.
            ENDIF.

            IF sum_mfgodr_qty GT 0.
              lv_gl_cost = ( lv_amt_neg / sum_mfgodr_qty ) * lv_qty.
            ENDIF.
            gs_data-cogmAmount = gs_data-cogmAmount + lv_gl_cost.
            "sum_gl_cost = sum_gl_cost + lv_gl_cost.

          ENDIF.

          CLEAR: ls_gl.
        ENDLOOP.

        """*******************Case-3
        IF ls_mfg-ManufacturingOrder IS NOT INITIAL.

          SELECT SINGLE
          SUM( AmountInCompanyCodeCurrency )
          FROM I_JournalEntryItem
          WHERE OrderID           = @ls_mfg-ManufacturingOrder AND
                ReferenceDocument = @ls_mfg-GoodsMovement AND
                GLAccount IN ( '0004000070', '0004000355' ) AND
                Ledger = '0L'
          INTO @DATA(lv_inh_cost1).

          IF ls_mfg-QuantityInBaseUnit GT 0.
            lv_gl_cost = ( lv_inh_cost1 / ls_mfg-QuantityInBaseUnit ) * lv_qty.
          ENDIF.
          gs_data-InhousePartCost = gs_data-InhousePartCost + lv_gl_cost.
          sum_gl_cost = sum_gl_cost + lv_gl_cost.

          SELECT SINGLE
          SUM( AmountInCompanyCodeCurrency )
          FROM I_JournalEntryItem
          WHERE OrderID           = @ls_mfg-ManufacturingOrder AND
                ReferenceDocument = @ls_mfg-GoodsMovement AND
                GLAccount IN @r_gl_acc AND
                Ledger = '0L'
          INTO @DATA(lv_inh_cost2).

          IF ls_mfg-QuantityInBaseUnit GT 0.
            lv_gl_cost = ( lv_inh_cost2 / ls_mfg-QuantityInBaseUnit ) * lv_qty.
          ENDIF.
          gs_data-InhousePartCost = gs_data-InhousePartCost + lv_gl_cost.
          sum_gl_cost = sum_gl_cost + lv_gl_cost.

        ENDIF.

        gs_data-PrdConfqty        = ''.
        gs_data-mat_cost_total    = ''.

        IF gs_data-plant = '1001'.

          lv_value_foh = ( sum_gl_cost * (  116 / 10 ) ) / 100.
          gs_data-foh = lv_value_foh.

        ELSEIF gs_data-plant = '1002'.

          lv_value_foh = ( sum_gl_cost * (  92 / 10 ) ) / 100.
          gs_data-foh = lv_value_foh.

        ELSEIF gs_data-plant = '1003'.

          lv_value_foh = ( sum_gl_cost * (  92 / 10 ) ) / 100.
          gs_data-foh = lv_value_foh.

        ENDIF.

        IF gs_data-plant = '1001'.

          lv_value_voh = ( sum_gl_cost * (  137 / 100 ) ) / 100.
          gs_data-warranty_voh = lv_value_voh.

        ELSEIF gs_data-plant = '1002'.

          lv_value_voh = ( sum_gl_cost * (  23 / 100 ) ) / 100.
          gs_data-warranty_voh = lv_value_voh.

        ELSEIF gs_data-plant = '1003'.

          lv_value_voh = ( sum_gl_cost * (  23 / 100 ) ) / 100.
          gs_data-warranty_voh = lv_value_voh.

        ENDIF.

        gs_data-prod_costtotal    = sum_gl_cost + lv_value_foh + lv_value_voh.

        lv_gm_value      = gs_data-net_value - gs_data-prod_costtotal. "sum_gl_cost.
        gs_data-gm_value = lv_gm_value.

        CONDENSE gs_data-net_value.
        IF gs_data-net_value IS NOT INITIAL.
          lv_gm_perct        = ( gs_data-gm_value / gs_data-net_value ) * 100.
          gs_data-final_per  = lv_gm_perct.
        ENDIF.

        CONDENSE:
        gs_data-sap_inv_num,
        gs_data-tax_inv_num,
        gs_data-tax_inv_date,
        gs_data-plant,
        gs_data-sold_to_party ,
        gs_data-sold_to_name,
        gs_data-saled_ocument,
        gs_data-dist_channel,
        gs_data-product,
        gs_data-item_desc,
        gs_data-latest_deliv_date,
        gs_data-sale_qty,
        gs_data-net_value,
        gs_data-sale_unit_price,
        gs_data-currency,
        gs_data-shop_sale_value,
        gs_data-profit_center,
        gs_data-delivery_no,
        gs_data-delivery_no_item,
        gs_data-gl_accout,
        gs_data-product_type,
        gs_data-product_family,
        gs_data-batch_no,
        gs_data-sho_ord_qty,
        gs_data-shop_ord_qty_uom,
        gs_data-shop_order_no,
        gs_data-raw_material_cost,
        gs_data-bop_cost,
        gs_data-job_work_cost,
        gs_data-mat_cost_total,
        gs_data-labour_cost,
        gs_data-reapir_manit,
        gs_data-power_cost ,
        gs_data-consumable_cost,
        gs_data-depriciation_cost ,
        gs_data-setup_cost ,
        gs_data-warranty_voh,
        gs_data-foh      ,
        gs_data-prod_costtotal ,
        gs_data-gm_value ,
        gs_data-batchqty ,
        gs_data-cogmamount,
        gs_data-prdconfqty ,
        gs_data-inhousepartcost,
        gs_data-final_per.

        APPEND gs_data TO gt_data.

        CLEAR:
        gs_data-BatchQty,
        gs_data-product_type,
        gs_data-product_family,
        gs_data-batch_no,
        gs_data-sho_ord_qty,
        gs_data-shop_ord_qty_uom,
        gs_data-shop_order_no,
        gs_data-raw_material_cost,
        gs_data-bop_cost,
        gs_data-job_work_cost,
        gs_data-labour_cost,
        gs_data-reapir_manit,
        gs_data-power_cost,
        gs_data-consumable_cost,
        gs_data-depriciation_cost,
        gs_data-setup_cost,
        gs_data-warranty_voh,
        gs_data-foh,
        gs_data-cogmAmount,
        gs_data-InhousePartCost,
        gs_data-PrdConfqty,
        gs_data-mat_cost_total,
        gs_data-prod_costtotal,
        gs_data-gm_value,
        gs_data-final_per.

        CLEAR: ls_obdel, ls_mfg, ls_conf, lv_qty, lv_inh_cost1, lv_inh_cost2, sum_gl_cost. "gs_data,
        CLEAR: ls_product, ls_bkpf, lv_value_foh, lv_value_voh.
      ENDLOOP.

      CLEAR: ls_sale.
    ENDLOOP.

    IF gt_data[] IS NOT INITIAL.
      et_data[] = gt_data[].
    ENDIF.

  ENDMETHOD.
ENDCLASS.
