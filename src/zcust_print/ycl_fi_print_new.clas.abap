CLASS ycl_fi_print_new  DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .


  PUBLIC SECTION.
    DATA:
      gt_final TYPE TABLE OF zstr_voucher_print,
      gs_final TYPE zstr_voucher_print, ""#EC CI_NOORDER
      lt_item  TYPE TABLE OF zstr_voucher_print_item,
      ls_item  TYPE zstr_voucher_print_item,
      lt_qlty  TYPE TABLE OF zstr_scn_qlty_data,
      ls_qlty  TYPE zstr_scn_qlty_data.

    DATA:
      ht_final TYPE TABLE OF zstr_chq_hdr,
      hs_final TYPE zstr_chq_hdr,
      st_item  TYPE TABLE OF zstr_chq_item,
      sw_item  TYPE zstr_chq_item.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sy_uname     TYPE c LENGTH 20.

    DATA:
      lv_char10  TYPE c LENGTH 10,
      lv_char120 TYPE c LENGTH 120,
      lv_char4   TYPE c LENGTH 4.

    METHODS:
      get_voucher_data
        IMPORTING
                  im_bukrs        LIKE lv_char4
                  im_belnr        LIKE lv_char10
                  im_gjahr        TYPE zi_dc_note-fiscalyear
                  im_action       LIKE lv_char10
        RETURNING VALUE(et_final) LIKE gt_final,

      prep_xml_voucher_print
        IMPORTING
                  it_final             LIKE gt_final
                  iv_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string,

      get_chqslip_data
        IMPORTING
                  im_bukrs        LIKE lv_char4
                  im_belnr        LIKE lv_char10
                  im_gjahr        TYPE zi_dc_note-fiscalyear
                  im_action       LIKE lv_char10
        RETURNING VALUE(et_final) LIKE ht_final,

      prep_xml_chqslip_print
        IMPORTING
                  it_final             LIKE ht_final
                  iv_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string,

      get_scn_data
        IMPORTING
                  im_bukrs        LIKE lv_char4
                  im_belnr        LIKE lv_char10
                  im_gjahr        TYPE zi_dc_note-fiscalyear
                  im_action       LIKE lv_char10
        RETURNING VALUE(et_final) LIKE gt_final,

      prep_xml_scn_print
        IMPORTING
                  it_final             LIKE gt_final
                  iv_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_FI_PRINT_NEW IMPLEMENTATION.


  METHOD get_chqslip_data.
    "data : cn type char100.
    DATA : cn TYPE c LENGTH 100 .
    SELECT a~accountingdocument, a~companycode, a~fiscalyear, a~supplier,
           a~financialaccounttype, a~amountincompanycodecurrency,
           b~bankaccount, b~businesspartner,  c~suppliername
           FROM  i_operationalacctgdocitem AS a
           LEFT OUTER JOIN i_businesspartnerbank AS b
           ON a~supplier = b~businesspartner
           LEFT OUTER JOIN i_supplier AS c
           ON a~supplier = c~supplier
           WHERE
           a~financialaccounttype = 'K'
           AND a~accountingdocument = @im_belnr AND
           a~companycode = @im_bukrs AND
           a~fiscalyear   = @im_gjahr
            INTO TABLE @DATA(i_oper).

    READ TABLE i_oper INTO DATA(ww_oper) INDEX 1.       "#EC CI_NOORDER

    "    hs_final-chequeslip = 'Cheque'.
    hs_final-companycode = ww_oper-companycode .
    hs_final-paymentdocument = ww_oper-accountingdocument .
    hs_final-fiscalyear  = ww_oper-fiscalyear .

    IF hs_final-companycode = '1000' .
      hs_final-cc_name =  'BHAGWATI LACTO VEGETARIAN EXPORTS PRIVATE LIMITED' .
    ELSEIF hs_final-companycode = '2000' .
      hs_final-cc_name = 'BHAGWATI LACTO FOODS PRIVATE LIMITED'.
    ELSEIF hs_final-companycode  = '3000' .
      hs_final-cc_name  = 'HEALTHY HARVESTED FOODS PRIVATE LIMITED'.
    ELSEIF hs_final-companycode  = '4000' .
      hs_final-cc_name  = 'BHAGWATI AGROCHEM PRIVATE LIMITED'.
    ENDIF.


    "DATA : srno  TYPE char2.
    DATA : srno  TYPE c LENGTH 2.
    "      srno = 1.

    LOOP AT i_oper INTO DATA(w_oper).

      srno = srno + 1.

      sw_item-serialno  = srno.
      sw_item-accountno  = w_oper-bankaccount .
      sw_item-amount    = w_oper-amountincompanycodecurrency .
      sw_item-partyname = w_oper-suppliername .

      APPEND sw_item TO st_item .
      CLEAR : sw_item .
    ENDLOOP .

    INSERT LINES OF st_item INTO TABLE hs_final-gt_item.
    APPEND hs_final TO et_final .

  ENDMETHOD.


  METHOD get_scn_data.
    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    DATA : lv_bardana   TYPE p LENGTH 13 DECIMALS 3,
           lv_deduction TYPE p LENGTH 13 DECIMALS 3.

    SELECT FROM i_operationalacctgdocitem AS a
    LEFT JOIN   i_supplier                AS b
    ON b~supplier EQ a~supplier
    FIELDS DISTINCT
    a~accountingdocument, a~accountingdocumentitem, a~companycode, a~fiscalyear, a~plant, a~supplier,
    a~purchasingdocument, a~purchasingdocumentitem, a~product, a~amountincompanycodecurrency,
    a~transactiontypedetermination, a~glaccount, a~invoicereference, a~invoicereferencefiscalyear,
    a~financialaccounttype,
    b~supplierfullname, b~streetname, b~cityname, b~postalcode
    WHERE a~companycode        EQ @im_bukrs
    AND   a~accountingdocument EQ @im_belnr
    AND   a~fiscalyear         EQ @im_gjahr
    INTO TABLE @DATA(i_oper).

    SELECT companycode, accountingdocument, fiscalyear, transactiontypedetermination,
           debitcreditcode, taxbaseamountincocodecrcy, amountincompanycodecurrency
    FROM i_operationalacctgdocitem WHERE companycode        EQ @im_bukrs
                                   AND   accountingdocument EQ @im_belnr
                                   AND   fiscalyear         EQ @im_gjahr
    INTO TABLE @DATA(it_tax).

    SELECT SINGLE accountingdocument, documentreferenceid
    FROM i_journalentry WHERE accountingdocument EQ @im_belnr
                        AND companycode EQ @im_bukrs AND fiscalyear EQ @im_gjahr
                        INTO @DATA(wa_jour).
    IF wa_jour IS NOT INITIAL.
      SELECT * FROM zi_token_ge
      WHERE invoice_no EQ @wa_jour-documentreferenceid
      INTO TABLE @DATA(it_token_data).

      SELECT FROM i_materialdocumentheader_2 AS a
      INNER JOIN  i_materialdocumentitem_2   AS b ON b~materialdocument EQ a~materialdocument
      LEFT JOIN   i_inspectionlot            AS c ON c~materialdocument EQ b~materialdocument
                                             AND     c~purchasingdocument EQ b~purchaseorder
                                             AND     c~purchasingdocumentitem EQ b~purchaseorderitem

***boc by neelam
******************      inner join i_journalentryitem as d on d~PurchasingDocument = c~PurchasingDocument
******************      and d~PurchasingDocumentItem =  c~purchasingdocumentitem
******************      and d~Ledger = '0L'
***boc by neelam
      FIELDS DISTINCT a~referencedocument, b~materialdocument, b~materialdocumentitem,
                      c~purchasingdocument, c~purchasingdocumentitem, c~inspectionlot
*****                      d~AccountingDocument "added by neelam
      WHERE a~referencedocument EQ @wa_jour-documentreferenceid
      AND   a~materialdocumentyear EQ @im_gjahr
***      and   d~AccountingDocument eq @im_belnr " added by neelam
      INTO TABLE @DATA(it_insp_data).

      IF it_insp_data IS NOT INITIAL.
        SELECT FROM i_inspectioncharacteristic AS a
        LEFT OUTER JOIN i_inspectionresult AS b ON b~inspectionlot EQ a~inspectionlot
        FIELDS DISTINCT a~inspectionlot, a~inspectioncharacteristictext, a~inspspectargetvalue,
                        b~inspectionresultoriginalvalue
        FOR ALL ENTRIES IN @it_insp_data
        WHERE a~inspectionlot EQ @it_insp_data-inspectionlot
        AND b~inspectioncharacteristic = a~inspectioncharacteristic " added by neelam
        INTO TABLE @DATA(it_qlty_data).            "#EC CI_NO_TRANSFORM
      ENDIF.
    ENDIF.
    CLEAR gs_final-no_of_bags.
    IF i_oper IS NOT INITIAL.
      READ TABLE i_oper INTO DATA(ls_oper_hdr) INDEX 1. "#EC CI_NOORDER

      IF ls_oper_hdr-companycode = '1000'.

        gs_final-heading            = 'BHAGWATI LACTO VEGETARIAN EXPORTS PVT. LTD'.
        gs_final-header_1           = 'Regd. Office :18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
        gs_final-header_2           = 'Works : Rural Focal Point Mana Singh wala , Firozepur Moga Road' ##NO_TEXT.
        gs_final-sub_heading        = 'Supplier Credit Note' ##NO_TEXT.

      ELSEIF ls_oper_hdr-companycode = '2000'.

        gs_final-heading            = 'BHAGWATI LACTO FOODS PRIVATE LIMITED'.
        gs_final-header_1           = 'VILL. RUKNA MUNGLA, FARIDKOT ROAD, FEROZEPUR, Ferozepur, Punjab, 152001' ##NO_TEXT.
        gs_final-sub_heading        = 'Supplier Credit Note' ##NO_TEXT.

      ELSEIF ls_oper_hdr-companycode = '3000'.

        gs_final-heading            = 'HEALTHY HARVESTED FOODS PRIVATE LIMITED'.
        gs_final-header_1           = 'PLOT NO 100, SECTOR 1/A, GANDHIDHAM, Kachchh, Gujarat, 370201' ##NO_TEXT.
        gs_final-sub_heading        = 'Supplier Credit Note' ##NO_TEXT.

      ELSEIF ls_oper_hdr-companycode = '4000'.

        gs_final-heading            = 'BHAGWATI AGROCHEM PRIVATE LIMITED'.
        gs_final-header_1           = 'M-14A VILL. MOHASA BABAI INDSUTRIAL AREA MAKHAN NAGAR NARMADAPURAM M.P 461661' ##NO_TEXT.
        gs_final-sub_heading        = 'Supplier Credit Note' ##NO_TEXT.

      ENDIF.

      IF ls_oper_hdr-plant = '1002'.
        CLEAR : gs_final-heading, gs_final-header_1  ##NO_TEXT.
        gs_final-heading = 'HHF Kandla' ##NO_TEXT.
        gs_final-header_1 = 'PLOT NO. 362, GIDC IND. ESTATE, VILL. MITHIROHAR, GANDHIDHAM (KANDLA) GUJRAT 370201' ##NO_TEXT.
        gs_final-sub_heading = 'Supplier Credit Note' ##NO_TEXT.
      ENDIF.

      IF ls_oper_hdr-plant = '4001'.
        CLEAR : gs_final-heading, gs_final-header_1  ##NO_TEXT.
        gs_final-heading = 'BHAGWATI AGROCHEM PRIVATE LIMITED'.
        gs_final-header_1 = 'M-14A VILL. MOHASA BABAI INDSUTRIAL AREA MAKHAN NAGAR NARMADAPURAM M.P 461661' ##NO_TEXT.
        gs_final-sub_heading = 'Supplier Credit Note' ##NO_TEXT.
      ENDIF.

      READ TABLE i_oper INTO DATA(wa_ded) WITH KEY glaccount = '0000301015'. "#EC CI_NOORDER
      IF sy-subrc = 0.
        gs_final-qlty_cut_amt = wa_ded-amountincompanycodecurrency.
        gs_final-tot_cut_ded    = gs_final-tot_cut_ded + gs_final-qlty_cut_amt.
      ENDIF.

      CLEAR wa_ded.
      READ TABLE i_oper INTO wa_ded WITH KEY glaccount = '0000301010'. "#EC CI_NOORDER
      IF sy-subrc = 0.
        gs_final-paddy_amt = wa_ded-amountincompanycodecurrency.
        gs_final-tot_cut_ded    = gs_final-tot_cut_ded + gs_final-paddy_amt. "#EC CI_NO_TRANSFORM
      ENDIF.

      CLEAR wa_ded.
      READ TABLE i_oper INTO wa_ded WITH KEY glaccount = '0000301011'. "#EC CI_NOORDER
      IF sy-subrc = 0.
        gs_final-rice_amt = wa_ded-amountincompanycodecurrency.
        gs_final-tot_cut_ded    = gs_final-tot_cut_ded + gs_final-rice_amt.
      ENDIF.

      CLEAR wa_ded.
      READ TABLE i_oper INTO wa_ded WITH KEY glaccount = '0000301012'. "#EC CI_NOORDER
      IF sy-subrc = 0.
        gs_final-weigh_amt = wa_ded-amountincompanycodecurrency.
        gs_final-tot_cut_ded    = gs_final-tot_cut_ded + gs_final-weigh_amt.
      ENDIF.

      CLEAR wa_ded.
      READ TABLE i_oper INTO wa_ded WITH KEY glaccount = '0000301013'. "#EC CI_NOORDER
      IF sy-subrc = 0.
        gs_final-fine_amt = wa_ded-amountincompanycodecurrency.
        gs_final-tot_cut_ded    = gs_final-tot_cut_ded + gs_final-fine_amt.
      ENDIF.

      CLEAR wa_ded.
      READ TABLE i_oper INTO wa_ded WITH KEY glaccount = '0000301014'. "#EC CI_NOORDER
      IF sy-subrc = 0.
        gs_final-bility_amt = wa_ded-amountincompanycodecurrency.
        gs_final-tot_cut_ded    = gs_final-tot_cut_ded + gs_final-bility_amt.
      ENDIF.

      CLEAR wa_ded.
      READ TABLE i_oper INTO wa_ded WITH KEY glaccount = '0000301001'. "#EC CI_NOORDER
      IF sy-subrc = 0.
        gs_final-other_amt = wa_ded-amountincompanycodecurrency.
        gs_final-tot_cut_ded    = gs_final-tot_cut_ded + gs_final-other_amt.
      ENDIF.

      CLEAR wa_ded.
      READ TABLE i_oper INTO wa_ded WITH KEY glaccount = '0000407025'. "#EC CI_NOORDER
      IF sy-subrc = 0.
        gs_final-other_exp_amt = wa_ded-amountincompanycodecurrency.
        gs_final-tot_cut_ded    = gs_final-tot_cut_ded + gs_final-other_exp_amt.
      ENDIF.

      gs_final-tot_ded_amt    = gs_final-tot_ded_amt + gs_final-tot_cut_ded.

      SORT i_oper BY accountingdocumentitem.

      READ TABLE i_oper INTO DATA(wa_hdr) WITH KEY financialaccounttype = 'K'. "#EC CI_NOORDER
      IF sy-subrc = 0 AND wa_hdr-invoicereference IS NOT INITIAL.
        SELECT * FROM i_operationalacctgdocitem
        WHERE companycode        EQ @im_bukrs
        AND   accountingdocument EQ @wa_hdr-invoicereference
        AND   fiscalyear         EQ @wa_hdr-invoicereferencefiscalyear
        INTO TABLE @DATA(i_data).                       "#EC CI_NOORDER

      ENDIF.

      IF i_data IS INITIAL.
        SELECT * FROM i_operationalacctgdocitem
        WHERE companycode        EQ @im_bukrs
        AND   accountingdocument EQ @im_belnr
        AND   fiscalyear         EQ @im_gjahr
        AND   purchasingdocument IS NOT INITIAL
        INTO TABLE @i_data.                             "#EC CI_NOORDER

      ENDIF.

      IF i_data IS NOT INITIAL.

        SELECT * FROM i_purchaseorderitemapi01 FOR ALL ENTRIES IN @i_data "#EC CI_ALL_FIELDS_NEEDED
        WHERE purchaseorder EQ @i_data-purchasingdocument
        INTO TABLE @DATA(it_ekpo).                 "#EC CI_NO_TRANSFORM
        READ TABLE i_data INTO DATA(ls_hdr) INDEX 1.    "#EC CI_NOORDER

        gs_final-scn_no = ls_hdr-accountingdocument.

        SORT i_data  BY product .
        DELETE ADJACENT DUPLICATES FROM i_data  COMPARING product .


*""""""""""""""""""""""""""""""""""""""""""""""""""""""""'
    SELECT
      FROM i_operationalacctgdocitem as a
    FIELDS  a~companycode,
           a~accountingdocument,
           a~fiscalyear,
           a~purchasingdocument,
           a~originalreferencedocument,
           a~transactiontypedetermination,
           a~AmountInTransactionCurrency,
           a~Product
       WHERE
          a~accountingdocument eq @im_belnr
       and a~companycode      eq @im_bukrs
        AND a~fiscalyear        eq  @im_gjahr
        AND a~transactiontypedetermination = 'WRX'
         INTO TABLE @data(lt_acdoc).




    LOOP AT lt_acdoc ASSIGNING FIELD-SYMBOL(<fs_acdoc>).



      DATA(lv_orig_ref) = <fs_acdoc>-originalreferencedocument(10).


      SELECT purchaseorder,
             purchasinghistorydocument,
             purchasinghistorydocumentyear,
             purchaseorderitem,
             material,
             quantity
        FROM i_purchaseorderhistoryapi01
        WHERE purchaseorder                = @<fs_acdoc>-purchasingdocument
          AND purchasinghistorydocument    = @lv_orig_ref
          AND purchasinghistorydocumentyear = @<fs_acdoc>-fiscalyear
        INTO TABLE @data(lt_pohist).

if lt_pohist is NOT INITIAL.

      SELECT purchaseorder,  "#EC CI_NO_TRANSFORM
             purchaseorderitem,
             orderquantity
        FROM i_purchaseorderitemapi01
        FOR ALL ENTRIES IN @lt_pohist
        WHERE companycode      = @<fs_acdoc>-companycode
          AND purchaseorder    = @lt_pohist-purchaseorder
         AND purchaseorderitem =  @lt_pohist-PurchaseOrderItem
        INTO TABLE @data(lt_poitem). "#EC CI_NO_TRANSFORM

ENDif.

    ENDLOOP.






        LOOP AT i_data INTO DATA(ls_itm).

          IF ls_itm-purchasingdocument IS INITIAL.
            CONTINUE.
          ENDIF.

          ls_item-item_po         = ls_itm-purchasingdocument.
          ls_item-item_name       = |{ ls_itm-product ALPHA = OUT }|.
          READ TABLE it_token_data INTO DATA(wa_token_data) WITH KEY ponum    = ls_itm-purchasingdocument
                                                                     material = ls_itm-product.
*          IF wa_token_data IS NOT INITIAL.
*            ls_item-item_load_wt    = wa_token_data-supp_loadwht.
*            ls_item-item_net_wt     = wa_token_data-net_wt.
*
*            ls_item-item_final_wt   = wa_token_data-supp_loadwht - wa_token_data-net_wt.
*          ENDIF.
          """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  READ TABLE  lt_acdoc ASSIGNING FIELD-SYMBOL(<fs_acdoc1>) WITH KEY
   PurchasingDocument =  ls_itm-purchasingdocument  product = ls_itm-Product .

   ls_item-item_amt1  = <fs_acdoc1>-AmountInTransactionCurrency.




  LOOP AT lt_pohist ASSIGNING FIELD-SYMBOL(<fs_pohist>) WHERE  purchaseorder =  ls_itm-purchasingdocument and  material = ls_itm-Product .

   ls_item-bill_qty1 = <fs_pohist>-Quantity.
     gs_final-no_of_qty +=  <fs_pohist>-Quantity.



        READ TABLE lt_poitem ASSIGNING FIELD-SYMBOL(<fs_poitem>)
            WITH KEY
            purchaseorder = <fs_pohist>-purchaseorder
                     purchaseorderitem = <fs_pohist>-purchaseorderitem.


    ls_item-order_qty1 = <fs_poitem>-OrderQuantity.




      ENDLOOP.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""''
          SELECT
          FROM i_journalentry AS a
          INNER JOIN i_operationalacctgdocitem AS b ON a~accountingdocument = b~accountingdocument  AND a~fiscalyear = b~fiscalyear
          INNER JOIN  zi_token_ge AS c ON c~ponum = b~purchasingdocument AND c~material = b~product
          AND c~invoice_no = a~documentreferenceid AND c~gentry_year = b~fiscalyear
          FIELDS
          a~fiscalyear, a~accountingdocument , a~companycode, c~totalbardana , c~totaldeduc, b~product ,c~noofbag
          WHERE a~fiscalyear  EQ @im_gjahr
            AND  a~accountingdocument EQ @im_belnr
            AND  a~companycode EQ @im_bukrs
           INTO TABLE @DATA(dedc).





          READ TABLE dedc INTO DATA(dec)
            WITH KEY fiscalyear         = ls_itm-fiscalyear
                     accountingdocument = ls_itm-accountingdocument
                     companycode        = ls_itm-companycode
                     product            =  ls_item-item_name .
          IF sy-subrc = 0.
            CLEAR : lv_bardana, lv_deduction.
            lv_bardana = dec-totalbardana.
            lv_deduction = dec-totaldeduc.
            ls_item-item_deduction  = ls_item-item_deduction + lv_bardana.
            ls_item-item_deduction  = ls_item-item_deduction + lv_deduction.

          ENDIF.
          """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""




          """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

          SELECT SINGLE incotermsclassification FROM i_purchaseorderapi01
          WHERE purchaseorder EQ @ls_item-item_po
          INTO @gs_final-deals_in.

          """"""""""""""""""""""""""""""""
          IF wa_token_data IS NOT INITIAL.
            ls_item-item_load_wt    = wa_token_data-supp_loadwht.
            ls_item-item_net_wt     = wa_token_data-net_wt.
            IF gs_final-deals_in = 'TRL'.
              ls_item-item_final_wt = ls_item-item_net_wt - ls_item-item_deduction.
            ELSEIF gs_final-deals_in = 'FOR'.
              IF ls_item-item_load_wt > ls_item-item_net_wt.
                ls_item-item_final_wt = ls_item-item_net_wt - ls_item-item_deduction.
              ELSE.
                ls_item-item_final_wt =  ls_item-item_load_wt - ls_item-item_deduction.
              ENDIF.
*            ls_item-item_final_wt   = wa_token_data-supp_loadwht - wa_token_data-net_wt.
            ENDIF.
          ENDIF.
*        """"""""""""""""""""""""""""



          IF wa_hdr-invoicereference IS NOT INITIAL.
            LOOP AT i_oper INTO DATA(itm_ded) WHERE accountingdocumentitem EQ ls_itm-accountingdocumentitem
                                                AND transactiontypedetermination EQ 'WRX'
                                                 OR transactiontypedetermination EQ 'BSX'
                                                 OR transactiontypedetermination EQ 'PRD'.

*              ls_item-item_deduction  = ls_item-item_deduction + itm_ded-amountincompanycodecurrency.
            ENDLOOP.
            gs_final-tot_ded_amt    = gs_final-tot_ded_amt + ls_item-item_deduction.
          ENDIF.
          READ TABLE it_ekpo INTO DATA(wa_ekpo) WITH KEY purchaseorder = ls_itm-purchasingdocument
                                                         purchaseorderitem = ls_itm-purchasingdocumentitem.
          IF sy-subrc = 0.
            ls_item-item_rate   = wa_ekpo-netpriceamount.
          ENDIF.
          ls_item-item_amt        = ls_item-item_final_wt * ls_item-item_rate.

          if im_action = 'scnpkg'.
           gs_final-gross_amt      = gs_final-gross_amt + ls_item-item_amt1 .
           ELSE.
           gs_final-gross_amt      = gs_final-gross_amt + ls_item-item_amt .
          ENDIF.

          APPEND ls_item TO lt_item.
          CLEAR: itm_ded, ls_item, wa_ekpo, wa_token_data ,dec ,<fs_poitem> , <fs_pohist> , <fs_acdoc1>.

        ENDLOOP.




      ENDIF.





      gs_final-net_amt    = gs_final-gross_amt + gs_final-tot_ded_amt.
      gs_final-party_name = wa_hdr-supplierfullname.
      gs_final-address    = wa_hdr-streetname && wa_hdr-cityname && '-' && wa_hdr-postalcode.

      SELECT * FROM i_journalentry
      WHERE companycode        EQ @im_bukrs
      AND   accountingdocument EQ @wa_hdr-invoicereference
      AND   fiscalyear         EQ @wa_hdr-invoicereferencefiscalyear
      INTO TABLE @DATA(i_jour).


      IF i_jour IS INITIAL.
        SELECT * FROM i_journalentry
        WHERE companycode        EQ @im_bukrs
        AND   accountingdocument EQ @im_belnr
        AND   fiscalyear         EQ @im_gjahr
        INTO TABLE @i_jour.
      ENDIF.                                       "#EC CI_NO_TRANSFORM

      IF i_jour IS NOT INITIAL.
        READ TABLE i_jour INTO DATA(ls_jour) INDEX 1.
        CLEAR ls_item.
        READ TABLE lt_item INTO ls_item INDEX 1.
        gs_final-scn_date   = ls_jour-postingdate+6(2) && '.' &&
                              ls_jour-postingdate+4(2) && '.' &&
                              ls_jour-postingdate+0(4).

        gs_final-invoice_no = ls_jour-documentreferenceid.
        gs_final-invoice_date = ls_jour-documentdate+6(2) && '.' &&
                                ls_jour-documentdate+4(2) && '.' &&
                                ls_jour-documentdate+0(4).

        SELECT SINGLE supplier FROM i_purchaseorderpartnerapi01
                               WHERE purchaseorder EQ @ls_item-item_po
                               INTO @DATA(supplier).
        IF supplier IS NOT INITIAL.
          SELECT SINGLE suppliername FROM i_supplier WHERE supplier EQ @supplier
          INTO @gs_final-broker_name.
        ENDIF.

        SELECT SINGLE * FROM zi_token_ge WHERE ponum      EQ @ls_item-item_po
                                         AND   material   EQ @ls_item-item_name
                                         AND   invoice_no EQ @ls_jour-documentreferenceid
                                         INTO @DATA(ls_token_ge).
        IF ls_token_ge IS INITIAL.
          SELECT SINGLE * FROM zi_ge_data WHERE ponum   EQ @ls_item-item_po
                                          AND   matnr   EQ @ls_item-item_name
                                          AND   billnum EQ @ls_jour-documentreferenceid
                                          INTO @DATA(ls_ge_data). "#EC CI_ALL_FIELDS_NEEDED
        ENDIF.

        gs_final-unloading_slip   = ls_token_ge-gentry_num.
        IF gs_final-unloading_slip IS INITIAL.
          gs_final-unloading_slip   = ls_ge_data-gentry_num.
        ENDIF.
        gs_final-gate_inward    = gs_final-unloading_slip.
        gs_final-vehicle_no     = ls_token_ge-vehicle_no.
        IF gs_final-vehicle_no IS INITIAL.
          gs_final-vehicle_no     = ls_ge_data-vechnum.
        ENDIF.
        gs_final-mobile         = ls_token_ge-mobileno.
        IF gs_final-mobile IS INITIAL.
          gs_final-mobile         = ls_ge_data-driver_num.
        ENDIF.


*      SELECT SINGLE incotermsclassification FROM i_purchaseorderapi01
*        WHERE purchaseorder EQ @ls_item-item_po
*        INTO @gs_final-deals_in.

      ENDIF.
*      ENDIF.
      READ TABLE it_token_data INTO wa_token_data INDEX 1. "#EC CI_NOORDER
      gs_final-gross_wt     =   wa_token_data-gross_wt.
      gs_final-tare_wt      =   wa_token_data-tare_wt.
      gs_final-net_wt       =   wa_token_data-net_wt.
      gs_final-party_wt     =   wa_token_data-supp_loadwht.

      SORT it_tax BY transactiontypedetermination.
      LOOP AT it_tax INTO DATA(wa_tax) WHERE transactiontypedetermination = 'JIC'.
        gs_final-cgst_amt = gs_final-cgst_amt + wa_tax-amountincompanycodecurrency.
        IF wa_tax-taxbaseamountincocodecrcy IS NOT INITIAL.
          gs_final-cgst_rate = ( wa_tax-amountincompanycodecurrency * 100 ) / wa_tax-taxbaseamountincocodecrcy.
        ENDIF.
      ENDLOOP.

      CLEAR wa_tax.
      LOOP AT it_tax INTO wa_tax WHERE transactiontypedetermination = 'JIS'.
        gs_final-sgst_amt = gs_final-sgst_amt + wa_tax-amountincompanycodecurrency.
        IF wa_tax-taxbaseamountincocodecrcy IS NOT INITIAL.
          gs_final-sgst_rate = ( wa_tax-amountincompanycodecurrency * 100 ) / wa_tax-taxbaseamountincocodecrcy.
        ENDIF.
      ENDLOOP.

      CLEAR wa_tax.
      LOOP AT it_tax INTO wa_tax WHERE transactiontypedetermination = 'JII'.
        gs_final-igst_amt = gs_final-igst_amt + wa_tax-amountincompanycodecurrency.
        IF wa_tax-taxbaseamountincocodecrcy IS NOT INITIAL.
          gs_final-igst_rate = ( wa_tax-amountincompanycodecurrency * 100 ) / wa_tax-taxbaseamountincocodecrcy.
        ENDIF.
      ENDLOOP.

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""''
*

      SELECT
      FROM i_accountingdocumentjournal
       FIELDS
        companycode,
        fiscalyear,
        accountingdocument,
*    TransactionTypeDetermination,
        debitamountincocodecrcy  AS totaldebit,
         creditamountincocodecrcy AS totalcredit,
         purchasingdocument
     WHERE companycode                EQ @im_bukrs
       AND fiscalyear                 EQ @im_gjahr
*   AND ACCOUNTINGDOCUMENT         EQ @im_belnr
       AND purchasingdocument = @ls_item-item_po
       AND accountingdocumenttype     = 'RE'
       AND transactiontypedetermination IN ('0PA', 'FR1', 'PK1') AND ledger = '0L'
     INTO TABLE @DATA(lt_journal).

      DATA : total_debit  TYPE p DECIMALS 2,
             total_credit TYPE p DECIMALS 2.

      LOOP AT lt_journal INTO DATA(wa).
        total_debit += wa-totaldebit.
        total_credit += wa-totalcredit.
        CLEAR : wa .
      ENDLOOP.


      SELECT
       FROM i_accountingdocumentjournal
        FIELDS
         companycode,
         fiscalyear,
         accountingdocument,
         transactiontypedetermination,
         debitamountincocodecrcy  AS totaldebit1,
          creditamountincocodecrcy AS totalcredit1,
          purchasingdocument
      WHERE companycode                EQ @im_bukrs
        AND fiscalyear                 EQ @im_gjahr
*   AND ACCOUNTINGDOCUMENT         EQ @im_belnr
        AND purchasingdocument = @ls_item-item_po
        AND accountingdocumenttype     = 'RE'
        AND transactiontypedetermination IN ('0PA', 'FR1', 'PK1') AND ledger = '0L'
      INTO TABLE @DATA(lt_journal1).

      DATA : cond_type1  TYPE string,
             cond_type2  TYPE string,
             cond_type3  TYPE string,
             cond_type4  TYPE string,
             cond_type5  TYPE string,
             cond_type6  TYPE string,
             cond_type7  TYPE string,
             cond_type8  TYPE string,
             cond_type9  TYPE string,
             cond_type10 TYPE string,
             cond_type11 TYPE string,
             cond_type12 TYPE string,
             cond_type13 TYPE string,
             cond_type14 TYPE string,
             cond_type15 TYPE string,
             cond_type16 TYPE string,
             cond_type17 TYPE string,
             cond_type18 TYPE string,
             cond_type19 TYPE string,
             cond_type20 TYPE string,
             cond_type21 TYPE string,
             cond_type22 TYPE string,
             cond_type23 TYPE string,
             cond_type24 TYPE string,
             cond_type25 TYPE string,
             cond_type26 TYPE string,
             cond_type27 TYPE string,
             cond_type28 TYPE string,
             cond_type29 TYPE string,
             cond_type30 TYPE string,
             cond_type31 TYPE string,
             cond_type32 TYPE string,
             cond_type33 TYPE string,
             cond_type34 TYPE string,
             cond_type35 TYPE string,
             cond_type36 TYPE string.

      DATA : cond_amt1  TYPE p DECIMALS 2,
             cond_amt2  TYPE p DECIMALS 2,
             cond_amt3  TYPE p DECIMALS 2,
             cond_amt4  TYPE p DECIMALS 2,
             cond_amt5  TYPE p DECIMALS 2,
             cond_amt6  TYPE p DECIMALS 2,
             cond_amt7  TYPE p DECIMALS 2,
             cond_amt8  TYPE p DECIMALS 2,
             cond_amt9  TYPE p DECIMALS 2,
             cond_amt10 TYPE p DECIMALS 2,
             cond_amt11 TYPE p DECIMALS 2,
             cond_amt12 TYPE p DECIMALS 2,
             cond_amt13 TYPE p DECIMALS 2,
             cond_amt14 TYPE p DECIMALS 2,
             cond_amt15 TYPE p DECIMALS 2,
             cond_amt16 TYPE p DECIMALS 2,
             cond_amt17 TYPE p DECIMALS 2,
             cond_amt18 TYPE p DECIMALS 2,
             cond_amt19 TYPE p DECIMALS 2,
             cond_amt20 TYPE p DECIMALS 2,
             cond_amt21 TYPE p DECIMALS 2,
             cond_amt22 TYPE p DECIMALS 2,
             cond_amt23 TYPE p DECIMALS 2,
             cond_amt24 TYPE p DECIMALS 2,
             cond_amt25 TYPE p DECIMALS 2,
             cond_amt26 TYPE p DECIMALS 2,
             cond_amt27 TYPE p DECIMALS 2,
             cond_amt28 TYPE p DECIMALS 2,
             cond_amt29 TYPE p DECIMALS 2,
             cond_amt30 TYPE p DECIMALS 2,
             cond_amt31 TYPE p DECIMALS 2,
             cond_amt32 TYPE p DECIMALS 2,
             cond_amt33 TYPE p DECIMALS 2,
             cond_amt34 TYPE p DECIMALS 2,
             cond_amt35 TYPE p DECIMALS 2,
             cond_amt36 TYPE p DECIMALS 2.


      LOOP AT  lt_journal1 INTO DATA(wa1).

*IF sy-subrc = 0.
        SELECT
         FROM i_purorditmpricingelementapi01  AS a
         INNER JOIN  i_conditiontypetext  AS b ON a~conditiontype = b~conditiontype
         FIELDS
         a~conditiontype,
         a~conditionamount,
         a~acctkeyforaccrualsglaccount,
         a~purchaseorder,
         b~conditiontypename
         WHERE
         b~language = 'E'
         AND a~purchaseorder = @ls_item-item_po  AND a~acctkeyforaccrualsglaccount IN ('0PA', 'FR1', 'PK1')
         INTO  TABLE @DATA(add_disc) .

*ENDIF.

        LOOP AT add_disc INTO  DATA(cond).


          CASE cond-conditiontype.

            WHEN 'ZHB0'.
              cond_type1 = cond-conditiontypename.
              cond_amt1  += cond-conditionamount.

            WHEN 'ZHB1'.
              cond_type2 = cond-conditiontypename.
              cond_amt2  += cond-conditionamount.

            WHEN 'ZHB2'.
              cond_type3 = cond-conditiontypename.
              cond_amt3  += cond-conditionamount.

            WHEN 'ZHB3'.
              cond_type4 = cond-conditiontypename.
              cond_amt4  += cond-conditionamount.

            WHEN 'ZPK1'.
              cond_type5 = cond-conditiontypename.
              cond_amt5  += cond-conditionamount.

            WHEN 'ZPK2'.
              cond_type6 = cond-conditiontypename.
              cond_amt6  += cond-conditionamount.

            WHEN 'ZFV1'.
              cond_type7 = cond-conditiontypename.
              cond_amt7  += cond-conditionamount.

            WHEN 'ZFVA'.
              cond_type8 = cond-conditiontypename.
              cond_amt8  += cond-conditionamount.

            WHEN 'ZFRA'.
              cond_type9 = cond-conditiontypename.
              cond_amt9  += cond-conditionamount.

            WHEN 'ZFRB'.
              cond_type10 = cond-conditiontypename.
              cond_amt10 += cond-conditionamount.

            WHEN 'ZFRC'.
              cond_type11 = cond-conditiontypename.
              cond_amt11 += cond-conditionamount.

            WHEN 'ZIN1'.
              cond_type12 = cond-conditiontypename.
              cond_amt12 += cond-conditionamount.

            WHEN 'ZIN2'.
              cond_type13 = cond-conditiontypename.
              cond_amt13 += cond-conditionamount.

            WHEN 'ZDHM'.
              cond_type14 = cond-conditiontypename.
              cond_amt14 += cond-conditionamount.

            WHEN 'ZCOM'.
              cond_type15 = cond-conditiontypename.
              cond_amt15 += cond-conditionamount.

            WHEN 'ZMFE'.
              cond_type16 = cond-conditiontypename.
              cond_amt16 += cond-conditionamount.

            WHEN 'ZHRD'.
              cond_type17 = cond-conditiontypename.
              cond_amt17 += cond-conditionamount.

            WHEN 'ZBC1'.
              cond_type18 = cond-conditiontypename.
              cond_amt18 += cond-conditionamount.

            WHEN 'ZLAB'.
              cond_type19 = cond-conditiontypename.
              cond_amt19 += cond-conditionamount.

            WHEN 'ZAUC'.
              cond_type20 = cond-conditiontypename.
              cond_amt20 += cond-conditionamount.

            WHEN 'ZHAM'.
              cond_type21 = cond-conditiontypename.
              cond_amt21 += cond-conditionamount.

            WHEN 'ZDLA'.
              cond_type22 = cond-conditiontypename.
              cond_amt22 += cond-conditionamount.

            WHEN 'ZOXR'.
              cond_type23 = cond-conditiontypename.
              cond_amt23 += cond-conditionamount.

            WHEN 'ZTUL'.
              cond_type24 = cond-conditiontypename.
              cond_amt24 += cond-conditionamount.

            WHEN 'ZLOD'.
              cond_type25 = cond-conditiontypename.
              cond_amt25 += cond-conditionamount.

            WHEN 'ZUNL'.
              cond_type26 = cond-conditiontypename.
              cond_amt26 += cond-conditionamount.

            WHEN 'ZSTH'.
              cond_type27 = cond-conditiontypename.
              cond_amt27 += cond-conditionamount.

            WHEN 'ZUNS'.
              cond_type28 = cond-conditiontypename.
              cond_amt28 += cond-conditionamount.

            WHEN 'ZGAU'.
              cond_type29 = cond-conditiontypename.
              cond_amt29 += cond-conditionamount.

            WHEN 'ZOTH'.
              cond_type30 = cond-conditiontypename.
              cond_amt30 += cond-conditionamount.

            WHEN 'ZOH1'.
              cond_type31 = cond-conditiontypename.
              cond_amt31 += cond-conditionamount.

            WHEN 'ZBAR'.
              cond_type32 = cond-conditiontypename.
              cond_amt32 += cond-conditionamount.

            WHEN 'ZCES'.
              cond_type33 = cond-conditiontypename.
              cond_amt33 += cond-conditionamount.

            WHEN 'ZASS'.
              cond_type34 = cond-conditiontypename.
              cond_amt34 += cond-conditionamount.

            WHEN 'ZCCS'.
              cond_type35 = cond-conditiontypename.
              cond_amt35 += cond-conditionamount.

            WHEN 'ZLLF'.
              cond_type36 = cond-conditiontypename.
              cond_amt36 += cond-conditionamount.

          ENDCASE.

        ENDLOOP.


        CLEAR : wa1 .
      ENDLOOP.

      """""""""""""""""""""""""""""""""""""""""""""""""""


      gs_final-addition_amt = gs_final-cgst_amt + gs_final-sgst_amt + gs_final-igst_amt + total_debit + total_credit .
      gs_final-add_cut_amt =  total_debit + total_credit .




      SELECT SINGLE amountincompanycodecurrency FROM  i_journalentryitem
                                                WHERE companycode        EQ @im_bukrs
                                                AND   accountingdocument EQ @im_belnr
                                                AND   fiscalyear         EQ @im_gjahr
                                                AND   glaccount          EQ '0000301000'
                                                AND   isopenitemmanaged  EQ ''
      INTO @gs_final-round_off.

      gs_final-tot_ded_amt    = gs_final-tot_ded_amt + gs_final-round_off.


      DATA : lv_deductiona TYPE c LENGTH 20.


      .
      IF gs_final-tot_ded_amt < 0.
        " Negative -> subtracting a negative = ADD
        gs_final-net_amt = gs_final-addition_amt + gs_final-gross_amt - gs_final-tot_ded_amt + gs_final-tot_ded_amt + gs_final-tot_ded_amt.
      ELSE.
        " Positive -> add directly
        gs_final-net_amt = gs_final-addition_amt + gs_final-gross_amt + gs_final-tot_ded_amt.
      ENDIF.


      LOOP AT it_qlty_data INTO DATA(wa_qlty_data).
        ls_qlty-impurity   = wa_qlty_data-inspectioncharacteristictext.
        ls_qlty-target_qty = wa_qlty_data-inspspectargetvalue.
        ls_qlty-anality    = wa_qlty_data-inspectionresultoriginalvalue.
        ls_qlty-result_qty = ''.
        ls_qlty-result_amt = ''.

        APPEND ls_qlty TO lt_qlty.
      ENDLOOP.

    ENDIF.

    SORT dedc  BY product .
    DELETE ADJACENT DUPLICATES FROM dedc  COMPARING product .

    LOOP AT dedc INTO DATA(no_bags) .
      gs_final-no_of_bags += no_bags-noofbag.
      CLEAR : no_bags.
      CONDENSE :gs_final-no_of_bags .
    ENDLOOP.




    INSERT LINES OF lt_item INTO TABLE gs_final-gt_item.
    INSERT LINES OF lt_qlty INTO TABLE gs_final-gt_qlty.
    APPEND gs_final TO et_final.
*



*clear :  gs_final-no_of_bags.
  ENDMETHOD.


  METHOD get_voucher_data.

    DATA:
      lo_amt_words  TYPE REF TO zcl_amt_words,
      lv_amount_neg TYPE c LENGTH 20,
      lv_dr_amount  TYPE p LENGTH 16 DECIMALS 2.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    CREATE OBJECT lo_amt_words.

    SELECT * FROM i_journalentry
             WHERE companycode  = @im_bukrs AND
                   fiscalyear   = @im_gjahr AND
                   accountingdocument = @im_belnr
             INTO TABLE @DATA(lt_bkpf).       "#EC CI_ALL_FIELDS_NEEDED

    SELECT * FROM zi_dc_note
             WHERE companycode  = @im_bukrs AND
                   fiscalyear   = @im_gjahr AND
                   accountingdocument = @im_belnr
             INTO TABLE @DATA(lt_acodca).     "#EC CI_ALL_FIELDS_NEEDED

    READ TABLE lt_bkpf INTO DATA(ls_bkpf) INDEX 1.
    READ TABLE lt_acodca INTO DATA(ls_acdoca1) INDEX 1. "#EC CI_NOORDER

    gs_final-companycode          = ls_bkpf-companycode.
    gs_final-accountingdocument   = ls_bkpf-accountingdocument.
    gs_final-fiscalyear           = ls_bkpf-fiscalyear.
    gs_final-heading              = ls_acdoca1-companycodename.

    IF ls_acdoca1-companycode = '1000'.

      gs_final-heading            = 'BHAGWATI LACTO VEGETARIAN EXPORTS PVT. LTD' ##NO_TEXT.
      gs_final-sub_heading        = 'Regd. Office :18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
      gs_final-header_1           = 'Works : Rural Focal Point Mana Singh wala , Firozepur Moga Road' ##NO_TEXT.

    ELSEIF ls_acdoca1-companycode = '2000'.

      gs_final-heading            = 'BHAGWATI LACTO FOODS PRIVATE LIMITED' ##NO_TEXT.
      gs_final-sub_heading        = 'VILL. RUKNA MUNGLA, FARIDKOT ROAD, FEROZEPUR, Ferozepur, Punjab, 152001' ##NO_TEXT.
      gs_final-header_1           = '' ##NO_TEXT.

    ELSEIF ls_acdoca1-companycode = '3000'.

      gs_final-heading            = 'HEALTHY HARVESTED FOODS PRIVATE LIMITED' ##NO_TEXT.
      gs_final-sub_heading        = 'PLOT NO 100, SECTOR 1/A, GANDHIDHAM, Kachchh, Gujarat, 370201' ##NO_TEXT.
      gs_final-header_1           = '' ##NO_TEXT.

    ELSEIF ls_acdoca1-companycode = '4000'.

      gs_final-heading     = 'BHAGWATI AGROCHEM PRIVATE LIMITED' ##NO_TEXT.
      gs_final-sub_heading = 'M-14A VILL. MOHASA BABAI INDSUTRIAL AREA MAKHAN NAGAR NARMADAPURAM M.P 461661' ##NO_TEXT.
      gs_final-header_1    = '' ##NO_TEXT.

    ENDIF.

    IF ls_acdoca1-plant = '1002'.
      CLEAR : gs_final-heading, gs_final-sub_heading ##NO_TEXT.
      gs_final-heading = 'HHF Kandla' ##NO_TEXT.
      gs_final-sub_heading = 'PLOT NO. 362, GIDC IND. ESTATE, VILL. MITHIROHAR, GANDHIDHAM (KANDLA) GUJRAT 370201' ##NO_TEXT.
    ENDIF.

    IF ls_acdoca1-plant = '4001'.
      CLEAR : gs_final-heading, gs_final-sub_heading.
      gs_final-heading = 'BHAGWATI AGROCHEM PRIVATE LIMITED'.
      gs_final-sub_heading = 'M-14A VILL. MOHASA BABAI INDSUTRIAL AREA MAKHAN NAGAR NARMADAPURAM M.P 461661' ##NO_TEXT.
    ENDIF.

    gs_final-header_2             = 'Journal Voucher' ##NO_TEXT.
    gs_final-header_3             = ''.

    gs_final-comp_name            = ls_acdoca1-companycodename.
    gs_final-comp_adrs1           = ''.
    gs_final-comp_adrs2           = ''.
    gs_final-comp_adrs3           = ''.
    gs_final-voucher_no           = ls_bkpf-accountingdocument.
    gs_final-voucher_date         = ls_bkpf-postingdate.

    SELECT SINGLE * FROM i_accountingdocumenttypetext
                    WHERE accountingdocumenttype = @ls_bkpf-accountingdocumenttype AND language = 'E'
                    INTO @DATA(ls_doctype_text). "#EC CI_ALL_FIELDS_NEEDED


    gs_final-doc_type_desc        = ls_doctype_text-accountingdocumenttypename.
    gs_final-doc_type             = ls_bkpf-accountingdocumenttype.

    gs_final-ref_num              = ls_bkpf-documentreferenceid.

    gs_final-posting_date         = ls_bkpf-postingdate+6(2) && '.' &&
                                    ls_bkpf-postingdate+4(2) && '.' &&
                                    ls_bkpf-postingdate+0(4).

    gs_final-doc_date             = ls_bkpf-documentdate+6(2) && '.' &&
                                    ls_bkpf-documentdate+4(2) && '.' &&
                                    ls_bkpf-documentdate+0(4).

    gs_final-currency             = ls_bkpf-transactioncurrency.
    gs_final-park_by              = ls_bkpf-parkedbyuser.
    gs_final-posted_by            = ls_bkpf-accountingdoccreatedbyuser.
    gs_final-amt_words            = ''.

    LOOP AT lt_acodca INTO DATA(ls_acdoca).

      IF ls_acdoca-financialaccounttype = 'S' OR
         ls_acdoca-financialaccounttype = 'K' OR
         ls_acdoca-financialaccounttype = 'D'.

        SELECT SINGLE * FROM i_glaccounttext
                        WHERE glaccount = @ls_acdoca-glaccount AND language = 'E'
                        INTO @DATA(ls_gltext). "#EC CI_ALL_FIELDS_NEEDED

        ls_item-companycode         = ls_acdoca-companycode.
        ls_item-accountingdocument  = ls_acdoca-accountingdocument.
        ls_item-fiscalyear          = ls_acdoca-fiscalyear.
        ls_item-sr_num              = ''.
        ls_item-acc_doc_item        = ls_acdoca-accountingdocumentitem .
        ls_item-bill_num            = ls_acdoca-documentreferenceid.
        ls_item-posting_key         = ls_acdoca-postingkey.
        ls_item-gl_code             = ls_acdoca-glaccount.
        ls_item-gl_desc             = ls_gltext-glaccountname.

        CLEAR: lv_amount_neg.
        lv_amount_neg = ls_acdoca-amountintransactioncurrency.
        CONDENSE lv_amount_neg.
        IF lv_amount_neg CA '-'.
          lv_dr_amount = ls_acdoca-amountintransactioncurrency * -1.
        ELSE.
          lv_dr_amount = ls_acdoca-amountintransactioncurrency.
        ENDIF.

        IF ls_acdoca-debitcreditcode = 'S'.
          ls_item-dr_amt              = lv_dr_amount.
        ELSE.
          ls_item-cr_amt              = lv_dr_amount.
        ENDIF.

        ls_item-c_center            = ''.
        ls_item-plant               = ls_acdoca-plant.
        ls_item-assign_ref          = ls_acdoca-assignmentreference.
        ls_item-tax_code            = ls_acdoca-taxcode.
        ls_item-item_qty            = ls_acdoca-quantity.
        ls_item-narration           = ls_acdoca-documentitemtext.
        ls_item-supplier_code       = ls_acdoca-supplier.
        "  ls_item-supplier_name       = ls_acdoca-supplierfullname.
*===
        SELECT SINGLE suppliername FROM  i_supplier WHERE supplier = @ls_item-supplier_code
         INTO @ls_item-supplier_name .
*===
        IF gs_final-header_3 IS INITIAL. ""Used as narration
          gs_final-header_3           = gs_final-header_3 && ls_acdoca-documentitemtext. "#EC CI_NOORDER
        ELSE.
          gs_final-header_3           = gs_final-header_3 && ',' && ls_acdoca-documentitemtext. "#EC CI_NOORDER
        ENDIF.
        APPEND ls_item TO lt_item.

        IF ls_acdoca-financialaccounttype = 'K' OR  ls_acdoca-financialaccounttype = 'D'.
          gs_final-header_3     = ls_acdoca-documentitemtext.. "*Used as Narration
        ENDIF.


        gs_final-sum_cr_amt   = gs_final-sum_cr_amt + ls_item-cr_amt.
        gs_final-sum_dr_amt   = gs_final-sum_dr_amt + ls_item-dr_amt.
        gs_final-sum_item_qty = gs_final-sum_item_qty + ls_acdoca-quantity.



      ENDIF.

      CLEAR: ls_acdoca, ls_gltext, ls_item.
    ENDLOOP.

    DATA: lv_grand_tot_word TYPE string.

    lv_grand_tot_word = gs_final-sum_dr_amt.

    lo_amt_words->number_to_words(
      EXPORTING
        iv_num   = lv_grand_tot_word
      RECEIVING
        rv_words = DATA(amt_words)
    ).

    CONCATENATE amt_words 'Only' INTO gs_final-amt_words SEPARATED BY space ##NO_TEXT.

    INSERT LINES OF lt_item INTO TABLE gs_final-gt_item.
    APPEND gs_final TO et_final.

  ENDMETHOD.


  METHOD prep_xml_chqslip_print.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    CLEAR: ht_final.
    ht_final[] = it_final[].

    READ TABLE ht_final INTO hs_final INDEX 1.

    DATA(lv_xml) =  |<Form>| &&
                    |<ChequeSlip>| &&
                    |<Company_Code>{ hs_final-companycode }</Company_Code>| &&
                    |<Company_Name>{ hs_final-cc_name }</Company_Name>| &&
                    |<Payment_Document>{ hs_final-paymentdocument }</Payment_Document>| &&
                    |<Fiscal_Year>{ hs_final-fiscalyear }</Fiscal_Year>| &&
                    |<ItemData>|  ##NO_TEXT.

    DATA : lv_item TYPE string,
           srn     TYPE c LENGTH 3.

    CLEAR : lv_item , srn .

    LOOP AT hs_final-gt_item INTO sw_item.

      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<SR_No>{ sw_item-serialno }</SR_No>| &&
                |<Account_Number>{ sw_item-accountno }</Account_Number>| &&
                |<Amount>{ sw_item-amount }</Amount>| &&
                |<Party_Name>{ sw_item-partyname }</Party_Name>| &&
                |</ItemDataNode>|  ##NO_TEXT .

      CLEAR: ls_item.
    ENDLOOP.

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                       |</ChequeSlip>| &&
                       |</Form>| ##NO_TEXT .

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.



  ENDMETHOD.


  METHOD prep_xml_scn_print.
    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    CLEAR: gt_final.
    gt_final[] = it_final[].

    READ TABLE gt_final INTO gs_final INDEX 1.

    DATA(lv_xml) =  |<Form>| &&
                    |<SCNNode>| &&
                    |<heading>{ gs_final-heading }</heading>| &&
                    |<sub_heading>{ gs_final-sub_heading }</sub_heading>| &&
                    |<plant_address_l1>{ gs_final-header_1 }</plant_address_l1>| &&
                    |<plant_address_l2>{ gs_final-header_2 }</plant_address_l2>| &&
                    |<plant_address_l3>{ gs_final-header_3 }</plant_address_l3>| &&
                    |<scn_no>{ gs_final-scn_no }</scn_no>| &&
                    |<party_name>{ gs_final-party_name }</party_name>| &&
                    |<address>{ gs_final-address }</address>| &&
                    |<unloading_slip>{ gs_final-unloading_slip }</unloading_slip>| &&
                    |<lab_slip_no>{ gs_final-lab_slip_no }</lab_slip_no>| &&
                    |<gate_inward_no>{ gs_final-gate_inward }</gate_inward_no>| &&
                    |<date>{ gs_final-scn_date }</date>| &&
                    |<broker_name>{ gs_final-broker_name }</broker_name>| &&
                    |<no_of_bags>{ gs_final-no_of_bags }</no_of_bags>| &&
                    |<invoice_no>{ gs_final-invoice_no }</invoice_no>| &&
                    |<invoice_date>{ gs_final-invoice_date }</invoice_date>| &&
                    |<vehicle_no>{ gs_final-vehicle_no }</vehicle_no>| &&
                    |<mobile_no>{ gs_final-mobile }</mobile_no>| &&
                    |<deals_in>{ gs_final-deals_in }</deals_in>| &&
                    |<gross_wt>{ gs_final-gross_wt }</gross_wt>| &&
                    |<tare_wt>{ gs_final-tare_wt }</tare_wt>| &&
                    |<net_wt>{ gs_final-net_wt }</net_wt>| &&
                    |<party_wt>{ gs_final-party_wt }</party_wt>| &&
                    |<qlty_cut_rate>{ gs_final-qlty_cut_rate }</qlty_cut_rate>| &&
                    |<qlty_cut_amt>{ gs_final-qlty_cut_amt }</qlty_cut_amt>| &&
                    |<paddy_rate>{ gs_final-paddy_rate }</paddy_rate>| &&
                    |<paddy_amt>{ gs_final-paddy_amt }</paddy_amt>| &&
                    |<rice_rate>{ gs_final-rice_rate }</rice_rate>| &&
                    |<rice_amt>{ gs_final-rice_amt }</rice_amt>| &&
                    |<weigh_rate>{ gs_final-weigh_rate }</weigh_rate>| &&
                    |<weigh_amt>{ gs_final-weigh_amt }</weigh_amt>| &&
                    |<fine_rate>{ gs_final-fine_rate }</fine_rate>| &&
                    |<fine_amt>{ gs_final-fine_amt }</fine_amt>| &&
                    |<bility_rate>{ gs_final-bility_rate }</bility_rate>| &&
                    |<bility_amt>{ gs_final-bility_amt }</bility_amt>| &&
                    |<other_rate>{ gs_final-other_rate }</other_rate>| &&
                    |<other_amt>{ gs_final-other_amt }</other_amt>| &&
                    |<other_exp_rate>{ gs_final-other_exp_rate }</other_exp_rate>| &&
                    |<other_exp_amt>{ gs_final-other_exp_amt }</other_exp_amt>| &&
                    |<add_cut_rate>{ gs_final-add_cut_rate }</add_cut_rate>| &&
                    |<add_cut_amt>{ gs_final-add_cut_amt }</add_cut_amt>| &&
                    |<tot_cut_ded>{ gs_final-tot_cut_ded }</tot_cut_ded>| &&
                    |<tot_ded_amt>{ gs_final-tot_ded_amt }</tot_ded_amt>| &&
                    |<gross_amt>{ gs_final-gross_amt }</gross_amt>| &&
                    |<addition_amt>{ gs_final-addition_amt }</addition_amt>| &&
                    |<net_amt>{ gs_final-net_amt }</net_amt>| &&
                    |<cgst_rate>{ gs_final-cgst_rate }</cgst_rate>| &&
                    |<cgst_amt>{ gs_final-cgst_amt }</cgst_amt>| &&
                    |<sgst_rate>{ gs_final-sgst_rate }</sgst_rate>| &&
                    |<sgst_amt>{ gs_final-sgst_amt }</sgst_amt>| &&
                    |<igst_rate>{ gs_final-igst_rate }</igst_rate>| &&
                    |<igst_amt>{ gs_final-igst_amt }</igst_amt>| &&
                    |<round_off>{ gs_final-round_off }</round_off>| &&
                    |<grand_total>{ gs_final-grand_total }</grand_total>| &&
                     |<no_of_qty>{  gs_final-no_of_qty }</no_of_qty>| &&
                    |<ItemData>|  ##NO_TEXT.

    DATA : lv_item TYPE string,
           lv_qlty TYPE string,
           srn     TYPE c LENGTH 3.

    CLEAR : lv_item , srn .
    LOOP AT gs_final-gt_item INTO DATA(ls_item).
      srn = srn + 1 .
      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<sl_num>{ srn }</sl_num>| &&
                |<item_po>{ ls_item-item_po }</item_po>| &&
                |<item_name>{ ls_item-item_name }</item_name>| &&
                |<item_load_wt>{ ls_item-item_load_wt }</item_load_wt>| &&
                |<item_net_wt>{ ls_item-item_net_wt }</item_net_wt>| &&
                |<item_deduction>{ ls_item-item_deduction }</item_deduction>| &&
                |<item_final_wt>{ ls_item-item_final_wt }</item_final_wt>| &&
                |<item_rate>{ ls_item-item_rate }</item_rate>| &&
                |<item_net_amt>{ ls_item-item_amt }</item_net_amt>| &&
                |<item_bill_qty1>{ ls_item-bill_qty1 }</item_bill_qty1>| &&
                |<item_order_qty1>{ ls_item-order_qty1 }</item_order_qty1>| &&
                |<item_item_amt1>{ ls_item-item_amt1 }</item_item_amt1>| &&
                |</ItemDataNode>|  ##NO_TEXT .
      CLEAR: ls_item.
    ENDLOOP.

    CLEAR : lv_qlty.
    LOOP AT gs_final-gt_qlty INTO DATA(ls_qlty).
      lv_qlty = |{ lv_qlty }| && |<QltyDataNode>| &&
                |<impurity>{ ls_qlty-impurity }</impurity>| &&
                |<target_qty>{ ls_qlty-target_qty }</target_qty>| &&
                |<anality>{ ls_qlty-anality }</anality>| &&
                |<result_qty>{ ls_qlty-result_qty }</result_qty>| &&
                |<result_amt>{ ls_qlty-result_amt }</result_amt>| &&
                |</QltyDataNode>| ##NO_TEXT.
      CLEAR : ls_qlty.
    ENDLOOP.
*IF IV_ACTION <> 'scnpkg'.
    lv_xml = |{ lv_xml }{ lv_item }| && |</ItemData>| &&
                       |<QltyData>| && |{ lv_qlty }| && |</QltyData>| &&
                       |</SCNNode>| &&
                       |</Form>| ##NO_TEXT .
* ENDIF.

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.
  ENDMETHOD.


  METHOD prep_xml_voucher_print.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    CLEAR: gt_final.
    gt_final[] = it_final[].

    READ TABLE gt_final INTO gs_final INDEX 1.

    DATA(lv_xml) =  |<Form>| &&
                    |<AccountDocumentNode>| &&
                    |<heading>{ gs_final-heading }</heading>| &&
                    |<sub_heading>{ gs_final-sub_heading }</sub_heading>| &&
                    |<header_1>{ gs_final-header_1 }</header_1>| &&
                    |<header_2>{ gs_final-header_2 }</header_2>| &&
                    |<header_3>{ gs_final-header_3 }</header_3>| &&
                    |<accounting_doc>{ gs_final-accountingdocument }</accounting_doc>| &&
                    |<fiscal_year>{ gs_final-fiscalyear }</fiscal_year>| &&
                    |<comp_code>{ gs_final-companycode }</comp_code>| &&
                    |<comp_name>{ gs_final-comp_name }</comp_name>| &&
                    |<comp_adrs1>{ gs_final-comp_adrs1 }</comp_adrs1>| &&
                    |<comp_adrs2>{ gs_final-comp_adrs2 }</comp_adrs2>| &&
                    |<comp_adrs3>{ gs_final-comp_adrs3 }</comp_adrs3>| &&
                    |<voucher_no>{ gs_final-voucher_no }</voucher_no>| &&
                    |<voucher_date>{ gs_final-voucher_date }</voucher_date>| &&
                    |<doc_type>{ gs_final-doc_type }</doc_type>| &&
                    |<doc_type_desc>{ gs_final-doc_type_desc }</doc_type_desc>| &&
                    |<ref_num>{ gs_final-ref_num }</ref_num>| &&
                    |<posting_date>{ gs_final-posting_date }</posting_date>| &&
                    |<doc_date>{ gs_final-doc_date }</doc_date>| &&
                    |<currency>{ gs_final-currency }</currency>| &&
                    |<park_by>{ gs_final-park_by }</park_by>| &&
                    |<posted_by>{ gs_final-posted_by }</posted_by>| &&
                    |<amt_words>{ gs_final-amt_words }</amt_words>| &&
                    |<sum_cr_amt>{ gs_final-sum_cr_amt }</sum_cr_amt>| &&
                    |<sum_dr_amt>{ gs_final-sum_dr_amt }</sum_dr_amt>| &&
                    |<sum_item_qty>{ gs_final-sum_item_qty }</sum_item_qty>| &&
                    |<ItemData>|  ##NO_TEXT.

    DATA : lv_item TYPE string,
           srn     TYPE c LENGTH 3.

    CLEAR : lv_item , srn .

    LOOP AT gs_final-gt_item INTO DATA(ls_item).
      srn = srn + 1 .

      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<sl_num>{ srn }</sl_num>| &&
                |<acc_doc>{ ls_item-accountingdocument }</acc_doc>| &&
                |<acc_doc_item>{ ls_item-acc_doc_item }</acc_doc_item>| &&
                |<bill_num>{ ls_item-bill_num }</bill_num>| &&
                |<posting_key>{ ls_item-posting_key }</posting_key>| &&
                |<gl_code>{ ls_item-gl_code }</gl_code>| &&
                |<gl_desc>{ ls_item-gl_desc }</gl_desc>| &&
                |<dr_amt>{ ls_item-dr_amt }</dr_amt>| &&
                |<cr_amt>{ ls_item-cr_amt }</cr_amt>| &&
                |<c_center>{ ls_item-c_center }</c_center>| &&
                |<plant>{ ls_item-plant }</plant>| &&
                |<assign_ref>{ ls_item-assign_ref }</assign_ref>| &&
                |<tax_code>{ ls_item-tax_code }</tax_code>| &&
                |<item_qty>{ ls_item-item_qty }</item_qty>| &&
                |<supplier_code>{ ls_item-supplier_code }</supplier_code>| &&
                |<supplier_name>{ ls_item-supplier_name }</supplier_name>| &&
                |<narration>{ ls_item-narration }</narration>| &&
                |</ItemDataNode>|  ##NO_TEXT .

      CLEAR: ls_item.
    ENDLOOP.

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                       |</AccountDocumentNode>| &&
                       |</Form>| ##NO_TEXT .

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.

  ENDMETHOD.
ENDCLASS.
