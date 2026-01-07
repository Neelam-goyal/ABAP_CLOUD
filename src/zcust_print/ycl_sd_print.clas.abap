CLASS ycl_sd_print DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA:
      gt_sodata  TYPE TABLE OF zstr_so_data,
      gt_deldata TYPE TABLE OF zstr_delivery_hdr,
      gt_quot    TYPE TABLE OF zstr_quotation_print.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sy_uname     TYPE c LENGTH 20.

    DATA:
      lv_char10  TYPE c LENGTH 10,
      lv_char4   TYPE c LENGTH 4,
      lv_char120 TYPE c LENGTH 120,
      sum_weight TYPE c LENGTH 80.


    METHODS:
      get_sales_data
        IMPORTING
                  iv_vbeln        LIKE lv_char10
                  iv_action       LIKE lv_char10
        RETURNING VALUE(et_final) LIKE gt_sodata,

      prep_xml_so_prnt
        IMPORTING
                  it_final             LIKE gt_sodata
                  iv_action            LIKE lv_char10
                  im_prntval           LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string,

      get_delivery_data
        IMPORTING
                  im_vbeln        LIKE lv_char10
                  im_plant        LIKE lv_char4
                  im_date         TYPE d
                  iv_action       LIKE lv_char10
        RETURNING VALUE(et_final) LIKE gt_deldata,

      prep_xml_delivery_prnt
        IMPORTING
                  it_final             LIKE gt_deldata
                  iv_action            LIKE lv_char10
                  iv_action1           LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string,

      "------------------------------------------ QUOTATION PRINT BEGIN
      get_quotation_data
        IMPORTING
                  im_vbeln        LIKE lv_char10
                  im_date         TYPE d
                  iv_action       LIKE lv_char10
        RETURNING VALUE(et_final) LIKE gt_quot,

      prep_xml_quotation_prnt
        IMPORTING
                  it_final             LIKE gt_quot
                  iv_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string.
    "------------------------------------------ QUOTATION PRINT END

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_sd_print IMPLEMENTATION.


  METHOD get_delivery_data.

    DATA : gt_del_hdr TYPE TABLE OF zstr_delivery_hdr,
           gs_del_hdr TYPE zstr_delivery_hdr,
           gt_del_itm TYPE TABLE OF zstr_delivery_itm,
           gs_del_itm TYPE zstr_delivery_itm.

    DATA : tot_qty TYPE p LENGTH 16 DECIMALS 2,
           tot_wt  TYPE p LENGTH 16 DECIMALS 2,
           broker  TYPE i_businesspartner-businesspartnername,
           qty_m   TYPE p LENGTH 16 DECIMALS 2,
           qty_u   TYPE p DECIMALS 2,
           qty_d   TYPE p DECIMALS 2,
           value   TYPE string.

    SELECT * FROM i_deliverydocument
             WHERE deliverydocument = @im_vbeln
             INTO TABLE @DATA(it_del_hdr).    "#EC CI_ALL_FIELDS_NEEDED

    IF it_del_hdr IS NOT INITIAL.

      SELECT
incotermsclassification,
language,
incotermsclassificationname
      FROM i_incotermsclassificationtext
                FOR ALL ENTRIES IN @it_del_hdr
                WHERE incotermsclassification = @it_del_hdr-incotermsclassification
                INTO TABLE @DATA(it_inco).         "#EC CI_NO_TRANSFORM

      SELECT
customer,
customername,
cityname
      FROM i_customer FOR ALL ENTRIES IN @it_del_hdr
               WHERE customer = @it_del_hdr-shiptoparty
               INTO TABLE @DATA(it_dstn).          "#EC CI_NO_TRANSFORM

      SELECT
        businesspartner,
        businesspartnername
      FROM i_businesspartner
               FOR ALL ENTRIES IN @it_del_hdr
               WHERE businesspartner = @it_del_hdr-soldtoparty
               INTO TABLE @DATA(it_party).         "#EC CI_NO_TRANSFORM

****************************** MM party Data **************************
      SELECT supplier,
             bpsuppliername
            FROM i_supplier
              FOR ALL ENTRIES IN @it_del_hdr    "LB
              WHERE supplier = @it_del_hdr-supplier        "DeliveryDocument
              INTO TABLE @DATA(it_m_party) .       "#EC CI_NO_TRANSFORM

      SELECT plant,
             plantname
            FROM i_plant
          FOR ALL ENTRIES IN @it_del_hdr
          WHERE plant = @it_del_hdr-receivingplant
          INTO TABLE @DATA(it_m_party1).           "#EC CI_NO_TRANSFORM

****************************** MM party Data **************************
      SELECT
      deliverydocument,
      deliverydocumentitem,
      plant,
orderdocument,
referencesddocument,
referencesddocumentitem,
deliverydocumentitemtext,
actualdeliveryquantity,
deliveryquantityunit,
actualdeliveredqtyinbaseunit,
itemnetweight,
storagelocation,
originaldeliveryquantity,
materialbycustomer,
material,
purchaseorder
      FROM i_deliverydocumentitem
                FOR ALL ENTRIES IN @it_del_hdr
                WHERE deliverydocument = @it_del_hdr-deliverydocument
                AND plant = @im_plant
                INTO TABLE @DATA(it_del_itm).      "#EC CI_NO_TRANSFORM

      IF it_del_itm[] IS NOT INITIAL.

        SELECT
        salesdocument,
        partnerfunction,
        customer,
        supplier
        FROM i_salesdocumentpartner
                FOR ALL ENTRIES IN @it_del_itm
                WHERE salesdocument = @it_del_itm-referencesddocument
                  AND partnerfunction = 'SP'
                INTO TABLE @DATA(it_sale).         "#EC CI_NO_TRANSFORM

********************************** MM inco Data *****************
        SELECT purchaseorder,
               incotermsclassification,
               supplier,
               supplyingplant
          FROM i_purchaseorderapi01
          FOR ALL ENTRIES IN @it_del_itm
           WHERE purchaseorder = @it_del_itm-purchaseorder
           INTO TABLE @DATA(it_m_inco).            "#EC CI_NO_TRANSFORM

        IF it_m_inco IS NOT INITIAL.

          SELECT supplier,
                 suppliername
           FROM i_supplier
           FOR ALL ENTRIES IN @it_m_inco
           WHERE supplier = @it_m_inco-supplier
           INTO TABLE @DATA(it_m_brokr).           "#EC CI_NO_TRANSFORM

          IF it_m_party IS INITIAL.
*  if it_m_inco is not INITIAL.

*    select plant,
*           plantname
*        from I_Plant
*      FOR ALL ENTRIES IN @it_m_inco
*      where plant = @it_m_inco-SupplyingPlant
*      into table @data(it_m_party1). "#EC CI_NO_TRANSFORM
*endif.
          ENDIF.

        ENDIF.

        SELECT purchaseorder,
               material,
               suppliermaterialnumber
              FROM i_purchaseorderitemapi01
              FOR ALL ENTRIES IN @it_del_itm
              WHERE purchaseorder = @it_del_itm-purchaseorder
                AND material = @it_del_itm-material
              INTO TABLE @DATA(it_m_brand).        "#EC CI_NO_TRANSFORM
                                                   "#EC CI_NO_TRANSFORM
********************************** MM inco Data *****************
***********************************MM QTY DATA*******************
        SELECT product,
               alternativeunit,
               quantitynumerator,
               quantitydenominator
               FROM i_productunitsofmeasure
               FOR ALL ENTRIES IN @it_del_itm
               WHERE product = @it_del_itm-material
               INTO TABLE @DATA(it_m_qty).         "#EC CI_NO_TRANSFORM

***********************************MM QTY DATA*******************
      ENDIF.

      IF it_sale[] IS NOT INITIAL.

        SELECT
        businesspartner,
        businesspartnername
        FROM i_businesspartner
                 FOR ALL ENTRIES IN @it_sale
                 WHERE businesspartner = @it_sale-supplier
                 INTO TABLE @DATA(it_brokr).       "#EC CI_NO_TRANSFORM

      ENDIF.

      IF it_del_itm IS NOT INITIAL.

        SELECT
salesdocument,
salesdocumentitem,
referencesddocument,
referencesddocumentitem,
materialbycustomer
        FROM i_salesdocumentitem
                 FOR ALL ENTRIES IN @it_del_itm
                 WHERE salesdocument = @it_del_itm-referencesddocument
                   AND salesdocumentitem = @it_del_itm-referencesddocumentitem
                   INTO TABLE @DATA(it_so).        "#EC CI_NO_TRANSFORM

        SELECT salesdocument,
               salesdocumentitem,
               conditiontype,
               conditionratevalue
             FROM i_salesdocitempricingelement
             FOR ALL ENTRIES IN @it_del_itm
             WHERE salesdocument = @it_del_itm-referencesddocument
               AND salesdocumentitem = @it_del_itm-referencesddocumentitem
               AND conditiontype = 'PMP0'
               INTO TABLE @DATA(it_rate).          "#EC CI_NO_TRANSFORM

      ENDIF.
    ENDIF.

    IF it_del_itm IS NOT INITIAL.

      READ TABLE it_del_itm INTO DATA(wa_del_itm) INDEX 1. "#EC CI_NOORDER

      SELECT SINGLE * FROM zi_plant_address
                WHERE plant = @wa_del_itm-plant
                INTO @DATA(wa_addr).          "#EC CI_ALL_FIELDS_NEEDED
    ENDIF.

    LOOP AT it_del_hdr INTO DATA(w_del_hdr).
*     shift w_del_hdr-DeliveryDocument LEFT DELETING LEADING '0'.
      gs_del_hdr-vechicle = w_del_hdr-yy1_vehiclenumber_dlh.
      gs_del_hdr-number = w_del_hdr-deliverydocument.
      gs_del_hdr-date = |{ w_del_hdr-deliverydate+6(2) }.{ w_del_hdr-deliverydate+4(2) }.{ w_del_hdr-deliverydate+0(4) }|.
      gs_del_hdr-lbor_cont = w_del_hdr-yy1_labourcontractor_dlh.
      gs_del_hdr-supvr_nam = w_del_hdr-yy1_supervisorname_dlh.
      gs_del_hdr-stchngby = w_del_hdr-yy1_stitchingby_dlh.
      gs_del_hdr-pckng = w_del_hdr-yy1_packingby_dlh.
*      gs_del_hdr-vch_no = w_del_hdr-YY1_VehicleNumber_DLH.
      IF w_del_hdr-deliverydocumenttype = 'LB'. "OR w_del_hdr-DeliveryDocumentType = 'NL'.
        READ TABLE it_m_party INTO DATA(wa_m_party) WITH KEY supplier = w_del_hdr-supplier.
        IF sy-subrc = 0.
          gs_del_hdr-party_nam = wa_m_party-bpsuppliername.
        ENDIF.
        gs_del_hdr-vch_no = 'PO'.
      ELSEIF w_del_hdr-deliverydocumenttype = 'NL'.
        READ TABLE it_m_party1 INTO DATA(wa_m_party1) WITH KEY plant = w_del_hdr-receivingplant.
        gs_del_hdr-party_nam = wa_m_party1-plantname.
      ELSE.
        READ TABLE it_party INTO DATA(wa_party) WITH KEY businesspartner = w_del_hdr-soldtoparty.
        gs_del_hdr-party_nam = wa_party-businesspartnername.

        READ TABLE it_inco INTO DATA(wa_inco) WITH KEY incotermsclassification = w_del_hdr-incotermsclassification.
        gs_del_hdr-inco_term = wa_inco-incotermsclassificationname.
        gs_del_hdr-vch_no = 'SO'.
      ENDIF.
      READ TABLE it_dstn INTO DATA(wa_dstn) WITH KEY customer = w_del_hdr-shiptoparty.
      gs_del_hdr-destination = wa_dstn-cityname.
      LOOP AT it_del_itm INTO DATA(w_del_itm) WHERE deliverydocument = w_del_hdr-deliverydocument.
        gs_del_itm-saleorder = w_del_itm-referencesddocument.
        gs_del_itm-item_name = w_del_itm-deliverydocumentitemtext.
*        gs_del_itm-qty       = w_del_itm-actualdeliveryquantity.
        gs_del_itm-unit = w_del_itm-deliveryquantityunit.
        gs_del_itm-disp_qty = w_del_itm-actualdeliveredqtyinbaseunit.
        IF w_del_hdr-deliverydocumenttype = 'LB' OR w_del_hdr-deliverydocumenttype = 'NL'.
          IF w_del_itm-deliveryquantityunit = 'ST'.
            gs_del_itm-unit = 'PC'.
          ENDIF.
          gs_del_itm-disp_wt = w_del_itm-originaldeliveryquantity.
          READ TABLE it_m_inco INTO DATA(w_m_inco) WITH KEY purchaseorder = w_del_itm-purchaseorder.
          gs_del_hdr-inco_term = w_m_inco-incotermsclassification.
          READ TABLE it_m_brand INTO DATA(w_m_brand) WITH KEY purchaseorder = w_del_itm-purchaseorder
                                                              material = w_del_itm-material.
          gs_del_itm-brand = w_m_brand-suppliermaterialnumber.
          READ TABLE it_m_qty INTO DATA(w_m_qty) WITH KEY product = w_del_itm-material.
          IF w_m_qty-alternativeunit = 'BAG' OR w_m_qty-alternativeunit = 'QTL' OR w_m_qty-alternativeunit = 'ST'.
            qty_m =  w_m_qty-quantitynumerator / w_m_qty-quantitydenominator.
*           gs_del_itm-qty = w_del_itm-OriginalDeliveryQuantity / qty_m.
            qty_u = w_del_itm-originaldeliveryquantity / qty_m.
            value = qty_u.
            SPLIT value  AT '.' INTO: DATA(var) DATA(var1).
            IF var1+0(1) LE '5'.
              gs_del_itm-qty = floor( qty_u ).
            ELSE.
              gs_del_itm-qty = ceil( qty_u ).
            ENDIF.
          ENDIF.
        ELSE.
*        gs_del_itm-qty       = w_del_itm-actualdeliveryquantity.
          qty_u = w_del_itm-actualdeliveryquantity.
          value = qty_u.
          SPLIT value  AT '.' INTO: DATA(var2) DATA(var3).
          IF var3+0(1) LE '5'.
            gs_del_itm-qty = floor( qty_u ).
          ELSE.
            gs_del_itm-qty = ceil( qty_u ).
          ENDIF.
          gs_del_itm-disp_wt = w_del_itm-itemnetweight / 100.
          gs_del_itm-brand = w_del_itm-materialbycustomer.
        ENDIF.
        gs_del_itm-rate =  w_del_itm-itemnetweight.
        gs_del_itm-godown = w_del_itm-storagelocation.

        tot_qty = tot_qty + gs_del_itm-qty."w_del_itm-actualdeliveryquantity.
        tot_wt = tot_wt +  gs_del_itm-disp_wt."w_del_itm-itemnetweight.
        READ TABLE it_so INTO DATA(wa_so) WITH KEY salesdocument = w_del_itm-referencesddocument
                                                   salesdocumentitem = w_del_itm-referencesddocumentitem.
        gs_del_itm-brand = wa_so-materialbycustomer.
        READ TABLE it_sale INTO DATA(w_sale) WITH KEY salesdocument = w_del_itm-referencesddocument
                                                      partnerfunction = 'SP'.

        READ TABLE it_rate INTO DATA(wa_rate) WITH KEY salesdocument = w_del_itm-referencesddocument
                                                   salesdocumentitem = w_del_itm-referencesddocumentitem
                                                   conditiontype = 'PMP0'.
        gs_del_itm-rate =  wa_rate-conditionratevalue.
        IF w_del_hdr-deliverydocumenttype = 'LB' OR w_del_hdr-deliverydocumenttype = 'NL'.
          READ TABLE it_m_brokr INTO DATA(w_m_brokr) WITH KEY supplier = w_m_inco-supplier.
          broker = w_m_brokr-suppliername.
        ELSE.
          READ TABLE it_brokr INTO DATA(w_brokr) WITH KEY businesspartner = w_sale-supplier.
          broker = w_brokr-businesspartnername.
        ENDIF.
        APPEND gs_del_itm TO gt_del_itm.
        CLEAR : gs_del_itm, qty_m .
      ENDLOOP.
      DELETE gt_del_itm WHERE qty IS INITIAL.
      gs_del_hdr-brokr_nam = broker.
      gs_del_hdr-tot_disp_qty = tot_qty.
      gs_del_hdr-tot_disp_wt = tot_wt.
      gs_del_hdr-plant_nam = wa_addr-plantname.
      gs_del_hdr-plant_addr = wa_addr-streetname.
      INSERT LINES OF gt_del_itm INTO TABLE gs_del_hdr-del_itm.
      APPEND gs_del_hdr TO gt_del_hdr.
      CLEAR gs_del_hdr.
    ENDLOOP.

    et_final[] = gt_del_hdr[].
  ENDMETHOD.


  METHOD get_quotation_data.
    DATA: gt_qothdr TYPE TABLE OF zstr_quotation_print,
          gs_qothdr TYPE zstr_quotation_print,
          gt_item   TYPE TABLE OF zstr_quotation_print_itm,
          gs_item   TYPE zstr_quotation_print_itm.

    SELECT * FROM i_salesdocument
             WHERE salesdocument = @im_vbeln
             INTO TABLE @DATA(lt_qothdr).     "#EC CI_ALL_FIELDS_NEEDED

    IF lt_qothdr IS NOT INITIAL.

      SELECT * FROM i_salesdocumentitem
               WHERE salesdocument = @im_vbeln
               INTO TABLE @DATA(lt_soitem).   "#EC CI_ALL_FIELDS_NEEDED

      SELECT * FROM i_salesdocitempricingelement
               WHERE salesdocument = @im_vbeln
               INTO TABLE @DATA(lt_soitem_price). "#EC CI_ALL_FIELDS_NEEDED

      SELECT * FROM zi_so_partner
               WHERE salesdocument = @im_vbeln
               INTO TABLE @DATA(lt_sopart).   "#EC CI_ALL_FIELDS_NEEDED

      SELECT * FROM i_salesquotationitemtp WHERE salesquotation = @im_vbeln INTO TABLE @DATA(i_quo_temp). "#EC CI_ALL_FIELDS_NEEDED
    ENDIF.

    LOOP AT lt_qothdr INTO DATA(ls_qothdr).

      gs_qothdr-no_date = ls_qothdr-salesdocument && ' ' && ',' && ' ' && ls_qothdr-salesdocumentdate+6(2) && '.' && ls_qothdr-salesdocumentdate+4(2) && '.' && ls_qothdr-salesdocumentdate+0(4).

      SELECT SINGLE
      customerpaymentterms,
      language,
      customerpaymenttermsname
      FROM i_customerpaymenttermstext
                      WHERE customerpaymentterms = @ls_qothdr-customerpaymentterms
                        AND language = 'E'
                      INTO @DATA(ls_payterm_desc). "#EC CI_NO_TRANSFORM


      DATA : lv_shp_adr1 TYPE c LENGTH 120,
             lv_shp_adr2 TYPE c LENGTH 120,
             lv_shp_adr3 TYPE c LENGTH 120,
             lv_shp_adr4 TYPE c LENGTH 120.

      DATA : lv_sold_adr1 TYPE c LENGTH 120,
             lv_sold_adr2 TYPE c LENGTH 120,
             lv_sold_adr3 TYPE c LENGTH 120,
             lv_sold_adr4 TYPE c LENGTH 120.

      READ TABLE lt_sopart INTO DATA(ls_sopart) WITH KEY salesdocument = ls_qothdr-salesdocument.
      IF ls_sopart-we_street IS NOT INITIAL .
        IF lv_shp_adr1 IS NOT INITIAL   .
          lv_shp_adr1 = |{ lv_shp_adr1 }{ ls_sopart-we_street }{ ls_sopart-we_streetprefixname1 }{ ls_sopart-we_streetprefixname2 }{ ls_sopart-we_streetsuffixname1 }| .
        ELSE .
          lv_shp_adr1 = |{ ls_sopart-we_street }{ ls_sopart-we_streetprefixname1 }{ ls_sopart-we_streetprefixname2 }{ ls_sopart-we_streetsuffixname1 }| .
        ENDIF .
      ENDIF .

      IF ls_sopart-we_street1 IS NOT INITIAL .
        IF lv_shp_adr1 IS NOT INITIAL   .
          lv_shp_adr1 = |{ lv_shp_adr1 }{ ls_sopart-we_street1 }{ ls_sopart-we_streetprefixname1 }{ ls_sopart-we_streetprefixname2 }{ ls_sopart-we_streetsuffixname1 }| .
        ELSE .
          lv_shp_adr1 = |{ ls_sopart-we_street1 }{ ls_sopart-we_streetprefixname1 }{ ls_sopart-we_streetprefixname2 }{ ls_sopart-we_streetsuffixname1 }| .
        ENDIF .
      ENDIF .

      DATA(len) = strlen( lv_shp_adr1 ) .
      len = len - 40.
      IF strlen( lv_shp_adr1 ) GT 40 .
        lv_shp_adr2 = |{ lv_shp_adr1+40(len) },| .
        lv_shp_adr1 = lv_shp_adr1+0(40) .
      ENDIF .

      READ TABLE lt_sopart INTO DATA(ls_sopart_ag) WITH KEY salesdocument = ls_qothdr-salesdocument.
      IF ls_sopart_ag-ag_street IS NOT INITIAL .
        IF lv_sold_adr1 IS NOT INITIAL   .
          lv_sold_adr1 = |{ lv_sold_adr1 }{ ls_sopart_ag-ag_street }{ ls_sopart_ag-ag_streetprefixname1 }|."{ ls_sopart_ag-ag_streetprefixname2 }{ ls_sopart_ag-ag_streetsuffixname1 }| .
        ELSE .
          lv_sold_adr1 = |{ ls_sopart_ag-ag_street }{ ls_sopart_ag-ag_streetprefixname1 }|."{ ls_sopart_ag-ag_streetprefixname2 }{ ls_sopart_ag-ag_streetsuffixname1 }| .
        ENDIF .
      ENDIF .

      IF ls_sopart_ag-ag_street1 IS NOT INITIAL .
        IF lv_sold_adr1 IS NOT INITIAL   .
          lv_sold_adr1 = |{ lv_sold_adr1 }{ ls_sopart_ag-ag_street1 }{ ls_sopart_ag-ag_streetprefixname1 }|."{ ls_sopart_ag-ag_streetprefixname2 }{ ls_sopart_ag-ag_streetsuffixname1 }| .
        ELSE .
          lv_sold_adr1 = |{ ls_sopart_ag-ag_street1 }{ ls_sopart_ag-ag_streetprefixname1 }| ."{ ls_sopart_ag-ag_streetprefixname2 }{ ls_sopart_ag-ag_streetsuffixname1 }| .
        ENDIF .
      ENDIF .

      ""comment by sb 12/01/2025""
*****      DATA(len_ag) = strlen( lv_sold_adr1 ) .
*****      len_ag = len_ag - 40.
*****      IF strlen( lv_sold_adr1 ) GT 40 .
*****        lv_sold_adr2 = |{ lv_sold_adr1+40(len_ag) }| .
*****        lv_sold_adr1 = lv_sold_adr1+0(40) .
*****      ENDIF .
      ""comment by sb 12/01/2025""
      SELECT SINGLE
            country,
            language,
            countryname,
            nationalityname,
            nationalitylongname,
            countryshortname
      FROM i_countrytext   WHERE country = @ls_sopart_ag-ag_country AND language = 'E'
      INTO @DATA(lv_cn_nm).                        "#EC CI_NO_TRANSFORM

      SELECT SINGLE
        country,
        language,
        region,
        regionname
      FROM i_regiontext  WHERE region = @ls_sopart_ag-ag_region AND language = 'E' AND country = @ls_sopart_ag-ag_country
      INTO @DATA(lv_st_name_ag).                   "#EC CI_NO_TRANSFORM

*      ""add by sb10162025"
      SELECT SINGLE phonenumber1
      FROM zi_customer_address
      WHERE customername = @ls_sopart_ag-ag_name
      INTO @DATA(phone).

      lv_sold_adr3 = ls_sopart_ag-ag_city && '-' && ls_sopart_ag-ag_pin && '' && lv_cn_nm-countryname && '-'
      && phone  && '-' &&  ls_sopart_ag-ag_email .
      lv_sold_adr4 = lv_st_name_ag-regionname && '-'
      && phone && '-' &&  ls_sopart_ag-ag_email
      . "ls_sopart_ag-ag_region && '(' && lv_st_name_ag-RegionName && ')'.
*      ""add by sb10162025"

      gs_qothdr-consig_1     = ls_sopart_ag-ag_name.
      gs_qothdr-consig_2     = lv_sold_adr1.
*****      gs_qothdr-consig_3      = lv_sold_adr2. comment by sb 12/02/2025
      gs_qothdr-consig_3      = |{ ls_sopart_ag-ag_streetprefixname2 }{ ls_sopart_ag-ag_streetsuffixname1 }|.
      gs_qothdr-consig_4      = lv_sold_adr3.
      IF gs_qothdr-consig_3 IS INITIAL.
        gs_qothdr-consig_3 = gs_qothdr-consig_4.
        CLEAR:gs_qothdr-consig_4.
      ENDIF.


      SELECT SINGLE
     country,
     language,
     countryname,
     nationalityname,
     nationalitylongname,
     countryshortname
      FROM i_countrytext   WHERE country = @ls_sopart-we_country AND language = 'E'
      INTO @DATA(lv_cn_nm1).                       "#EC CI_NO_TRANSFORM

      SELECT SINGLE
      country,
      language,
      region,
      regionname
      FROM i_regiontext  WHERE region = @ls_sopart-we_region AND language = 'E' AND country = @ls_sopart-we_country
      INTO @DATA(lv_st_name_we1).                  "#EC CI_NO_TRANSFORM

      lv_shp_adr3 = ls_sopart-we_city && '-' && ls_sopart-we_pin && ',' && lv_cn_nm1-countryname.
      lv_shp_adr4 = lv_st_name_we1-regionname. "ls_sopart-we_region && '(' && lv_st_name_we1-RegionName && ')'.


      IF ls_qothdr-purchaseorderbycustomer IS INITIAL AND ls_qothdr-customerpurchaseorderdate IS NOT INITIAL.
        gs_qothdr-buy_order_date = ls_qothdr-customerpurchaseorderdate.
      ELSEIF ls_qothdr-purchaseorderbycustomer IS NOT INITIAL AND ls_qothdr-customerpurchaseorderdate IS INITIAL.
        gs_qothdr-buy_order_date = ls_qothdr-purchaseorderbycustomer.
      ELSEIF ls_qothdr-purchaseorderbycustomer IS NOT INITIAL AND ls_qothdr-customerpurchaseorderdate IS NOT INITIAL.
        gs_qothdr-buy_order_date = ls_qothdr-purchaseorderbycustomer && ',' &&  ls_qothdr-customerpurchaseorderdate.
      ENDIF.


      READ TABLE lt_qothdr INTO DATA(w_qot) INDEX 1.
      gs_qothdr-comp_code = w_qot-billingcompanycode.
      IF w_qot-billingcompanycode = '1000'.
        gs_qothdr-header_blv = 'Bhagwati Lacto Vegetarian Exports(P) Ltd.' ##NO_TEXT.
        gs_qothdr-shipper_1 = 'BHAGWATI LACTO VEGETARIAN EXPORTS PVT. LTD.' ##NO_TEXT.
        gs_qothdr-shipper_2 = 'Regd. Office : 18-1/2 ANAJ MANDI,' ##NO_TEXT.
        gs_qothdr-shipper_3 = 'FEROZEPUR CANTT. 152001 Firozepur Punjab' ##NO_TEXT.
        gs_qothdr-shipper_4 = 'TELEFAX : +91 73475-66565' ##NO_TEXT.
      ELSEIF w_qot-billingcompanycode = '2000'.
        gs_qothdr-shipper_1 = 'BHAGWATI LACTO FOODS PVT. LTD.' ##NO_TEXT.
*        gs_qothdr-shipper_2 = 'VILL. RUKNA MUNGLA, FARIDKOT ROAD, FEROZEPUR,' ##NO_TEXT.

******************************ADDED BY Madhur****************************************

        gs_qothdr-shipper_2 = 'VILLAGE RUKNA MUNGLA, FARIDKOT ROAD,P.O.PATEL NAGAR' ##NO_TEXT.
        gs_qothdr-shipper_3 =  'FEROZPUR(PB), 152001 ,INDIA' ##NO_TEXT.
        gs_qothdr-shipper_4 = 'TELEFAX : +91 1632-244765' ##NO_TEXT.

*/  ***********************************************************************************

        gs_qothdr-header_blv = 'Bhagwati Lacto Foods Pvt Ltd.' ##NO_TEXT.
      ELSEIF w_qot-billingcompanycode = '3000'.
      ELSEIF w_qot-billingcompanycode = '4000'.
        gs_qothdr-header_blv = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
        gs_qothdr-shipper_1 = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
        gs_qothdr-shipper_2 = 'M-14A VILL-Mohasa Babai, Makhan Nagar' ##NO_TEXT.
        gs_qothdr-shipper_3 = 'MAKHAN NAGAR - 461661 MP - INDIA' ##NO_TEXT.
        gs_qothdr-shipper_4 = ' ' ##NO_TEXT.
      ENDIF.

*      IF ls_sopart_ag-ag_code <> ls_sopart-ship_to_party.
*        gs_qothdr-notify_party_21 = gs_qothdr-consig_1     .
*        gs_qothdr-notify_party_22 = gs_qothdr-consig_2     .
*        gs_qothdr-notify_party_23 = gs_qothdr-consig_3     .
*        gs_qothdr-notify_party_24 = gs_qothdr-consig_4     .
*        "   CLEAR: gs_qothdr-consig_1, gs_qothdr-consig_2, gs_qothdr-consig_3, gs_qothdr-consig_4.
*      ENDIF.


      IF w_qot-billingcompanycode = '1000'.

        gs_qothdr-bank_uco     = 'UCO BANK, MIDCORPORATE BRANCH (2066)' ##NO_TEXT.
        gs_qothdr-bank_near    = 'R.K. ROAD, INDUSTRIAL AREA, LUDHIANA (PB) ' ##NO_TEXT.
        gs_qothdr-bank_city    = 'LUDHIANA-141001 (PB) INDIA' ##NO_TEXT.
        gs_qothdr-bank_account = 'A/C NO.20660510000250' ##NO_TEXT.
        gs_qothdr-bank_swift   = 'SWIFT code : UCBAINBB216' ##NO_TEXT.
        gs_qothdr-count_of_origin = 'INDIA'.

        gs_qothdr-footer1 = 'Government Recognised 3 Star Export House' ##NO_TEXT.
        gs_qothdr-footer2 = '18 1/2 , Old Anal Mandi,Ferozepur Cantt.-152001 Punjab,India' ##NO_TEXT.
        gs_qothdr-footer3 = 'Works : Rural Focal Point,VPO Mana Singh Wala,Distt. Ferozepur,Punjab,India' ##NO_TEXT.
        gs_qothdr-footer4 = 'Tele-fax.: +91-1632-244765' ##NO_TEXT.
        gs_qothdr-footer5 = 'Info@blvxports.com  Website : www.blvxports.com' ##NO_TEXT.

      ELSEIF w_qot-billingcompanycode = '2000'.



        gs_qothdr-bank_uco     = 'UCO BANK, MIDCORPORATE BRANCH (2066)' ##NO_TEXT.
        gs_qothdr-bank_near    = 'R.K. ROAD, INDUSTRIAL AREA, LUDHIANA (PB) ' ##NO_TEXT.
        gs_qothdr-bank_city    = 'LUDHIANA-141001 (PB) INDIA' ##NO_TEXT.
        gs_qothdr-bank_account = 'A/C NO.20660210001981' ##NO_TEXT.
        gs_qothdr-bank_swift   = 'SWIFT code : UCBAINBB216 AD CODE : 0320541' ##NO_TEXT.

        gs_qothdr-count_of_origin = 'INDIA'.
*        gs_qothdr-footer1 = 'Government Recognised 3 Star Export House' ##NO_TEXT.
        gs_qothdr-footer1 = 'Address:Vill.Rukna Mungla, Faridokt Road, Ferozeour (pb) 152001' ##NO_TEXT.
*        gs_qothdr-footer2 = 'Address:Vill.Rukna Mungla, Faridokt Road, Ferozeour (pb) 152001' ##NO_TEXT.
*        gs_qothdr-footer2 = 'Vill.Rukna Mungla, Faridkot Road, Ferozpur (Pb)152001' ##NO_TEXT.
*        gs_qothdr-footer3 = 'Works : Distt. Ferozepur,Punjab,India' ##NO_TEXT. "Rural Focal Point,VPO Mana Singh Wala,
*        gs_qothdr-footer4 = 'Tele-fax.: +91-1632-244765' ##NO_TEXT.
*        gs_qothdr-footer5 = 'Info@blvxports.com  Website : www.blvxports.com' ##NO_TEXT.

      ENDIF.





      " gs_qothdr-delivery = ls_qothdr-sal

      SELECT * FROM zsd_txtid_field
       WHERE documentnumber =  @ls_qothdr-salesdocument INTO TABLE @DATA(i_txt).
      READ TABLE i_txt INTO DATA(w_txt) INDEX 1.        "#EC CI_NOORDER
      gs_qothdr-pre_carr       =  w_txt-pre_carrier.
      gs_qothdr-vessel         =  w_txt-vessal.
      gs_qothdr-port_of_disc   =  w_txt-port_of_discharge.
      gs_qothdr-port_of_loading   =  w_txt-port_of_loading.
      gs_qothdr-final_dest     =  w_txt-final_destination.
      gs_qothdr-recp_by_pre_carr =  w_txt-pre_carig_by.
      gs_qothdr-count_of_final_dest =  w_txt-cntry_of_fdest.

      SELECT SINGLE
      a~customername,
      a~streetname,
      a~streetprefixname1,
      a~streetprefixname2,
      a~streetsuffixname1,
      a~country,
      a~cityname,
      a~postalcode,
      a~phonenumber1,
      a~emailaddress,
      b~countryname
      FROM zi_customer_address AS a
      LEFT JOIN i_countrytext  AS b ON  a~country = b~country AND  b~language = 'E'
      WHERE customer = @w_txt-notify_party1
      INTO @DATA(ls_notify_adrs1).

      gs_qothdr-notify_party_1 = ls_notify_adrs1-customername.
*      gs_qothdr-notify_party_2 = ls_notify_adrs1-streetname . comment by sb15102025
*add by sb15102025""""""""""""""""
      CONCATENATE
       ls_notify_adrs1-streetname
       ls_notify_adrs1-streetprefixname1
***       ls_notify_adrs1-streetprefixname2
       INTO gs_qothdr-notify_party_2
       SEPARATED BY space.

      IF ls_notify_adrs1-cityname IS INITIAL AND ls_notify_adrs1-postalcode IS NOT INITIAL.
        CONCATENATE
         ls_notify_adrs1-streetprefixname2
*****         ls_notify_adrs1-postalcode
         ls_notify_adrs1-streetsuffixname1
*****         ls_notify_adrs1-countryname
*****         ls_notify_adrs1-phonenumber1
*****         ls_notify_adrs1-emailaddress
         INTO  gs_qothdr-notify_party_3
          SEPARATED BY space.

      ELSEIF ls_notify_adrs1-cityname IS NOT INITIAL AND ls_notify_adrs1-postalcode IS INITIAL.
        CONCATENATE
        ls_notify_adrs1-streetprefixname2
*****       ls_notify_adrs1-cityname
       ls_notify_adrs1-streetsuffixname1
*****       ls_notify_adrs1-countryname
*****       ls_notify_adrs1-phonenumber1
*****       ls_notify_adrs1-emailaddress
       INTO  gs_qothdr-notify_party_3
        SEPARATED BY space.
      ELSEIF ls_notify_adrs1-cityname IS NOT INITIAL AND ls_notify_adrs1-postalcode IS NOT INITIAL.
        CONCATENATE
        ls_notify_adrs1-streetprefixname2
*****        ls_notify_adrs1-cityname '-'  ls_notify_adrs1-postalcode
        ls_notify_adrs1-streetsuffixname1
*****        ls_notify_adrs1-countryname
*****        ls_notify_adrs1-phonenumber1
*****       ls_notify_adrs1-emailaddress
         INTO  gs_qothdr-notify_party_3
         SEPARATED BY space. .
      ENDIF.

      CONCATENATE
      ls_notify_adrs1-cityname '-'  ls_notify_adrs1-postalcode
      ls_notify_adrs1-countryname
      ls_notify_adrs1-phonenumber1
      ls_notify_adrs1-emailaddress
       INTO  gs_qothdr-notify_party_4
       SEPARATED BY space.

      IF gs_qothdr-notify_party_3 IS INITIAL.
        gs_qothdr-notify_party_3 = gs_qothdr-notify_party_4.
        CLEAR:gs_qothdr-notify_party_4.
      ENDIF.

      """"""""""add by sb 12/02/2025""""""""""all address"""

*      IF ls_notify_adrs1-cityname IS INITIAL AND ls_notify_adrs1-postalcode IS NOT INITIAL.
*        gs_qothdr-notify_party_3 = ls_notify_adrs1-postalcode.
*      ELSEIF ls_notify_adrs1-cityname IS NOT INITIAL AND ls_notify_adrs1-postalcode IS INITIAL.
*        gs_qothdr-notify_party_3 =  ls_notify_adrs1-cityname .
*      ELSEIF ls_notify_adrs1-cityname IS NOT INITIAL AND ls_notify_adrs1-postalcode IS NOT INITIAL.
*        gs_qothdr-notify_party_3 =  ls_notify_adrs1-cityname && '-' && ls_notify_adrs1-postalcode.
*      ENDIF.

      CLEAR:ls_notify_adrs1.
      SELECT SINGLE
      a~customername,
      a~streetname,
      a~streetprefixname1,
      a~streetprefixname2,
      a~streetsuffixname1,
      a~country,
      a~cityname,
      a~postalcode,
      a~phonenumber1,
      a~emailaddress,
      b~countryname
     FROM zi_customer_address AS a
     LEFT JOIN i_countrytext  AS b ON  a~country = b~country AND  b~language = 'E'
     WHERE customer = @w_txt-notify_party2
     INTO @ls_notify_adrs1.

      gs_qothdr-notify_party_31 = ls_notify_adrs1-customername.
*      gs_qothdr-notify_party_32 = ls_notify_adrs1-streetname . ""comment by sb 15102025
* *add by sb15102025""""""""""""""""
      CONCATENATE
       ls_notify_adrs1-streetname
       ls_notify_adrs1-streetprefixname1
       INTO gs_qothdr-notify_party_32
       SEPARATED BY space.

      IF ls_notify_adrs1-cityname IS INITIAL AND ls_notify_adrs1-postalcode IS NOT INITIAL.
        CONCATENATE ls_notify_adrs1-streetprefixname2
*****                    ls_notify_adrs1-postalcode
                    ls_notify_adrs1-streetsuffixname1
*****                    ls_notify_adrs1-countryname
*****                    ls_notify_adrs1-phonenumber1
*****                    ls_notify_adrs1-emailaddress
                   INTO gs_qothdr-notify_party_33
                   SEPARATED BY space.
      ELSEIF ls_notify_adrs1-cityname IS NOT INITIAL AND ls_notify_adrs1-postalcode IS INITIAL.
        CONCATENATE  ls_notify_adrs1-streetprefixname2
*****                     ls_notify_adrs1-cityname
*****                     ls_notify_adrs1-postalcode
                     ls_notify_adrs1-streetsuffixname1
*****                     ls_notify_adrs1-countryname
*****                     ls_notify_adrs1-phonenumber1
*****                     ls_notify_adrs1-emailaddress
                     INTO gs_qothdr-notify_party_33
                     SEPARATED BY space.
      ELSEIF ls_notify_adrs1-cityname IS NOT INITIAL AND ls_notify_adrs1-postalcode IS NOT INITIAL.
        CONCATENATE  ls_notify_adrs1-streetprefixname2
*****                     ls_notify_adrs1-cityname
*****                     ls_notify_adrs1-postalcode
                     ls_notify_adrs1-streetsuffixname1
*****                     ls_notify_adrs1-countryname
*****                     ls_notify_adrs1-phonenumber1
*****                     ls_notify_adrs1-emailaddress
                     INTO gs_qothdr-notify_party_33
                     SEPARATED BY space.
      ENDIF.



      CONCATENATE
                   ls_notify_adrs1-cityname
                   ls_notify_adrs1-postalcode
                   ls_notify_adrs1-countryname
                   ls_notify_adrs1-phonenumber1
                   ls_notify_adrs1-emailaddress
                   INTO gs_qothdr-notify_party_34.

      IF gs_qothdr-notify_party_33 IS INITIAL.
        gs_qothdr-notify_party_33 = gs_qothdr-notify_party_34.
        CLEAR:gs_qothdr-notify_party_34.
      ENDIF.

*add by sb15102025 """""""""""""""""""""
*      IF ls_notify_adrs1-cityname IS INITIAL AND ls_notify_adrs1-postalcode IS NOT INITIAL.
*        gs_qothdr-notify_party_33 =  ls_notify_adrs1-postalcode.
*      ELSEIF ls_notify_adrs1-cityname IS NOT INITIAL AND ls_notify_adrs1-postalcode IS INITIAL.
*        gs_qothdr-notify_party_33 =  ls_notify_adrs1-cityname .
*      ELSEIF ls_notify_adrs1-cityname IS NOT INITIAL AND ls_notify_adrs1-postalcode IS NOT INITIAL.
*        gs_qothdr-notify_party_33 =  ls_notify_adrs1-cityname && '-' && ls_notify_adrs1-postalcode.
*      ENDIF.

      CLEAR:ls_notify_adrs1.
      SELECT SINGLE
     customername,
     streetname,
     cityname,
     postalcode
     FROM zi_customer_address
     WHERE customer = @w_txt-buyer_oth_consinee
     INTO @ls_notify_adrs1.

      gs_qothdr-notify_party_21 = ls_notify_adrs1-customername.
      gs_qothdr-notify_party_22 = ls_notify_adrs1-streetname .
      IF ls_notify_adrs1-cityname IS INITIAL AND ls_notify_adrs1-postalcode IS NOT INITIAL.
        gs_qothdr-notify_party_23 =  ls_notify_adrs1-postalcode.
      ELSEIF ls_notify_adrs1-cityname IS NOT INITIAL AND ls_notify_adrs1-postalcode IS INITIAL.
        gs_qothdr-notify_party_23 =  ls_notify_adrs1-cityname .
      ELSEIF ls_notify_adrs1-cityname IS NOT INITIAL AND ls_notify_adrs1-postalcode IS NOT INITIAL.
        gs_qothdr-notify_party_23 =  ls_notify_adrs1-cityname && '-' && ls_notify_adrs1-postalcode.
      ENDIF.



      gs_qothdr-delivery = |{ ls_qothdr-incotermsclassification } ({ ls_qothdr-incotermstransferlocation })|.

*          SELECT SINGLE  IncotermsClassificationName from i_incotermsclassificationtext
*                        where IncotermsClassification = @ls_qothdr-IncotermsClassification into @gs_qothdr-delivery.
      SELECT SINGLE paymenttermsconditiondesc FROM i_paymenttermsconditionstext WHERE paymentterms = @ls_qothdr-customerpaymentterms
                   INTO @gs_qothdr-payment.

      gs_qothdr-coc_test_rep = ls_qothdr-yy1_salesremark_sdh .
      DATA:lv_dec TYPE p DECIMALS 0.


      LOOP AT lt_soitem INTO DATA(ls_souitem) WHERE salesdocument = ls_qothdr-salesdocument.
        READ TABLE i_txt INTO w_txt WITH KEY documentitem = ls_souitem-salesdocumentitem.
        IF sy-subrc IS INITIAL.
          gs_item-marks_nos = w_txt-marks_no .
        ENDIF.
        READ TABLE i_quo_temp INTO DATA(w_quo_temp) WITH KEY salesquotationitem = ls_souitem-salesdocumentitem.
        IF sy-subrc IS INITIAL.
          gs_item-marks_nos = w_quo_temp-yy1_marksnos_sdi.
          CONDENSE w_quo_temp-yy1_noofpackage_sdi.
***          CONCATENATE w_quo_temp-yy1_noofpackage_sdi ' Bags,  '  cl_abap_char_utilities=>newline w_quo_temp-yy1_kindofpacking_sdi INTO gs_item-kind_of_pack SEPARATED BY ' ' ##NO_TEXT .
***        added by neelam/ arvind kushwah
          CONCATENATE w_quo_temp-yy1_noofpackage_sdi ','  cl_abap_char_utilities=>newline w_quo_temp-yy1_kindofpacking_sdi INTO gs_item-kind_of_pack SEPARATED BY ' ' ##NO_TEXT .
*         gs_item-kind_of_pack = W_QUO_TEMP-YY1_Noofpackage_SDI && ' & ' && W_QUO_TEMP-YY1_KindofPacking_SDI.
          gs_item-fcl = w_quo_temp-yy1_fcl_sdi.
        ENDIF.

        gs_item-brand = ls_souitem-materialbycustomer.

        gs_item-desc_of_goods = ls_souitem-salesdocumentitemtext.
        gs_item-quantity = ls_souitem-orderquantity.
        gs_item-amount = ls_souitem-netamount . "gs_item-rate * gs_item-quantity.  by Vishaltyagi

        READ TABLE lt_soitem_price INTO DATA(wa_price)
         WITH KEY salesdocument = ls_souitem-salesdocument
                  salesdocumentitem = ls_souitem-salesdocumentitem
                  conditiontype = 'PMP0'.
        IF sy-subrc IS INITIAL.
          gs_item-rate = gs_item-amount /  gs_item-quantity. "wa_price-conditionratevalue.
        ENDIF.



        gs_qothdr-tot_qty += gs_item-quantity.
        gs_qothdr-tot_fcl += gs_item-fcl.
        gs_qothdr-tot_amt +=  gs_item-amount.

        gs_qothdr-tot_net_wt =  gs_qothdr-tot_net_wt + ls_souitem-itemnetweight .
        gs_qothdr-tot_gross_wt =  gs_qothdr-tot_gross_wt + ls_souitem-itemgrossweight.
***        gs_qothdr-tot_no_pkgs =  gs_qothdr-tot_no_pkgs + w_quo_temp-yy1_noofpackage_sdi.
        IF iv_action = 'quotprnt'.

          DATA: lv_extracted_number          TYPE p DECIMALS 0. " To hold 500
          DATA: lv_extracted_unit_text       TYPE string.        " To hold 'boxes'

* Helper variables for string processing
          DATA: lv_offset       TYPE i.
          DATA: lv_input_len    TYPE i.
          IF w_quo_temp-yy1_noofpackage_sdi CA 'Boxes' OR w_quo_temp-yy1_noofpackage_sdi CA 'Bags' OR
          w_quo_temp-yy1_noofpackage_sdi CA 'Box' OR w_quo_temp-yy1_noofpackage_sdi CA 'Bag' .

            " Clear variables for current iteration
            CLEAR: lv_extracted_number, lv_extracted_unit_text, lv_offset.
            lv_input_len = strlen( w_quo_temp-yy1_noofpackage_sdi ).

            " Find the end of the numeric part (first non-numeric character)
            DO.
              lv_offset = sy-index - 1. " sy-index starts from 1, so adjust for 0-based offset
              IF lv_offset >= lv_input_len.
                EXIT. " Reached end of string
              ENDIF.
              IF w_quo_temp-yy1_noofpackage_sdi+lv_offset(1) CO '0123456789'.
                CONTINUE. " Current character is numeric, keep going
              ELSE.
                EXIT. " Found the first non-numeric character (e.g., space or 'b')
              ENDIF.
            ENDDO.
            IF lv_offset > 0. " If a numeric part was found at the beginning
              lv_extracted_number = w_quo_temp-yy1_noofpackage_sdi(lv_offset). " e.g., '500' -> 500
              lv_extracted_unit_text = w_quo_temp-yy1_noofpackage_sdi+lv_offset. " e.g., ' boxes'
              CONDENSE lv_extracted_unit_text. " Remove leading/trailing spaces -> 'boxes'

              " --- Sum the numeric value ---
              gs_qothdr-tot_no_pkgs = gs_qothdr-tot_no_pkgs + lv_extracted_number.
            ENDIF.

          ENDIF.
        ENDIF.

        APPEND gs_item TO gt_item.
        CLEAR : gs_item, ls_souitem.
      ENDLOOP.
      "      CONCATENATE gs_qothdr-tot_no_pkgs 'Bags' into gs_qothdr-tot_no_pkgs SEPARATED BY ' ' .
      DATA:
        lo_amt_words TYPE REF TO zcl_amt_words.

      CREATE OBJECT lo_amt_words.

      DATA : lv_grand_tot_word TYPE string.

      lv_grand_tot_word = gs_qothdr-tot_amt .

      lo_amt_words->number_to_words(
       EXPORTING
         iv_num   = lv_grand_tot_word
       RECEIVING
         rv_words = DATA(grand_tot_amt_words)
     ).

      grand_tot_amt_words = |{ grand_tot_amt_words } Only. | ##NO_TEXT.
      gs_qothdr-amount_chargeable = grand_tot_amt_words.

      IF ls_qothdr-transactioncurrency = 'USD'.
**""""""add by sb16102025"""""""""

***commented by neelam 27.nov.2025:arvind sd
*        lv_grand_tot_word  = gs_qothdr-tot_amt.
*        lo_amt_words->number_to_words_export(
*         EXPORTING
*           iv_num   = lv_grand_tot_word
*         RECEIVING
*           rv_words = grand_tot_amt_words
*       ).
*        gs_qothdr-amount_chargeable = grand_tot_amt_words.
***commented by neelam 27.nov.2025:arvind sd
**""""""add by sb16102025""""""""""""""""
***        IF gs_qothdr-amount_chargeable CS 'Lakh'.
          REPLACE ALL OCCURRENCES OF 'Lakh'  IN gs_qothdr-amount_chargeable  WITH 'Hundred ' ##NO_TEXT.
****        ELSE.
          REPLACE ALL OCCURRENCES OF 'Rupees'  IN gs_qothdr-amount_chargeable  WITH 'Dollars' ##NO_TEXT.
          REPLACE ALL OCCURRENCES OF 'Paisa'  IN gs_qothdr-amount_chargeable  WITH 'Cents' ##NO_TEXT.
***        ENDIF.
*comment by sb1610"""""""""""""""""""""""""""
      ELSEIF ls_qothdr-transactioncurrency = 'AED'.
        REPLACE ALL OCCURRENCES OF 'Rupees'  IN gs_qothdr-amount_chargeable  WITH 'Dirhams' ##NO_TEXT.
        REPLACE ALL OCCURRENCES OF 'Paisa'  IN gs_qothdr-amount_chargeable  WITH 'Fils' ##NO_TEXT.
      ELSEIF ls_qothdr-transactioncurrency = 'EURO'.
        REPLACE ALL OCCURRENCES OF 'Rupees'  IN gs_qothdr-amount_chargeable  WITH 'Euros' ##NO_TEXT.
        REPLACE ALL OCCURRENCES OF 'Paisa'  IN gs_qothdr-amount_chargeable  WITH 'Cents' ##NO_TEXT.
      ELSEIF ls_qothdr-transactioncurrency = 'CAD'.
        REPLACE ALL OCCURRENCES OF 'Rupees'  IN gs_qothdr-amount_chargeable  WITH 'Canadian dollars' ##NO_TEXT.
        REPLACE ALL OCCURRENCES OF 'Paisa'  IN gs_qothdr-amount_chargeable  WITH 'Cents' ##NO_TEXT.
      ELSEIF ls_qothdr-transactioncurrency = 'CAD'.
        REPLACE ALL OCCURRENCES OF 'Rupees'  IN gs_qothdr-amount_chargeable  WITH 'Australian dollars' ##NO_TEXT.
        REPLACE ALL OCCURRENCES OF 'Paisa'  IN gs_qothdr-amount_chargeable  WITH 'Cents' ##NO_TEXT.
      ENDIF.

      gs_qothdr-currency = ls_qothdr-transactioncurrency.
      " clear:gs_qothdr-amount_chargeable.
      gs_qothdr-tot_net_wt = gs_qothdr-tot_net_wt / 1000.
      gs_qothdr-tot_gross_wt = gs_qothdr-tot_gross_wt / 1000.
      CONDENSE gs_qothdr-tot_no_pkgs.
      INSERT LINES OF gt_item INTO TABLE gs_qothdr-quot_itm.
      APPEND gs_qothdr TO gt_qothdr.
    ENDLOOP.
    et_final[] = gt_qothdr[].

  ENDMETHOD.


  METHOD get_sales_data.
    DATA: gt_sohdr     TYPE TABLE OF zstr_so_data,
          gs_sohdr     TYPE zstr_so_data,
          gt_item      TYPE TABLE OF zstr_so_item,
          gs_item      TYPE zstr_so_item,
          lv_grand_tot TYPE p LENGTH 16 DECIMALS 2,
          lv_sum_cgst  TYPE p LENGTH 16 DECIMALS 2,
          lv_sum_sgst  TYPE p LENGTH 16 DECIMALS 2,
          lv_sum_frt   TYPE p LENGTH 16 DECIMALS 2,
          lv_sum_igst  TYPE p LENGTH 16 DECIMALS 2,
          lv_sum_qty   TYPE p LENGTH 16 DECIMALS 0,
          lv_itm_qty   TYPE p LENGTH 16 DECIMALS 0,
          lv_tot_amt   TYPE p LENGTH 16 DECIMALS 2.

    SELECT * FROM i_salesdocument
             WHERE salesdocument = @iv_vbeln
             INTO TABLE @DATA(lt_sohdr).      "#EC CI_ALL_FIELDS_NEEDED

    IF lt_sohdr[] IS NOT INITIAL.

      SELECT * FROM i_salesdocumentitem
               WHERE salesdocument = @iv_vbeln
               INTO TABLE @DATA(lt_soitem).   "#EC CI_ALL_FIELDS_NEEDED

      SELECT * FROM i_salesdocitempricingelement
               WHERE salesdocument = @iv_vbeln
               INTO TABLE @DATA(lt_soitem_price). "#EC CI_ALL_FIELDS_NEEDED

      SELECT * FROM zi_so_partner
               WHERE salesdocument = @iv_vbeln
               INTO TABLE @DATA(lt_sopart).   "#EC CI_ALL_FIELDS_NEEDED

      IF lt_soitem[] IS NOT INITIAL.

        SELECT
        product,
        productoid,
        producttype
        FROM i_product
                 FOR ALL ENTRIES IN @lt_soitem
                 WHERE product  = @lt_soitem-product
                 INTO TABLE @DATA(lt_product).     "#EC CI_NO_TRANSFORM

      ENDIF.

      SELECT * FROM i_salesdocumentscheduleline
               WHERE salesdocument = @iv_vbeln
               INTO TABLE @DATA(lt_schdl).    "#EC CI_ALL_FIELDS_NEEDED

    ENDIF.

    LOOP AT lt_sohdr INTO DATA(ls_sohdr).

      READ TABLE lt_soitem INTO DATA(ls_souitem_new) WITH KEY salesdocument = ls_sohdr-salesdocument.

      gs_sohdr-saleorder      = ls_sohdr-salesdocument.
      gs_sohdr-saleorderdate  = ls_sohdr-salesdocumentdate+6(2) && '.' && ls_sohdr-salesdocumentdate+4(2) && '.' && ls_sohdr-salesdocumentdate+0(4).
      gs_sohdr-saleordertype  = ls_sohdr-salesdocumenttype.

      """"" plant address       hardcode .....
      gs_sohdr-exptr_code = '' .

      gs_sohdr-plant_code = ls_souitem_new-plant .
      IF ls_souitem_new-plant = '1001'.

        gs_sohdr-exptr_name   = 'Bhagwati Lacto Vegetarian Exports Pvt. Ltd.' ##NO_TEXT.
        gs_sohdr-exptr_gstin = '03AADCB4295Q1ZA' ##NO_TEXT.
        gs_sohdr-exptr_pan   = 'AADCB4295Q' ##NO_TEXT.
        gs_sohdr-exptr_email = 'sales@blvexports.com' ##NO_TEXT.
        gs_sohdr-exptr_phone = '9592003165' ##NO_TEXT.
        gs_sohdr-work_addrs  = 'Rural Focal Point Mana Singh wala , Firozepur Moga Road' ##NO_TEXT.

      ELSEIF ls_souitem_new-plant = '2001'.

        gs_sohdr-exptr_name   = 'Bhagwati Lacto Foods Pvt Ltd' ##NO_TEXT.
        gs_sohdr-exptr_gstin = '03AADCB6737H1ZU' ##NO_TEXT.
        gs_sohdr-exptr_pan   = 'AADCB6737H' ##NO_TEXT.
        gs_sohdr-exptr_email = '' .
        gs_sohdr-exptr_phone = '9814844765' ##NO_TEXT.
        gs_sohdr-work_addrs  = 'VILL. RUKNA MUNGLA, FARIDKOT ROAD, FEROZEPUR, Ferozepur, Punjab, 152001' ##NO_TEXT.

      ELSEIF ls_souitem_new-plant = '3001'.

        gs_sohdr-exptr_name   = 'Healthy Harvested Foods Private Limited' ##NO_TEXT.
        gs_sohdr-exptr_gstin = '24AAFCH2454K1ZL' ##NO_TEXT.
        gs_sohdr-exptr_pan   = 'AAFCH2454K' ##NO_TEXT.
        gs_sohdr-exptr_email = '' .
        gs_sohdr-exptr_phone = '9592003165' ##NO_TEXT.
        gs_sohdr-work_addrs  = 'PLOT NO362, Gidc Mithi Rohar, GANDHIDHAM, Kachchh, Gujarat, 370201' ##NO_TEXT.

      ELSEIF ls_souitem_new-plant = '4001'.

        gs_sohdr-exptr_name  = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
        gs_sohdr-exptr_gstin = '03AADCB6737H1ZU' ##NO_TEXT.
        gs_sohdr-exptr_pan   = 'AAJCB8215E1' ##NO_TEXT.
        gs_sohdr-exptr_email = 'sales@blvexports.com' ##NO_TEXT.
        gs_sohdr-exptr_phone = '9592003165' ##NO_TEXT.
        gs_sohdr-work_addrs  = 'Rural Focal Point Mana Singh wala , Firozepur Moga Road' ##NO_TEXT.

      ENDIF.

      gs_sohdr-exptr_addrs1 = '18 Old Anaj Mandi, Ferozepur Cantt. - 152001 Punjab' ##NO_TEXT.
      gs_sohdr-exptr_addrs2 = 'India' ##NO_TEXT.
      gs_sohdr-exptr_addrs3 = '' .
      gs_sohdr-exptr_addrs4 = ''.

      """" factory address
      gs_sohdr-fact_addrs1 = '18 Old Anaj Mandi, Ferozepur Cantt. - 152001 Punjab ' ##NO_TEXT.
      gs_sohdr-fact_addrs2 = 'India' ##NO_TEXT.
      gs_sohdr-fact_addrs3 = '' .

      gs_sohdr-our_ref          = ls_sohdr-salesdocument.
      gs_sohdr-our_ref_date     = ls_sohdr-salesdocumentdate+6(2) && '.' && ls_sohdr-salesdocumentdate+4(2) && '.' && ls_sohdr-salesdocumentdate+0(4).
      gs_sohdr-cust_odr_ref     = ls_sohdr-purchaseorderbycustomer .
      gs_sohdr-cust_odr_date    = ls_sohdr-customerpurchaseorderdate+6(2) && '.' && ls_sohdr-customerpurchaseorderdate+4(2) && '.' && ls_sohdr-customerpurchaseorderdate+0(4).
      gs_sohdr-buyr_code        = ls_sohdr-soldtoparty .
      gs_sohdr-inco_term        = ls_sohdr-incotermsclassification.
      gs_sohdr-price_term       = ls_sohdr-incotermslocation1. "IncotermsClassification.
      gs_sohdr-amtount_curr     = ls_sohdr-transactioncurrency.

      SELECT SINGLE * FROM i_customerpaymenttermstext
                      WHERE customerpaymentterms = @ls_sohdr-customerpaymentterms
                        AND language = 'E'
                      INTO @DATA(ls_payterm_desc).

      gs_sohdr-pay_term   = ls_payterm_desc-customerpaymenttermsname.

      DATA : lv_shp_adr1 TYPE c LENGTH 100,
             lv_shp_adr2 TYPE c LENGTH 100,
             lv_shp_adr3 TYPE c LENGTH 100,
             lv_shp_adr4 TYPE c LENGTH 100.

      DATA : lv_sold_adr1 TYPE c LENGTH 100,
             lv_sold_adr2 TYPE c LENGTH 100,
             lv_sold_adr3 TYPE c LENGTH 100,
             lv_sold_adr4 TYPE c LENGTH 100.

      READ TABLE lt_sopart INTO DATA(ls_sopart) WITH KEY salesdocument = ls_sohdr-salesdocument.
      IF ls_sopart-we_street IS NOT INITIAL .
        IF lv_shp_adr1 IS NOT INITIAL   .
          lv_shp_adr1 = |{ lv_shp_adr1 } , { ls_sopart-we_street }, { ls_sopart-we_streetprefixname1 }, { ls_sopart-we_streetprefixname2 }, { ls_sopart-we_streetsuffixname1 }| .
        ELSE .
          lv_shp_adr1 = |{ ls_sopart-we_street }, { ls_sopart-we_streetprefixname1 }, { ls_sopart-we_streetprefixname2 }, { ls_sopart-we_streetsuffixname1 }| .
        ENDIF .
      ENDIF .

      IF ls_sopart-we_street1 IS NOT INITIAL .
        IF lv_shp_adr1 IS NOT INITIAL   .
          lv_shp_adr1 = |{ lv_shp_adr1 } , { ls_sopart-we_street1 }, { ls_sopart-we_streetprefixname1 }, { ls_sopart-we_streetprefixname2 }, { ls_sopart-we_streetsuffixname1 }| .
        ELSE .
          lv_shp_adr1 = |{ ls_sopart-we_street1 }, { ls_sopart-we_streetprefixname1 }, { ls_sopart-we_streetprefixname2 }, { ls_sopart-we_streetsuffixname1 }| .
        ENDIF .
      ENDIF .

      DATA(len) = strlen( lv_shp_adr1 ) .
      len = len - 40.
      IF strlen( lv_shp_adr1 ) GT 40 .
        lv_shp_adr2 = |{ lv_shp_adr1+40(len) },| .
        lv_shp_adr1 = lv_shp_adr1+0(40) .
      ENDIF .

      READ TABLE lt_sopart INTO DATA(ls_sopart_ag) WITH KEY salesdocument = ls_sohdr-salesdocument.
      IF ls_sopart_ag-ag_street IS NOT INITIAL .
        IF lv_sold_adr1 IS NOT INITIAL   .
          lv_sold_adr1 = |{ lv_sold_adr1 } , { ls_sopart_ag-ag_street }, { ls_sopart_ag-ag_streetprefixname1 }, { ls_sopart_ag-ag_streetprefixname2 }, { ls_sopart_ag-ag_streetsuffixname1 }| .
        ELSE .
          lv_sold_adr1 = |{ ls_sopart_ag-ag_street }, { ls_sopart_ag-ag_streetprefixname1 }, { ls_sopart_ag-ag_streetprefixname2 }, { ls_sopart_ag-ag_streetsuffixname1 }| .
        ENDIF .
      ENDIF .

      IF ls_sopart_ag-ag_street1 IS NOT INITIAL .
        IF lv_sold_adr1 IS NOT INITIAL   .
          lv_sold_adr1 = |{ lv_sold_adr1 } , { ls_sopart_ag-ag_street1 }, { ls_sopart_ag-ag_streetprefixname1 }, { ls_sopart_ag-ag_streetprefixname2 }, { ls_sopart_ag-ag_streetsuffixname1 }| .
        ELSE .
          lv_sold_adr1 = |{ ls_sopart_ag-ag_street1 }, { ls_sopart_ag-ag_streetprefixname1 }, { ls_sopart_ag-ag_streetprefixname2 }, { ls_sopart_ag-ag_streetsuffixname1 }| .
        ENDIF .
      ENDIF .

      DATA(len_ag) = strlen( lv_sold_adr1 ) .
      len_ag = len_ag - 40.
      IF strlen( lv_sold_adr1 ) GT 40 .
        lv_sold_adr2 = |{ lv_sold_adr1+40(len_ag) },| .
        lv_sold_adr1 = lv_sold_adr1+0(40) .
      ENDIF .

      READ TABLE lt_sopart INTO DATA(ls_sopart_sp) WITH KEY salesdocument = ls_sohdr-salesdocument.

      SELECT SINGLE
country,
language,
countryname,
nationalityname,
nationalitylongname,
countryshortname
      FROM i_countrytext   WHERE country = @ls_sopart_ag-ag_country AND language = 'E'
      INTO @DATA(lv_cn_nm).                        "#EC CI_NO_TRANSFORM

      SELECT SINGLE
country,
language,
region,
regionname
      FROM i_regiontext  WHERE region = @ls_sopart_ag-ag_region AND language = 'E' AND country = @ls_sopart_ag-ag_country
      INTO @DATA(lv_st_name_ag).                   "#EC CI_NO_TRANSFORM

      lv_sold_adr3 = ls_sopart_ag-ag_city && '-' && ls_sopart_ag-ag_pin && ',' && lv_cn_nm-countryname.
      lv_sold_adr4 = lv_st_name_ag-regionname. "ls_sopart_ag-ag_region && '(' && lv_st_name_ag-RegionName && ')'.
      gs_sohdr-buyr_name        = ls_sopart_ag-ag_name.
      gs_sohdr-buyr_addrs1      = lv_sold_adr1.
      gs_sohdr-buyr_addrs2      = lv_sold_adr2.
      gs_sohdr-buyr_addrs3      = lv_sold_adr3.
      gs_sohdr-buyr_addrs4      = ''. "lv_sold_adr4.
      gs_sohdr-buyr_phone        = ''.
      gs_sohdr-buyr_gstin        = ls_sopart_sp-ag_tax.
      gs_sohdr-buyr_state        = lv_st_name_ag-regionname.
      gs_sohdr-buyr_state_code   = ls_sopart_ag-ag_region.

      SELECT SINGLE
country,
language,
countryname,
nationalityname,
nationalitylongname,
countryshortname
      FROM i_countrytext   WHERE country = @ls_sopart-we_country AND language = 'E'
      INTO @DATA(lv_cn_nm1).                       "#EC CI_NO_TRANSFORM

      SELECT SINGLE
country,
language,
region,
regionname
      FROM i_regiontext  WHERE region = @ls_sopart-we_region AND language = 'E' AND country = @ls_sopart-we_country
      INTO @DATA(lv_st_name_we1).                  "#EC CI_NO_TRANSFORM

      lv_shp_adr3 = ls_sopart-we_city && '-' && ls_sopart-we_pin && ',' && lv_cn_nm1-countryname.
      lv_shp_adr4 = lv_st_name_we1-regionname. "ls_sopart-we_region && '(' && lv_st_name_we1-RegionName && ')'.

      gs_sohdr-cnsinee_code     = ''.
      gs_sohdr-cnsinee_name     = ls_sopart-we_name.
      gs_sohdr-cnsinee_addrs1   = lv_shp_adr1.
      gs_sohdr-cnsinee_addrs2   = lv_shp_adr2.
      gs_sohdr-cnsinee_addrs3   = lv_shp_adr3.
      gs_sohdr-cnsinee_addrs4   = ''. "lv_shp_adr4.
      gs_sohdr-cnsinee_gstin     = ls_sopart-we_tax.
      gs_sohdr-cnsinee_stat_name = lv_st_name_we1-regionname.
      gs_sohdr-cnsinee_stat_code = ls_sopart-we_region.

      IF ls_sopart_ag-ag_code EQ ls_sopart-ship_to_party.
        gs_sohdr-cnsinee_name = 'Same as buyer' ##NO_TEXT.
        CLEAR: gs_sohdr-cnsinee_addrs1, gs_sohdr-cnsinee_addrs2, gs_sohdr-cnsinee_addrs3, gs_sohdr-cnsinee_addrs4.
        gs_sohdr-cnsinee_stat_name = lv_st_name_ag-regionname.
        gs_sohdr-cnsinee_stat_code = ls_sopart_ag-ag_region.
      ENDIF.

      gs_sohdr-ship_mode        = ''.
      gs_sohdr-port_disch       = ''.
      gs_sohdr-port_delivry     = ''.
      gs_sohdr-pinst_box        = ''.
      gs_sohdr-pinst_stickr     = ''.
      gs_sohdr-pinst_make       = ''.
      gs_sohdr-made_in_india    = ''.
      gs_sohdr-making_inst      = ''.

      gs_sohdr-agent_from        = ''.
      gs_sohdr-tax_type          = ''.
      gs_sohdr-del_from_date     = ''.

      gs_sohdr-del_to_date       = ls_sohdr-requesteddeliverydate+6(2) && '.' &&
                                   ls_sohdr-requesteddeliverydate+4(2) && '.'
                                   && ls_sohdr-requesteddeliverydate+0(4).

      gs_sohdr-broker_name       = ls_sopart_sp-sp_name.
      IF ls_souitem_new-plant = '1001'.
        gs_sohdr-bank_name         = 'PUNJAB NATIONAL BANK'.
        gs_sohdr-bank_branch       = 'FIROZEPUR CANTT'.
        gs_sohdr-bank_acc          = '0171008700006312'.
        gs_sohdr-bank_ifsc         = 'PUNB0017100'.
      ELSEIF ls_souitem_new-plant = '2001'.
        gs_sohdr-bank_name         = 'State Bank of India'.
        gs_sohdr-bank_branch       = 'FIROZEPUR CANTT'.
        gs_sohdr-bank_acc          = '65176337258'.
        gs_sohdr-bank_ifsc         = 'SBIN0050627'.
      ELSEIF ls_souitem_new-plant = '3001'.
        gs_sohdr-bank_name         = 'Indian Bank'.
        gs_sohdr-bank_branch       = 'CHEEMA CHOWK'.
        gs_sohdr-bank_acc          = '7111281156'.
        gs_sohdr-bank_ifsc         = 'IDIB0M0363'.
      ELSEIF ls_souitem_new-plant = '4001'.
        gs_sohdr-bank_name         = 'Indian Bank'.
        gs_sohdr-bank_branch       = 'LUDHIANA'.
        gs_sohdr-bank_acc          = '8064847477'.
        gs_sohdr-bank_ifsc         = 'IDIB0M0363'.
      ENDIF.

      LOOP AT lt_soitem INTO DATA(ls_souitem) WHERE salesdocument = ls_sohdr-salesdocument.


        gs_item-saleorder       = ls_souitem-salesdocument.
        gs_item-saleitem        = ls_souitem-salesdocumentitem.
        gs_item-sr_num          = ls_souitem-salesdocumentitem.
        gs_item-byur_code       = ls_sohdr-soldtoparty.
        SHIFT gs_item-byur_code LEFT DELETING LEADING '0'.

        gs_item-brand        = ls_souitem-materialbycustomer.
        gs_item-net_weight   = ls_souitem-itemnetweight / 100.
        sum_weight = sum_weight + gs_item-net_weight .
        CONDENSE sum_weight.

        READ TABLE lt_product INTO DATA(ls_product) WITH KEY product = ls_souitem-product.
        gs_item-item_code       = ls_souitem-product. "ls_product-productoldid.
        SHIFT gs_item-item_code LEFT DELETING LEADING '0'.
        CLEAR: ls_product.

        gs_item-item_desc       = ls_souitem-salesdocumentitemtext .
        lv_itm_qty = ls_souitem-orderquantity.
        gs_item-item_qty        = lv_itm_qty.
        SHIFT gs_item-item_qty LEFT DELETING LEADING ''.
        lv_sum_qty = lv_sum_qty + ls_souitem-orderquantity.

        gs_item-item_uom        = ls_souitem-orderquantityunit.

        IF gs_item-item_uom = 'ST'.
          gs_item-item_uom = 'PC'.
        ENDIF.

        READ TABLE lt_schdl INTO DATA(ls_schdl) WITH KEY salesdocument = ls_souitem-salesdocument
                                                         salesdocumentitem = ls_souitem-salesdocumentitem.

        gs_item-dispatch_date   = ls_schdl-deliverydate+6(2) && '.' && ls_schdl-deliverydate+4(2) && '.' && ls_schdl-deliverydate+0(4).


        READ TABLE lt_soitem_price INTO DATA(w_item_price)
         WITH KEY salesdocument = ls_sohdr-salesdocument
                  salesdocumentitem = ls_souitem-salesdocumentitem
                  conditiontype = 'PMP0'.

        IF sy-subrc = 0 .
          gs_item-price_usd_fob   = w_item_price-conditionratevalue. "/ w_item_price-conditionquantity .
          IF ls_sohdr-pricelisttype = 'S2'.
            gs_item-amt_usd_fob     = gs_item-item_qty * gs_item-price_usd_fob.
          ELSE.
            gs_item-amt_usd_fob     = gs_item-net_weight * gs_item-price_usd_fob.
          ENDIF.

          lv_tot_amt = lv_tot_amt + gs_item-amt_usd_fob.
        ENDIF .

        READ TABLE lt_soitem_price INTO w_item_price
        WITH KEY salesdocument = ls_sohdr-salesdocument
                 salesdocumentitem = ls_souitem-salesdocumentitem
                 conditiontype = 'ZDIS'.
        IF sy-subrc = 0 .
          gs_sohdr-disc_amt = gs_sohdr-disc_amt + w_item_price-conditionamount.
        ENDIF .

*        READ TABLE lt_soitem_price INTO DATA(w_item_gst)
*         WITH KEY salesdocument = ls_sohdr-salesdocument
*                  salesdocumentitem = ls_souitem-salesdocumentitem
*                  conditiontype = 'JOIG'.
*        IF sy-subrc = 0 .
*          IF w_item_gst-conditionratevalue EQ '0.100000000'.
*            lv_sum_igst = lv_sum_igst + w_item_gst-conditionamount.
*          ENDIF.
*        ENDIF .

*        CLEAR: w_item_gst.
*        READ TABLE lt_soitem_price INTO w_item_gst
*         WITH KEY salesdocument = ls_sohdr-salesdocument
*                  salesdocumentitem = ls_souitem-salesdocumentitem
*                  conditiontype = 'JOCG'.
*        IF sy-subrc = 0 .
*          IF w_item_gst-conditionratevalue EQ '0.100000000'.
*            lv_sum_cgst = lv_sum_cgst + w_item_gst-conditionamount.
*          ENDIF.
*        ENDIF .

*        CLEAR: w_item_gst.
*        READ TABLE lt_soitem_price INTO w_item_gst
*         WITH KEY salesdocument = ls_sohdr-salesdocument
*                  salesdocumentitem = ls_souitem-salesdocumentitem
*                  conditiontype = 'JOSG'.
*        IF sy-subrc = 0 .
*
*         lv_sum_sgst = lv_sum_sgst + w_item_gst-conditionamount.
*        ENDIF .

        LOOP AT lt_soitem_price INTO DATA(w_item_gst)
        WHERE salesdocument = ls_sohdr-salesdocument AND
                  salesdocumentitem = ls_souitem-salesdocumentitem.

          IF w_item_gst-conditiontype = 'ZFRT'.
            lv_sum_frt = lv_sum_frt + w_item_gst-conditionamount.
          ENDIF .

          IF w_item_gst-conditiontype = 'JOSG'.
            lv_sum_sgst = lv_sum_sgst + w_item_gst-conditionamount.
          ENDIF .

          IF w_item_gst-conditiontype = 'JOCG'.
            lv_sum_cgst = lv_sum_cgst + w_item_gst-conditionamount.
          ENDIF .

          IF w_item_gst-conditiontype = 'JOIG'.
            lv_sum_igst = lv_sum_igst + w_item_gst-conditionamount.
          ENDIF .


        ENDLOOP.

        lv_grand_tot = lv_grand_tot + ls_souitem-netamount .

        APPEND gs_item TO gt_item.
        CLEAR: ls_souitem , gs_item , w_item_price, w_item_gst.
      ENDLOOP.

      gs_sohdr-sum_cgst_amt = lv_sum_cgst.
      gs_sohdr-sum_sgst_amt = lv_sum_sgst.
      gs_sohdr-sum_frt_amt  = lv_sum_frt.
      gs_sohdr-igst_amt = lv_sum_igst.
      gs_sohdr-sum_qty  = lv_sum_qty.

      gs_sohdr-grand_total      = lv_tot_amt + gs_sohdr-disc_amt + gs_sohdr-igst_amt
                                  + gs_sohdr-sum_cgst_amt + gs_sohdr-sum_sgst_amt + gs_sohdr-sum_frt_amt.

      gs_sohdr-total_amt        = lv_tot_amt.

      gs_sohdr-disc_amt         = gs_sohdr-disc_amt .

      DATA : lv_grand_tot_word TYPE string,
             lv_gst_tot_word   TYPE string.

      DATA:
        lo_amt_words TYPE REF TO zcl_amt_words.

      CREATE OBJECT lo_amt_words.

      lv_grand_tot_word = gs_sohdr-grand_total. "lv_grand_tot.   "number_to_words

      lo_amt_words->number_to_words(                     "number_to_words_export(
       EXPORTING
         iv_num   = lv_grand_tot_word
       RECEIVING
         rv_words = DATA(grand_tot_amt_words)
     ).

      gs_sohdr-amt_words        = |{ gs_sohdr-amtount_curr } | && grand_tot_amt_words.

      INSERT LINES OF gt_item INTO TABLE gs_sohdr-xt_item.
      APPEND gs_sohdr TO gt_sohdr.
      CLEAR: ls_sohdr, ls_souitem_new.
    ENDLOOP.

    et_final[] = gt_sohdr[].
  ENDMETHOD.


  METHOD prep_xml_delivery_prnt.

    DATA : heading      TYPE c LENGTH 100,
           sub_heading  TYPE c LENGTH 200,
           manager      TYPE c LENGTH 200,
           lv_xml_final TYPE string.

    IF iv_action1 = 'MDN'.
      heading = 'Material Dispatch Note' ##NO_TEXT.
    ELSE.
      heading = 'Packing Advice' ##NO_TEXT.
    ENDIF.

    IF iv_action = 'sdopass'.
      heading = 'OUT PASS' ##NO_TEXT.
    ENDIF.

    manager = 'Manager Sign' ##NO_TEXT.

    READ TABLE it_final INTO DATA(ls_final) INDEX 1.

    DATA(lv_xml) =  |<Form>| &&
                    |<MaterialDispatchNode>| &&
                    |<header_1>{ ls_final-plant_nam }</header_1>| &&
                    |<plant_adr>{ ls_final-plant_addr }</plant_adr>| &&
                    |<header_2>{ heading }</header_2>| &&
                    |<number>{ ls_final-number }</number>| &&
                    |<advice_no>{ ls_final-number }</advice_no>| &&
                    |<party_name>{ ls_final-party_nam }</party_name>| &&
                    |<inco_term>{ ls_final-inco_term }</inco_term>| &&
                    |<date>{ ls_final-date }</date>| &&
                    |<vechicle>{ ls_final-vechicle }</vechicle>| &&
                    |<description>{ ls_final-destination }</description>| &&
                    |<Brokr_nam>{ ls_final-brokr_nam }</Brokr_nam>| &&
                    |<lbor_cont>{ ls_final-lbor_cont }</lbor_cont>| &&
                    |<supvr_nam>{ ls_final-supvr_nam }</supvr_nam>| &&
                    |<stchngby>{ ls_final-stchngby }</stchngby>| &&
                    |<pckng>{ ls_final-pckng }</pckng>| &&
                    |<vch_no>{ ls_final-vch_no }</vch_no>| &&
                    |<dispatch_qty>{ ls_final-tot_disp_qty }</dispatch_qty>| &&
                    |<dispatch_wt>{ ls_final-tot_disp_wt }</dispatch_wt>| &&
                    |<remarks>{ ls_final-remarks }</remarks>| &&
                    |<loaded_by>{ ls_final-loaded_by }</loaded_by>| &&
                    |<store_incharge>{ manager }</store_incharge>| &&
                    |<ItemData>| .


    DATA : lv_item TYPE string .
    DATA : srn TYPE c LENGTH 3.
    CLEAR : lv_item , srn .

    LOOP AT ls_final-del_itm INTO DATA(ls_item).

      srn = srn + 1 .
*      CONDENSE srn.

      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<sl_num>{ srn }</sl_num>| &&
                |<so>{ ls_item-saleorder }</so>| &&
                |<item_name>{ ls_item-item_name }</item_name> | &&
                |<Qty>{ ls_item-qty }</Qty>| &&
                |<unit>{ ls_item-unit }</unit>| &&
*                |<dispatch_qty>{ ls_item-disp_qty }</dispatch_qty>| &&
                |<dispatch_weight>{ ls_item-disp_wt }</dispatch_weight>| &&
                |<rate>{ ls_item-rate }</rate>| &&
                |<godown>{ ls_item-godown }</godown>| &&
                |<brand>{ ls_item-brand }</brand>| &&
                |</ItemDataNode>|  .

    ENDLOOP.

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                       |</MaterialDispatchNode>| &&
                       |</Form>|.

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.
  ENDMETHOD.


  METHOD prep_xml_quotation_prnt.

    DATA : heading      TYPE c LENGTH 100,
           sub_heading  TYPE c LENGTH 200,
           lv_xml_final TYPE string.

    heading = 'Quotation Print' ##NO_TEXT.

    READ TABLE it_final INTO DATA(ls_final) INDEX 1.
    REPLACE ALL OCCURRENCES OF 'ü' IN ls_final-consig_4 WITH 'u'.
    DATA(lv_xml) =  |<Form>| &&
                    |<QuotationInvoiceNode>| &&
                    |<header_blv>{ ls_final-header_blv }</header_blv>| &&
                    |<shipper_1>{ ls_final-shipper_1 }</shipper_1>| &&
                    |<shipper_2>{ ls_final-shipper_2 }</shipper_2>| &&
                    |<shipper_3>{ ls_final-shipper_3 }</shipper_3>| &&
                    |<shipper_4>{ ls_final-shipper_4 }</shipper_4>| &&
                    |<consig_1>{ ls_final-consig_1 }</consig_1>| &&
                    |<consig_2>{ ls_final-consig_2 }</consig_2>| &&
                    |<consig_3>{ ls_final-consig_3 }</consig_3>| &&
                    |<consig_4>{ ls_final-consig_4 }</consig_4>| &&
                    |<notify_party_1>{ ls_final-notify_party_1 }</notify_party_1>| &&
                    |<notify_party_2>{ ls_final-notify_party_2 }</notify_party_2>| &&
                    |<notify_party_3>{ ls_final-notify_party_3 }</notify_party_3>| &&
                    |<notify_party_4>{ ls_final-notify_party_4 }</notify_party_4>| &&
                    |<notify_party_21>{ ls_final-notify_party_21 }</notify_party_21>| &&
                    |<notify_party_22>{ ls_final-notify_party_22 }</notify_party_22>| &&
                    |<notify_party_23>{ ls_final-notify_party_23 }</notify_party_23>| &&
                    |<notify_party_24>{ ls_final-notify_party_24 }</notify_party_24>| &&
                    |<notify_party_31>{ ls_final-notify_party_31 }</notify_party_31>| &&
                    |<notify_party_32>{ ls_final-notify_party_32 }</notify_party_32>| &&
                    |<notify_party_33>{ ls_final-notify_party_33 }</notify_party_33>| &&
                    |<notify_party_34>{ ls_final-notify_party_34 }</notify_party_34>| &&
                    |<pre_carr>{ ls_final-pre_carr }</pre_carr>| &&
                    |<vessel>{ ls_final-vessel }</vessel>| &&
                    |<port_of_loading>{ ls_final-port_of_loading }</port_of_loading>| &&
                    |<recp_by_pre_carr>{ ls_final-recp_by_pre_carr }</recp_by_pre_carr>| &&
                    |<port_of_disc>{ ls_final-port_of_disc }</port_of_disc>| &&
                    |<Payment_terms>{ ls_final-payment_terms }</Payment_terms>| &&
                    |<port_of_loading>{ ls_final-port_of_loading }</port_of_loading>| &&
                    |<final_dest>{ ls_final-final_dest }</final_dest>| &&
                    |<no_date>{ ls_final-no_date }</no_date>| &&
                    |<buy_order_date>{ ls_final-buy_order_date }</buy_order_date>| &&
                    |<buyer>{ ls_final-buyer }</buyer>| &&
                    |<count_of_origin>{ ls_final-count_of_origin }</count_of_origin>| &&
                    |<count_of_final_dest>{ ls_final-count_of_final_dest }</count_of_final_dest>| &&
                    |<delivery>{ ls_final-delivery }</delivery>| &&
                    |<payment>{ ls_final-payment }</payment>| &&
                    |<coc_test_rep>{ ls_final-coc_test_rep }</coc_test_rep>| &&
                    |<bank_uco>{ ls_final-bank_uco }</bank_uco>| &&
                    |<bank_near>{ ls_final-bank_near }</bank_near>| &&
                    |<bank_city>{ ls_final-bank_city }</bank_city>| &&
                    |<bank_account>{ ls_final-bank_account }</bank_account>| &&
                    |<bank_swift>{ ls_final-bank_swift }</bank_swift>| &&
                    |<tot_no_pkgs>{ ls_final-tot_no_pkgs }</tot_no_pkgs>| &&
                    |<tot_net_wt>{ ls_final-tot_net_wt }</tot_net_wt>| &&
                    |<tot_gross_wt>{ ls_final-tot_gross_wt }</tot_gross_wt>| &&
                    |<note>{ ls_final-note }</note>| &&
                    |<amount_chargeable>{ ls_final-amount_chargeable }</amount_chargeable>| &&
                    |<tot_fcl>{ ls_final-tot_fcl }</tot_fcl>| &&
                    |<tot_qty>{ ls_final-tot_qty }</tot_qty>| &&
                    |<tot_amt>{ ls_final-tot_amt }</tot_amt>| &&
                    |<ship_conf>{ ls_final-ship_conf }</ship_conf>| &&
                    |<footer1>{ ls_final-footer1 }</footer1>| &&
                    |<footer2>{ ls_final-footer2 }</footer2>| &&
                    |<footer3>{ ls_final-footer3 }</footer3>| &&
                    |<footer4>{ ls_final-footer4 }</footer4>| &&
                    |<footer5>{ ls_final-footer5 }</footer5>| &&
                    |<comp_code>{ ls_final-comp_code }</comp_code>| &&
                    |<currency>{ ls_final-currency }</currency>| &&
                    |<ItemData>|.


    DATA : lv_item TYPE string .
    DATA : srn TYPE c LENGTH 3.
    CLEAR : lv_item , srn .


    LOOP AT ls_final-quot_itm INTO DATA(ls_item).

      srn = srn + 1 .

      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<marks_nos>{ ls_item-marks_nos }</marks_nos>| &&
                |<brand>{ ls_item-brand }</brand>| &&
                |<kind_of_pack>{ ls_item-kind_of_pack }</kind_of_pack>| &&
                |<desc_of_goods>{ ls_item-desc_of_goods }</desc_of_goods>| &&
                |<fcl>{ ls_item-fcl }</fcl>| &&
                |<quantity>{ ls_item-quantity }</quantity>| &&
                |<rate>{ ls_item-rate }</rate>| &&
                |<amount>{ ls_item-amount }</amount>| &&
                |</ItemDataNode>|  .

    ENDLOOP.

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                       |</QuotationInvoiceNode>| &&
                       |</Form>|.


    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.

  ENDMETHOD.


  METHOD prep_xml_so_prnt.

    DATA : heading      TYPE c LENGTH 100,
           sub_heading  TYPE c LENGTH 200,
           lv_igst      TYPE string,
           lv_cgst      TYPE string,
           lv_sgst      TYPE string,
           lv_xml_final TYPE string,
           lv_remarks   TYPE string.

    heading = 'SALE ORDER'.

    READ TABLE it_final INTO DATA(ls_final) INDEX 1.
    SHIFT ls_final-sum_qty LEFT DELETING LEADING space.

    SELECT SINGLE incotermsclassificationname
    FROM i_incotermsclassificationtext
    WHERE incotermsclassification = @ls_final-inco_term
    INTO @DATA(lv_inco_text) .
    ls_final-inco_term = lv_inco_text .

    SELECT SINGLE purchaseorderbycustomer
    FROM i_salesdocument
    WHERE salesdocument = @ls_final-saleorder
    INTO @DATA(lv_order) .
    ls_final-agent_from = lv_order .

    SELECT SINGLE conditionratevalue
    FROM i_salesdocitempricingelement
    WHERE salesdocument = @ls_final-saleorder
    AND conditiontype = 'JOIG'
    INTO @DATA(v_igst) .

    lv_igst = v_igst .
    SPLIT lv_igst AT '.' INTO: DATA(ig1) DATA(ig2) .
    CLEAR lv_igst .
    CONCATENATE ig1 '%' INTO lv_igst .

    SELECT SINGLE conditionratevalue
    FROM i_salesdocitempricingelement
    WHERE salesdocument = @ls_final-saleorder
    AND conditiontype = 'JOCG'
    INTO @DATA(v_cgst) .

    lv_cgst = v_cgst .
    SPLIT lv_cgst AT '.' INTO: DATA(cg1) DATA(cg2) .
    CLEAR lv_cgst .
    CONCATENATE cg1 '%' INTO lv_cgst .

    SELECT SINGLE conditionratevalue
    FROM i_salesdocitempricingelement
    WHERE salesdocument = @ls_final-saleorder
    AND conditiontype = 'JOSG'
    INTO @DATA(v_sgst) .

    lv_sgst = v_sgst .
    SPLIT lv_sgst AT '.' INTO: DATA(sg1) DATA(sg2) .
    CLEAR lv_sgst .
    CONCATENATE sg1 '%' INTO lv_sgst .

    SELECT yy1_salesremark_sdh
    FROM i_salesdocument
    WHERE salesdocument = @ls_final-saleorder
    INTO TABLE @DATA(t_remarks) .

    LOOP AT t_remarks INTO DATA(w_remarks) .
      CONCATENATE lv_remarks w_remarks-yy1_salesremark_sdh INTO lv_remarks SEPARATED BY space .
      CLEAR: w_remarks .
    ENDLOOP .


    DATA(lv_xml) =  |<Form>| &&
                    |<SalesDocumentNode>| &&
                    |<heading>{ heading }</heading>| &&
                    |<sub_heading>{ sub_heading }</sub_heading>| &&
                    |<work_addrs>{ ls_final-work_addrs }</work_addrs>| &&
                    |<exptr_code>{ ls_final-exptr_code }</exptr_code>| &&
                    |<plant_code>{ ls_final-plant_code }</plant_code>| &&
                    |<exptr_name>{ ls_final-exptr_name }</exptr_name>| &&
                    |<exptr_addrs1>{ ls_final-exptr_addrs1 }</exptr_addrs1>| &&
                    |<exptr_addrs2>{ ls_final-exptr_addrs2 }</exptr_addrs2>| &&
                    |<exptr_addrs3>{ ls_final-exptr_addrs3 }</exptr_addrs3>| &&
                    |<exptr_addrs4>{ ls_final-exptr_addrs4 }</exptr_addrs4>| &&
                    |<exptr_gstin>{ ls_final-exptr_gstin }</exptr_gstin>| &&
                    |<exptr_pan>{ ls_final-exptr_pan }</exptr_pan>| &&
                    |<exptr_email>{ ls_final-exptr_email }</exptr_email>| &&
                    |<exptr_phone>{ ls_final-exptr_phone }</exptr_phone>| &&
                    |<fact_addrs1>{ ls_final-fact_addrs1 }</fact_addrs1>| &&
                    |<fact_addrs2>{ ls_final-fact_addrs2 }</fact_addrs2>| &&
                    |<fact_addrs3>{ ls_final-fact_addrs3 }</fact_addrs3>| &&
                    |<our_ref>{ ls_final-our_ref }</our_ref>| &&
                    |<our_ref_date>{ ls_final-our_ref_date }</our_ref_date>| &&
                    |<cust_odr_ref>{ ls_final-cust_odr_ref }</cust_odr_ref>| &&
                    |<cust_odr_date>{ ls_final-cust_odr_date }</cust_odr_date>| &&
                    |<buyr_code>{ ls_final-buyr_code }</buyr_code>| &&
                    |<buyr_name>{ ls_final-buyr_name }</buyr_name>| &&
                    |<buyr_gstin>{ ls_final-buyr_gstin }</buyr_gstin>| &&
                    |<buyr_addrs1>{ ls_final-buyr_addrs1 }</buyr_addrs1>| &&
                    |<buyr_addrs2>{ ls_final-buyr_addrs2 }</buyr_addrs2>| &&
                    |<buyr_addrs3>{ ls_final-buyr_addrs3 }</buyr_addrs3>| &&
                    |<buyr_addrs4>{ ls_final-buyr_addrs4 }</buyr_addrs4>| &&
                    |<buyr_phone>{ ls_final-buyr_phone }</buyr_phone>| &&
                    |<buyr_state>{ ls_final-buyr_state }</buyr_state>| &&
                    |<buyr_state_code>{ ls_final-buyr_state_code }</buyr_state_code>| &&
                    |<cnsinee_code>{ ls_final-cnsinee_code }</cnsinee_code>| &&
                    |<cnsinee_name>{ ls_final-cnsinee_name }</cnsinee_name>| &&
                    |<cnsinee_addrs1>{ ls_final-cnsinee_addrs1 }</cnsinee_addrs1>| &&
                    |<cnsinee_addrs2>{ ls_final-cnsinee_addrs2 }</cnsinee_addrs2>| &&
                    |<cnsinee_addrs3>{ ls_final-cnsinee_addrs3 }</cnsinee_addrs3>| &&
                    |<cnsinee_addrs4>{ ls_final-cnsinee_addrs4 }</cnsinee_addrs4>| &&
                    |<cnsinee_gstin>{ ls_final-cnsinee_gstin }</cnsinee_gstin>| &&
                    |<cnsinee_stat_name>{ ls_final-cnsinee_stat_name }</cnsinee_stat_name>| &&
                    |<cnsinee_stat_code>{ ls_final-cnsinee_stat_code }</cnsinee_stat_code>| &&
                    |<agent_from>{ ls_final-agent_from }</agent_from>| &&
                    |<tax_type>{ ls_final-tax_type }</tax_type>| &&
                    |<del_from_date>{ ls_final-del_from_date }</del_from_date>| &&
                    |<del_to_date>{ ls_final-del_to_date }</del_to_date>| &&
                    |<broker_name>{ ls_final-broker_name }</broker_name>| &&
                    |<bank_name>{ ls_final-bank_name }</bank_name>| &&
                    |<bank_branch>{ ls_final-bank_branch }</bank_branch>| &&
                    |<bank_acc>{ ls_final-bank_acc }</bank_acc>| &&
                    |<bank_ifsc>{ ls_final-bank_ifsc }</bank_ifsc>| &&
                    |<price_term>{ ls_final-price_term }</price_term>| &&
                    |<pay_term>{ ls_final-pay_term }</pay_term>| &&
                    |<inco_term>{ ls_final-inco_term }</inco_term>| &&
                    |<amount_curr>{ ls_final-amtount_curr }</amount_curr>| &&
                    |<ship_mode>{ ls_final-ship_mode }</ship_mode>| &&
                    |<port_disch>{ ls_final-port_disch }</port_disch>| &&
                    |<port_delivry>{ ls_final-port_delivry }</port_delivry>| &&
                    |<total_amt>{ ls_final-total_amt }</total_amt>| &&
                    |<disc_amt>{ ls_final-disc_amt }</disc_amt>| &&
                    |<amt_words>{ ls_final-amt_words }</amt_words>| &&
                    |<igst_amt>{ ls_final-igst_amt }</igst_amt>| &&
                    |<sum_cgst_amt>{ ls_final-sum_cgst_amt }</sum_cgst_amt>| &&
                    |<sum_sgst_amt>{ ls_final-sum_sgst_amt }</sum_sgst_amt>| &&
                    |<rate_igst>{ lv_igst }</rate_igst>| &&
                    |<rate_cgst>{ lv_cgst }</rate_cgst>| &&
                    |<rate_sgst>{ lv_sgst }</rate_sgst>| &&
                    |<sum_frt_amt>{ ls_final-sum_frt_amt }</sum_frt_amt>| &&
                    |<sum_qty>{ ls_final-sum_qty }</sum_qty>| &&
                    |<sum_weight>{ sum_weight }</sum_weight>| &&
                    |<grand_total>{ ls_final-grand_total }</grand_total>| &&
                    |<pinst_box>{ ls_final-pinst_box }</pinst_box>| &&
                    |<pinst_stickr>{ ls_final-pinst_stickr }</pinst_stickr>| &&
                    |<pinst_make>{ ls_final-pinst_make }</pinst_make>| &&
                    |<made_in_india>{ ls_final-made_in_india }</made_in_india>| &&
                    |<making_inst>{ ls_final-making_inst }</making_inst>| &&
                    |<ItemData>| .

    DATA : lv_item TYPE string .
    DATA : srn TYPE c LENGTH 10.
    CLEAR : lv_item , srn .

    LOOP AT ls_final-xt_item INTO DATA(ls_item).

      srn = ls_item-sr_num. "srn + 1 .
      SHIFT srn LEFT DELETING LEADING '0'.

      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<sr_num>{ srn }</sr_num>| &&
                |<saleorder>{ ls_item-saleorder }</saleorder>| &&
                |<saleitem>{ ls_item-saleitem }</saleitem> | &&
                |<brand>{ ls_item-brand }</brand>| &&
                |<byur_code>{ ls_item-byur_code }</byur_code>| &&
                |<item_code>{ ls_item-item_code }</item_code>| &&
                |<item_desc>{ ls_item-item_desc }</item_desc>| &&
                |<item_qty>{ ls_item-item_qty }</item_qty>| &&
                |<item_uom>{ ls_item-item_uom }</item_uom>| &&
                |<net_weight>{ ls_item-net_weight }</net_weight>| &&
                |<dispatch_date>{ ls_item-dispatch_date }</dispatch_date>| &&
                |<price_usd_fob>{ ls_item-price_usd_fob }</price_usd_fob>| &&
                |<amt_usd_fob>{ ls_item-amt_usd_fob }</amt_usd_fob>| &&
                |</ItemDataNode>|  .

    ENDLOOP.

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                       |</SalesDocumentNode>| &&
                       |</Form>|.

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.

  ENDMETHOD.
ENDCLASS.
