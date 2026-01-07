CLASS zcl_mm_po_print DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sy_uname     TYPE c LENGTH 20.

    DATA:
      lv_char10         TYPE c LENGTH 10,
      lv_char120        TYPE c LENGTH 120,
      total_gst         TYPE string,
      total_gst_words   TYPE string,
      grand_total       TYPE string,
      grand_total_words TYPE string,
      item_cgst         TYPE string,
      item_sgst         TYPE string,
      item_igst         TYPE string,
      charge_per        TYPE p LENGTH 16 DECIMALS 2.

    DATA:
      brokerage     TYPE string,
      mandi_cess    TYPE string,
      associat_chrg TYPE string,
      insurance     TYPE string,
      discount      TYPE string,
      dhami         TYPE string,
      market_fee    TYPE string,
      hrdf          TYPE string,
      labour_chrg   TYPE string,
      auction_chrg  TYPE string,
      hammali_chrg  TYPE string,
      dala_chrg     TYPE string,
      oxner_chrg    TYPE string,
      tulai_chrg    TYPE string,
      loading_chrg  TYPE string.

    DATA:
      gt_final TYPE TABLE OF zstr_mm_po_print_hdr,
      gs_final TYPE zstr_mm_po_print_hdr,
      lt_item  TYPE TABLE OF zstr_mm_po_print_itm,
      ls_item  TYPE zstr_mm_po_print_itm.

    DATA:
      gt_final_pr TYPE TABLE OF zstr_mm_pr_print_hdr,
      gs_final_pr TYPE zstr_mm_pr_print_hdr,
      lt_item_pr  TYPE TABLE OF zstr_mm_pr_print_itm,
      ls_item_pr  TYPE zstr_mm_pr_print_itm.

    DATA: lo_amt_words   TYPE REF TO zcl_amt_words.

    METHODS:
      get_po_data
        IMPORTING
                  im_action       LIKE lv_char10
                  im_ponum        TYPE zi_po_print_data-purchaseorder
                  im_podate       TYPE zi_po_print_data-purchaseorderdate
                  im_poplant      TYPE zi_po_print_data-plant
        RETURNING VALUE(et_final) LIKE gt_final,

      prep_xml_po_print
        IMPORTING
                  it_final             LIKE gt_final
                  im_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string,

      get_pr_data
        IMPORTING
                  im_action          LIKE lv_char10
                  im_prnum           TYPE zi_po_print_data-purchaseorder
                  im_prdate          TYPE zi_po_print_data-purchaseorderdate
                  im_prplant         TYPE zi_po_print_data-plant
        RETURNING VALUE(et_final_pr) LIKE gt_final_pr,

      prep_xml_pr_print
        IMPORTING
                  it_final_pr          LIKE gt_final_pr
                  im_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MM_PO_PRINT IMPLEMENTATION.


  METHOD get_po_data.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT * FROM zi_po_data WHERE purchaseorder     EQ @im_ponum
                             AND   purchaseorderdate EQ @im_podate
                             AND   plant             EQ @im_poplant
                             INTO TABLE @DATA(it_po_data).

    SELECT * FROM zi_schedgagrmt_po WHERE schedulingagreement      EQ @im_ponum
*                                    AND   ScheduleLineDeliveryDate EQ @im_podate
*                                    AND   plant                    EQ @im_poplant
                                    INTO TABLE @DATA(it_sa_data).

    IF it_po_data IS NOT INITIAL.

      SELECT product,
             plant,
             consumptiontaxctrlcode
             FROM i_productplantbasic
             FOR ALL ENTRIES IN @it_po_data
             WHERE product = @it_po_data-material
             AND   plant   = @it_po_data-plant
             INTO TABLE @DATA(it_hsn).             "#EC CI_NO_TRANSFORM

      SELECT
      taxcode,
      cgstrate,
      sgstrate,
      igstrate
      FROM ze_tax_gst_prcnt
      FOR ALL ENTRIES IN @it_po_data
      WHERE taxcode = @it_po_data-taxcode
      INTO TABLE @DATA(it_gst_rate).               "#EC CI_NO_TRANSFORM

      SELECT product,
             alternativeunit,
             quantitynumerator,
             quantitydenominator
             FROM i_productunitsofmeasure
             FOR ALL ENTRIES IN @it_po_data
             WHERE product = @it_po_data-material
             AND   alternativeunit = 'BAG'
             INTO TABLE @DATA(it_bags).            "#EC CI_NO_TRANSFORM

    ENDIF.

    IF it_po_data IS NOT INITIAL OR it_sa_data IS NOT INITIAL.

      READ TABLE it_po_data INTO DATA(wa_po_hdr) INDEX 1. "#EC CI_NOORDER
      READ TABLE it_sa_data INTO DATA(wa_sa_hdr) INDEX 1. "#EC CI_NOORDER

      SELECT SINGLE * FROM zi_plant_address WHERE plant EQ @wa_po_hdr-plant  INTO @DATA(wa_plant_address). "#EC CI_ALL_FIELDS_NEEDED

      SELECT SINGLE * FROM zi_supplier_address WHERE supplier EQ @wa_po_hdr-supplier  INTO @DATA(wa_supplier_adr). "#EC CI_ALL_FIELDS_NEEDED

      SELECT
        purchaseorder,
        purchaseorderitem,
        pricingdocument,
        pricingdocumentitem,
        pricingprocedurestep,
        pricingprocedurecounter,
        conditionapplication,
        conditiontype,
        conditionamount,
        conditionquantity,
        conditioncalculationtype,
        conditionratevalue
      FROM i_purorditmpricingelementapi01 WHERE purchaseorder EQ @wa_po_hdr-purchaseorder
                                                   AND   conditiontype IN ( 'ZFV1', 'ZFVA', 'ZFRA', 'ZFRB', 'ZFRC',
                                                                            'ZHB0', 'ZHB2', 'ZHB3', 'ZOTH', 'ZOH1',
                                                                            'ZBC1', 'ZCES', 'ZASS', 'ZIN1', 'ZIN2',
                                                                            'ZLAB', 'ZLOD', 'ZDHM', 'ZMFE', 'ZHRD',
                                                                            'ZAUC', 'ZHAM', 'ZDLA', 'ZOXR', 'ZTUL'  )
                                                   INTO TABLE @DATA(it_charges). "#EC CI_NO_TRANSFORM

      DATA(it_charges_pdy) = it_charges[].
      DELETE it_charges_pdy WHERE conditionratevalue IS INITIAL.

      SELECT SINGLE
paymentterms,
language,
paymenttermsname,
paymenttermsdescription

      FROM i_paymenttermstext WHERE paymentterms = @wa_po_hdr-paymentterms
                                              AND   language     = @sy-langu
                                              INTO @DATA(payment_term). "#EC CI_NO_TRANSFORM

      IF im_action = 'saprint'.

        "SELECT SINGLE * FROM zi_plant_address WHERE plant EQ @wa_sa_hdr-plant  INTO @wa_plant_address. "#EC CI_ALL_FIELDS_NEEDED

        "SELECT SINGLE * FROM zi_supplier_address WHERE supplier EQ @wa_sa_hdr-supplier  INTO @wa_supplier_adr. "#EC CI_ALL_FIELDS_NEEDED

      ENDIF.
      """**Preparing Header Data
      IF wa_po_hdr-companycode  = '1000'.

        gs_final-plant_name       = 'Bhagwati Lacto Vegetarian Exports Pvt. Ltd.' ##NO_TEXT.
        gs_final-plant_adr1       = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
        gs_final-plant_adr2       = 'Works : Rural Focal Point Mana Singh wala ,' ##NO_TEXT.
        gs_final-plant_adr3       = 'Firozepur Moga Road' ##NO_TEXT.
        gs_final-plant_gstin      = '03AADCB4295Q1ZA' ##NO_TEXT.
        gs_final-plant_pan        = 'AADCB4295Q' ##NO_TEXT.

      ELSEIF wa_po_hdr-companycode = '2000'.

        gs_final-plant_name       = 'Bhagwati Lacto Foods Pvt. Ltd.' ##NO_TEXT.
*        gs_final-plant_adr1       = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
        gs_final-plant_adr2       = 'Works : VILL. RUKNA MUNGLA, FARIDKOT ROAD,' ##NO_TEXT.
        gs_final-plant_adr3       = 'Ferozepur, Punjab, 152001' ##NO_TEXT.
        gs_final-plant_gstin      = '03AADCB6737H1ZU' ##NO_TEXT.
        gs_final-plant_pan        = 'AADCB6737H' ##NO_TEXT.

      ELSEIF wa_po_hdr-companycode = '3000'.

        gs_final-plant_name       = 'Healthy Harvested Foods Private Limited' ##NO_TEXT.
*        gs_final-plant_adr1       = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
        gs_final-plant_adr2       = 'Works : PLOT NO100, SECTOR 1/A,' ##NO_TEXT.
        gs_final-plant_adr3       = 'GANDHIDHAM, Kachchh, Gujarat, 370201' ##NO_TEXT.
        gs_final-plant_gstin      = '24AAFCH2454K1ZL' ##NO_TEXT.
        gs_final-plant_pan        = 'AAFCH2454K' ##NO_TEXT.

      ELSEIF wa_po_hdr-companycode = '4000'.

        gs_final-plant_name       = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
*        gs_final-plant_adr1       = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
        gs_final-plant_adr2       = 'Works : M-14A VILL-Mohasa Babai,' ##NO_TEXT.
        gs_final-plant_adr3       = 'Makhan Nagar, Narmadapuram MP, 461661' ##NO_TEXT.
        gs_final-plant_gstin      = '23AAJCB8215E1ZZ' ##NO_TEXT.
        gs_final-plant_pan        = 'AAJCB8215E1' ##NO_TEXT.
      ENDIF.

      IF wa_po_hdr-plant = '1002'.
        CLEAR : gs_final-plant_name, gs_final-plant_adr1, gs_final-plant_adr2, gs_final-plant_adr3,
                gs_final-plant_gstin, gs_final-plant_pan.


***************************Start of Changes****************MS******************

             IF wa_po_hdr-yy1_shipto_pdh IS NOT INITIAL.
        SELECT SINGLE * FROM zi_supplier_address
        WHERE supplier = @wa_po_hdr-yy1_shipto_pdh
        INTO @DATA(wa_subcon1).                     "#EC CI_NO_TRANSFORM
*        gs_final-delvto_name    = wa_subcon1-suppliername.
*        gs_final-delvto_adr1    = wa_subcon1-streetname.
*        gs_final-delvto_adr2    = wa_subcon1-cityname && wa_subcon1-postalcode.
*        gs_final-delvto_state   = wa_subcon1-regio.
*        gs_final-delvto_mob     = wa_subcon1-phonenumber1.
*        gs_final-delvto_gst     = wa_subcon1-taxnumber3.
*        gs_final-delvto_pan     = wa_subcon1-businesspartnerpannumber.

        gs_final-plant_name       = wa_subcon1-suppliername ##NO_TEXT.
        gs_final-plant_adr2       = wa_subcon1-streetname ##NO_TEXT.
        gs_final-plant_adr3       = wa_subcon1-cityname && wa_subcon1-postalcode ##NO_TEXT.
*        gs_final-plant_gstin      = '24AAFCH2454K1ZL' ##NO_TEXT.
*        gs_final-plant_pan        = 'AADCB4295Q' ##NO_TEXT.

        else.

         gs_final-plant_name       = wa_plant_address-plantname ##NO_TEXT.
        gs_final-plant_adr2       = wa_plant_address-streetname ##NO_TEXT.
        gs_final-plant_adr3       = wa_plant_address-streetprefixname1 && wa_plant_address-regionname ##NO_TEXT.
*         gs_final-delvto_state   = wa_plant_address-regionname.
*        gs_final-plant_gstin      = '24AAFCH2454K1ZL' ##NO_TEXT.
*        gs_final-plant_pan        = 'AADCB4295Q' ##NO_TEXT.

             IF wa_po_hdr-plant  = '1001'.
          gs_final-plant_gstin   = '03AADCB4295Q1ZA'.
          gs_final-plant_pan     = 'AADCB4295Q'.

           ELSEIF wa_po_hdr-plant  = '1002'.

*        gs_final-plant_gstin      = '24AAFCH2454K1ZL' ##NO_TEXT.
         gs_final-plant_gstin      = '03AADCB4295Q1ZA' ##NO_TEXT.

        gs_final-plant_pan        = 'AADCB4295Q' ##NO_TEXT.

        ELSEIF wa_po_hdr-plant  = '2001'.
          gs_final-plant_gstin     = '03AADCB6737H1ZU'.
          gs_final-plant_pan      = 'AADCB6737H'.
        ELSEIF wa_po_hdr-plant  = '3001'.
          gs_final-plant_gstin     = '24AAFCH2454K1ZL'.
          gs_final-plant_pan    = 'AAFCH2454K'.
        ELSEIF wa_po_hdr-plant  = '4001'.
          gs_final-plant_gstin     = '23AAJCB8215E1ZZ'.
          gs_final-plant_gstin    = 'AAJCB8215E1'.
        ENDIF.



*        gs_final-plant_name       = 'HHF Kandla' ##NO_TEXT.
**        gs_final-plant_adr1       = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
*        gs_final-plant_adr2       = 'PLOT NO. 362, GIDC IND. ESTATE,' ##NO_TEXT.
*        gs_final-plant_adr3       = 'VILL. MITHIROHAR, GANDHIDHAM (KANDLA) GUJRAT 370201' ##NO_TEXT.
*        gs_final-plant_gstin      = '24AAFCH2454K1ZL' ##NO_TEXT.
*        gs_final-plant_pan        = 'AADCB4295Q' ##NO_TEXT.
         endif.
      ENDIF.

**********************************END OF CHANGES******************************

      IF wa_po_hdr-plant = '4001'.
        CLEAR : gs_final-plant_name, gs_final-plant_adr1, gs_final-plant_adr2, gs_final-plant_adr3,
                gs_final-plant_gstin, gs_final-plant_pan.
        gs_final-plant_name       = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
*        gs_final-plant_adr1       = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
        gs_final-plant_adr2       = 'M-14A VILL. MOHASA BABAI,' ##NO_TEXT.
        gs_final-plant_adr3       = 'INDSUTRIAL AREA MAKHAN NAGAR NARMADAPURAM M.P 461661' ##NO_TEXT.
        gs_final-plant_gstin      = '23AAJCB8215E1ZZ' ##NO_TEXT.
        gs_final-plant_pan        = 'AAJCB8215E1' ##NO_TEXT.
      ENDIF.

      gs_final-po_type        = wa_po_hdr-incotermsclassification.

      IF im_action = 'poprint'.
        gs_final-purchase_order = wa_po_hdr-purchaseorder.
        gs_final-po_date        = wa_po_hdr-purchaseorderdate+6(2) && '.' && wa_po_hdr-purchaseorderdate+4(2) && '.'
                                  && wa_po_hdr-purchaseorderdate+0(4).
      ELSEIF im_action = 'saprint'.
        gs_final-purchase_order = wa_sa_hdr-schedulingagreement.
        gs_final-po_date        = wa_sa_hdr-schedulelinedeliverydate+6(2) && '.' && wa_sa_hdr-schedulelinedeliverydate+4(2) && '.'
                                  && wa_sa_hdr-schedulelinedeliverydate+0(4).
      ENDIF.

      gs_final-amd_no         = ''.
      gs_final-amd_date       = ''.
      gs_final-currency       = wa_po_hdr-documentcurrency.
      gs_final-po_ref_no      = wa_po_hdr-referencedeliveryaddressid.
      gs_final-bardana_ded    = wa_po_hdr-yy1_bardana_deduction_pdh.
      gs_final-lifting_from   = wa_po_hdr-incotermslocation1.
      gs_final-policy_no      = wa_po_hdr-yy1_policy_no_pdh.

      SELECT SINGLE supplier FROM i_purchaseorderpartnerapi01 WHERE purchaseorder EQ @wa_po_hdr-purchaseorder
      INTO @DATA(supplier_code).
      SELECT SINGLE suppliername FROM i_supplier WHERE supplier EQ @supplier_code INTO @gs_final-broker_name.

      SHIFT wa_supplier_adr-supplier LEFT DELETING LEADING '0'.
      gs_final-vend_code      = wa_supplier_adr-supplier.
      gs_final-vend_name      = wa_supplier_adr-suppliername.
      gs_final-vend_adr1      = wa_supplier_adr-streetname.
      gs_final-vend_adr2      = wa_supplier_adr-districtname.
      gs_final-vend_adr3      = wa_supplier_adr-region && '-' && wa_supplier_adr-postalcode.
      gs_final-vend_gstin     = wa_supplier_adr-taxnumber3.
      gs_final-vend_pan       = wa_supplier_adr-businesspartnerpannumber.
      gs_final-vend_mail      = wa_supplier_adr-emailaddress.
      gs_final-vend_mob       = wa_supplier_adr-phonenumber1.


*******************Start of Changes ****************MS*************

      select single * from  I_Purchaseorderapi01 WHERE PurchaseOrder EQ @im_ponum
                                                           AND Purchaseordertype EQ 'ZSTO'
                                                           AND CompanyCode EQ '1000'
                                                           into  @data(ls_billaddr).

      SELECT SINGLE * FROM ZI_Plant
       WHERE plant EQ @ls_billaddr-SupplyingPlant
        INTO @DATA(WA_PLATADDR) .


*      gs_final-billto_name    = wa_plant_address-plantname.
       gs_final-billto_name    = WA_PLATADDR-PlantName.

*      gs_final-billto_adr1    = wa_plant_address-streetname.
       gs_final-billto_adr1    = WA_PLATADDR-streetname.
*       gs_final-billto_adr2    = wa_plant_address-streetprefixname1.
*      gs_final-billto_state   = wa_plant_address-regionname.
       gs_final-billto_state   = WA_PLATADDR-Region.

       gs_final-billto_mob     = ''.




          IF wa_po_hdr-plant  = '1002'.
       gs_final-plant   = '1002'.
        gs_final-billto_gst     = '03AADCB4295Q1ZA'.
        gs_final-billto_pan     = 'AADCB4295Q'.



      ELSEIF wa_po_hdr-plant  = '1001'.
       gs_final-plant   = '1001'.
        gs_final-billto_gst     = '03AADCB4295Q1ZA'.
        gs_final-billto_pan     = 'AADCB4295Q'.
      ELSEIF wa_po_hdr-plant  = '2001'.
       gs_final-plant   = '2001'.
        gs_final-billto_gst     = '03AADCB6737H1ZU'.
        gs_final-billto_pan     = 'AADCB6737H'.
      ELSEIF wa_po_hdr-plant  = '3001'.
       gs_final-plant   = '3001'.
        gs_final-billto_gst     = '24AAFCH2454K1ZL'.
        gs_final-billto_pan     = 'AAFCH2454K'.
      ELSEIF wa_po_hdr-plant  = '4001'.
       gs_final-plant   = '4001'.
        gs_final-billto_gst     = '23AAJCB8215E1ZZ'.
        gs_final-billto_pan     = 'AAJCB8215E1'.
      ENDIF.


********************End of Changes*********************************************

      gs_final-billto_cin     = ''.

      IF wa_po_hdr-yy1_shipto_pdh IS NOT INITIAL.
        SELECT SINGLE * FROM zi_supplier_address
        WHERE supplier = @wa_po_hdr-yy1_shipto_pdh
        INTO @DATA(wa_subcon).                     "#EC CI_NO_TRANSFORM
        gs_final-delvto_name    = wa_subcon-suppliername.
        gs_final-delvto_adr1    = wa_subcon-streetname.
        gs_final-delvto_adr2    = wa_subcon-cityname && wa_subcon-postalcode.
        gs_final-delvto_state   = wa_subcon-regio.
        gs_final-delvto_mob     = wa_subcon-phonenumber1.
        gs_final-delvto_gst     = wa_subcon-taxnumber3.
        gs_final-delvto_pan     = wa_subcon-businesspartnerpannumber.

      ELSEIF wa_po_hdr-subcontractor IS NOT INITIAL.
        SELECT SINGLE * FROM zi_supplier_address
        WHERE supplier = @wa_po_hdr-subcontractor
        INTO @wa_subcon.
        gs_final-delvto_name    = wa_subcon-suppliername.
        gs_final-delvto_adr1    = wa_subcon-streetname && wa_subcon-districtname.
        gs_final-delvto_adr2    = wa_subcon-cityname && wa_subcon-postalcode.
        gs_final-delvto_state   = wa_subcon-regio.
        gs_final-delvto_mob     = wa_subcon-phonenumber1.
        gs_final-delvto_gst     = wa_subcon-taxnumber3.
        gs_final-delvto_pan     = wa_subcon-businesspartnerpannumber.

      ELSE.
        gs_final-delvto_name    = wa_plant_address-plantname.
        gs_final-delvto_adr1    = wa_plant_address-streetname.
        gs_final-delvto_adr2    = wa_plant_address-streetprefixname1.
        gs_final-delvto_state   = wa_plant_address-regionname.
        gs_final-delvto_mob     = ''.



          IF wa_po_hdr-plant  = '1002'.
          gs_final-delvto_gst     = '24AADCB4295Q1Z6'.
          gs_final-delvto_pan     = 'AADCB4295Q'.


        ELSEIF wa_po_hdr-plant  = '1001'.
          gs_final-delvto_gst     = '03AADCB4295Q1ZA'.
          gs_final-delvto_pan     = 'AADCB4295Q'.
        ELSEIF wa_po_hdr-plant  = '2001'.
          gs_final-delvto_gst     = '03AADCB6737H1ZU'.
          gs_final-delvto_pan     = 'AADCB6737H'.
        ELSEIF wa_po_hdr-plant  = '3001'.
          gs_final-delvto_gst     = '24AAFCH2454K1ZL'.
          gs_final-delvto_pan    = 'AAFCH2454K'.
        ELSEIF wa_po_hdr-plant  = '4001'.
          gs_final-delvto_gst     = '23AAJCB8215E1ZZ'.
          gs_final-delvto_pan    = 'AAJCB8215E1'.
        ENDIF.
      ENDIF.
      gs_final-payment_terms  = payment_term-paymenttermsname.
      IF gs_final-payment_terms IS INITIAL.
        gs_final-payment_terms  = payment_term-paymenttermsdescription.
      ENDIF.
      gs_final-freight        = ''.
      gs_final-insurance      = ''.
      gs_final-delivery_term  = ''.

      READ TABLE it_charges INTO DATA(wa_charges) WITH KEY conditiontype = 'ZFV1'.
      IF sy-subrc = 0.
        gs_final-freight_chrg   = wa_charges-conditionamount.
      ELSE.
        READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZFVA'.
        IF sy-subrc = 0.
          gs_final-freight_chrg   = wa_charges-conditionamount.
        ELSE.
          READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZFRA'.
          IF sy-subrc = 0.
            gs_final-freight_chrg   = wa_charges-conditionamount.
          ELSE.
            READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZFRB'.
            IF sy-subrc = 0.
              gs_final-freight_chrg   = wa_charges-conditionamount.
            ELSE.
              READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZFRC'.
              IF sy-subrc = 0.
                gs_final-freight_chrg   = wa_charges-conditionamount.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      gs_final-packing_chrg   = ''.

      CLEAR wa_charges.
      READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZOTH'.
      IF sy-subrc = 0.
        gs_final-other_chrg     = wa_charges-conditionamount.
      ELSE.
        READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZOH1'.
        IF sy-subrc = 0 AND wa_po_hdr-purchaseordertype = 'ZPDY'.
          gs_final-other_chrg     = wa_charges-conditionamount.
        ENDIF.
      ENDIF.

      CLEAR wa_charges.
      READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZOTH'.
      IF sy-subrc = 0 AND wa_po_hdr-purchaseordertype = 'ZIMP'.
        gs_final-other_chrg = wa_charges-conditionamount.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZBC1'.
        gs_final-bookerage     = gs_final-bookerage + wa_charges-conditionamount.
      ENDLOOP.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZBC1' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        brokerage = charge_per && '% / ' && gs_final-bookerage.
      ELSE.
        brokerage = gs_final-bookerage.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZLAB'.
        gs_final-labour_chrg     = gs_final-labour_chrg + wa_charges-conditionamount.
      ENDLOOP.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZLAB' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        labour_chrg = charge_per && '% / ' && gs_final-labour_chrg.
      ELSE.
        labour_chrg = gs_final-labour_chrg.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZHB0'.
        gs_final-total_discount     = gs_final-total_discount + wa_charges-conditionamount.
      ENDLOOP.
      IF gs_final-total_discount IS INITIAL.
        LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZHB1'.
          gs_final-total_discount     = gs_final-total_discount + wa_charges-conditionamount.
        ENDLOOP.
      ENDIF.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZHB0' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        discount = charge_per && '% / ' && gs_final-total_discount.
      ELSE.
        READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZHB1' conditioncalculationtype = 'A'.
        IF sy-subrc = 0.
          CLEAR charge_per.
          charge_per = wa_charges-conditionratevalue.
          discount = charge_per && '% / ' && gs_final-total_discount.
        ELSE.
          discount = gs_final-total_discount.
        ENDIF.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZLOD'.
        gs_final-loading_chrg     = gs_final-loading_chrg + wa_charges-conditionamount.
      ENDLOOP.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZLOD' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        loading_chrg = charge_per && '% / ' && gs_final-loading_chrg.
      ELSE.
        loading_chrg = gs_final-loading_chrg.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZIN1'.
        gs_final-insurance_chrg     = gs_final-insurance_chrg + wa_charges-conditionamount.
      ENDLOOP.
      IF gs_final-insurance_chrg IS INITIAL.
        LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZIN2'.
          gs_final-insurance_chrg     = gs_final-insurance_chrg + wa_charges-conditionamount.
        ENDLOOP.
      ENDIF.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZIN1' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        insurance = charge_per && '% / ' && gs_final-insurance_chrg.
      ELSE.
        READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZIN2' conditioncalculationtype = 'A'.
        IF sy-subrc = 0.
          CLEAR charge_per.
          charge_per = wa_charges-conditionratevalue.
          insurance = charge_per && '% / ' && gs_final-insurance_chrg.
        ELSE.
          insurance = gs_final-insurance_chrg.
        ENDIF.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZCES'.
        gs_final-mandi_cess     = gs_final-mandi_cess + wa_charges-conditionamount.
      ENDLOOP.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZCES' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        mandi_cess = charge_per && '% / ' && gs_final-mandi_cess.
      ELSE.
        mandi_cess = gs_final-mandi_cess.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZASS'.
        gs_final-asso_chrg     = gs_final-asso_chrg + wa_charges-conditionamount.
      ENDLOOP.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZASS' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        associat_chrg = charge_per && '% / ' && gs_final-asso_chrg.
      ELSE.
        associat_chrg = gs_final-asso_chrg.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZDHM'.
        gs_final-dhami     = gs_final-dhami + wa_charges-conditionamount.
      ENDLOOP.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZDHM' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        dhami = charge_per && '% / ' && gs_final-dhami.
      ELSE.
        dhami = gs_final-dhami.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZMFE'.
        gs_final-market_fee     = gs_final-market_fee + wa_charges-conditionamount.
      ENDLOOP.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZMFE' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        market_fee = charge_per && '% / ' && gs_final-market_fee.
      ELSE.
        market_fee = gs_final-market_fee.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZHRD'.
        gs_final-hrdf     = gs_final-hrdf + wa_charges-conditionamount.
      ENDLOOP.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZHRD' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        hrdf = charge_per && '% / ' && gs_final-hrdf.
      ELSE.
        hrdf = gs_final-hrdf.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZAUC'.
        gs_final-auction_chrg     = gs_final-auction_chrg + wa_charges-conditionamount.
      ENDLOOP.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZAUC' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        auction_chrg = charge_per && '% / ' && gs_final-auction_chrg.
      ELSE.
        auction_chrg = gs_final-auction_chrg.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZHAM'.
        gs_final-hammali_chrg     = gs_final-hammali_chrg + wa_charges-conditionamount.
      ENDLOOP.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZHAM' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        hammali_chrg = charge_per && '% / ' && gs_final-hammali_chrg.
      ELSE.
        hammali_chrg = gs_final-hammali_chrg.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZDLA'.
        gs_final-dala_chrg     = gs_final-dala_chrg + wa_charges-conditionamount.
      ENDLOOP.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZDLA' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        dala_chrg = charge_per && '% / ' && gs_final-dala_chrg.
      ELSE.
        dala_chrg = gs_final-dala_chrg.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZOXR'.
        gs_final-oxner_chrg     = gs_final-oxner_chrg + wa_charges-conditionamount.
      ENDLOOP.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZOXR' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        oxner_chrg = charge_per && '% / ' && gs_final-oxner_chrg.
      ELSE.
        oxner_chrg = gs_final-oxner_chrg.
      ENDIF.

      CLEAR wa_charges.
      LOOP AT it_charges INTO wa_charges WHERE conditiontype = 'ZTUL'.
        gs_final-tulai_chrg     = gs_final-tulai_chrg + wa_charges-conditionamount.
      ENDLOOP.
      CLEAR wa_charges.
      READ TABLE it_charges_pdy INTO wa_charges WITH KEY conditiontype = 'ZTUL' conditioncalculationtype = 'A'.
      IF sy-subrc = 0.
        CLEAR charge_per.
        charge_per = wa_charges-conditionratevalue.
        tulai_chrg = charge_per && '% / ' && gs_final-tulai_chrg.
      ELSE.
        tulai_chrg = gs_final-tulai_chrg.
      ENDIF.

      CLEAR wa_charges.
      READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZACD'.
      IF sy-subrc = 0.
        gs_final-add_cust_duty = wa_charges-conditionamount.
      ENDIF.

      CLEAR wa_charges.
      READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZSAD'.
      IF sy-subrc = 0.
        gs_final-spl_add_duty = wa_charges-conditionamount.
      ENDIF.

      CLEAR wa_charges.
      READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'JSWC'.
      IF sy-subrc = 0.
        gs_final-social_welfare = wa_charges-conditionamount.
      ENDIF.

      CLEAR wa_charges.
      READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZCVD'.
      IF sy-subrc = 0.
        gs_final-counter_duty = wa_charges-conditionamount.
      ENDIF.

      CLEAR wa_charges.
      READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZCH1'.
      IF sy-subrc = 0.
        gs_final-cha_charges = wa_charges-conditionamount.
      ELSE.
        READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZCH2'.
        IF sy-subrc = 0.
          gs_final-cha_charges = wa_charges-conditionamount.
        ENDIF.
      ENDIF.

      CLEAR wa_charges.
      READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZADD'.
      IF sy-subrc = 0.
        gs_final-anit_dump_duty = wa_charges-conditionamount.
      ENDIF.

      CLEAR wa_charges.
      READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'JCDB'.
      IF sy-subrc = 0.
        gs_final-custom_charges = wa_charges-conditionamount.
      ENDIF.

      CLEAR wa_charges.
      READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZLC1'.
      IF sy-subrc = 0.
        gs_final-landing_chrg = wa_charges-conditionamount.
      ENDIF.

      CLEAR wa_charges.
      READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZPK1'.
      IF sy-subrc = 0.
        gs_final-pack_and_forw = wa_charges-conditionamount.
      ELSE.
        READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZPK2'.
        IF sy-subrc = 0.
          gs_final-pack_and_forw = wa_charges-conditionamount.
        ENDIF.
      ENDIF.

      gs_final-tot_cgst       = ''.
      gs_final-tot_sgst       = ''.
      gs_final-tot_igst       = ''.
      gs_final-remarks        = ''.


      DATA:
        lv_cgst_rate TYPE p LENGTH 13 DECIMALS 2,
        lv_sgst_rate TYPE p LENGTH 13 DECIMALS 2,
        lv_igst_rate TYPE p LENGTH 13 DECIMALS 2,
        lv_cgst_sum  TYPE p LENGTH 16 DECIMALS 2,
        lv_sgst_sum  TYPE p LENGTH 16 DECIMALS 2,
        lv_igst_sum  TYPE p LENGTH 16 DECIMALS 2.

      DATA: split0 TYPE c LENGTH 10,
            split1 TYPE c LENGTH 10,
            split2 TYPE c LENGTH 10.

      """**Preparing Item Data
      IF im_action = 'poprint'.
        SORT it_po_data BY purchaseorder purchaseorderitem.
        LOOP AT it_po_data INTO DATA(wa_po_itm).

          READ TABLE it_gst_rate INTO DATA(ls_gst_rate) WITH KEY taxcode = wa_po_itm-taxcode.
          IF sy-subrc EQ 0.

            IF ls_gst_rate-cgstrate IS NOT INITIAL.
              CONDENSE ls_gst_rate-cgstrate.
              lv_cgst_rate = ls_gst_rate-cgstrate.
            ENDIF.

            IF ls_gst_rate-sgstrate IS NOT INITIAL.
              CONDENSE ls_gst_rate-sgstrate.
              lv_sgst_rate = ls_gst_rate-sgstrate.
            ENDIF.

            IF ls_gst_rate-igstrate IS NOT INITIAL.
              CONDENSE ls_gst_rate-igstrate.
              lv_igst_rate = ls_gst_rate-igstrate.
            ENDIF.

          ENDIF.

          ls_item-purchase_order = wa_po_itm-purchaseorder.
          ls_item-purchase_item  = wa_po_itm-purchaseorderitem.
          ls_item-sl_no          = ''.
          CONCATENATE wa_po_itm-material '/' wa_po_itm-yy1_sampleinspno_pdi INTO ls_item-item_code
          SEPARATED BY space.
          SHIFT ls_item-item_code LEFT DELETING LEADING '0'.
*          ls_item-item_name      = wa_po_itm-productdescription && '/' && wa_po_itm-yy1_item_remark_pdi.
          CONCATENATE wa_po_itm-productdescription '/' wa_po_itm-yy1_item_remark_pdi INTO ls_item-item_name
          SEPARATED BY space.
          READ TABLE it_hsn INTO DATA(wa_hsn) WITH KEY product = wa_po_itm-material.
          IF sy-subrc = 0.
            ls_item-item_hsn       = wa_hsn-consumptiontaxctrlcode.
          ENDIF.
          ls_item-item_brand     = wa_po_itm-suppliermaterialnumber.
          READ TABLE it_bags INTO DATA(wa_bags) WITH KEY product = wa_po_itm-material.
          IF wa_bags-quantitydenominator IS NOT INITIAL.
            ls_item-item_no_bag = wa_po_itm-orderquantity / ( wa_bags-quantitynumerator / wa_bags-quantitydenominator ).
            CLEAR: split0, split1, split2.
            split0 = ls_item-item_no_bag.
            SPLIT split0 AT '.' INTO split1 split2.
            CLEAR split0.
            IF split2 GE '50'.
              split0 = split1 + 1.
            ELSE.
              split0 = split1.
            ENDIF.
            CONDENSE split0.
            ls_item-item_no_bag = split0.
          ENDIF.
          IF wa_po_itm-purchaseorderquantityunit  = 'ST'.
            ls_item-item_unit    = 'PC'.
          ELSE.
            ls_item-item_unit    = wa_po_itm-purchaseorderquantityunit.
          ENDIF.
          ls_item-item_qty       = wa_po_itm-orderquantity.
          gs_final-tot_qty       = gs_final-tot_qty + wa_po_itm-orderquantity.
          IF wa_po_itm-orderquantity IS NOT INITIAL.
            ls_item-item_rate      = wa_po_itm-netamount / wa_po_itm-orderquantity.
          ENDIF.

          CLEAR wa_charges.
          READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZHB2' purchaseorderitem = wa_po_itm-purchaseorderitem.
          IF sy-subrc = 0.
            ls_item-disc_amt       = wa_charges-conditionamount.
          ELSE.
            READ TABLE it_charges INTO wa_charges WITH KEY conditiontype = 'ZHB3' purchaseorderitem = wa_po_itm-purchaseorderitem.
            IF sy-subrc = 0.
              ls_item-disc_amt       = wa_charges-conditionamount.
            ENDIF.
          ENDIF.

          SELECT SINGLE schedulelinedeliverydate FROM i_purordschedulelineapi01
          WHERE purchaseorder EQ @wa_po_itm-purchaseorder AND purchaseorderitem EQ @wa_po_itm-purchaseorderitem
          INTO @DATA(dis_del_dt).

          IF dis_del_dt IS NOT INITIAL.
            ls_item-disc_del_dt = dis_del_dt+6(2) && '.' && dis_del_dt+4(2) && '.' && dis_del_dt+0(4).
            CLEAR dis_del_dt.
          ENDIF.
          ls_item-cgst_per       = lv_cgst_rate.
          ls_item-sgst_per       = lv_sgst_rate.
          ls_item-igst_per       = lv_igst_rate.

          ls_item-cgst_amt       = ( wa_po_itm-netamount * lv_cgst_rate ) / 100.
          ls_item-sgst_amt       = ( wa_po_itm-netamount * lv_sgst_rate ) / 100.
          ls_item-igst_amt       = ( wa_po_itm-netamount * lv_igst_rate ) / 100.

          lv_cgst_sum   = lv_cgst_sum + ( wa_po_itm-netamount * lv_cgst_rate ) / 100.
          lv_sgst_sum   = lv_sgst_sum + ( wa_po_itm-netamount * lv_sgst_rate ) / 100.
          lv_igst_sum   = lv_igst_sum + ( wa_po_itm-netamount * lv_igst_rate ) / 100.

          ls_item-net_val        = wa_po_itm-netamount.
          gs_final-subtotal      = gs_final-subtotal + wa_po_itm-netamount.
          APPEND ls_item TO lt_item.
          CLEAR : wa_po_itm, wa_hsn, wa_charges, ls_item, wa_bags, ls_gst_rate, lv_cgst_rate, lv_sgst_rate,
                  lv_igst_rate.
        ENDLOOP.

*        IF gs_final-total_discount IS INITIAL.
*          LOOP AT lt_item INTO ls_item.
*            gs_final-total_discount = gs_final-total_discount + ls_item-disc_amt.
*          ENDLOOP.
*        ENDIF.
      ENDIF.

      IF im_action = 'saprint'.
        LOOP AT it_sa_data INTO DATA(wa_sa_itm).

          READ TABLE it_gst_rate INTO ls_gst_rate WITH KEY taxcode = wa_sa_itm-taxcode.
          IF sy-subrc EQ 0.

            IF ls_gst_rate-cgstrate IS NOT INITIAL.
              CONDENSE ls_gst_rate-cgstrate.
              lv_cgst_rate = ls_gst_rate-cgstrate.
            ENDIF.

            IF ls_gst_rate-sgstrate IS NOT INITIAL.
              CONDENSE ls_gst_rate-sgstrate.
              lv_sgst_rate = ls_gst_rate-sgstrate.
            ENDIF.

            IF ls_gst_rate-igstrate IS NOT INITIAL.
              CONDENSE ls_gst_rate-igstrate.
              lv_igst_rate = ls_gst_rate-igstrate.
            ENDIF.

          ENDIF.

          ls_item-purchase_order = wa_sa_itm-schedulingagreement.
          ls_item-purchase_item  = wa_sa_itm-schedulingagreementitem.
          ls_item-sl_no          = ''.
          SHIFT wa_sa_itm-material LEFT DELETING LEADING '0'.
          ls_item-item_code      = wa_sa_itm-material.
          ls_item-item_name      = wa_sa_itm-productdescription.
          ls_item-item_del_dt    = wa_sa_itm-schedulelinedeliverydate+6(2) && '.' &&
                                   wa_sa_itm-schedulelinedeliverydate+4(2) && '.' &&
                                   wa_sa_itm-schedulelinedeliverydate+0(4).
          ls_item-item_unit      = wa_sa_itm-orderpriceunit.
          ls_item-item_qty       = wa_sa_itm-netpricequantity.
          gs_final-tot_qty       = gs_final-tot_qty + ls_item-item_qty.
          IF ls_item-item_qty IS NOT INITIAL.
            ls_item-item_rate      = wa_sa_itm-netpriceamount / ls_item-item_qty.
          ENDIF.
          ls_item-disc_amt       = ''.

          ls_item-cgst_amt       = lv_cgst_rate.
          ls_item-sgst_amt       = lv_sgst_rate.
          ls_item-igst_amt       = lv_igst_rate.

          lv_cgst_sum   = lv_cgst_sum + ( wa_sa_itm-netpriceamount * lv_cgst_rate ) / 100.
          lv_sgst_sum   = lv_sgst_sum + ( wa_sa_itm-netpriceamount * lv_sgst_rate ) / 100.
          lv_igst_sum   = lv_igst_sum + ( wa_sa_itm-netpriceamount * lv_igst_rate ) / 100.

          ls_item-net_val        = wa_sa_itm-netpriceamount.
          gs_final-subtotal      = gs_final-subtotal + ls_item-net_val.
          APPEND ls_item TO lt_item.

        ENDLOOP.
      ENDIF.

      gs_final-cgst_rate = lv_cgst_rate && '%'.
      gs_final-sgst_rate = lv_sgst_rate && '%'.
      gs_final-igst_rate = lv_igst_rate && '%'.
      CONDENSE : gs_final-cgst_rate, gs_final-sgst_rate, gs_final-igst_rate.

      gs_final-tot_cgst       = lv_cgst_sum.
      gs_final-tot_sgst       = lv_sgst_sum.
      gs_final-tot_igst       = lv_igst_sum.

      gs_final-tot_gst = gs_final-tot_cgst  + gs_final-tot_sgst  + gs_final-tot_igst ##TYPE.

      IF wa_po_hdr-purchaseordertype = 'ZPDY'.

        gs_final-grand_total = gs_final-subtotal + gs_final-tot_cgst + gs_final-tot_sgst + gs_final-tot_igst +
                               gs_final-freight_chrg + gs_final-packing_chrg + gs_final-other_chrg +
                               gs_final-insurance_chrg + gs_final-bookerage + gs_final-labour_chrg +
                               gs_final-total_discount + gs_final-loading_chrg + gs_final-mandi_cess +
                               gs_final-asso_chrg + gs_final-dhami + gs_final-market_fee + gs_final-hrdf +
                               gs_final-auction_chrg + gs_final-hammali_chrg + gs_final-dala_chrg +
                               gs_final-oxner_chrg + gs_final-tulai_chrg.

        IF gs_final-tot_qty IS NOT INITIAL.
          gs_final-product_cost = gs_final-grand_total / gs_final-tot_qty.
        ENDIF.

      ELSE.

        gs_final-grand_total = gs_final-subtotal + gs_final-tot_cgst + gs_final-tot_sgst + gs_final-tot_igst +
                               gs_final-freight_chrg + gs_final-packing_chrg + gs_final-other_chrg +
                               gs_final-insurance_chrg + gs_final-bookerage + gs_final-labour_chrg +
                               gs_final-total_discount + gs_final-loading_chrg.

      ENDIF.

      grand_total = gs_final-grand_total.
      CREATE OBJECT lo_amt_words.
      grand_total_words = lo_amt_words->number_to_words( iv_num = grand_total ).
      REPLACE ALL OCCURRENCES OF 'Rupees' IN grand_total_words WITH space ##NO_TEXT.
      CONCATENATE 'Rupees' grand_total_words INTO gs_final-grand_tot_word SEPARATED BY space ##NO_TEXT.
      CONDENSE gs_final-grand_tot_word.

      total_gst = gs_final-tot_gst.
      total_gst_words = lo_amt_words->number_to_words( iv_num = total_gst ).
      REPLACE ALL OCCURRENCES OF 'Rupees' IN total_gst_words WITH space ##NO_TEXT.
      CONCATENATE 'Rupees' total_gst_words INTO gs_final-tot_gst_word SEPARATED BY space ##NO_TEXT.
      CONDENSE gs_final-tot_gst_word.

    ENDIF.

    """**Combining Header & Item
    INSERT LINES OF lt_item INTO TABLE gs_final-gt_item.
    APPEND gs_final TO gt_final.
    et_final[] = gt_final[].

  ENDMETHOD.


  METHOD get_pr_data.
    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT * FROM i_purchaserequisitionitemapi01
             WHERE purchaserequisition EQ @im_prnum
             INTO TABLE @DATA(it_pr_data).

    IF it_pr_data IS NOT INITIAL.

      READ TABLE it_pr_data INTO DATA(wa_pr_hdr) INDEX 1. "#EC CI_NOORDER

      SELECT SINGLE
plant,
plantname,
addressid,
addresseefullname,
streetprefixname1,
streetprefixname2,
streetname,
streetsuffixname1,
districtname,
cityname,
postalcode,
addressrepresentationcode,
region,
country,
regionname,
addresspersonid,
street,
housenumber,
formofaddress,
addresstimezone,
phoneareacodesubscribernumber,
emailaddress

      FROM zi_plant_address
                      WHERE plant EQ @wa_pr_hdr-plant
                      INTO @DATA(plant_adr).       "#EC CI_NO_TRANSFORM

      """**Preparing Header Data
      gs_final_pr-plant            = wa_pr_hdr-plant.
      gs_final_pr-plant_name       = plant_adr-plantname.

      IF wa_pr_hdr-companycode     = '1000'.
        gs_final_pr-comp_name        = 'Bhagwati Lacto Vegetarian Exports Pvt. Ltd.' ##NO_TEXT.
        gs_final_pr-plant_adr1       = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
        gs_final_pr-plant_adr2       = 'Works : Rural Focal Point Mana Singh wala ,' ##NO_TEXT.
        gs_final_pr-plant_adr3       = 'Firozepur Moga Road' ##NO_TEXT.
        gs_final_pr-plant_gstin      = '03AADCB4295Q1ZA' ##NO_TEXT.

      ELSEIF wa_pr_hdr-companycode = '2000'.
        gs_final_pr-comp_name        = 'Bhagwati Lacto Foods Pvt. Ltd.' ##NO_TEXT.
*        gs_final_pr-plant_adr1       = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
        gs_final_pr-plant_adr2       = 'Works : VILL. RUKNA MUNGLA, FARIDKOT ROAD,' ##NO_TEXT.
        gs_final_pr-plant_adr3       = 'Ferozepur, Punjab, 152001' ##NO_TEXT.
        gs_final_pr-plant_gstin      = '03AADCB6737H1ZU' ##NO_TEXT.

      ELSEIF wa_pr_hdr-companycode = '3000'.
        gs_final_pr-comp_name        = 'Healthy Harvested Foods Private Limited' ##NO_TEXT.
*        gs_final_pr-plant_adr1       = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
        gs_final_pr-plant_adr2       = 'Works : PLOT NO100, SECTOR 1/A,' ##NO_TEXT.
        gs_final_pr-plant_adr3       = 'GANDHIDHAM, Kachchh, Gujarat, 370201' ##NO_TEXT.
        gs_final_pr-plant_gstin      = '24AAFCH2454K1ZL' ##NO_TEXT.
      ENDIF.

      IF wa_pr_hdr-plant = '1002'.
        CLEAR : gs_final_pr-plant_name, gs_final_pr-plant_adr1, gs_final_pr-plant_adr2, gs_final_pr-plant_adr3,
                gs_final_pr-plant_gstin.", gs_final_pr-plant_pan.
        gs_final_pr-plant_name       = 'HHF Kandla' ##NO_TEXT.
*        gs_final_pr-plant_adr1       = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
        gs_final_pr-plant_adr2       = 'PLOT NO. 362, GIDC IND. ESTATE,' ##NO_TEXT.
        gs_final_pr-plant_adr3       = 'VILL. MITHIROHAR, GANDHIDHAM (KANDLA) GUJRAT 370201' ##NO_TEXT.
        gs_final_pr-plant_gstin      = '24AAFCH2454K1ZL' ##NO_TEXT.
*        gs_final_pr-plant_pan        = 'AADCB4295Q' ##NO_TEXT.
      ENDIF.

      IF wa_pr_hdr-plant = '4001'.
        CLEAR : gs_final_pr-plant_name, gs_final_pr-plant_adr1, gs_final_pr-plant_adr2, gs_final_pr-plant_adr3,
                gs_final_pr-plant_gstin.", gs_final_pr-plant_pan.
        gs_final_pr-plant_name       = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
*        gs_final_pr-plant_adr1       = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
        gs_final_pr-plant_adr2       = 'M-14A VILL. MOHASA BABAI,' ##NO_TEXT.
        gs_final_pr-plant_adr3       = 'INDSUTRIAL AREA MAKHAN NAGAR NARMADAPURAM M.P 461661' ##NO_TEXT.
        gs_final_pr-plant_gstin      = '23AAJCB8215E1ZZ' ##NO_TEXT.
*        gs_final_pr-plant_pan        = 'AAJCB8215E1' ##NO_TEXT.
      ENDIF.

      gs_final_pr-plant_phone   = ''.
      gs_final_pr-plant_fax     = ''.
      gs_final_pr-pr_no         = wa_pr_hdr-purchaserequisition.
      gs_final_pr-pr_date       = wa_pr_hdr-purchasereqncreationdate+6(2) && '.' &&
                                  wa_pr_hdr-purchasereqncreationdate+4(2) && '.' &&
                                  wa_pr_hdr-purchasereqncreationdate+0(4).
      gs_final_pr-pr_group      = wa_pr_hdr-purchaserequisitiontype.
      gs_final_pr-requisitioner = ''.
      gs_final_pr-cost_no       = ''.
      gs_final_pr-cost_name     = ''.
      gs_final_pr-equipment     = ''.
      gs_final_pr-plant_cin     = ''.
      gs_final_pr-status        = ''.

      """**Preparing Item Data
      LOOP AT it_pr_data INTO DATA(wa_pr_itm).
        ls_item_pr-sl_no        = ''.
        ls_item_pr-pr_no        = wa_pr_itm-purchaserequisition.
        ls_item_pr-item_code    = wa_pr_itm-material.
        ls_item_pr-item_name    = wa_pr_itm-purchaserequisitionitemtext.
        ls_item_pr-item_part    = ''.
        ls_item_pr-item_qty     = wa_pr_itm-requestedquantity.
        ls_item_pr-item_rate    = ''.
        IF wa_pr_itm-baseunit   = 'ST'.
          ls_item_pr-item_unit    = 'PC'.
        ELSE.
          ls_item_pr-item_unit    = wa_pr_itm-baseunit.
        ENDIF.
        ls_item_pr-item_requi   = wa_pr_itm-requisitionername.
        ls_item_pr-item_remk    = wa_pr_itm-yy1_pritmeremarks_pri.
        ls_item_pr-unrest_stock = ''.
        ls_item_pr-net_amt      = ''.
        gs_final_pr-total_qty   = gs_final_pr-total_qty + ls_item_pr-item_qty.
        APPEND ls_item_pr TO lt_item_pr.
      ENDLOOP.

      """**Combining Header & Item
      INSERT LINES OF lt_item_pr INTO TABLE gs_final_pr-gt_item_pr.
      APPEND gs_final_pr TO gt_final_pr.
      et_final_pr[] = gt_final_pr[].

    ENDIF.

  ENDMETHOD.


  METHOD prep_xml_po_print.

    DATA : heading      TYPE c LENGTH 100,
           sub_heading  TYPE c LENGTH 200,
           draft        TYPE string,
           lv_xml_final TYPE string.

    READ TABLE it_final INTO DATA(ls_final) INDEX 1.
    SHIFT ls_final-purchase_order LEFT DELETING LEADING ''.

    IF im_action = 'poprint'.
      heading = 'Purchase Order' ##NO_TEXT.
    ELSEIF im_action = 'saprint'.
      heading = 'Scheduling Agreement' ##NO_TEXT.
    ENDIF.

    draft = 'X'.

    DATA(lv_xml) =  |<Form>| &&
                    |<PurchaseOrderNode>| &&
                    |<header_1>{ heading }</header_1>| &&
                    |<header_2>{ ls_final-plant_name }</header_2>| &&
                    |<newplant>{ ls_final-plant }</newplant>| &&
                    |<header_3>{ ls_final-plant_adr1 }</header_3>| &&
                    |<header_4>{ ls_final-plant_adr2 }</header_4>| &&
                    |<header_5>{ ls_final-plant_adr3 }</header_5>| &&
                    |<draft>{ draft }</draft>| &&
                    |<gstin>{ ls_final-plant_gstin }</gstin>| &&
                    |<plant_pan>{ ls_final-plant_pan }</plant_pan>| &&
                    |<po_type>{ ls_final-po_type }</po_type>| &&
                    |<po_no>{ ls_final-purchase_order }</po_no>| &&
                    |<po_date>{ ls_final-po_date }</po_date>| &&
                    |<amd_no>{ ls_final-amd_no }</amd_no>| &&
                    |<amd_date>{ ls_final-amd_date }</amd_date>| &&
                    |<currency>{ ls_final-currency }</currency>| &&
                    |<po_ref_no>{ ls_final-po_ref_no }</po_ref_no>| &&
                    |<bardana_ded>{ ls_final-bardana_ded }</bardana_ded>| &&
                    |<lifting_from>{ ls_final-lifting_from }</lifting_from>| &&
                    |<broker>{ ls_final-broker_name }</broker>| &&
                    |<policy_no>{ ls_final-policy_no }</policy_no>| &&
                    |<ven_code>{ ls_final-vend_code }</ven_code>| &&
                    |<ven_name>{ ls_final-vend_name }</ven_name>| &&
                    |<ven_adrs1>{ ls_final-vend_adr1 }</ven_adrs1>| &&
                    |<ven_adrs2>{ ls_final-vend_adr2 }</ven_adrs2>| &&
                    |<ven_adrs3>{ ls_final-vend_adr3 }</ven_adrs3>| &&
                    |<ven_gstin>{ ls_final-vend_gstin }</ven_gstin>| &&
                    |<ven_pan>{ ls_final-vend_pan }</ven_pan>| &&
                    |<ven_mail>{ ls_final-vend_mail }</ven_mail>| &&
                    |<ven_mob>{ ls_final-vend_mob }</ven_mob>| &&
                    |<bill_to_name>{ ls_final-billto_name }</bill_to_name>| &&
                    |<bill_to_adrs1>{ ls_final-billto_adr1 }</bill_to_adrs1>| &&
                    |<bill_to_adrs2>{ ls_final-billto_adr2 }</bill_to_adrs2>| &&
                    |<bill_to_state_code>{ ls_final-billto_state }</bill_to_state_code>| &&
                    |<bill_to_mob>{ ls_final-billto_mob }</bill_to_mob>| &&
                    |<bill_to_gst>{ ls_final-billto_gst }</bill_to_gst>| &&
                    |<bill_to_pan>{ ls_final-billto_pan }</bill_to_pan>| &&
                    |<bill_to_cin>{ ls_final-billto_cin }</bill_to_cin>| &&
                    |<del_to_name>{ ls_final-delvto_name }</del_to_name>| &&
                    |<del_to_adrs1>{ ls_final-delvto_adr1 }</del_to_adrs1>| &&
                    |<del_to_adrs2>{ ls_final-delvto_adr2 }</del_to_adrs2>| &&
                    |<del_to_state_code>{ ls_final-delvto_state }</del_to_state_code>| &&
                    |<del_to_mob>{ ls_final-delvto_mob }</del_to_mob>| &&
                    |<del_to_gst>{ ls_final-delvto_gst }</del_to_gst>| &&
                    |<del_to_pan>{ ls_final-delvto_pan }</del_to_pan>| &&
                    |<payment_terms>{ ls_final-payment_terms }</payment_terms>| &&
                    |<freight>{ ls_final-freight }</freight>| &&
                    |<insurance>{ ls_final-insurance }</insurance>| &&
                    |<delivery_terms>{ ls_final-delivery_term }</delivery_terms>| &&
                    |<total_qty>{ ls_final-tot_qty }</total_qty>| &&
                    |<subtotal>{ ls_final-subtotal }</subtotal>| &&
                    |<cgst_rate>{ ls_final-cgst_rate }</cgst_rate>| &&
                    |<sgst_rate>{ ls_final-sgst_rate }</sgst_rate>| &&
                    |<igst_rate>{ ls_final-igst_rate }</igst_rate>| &&
                    |<total_cgst_amt>{ ls_final-tot_cgst }</total_cgst_amt>| &&
                    |<total_sgst_amt>{ ls_final-tot_sgst }</total_sgst_amt>| &&
                    |<total_igst_amt>{ ls_final-tot_igst }</total_igst_amt>| &&
                    |<total_gst_amt>{ ls_final-tot_gst }</total_gst_amt>| &&
                    |<add_cust_duty>{ ls_final-add_cust_duty }</add_cust_duty>| &&
                    |<spl_add_duty>{ ls_final-spl_add_duty }</spl_add_duty>| &&
                    |<social_welfare>{ ls_final-social_welfare }</social_welfare>| &&
                    |<counter_duty>{ ls_final-counter_duty }</counter_duty>| &&
                    |<cha_charges>{ ls_final-cha_charges }</cha_charges>| &&
                    |<anit_dump_duty>{ ls_final-anit_dump_duty }</anit_dump_duty>| &&
                    |<custom_charges>{ ls_final-custom_charges }</custom_charges>| &&
                    |<landing_charges>{ ls_final-landing_chrg }</landing_charges>| &&
                    |<pack_and_forw>{ ls_final-pack_and_forw }</pack_and_forw>| &&
                    |<freight_charges>{ ls_final-freight_chrg }</freight_charges>| &&
                    |<packing_charges>{ ls_final-packing_chrg }</packing_charges>| &&
                    |<other_charges>{ ls_final-other_chrg }</other_charges>| &&
                    |<bookerage>{ brokerage }</bookerage>| &&
                    |<labour_charges>{ labour_chrg }</labour_charges>| &&
                    |<total_discount>{ discount }</total_discount>| &&
                    |<loading_charges>{ loading_chrg }</loading_charges>| &&
                    |<insurance_charges>{ insurance }</insurance_charges>| &&
                    |<mandi_cess>{ mandi_cess }</mandi_cess>| &&
                    |<association_charges>{ associat_chrg }</association_charges>| &&
                    |<dhami>{ dhami }</dhami>| &&
                    |<market_fee>{ market_fee }</market_fee>| &&
                    |<hrdf>{ hrdf }</hrdf>| &&
                    |<auction_charges>{ auction_chrg }</auction_charges>| &&
                    |<hammali_charges>{ hammali_chrg }</hammali_charges>| &&
                    |<dala_charges>{ dala_chrg }</dala_charges>| &&
                    |<oxner_charges>{ oxner_chrg }</oxner_charges>| &&
                    |<tulai_charges>{ tulai_chrg }</tulai_charges>| &&
                    |<grand_total>{ ls_final-grand_total }</grand_total>| &&
                    |<product_cost>{ ls_final-product_cost }</product_cost>| &&
                    |<grand_total_words>{ ls_final-grand_tot_word }</grand_total_words>| &&
                    |<total_gst_words>{ ls_final-tot_gst_word }</total_gst_words>| &&
                    |<remarks>{ ls_final-remarks }</remarks>| &&
                    |<ItemData>|  ##NO_TEXT.

    DATA : lv_item TYPE string .
    DATA : item_desc TYPE string.
    DATA : srn TYPE c LENGTH 3 .
    CLEAR : lv_item , srn .

    LOOP AT ls_final-gt_item INTO DATA(ls_item).

      CLEAR : item_cgst, item_sgst, item_igst.
      item_cgst = ls_item-cgst_per && '%' && cl_abap_char_utilities=>newline && ls_item-cgst_amt.
      item_sgst = ls_item-sgst_per && '%' && cl_abap_char_utilities=>newline && ls_item-sgst_amt.
      item_igst = ls_item-igst_per && '%' && cl_abap_char_utilities=>newline && ls_item-igst_amt.

      CLEAR item_desc.
      IF ls_final-po_type NE 'ZIMP' AND ls_final-po_type NE 'ZPDY'.
        item_desc = ls_item-item_name && cl_abap_char_utilities=>newline && 'HSN - ' && ls_item-item_hsn.
      ELSE.
        item_desc = ls_item-item_name.
      ENDIF.

      srn = srn + 1 .
      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<sl_num>{ srn }</sl_num>| &&
                |<item_code>{ ls_item-item_code }</item_code>| &&
                |<item_name>{ item_desc }</item_name>| &&
                |<item_del_dt>{ ls_item-item_del_dt }</item_del_dt>| &&
                |<item_hsn>{ ls_item-item_hsn }</item_hsn>| &&
                |<item_brand>{ ls_item-item_brand }</item_brand>| &&
                |<item_no_bag>{ ls_item-item_no_bag }</item_no_bag>| &&
                |<item_unit>{ ls_item-item_unit }</item_unit>| &&
                |<item_qty>{ ls_item-item_qty }</item_qty>| &&
                |<item_rate>{ ls_item-item_rate }</item_rate>| &&
                |<disc_amt>{ ls_item-disc_amt }</disc_amt>| &&
                |<disc_del_dt>{ ls_item-disc_del_dt }</disc_del_dt>| &&
                |<cgst_amt>{ item_cgst }</cgst_amt>| &&
                |<sgst_amt>{ item_sgst }</sgst_amt>| &&
                |<igst_amt>{ item_igst }</igst_amt>| &&
                |<net_value>{ ls_item-net_val }</net_value>| &&
                |</ItemDataNode>|  ##NO_TEXT .

    ENDLOOP.

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                       |</PurchaseOrderNode>| &&
                       |</Form>| ##NO_TEXT .

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.

  ENDMETHOD.


  METHOD prep_xml_pr_print.
    DATA : heading      TYPE c LENGTH 100,
           sub_heading  TYPE c LENGTH 200,
           lv_xml_final TYPE string.

    READ TABLE it_final_pr INTO DATA(ls_final_pr) INDEX 1.
    SHIFT ls_final_pr-pr_no LEFT DELETING LEADING ''.
    heading = 'Purchase Requisition' ##NO_TEXT.

    DATA(lv_xml) =  |<Form>| &&
                    |<PurchaseReqNode>| &&
                    |<header_1>{ heading }</header_1>| &&
                    |<company_name>{ ls_final_pr-comp_name }</company_name>| &&
                    |<plant_name>{ ls_final_pr-plant_name }</plant_name>| &&
                    |<plant_code>{ ls_final_pr-plant }</plant_code>| &&
                    |<plant_adr1>{ ls_final_pr-plant_adr1 }</plant_adr1>| &&
                    |<plant_adr2>{ ls_final_pr-plant_adr2 }</plant_adr2>| &&
                    |<plant_adr3>{ ls_final_pr-plant_adr3 }</plant_adr3>| &&
                    |<plant_phone>{ ls_final_pr-plant_phone }</plant_phone>| &&
                    |<plant_fax>{ ls_final_pr-plant_fax }</plant_fax>| &&
                    |<plant_gstin>{ ls_final_pr-plant_gstin }</plant_gstin>| &&
                    |<plant_cin>{ ls_final_pr-plant_cin }</plant_cin>| &&
                    |<status>{ ls_final_pr-status }</status>| &&
                    |<pr_no>{ ls_final_pr-pr_no }</pr_no>| &&
                    |<pr_date>{ ls_final_pr-pr_date }</pr_date>| &&
                    |<requisitioner>{ ls_final_pr-requisitioner }</requisitioner>| &&
                    |<purchase_group>{ ls_final_pr-pr_group }</purchase_group>| &&
                    |<cost_centre_name>{ ls_final_pr-cost_name }</cost_centre_name>| &&
                    |<cost_centre_no>{ ls_final_pr-cost_no }</cost_centre_no>| &&
                    |<equipment>{ ls_final_pr-equipment }</equipment>| &&
                    |<supplier>{ ls_final_pr-equipment }</supplier>| &&
                    |<supplier_name>{ ls_final_pr-equipment }</supplier_name>| &&
                    |<total_qty>{ ls_final_pr-total_qty }</total_qty>| &&
                    |<total_amt>{ ls_final_pr-total_amt }</total_amt>| &&
                    |<remarks>{ ls_final_pr-remarks }</remarks>| &&
                    |<prepared_by>{ ls_final_pr-remarks }</prepared_by>| &&
                    |<checked_by>{ ls_final_pr-remarks }</checked_by>| &&
                    |<format_no>{ ls_final_pr-format_no }</format_no>| &&
                    |<ItemData>|  ##NO_TEXT.

    DATA : lv_item TYPE string .
    DATA : srn TYPE c LENGTH 3 .
    CLEAR : lv_item , srn .

    LOOP AT ls_final_pr-gt_item_pr INTO DATA(ls_item_pr).

      srn = srn + 1 .
      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<sl_num>{ srn }</sl_num>| &&
                |<item_code>{ ls_item_pr-item_code }</item_code>| &&
                |<item_name>{ ls_item_pr-item_name }</item_name>| &&
                |<item_part>{ ls_item_pr-item_part }</item_part>| &&
                |<item_qty>{ ls_item_pr-item_qty }</item_qty>| &&
                |<item_unit>{ ls_item_pr-item_unit }</item_unit>| &&
                |<item_requi>{ ls_item_pr-item_requi }</item_requi>| &&
                |<item_remk>{ ls_item_pr-item_remk }</item_remk>| &&
                |<item_rate>{ ls_item_pr-item_rate }</item_rate>| &&
                |<unrest_stock>{ ls_item_pr-unrest_stock }</unrest_stock>| &&
                |<net_amt>{ ls_item_pr-net_amt }</net_amt>| &&
                |</ItemDataNode>|  ##NO_TEXT .

    ENDLOOP.

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                       |</PurchaseReqNode>| &&
                       |</Form>| ##NO_TEXT .

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.

  ENDMETHOD.
ENDCLASS.
