CLASS zcl_fi_custom_print DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      gt_dbnote TYPE TABLE OF zstr_fi_debit_note,
      gs_dbnote TYPE zstr_fi_debit_note,
      gt_item   TYPE TABLE OF zstr_fi_debit_note_item,
      xt_item   TYPE TABLE OF zstr_fi_debit_note_item,
      gs_item   TYPE zstr_fi_debit_note_item.

    DATA:
      lv_char10 TYPE c LENGTH 10,
      lv_char4  TYPE c LENGTH 4.

    METHODS:
      get_fidebit_data
        IMPORTING
                  im_bukrs         LIKE lv_char4
                  im_belnr         LIKE lv_char10
                  im_gjahr         TYPE zi_dc_note-fiscalyear
                  im_action        LIKE lv_char10
        RETURNING VALUE(et_dbdata) LIKE gt_dbnote,

      get_payadv_data
        IMPORTING
                  im_bukrs         LIKE lv_char4
                  im_belnr         LIKE lv_char10
                  im_gjahr         TYPE zi_dc_note-fiscalyear
                  im_action        LIKE lv_char10
        RETURNING VALUE(et_payadv) LIKE gt_dbnote,

      get_chqprnt_data
        IMPORTING
                  im_bukrs          LIKE lv_char4
                  im_belnr          LIKE lv_char10
                  im_gjahr          TYPE zi_dc_note-fiscalyear
                  im_action         LIKE lv_char10
        RETURNING VALUE(et_chqprnt) LIKE gt_dbnote,

      prep_xml_fidebit
        IMPORTING
                  it_dbnote            LIKE gt_dbnote
                  im_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string,

      prep_xml_payadv
        IMPORTING
                  it_payadv            LIKE gt_dbnote
                  im_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string,


      prep_xml_chqprnt
        IMPORTING
                  it_chqprnt           LIKE gt_dbnote
                  im_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_FI_CUSTOM_PRINT IMPLEMENTATION.


  METHOD get_chqprnt_data.

    DATA:
      lo_amt_words   TYPE REF TO zcl_amt_words,
      lv_sum_chq_amt TYPE p LENGTH 16 DECIMALS 2.

    DATA:
      lv_amount_neg TYPE c LENGTH 20.

    CREATE OBJECT lo_amt_words.

    IF im_belnr IS NOT INITIAL.

      SELECT
      *
      FROM zi_dc_note
      WHERE
        companycode = @im_bukrs AND accountingdocument = @im_belnr AND fiscalyear = @im_gjahr
        INTO TABLE @DATA(lt_acc).             "#EC CI_ALL_FIELDS_NEEDED

      SELECT * FROM zi_cheque_detail
      WHERE
      paymentcompanycode = @im_bukrs AND paymentdocument = @im_belnr AND fiscalyear = @im_gjahr
      INTO TABLE @DATA(lt_chq).               "#EC CI_ALL_FIELDS_NEEDED

      DATA(xt_acc) = lt_acc[].
      READ TABLE xt_acc INTO DATA(xs_acc) INDEX 1.      "#EC CI_NOORDER

      SELECT SINGLE * FROM zi_supplier_address
      WHERE supplier = @xs_acc-supplier INTO @DATA(ls_supplier). "#EC CI_ALL_FIELDS_NEEDED

      gs_dbnote-companycode          = xs_acc-companycode.
      gs_dbnote-accountingdocument   = xs_acc-accountingdocument.
      gs_dbnote-fiscalyear           = xs_acc-fiscalyear.
      gs_dbnote-postingdate          = xs_acc-postingdate.
      gs_dbnote-documentdate         = xs_acc-documentdate.
      gs_dbnote-acc_payee            = 'A/C Payee' ##NO_TEXT.


      READ TABLE lt_chq INTO DATA(ls_chq) WITH KEY paymentdocument = xs_acc-accountingdocument
                                                   chequestatus    = '10'.

      IF sy-subrc EQ 0.

        gs_dbnote-bank_name         = ls_chq-bankname.
        gs_dbnote-bank_det1         = ls_chq-housebankaccount.
        gs_dbnote-suppl_code        = ls_chq-supplier.  "ls_supplier-Supplier.
        gs_dbnote-suppl_name        = ls_chq-payeename. "ls_supplier-AddresseeFullName.
        gs_dbnote-cheque_no         = ls_chq-outgoingcheque.
        gs_dbnote-cheque_date       = ls_chq-chequepaymentdate.

        lv_amount_neg = ls_chq-paidamountinpaytcurrency. "xs_acc-AmountInCompanyCodeCurrency.
        IF lv_amount_neg CA '-'.
          gs_dbnote-chq_amt   = ls_chq-paidamountinpaytcurrency * -1. "xs_acc-AmountInCompanyCodeCurrency * -1.
        ELSE.
          gs_dbnote-chq_amt   = ls_chq-paidamountinpaytcurrency. "xs_acc-AmountInCompanyCodeCurrency.
        ENDIF.

      ENDIF.

      DATA: lv_grand_tot_word TYPE string.

      IF gs_dbnote-chq_amt IS NOT INITIAL.

        lv_grand_tot_word = gs_dbnote-chq_amt.

        lo_amt_words->number_to_words(
          EXPORTING
            iv_num   = lv_grand_tot_word
          RECEIVING
            rv_words = DATA(amt_words)
        ).

        gs_dbnote-tot_amt_words = amt_words.

      ENDIF.

      APPEND gs_dbnote TO et_chqprnt.

    ENDIF.

  ENDMETHOD.


  METHOD get_fidebit_data.

    DATA:
      lo_amt_words     TYPE REF TO zcl_amt_words,
      lv_total_value   TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_frt_amt   TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_cgst_amt  TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_sgst_amt  TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_igst_amt  TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_tcs_amt   TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_load_amt  TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_rndf_amt  TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_other_amt TYPE p LENGTH 16 DECIMALS 2,
      lv_grand_total   TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_gst_amt   TYPE p LENGTH 16 DECIMALS 2,
      lv_amount_neg    TYPE c LENGTH 20,
      region_desc      TYPE c LENGTH 20.

    CREATE OBJECT lo_amt_words.

    IF im_belnr IS NOT INITIAL.

      SELECT
        *
        FROM zi_dc_note
        WHERE companycode = @im_bukrs AND accountingdocument = @im_belnr AND fiscalyear = @im_gjahr
        INTO TABLE @DATA(lt_acc).

*      SELECT
*      CompanyCode,
*      FiscalYear,
*      AccountingDocument,
*      AccountingDocumentHeaderText
*      FROM I_AccountingDocument
*      WHERE companycode = @im_bukrs AND accountingdocument = @im_belnr AND fiscalyear = @im_gjahr
*      INTO TABLE @DATA(lt_bkpf).

      IF ( im_action = 'fidebit' OR im_action = 'fircm' ).

        SELECT
          *
          FROM zi_dc_note
          WHERE companycode = @im_bukrs AND accountingdocument = @im_belnr
             AND fiscalyear = @im_gjahr AND transactiontypedetermination IN ( 'WRX', 'BSX', 'EGK', 'PRD' )
             INTO TABLE @DATA(lt_wrx_bsx).

      ELSEIF ( im_action = 'ficredit' OR im_action = 'fitaxinv' ).

        SELECT
          *
          FROM zi_dc_note
          WHERE companycode = @im_bukrs AND accountingdocument = @im_belnr
             AND fiscalyear = @im_gjahr AND transactiontypedetermination EQ ''
             INTO TABLE @lt_wrx_bsx.

      ENDIF.








      """*******Start: IRN Data processing******************************************************
      IF ( im_action = 'ficredit' OR im_action = 'fitaxinv' ).

        DATA: lo_irn  TYPE REF TO zcl_get_irn_detail,
              lo_eway TYPE REF TO zcl_get_eway_detail.

        TYPES: BEGIN OF lty_irn_data,
                 ackno         TYPE string,
                 ackdt         TYPE string,
                 irn           TYPE string,
                 signedinvoice TYPE string,
                 signedqrcode  TYPE string,
                 status        TYPE string,
                 ewbno         TYPE string,
                 ewbdt         TYPE string,
                 ewbvalidtill  TYPE string,
                 remarks       TYPE string,
               END OF lty_irn_data.

*    TYPES: BEGIN OF lty_eway_data,
*             ewayBillNo   TYPE string,
*             ewayBillDate TYPE string,
*             validUpto    TYPE string,
*             alert        TYPE string,
*           END OF lty_eway_data.

        DATA:
          lt_irn      TYPE TABLE OF lty_irn_data,
          ls_irn      TYPE lty_irn_data,
          gt_irn      TYPE TABLE OF zsd_einvoice,
          gs_irn      TYPE zsd_einvoice,
*      gt_eway     TYPE TABLE OF zsd_eway_data,
*      gs_eway     TYPE zsd_eway_data,
*      lt_eway     TYPE TABLE OF lty_eway_data,
*      ls_eway     TYPE lty_eway_data,
          lv_doc_num  TYPE string,
          lv_doc_typ  TYPE string,
          lv_doc_date TYPE string,
          lv_sysid    TYPE zsd_sysid-sysid.

        CREATE OBJECT lo_irn.
        CREATE OBJECT lo_eway.

        READ TABLE lt_wrx_bsx INTO DATA(xs_final) INDEX 1. "#EC CI_NOORDER

        SELECT SINGLE * FROM zsd_sysid
                        WHERE objcode = 'IRN' AND sysid = @sy-sysid
                        INTO @DATA(ls_sysid).
        IF sy-subrc EQ 0.
          lv_sysid = ls_sysid-sysid.
        ENDIF.

        SELECT SINGLE billingdocument,
                      irn,
                      ewbno
                      FROM zsd_einvoice WHERE billingdocument = @xs_final-accountingdocument
                      INTO @DATA(w_einvvoicex).             "#EC WARNOK

        IF sy-subrc NE 0 AND sy-sysid = lv_sysid.

          "upload_sample_data(  ).
          DATA(lv_auth_token) = lo_irn->get_excelon_auth_token(  ).
          DATA(lv_app_key)    = lo_irn->get_excelon_app_key( im_access_token = lv_auth_token  ).
          DATA(lv_encypt_login_data) = lo_irn->encrypt_logon_detail( im_auth_token = lv_auth_token im_app_key = lv_app_key ).
          DATA(lv_irp_data) = lo_irn->get_irp_token( im_auth_token = lv_auth_token im_encrypt_login = lv_encypt_login_data ).

          lv_doc_num  = xs_final-documentreferenceid.
          IF xs_final-documentreferenceid+0(2) = 'DR'.
            lv_doc_typ  = 'INV'.
          ELSEIF xs_final-documentreferenceid+0(2) = 'DG'.
            lv_doc_typ  = 'CRN'.
          ENDIF.
          lv_doc_date = xs_final-postingdate+6(2) && '/' && xs_final-postingdate+4(2) && '/' && xs_final-postingdate+0(4).

          DATA(lv_encrypt_irn_data) = lo_irn->get_encrypty_irn_detail(
                                      im_auth_token = lv_auth_token
                                      im_irp_data   = lv_irp_data
                                      im_doc_num    = lv_doc_num
                                      im_doc_typ    = lv_doc_typ
                                      im_doc_date   = lv_doc_date
                                       ).

          DATA(lv_irn_data) = lo_irn->get_decrypted_doc(
                                im_auth_token  = lv_auth_token
                                im_irp_data    = lv_irp_data
                                im_irn_encrypt = lv_encrypt_irn_data
                                im_app_key     = lv_app_key
                              ).

          /ui2/cl_json=>deserialize(
                          EXPORTING json = lv_irn_data
                             pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                             CHANGING data = lt_irn
                       ).

          DATA: lv_irn     TYPE string,
                lv_ackdate TYPE c LENGTH 10.

          IF lt_irn[] IS NOT INITIAL.

            READ TABLE lt_irn INTO ls_irn INDEX 1.

            IF ls_irn-ackno IS NOT INITIAL.

              MOVE-CORRESPONDING ls_irn TO gs_irn.
              lv_ackdate = ls_irn-ackdt+0(10).

              gs_irn-billingdocument = xs_final-accountingdocument.
              gs_irn-plant_gstin     = '06AAACI2419N1ZK'.
              gs_irn-companycode     = xs_final-companycode.
              gs_irn-fiscalyear      = xs_final-fiscalyear.
              gs_irn-erdat     = cl_abap_context_info=>get_system_date( ). "sy-datum.
              gs_irn-uname     = cl_abap_context_info=>get_user_technical_name( ). "sy-uname.
              gs_irn-ackdt     = lv_ackdate+0(4) && lv_ackdate+5(2) && lv_ackdate+8(2)."cl_abap_context_info=>get_system_date( ). "sy-datum.
              gs_irn-uzeit     = cl_abap_context_info=>get_system_time( ). "sy-uzeit.

              APPEND gs_irn TO gt_irn.

            ENDIF.

            IF gt_irn[] IS NOT INITIAL.
              MODIFY zsd_einvoice FROM TABLE @gt_irn.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.
      """*******End: IRN Data processing******************************************************










      DATA(xt_acc) = lt_acc[].
      IF im_action = 'ficredit'.
        DELETE xt_acc WHERE financialaccounttype NE 'D'.
      ENDIF.
      SORT xt_acc BY companycode accountingdocument fiscalyear.
      DELETE ADJACENT DUPLICATES FROM xt_acc COMPARING companycode accountingdocument fiscalyear.

      LOOP AT xt_acc INTO DATA(xs_acc).

        """******Header Data
        gs_dbnote-companycode          = xs_acc-companycode.
        gs_dbnote-accountingdocument   = xs_acc-accountingdocument.
        gs_dbnote-fiscalyear           = xs_acc-fiscalyear.
        gs_dbnote-postingdate          = xs_acc-postingdate.
        gs_dbnote-documentdate         = xs_acc-documentdate.


        DATA(lt_acc_plant) = lt_acc[].
        DELETE lt_acc_plant WHERE plant EQ ''.
        IF lt_acc_plant[] IS INITIAL.
          lt_acc_plant[] = lt_acc[].
          DELETE lt_acc_plant WHERE businessplace EQ ''.
        ENDIF.

        READ TABLE lt_acc_plant INTO DATA(ls_acc_plant) INDEX 1. "#EC CI_NOORDER
        IF sy-subrc EQ 0.

          SELECT SINGLE * FROM zi_plant_address
          WHERE plant = @ls_acc_plant-businessplace INTO @DATA(ls_plant_adrs). "#EC CI_NOORDER

          gs_dbnote-suppl_code         = ls_acc_plant-businessplace. "ls_acc_plant-Plant.
          gs_dbnote-suppl_name         = ls_plant_adrs-addresseefullname. "ls_plant_adrs-PlantName.

*          IF ls_plant_adrs-StreetPrefixName2 IS NOT INITIAL.
*            gs_dbnote-suppl_addr1        = ls_plant_adrs-StreetPrefixName1 && ',' && ls_plant_adrs-StreetPrefixName2.
*          ELSE.
*            gs_dbnote-suppl_addr1        = ls_plant_adrs-StreetPrefixName1.
*          ENDIF.

          region_desc = ls_plant_adrs-regionname.
          gs_dbnote-suppl_addr1 = ls_plant_adrs-streetname &&  ',' && ls_plant_adrs-streetsuffixname1.
          gs_dbnote-suppl_addr2 = ls_plant_adrs-cityname &&  ',' &&  region_desc && ',' && ls_plant_adrs-postalcode ."&&  ',' && ls_plant_adrs-DistrictName.

          IF ls_plant_adrs-region EQ 'HR'.
            region_desc = 'Haryana' ##NO_TEXT.
          ENDIF.

          gs_dbnote-suppl_addr3        = ''.

          IF xs_acc-companycode = '1000'.

            gs_dbnote-suppl_addr1 = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
            gs_dbnote-suppl_addr2 = 'Works : Point Mana Singh wala , Moga Road, Firozepur' ##NO_TEXT.
            gs_dbnote-suppl_cin          = 'U45100PB2007PTC031212' ##NO_TEXT.
            gs_dbnote-suppl_gstin        = '03AADCB4295Q1ZA' ##NO_TEXT. "for plant 1001
            gs_dbnote-suppl_pan          = gs_dbnote-suppl_gstin+0(10).

          ELSEIF xs_acc-companycode = '2000'.

            gs_dbnote-suppl_cin          = 'U15203PB2009PTC032538' ##NO_TEXT.
            gs_dbnote-suppl_gstin        = '03AADCB6737H1ZU' ##NO_TEXT. "for plant 1001
            gs_dbnote-suppl_pan          = gs_dbnote-suppl_gstin+0(10).

          ELSEIF xs_acc-companycode = '3000'.

            gs_dbnote-suppl_cin          = 'U01100GJ2020PTC113206' ##NO_TEXT.
            gs_dbnote-suppl_gstin        = '24AAFCH2454K1ZL' ##NO_TEXT. "for plant 1001
            gs_dbnote-suppl_pan          = gs_dbnote-suppl_gstin+0(10).

          ELSEIF xs_acc-companycode = '4000'.

            gs_dbnote-suppl_cin          = 'U63090PB2021PTC053299' ##NO_TEXT.
            gs_dbnote-suppl_gstin        = '23AAJCB8215E1ZZ' ##NO_TEXT. "for plant 1001
            gs_dbnote-suppl_pan          = gs_dbnote-suppl_gstin+0(10).

          ENDIF.


          SELECT SINGLE * FROM zi_regiontext WHERE region = @ls_plant_adrs-region AND language = 'E' AND country = @ls_plant_adrs-country
           INTO @DATA(lv_st_nm).              "#EC CI_ALL_FIELDS_NEEDED

          gs_dbnote-suppl_stat_code    = lv_st_nm-regionname. "ls_plant_adrs-Region.

          IF ( im_action = 'fidebit' OR im_action = 'fircm' ).

            gs_dbnote-suppl_phone        = '+91-7009234328' ##NO_TEXT.
            gs_dbnote-suppl_email        = 'Purchase@blvexports.com' ##NO_TEXT.

          ELSEIF ( im_action = 'ficredit' OR im_action = 'fitaxinv' ).

            gs_dbnote-suppl_phone        = '+91-7009234322' ##NO_TEXT.
            gs_dbnote-suppl_email        = 'Sales@blvexports.com' ##NO_TEXT.

          ENDIF.

        ENDIF.

        IF ( im_action = 'fidebit' OR im_action = 'fircm' ).

          READ TABLE lt_acc INTO DATA(ls_acc_kbs) WITH KEY transactiontypedetermination = 'KBS'. " and K
          SELECT SINGLE * FROM zi_supplier_address
          WHERE supplier = @ls_acc_kbs-supplier INTO @DATA(ls_supplier).

          IF ls_acc_kbs IS INITIAL.
            READ TABLE lt_acc INTO ls_acc_kbs WITH KEY transactiontypedetermination = 'EGK'. " and K
            SELECT SINGLE * FROM zi_supplier_address
            WHERE supplier = @ls_acc_kbs-supplier INTO @ls_supplier.
          ENDIF.

        ELSEIF ( im_action = 'ficredit' OR im_action = 'fitaxinv' ).

          CLEAR: ls_acc_kbs.
          READ TABLE lt_acc INTO ls_acc_kbs WITH KEY transactiontypedetermination = 'AGD'.
          SELECT SINGLE * FROM zi_customer_address
          WHERE customer = @ls_acc_kbs-customer INTO @DATA(ls_customer).

          ls_supplier = CORRESPONDING #( ls_customer ).

        ENDIF.

        IF sy-subrc EQ 0.

          SELECT SINGLE * FROM zi_countrytext   WHERE country = @ls_supplier-country AND language = 'E'
          INTO @DATA(lv_cn_name_we).          "#EC CI_ALL_FIELDS_NEEDED

          SELECT SINGLE * FROM zi_regiontext  WHERE region = @ls_supplier-regio AND language = 'E' AND country = @ls_supplier-country
          INTO @DATA(lv_st_name_we).          "#EC CI_ALL_FIELDS_NEEDED

          gs_dbnote-billto_code        = xs_acc-supplier.
          gs_dbnote-billto_name        = ls_supplier-addresseefullname.
          gs_dbnote-billto_addr1       = ls_supplier-streetprefixname1 && ',' && ls_supplier-streetprefixname2.
          gs_dbnote-billto_addr2       = ls_supplier-streetname &&  ',' && ls_supplier-streetsuffixname1 &&  ',' && ls_supplier-districtname.
          gs_dbnote-billto_addr3       = ls_supplier-cityname &&  ',' && ls_supplier-postalcode &&  ',' && lv_cn_name_we-countryname.
          gs_dbnote-billto_cin         = ''.
          gs_dbnote-billto_gstin       = ls_supplier-taxnumber3.
          gs_dbnote-billto_pan         = ''.
          gs_dbnote-billto_stat_code   = lv_st_name_we-regionname. "ls_supplier-regio.
          gs_dbnote-billto_phone       = ls_supplier-phonenumber1.
          gs_dbnote-billto_email       = ''.

          gs_dbnote-shipto_code        = xs_acc-supplier.
          gs_dbnote-shipto_name        = ls_supplier-addresseefullname.
          gs_dbnote-shipto_addr1       = ls_supplier-streetprefixname1 && ',' && ls_supplier-streetprefixname2.
          gs_dbnote-shipto_addr2       = ls_supplier-streetname &&  ',' && ls_supplier-streetsuffixname1 &&  ',' && ls_supplier-districtname.
          gs_dbnote-shipto_addr3       = ls_supplier-cityname &&  ',' && ls_supplier-postalcode &&  ',' && lv_cn_name_we-countryname.
          gs_dbnote-shipto_cin         = ''.
          gs_dbnote-shipto_gstin       = ls_supplier-taxnumber3.
          gs_dbnote-shipto_pan         = ls_supplier-taxnumber3+2(10).
          gs_dbnote-shipto_stat_code   = lv_st_name_we-regionname. "ls_supplier-regio.
          gs_dbnote-shipto_phone       = ls_supplier-phonenumber1.
          gs_dbnote-shipto_email       = ''.
          gs_dbnote-shipto_place_supply  = ls_supplier-region.
          CONDENSE gs_dbnote-shipto_place_supply.

          CLEAR: lv_amount_neg.
          lv_amount_neg = xs_acc-amountincompanycodecurrency.
          CONDENSE lv_amount_neg.
          IF lv_amount_neg CA '-'.
            lv_grand_total = xs_acc-amountincompanycodecurrency * -1.
          ELSE.
            lv_grand_total = xs_acc-amountincompanycodecurrency.
          ENDIF.

        ENDIF.

        gs_dbnote-veh_no               = ''.
        gs_dbnote-trnas_mode           = ''.
        gs_dbnote-inv_no               = xs_acc-accountingdocument.
        gs_dbnote-inv_date             = xs_acc-postingdate+6(2) && '.' && xs_acc-postingdate+4(2) && '.' && xs_acc-postingdate+0(4).
        gs_dbnote-inv_ref_no           = xs_acc-documentreferenceid. "InvoiceReference.
        gs_dbnote-tax_payable_rev      = ''.
        gs_dbnote-remark               = xs_acc-documentitemtext.
        gs_dbnote-trans_curr           = xs_acc-transactioncurrency.

        IF im_action = 'fircm'.
          gs_dbnote-inv_ref_no           = xs_acc-alternativereferencedocument.
          gs_dbnote-inv_no               = xs_acc-documentreferenceid.
          gs_dbnote-inv_date             = xs_acc-documentdate+6(2) && '.' && xs_acc-documentdate+4(2) && '.' && xs_acc-documentdate+0(4).
          READ TABLE lt_acc INTO DATA(xs_remark) WITH KEY financialaccounttype = 'K'.
          gs_dbnote-remark               = xs_remark-documentitemtext.
        ENDIF.

        IF im_action = 'fircm'.
          gs_dbnote-inv_date             = xs_acc-documentdate+6(2) && '.' && xs_acc-documentdate+4(2) && '.' && xs_acc-documentdate+0(4).
          gs_dbnote-inv_ref_no           = xs_acc-alternativereferencedocument. "xs_acc-accountingdocument. "
          gs_dbnote-inv_no               = xs_acc-documentreferenceid.
          gs_dbnote-inv_ref_date         = xs_acc-postingdate+6(2) && '.' && xs_acc-postingdate+4(2) && '.' && xs_acc-postingdate+0(4).
          READ TABLE lt_acc INTO xs_remark WITH KEY financialaccounttype = 'K'.
          gs_dbnote-remark               = xs_remark-documentitemtext.
        ENDIF.

        IF im_action = 'fidebit'.

          CLEAR: xs_remark .
          READ TABLE lt_acc INTO xs_remark WITH KEY financialaccounttype = 'K'.
          gs_dbnote-remark               = xs_remark-documentitemtext.

          SELECT SINGLE documentdate FROM zi_dc_note
                 WHERE accountingdocument = @xs_remark-invoicereference AND
                       fiscalyear         = @xs_remark-invoicereferencefiscalyear
                       INTO @DATA(lv_doc_datn).

          gs_dbnote-inv_no               = xs_acc-accountingdocument.
          gs_dbnote-ref_doc_no           = xs_remark-invoicereference.
          gs_dbnote-inv_ref_date         = lv_doc_datn+6(2) && '.' && lv_doc_datn+4(2) && '.' && lv_doc_datn+0(4).

        ENDIF.

        IF im_action = 'ficredit'.

          CLEAR: xs_remark .
          READ TABLE lt_acc INTO xs_remark WITH KEY financialaccounttype = 'D'.
          gs_dbnote-remark               = xs_remark-documentitemtext.
          gs_dbnote-inv_no               = xs_acc-documentreferenceid.

          SELECT SINGLE documentreferenceid, postingdate FROM zi_dc_note
                 WHERE accountingdocument = @xs_acc-invoicereference AND
                       fiscalyear         = @xs_acc-invoicereferencefiscalyear
                       INTO @DATA(ls_doc_ref).

          gs_dbnote-inv_ref_no           = ls_doc_ref-documentreferenceid. "xs_acc-InvoiceReference.
          gs_dbnote-inv_ref_date         = ls_doc_ref-postingdate+6(2) && '.' && ls_doc_ref-postingdate+4(2) && '.' && ls_doc_ref-postingdate+0(4). "xs_acc-DocumentDate+6(2) && '.' && xs_acc-DocumentDate+4(2) && '.' && xs_acc-DocumentDate+0(4).

          IF gs_dbnote-accountingdocumenttype = 'DR'.
            gs_dbnote-inv_no               = ''.
            gs_dbnote-inv_ref_no           = xs_acc-documentreferenceid.
            gs_dbnote-inv_ref_date         = gs_dbnote-inv_date.
            gs_dbnote-inv_date             = ''.
          ENDIF.

        ENDIF.

        IF im_action = 'fitaxinv'.
          CLEAR: xs_remark .
          READ TABLE lt_acc INTO xs_remark WITH KEY financialaccounttype = 'D'.
          gs_dbnote-remark               = xs_remark-documentitemtext.
          gs_dbnote-inv_no               = xs_acc-documentreferenceid.
          gs_dbnote-inv_date             = xs_acc-documentdate+6(2) && '.' && xs_acc-documentdate+4(2) && '.' && xs_acc-documentdate+0(4).
        ENDIF.

        """******Item Data
        CLEAR: gs_item.
        LOOP AT lt_acc INTO DATA(ls_acc)
                       WHERE companycode  = xs_acc-companycode AND
                       accountingdocument = xs_acc-accountingdocument AND
                       fiscalyear = xs_acc-fiscalyear AND
                       ( transactiontypedetermination = 'JIC' OR transactiontypedetermination = 'JIS' OR
                         transactiontypedetermination = 'JII' OR transactiontypedetermination = 'FR1' OR
                         transactiontypedetermination = 'RND' OR transactiontypedetermination = 'LOD' OR
                         transactiontypedetermination = 'OTH' OR transactiontypedetermination = 'TCS' OR
                         transactiontypedetermination = 'JOC' OR transactiontypedetermination = 'JOS' OR
                         transactiontypedetermination = 'JOI'
                       ).


          gs_item-companycode            = ls_acc-companycode.
          gs_item-accountingdocument     = ls_acc-accountingdocument.
          gs_item-fiscalyear             = ls_acc-fiscalyear.
          gs_item-accountingdocumentitem = ls_acc-accountingdocumentitem.
          gs_item-taxitemgroup           = ls_acc-taxitemgroup.

          CLEAR: lv_amount_neg.
          lv_amount_neg = ls_acc-amountincompanycodecurrency.
          CONDENSE lv_amount_neg.
          IF lv_amount_neg CA '-'.
            ls_acc-amountincompanycodecurrency = ls_acc-amountincompanycodecurrency * -1.
          ENDIF.

          IF ( ls_acc-transactiontypedetermination = 'JIC' OR ls_acc-transactiontypedetermination = 'JOC' ).

            gs_item-cgst_amt     = ls_acc-amountincompanycodecurrency.
            lv_sum_cgst_amt      = lv_sum_cgst_amt  + ls_acc-amountincompanycodecurrency.
            gs_item-cgst_rate    = ''.

          ELSEIF ( ls_acc-transactiontypedetermination = 'JIS' OR ls_acc-transactiontypedetermination = 'JOS' ).

            gs_item-sgst_amt            = ls_acc-amountincompanycodecurrency.
            lv_sum_sgst_amt  = lv_sum_sgst_amt + ls_acc-amountincompanycodecurrency.
            gs_item-sgst_rate           = ''.

          ELSEIF ( ls_acc-transactiontypedetermination = 'JII' OR ls_acc-transactiontypedetermination = 'JOI' ).

            gs_item-igst_amt     = ls_acc-amountincompanycodecurrency.
            lv_sum_igst_amt  = lv_sum_igst_amt + ls_acc-amountincompanycodecurrency.
            gs_item-igst_rate           = ''.

          ELSEIF ls_acc-transactiontypedetermination = 'FR1'.

            gs_item-frt_amt             = ls_acc-amountincompanycodecurrency.
            lv_sum_frt_amt  = lv_sum_frt_amt + ls_acc-amountincompanycodecurrency.

          ELSEIF ls_acc-transactiontypedetermination = 'RND'.

            gs_item-rndf_amt            = ls_acc-amountincompanycodecurrency.
            lv_sum_rndf_amt  = lv_sum_rndf_amt + ls_acc-amountincompanycodecurrency.

          ELSEIF ls_acc-transactiontypedetermination = 'LOD'.

            gs_item-load_amt            = ls_acc-amountincompanycodecurrency.
            lv_sum_load_amt  = lv_sum_load_amt + ls_acc-amountincompanycodecurrency.

          ELSEIF ls_acc-transactiontypedetermination = 'OTH'.

            gs_item-othr_amt            = ls_acc-amountincompanycodecurrency.
            lv_sum_other_amt  = lv_sum_other_amt + ls_acc-amountincompanycodecurrency.

          ELSEIF ls_acc-transactiontypedetermination = 'TCS'.

            gs_item-tcs_amt            = ls_acc-amountincompanycodecurrency.
            lv_sum_tcs_amt  = lv_sum_tcs_amt + ls_acc-amountincompanycodecurrency.

          ENDIF.
          APPEND gs_item TO xt_item.


*          IF ls_acc-TransactionTypeDetermination = 'BSX' OR ls_acc-TransactionTypeDetermination = 'WRX'.
*
*            SELECT SINGLE * FROM I_ProductDescription
*                            WHERE Product = @ls_acc-Material AND Language = 'E'
*                            INTO @DATA(ls_maktx).
*
*            SELECT SINGLE * FROM I_GLAccountText
*                            WHERE GLAccount = @ls_acc-GLAccount AND Language = 'E'
*                            INTO @DATA(ls_gltext).
*
*            SELECT SINGLE
*            Product,
*            plant,
*            ConsumptionTaxCtrlCode
*            FROM I_ProductPlantBasic
*            WHERE Product = @ls_acc-Material AND plant = @ls_acc-Plant
*            INTO @DATA(ls_hsn).
*
*            gs_item-itemcode            = ls_acc-Material.
*            gs_item-itemdesc            = ls_maktx-ProductDescription. "ls_gltext-GLAccountName.
*            gs_item-hsncode             = ls_hsn-ConsumptionTaxCtrlCode.
*            gs_item-uom                 = ls_acc-BaseUnit.
*            gs_item-itmqty              = ls_acc-Quantity.
*            gs_item-amount              = ls_acc-AmountInCompanyCodeCurrency * -1.
*            gs_item-discount            = ''.
*            gs_item-taxable_amt         = ls_acc-AmountInCompanyCodeCurrency * -1.
*            lv_total_value              = lv_total_value + gs_item-taxable_amt.
*            gs_item-unit_rate           = gs_item-amount / gs_item-itmqty.
*
*            IF gs_item-cgst_amt IS NOT INITIAL.
*              gs_item-cgst_rate    = ( gs_item-cgst_amt * 100 ) / gs_item-taxable_amt.
*            ENDIF.
*
*            IF gs_item-sgst_amt IS NOT INITIAL.
*              gs_item-sgst_rate    = ( gs_item-igst_amt * 100 ) / gs_item-taxable_amt.
*            ENDIF.
*
*            IF gs_item-igst_amt IS NOT INITIAL.
*              gs_item-igst_rate    = ( gs_item-igst_amt * 100 ) / gs_item-taxable_amt.
*            ENDIF.
*
*            APPEND gs_item TO gt_item.
*
*          ENDIF.

          CLEAR: ls_acc, gs_item.
        ENDLOOP.

        gs_dbnote-sum_cgst_amt1         = lv_sum_cgst_amt.
        gs_dbnote-sum_sgst_amt1         = lv_sum_sgst_amt.
        gs_dbnote-sum_igst_amt1         = lv_sum_igst_amt.
        lv_sum_gst_amt = lv_sum_cgst_amt + lv_sum_sgst_amt + lv_sum_igst_amt.

        CLEAR: ls_acc, gs_item.
        CLEAR: lv_sum_cgst_amt, lv_sum_sgst_amt, lv_sum_igst_amt.
        DATA(lt_bsx) = lt_wrx_bsx[].
        DATA(lt_prd) = lt_wrx_bsx[].
        DELETE lt_bsx WHERE transactiontypedetermination NE 'BSX'.
        DELETE lt_prd WHERE transactiontypedetermination NE 'PRD'.

        IF lt_wrx_bsx[] IS NOT INITIAL.
          DATA(lv_wrx_bsx_line) = lines( lt_wrx_bsx ).
          IF lv_wrx_bsx_line GT 1.

            READ TABLE lt_wrx_bsx INTO DATA(lvs_egk) WITH KEY transactiontypedetermination = 'EGK'.
            IF sy-subrc NE 0.

              READ TABLE lt_wrx_bsx INTO DATA(cs_wrx) WITH KEY transactiontypedetermination = 'WRX'.
              IF sy-subrc EQ 0.
                DELETE lt_wrx_bsx WHERE transactiontypedetermination NE 'WRX'.
              ELSE.
                DELETE lt_wrx_bsx WHERE transactiontypedetermination NE 'BSX'.
              ENDIF.

            ENDIF.

          ENDIF.
        ENDIF.

        LOOP AT lt_wrx_bsx INTO ls_acc
                       WHERE companycode  = xs_acc-companycode AND
                       accountingdocument = xs_acc-accountingdocument AND
                       fiscalyear = xs_acc-fiscalyear. "AND
          "TransactionTypeDetermination ne 'BSX'.

*          gs_dbnote-remark = ls_acc-DocumentItemText.

          READ TABLE lt_bsx INTO DATA(ls_bsx) INDEX 1.  "#EC CI_NOORDER
          READ TABLE lt_prd INTO DATA(ls_prd) INDEX 1.  "#EC CI_NOORDER

          IF ls_acc-transactiontypedetermination = 'EGK'.

            READ TABLE lt_acc INTO DATA(cs_acc) WITH KEY companycode  = xs_acc-companycode
                                                   accountingdocument = xs_acc-accountingdocument
                                                           fiscalyear = xs_acc-fiscalyear
                                                           transactiontypedetermination = ''.

            ls_acc-product = cs_acc-product.
            ls_acc-baseunit = cs_acc-baseunit.
            ls_acc-quantity = cs_acc-quantity.
            ls_acc-taxitemgroup = cs_acc-taxitemgroup.
            ls_acc-plant        = cs_acc-plant.
            ls_acc-amountincompanycodecurrency = cs_acc-amountincompanycodecurrency .
            ls_acc-in_hsnorsaccode = cs_acc-in_hsnorsaccode.
*            gs_dbnote-remark = cs_acc-documentitemtext.

          ENDIF.

          gs_item-companycode            = ls_acc-companycode.
          gs_item-accountingdocument     = ls_acc-accountingdocument.
          gs_item-fiscalyear             = ls_acc-fiscalyear.
          gs_item-accountingdocumentitem = ls_acc-accountingdocumentitem.
          gs_item-trans_curr             = ls_acc-transactioncurrency.

          SELECT SINGLE * FROM i_productdescription
                          WHERE product = @ls_acc-product AND language = 'E'
                          INTO @DATA(ls_maktx). "#EC CI_ALL_FIELDS_NEEDED

          SELECT SINGLE * FROM i_glaccounttext
                          WHERE glaccount = @ls_acc-glaccount AND language = 'E'
                          INTO @DATA(ls_gltext). "#EC CI_ALL_FIELDS_NEEDED

          SELECT SINGLE
          product,
          plant,
          consumptiontaxctrlcode
          FROM i_productplantbasic
          WHERE product = @ls_acc-product AND plant = @ls_acc-plant
          INTO @DATA(ls_hsn).

          CLEAR: lv_amount_neg.
          lv_amount_neg = ls_acc-amountincompanycodecurrency.
          CONDENSE lv_amount_neg.
          IF lv_amount_neg CA '-'.
            ls_acc-amountincompanycodecurrency = ls_acc-amountincompanycodecurrency * -1.
          ENDIF.

          CLEAR: lv_amount_neg.
          lv_amount_neg = ls_bsx-amountincompanycodecurrency.
          CONDENSE lv_amount_neg.
          IF lv_amount_neg CA '-'.
            ls_bsx-amountincompanycodecurrency = ls_bsx-amountincompanycodecurrency * -1.
          ENDIF.

          CLEAR: lv_amount_neg.
          lv_amount_neg = ls_prd-amountincompanycodecurrency.
          CONDENSE lv_amount_neg.
          IF lv_amount_neg CA '-'.
            ls_prd-amountincompanycodecurrency = ls_prd-amountincompanycodecurrency * -1.
          ENDIF.

          gs_item-itemcode            = ls_acc-product.
          gs_item-itemdesc            = ls_maktx-productdescription. "ls_gltext-GLAccountName.
          gs_item-hsncode             = ls_acc-in_hsnorsaccode. "ls_hsn-ConsumptionTaxCtrlCode.
          gs_item-uom                 = ls_acc-baseunit.
          gs_item-itmqty              = ls_acc-quantity.

          IF ls_acc-transactiontypedetermination NE 'BSX'.
            gs_item-amount              = ls_acc-amountincompanycodecurrency + ls_bsx-amountincompanycodecurrency + ls_prd-amountincompanycodecurrency.
            gs_item-taxable_amt         = ls_acc-amountincompanycodecurrency + ls_bsx-amountincompanycodecurrency + ls_prd-amountincompanycodecurrency.
          ELSE.
            gs_item-amount              = ls_acc-amountincompanycodecurrency + ls_prd-amountincompanycodecurrency.
            gs_item-taxable_amt         = ls_acc-amountincompanycodecurrency + ls_prd-amountincompanycodecurrency.
          ENDIF.

          gs_item-discount            = ''.


          lv_total_value              = lv_total_value + gs_item-taxable_amt.
          IF gs_item-itmqty IS NOT INITIAL.
            gs_item-unit_rate           = gs_item-amount / gs_item-itmqty.
          ENDIF.

          LOOP AT xt_item INTO DATA(xs_item) WHERE taxitemgroup = ls_acc-taxitemgroup.

            IF xs_item-cgst_amt IS NOT INITIAL.
              gs_item-cgst_amt   = xs_item-cgst_amt.
              lv_sum_cgst_amt    = lv_sum_cgst_amt + gs_item-cgst_amt.
            ENDIF.

            IF xs_item-sgst_amt IS NOT INITIAL.
              gs_item-sgst_amt   = xs_item-sgst_amt.
              lv_sum_sgst_amt    = lv_sum_sgst_amt + gs_item-sgst_amt.
            ENDIF.

            IF xs_item-igst_amt IS NOT INITIAL.
              gs_item-igst_amt   = xs_item-igst_amt.
              lv_sum_igst_amt    = lv_sum_igst_amt + gs_item-igst_amt.
            ENDIF.

            CLEAR: xs_item.
          ENDLOOP.

          IF gs_item-cgst_amt IS NOT INITIAL.
            gs_item-cgst_rate    = ( gs_item-cgst_amt * 100 ) / gs_item-taxable_amt.
          ENDIF.

          IF gs_item-sgst_amt IS NOT INITIAL.
            gs_item-sgst_rate    = ( gs_item-sgst_amt * 100 ) / gs_item-taxable_amt.
          ENDIF.

          IF gs_item-igst_amt IS NOT INITIAL.
            gs_item-igst_rate    = ( gs_item-igst_amt * 100 ) / gs_item-taxable_amt.
          ENDIF.

          IF ls_acc-taxcode = 'C1' OR
             ls_acc-taxcode = 'C2' OR
             ls_acc-taxcode = 'C3' OR
             ls_acc-taxcode = 'C4' OR
             ls_acc-taxcode = 'C5' OR
             ls_acc-taxcode = 'C6' OR
             ls_acc-taxcode = 'C7' OR
             ls_acc-taxcode = 'C8' OR
             ls_acc-taxcode = 'D1' OR
             ls_acc-taxcode = 'D2' OR
             ls_acc-taxcode = 'D3' OR
             ls_acc-taxcode = 'D4' OR
             ls_acc-taxcode = 'D5' OR
             ls_acc-taxcode = 'D6' OR
             ls_acc-taxcode = 'D7' OR
             ls_acc-taxcode = 'D8'.

            gs_dbnote-revrs_yes_no = 'Yes'.

          ELSE.

            gs_dbnote-revrs_yes_no = 'No'.

          ENDIF.

          APPEND gs_item TO gt_item.
          CLEAR: ls_acc, gs_item.
        ENDLOOP.

        lv_grand_total = lv_total_value +
                         lv_sum_frt_amt +
                         lv_sum_cgst_amt +
                         lv_sum_sgst_amt +
                         lv_sum_igst_amt +
                         lv_sum_load_amt +
                         lv_sum_rndf_amt.

        gs_dbnote-sum_frt_amt          = lv_sum_frt_amt.
        gs_dbnote-sum_other_amt        = lv_sum_frt_amt. "lv_sum_other_amt.
        gs_dbnote-sum_cgst_amt         = lv_sum_cgst_amt.
        gs_dbnote-sum_sgst_amt         = lv_sum_sgst_amt.
        gs_dbnote-sum_igst_amt         = lv_sum_igst_amt.
        gs_dbnote-sum_tcs_amt          = lv_sum_tcs_amt.
        gs_dbnote-sum_load_amt         = lv_sum_load_amt.
        gs_dbnote-sum_rndf_amt         = lv_sum_rndf_amt.
        gs_dbnote-sum_gst_amt          = lv_sum_gst_amt.
        gs_dbnote-total_value          = lv_total_value.
        gs_dbnote-grand_total          = lv_grand_total.

*      ""****Start:GST %***************
*      IF tot_amt_gst IS NOT INITIAL.
*        ls_dcnote-cgst_per  = ( ls_dcnote-cgst_amt * 100 ) / tot_amt_gst .
*      ENDIF.
*
*      IF tot_amt_gst IS NOT INITIAL.
*        ls_dcnote-sgst_per  = ( ls_dcnote-sgst_amt * 100 ) / tot_amt_gst .
*      ENDIF.
*
*      IF tot_amt_igst IS NOT INITIAL.
*        ls_dcnote-igst_per  = ( ls_dcnote-igst_amt * 100 ) / tot_amt_igst .
*      ENDIF.
*
*      IF ls_dcnote-total_val IS NOT INITIAL.
*        ls_dcnote-tcs_per  = ( ls_dcnote-tcs_amt * 100 ) / ls_dcnote-total_val .
*      ENDIF.
*      ""****End:GST %***************

        DATA: lv_grand_tot_word TYPE string,
              lv_gst_amt_word   TYPE string.

        lv_grand_tot_word = gs_dbnote-grand_total.
        lo_amt_words->number_to_words(
          EXPORTING
            iv_num   = lv_grand_tot_word
          RECEIVING
            rv_words = DATA(amt_words)
        ).

        lv_gst_amt_word = gs_dbnote-sum_gst_amt.
        lo_amt_words->number_to_words(
          EXPORTING
            iv_num   = lv_gst_amt_word
          RECEIVING
            rv_words = DATA(amt_words_gst)
        ).

        CONCATENATE amt_words 'Only' INTO gs_dbnote-tot_amt_words SEPARATED BY space ##NO_TEXT.
        CONCATENATE amt_words_gst 'Only' INTO gs_dbnote-gst_amt_words SEPARATED BY space ##NO_TEXT.


        INSERT LINES OF gt_item INTO TABLE gs_dbnote-gt_item.
        APPEND gs_dbnote TO et_dbdata. "gt_dbnote.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD get_payadv_data.
    DATA:
      lo_amt_words    TYPE REF TO zcl_amt_words,

*      lv_total_value   TYPE p LENGTH 16 DECIMALS 2,
*      lv_sum_frt_amt   TYPE p LENGTH 16 DECIMALS 2,
*      lv_sum_cgst_amt  TYPE p LENGTH 16 DECIMALS 2,
*      lv_sum_sgst_amt  TYPE p LENGTH 16 DECIMALS 2,
*      lv_sum_igst_amt  TYPE p LENGTH 16 DECIMALS 2,
*      lv_sum_tcs_amt   TYPE p LENGTH 16 DECIMALS 2,
*      lv_sum_load_amt  TYPE p LENGTH 16 DECIMALS 2,
*      lv_sum_rndf_amt  TYPE p LENGTH 16 DECIMALS 2,
*      lv_sum_other_amt TYPE p LENGTH 16 DECIMALS 2,

      lv_grand_total  TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_bill_amt TYPE p LENGTH 16 DECIMALS 2,
      lv_sum_tds_amt  TYPE p LENGTH 16 DECIMALS 2.

    DATA:
      lv_amount_neg TYPE c LENGTH 20,
      lv_advanced   TYPE c.

    CREATE OBJECT lo_amt_words.

    IF im_belnr IS NOT INITIAL.

      SELECT
      *
      FROM zi_dc_note
      WHERE
      companycode = @im_bukrs AND clearingjournalentry = @im_belnr AND clearingjournalentryfiscalyear = @im_gjahr
      INTO TABLE @DATA(lt_acc).               "#EC CI_ALL_FIELDS_NEEDED


      DATA(at_acc) = lt_acc[].
      DELETE at_acc WHERE accountingdocument NE im_belnr.

      IF at_acc[] IS INITIAL.

        SELECT
        *
        FROM zi_dc_note
        WHERE
        companycode = @im_bukrs AND accountingdocument = @im_belnr AND fiscalyear = @im_gjahr
        INTO TABLE @lt_acc.                   "#EC CI_ALL_FIELDS_NEEDED

        lv_advanced = abap_true.

      ENDIF.

      IF lt_acc[] IS NOT INITIAL.

        SELECT

        companycode,
        accountingdocument,
        fiscalyear,
        accountingdocumentitem,
        financialaccounttype,
        chartofaccounts,
        accountingdocumentitemtype,
        postingkey,
        product,
        plant,
        postingdate,
        documentdate,
        debitcreditcode,
        taxcode,
        taxitemgroup,
        transactiontypedetermination,
        glaccount,
        customer,
        supplier,
        purchasingdocument,
        purchasingdocumentitem,
        purchaseorderqty,
        profitcenter,
        documentitemtext,
        amountincompanycodecurrency,
        amountintransactioncurrency,
        cashdiscountbaseamount,
        netpaymentamount,
        assignmentreference,
        invoicereference,
        invoicereferencefiscalyear,
        invoiceitemreference,
        quantity,
        baseunit,
        materialpriceunitqty,
        taxbaseamountintranscrcy,
        clearingjournalentry,
        clearingdate,
        clearingcreationdate,
        clearingjournalentryfiscalyear,
        clearingitem,
        housebank,
        bpbankaccountinternalid,
        housebankaccount,
        in_hsnorsaccode,
        costcenter,
        accountingdocumenttype,
        netduedate,
        offsettingaccount,
        transactioncurrency,
        paymentterms,
        businessplace,
        valuedate,
        paymentmethod,
        specialglcode,
        specialgltransactiontype,
        documentreferenceid,
        alternativereferencedocument,
        accountingdocumentheadertext,
        companycodename,
        addressid,
        supplierfullname,
        customerfullname

          FROM zi_dc_note
          FOR ALL ENTRIES IN @lt_acc
          WHERE companycode = @lt_acc-companycode AND
          accountingdocument = @lt_acc-accountingdocument AND fiscalyear = @lt_acc-fiscalyear
          INTO TABLE @DATA(lt_acc_clear).          "#EC CI_NO_TRANSFORM

        IF lt_acc_clear[] IS NOT INITIAL.

          READ TABLE lt_acc_clear INTO DATA(lcs_acc_clear) INDEX 1. "#EC CI_NOORDER

          SELECT SINGLE
companycode,
accountingdocument,
fiscalyear,
bankname,
benefbankaccount,
banknumber,
paymentmethoddescription
          FROM zi_bank_payment
          WHERE companycode        = @lcs_acc_clear-companycode AND
                accountingdocument = @lcs_acc_clear-accountingdocument AND
                fiscalyear         = @lcs_acc_clear-fiscalyear AND
                financialaccounttype = 'K'
          INTO @DATA(ls_bank).                     "#EC CI_NO_TRANSFORM


          SELECT * FROM i_housebankaccountlinkage
          FOR ALL ENTRIES IN @lt_acc_clear
          WHERE companycode = @lt_acc_clear-companycode AND
                housebank    = @lt_acc_clear-housebank AND
                housebankaccount = @lt_acc_clear-housebankaccount
                INTO TABLE @DATA(lt_bank_acc). "#EC CI_ALL_FIELDS_NEEDED

        ENDIF.

      ENDIF.

      SELECT * FROM zi_cheque_detail
      WHERE
      paymentcompanycode = @im_bukrs AND paymentdocument = @im_belnr AND fiscalyear = @im_gjahr
      INTO TABLE @DATA(lt_chq).               "#EC CI_ALL_FIELDS_NEEDED

      DATA(xt_acc) = lt_acc[].
      DELETE xt_acc WHERE accountingdocument NE im_belnr.
      SORT xt_acc BY accountingdocument.
      DELETE ADJACENT DUPLICATES FROM xt_acc COMPARING accountingdocument.


      SELECT
        *
        FROM zi_dc_note
        WHERE companycode = @im_bukrs AND
        accountingdocument = @im_belnr AND fiscalyear = @im_gjahr
        AND invoicereference NE ''
        INTO TABLE @DATA(xt_part).            "#EC CI_ALL_FIELDS_NEEDED

      IF xt_part[] IS NOT INITIAL.

        LOOP AT xt_part ASSIGNING FIELD-SYMBOL(<lfs_part>).
          <lfs_part>-accountingdocument = <lfs_part>-invoicereference.
        ENDLOOP.

        SELECT
          *
          FROM zi_dc_note
          FOR ALL ENTRIES IN @xt_part
          WHERE companycode = @xt_part-companycode AND
          accountingdocument = @xt_part-accountingdocument AND fiscalyear = @xt_part-fiscalyear
          INTO TABLE @DATA(xt_acc_part).      "#EC CI_ALL_FIELDS_NEEDED

        APPEND LINES OF xt_part TO lt_acc.
      ENDIF.

      LOOP AT xt_acc INTO DATA(xs_acc).

        """******Header Data
        gs_dbnote-companycode          = xs_acc-companycode.
        gs_dbnote-accountingdocument   = xs_acc-accountingdocument.
        gs_dbnote-fiscalyear           = xs_acc-fiscalyear.
        gs_dbnote-postingdate          = xs_acc-postingdate.
        gs_dbnote-documentdate         = xs_acc-documentdate.
        gs_dbnote-trans_curr           = xs_acc-transactioncurrency.

        gs_dbnote-voucher_no        = im_belnr. "xs_acc-ClearingAccountingDocument. "xs_acc-clearingjournalentry.
        gs_dbnote-voucher_date      = xs_acc-documentdate+6(2) && '.' && xs_acc-documentdate+4(2) && '.' && xs_acc-documentdate+0(4).

        READ TABLE lt_chq INTO DATA(ls_chq) WITH KEY paymentdocument = xs_acc-clearingjournalentry
                                                     chequestatus    = '10'.

        gs_dbnote-bank_name         = ls_bank-bankname.
        gs_dbnote-bank_acc_no       = ls_bank-benefbankaccount.
        gs_dbnote-bank_ifsc_code    = ls_bank-banknumber.
        gs_dbnote-payment_mode      = ls_bank-paymentmethoddescription.
        gs_dbnote-bank_utr_no       = xs_acc-accountingdocumentheadertext.

        gs_dbnote-bank_det1         = ls_chq-housebankaccount.
        gs_dbnote-bank_det2         = ''.
        gs_dbnote-cheque_no         = ls_chq-outgoingcheque.
        gs_dbnote-cheque_date       = ls_chq-chequepaymentdate+6(2) && '.' && ls_chq-chequepaymentdate+4(2) && '.' && ls_chq-chequepaymentdate+0(4).
        gs_dbnote-po_num            = ''.

        IF gs_dbnote-bank_name IS INITIAL.

          READ TABLE lt_acc_clear INTO DATA(ls_acc_wit1) WITH KEY
                                       companycode = xs_acc-companycode
                                       accountingdocument =  xs_acc-accountingdocument
                                       fiscalyear = xs_acc-fiscalyear
                                       debitcreditcode = 'H'.

          IF sy-subrc EQ 0.

            READ TABLE lt_bank_acc INTO DATA(ls_bank_acc) WITH KEY
                                   companycode = xs_acc-companycode
                                   housebank     = ls_acc_wit1-housebank
                                   housebankaccount = ls_acc_wit1-housebankaccount.

            "gs_dbnote-bank_name  = ls_bank_acc-BankAccountNumber.     "ls_acc_wit1-HouseBank.
            gs_dbnote-bank_det1  = ''. "ls_acc_wit1-HouseBankAccount.s

          ENDIF.
        ENDIF.

*        DATA(lt_acc_plant) = lt_acc[].
*        DELETE lt_acc_plant WHERE Plant EQ ''.
*        READ TABLE lt_acc_plant INTO DATA(ls_acc_plant) INDEX 1.
*        IF sy-subrc EQ 0.
*
*          SELECT SINGLE * FROM zi_plant_address
*          WHERE plant = @ls_acc_plant-Plant INTO @DATA(ls_plant_adrs).
*
*          gs_dbnote-suppl_code         = ls_acc_plant-Plant.
*          gs_dbnote-suppl_name         = ls_plant_adrs-PlantName.
*          gs_dbnote-suppl_addr1        = ls_plant_adrs-StreetPrefixName1 && ',' && ls_plant_adrs-StreetPrefixName2.
*          gs_dbnote-suppl_addr2        = ls_plant_adrs-StreetName &&  ',' && ls_plant_adrs-StreetSuffixName1 &&  ',' && ls_plant_adrs-DistrictName.
*          gs_dbnote-suppl_addr3        = ls_plant_adrs-CityName &&  ',' && ls_plant_adrs-PostalCode .
*          gs_dbnote-suppl_cin          = 'U74899DL1988PTC031984'.
*          gs_dbnote-suppl_gstin        = '06AAECA0297J1ZO'. "for plant 1001
*          gs_dbnote-suppl_pan          = gs_dbnote-suppl_gstin+0(10).
*          gs_dbnote-suppl_stat_code    = ls_plant_adrs-Region.
*          gs_dbnote-suppl_phone        = ''.
*          gs_dbnote-suppl_email        = 'info@anandnvh.com'.
*
*        ENDIF.

        DATA(lt_acc_suppl) = lt_acc[].
        DELETE lt_acc_suppl WHERE supplier EQ ''.
        READ TABLE lt_acc_suppl INTO DATA(ls_acc_suppl) INDEX 1. "#EC CI_NOORDER
        IF sy-subrc EQ 0.

          SELECT SINGLE * FROM zi_supplier_address
          WHERE supplier = @ls_acc_suppl-supplier INTO @DATA(ls_supplier). "#EC CI_NOORDER

          gs_dbnote-suppl_code         = ls_supplier-supplier.
          gs_dbnote-suppl_name         = ls_supplier-suppliername.
          gs_dbnote-suppl_addr1        = ls_supplier-streetprefixname1 && ',' && ls_supplier-streetprefixname2.
          gs_dbnote-suppl_addr2        = ls_supplier-streetname &&  ',' && ls_supplier-streetsuffixname1 &&  ',' && ls_supplier-districtname.
          gs_dbnote-suppl_addr3        = ls_supplier-cityname &&  ',' && ls_supplier-postalcode .
          gs_dbnote-suppl_cin          = 'U15100UR2003PLC028002' ##NO_TEXT.
          gs_dbnote-suppl_gstin        = '05AACCB5752D1Z2' ##NO_TEXT. "for plant 1001
          gs_dbnote-suppl_pan          = gs_dbnote-suppl_gstin+0(10).
          gs_dbnote-suppl_stat_code    = ls_supplier-region.
          gs_dbnote-suppl_phone        = ''.
          gs_dbnote-suppl_email        = 'info@blv.com' ##NO_TEXT.


*          clear: lv_amount_neg.
*          lv_amount_neg = xs_acc-AmountInCompanyCodeCurrency .
*          CONDENSE lv_amount_neg.
*          IF lv_amount_neg CA '-'.
*            lv_grand_total = xs_acc-AmountInCompanyCodeCurrency * -1.
*          else.
*            lv_grand_total = xs_acc-AmountInCompanyCodeCurrency.
*          ENDIF.

        ENDIF.

        """******Item Data


        CLEAR: gs_item.
        LOOP AT lt_acc INTO DATA(ls_acc).

          IF ls_acc-accountingdocument NE xs_acc-accountingdocument.

            gs_item-companycode            = ls_acc-companycode.
            gs_item-accountingdocument     = ls_acc-accountingdocument.
            gs_item-fiscalyear             = ls_acc-fiscalyear.
            gs_item-accountingdocumentitem = ls_acc-accountingdocumentitem.

            gs_item-bill_num        = ls_acc-documentreferenceid.
            gs_item-bill_date       = ls_acc-postingdate+6(2) && '.' && ls_acc-postingdate+4(2) && '.' && ls_acc-postingdate+0(4).
            gs_item-debit_note_no   = ''.
            gs_item-debit_date      = ''.
            gs_item-debit_amt       = ''.

            READ TABLE xt_acc_part INTO DATA(xs_acc_part) WITH KEY
                                         companycode = ls_acc-companycode
                                         accountingdocument =  ls_acc-accountingdocument
                                         fiscalyear = ls_acc-fiscalyear.

            IF sy-subrc NE 0.

              READ TABLE lt_acc_clear INTO DATA(ls_acc_wit) WITH KEY
                                           companycode = ls_acc-companycode
                                           accountingdocument =  ls_acc-accountingdocument
                                           fiscalyear = ls_acc-fiscalyear
                                           transactiontypedetermination = 'WIT'.

              IF sy-subrc EQ 0.

                CLEAR: lv_amount_neg.
                lv_amount_neg = ls_acc_wit-amountincompanycodecurrency .
                CONDENSE lv_amount_neg.
                IF lv_amount_neg CA '-'.
                  ls_acc_wit-amountincompanycodecurrency =  ls_acc_wit-amountincompanycodecurrency * -1.
                ENDIF.

                gs_item-tds_amt         = ls_acc_wit-amountincompanycodecurrency .
                "*lv_sum_tds_amt        = lv_sum_tds_amt + ls_acc_wit-AmountInCompanyCodeCurrency .

                IF ls_acc-specialglcode = 'A'.

                  CLEAR: lv_amount_neg.
                  lv_amount_neg = ls_acc-amountincompanycodecurrency.
                  CONDENSE lv_amount_neg.
                  IF lv_amount_neg CA '-'.
                    gs_item-bill_amt        = ls_acc-amountincompanycodecurrency * -1.
                  ELSE.
                    gs_item-bill_amt        = ls_acc-amountincompanycodecurrency.
                  ENDIF.

                ELSE.

                  CLEAR: lv_amount_neg.
                  lv_amount_neg = ls_acc-cashdiscountbaseamount.
                  CONDENSE lv_amount_neg.
                  IF lv_amount_neg CA '-'.
                    gs_item-bill_amt        = ls_acc-cashdiscountbaseamount * -1.
                  ELSE.
                    gs_item-bill_amt        = ls_acc-cashdiscountbaseamount.
                  ENDIF.

                ENDIF.

              ELSE.

                CLEAR: lv_amount_neg.
                lv_amount_neg = ls_acc-amountincompanycodecurrency. "ls_acc-CashDiscountBaseAmount.
                CONDENSE lv_amount_neg.
                IF lv_amount_neg CA '-'.
                  gs_item-bill_amt        = ls_acc-amountincompanycodecurrency * -1.
                ELSE.
                  gs_item-bill_amt        = ls_acc-amountincompanycodecurrency.
                ENDIF.

              ENDIF.

              gs_item-net_amt         = gs_item-bill_amt + ( gs_item-tds_amt * -1 ).

              IF ls_acc-debitcreditcode EQ 'S'.
                gs_item-dr_cr           = 'Dr'.
                lv_grand_total          = lv_grand_total + ( gs_item-net_amt * -1 ).
                lv_sum_bill_amt         = lv_sum_bill_amt + ( gs_item-bill_amt * -1 ).
                lv_sum_tds_amt          = lv_sum_tds_amt + ( gs_item-tds_amt * -1 ) .
              ELSE.
                gs_item-dr_cr           = 'Cr'.
                lv_grand_total          = lv_grand_total + gs_item-net_amt.
                lv_sum_bill_amt         = lv_sum_bill_amt + gs_item-bill_amt.
                lv_sum_tds_amt          = lv_sum_tds_amt + gs_item-tds_amt.
              ENDIF.

            ELSE.

              READ TABLE xt_acc_part INTO DATA(xs_acc_egk) WITH KEY
                                           companycode = ls_acc-companycode
                                           accountingdocument =  ls_acc-accountingdocument
                                           fiscalyear = ls_acc-fiscalyear
                                           transactiontypedetermination = 'EGK'.

              gs_item-bill_num        = xs_acc_egk-documentreferenceid.
              gs_item-bill_date       = xs_acc_egk-postingdate+6(2) && '.' && xs_acc_egk-postingdate+4(2) && '.' && xs_acc_egk-postingdate+0(4).

              READ TABLE xt_acc_part INTO DATA(xs_acc_wit) WITH KEY
                                           companycode = ls_acc-companycode
                                           accountingdocument =  ls_acc-accountingdocument
                                           fiscalyear = ls_acc-fiscalyear
                                           transactiontypedetermination = 'WIT'.

              IF sy-subrc EQ 0.

                CLEAR: lv_amount_neg.
                lv_amount_neg = xs_acc_wit-amountincompanycodecurrency .
                CONDENSE lv_amount_neg.
                IF lv_amount_neg CA '-'.
                  xs_acc_wit-amountincompanycodecurrency =  xs_acc_wit-amountincompanycodecurrency * -1.
                ENDIF.

                gs_item-tds_amt         = xs_acc_wit-amountincompanycodecurrency .
                lv_sum_tds_amt          = lv_sum_tds_amt + xs_acc_wit-amountincompanycodecurrency .

                CLEAR: lv_amount_neg.
                lv_amount_neg = xs_acc_egk-amountincompanycodecurrency. "xs_acc_egk-CashDiscountBaseAmount.
                CONDENSE lv_amount_neg.
                IF lv_amount_neg CA '-'.
                  gs_item-bill_amt        = xs_acc_egk-amountincompanycodecurrency * -1.
                ELSE.
                  gs_item-bill_amt        = xs_acc_egk-amountincompanycodecurrency.
                ENDIF.

              ELSE.

                CLEAR: lv_amount_neg.
                lv_amount_neg = xs_acc_egk-cashdiscountbaseamount.
                CONDENSE lv_amount_neg.
                IF lv_amount_neg CA '-'.
                  gs_item-bill_amt        = xs_acc_egk-cashdiscountbaseamount * -1.
                ELSE.
                  gs_item-bill_amt        = xs_acc_egk-cashdiscountbaseamount.
                ENDIF.

              ENDIF.




              READ TABLE xt_part INTO DATA(cs_part)
                                         WITH KEY companycode = ls_acc-companycode
                                           accountingdocument =  ls_acc-accountingdocument
                                           fiscalyear = ls_acc-fiscalyear.
              IF sy-subrc EQ 0.

                CLEAR: lv_amount_neg.
                lv_amount_neg = cs_part-amountincompanycodecurrency.
                CONDENSE lv_amount_neg.

                IF lv_amount_neg CA '-'.
                  gs_item-net_amt         = cs_part-amountincompanycodecurrency * -1.
                ELSE.
                  gs_item-net_amt         = cs_part-amountincompanycodecurrency.
                ENDIF.

              ELSE.

                gs_item-net_amt         = gs_item-bill_amt + ( gs_item-tds_amt * -1 ).

              ENDIF.

              IF xs_acc_egk-debitcreditcode EQ 'S'.
                gs_item-dr_cr           = 'Dr'.
                lv_grand_total          = lv_grand_total + ( gs_item-net_amt * -1 ).
                lv_sum_bill_amt         = lv_sum_bill_amt + ( gs_item-bill_amt * -1 ).
              ELSE.
                gs_item-dr_cr           = 'Cr'.
                lv_grand_total          = lv_grand_total + gs_item-net_amt.
                lv_sum_bill_amt         = lv_sum_bill_amt + gs_item-bill_amt.
              ENDIF.


              CLEAR: xs_acc_egk, xs_acc_wit.

            ENDIF.

            gs_item-trans_curr = ls_acc-transactioncurrency.

            APPEND gs_item TO gt_item.

          ENDIF.

          """*****For Advanced*************************************************
          IF lv_advanced = abap_true AND
             ls_acc-clearingjournalentry NE ls_acc-accountingdocument AND
             ls_acc-specialglcode NE ''.

            gs_item-companycode            = ls_acc-companycode.
            gs_item-accountingdocument     = ls_acc-accountingdocument.
            gs_item-fiscalyear             = ls_acc-fiscalyear.
            gs_item-accountingdocumentitem = ls_acc-accountingdocumentitem.
            gs_item-bill_num               = ls_acc-documentreferenceid.
            gs_item-trans_curr             = ls_acc-transactioncurrency.


            gs_item-bill_num        = ls_acc-documentreferenceid.
            gs_item-bill_date       = ls_acc-postingdate+6(2) && '.' && ls_acc-postingdate+4(2) && '.' && ls_acc-postingdate+0(4).

            CLEAR: ls_acc_wit.
            READ TABLE lt_acc_clear INTO ls_acc_wit WITH KEY
                                         companycode = ls_acc-companycode
                                         accountingdocument =  ls_acc-accountingdocument
                                         fiscalyear = ls_acc-fiscalyear
                                         transactiontypedetermination = 'WIT'.

            IF sy-subrc EQ 0.

              CLEAR: lv_amount_neg.
              lv_amount_neg = ls_acc_wit-amountincompanycodecurrency .
              CONDENSE lv_amount_neg.
              IF lv_amount_neg CA '-'.
                ls_acc_wit-amountincompanycodecurrency =  ls_acc_wit-amountincompanycodecurrency * -1.
              ENDIF.

              gs_item-tds_amt         = ls_acc_wit-amountincompanycodecurrency .

              CLEAR: lv_amount_neg.
              lv_amount_neg = ls_acc-amountincompanycodecurrency.
              CONDENSE lv_amount_neg.
              IF lv_amount_neg CA '-'.
                gs_item-bill_amt        = ls_acc-amountincompanycodecurrency * -1.
              ELSE.
                gs_item-bill_amt        = ls_acc-amountincompanycodecurrency.
              ENDIF.

            ENDIF.

            gs_item-net_amt         = gs_item-bill_amt + ( gs_item-tds_amt * -1 ).

            IF ls_acc-debitcreditcode EQ 'S'.
              gs_item-dr_cr           = 'Dr'.
            ELSE.
              gs_item-dr_cr           = 'Cr'.
            ENDIF.

            lv_grand_total          = lv_grand_total + gs_item-net_amt.
            lv_sum_bill_amt         = lv_sum_bill_amt + gs_item-bill_amt.
            lv_sum_tds_amt          = lv_sum_tds_amt + gs_item-tds_amt.

            APPEND gs_item TO gt_item.

          ENDIF.

          CLEAR: ls_acc, ls_acc_wit, gs_item.
        ENDLOOP.


        gs_dbnote-grand_total     = lv_grand_total.
        gs_dbnote-chq_amt         = ''.
        gs_dbnote-sum_bil_amt     = lv_sum_bill_amt.
        gs_dbnote-sum_tds_amt     = lv_sum_tds_amt.
        gs_dbnote-sum_debit_amt   = ''.
        gs_dbnote-sum_net_amt     = lv_grand_total.

        CLEAR: lv_amount_neg.
        lv_amount_neg = lv_grand_total.
        CONDENSE lv_amount_neg.
        CONCATENATE 'Being amount of INR'
                    lv_amount_neg
                    'Paid To'
                    gs_dbnote-suppl_name
*                    'against bill no'
*                    im_belnr
                    INTO gs_dbnote-narration SEPARATED BY space ##NO_TEXT.


        DATA: lv_grand_tot_word TYPE string,
              lv_gst_amt_word   TYPE string.

        IF gs_dbnote-grand_total IS NOT INITIAL.

          lv_grand_tot_word = gs_dbnote-grand_total.
          lo_amt_words->number_to_words(
            EXPORTING
              iv_num   = lv_grand_tot_word
            RECEIVING
              rv_words = DATA(amt_words)
          ).

        ENDIF.

        gs_dbnote-tot_amt_words = amt_words.

        INSERT LINES OF gt_item INTO TABLE gs_dbnote-gt_item.
        APPEND gs_dbnote TO et_payadv.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD prep_xml_chqprnt.

    DATA : heading      TYPE c LENGTH 100,
           lv_xml_final TYPE string.

    heading      = 'Bhagwati Lacto Vegetarian Exports Pvt. Ltd.' ##NO_TEXT.

    READ TABLE it_chqprnt INTO DATA(ls_chqprnt) INDEX 1.

    DATA(lv_xml) =  |<Form>| &&
                    |<AccountDocumentNode>| &&
                    |<heading>{ ls_chqprnt-suppl_name }</heading>| &&
                    |<cheque_no>{ ls_chqprnt-cheque_no }</cheque_no>| &&
                    |<cheque_date>{ ls_chqprnt-cheque_date }</cheque_date>| &&
                    |<chq_amt>{ ls_chqprnt-chq_amt }</chq_amt>| &&
                    |<amt_words>{ ls_chqprnt-tot_amt_words }</amt_words>| &&
                    |<ac_payee>{ ls_chqprnt-acc_payee  }</ac_payee>| &&
                    |<ItemData>| .

    DATA : lv_item TYPE string,
           lv_date TYPE c LENGTH 10,
           lv_dat1 TYPE c,
           lv_dat2 TYPE c,
           lv_dat3 TYPE c,
           lv_dat4 TYPE c,
           lv_dat5 TYPE c,
           lv_dat6 TYPE c,
           lv_dat7 TYPE c,
           lv_dat8 TYPE c.

    lv_date = ls_chqprnt-postingdate.
    lv_date = lv_date+6(2) && lv_date+4(2) && lv_date+0(4).

    lv_dat1 =  lv_date+0(1).
    lv_dat2 =  lv_date+1(1).
    lv_dat3 =  lv_date+2(1).
    lv_dat4 =  lv_date+3(1).
    lv_dat5 =  lv_date+4(1).
    lv_dat6 =  lv_date+5(1).
    lv_dat7 =  lv_date+6(1).
    lv_dat8 =  lv_date+7(1).

    CLEAR : lv_item.
    lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                    |<chq_d1>{ lv_dat1 }</chq_d1>| &&
                    |<chq_d2>{ lv_dat2 }</chq_d2>| &&
                    |<chq_d3>{ lv_dat3 }</chq_d3>| &&
                    |<chq_d4>{ lv_dat4 }</chq_d4>| &&
                    |<chq_d5>{ lv_dat5 }</chq_d5>| &&
                    |<chq_d6>{ lv_dat6 }</chq_d6>| &&
                    |<chq_d7>{ lv_dat7 }</chq_d7>| &&
                    |<chq_d8>{ lv_dat8 }</chq_d8>| &&
              |</ItemDataNode>|  .

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                       |</AccountDocumentNode>| &&
                       |</Form>|.

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.

  ENDMETHOD.


  METHOD prep_xml_fidebit.

    DATA : lv_qty        TYPE p LENGTH 16 DECIMALS 2,
           lv_netwt      TYPE p LENGTH 16 DECIMALS 2,
           lv_grosswt    TYPE p LENGTH 16 DECIMALS 2,
           lv_dis        TYPE p LENGTH 16 DECIMALS 2,
           lv_tot_amt    TYPE p LENGTH 16 DECIMALS 2,
           lv_tax_amt    TYPE p LENGTH 16 DECIMALS 2,
           lv_tot_sgst   TYPE p LENGTH 16 DECIMALS 2,
           lv_tot_cgst   TYPE p LENGTH 16 DECIMALS 2,
           lv_tot_igst   TYPE p LENGTH 16 DECIMALS 2,
           lv_tot_igst1  TYPE p LENGTH 16 DECIMALS 2,
           lv_tot_cgst1  TYPE p LENGTH 16 DECIMALS 2,
           lv_tot_sgst1  TYPE p LENGTH 16 DECIMALS 2,
           lv_tcs        TYPE p LENGTH 16 DECIMALS 2,
           lv_other_chrg TYPE p LENGTH 16 DECIMALS 2,
           lv_round_off  TYPE p LENGTH 16 DECIMALS 2,
           lv_tot_gst    TYPE p LENGTH 16 DECIMALS 2,
           lv_grand_tot  TYPE p LENGTH 16 DECIMALS 2,
           lv_gross      TYPE p LENGTH 16 DECIMALS 2,
           lv_net        TYPE p LENGTH 16 DECIMALS 2,
           heading       TYPE c LENGTH 100,
           sub_heading   TYPE c LENGTH 100,
           for_sign      TYPE c LENGTH 100,
           odte_text     TYPE c LENGTH 20,
           head_lut      TYPE c LENGTH 100,
           lv_jurid      TYPE c LENGTH 100,
           curr          TYPE c LENGTH 100,
           exc_rt        TYPE c LENGTH 100,
           lv_dt_bill    TYPE c LENGTH 10,
           lv_dt_po      TYPE c LENGTH 10,
           lv_dt_ack     TYPE c LENGTH 10,
           lv_item       TYPE string,
           srn           TYPE c LENGTH 3.

    DATA: lv_vbeln_n   TYPE c LENGTH 10,
          lv_qr_code   TYPE string,
          lv_irn_num   TYPE c LENGTH 64, "w_irn-irnno
          lv_ack_no    TYPE c LENGTH 20, "w_irn-ackno
          lv_ack_date  TYPE c LENGTH 10, "w_irn-ackdat
          lv_ref_sddoc TYPE c LENGTH 20. "w_item-ReferenceSDDocument

    READ TABLE it_dbnote INTO DATA(ls_dbnote) INDEX 1.

    SELECT SINGLE

billingdocument ,
plant_gstin     ,
companycode     ,
fiscalyear      ,
plant               ,
ackno               ,
ackdt               ,
irn                 ,
signedinvoice       ,
signedqrcode        ,
ewbno               ,
ewbdt               ,
ewbvalidtill        ,
qrcodeurl           ,
erdat               ,
uname               ,
uzeit               ,
cancel_status       ,
cancel_date         ,
cancel_time

    FROM zsd_einvoice WHERE billingdocument = @ls_dbnote-accountingdocument AND
                                            companycode     = @ls_dbnote-companycode AND
                                            fiscalyear      = @ls_dbnote-fiscalyear
      INTO @DATA(w_einvvoice) .                         "#EC CI_NOORDER

    CLEAR : lv_qr_code , lv_irn_num   , lv_ack_no ,lv_ack_date .

    lv_qr_code  = w_einvvoice-signedqrcode .
    lv_irn_num  = w_einvvoice-irn .
    lv_ack_no   = w_einvvoice-ackno .
    lv_ack_date = w_einvvoice-ackdt+6(2) && '.' && w_einvvoice-ackdt+4(2) && '.' && w_einvvoice-ackdt+0(4).

    heading      = 'Debit Note' ##NO_TEXT.

    IF im_action = 'ficredit' ##NO_TEXT.
      heading     = 'Credit Note' ##NO_TEXT.
    ELSEIF im_action = 'fircm' ##NO_TEXT.
      heading     = 'Tax Invoice' ##NO_TEXT.
    ELSEIF im_action = 'fitaxinv'.
      heading     = 'Tax Invoice' ##NO_TEXT.
    ENDIF.




    IF ls_dbnote-companycode = '1000'.
      lv_jurid = 'Ferozepur' ##NO_TEXT.
      for_sign  = 'Bhagwati Lacto Vegetarian Exports Pvt. Ltd.' ##NO_TEXT.
    ELSEIF ls_dbnote-companycode = '2000'.
      lv_jurid = 'Ferozepur' ##NO_TEXT.
      for_sign  = 'Bhagwati Lacto Foods Private Limited' ##NO_TEXT.
    ELSEIF ls_dbnote-companycode = '3000'.
      lv_jurid = 'Gandhidham' ##NO_TEXT.
      for_sign  = 'Healthy Harvested Foods Private Limited' ##NO_TEXT.
    ELSEIF ls_dbnote-companycode = '4000'.
      lv_jurid = 'Narmadapuram' ##NO_TEXT.
      for_sign  = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
    ENDIF.

    if w_einvvoice-plant = '1002'.
       lv_jurid = 'Gandhidham' ##NO_TEXT.
       for_sign  = 'HHF Kandla' ##NO_TEXT.
    endif.

    if w_einvvoice-plant = '4001'.
       lv_jurid = 'Narmadapuram' ##NO_TEXT.
       for_sign  = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
    endif.

    DATA(lv_xml) = |<Form>| &&
                   |<BillingDocumentNode>| &&
                   |<heading>{ heading }</heading>| &&
                   |<sub_heading>{ sub_heading }</sub_heading>| &&
                   |<jurid_city>{ lv_jurid }</jurid_city>| &&
                   |<head_lut>{ head_lut }</head_lut>| &&
                   |<for_sign>{ for_sign }</for_sign>| &&
                   |<odte_text>{ gs_dbnote-revrs_yes_no }</odte_text>| &&
                   |<doc_curr>{ curr }</doc_curr>| &&
                   |<plant_code>{ ls_dbnote-suppl_code }</plant_code>| &&
                   |<plant_name>{ ls_dbnote-suppl_name }</plant_name>| &&
                   |<plant_address_l1>{ ls_dbnote-suppl_addr1 }</plant_address_l1>| &&
                   |<plant_address_l2>{ ls_dbnote-suppl_addr2 }</plant_address_l2>| &&
                   |<plant_address_l3>{ ls_dbnote-suppl_addr3 }</plant_address_l3>| &&
                   |<plant_cin>{ ls_dbnote-suppl_cin }</plant_cin>| &&
                   |<plant_gstin>{ ls_dbnote-suppl_gstin }</plant_gstin>| &&
                   |<plant_pan>{ ls_dbnote-suppl_gstin+2(10) }</plant_pan>| &&
                   |<plant_state_code>{ ls_dbnote-suppl_stat_code }</plant_state_code>| &&
                   |<plant_state_name></plant_state_name>| &&
                   |<plant_phone>{ ls_dbnote-suppl_phone }</plant_phone>| &&
                   |<plant_email>{ ls_dbnote-suppl_email }</plant_email>| &&

                   |<billto_code>{ ls_dbnote-billto_code }</billto_code>| &&
                   |<billto_name>{ ls_dbnote-billto_name }</billto_name>| &&
                   |<billto_address_l1>{ ls_dbnote-billto_addr1 }</billto_address_l1>| &&
                   |<billto_address_l2>{ ls_dbnote-billto_addr2 }</billto_address_l2>| &&
                   |<billto_address_l3>{ ls_dbnote-billto_addr3 }</billto_address_l3>| &&
                   |<billto_cin>{ ls_dbnote-billto_cin }</billto_cin>| &&
                   |<billto_gstin>{ ls_dbnote-billto_gstin }</billto_gstin>| &&
                   |<billto_pan>{ ls_dbnote-billto_gstin+2(10) }</billto_pan>| &&
                   |<billto_state_code>{ ls_dbnote-billto_stat_code }</billto_state_code>| &&
                   |<billto_state_name></billto_state_name>| &&
*                  |<billto_place_suply>{ ls_dbnote-sup }</billto_place_suply>| &&
                   |<billto_phone>{ ls_dbnote-billto_phone }</billto_phone>| &&
                   |<billto_email>{ ls_dbnote-billto_email }</billto_email>| &&

                   |<shipto_code>{ ls_dbnote-shipto_code }</shipto_code>| &&
                   |<shipto_name>{ ls_dbnote-shipto_name }</shipto_name>| &&
                   |<shipto_address_l1>{ ls_dbnote-shipto_addr1 }</shipto_address_l1>| &&
                   |<shipto_address_l2>{ ls_dbnote-shipto_addr2 }</shipto_address_l2>| &&
                   |<shipto_address_l3>{ ls_dbnote-shipto_addr3 }</shipto_address_l3>| &&
*                  |<shipto_cin>{ W_FINAL-PlantName }</shipto_cin>| &&
                   |<shipto_gstin>{ ls_dbnote-shipto_gstin }</shipto_gstin>| &&
                   |<shipto_pan>{ ls_dbnote-shipto_pan }</shipto_pan>| &&
                   |<shipto_state_code>{ ls_dbnote-shipto_stat_code }</shipto_state_code>| &&
                   |<shipto_state_name></shipto_state_name>| &&
                   |<shipto_place_suply>{ ls_dbnote-shipto_place_supply }</shipto_place_suply>| &&
                   |<shipto_phone>{ ls_dbnote-shipto_phone }</shipto_phone>| &&
                   |<shipto_email>{ ls_dbnote-shipto_email }</shipto_email>| &&

                   |<inv_no>{ ls_dbnote-inv_no }  </inv_no>| &&
                   |<inv_date>{ ls_dbnote-inv_date }</inv_date>| &&
                   |<inv_ref>{ ls_dbnote-inv_ref_no }</inv_ref>| &&
*                        |<exchange_rate>{ exc_rt }</exchange_rate>| &&
*                        |<currency>{ w_final-TransactionCurrency }</currency>| &&
*                        |<Exp_Inv_No>{ lv_exp_no }</Exp_Inv_No>| &&       """""""
                   |<IRN_num>{ lv_irn_num }</IRN_num>| &&
                   |<IRN_ack_No>{ lv_ack_no }</IRN_ack_No>| &&
                   |<irn_ack_date>{ lv_ack_date }</irn_ack_date>| &&
*                        |<irn_doc_type></irn_doc_type>| &&     """"""
*                        |<irn_category></irn_category>| &&     """"""
                        |<qrcode>{ lv_qr_code }</qrcode>| &&
*                        |<vcode>{ vcode }</vcode>| &&
*                        |<vplant>{ lv_cus_pl }</vplant>| &&
*                        |<pur_odr_no>{ w_final-PurchaseOrderByCustomer }</pur_odr_no>| &&
*                        |<pur_odr_date>{ lv_dt_po }</pur_odr_date>| &&
*                        |<Pay_term>{ w_final-CustomerPaymentTerms }:{ w_final-CustomerPaymentTermsName }</Pay_term>| &&  """"
                   |<Veh_no>{ ls_dbnote-veh_no }</Veh_no>| &&
                   |<Trans_mode>{ ls_dbnote-trnas_mode }</Trans_mode>| &&
*                        |<Ewaybill_no>{ lv_eway }</Ewaybill_no>| &&
*                        |<Ewaybill_date>{ lv_eway_dt }</Ewaybill_date>| &&

                   |<ItemData>| .

    LOOP AT ls_dbnote-gt_item INTO DATA(w_item) .

      srn = srn + 1 .
      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<sno>{ srn }</sno>| &&

                    |<item_code>{ w_item-itemcode }</item_code>| &&
*                    |<item_cust_refno>{ lv_ref_sddoc }</item_cust_refno>| &&
                    |<item_desc>{ w_item-itemdesc }</item_desc>| &&
                    |<item_hsn_code>{ w_item-hsncode }</item_hsn_code>| &&
                    |<item_uom>{ w_item-uom }</item_uom>| &&
                    |<item_qty>{ w_item-itmqty }</item_qty>| &&
                    |<item_unit_rate>{ w_item-unit_rate }</item_unit_rate>| &&
                    |<item_amt_inr>{ w_item-amount }</item_amt_inr>| &&
                    |<item_discount>{ w_item-discount }</item_discount>| &&
                    |<item_taxable_amt>{ w_item-amount }</item_taxable_amt>| &&
                    |<item_sgst_rate>{ w_item-sgst_rate }</item_sgst_rate>| &&
                    |<item_sgst_amt>{ w_item-sgst_amt }</item_sgst_amt>| &&
                    |<item_cgst_amt>{ w_item-cgst_amt }</item_cgst_amt>| &&
                    |<item_cgst_rate>{ w_item-cgst_rate }</item_cgst_rate>| &&
                    |<item_igst_amt>{ w_item-igst_amt }</item_igst_amt>| &&
                    |<item_igst_rate>{ w_item-igst_rate }</item_igst_rate>| &&
                    |<item_curr>{ w_item-trans_curr }</item_curr>| &&
*                    |<item_amort_amt>{ w_item-item_amotization }</item_amort_amt>| &&

                |</ItemDataNode>|  .

    ENDLOOP .

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                        |<total_amount_words>(INR) { ls_dbnote-tot_amt_words }</total_amount_words>| &&
                        |<gst_amt_words>(INR) { ls_dbnote-gst_amt_words }</gst_amt_words>| &&
                        |<remark_if_any>{ ls_dbnote-remark }</remark_if_any>| &&
*                        |<no_of_package>{ lv_no_pck }</no_of_package>| &&
*                        |<total_Weight>{ lv_qty }</total_Weight>| &&
*                        |<gross_Weight>{ lv_gross }</gross_Weight>| &&
*                        |<net_Weight>{ lv_net }</net_Weight>| &&

                         |<tot_qty></tot_qty>| &&  """ line item total quantity
                         |<total_amount>{ ls_dbnote-total_value }</total_amount>| &&
                         |<total_discount></total_discount>| &&

                        |<total_taxable_value>{ ls_dbnote-total_value }</total_taxable_value>| &&
                        |<total_cgst>{ ls_dbnote-sum_cgst_amt }</total_cgst>| &&
                        |<total_sgst>{ ls_dbnote-sum_sgst_amt }</total_sgst>| &&
                        |<total_igst>{ ls_dbnote-sum_igst_amt }</total_igst>| &&

                        |<total_igst1>{ ls_dbnote-sum_igst_amt1 }</total_igst1>| &&
                        |<total_sgst1>{ ls_dbnote-sum_sgst_amt1 }</total_sgst1>| &&
                        |<total_cgst1>{ ls_dbnote-sum_cgst_amt1 }</total_cgst1>| &&

                    ""   |<total_igst1>{ lv_tot_igst }</total_igst1>| &&
                        |<total_tcs>{ ls_dbnote-sum_tcs_amt }</total_tcs>| &&
                        |<total_other_chrg>{ ls_dbnote-sum_other_amt }</total_other_chrg>| &&
                        |<round_off>{ ls_dbnote-sum_rndf_amt }</round_off>| &&
                        |<grand_total>{ ls_dbnote-grand_total }</grand_total>| &&

                    |</BillingDocumentNode>| &&
                    |</Form>|.

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.

  ENDMETHOD.


  METHOD prep_xml_payadv.

    DATA : heading      TYPE c LENGTH 100,
           sub_heading  TYPE c LENGTH 200,
           sub_heading1 TYPE c LENGTH 200,
           lv_xml_final TYPE string,
           lv_user_name TYPE string,
           lv_user_id   TYPE i_iambusinessuserlogondetails-userid.

    sub_heading  = 'Payment Advice- ADV' ##NO_TEXT.

    READ TABLE it_payadv INTO DATA(ls_payadv) INDEX 1.

    CONDENSE ls_payadv-bank_name.
*    SHIFT ls_payadv-bank_name LEFT DELETING LEADING '0'.
*    DATA(lv_acc_len) = strlen( ls_payadv-bank_name ).
*    lv_acc_len = lv_acc_len - 4.
*    ls_payadv-bank_name = ls_payadv-bank_name+lv_acc_len(4). "&& '-' && ls_payadv-bank_det1.

    lv_user_id = sy-uname.
    SELECT SINGLE username FROM i_iambusinessuserlogondetails
                  WHERE userid = @lv_user_id
                  INTO @DATA(lv_user_detail).

    lv_user_name = lv_user_detail. "sy-uname

    IF ls_payadv-companycode = '1000'.

      heading            = 'BHAGWATI LACTO VEGETARIAN EXPORTS PVT. LTD'.
      sub_heading        = 'Regd. Office :18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
      sub_heading1       = 'Works : Rural Focal Point Mana Singh wala , Firozepur Moga Road' ##NO_TEXT.
      ls_payadv-suppl_cin = 'U45100PB2007PTC031212'.

    ELSEIF ls_payadv-companycode = '2000'.

      heading            = 'BHAGWATI LACTO FOODS PRIVATE LIMITED'.
      sub_heading        = 'VILL. RUKNA MUNGLA, FARIDKOT ROAD, FEROZEPUR, Ferozepur, Punjab, 152001' ##NO_TEXT.
      sub_heading1           = '' ##NO_TEXT.
      ls_payadv-suppl_cin = 'U15203PB2009PTC032538'.

    ELSEIF ls_payadv-companycode = '3000'.

      heading            = 'HEALTHY HARVESTED FOODS PRIVATE LIMITED'.
      sub_heading        = 'PLOT NO 100, SECTOR 1/A, GANDHIDHAM, Kachchh, Gujarat, 370201' ##NO_TEXT.
      sub_heading1       = '' ##NO_TEXT.
      ls_payadv-suppl_cin = 'U01100GJ2020PTC113206'.

    ELSEIF ls_payadv-companycode = '4000'.

      heading            = 'BHAGWATI AGROCHEM PRIVATE LIMITED'.
      sub_heading        = 'M-14A VILL. MOHASA BABAI INDSUTRIAL AREA MAKHAN NAGAR NARMADAPURAM M.P 461661' ##NO_TEXT.
      sub_heading1       = '' ##NO_TEXT.
      ls_payadv-suppl_cin = 'U63090PB2021PTC053299'.

    ENDIF.

    DATA(lv_xml) =  |<Form>| &&
                    |<AccountDocumentNode>| &&
                    |<heading>{ heading }</heading>| &&
                    |<sub_heading>{ sub_heading }</sub_heading>| &&
                    |<sub_heading1>{ sub_heading1 }</sub_heading1>| &&
                    |<suppl_cin>{ ls_payadv-suppl_cin }</suppl_cin>| &&
                    |<suppl_code>{ ls_payadv-suppl_code }</suppl_code>| &&
                    |<suppl_name>{ ls_payadv-suppl_name }</suppl_name>| &&
                    |<suppl_addrs1>{ ls_payadv-suppl_addr1 }</suppl_addrs1>| &&
                    |<suppl_addrs2>{ ls_payadv-suppl_addr2 }</suppl_addrs2>| &&
                    |<suppl_addrs3>{ ls_payadv-suppl_addr3 }</suppl_addrs3>| &&
                    |<suppl_addrs4>{ ls_payadv-suppl_addr4 }</suppl_addrs4>| &&
                    |<voucher_no>{ ls_payadv-voucher_no }</voucher_no>| &&
                    |<voucher_date>{ ls_payadv-voucher_date }</voucher_date>| &&
                    |<bank_name>{ ls_payadv-bank_name }</bank_name>| &&
                    |<bank_det1>{ ls_payadv-bank_det1 }</bank_det1>| &&
                    |<bank_det2>{ ls_payadv-bank_det2 }</bank_det2>| &&
                    |<cheque_no>{ ls_payadv-cheque_no }</cheque_no>| &&
                    |<cheque_date>{ ls_payadv-cheque_date }</cheque_date>| &&
                    |<po_num>{ ls_payadv-po_num }</po_num>| &&
                    |<chq_amt>{ ls_payadv-chq_amt }</chq_amt>| &&
                    |<amt_words>{ ls_payadv-tot_amt_words }</amt_words>| &&
                    |<narration>{ ls_payadv-narration }</narration>| &&
                    |<sum_bil_amt>{ ls_payadv-sum_bil_amt }</sum_bil_amt>| &&
                    |<sum_tds_amt>{ ls_payadv-sum_tds_amt }</sum_tds_amt>| &&
                    |<sum_debit_amt>{ ls_payadv-sum_debit_amt }</sum_debit_amt>| &&
                    |<sum_net_amt>{ ls_payadv-sum_net_amt }</sum_net_amt>| &&
                    |<header_curr>{ ls_payadv-trans_curr }</header_curr>| &&
                    |<user_name>{ lv_user_name }</user_name>| &&

                    |<bank_acc_no>{ ls_payadv-bank_acc_no }</bank_acc_no>| &&
                    |<bank_ifsc_code>{ ls_payadv-bank_ifsc_code }</bank_ifsc_code>| &&
                    |<payment_mode>{ ls_payadv-payment_mode }</payment_mode>| &&
                    |<bank_utr_no>{ ls_payadv-bank_utr_no }</bank_utr_no>| &&

                    |<ItemData>| .

    DATA : lv_item TYPE string .
    DATA : srn TYPE c LENGTH 3 .
    CLEAR : lv_item , srn .

    LOOP AT ls_payadv-gt_item INTO DATA(w_item) .

      srn = srn + 1 .

      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<sr_num>{ srn }</sr_num>| &&
                |<doc_num>{ w_item-accountingdocument }</doc_num>| &&
                |<bill_num>{ w_item-bill_num }</bill_num>| &&
                |<bill_date>{ w_item-bill_date }</bill_date>| &&
                |<bill_amt>{ w_item-bill_amt }</bill_amt>| &&
                |<tds_amt>{ w_item-tds_amt }</tds_amt>| &&
                |<debit_note_no>{ w_item-debit_note_no }</debit_note_no>| &&
                |<debit_date>{ w_item-debit_date }</debit_date>| &&
                |<debit_amt>{ w_item-debit_amt }</debit_amt>| &&
                |<net_amt>{ w_item-net_amt }</net_amt>| &&
                |<dr_cr>{ w_item-dr_cr }</dr_cr>| &&
                |<item_curr>{ w_item-trans_curr }</item_curr>| &&
                |</ItemDataNode>|  .

    ENDLOOP.

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                       |</AccountDocumentNode>| &&
                       |</Form>|.

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.

  ENDMETHOD.
ENDCLASS.
