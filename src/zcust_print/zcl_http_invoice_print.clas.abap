CLASS zcl_http_invoice_print DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

   DATA: xt_final1        TYPE TABLE OF zsd_pack_adv.
    DATA: xt_final        TYPE TABLE OF zi_sale_reg,
          ls_xml_base64   TYPE string,
          lv_access_token TYPE string.

    DATA: lv_vbeln      TYPE c LENGTH 10,
          lv_vbeln_n    TYPE c LENGTH 10,
          lv_pack_num   TYPE c LENGTH 10,
          rv_response   TYPE string,
          rv_resp_signd TYPE string,
          lv_action     TYPE c LENGTH 10,
          lv_prntval    TYPE c LENGTH 10.

    DATA: lo_client TYPE REF TO zcl_sd_custom_print,
          lo_ads    TYPE REF TO zcl_ads_service.

    INTERFACES if_http_service_extension .

  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_HTTP_INVOICE_PRINT IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

*    DATA: xt_final        TYPE TABLE OF zi_sale_reg,
*          ls_xml_base64   TYPE string,
*          lv_access_token TYPE string.
*
*
*    DATA: lv_vbeln      TYPE char10,
*          lv_vbeln_n    TYPE char10,
*          rv_response   TYPE string,
*          rv_resp_signd TYPE string,
*          lv_action     TYPE char10.
*
*    DATA: lo_client TYPE REF TO zcl_sd_custom_print,
*          lo_ads    TYPE REF TO zcl_ads_service.

    ""**Get input Data
    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname'.
    IF sy-subrc EQ 0.
      lv_action = ls_action-value.
    ENDIF.

    READ TABLE lt_input INTO DATA(ls_action1) WITH KEY name = 'radiovalue'.
    IF sy-subrc EQ 0.
      lv_prntval = ls_action1-value.
    ENDIF.

    READ TABLE lt_input INTO DATA(ls_input) WITH KEY name = 'billingdocument'.
    IF sy-subrc EQ 0.
      lv_vbeln = ls_input-value.
    ENDIF.

    lv_vbeln_n = lv_vbeln.
    lv_vbeln = |{ lv_vbeln ALPHA = IN }| .


*    READ TABLE lt_input INTO DATA(ls_pack) WITH KEY name = 'pack_num'.
*    IF sy-subrc EQ 0.
*      lv_pack_num = ls_pack-value.
*    ENDIF.
*    lv_pack_num = |{ lv_pack_num ALPHA = IN }| .

***    READ TABLE lt_input INTO DATA(ls_input1) WITH KEY name = 'packadvice'.
***    IF sy-subrc EQ 0.
***      lv_vbeln = ls_input1-value.
***    ENDIF.


    "********Creation of object**************
    CREATE OBJECT lo_client.
    CREATE OBJECT lo_ads.

    ""*****Calling Methods to get PDF code base64
    lv_access_token = lo_ads->get_ads_access_token(  ).

    IF lv_action = 'export'.

      lv_pack_num = lv_vbeln.
      SHIFT lv_pack_num LEFT DELETING LEADING '0'.
      xt_final      = lo_client->get_packing_data( im_pack = lv_pack_num  iv_action = lv_action ).

      IF xt_final[] IS NOT INITIAL.

        ls_xml_base64 = lo_client->prep_xml_pack_inv( it_final = xt_final[] iv_action = lv_action im_prntval = lv_prntval im_pack = lv_pack_num ).
        rv_response = lo_ads->get_ads_api_toget_base64(
          im_access_token  = lv_access_token
          im_template_name = 'ZSD_FORM_EXPORT_COMM_INV/ZSD_BLV_EXP_COMM_INV'
          im_xml_base64    = ls_xml_base64 ).

      ENDIF.

    ENDIF.


    IF lv_action = 'packls'.

      lv_pack_num = lv_vbeln.
      SHIFT lv_pack_num LEFT DELETING LEADING '0'.
      xt_final      = lo_client->get_packing_data( im_pack = lv_pack_num  iv_action = lv_action ).

      IF xt_final[] IS NOT INITIAL.
        ls_xml_base64 = lo_client->prep_xml_pack_inv( it_final = xt_final[] iv_action = lv_action im_prntval = lv_prntval im_pack = lv_pack_num ).
        rv_response   = lo_ads->get_ads_api_toget_base64(
        im_access_token  = lv_access_token
        im_template_name = 'ZSD_FORM_PACK/ZSD_BLV_PACK_LIST'
        im_xml_base64    = ls_xml_base64 ).
      ENDIF.

    ENDIF.

    IF lv_action = 'oeminv'.

      xt_final      = lo_client->get_billing_data( iv_vbeln = lv_vbeln  iv_action = lv_action ).

      IF xt_final[] IS NOT INITIAL.

        READ TABLE xt_final INTO DATA(w_final) INDEX 1 .

*        IF w_final-DistributionChannel = '30' .
*
*          ls_xml_base64 = lo_client->prep_xml_tax_inv( it_final = xt_final[] iv_action = lv_action im_prntval = lv_prntval ).
*          rv_response = lo_ads->get_ads_api_toget_base64(
*            im_access_token  = lv_access_token
*            im_template_name = 'ZSD_EXPORT_TAX_INVOICE/ZSD_SIPL_EXPORT_TAX_INVOICE'
*            im_xml_base64    = ls_xml_base64 ).
*
*        ELSE.

        ls_xml_base64 = lo_client->prep_xml_tax_inv( it_final = xt_final[] iv_action = lv_action im_prntval = lv_prntval ).
        rv_response = lo_ads->get_ads_api_toget_base64(
          im_access_token  = lv_access_token
          im_template_name = 'ZSD_FORM_INVOICE/ZSD_BLV_TAX_INVOICE'
          im_xml_base64    = ls_xml_base64 ).

*        ENDIF.

      ENDIF.

    ENDIF.

    IF lv_action = 'taxinv'.

      xt_final      = lo_client->get_billing_data( iv_vbeln = lv_vbeln  iv_action = lv_action ).

      IF xt_final[] IS NOT INITIAL.

        ls_xml_base64 = lo_client->prep_xml_tax_inv( it_final = xt_final[] iv_action = lv_action im_prntval = lv_prntval ).

        rv_response = lo_ads->get_ads_api_toget_base64(
          im_access_token  = lv_access_token
          im_template_name = 'ZSD_FORM_EXPORT_TAX_INV/ZSD_BLV_EXPORT_TAX_INVOICE'
          im_xml_base64    = ls_xml_base64 ).

      ENDIF.

    ENDIF.

    IF lv_action = 'dcnote'.

      xt_final      = lo_client->get_billing_data( iv_vbeln = lv_vbeln iv_action = lv_action ).

      IF xt_final[] IS NOT INITIAL.

        CLEAR: w_final.
        READ TABLE xt_final INTO w_final INDEX 1 .

        ls_xml_base64 = lo_client->prep_xml_tax_inv( it_final = xt_final[] iv_action = lv_action im_prntval = lv_prntval ).
        rv_response = lo_ads->get_ads_api_toget_base64(
          im_access_token  = lv_access_token
          im_template_name = 'ZSD_FORM_DEBIT_CREDIT/ZSD_BLV_DEBIT_CREDIT_NOTE'
          im_xml_base64    = ls_xml_base64 ).
      ENDIF.

*      clear : lv_vbeln .
*      clear : xt_final[].

    ENDIF.

    IF lv_action = 'aftinv'.

      xt_final      = lo_client->get_billing_data( iv_vbeln = lv_vbeln iv_action = lv_action ).

      IF xt_final[] IS NOT INITIAL.

        ls_xml_base64 = lo_client->prep_xml_tax_inv( it_final = xt_final[] iv_action = lv_action im_prntval = lv_prntval ).
        rv_response = lo_ads->get_ads_api_toget_base64(
          im_access_token  = lv_access_token
          im_template_name = 'ZSD_FORM_AFT_MKT/ZSD_BLV_AFT_MKT'
          im_xml_base64    = ls_xml_base64 ).

      ELSE.

        rv_response = 'No suitable data found' ##NO_TEXT.

      ENDIF.

    ENDIF.


    IF lv_action = 'dchlpr'.

      xt_final      = lo_client->get_billing_data( iv_vbeln = lv_vbeln  iv_action = lv_action ).

      IF xt_final[] IS NOT INITIAL.

        ls_xml_base64 = lo_client->prep_xml_tax_inv( it_final = xt_final[] iv_action = lv_action im_prntval = lv_prntval ).
        rv_response = lo_ads->get_ads_api_toget_base64(
          im_access_token  = lv_access_token
          im_template_name = 'ZSD_FORM_DEL_CHL/ZSD_BLV_DEL_CHALLAN'
          im_xml_base64    = ls_xml_base64 ).

      ENDIF.

    ENDIF.

    IF lv_action = 'shpinst'.

      xt_final      = lo_client->get_packing_data( im_pack = lv_pack_num  iv_action = lv_action ).

      IF xt_final[] IS NOT INITIAL.

        ls_xml_base64 = lo_client->prep_xml_pack_inv( it_final = xt_final[] iv_action = lv_action im_prntval = lv_prntval im_pack = lv_pack_num ).
        rv_response = lo_ads->get_ads_api_toget_base64(
          im_access_token  = lv_access_token
          im_template_name = 'ZSD_FORM_SHIP_INST/ZSD_BLV_SHIP_INST'
          im_xml_base64    = ls_xml_base64 ).

      ELSE.

        rv_response = 'No suitable data found' ##NO_TEXT.

      ENDIF.

    ENDIF.


    """***Bill of Supply
    IF lv_action = 'bsupply'.

      xt_final      = lo_client->get_billing_data( iv_vbeln = lv_vbeln  iv_action = lv_action ).

      IF xt_final[] IS NOT INITIAL.

        READ TABLE xt_final INTO w_final INDEX 1 .

        ls_xml_base64 = lo_client->prep_xml_tax_inv( it_final = xt_final[] iv_action = lv_action im_prntval = lv_prntval ).

        rv_response = lo_ads->get_ads_api_toget_base64(
          im_access_token  = lv_access_token
          im_template_name = 'ZSD_FORM_BSUPPLY/ZSD_BLV_BILLOF_SUPPLY'
          im_xml_base64    = ls_xml_base64 ).

      ENDIF.

    ENDIF.

    """***Shipping Invoice
    IF lv_action = 'shipinv'.

      xt_final      = lo_client->get_billing_data( iv_vbeln = lv_vbeln  iv_action = lv_action ).

      IF xt_final[] IS NOT INITIAL.

        ls_xml_base64 = lo_client->prep_xml_pack_inv( it_final = xt_final[] iv_action = lv_action im_prntval = lv_prntval im_pack = lv_pack_num ).

        rv_response = lo_ads->get_ads_api_toget_base64(
          im_access_token  = lv_access_token
          im_template_name = 'ZSD_FORM_SHIPPING_INV/ZSD_BLV_SHIPPING_INV'
          im_xml_base64    = ls_xml_base64 ).

      ENDIF.

    ENDIF.


    """"""""""""""""""Packing Advice"""


    IF lv_action = 'packadvice'.

      lv_pack_num = lv_vbeln.
      SHIFT lv_pack_num LEFT DELETING LEADING '0'.
      xt_final1      = lo_client->get_packing_advice_data( im_pack = lv_pack_num  iv_action = lv_action ).

      IF xt_final1[] IS NOT INITIAL.
        ls_xml_base64 = lo_client->prep_xml_pack_Adv( it_final1 = xt_final1[] iv_action = lv_action im_prntval = lv_prntval im_pack = lv_pack_num ).
        rv_response   = lo_ads->get_ads_api_toget_base64(
        im_access_token  = lv_access_token
        im_template_name = 'ZSD_PACKING_ADVICE_BLV/SD_PACKING_ADVICE_BLV    '
        im_xml_base64    = ls_xml_base64 ).
      ENDIF.

    ENDIF.

    """"""""""""""""""""""""

    ""**Setiing response/pdf in base64 format to UI5
    response->set_text(
      EXPORTING
        i_text = rv_response ).

  ENDMETHOD.
ENDCLASS.
