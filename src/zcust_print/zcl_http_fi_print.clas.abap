CLASS zcl_http_fi_print DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA: xt_final        TYPE TABLE OF zstr_voucher_print,
          ls_xml_base64   TYPE string,
          lv_access_token TYPE string.
    DATA : ct_final TYPE TABLE OF zstr_chq_hdr.

    DATA: lv_belnr      TYPE c LENGTH 10,
          lv_gjahr      TYPE zstr_fi_debit_note-fiscalyear,
          lv_bukrs      TYPE c LENGTH 4,
          rv_response   TYPE string,
          rv_resp_signd TYPE string,
          lv_action     TYPE c LENGTH 10.
*
*    DATA: lo_client TYPE REF TO ycl_fi_print,
*          lo_ads    TYPE REF TO zcl_ads_service.
    DATA: lo_client TYPE REF TO ycl_fi_print_new,
          lo_ads    TYPE REF TO zcl_ads_service.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_FI_PRINT IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname'.
    IF sy-subrc EQ 0.
      lv_action = ls_action-value.
    ENDIF.

    READ TABLE lt_input INTO DATA(ls_input) WITH KEY name = 'accountingdocument'.
    IF sy-subrc EQ 0.
      lv_belnr = ls_input-value.
    ENDIF.
    lv_belnr = |{ lv_belnr ALPHA = IN }| .

    CLEAR: ls_input.
    READ TABLE lt_input INTO ls_input WITH KEY name = 'companycode'.
    IF sy-subrc EQ 0.
      lv_bukrs = ls_input-value.
    ENDIF.

    CLEAR: ls_input.
    READ TABLE lt_input INTO ls_input WITH KEY name = 'fiscalyear'.
    IF sy-subrc EQ 0.
      lv_gjahr = ls_input-value.
    ENDIF.

    CREATE OBJECT lo_client.
    CREATE OBJECT lo_ads.

    DATA:
      lv_resp_new TYPE string.

    ""*****Calling Methods to get PDF code base64
    lv_access_token = lo_ads->get_ads_access_token(  ).

    IF lv_action = 'scnprint'.

      lv_access_token = lo_ads->get_ads_access_token(  ).

      lo_client->get_scn_data(
        EXPORTING
          im_bukrs  = lv_bukrs
          im_belnr  = lv_belnr
          im_gjahr  = lv_gjahr
          im_action = lv_action
        RECEIVING
          et_final =  xt_final
      ).

      ls_xml_base64 = lo_client->prep_xml_scn_print( it_final = xt_final[] iv_action = lv_action ).

      rv_response = lo_ads->get_ads_api_toget_base64(
        im_access_token  = lv_access_token
        im_template_name = 'ZFI_FORM_SCN/ZFI_BLV_SCN_PRINT'
        im_xml_base64    = ls_xml_base64 ).

    ENDIF.


    IF lv_action = 'scnpkg'.

      lv_access_token = lo_ads->get_ads_access_token(  ).

      lo_client->get_scn_data(
        EXPORTING
          im_bukrs  = lv_bukrs
          im_belnr  = lv_belnr
          im_gjahr  = lv_gjahr
          im_action = lv_action
        RECEIVING
          et_final =  xt_final
      ).

      ls_xml_base64 = lo_client->prep_xml_scn_print( it_final = xt_final[] iv_action = lv_action ).

      rv_response = lo_ads->get_ads_api_toget_base64(
        im_access_token  = lv_access_token
        im_template_name = 'ZFI_SCN_BLV_PKG/ZFI_SCN_BLV_PKG'
        im_xml_base64    = ls_xml_base64 ).

    ENDIF.

    IF lv_action = 'vchprint'.

      lo_client->get_voucher_data(
      EXPORTING
        im_bukrs  = lv_bukrs
        im_belnr  = lv_belnr
        im_gjahr  = lv_gjahr
        im_action = lv_action
      RECEIVING
        et_final =  xt_final
    ).

      ls_xml_base64 = lo_client->prep_xml_voucher_print( it_final = xt_final[] iv_action = lv_action ).

      rv_response = lo_ads->get_ads_api_toget_base64(
      im_access_token  = lv_access_token
      im_template_name = 'ZFI_FORM_VOUCHER/ZFI_BLV_VOUCHER_PRINT'
      im_xml_base64    = ls_xml_base64 ).

    ENDIF.

    IF lv_action = 'nwvchprint'.

      lo_client->get_voucher_data(
      EXPORTING
        im_bukrs  = lv_bukrs
        im_belnr  = lv_belnr
        im_gjahr  = lv_gjahr
        im_action = lv_action
      RECEIVING
        et_final =  xt_final
    ).

      ls_xml_base64 = lo_client->prep_xml_voucher_print( it_final = xt_final[] iv_action = lv_action ).

      rv_response = lo_ads->get_ads_api_toget_base64(
      im_access_token  = lv_access_token
      im_template_name = 'ZFI_FORM_NEW_VCH/ZFI_BLV_NEW_VCH_PRINT'
      im_xml_base64    = ls_xml_base64 ).

    ENDIF.

    IF lv_action = 'checkslip'.

      lo_client->get_chqslip_data(
           EXPORTING
             im_bukrs  = lv_bukrs
             im_belnr  = lv_belnr
             im_gjahr  = lv_gjahr
             im_action = lv_action
           RECEIVING
             et_final =  ct_final
         ).

      ls_xml_base64 = lo_client->prep_xml_chqslip_print( it_final = ct_final[] iv_action = lv_action ).

      rv_response = lo_ads->get_ads_api_toget_base64(
      im_access_token  = lv_access_token
      im_template_name = 'ZFI_FORM_CHQ_SLIP/ZFI_BLV_CHQ_SLIP'
      im_xml_base64    = ls_xml_base64 ).

    ENDIF.

    ""**Setiing response/pdf in base64 format to UI5
    response->set_text(
      EXPORTING
        i_text = rv_response ).

  ENDMETHOD.
ENDCLASS.
