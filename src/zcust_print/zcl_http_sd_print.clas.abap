class ZCL_HTTP_SD_PRINT definition
  public
  create public .

public section.

    DATA:
      xt_final_so     TYPE TABLE OF zstr_so_data,
      xt_final_del    TYPE TABLE OF zstr_delivery_hdr,
      xt_quot         TYPE TABLE OF zstr_quotation_print,
      ls_xml_base64   TYPE string,
      lv_access_token TYPE string.

    DATA: lv_vbeln    TYPE c LENGTH 10,
          lv_vbeln_n  TYPE c LENGTH 10,
          lv_plant    TYPE c LENGTH 4,
          lv_date1    TYPE c LENGTH 10,
          lv_date     TYPE d,
          rv_response TYPE string,
          lv_action   TYPE c LENGTH 10,
          lv_action1  TYPE c LENGTH 10,
          lv_prntval  TYPE c LENGTH 10.

    DATA: lo_client TYPE REF TO ycl_sd_print,
          lo_ads    TYPE REF TO zcl_ads_service.

  interfaces IF_HTTP_SERVICE_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HTTP_SD_PRINT IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.
    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname'.
    IF sy-subrc EQ 0.
      lv_action = ls_action-value.
    ENDIF.

    clear: ls_action.
    READ TABLE lt_input INTO ls_action WITH KEY name = 'selectedradiovalue'.
    IF sy-subrc EQ 0.
      lv_action1 = ls_action-value.
    ENDIF.

    READ TABLE lt_input INTO DATA(ls_input_so) WITH KEY name = 'salesdocument'.
    IF sy-subrc EQ 0.
      lv_vbeln   = ls_input_so-value.
      lv_vbeln_n = lv_vbeln.
      lv_vbeln   = |{ lv_vbeln ALPHA = IN }| .
    ENDIF.

""***Start:Input for Delivery Print**************************************
    READ TABLE lt_input INTO DATA(ls_input) WITH KEY name = 'deliverydocument'.
    IF sy-subrc EQ 0.
      lv_vbeln   = ls_input-value.
      lv_vbeln_n = lv_vbeln.
      lv_vbeln   = |{ lv_vbeln ALPHA = IN }| .
    ENDIF.

    clear: ls_input.
    READ TABLE lt_input INTO ls_input WITH KEY name = 'deliverydate'.
    IF sy-subrc EQ 0.
      lv_date1 = ls_input-value.
      lv_date  = lv_date1+0(4) && lv_date1+5(2) && lv_date1+8(2).
    ENDIF.

    clear: ls_input.
    READ TABLE lt_input INTO ls_input WITH KEY name = 'plant'.
    IF sy-subrc EQ 0.
      lv_plant = ls_input-value.
    ENDIF.
""***End:Input for Delivery Print****************************************

    "********Creation of object**************
    CREATE OBJECT lo_client.
    CREATE OBJECT lo_ads.

    DATA:
      lv_resp_new TYPE string.

    ""*****Calling Methods to get PDF code base64
    lv_access_token = lo_ads->get_ads_access_token(  ).

    IF lv_action = 'soprnt'. "Sales order Print

      xt_final_so      = lo_client->get_sales_data( iv_vbeln = lv_vbeln  iv_action = lv_action ).

      IF xt_final_so[] IS NOT INITIAL.

        ls_xml_base64 = lo_client->prep_xml_so_prnt( it_final = xt_final_so[] iv_action = lv_action im_prntval = lv_prntval ).
        rv_response = lo_ads->get_ads_api_toget_base64(
          im_access_token  = lv_access_token
          im_template_name = 'ZSD_FORM_SO_PRINT/ZSD_BLV_SO_PRINT'
          im_xml_base64    = ls_xml_base64 ).

      ENDIF.

    ENDIF.

""****Start: Quotation Print*****************************************
    IF lv_action = 'quotprnt'. "Quotation Print

      xt_quot      = lo_client->get_quotation_data(
                           im_vbeln  = lv_vbeln
                           im_date   = lv_date
                           iv_action = lv_action
                         ).

        ls_xml_base64 = lo_client->prep_xml_quotation_prnt( it_final = xt_quot[] iv_action = lv_action ).

        rv_response = lo_ads->get_ads_api_toget_base64(
          im_access_token  = lv_access_token
          im_template_name = 'ZSD_FORM_QUOTATION_PRINT/ZSD_QUOTATION_PRINT'
          im_xml_base64    = ls_xml_base64 ).


    ENDIF.
""****End: Quotation Print*****************************************

""****Start: Delivery unit Print*****************************************
    IF lv_action = 'dlvryprnt' or lv_action = 'sdopass' . "Delivery Unit Print / SD Out Pass

      xt_final_del = lo_client->get_delivery_data(
                           im_vbeln  = lv_vbeln
                           im_plant  = lv_plant
                           im_date   = lv_date
                           iv_action = lv_action
                         ).

      IF xt_final_del[] IS NOT INITIAL.

        ls_xml_base64 = lo_client->prep_xml_delivery_prnt( it_final = xt_final_del[] iv_action = lv_action iv_action1 = lv_action1 ).

        if lv_action = 'dlvryprnt'.

        rv_response = lo_ads->get_ads_api_toget_base64(
          im_access_token  = lv_access_token
          im_template_name = 'ZSD_FORM_DEL_PRINT/ZSD_DEL_PRINT'
          im_xml_base64    = ls_xml_base64 ).

        ELSEIF lv_action = 'sdopass'.

        rv_response = lo_ads->get_ads_api_toget_base64(
          im_access_token  = lv_access_token
          im_template_name = 'ZSD_FORM_OUT_PASS/ZSD_OUT_PASS'
          im_xml_base64    = ls_xml_base64 ).

        ENDIF.

      ENDIF.

    ENDIF.
""****End: Delivery unit Print*****************************************

    ""**Setiing response/pdf in base64 format to UI5
    response->set_text(
      EXPORTING
        i_text = rv_response ).


  endmethod.
ENDCLASS.
