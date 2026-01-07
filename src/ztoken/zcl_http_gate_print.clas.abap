CLASS zcl_http_gate_print DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA: xt_final        TYPE TABLE OF zstr_unload_hdr,
          ls_xml_base64   TYPE string,
          lv_access_token TYPE string.

    DATA: lv_genum        TYPE c LENGTH 10,
          lv_geyear       TYPE c LENGTH 4,
          lv_currentvalue TYPE string,
          rv_response     TYPE string,
          lv_action       TYPE c LENGTH 10.

    DATA: lo_client TYPE REF TO ycl_gate_print,
          lo_ads    TYPE REF TO zcl_ads_service.

    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_GATE_PRINT IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname'.
    IF sy-subrc EQ 0.
      lv_action = ls_action-value.
    ENDIF.
    IF lv_action = 'unloadslip'.
      READ TABLE lt_input INTO DATA(ls_input) WITH KEY name = 'gentry_num'.
      IF sy-subrc EQ 0.
        lv_genum = ls_input-value.
        "lv_genum = |{ lv_genum ALPHA = IN }| .
      ENDIF.
    ELSEIF lv_action = 'weighslip'.
      READ TABLE lt_input INTO DATA(ls_input1) WITH KEY name = 'gentrynum'.
      IF sy-subrc EQ 0.
        lv_genum = ls_input1-value.
        "lv_genum = |{ lv_genum ALPHA = IN }| .
      ENDIF.
*====
      "BOC by neelam on 19.06.2025
    ELSEIF lv_action = 'gateoutsli'.
      READ TABLE lt_input INTO ls_input1 WITH KEY name = 'gentrynum'.
      IF sy-subrc EQ 0.
        lv_genum = ls_input1-value.
        "lv_genum = |{ lv_genum ALPHA = IN }| .
      ENDIF.
      "EOC by neelam on 19.06.2025

    ELSEIF lv_action = 'sdweighslp'.
      READ TABLE lt_input INTO ls_input1 WITH KEY name = 'gentrynum'.
      IF sy-subrc EQ 0.
        lv_genum = ls_input1-value.
        "lv_genum = |{ lv_genum ALPHA = IN }| .
      ENDIF.
*====
    ENDIF.

    CLEAR: ls_input.
    READ TABLE lt_input INTO ls_input WITH KEY name = 'erdat'.
    IF sy-subrc EQ 0.
      lv_geyear = ls_input-value.
      "lv_genum = |{ lv_genum ALPHA = IN }| .
    ENDIF.


    "********Creation of object**************
    CREATE OBJECT lo_client.
    CREATE OBJECT lo_ads.

    DATA:
      lv_resp_new TYPE string.

    ""*****Calling Methods to get PDF code base64
    lv_access_token = lo_ads->get_ads_access_token(  ).

    IF lv_action = 'unloadslip'.

      lo_client->get_gate_entry_data(
        EXPORTING
          im_genum  = lv_genum
          im_geyear = lv_geyear
          im_action = lv_action
        RECEIVING
          et_final  = xt_final
      ).

      ls_xml_base64 = lo_client->prep_xml_gate_entry_print( it_final = xt_final[] im_action = lv_action ).

      rv_response = lo_ads->get_ads_api_toget_base64(
        im_access_token  = lv_access_token
        im_template_name = 'ZMM_FORM_UNLOAD/ZMM_UNLOADING_PASS_PRINT'
        im_xml_base64    = ls_xml_base64 ).

    ENDIF.

    IF lv_action = 'weighslip'.

      lo_client->get_gate_entry_data(
        EXPORTING
          im_genum  = lv_genum
          im_geyear = lv_geyear
          im_action = lv_action
        RECEIVING
          et_final  = xt_final
      ).

      ls_xml_base64 = lo_client->prep_xml_gate_entry_print( it_final = xt_final[] im_action = lv_action ).

      rv_response = lo_ads->get_ads_api_toget_base64(
        im_access_token  = lv_access_token
        im_template_name = 'ZMM_FORM_WEIGHMENT/ZWEIGHMENT_SLIP_PRINTOUT'
        im_xml_base64    = ls_xml_base64 ).

    ENDIF.

    "BOC by neelam on 19.06.2025

    IF lv_action = 'gateoutsli'.

      lo_client->get_gate_entry_data(
        EXPORTING
          im_genum  = lv_genum
          im_geyear = lv_geyear
          im_action = lv_action
        RECEIVING
          et_final  = xt_final
      ).

      ls_xml_base64 = lo_client->prep_xml_gate_entry_print( it_final = xt_final[] im_action = lv_action ).

      rv_response = lo_ads->get_ads_api_toget_base64(
        im_access_token  = lv_access_token
        im_template_name = 'ZMM_FORM_WEIGHMENTOUT/ZWEIGHMENTOUT_SLIP_PRINTOUT'
        im_xml_base64    = ls_xml_base64 ).

    ENDIF.

    "EOC by neelam on 19.06.2025

    IF lv_action = 'sdweighslp'.

      lo_client->get_gate_out_data(
        EXPORTING
          im_genum  = lv_genum
          im_geyear = lv_geyear
          im_action = lv_action
        RECEIVING
          et_final  = xt_final
      ).

      ls_xml_base64 = lo_client->prep_xml_gate_entry_print( it_final = xt_final[] im_action = lv_action ).

      rv_response = lo_ads->get_ads_api_toget_base64(
        im_access_token  = lv_access_token
        im_template_name = 'ZSD_FORM_WEIGH_SLIP/ZSD_WEIGH_SLIP'        "ZMM_FORM_WEIGHMENT/ZWEIGHMENT_SLIP_PRINTOUT'
        im_xml_base64    = ls_xml_base64 ).

    ENDIF.

    ""**Setiing response/pdf in base64 format to UI5
    response->set_text(
      EXPORTING
        i_text = rv_response ).

  ENDMETHOD.
ENDCLASS.
