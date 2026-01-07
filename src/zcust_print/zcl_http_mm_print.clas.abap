CLASS zcl_http_mm_print DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      lv_rfqnum   TYPE zi_rfq_data-requestforquotation,
      lv_partcntr TYPE zi_rfq_data-partnercounter,
      lv_supplier TYPE zi_rfq_data-supplier,
      xt_rfq_data TYPE table of zstr_rfq_hdr.

    DATA: xt_final        TYPE TABLE OF zstr_schd_line_print,
          ls_xml_base64   TYPE string,
          lv_access_token TYPE string.

    DATA: lv_ebeln      TYPE c LENGTH 10,
          lv_date       TYPE c LENGTH 10,
          rv_response   TYPE string,
          rv_resp_signd TYPE string,
          lv_action     TYPE c LENGTH 10.

    DATA: lo_client TYPE REF TO ycl_mm_prog,
          lo_ads    TYPE REF TO zcl_ads_service.


    INTERFACES if_http_service_extension .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_MM_PRINT IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    ""**Get input Data
    DATA(lt_input) = request->get_form_fields( ).

    READ TABLE lt_input INTO DATA(ls_action) WITH KEY name = 'actionname'.
    IF sy-subrc EQ 0.
      lv_action = ls_action-value.
    ENDIF.

    IF lv_action = 'schdlprint'.

      READ TABLE lt_input INTO DATA(ls_input) WITH KEY name = 'schedulingagreement'.
      IF sy-subrc EQ 0.
        lv_ebeln = ls_input-value.
      ENDIF.
      lv_ebeln = |{ lv_ebeln ALPHA = IN }| .

      CLEAR: ls_input.
      READ TABLE lt_input INTO ls_input WITH KEY name = 'schedulingagreementdate'.
      IF sy-subrc EQ 0.
        lv_date = ls_input-value.
      ENDIF.

    ENDIF.

    IF lv_action = 'rfqprint'.

      CLEAR: ls_input.
      READ TABLE lt_input INTO ls_input WITH KEY name = 'requestforquotation'.
      IF sy-subrc EQ 0.
        lv_rfqnum = ls_input-value.
      ENDIF.
      lv_rfqnum = |{ lv_rfqnum ALPHA = IN }| .

      CLEAR: ls_input.
      READ TABLE lt_input INTO ls_input WITH KEY name = 'partnercounter'.
      IF sy-subrc EQ 0.
        lv_partcntr = ls_input-value.
      ENDIF.

      CLEAR: ls_input.
      READ TABLE lt_input INTO ls_input WITH KEY name = 'supplier'.
      IF sy-subrc EQ 0.
        lv_supplier = ls_input-value.
      ENDIF.
      lv_supplier = |{ lv_supplier ALPHA = IN }| .

    ENDIF.

    "********Creation of object**************
    CREATE OBJECT lo_client.
    CREATE OBJECT lo_ads.

    DATA:
      lv_resp_new TYPE string.

    ""*****Calling Methods to get PDF code base64
    lv_access_token = lo_ads->get_ads_access_token(  ).

    """**RFQ print
    IF lv_action = 'rfqprint'.

      lo_client->get_rfq_data(
        EXPORTING
          im_rfqnum   = lv_rfqnum
          im_partcntr = lv_partcntr
          im_supplier = lv_supplier
          im_action   = lv_action
        RECEIVING
          et_final    = xt_rfq_data
      ).

      ls_xml_base64 = lo_client->prep_xml_rfq_print( it_final = xt_rfq_data[] im_action = lv_action ).

      rv_response = lo_ads->get_ads_api_toget_base64(
      im_access_token  = lv_access_token
      im_template_name = 'ZMM_FORM_RFQ/ZMM_BLV_RFQ_PRINT'
      im_xml_base64    = ls_xml_base64 ).

    ENDIF.

    """**Schedule line print
    IF lv_action = 'schdlprint'.

      xt_final      = lo_client->get_sa_data( iv_ebeln = lv_ebeln iv_action = lv_action ).

      ls_xml_base64 = lo_client->prep_xml_schdl_print( it_final = xt_final[] iv_action = lv_action ).

      rv_response = lo_ads->get_ads_api_toget_base64(
      im_access_token  = lv_access_token
      im_template_name = 'ZMM_FORM_SCHDL/ZMM_BLV_SASL_PRINT'
      im_xml_base64    = ls_xml_base64 ).

    ENDIF.

    ""**Setiing response/pdf in base64 format to UI5
    response->set_text(
      EXPORTING
        i_text = rv_response ).

  ENDMETHOD.
ENDCLASS.
