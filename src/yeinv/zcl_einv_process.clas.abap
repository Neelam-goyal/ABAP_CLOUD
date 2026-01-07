CLASS zcl_einv_process DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      gt_final  TYPE TABLE OF zstr_einv_data,
      gs_final  TYPE zstr_einv_data,
      lv_char10 TYPE c LENGTH 10,
      lv_char4  TYPE c LENGTH 4.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20.

    METHODS:
      fetch_billing_data
        IMPORTING
                  im_vbeln           LIKE lv_char10
                  im_bukrs           LIKE lv_char4
                  im_fyear           LIKE lv_char4
                  im_plant           LIKE lv_char4
                  im_date            LIKE lv_char10
                  iv_action          LIKE lv_char10
        RETURNING VALUE(r_einv_data) LIKE gt_final,

      prepare_einv_json_mi
        IMPORTING
                  im_vbeln           LIKE lv_char10
                  im_bukrs           LIKE lv_char4
                  im_fyear           LIKE lv_char4
                  im_plant           LIKE lv_char4
                  im_date            LIKE lv_char10
                  iv_action          LIKE lv_char10
                  im_access_token    TYPE string
        RETURNING VALUE(r_einv_json) TYPE string,

      get_token_from_mi
        IMPORTING
                  im_action             LIKE lv_char10
        RETURNING VALUE(r_access_token) TYPE string,

      gen_einv_from_mi
        IMPORTING
                  im_action         LIKE lv_char10
                  im_einv_json      TYPE string
        RETURNING VALUE(r_gen_einv) TYPE string,

      save_irn_data_mi
        IMPORTING
                  im_vbeln            LIKE lv_char10
                  im_bukrs            LIKE lv_char4
                  im_fyear            LIKE lv_char4
                  im_plant            LIKE lv_char4
                  im_date             LIKE lv_char10
                  im_einv_resp        TYPE string
        RETURNING VALUE(r_saved_resp) TYPE string,

      cancel_einv_irn_mi
        IMPORTING
                  im_vbeln           LIKE lv_char10
                  im_bukrs           LIKE lv_char4
                  im_fyear           LIKE lv_char4
                  im_plant           LIKE lv_char4
                  im_date            LIKE lv_char10
                  iv_action          LIKE lv_char10
                  im_access_token    TYPE string
        RETURNING VALUE(r_einv_canc) TYPE string.

*    INTERFACES if_oo_adt_classrun .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_EINV_PROCESS IMPLEMENTATION.


  METHOD cancel_einv_irn_mi.

    TYPES: BEGIN OF gty_irn,
             irn        TYPE string,
             CancelDate TYPE string,
           END OF gty_irn.

    TYPES: BEGIN OF gty_msg_data,
             message      TYPE gty_irn,
             errormessage TYPE string,
             infodtls     TYPE string,
             status       TYPE string,
             code         TYPE string,
             requestid    TYPE string,
           END OF gty_msg_data.

    TYPES: BEGIN OF gty_resp,
             results TYPE gty_msg_data,
           END OF gty_resp.

    DATA:
      ls_resp TYPE gty_resp.

    ""***Start: Data Preparation***************************
    DATA:
      cw_string TYPE string.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT SINGLE * FROM zsd_einv_data
    WHERE billingdocument = @im_vbeln AND
          companycode     = @im_bukrs  AND
          fiscalyear      = @im_fyear AND
*          plant           = @im_plant AND
          cancel_status   = ''
          INTO @DATA(cs_einv) . "#EC CI_NOORDER "#EC CI_ALL_FIELDS_NEEDED

    CLEAR: cw_string.
    cw_string = '{'
    && '"access_token":' && '"' && im_access_token && '",'
    && '"user_gstin":'   && '"' && cs_einv-plant_gstin && '",'
    && '"irn":'          && '"' && cs_einv-irn && '",'
    && '"cancel_reason":"1",'
    && '"cancel_remarks":"cancel Remarks",'
    && '"ewaybill_cancel":"0"'
    && '}'.

    ""***End: Data Preparation***************************

    """"Start:Cancellation:*****************************************************************************************
    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string.

    url = 'https://clientbasic.mastersindia.co/cancelEinvoice'.

    TRY.
        DATA(dest1) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest1 ).

        DATA(lo_request1) = lo_http_client->get_http_request( ).
        lo_request1->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        CLEAR: miw_string.
        miw_string = cw_string.

        lo_request1->append_text(
          EXPORTING
            data = miw_string
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'Host' ##NO_TEXT
            i_value = url
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response1) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        DATA(response_body1) = lo_response1->get_text( ).
        r_einv_canc = response_body1.

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.
    """"End:Cancellation**********************************************************************

    """**Start: Update table for cancelled document*******************************************
    CLEAR: miw_string.
    miw_string = r_einv_canc.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = miw_string
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_resp
    ).

    DATA(ls_res) = ls_resp-results.

    IF ls_res-code = '200' AND ls_res-status = 'Success'.

      cs_einv-cancel_date   = sys_date.
      cs_einv-cancel_status = abap_true.
      cs_einv-cancel_time   = sys_time.
      MODIFY zsd_einv_data FROM @cs_einv.
      COMMIT WORK.

    ENDIF.
    """**End: Update table for cancelled document*********************************************

  ENDMETHOD.


  METHOD fetch_billing_data.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-FiscalYear, "zi_dc_note-FiscalYear,
          lv_doc_month(2) TYPE n.

    DATA:
      r_vbeln  TYPE RANGE OF zi_sale_reg-BillingDocument,
      r_date   TYPE RANGE OF zi_sale_reg-BillingDocumentDate,
      r_plant  TYPE RANGE OF zi_sale_reg-Plant,
      wr_vbeln LIKE LINE OF r_vbeln,
      wr_date  LIKE LINE OF r_date,
      wr_plant LIKE LINE OF r_plant.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    lv_doc_date  = sys_date.
    lv_doc_month = lv_doc_date+4(2).
    lv_fis_year  = lv_doc_date+0(4).

    IF lv_doc_month LT 4.
      lv_fis_year = lv_fis_year - 1.
    ENDIF.

    IF im_vbeln IS NOT INITIAL.
      wr_vbeln-low    = im_vbeln.
      wr_vbeln-high   = im_vbeln.
      wr_vbeln-sign   = 'I'.
      wr_vbeln-option = 'BT'.
      APPEND wr_vbeln TO r_vbeln.
    ENDIF.

    IF im_date IS NOT INITIAL.
      wr_date-low    = im_date.
      wr_date-high   = im_date.
      wr_date-sign   = 'I'.
      wr_date-option = 'BT'.
      APPEND wr_date TO r_date.
    ENDIF.

    IF im_plant IS NOT INITIAL.
      wr_plant-low    = im_plant.
      wr_plant-high   = im_plant.
      wr_plant-sign   = 'I'.
      wr_plant-option = 'BT'.
      APPEND wr_plant TO r_plant.
    ENDIF.

    ""*****Start:Dummy Data************************

    gs_final-BillingDocument  = '2311000603'.
    gs_final-CompanyCode      = '1001'.
    gs_final-fiscalyear       = '2024'.
    gs_final-docdate          = 20240529.
    gs_final-plant            = '4001'.
    gs_final-plant_gstin      = '06AABCE7582R1ZX'.

    SELECT SINGLE
    irn,
    ackno,
    ackdt,
    ewbno,
    ewbdt
    FROM zsd_einv_data
    WHERE billingdocument = @gs_final-BillingDocument AND
          companycode     = @gs_final-CompanyCode  AND
          fiscalyear      = @gs_final-fiscalyear AND
          cancel_status   = ''
          INTO @DATA(vs_einv) .   "#EC WARNOK "#EC CI_ALL_FIELDS_NEEDED

    IF sy-subrc EQ 0.
      gs_final-status     = 'generated'.
      gs_final-irn        = vs_einv-irn.
      gs_final-ackno      = vs_einv-ackno.
      gs_final-ackdt      = vs_einv-ackdt.
      gs_final-ewbno      = vs_einv-ewbno.
      gs_final-ewbdt      = vs_einv-ewbdt.
    ENDIF.
    APPEND gs_final TO gt_final.


    gs_final-BillingDocument  = '2311000604'.
    CLEAR: vs_einv.
    CLEAR: gs_final-status, gs_final-irn, gs_final-ackno, gs_final-ackdt, gs_final-ewbno, gs_final-ewbdt.
    SELECT SINGLE
    irn,
    ackno,
    ackdt,
    ewbno,
    ewbdt
    FROM zsd_einv_data
    WHERE billingdocument = @gs_final-BillingDocument AND
          companycode     = @gs_final-CompanyCode  AND
          fiscalyear      = @gs_final-fiscalyear AND
          cancel_status   = ''
          INTO @vs_einv. "#EC WARNOK                   "#EC CI_ALL_FIELDS_NEEDED

    IF sy-subrc EQ 0.
      gs_final-status     = 'generated'.
      gs_final-irn        = vs_einv-irn.
      gs_final-ackno      = vs_einv-ackno.
      gs_final-ackdt      = vs_einv-ackdt.
      gs_final-ewbno      = vs_einv-ewbno.
      gs_final-ewbdt      = vs_einv-ewbdt.
    ENDIF.
    APPEND gs_final TO gt_final.

    gs_final-BillingDocument  = '2311000605'.
    CLEAR: vs_einv.
    CLEAR: gs_final-status, gs_final-irn, gs_final-ackno, gs_final-ackdt, gs_final-ewbno, gs_final-ewbdt.
    SELECT SINGLE
    irn,
    ackno,
    ackdt,
    ewbno,
    ewbdt
    FROM zsd_einv_data
    WHERE billingdocument = @gs_final-BillingDocument AND
          companycode     = @gs_final-CompanyCode  AND
          fiscalyear      = @gs_final-fiscalyear AND
          cancel_status   = ''
          INTO @vs_einv. "#EC WARNOK                    "#EC CI_ALL_FIELDS_NEEDED

    IF sy-subrc EQ 0.
      gs_final-status     = 'generated'.
      gs_final-irn        = vs_einv-irn.
      gs_final-ackno      = vs_einv-ackno.
      gs_final-ackdt      = vs_einv-ackdt.
      gs_final-ewbno      = vs_einv-ewbno.
      gs_final-ewbdt      = vs_einv-ewbdt.
    ENDIF.
    APPEND gs_final TO gt_final.

    gs_final-BillingDocument  = '2311000606'.
    CLEAR: vs_einv.
    CLEAR: gs_final-status, gs_final-irn, gs_final-ackno, gs_final-ackdt, gs_final-ewbno, gs_final-ewbdt.
    SELECT SINGLE
    irn,
    ackno,
    ackdt,
    ewbno,
    ewbdt
    FROM zsd_einv_data
    WHERE billingdocument = @gs_final-BillingDocument AND
          companycode     = @gs_final-CompanyCode  AND
          fiscalyear      = @gs_final-fiscalyear AND
          cancel_status   = ''
          INTO @vs_einv.          "#EC WARNOK "#EC CI_ALL_FIELDS_NEEDED

    IF sy-subrc EQ 0.
      gs_final-status     = 'generated'.
      gs_final-irn        = vs_einv-irn.
      gs_final-ackno      = vs_einv-ackno.
      gs_final-ackdt      = vs_einv-ackdt.
      gs_final-ewbno      = vs_einv-ewbno.
      gs_final-ewbdt      = vs_einv-ewbdt.
    ENDIF.
    APPEND gs_final TO gt_final.

    gs_final-BillingDocument  = '2311000607'.
    CLEAR: vs_einv.
    CLEAR: gs_final-status, gs_final-irn, gs_final-ackno, gs_final-ackdt, gs_final-ewbno, gs_final-ewbdt.
    SELECT SINGLE
    irn,
    ackno,
    ackdt,
    ewbno,
    ewbdt
    FROM zsd_einv_data
    WHERE billingdocument = @gs_final-BillingDocument AND
          companycode     = @gs_final-CompanyCode  AND
          fiscalyear      = @gs_final-fiscalyear AND
          cancel_status   = ''
          INTO @vs_einv. "#EC WARNOK                   "#EC CI_ALL_FIELDS_NEEDED

    IF sy-subrc EQ 0.
      gs_final-status     = 'generated'.
      gs_final-irn        = vs_einv-irn.
      gs_final-ackno      = vs_einv-ackno.
      gs_final-ackdt      = vs_einv-ackdt.
      gs_final-ewbno      = vs_einv-ewbno.
      gs_final-ewbdt      = vs_einv-ewbdt.
    ENDIF.
    APPEND gs_final TO gt_final.


    r_einv_data[] = gt_final[].
    ""*****End:Dummy Data************************

    SELECT * FROM zi_sale_reg
    WHERE BillingDocument     IN @r_vbeln AND
          BillingDocumentDate IN @r_date  AND
          Plant               IN @r_plant AND
          BillingDocumentIsCancelled = ''
    INTO TABLE @DATA(it_final) .              "#EC CI_ALL_FIELDS_NEEDED

    IF it_final IS NOT INITIAL.

      SELECT
  billingdocument ,
  companycode     ,
  fiscalyear      ,
  plant_gstin     ,
  billingtype         ,
  docdate             ,
  plant               ,
  status              ,
  businessmodule      ,
  irn                 ,
  ackno               ,
  ackdt               ,
  ewayackno           ,
  ewayackdt           ,
  ewbno               ,
  ewbdt               ,
  signedinvoice       ,
  signedqrcode        ,
  ewbvalidtill        ,
  qrcodeurl           ,
  erdat               ,
  uname               ,
  uzeit               ,
  cancel_status       ,
  cancel_date         ,
  cancel_time         ,
  canc_status_eway    ,
  cancel_date_eway    ,
  cancel_time_eway    ,
  transporterid       ,
  modeoftransp        ,
  transpdocnum        ,
  transdocdate        ,
  vehiclenum          ,
  vehicletype         ,
  transpname          ,
  datasource          ,
  distance

      FROM zsd_einv_data
      FOR ALL ENTRIES IN @it_final
      WHERE billingdocument   = @it_final-BillingDocument AND
            companycode   = @it_final-CompanyCode  AND
            fiscalyear = @lv_fis_year AND
            plant   = @it_final-Plant AND
            cancel_status   = ''
      INTO TABLE @DATA(it_einv) ##SELECT_FAE_WITH_LOB[SIGNEDINVOICE] ##SELECT_FAE_WITH_LOB[SIGNEDQRCODE]. "#EC CI_NO_TRANSFORM

      LOOP AT it_final INTO DATA(ls_final).

        gs_final-BillingDocument  = ls_final-BillingDocument.
        gs_final-CompanyCode      = ls_final-CompanyCode.
        gs_final-fiscalyear       = lv_fis_year.
        gs_final-docdate          = ls_final-BillingDocumentDate.
        gs_final-plant            = ls_final-Plant.
        gs_final-plant_gstin      = ls_final-plant_gstin.

        READ TABLE it_einv INTO DATA(ls_einv) WITH KEY
                           BillingDocument   = ls_final-BillingDocument
                           CompanyCode       = ls_final-CompanyCode
                           fiscalyear        = lv_fis_year.
        IF sy-subrc EQ 0.
          gs_final-status     = ''.
          gs_final-irn        = ls_einv-irn.
          gs_final-ackno      = ls_einv-ackno.
          gs_final-ackdt      = ls_einv-ackdt.
          gs_final-ewbno      = ls_einv-ewbno.
          gs_final-ewbdt      = ls_einv-ewbdt.
        ENDIF.

        APPEND gs_final TO gt_final.

        CLEAR: ls_final, ls_einv.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD gen_einv_from_mi.

    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string.

    """"Generation:*****************************************************************************************
    url = 'https://clientbasic.mastersindia.co/generateEinvoice'.

    TRY.
        DATA(dest1) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest1 ).

        DATA(lo_request1) = lo_http_client->get_http_request( ).
        lo_request1->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        CLEAR: miw_string.
        miw_string = im_einv_json.

        lo_request1->append_text(
          EXPORTING
            data = miw_string
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'Host' ##NO_TEXT
            i_value = url
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response1) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        DATA(response_body1) = lo_response1->get_text( ).
        r_gen_einv = response_body1.
        """"*****************************************************************************************

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.


  METHOD get_token_from_mi.

    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string.

    DATA: mi_uname     TYPE string,
          mi_pass      TYPE string,
          mi_clnt      TYPE string,
          mi_scrt      TYPE string,
          mi_grnt      TYPE string,
          access_token TYPE string.

    url = 'https://clientbasic.mastersindia.co/oauth/access_token'.

    TRY.
        DATA(dest11) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest11 ).

        DATA(lo_request11) = lo_http_client->get_http_request( ).
        lo_request11->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        CLEAR: mi_uname, mi_pass, mi_clnt, mi_scrt, mi_grnt.
        mi_uname  = '"testeway@mastersindia.co",'.
        mi_pass   = '"Client!@#Demo987",'.
        mi_clnt   = '"TMDIIbTwzkCQWFTHpA",'.
        mi_scrt   = '"BZpqtmFRIkfIrgjOfhyzQjLX",'.
        mi_grnt   = '"password"'.

        CLEAR: miw_string.
        miw_string = '{'
        && '"username":' && mi_uname
        && '"password":' && mi_pass
        && '"client_id":' && mi_clnt
        && '"client_secret":' && mi_scrt
        && '"grant_type":' && mi_grnt
        && '}'.

        lo_request11->append_text(
          EXPORTING
            data = miw_string
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'Host' ##NO_TEXT
            i_value = url
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response11) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        DATA(response_body11) = lo_response11->get_text( ).

        DATA: at1 TYPE string,at2 TYPE string,at3 TYPE string,at4 TYPE string.
        CLEAR: at1,at2.
        SPLIT response_body11 AT ':' INTO at1 at2.
        SPLIT at2 AT ',' INTO at3 at4.
        REPLACE ALL OCCURRENCES OF '"' IN at3 WITH space.
        CLEAR: access_token.
        access_token   = at3.
        r_access_token = access_token.

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.


  METHOD prepare_einv_json_mi.

    DATA:
      miw_string        TYPE string,
      miw_einv_json_hdr TYPE string,
      miw_einv_json_itm TYPE string.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-FiscalYear,
          lv_doc_month(2) TYPE n.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    lv_doc_date  = sys_date.
    lv_doc_month = lv_doc_date+4(2).
    lv_fis_year  = lv_doc_date+0(4).

    IF lv_doc_month LT 4.
      lv_fis_year = lv_fis_year - 1.
    ENDIF.

    SELECT * FROM zi_sale_reg
    WHERE BillingDocument     = @im_vbeln AND
          BillingDocumentDate = @im_date  AND
          Plant               = @im_plant AND
          CompanyCode         = @im_bukrs AND
          BillingDocumentIsCancelled = ''
    INTO TABLE @DATA(it_final) .              "#EC CI_ALL_FIELDS_NEEDED

*    IF it_final IS NOT INITIAL.
*
*      SELECT * FROM zsd_einv_data
*      FOR ALL ENTRIES IN @it_final
*      WHERE vbeln   = @it_final-BillingDocument AND
*            bukrs   = @it_final-CompanyCode AND
*            fisyear = @lv_fis_year AND
*            plant   = @it_final-Plant
*      INTO TABLE @DATA(it_einv) .             "#EC CI_ALL_FIELDS_NEEDED
*
*   endif.

    DATA:
      lv_seller_addrs1 TYPE string,
      lv_seller_addrs2 TYPE string,
      lv_buyer_addrs1  TYPE string,
      lv_buyer_addrs2  TYPE string,
      lv_shipto_addrs1 TYPE string,
      lv_shipto_addrs2 TYPE string,
      lv_buyer_gstin   TYPE c LENGTH 20,
      lv_shipto_gstin  TYPE c LENGTH 20,
      itm_count        TYPE sy-tabix.

    DATA:
      lv_val_total_assess       TYPE c LENGTH 40,
      lv_val_total_cgst         TYPE c LENGTH 40,
      lv_val_total_sgst         TYPE c LENGTH 40,
      lv_val_total_igst         TYPE c LENGTH 40,
      lv_val_total_cess         TYPE c LENGTH 40,
      lv_val_total_cess_non     TYPE c LENGTH 40,
      lv_val_other_charge       TYPE c LENGTH 40,
      lv_val_total_invoice      TYPE c LENGTH 40,
      lv_val_total_cess_v_state TYPE c LENGTH 40,
      lv_val_dt_roundoff        TYPE c LENGTH 40,
      lv_val_dt_tot_inv_ac      TYPE c LENGTH 40,
      lv_gst_rate               TYPE c LENGTH 40.

    READ TABLE it_final INTO DATA(ls_final_hdr) INDEX 1. "#EC CI_NOORDER
    DATA(line_itm) = lines( it_final[] ).

    ""**Main Header detail
    ls_final_hdr-plant_gstin = ''.
    ls_final_hdr-supply_type = ''.
    ls_final_hdr-doc_type    = ''.
    ls_final_hdr-BillingDocument     = ''.
    "*ls_final_hdr-BillingDocumentDate = ''.

    "*Seller Detail
    ls_final_hdr-plant_gstin    = ''.
    ls_final_hdr-PlantName      = ''.
    ls_final_hdr-CityName       = ''.
    ls_final_hdr-PostalCode     = ''.
    ls_final_hdr-Region         = ''.
    ls_final_hdr-plant_email    = ''.
    ls_final_hdr-PhoneAreaCodeSubscriberNumber = ''.
    lv_seller_addrs1 = ls_final_hdr-StreetName.
    lv_seller_addrs2 = ''.

    "*Buyer/sold to Detail
    lv_buyer_gstin = ''.
    ls_final_hdr-ag_name    = ''.
    ls_final_hdr-ag_city    = ''.
    ls_final_hdr-ag_pin     = ''.
    ls_final_hdr-ag_region  = ''.
    ls_final_hdr-ag_phone4  = ''.
    ls_final_hdr-ag_email   = ''.
    lv_buyer_addrs1         = ''.
    lv_buyer_addrs2         = ''.

    "*shipto to Detail
    lv_shipto_gstin  = ''.
    lv_shipto_addrs1 = ''.
    lv_shipto_addrs1 = ''.

***    CLEAR: miw_string, miw_einv_json_hdr, miw_einv_json_itm.
***    miw_einv_json_hdr = '{'
***        && '"access_token":' && '"' && im_access_token && '",'
***        && '"user_gstin":' && '"' && ls_final_hdr-plant_gstin_new && '",'
***        && '"data_source": "erp",'
***        && '"transaction_details": {'
***            && '"supply_type":' && '"' && ls_final_hdr-supply_type && '",'
***            && '"charge_type": "N",'
***            && '"ecommerce_gstin": ""'
***        && '},'
***        && '"document_details": {'
***            && '"document_type":'   && '"' && ls_final_hdr-doc_type && '",'
***            && '"document_number":' && '"' && ls_final_hdr-BillingDocument && '",'
***            && '"document_date":'   && '"' && ls_final_hdr-BillingDocumentDate && '",'
***        && '},'
***        && '"seller_details": {'
***            && '"gstin":' && '"' && ls_final_hdr-plant_gstin_new && '",'
***            && '"legal_name":' && '"' && ls_final_hdr-PlantName && '",'
***            && '"trade_name":' && '"' && ls_final_hdr-PlantName && '",'
***            && '"address1":'   && '"' && lv_seller_addrs1 && '",'
***            && '"address2":'   && '"' && lv_seller_addrs2 && '",'
***            && '"location":'   && '"' && ls_final_hdr-CityName && '",'
***            && '"pincode":'    && '"' && ls_final_hdr-PostalCode && '",'
***            && '"state_code":' && '"' && ls_final_hdr-Region && '",'
***            && '"phone_number":' && '"' && ls_final_hdr-PhoneAreaCodeSubscriberNumber && '",'
***            && '"email":'     && '"' && ls_final_hdr-plant_email && '"'
***        && '},'
***        && '"buyer_details": {'
***            && '"gstin":'           && '"' && lv_buyer_gstin && '",'
***            && '"legal_name":'      && '"' && ls_final_hdr-ag_name && '",'
***            && '"trade_name":'      && '"' && ls_final_hdr-ag_name && '",'
***            && '"address1":'        && '"' && lv_buyer_addrs1 && '",'
***            && '"address2":'        && '"' && lv_buyer_addrs2 && '",'
***            && '"location":'        && '"' && ls_final_hdr-ag_city && '",'
***            && '"pincode":'         && '"' && ls_final_hdr-ag_pin && '",'
***            && '"place_of_supply":' && '"' && ls_final_hdr-ag_region && '",'
***            && '"state_code":'      && '"' && ls_final_hdr-ag_region && '",'
***            && '"phone_number":'    && '"' && ls_final_hdr-ag_phone4 && '",'
***            && '"email":'           && '"' && ls_final_hdr-ag_email && '"'
***        && '},'.
***
******"SOC for ship details changes
****    IF wa_data-tran_typ EQ 'SHP'.
***    miw_einv_json_hdr = miw_einv_json_hdr
***   &&  '"ship_details":'
***   &&  '{'
***   &&  '"gstin":'           && '"' && lv_shipto_gstin && '",'
***   &&  '"legal_name":'      && '"' && ls_final_hdr-we_name && '",'
***   &&  '"trade_name":'      && '"' && ls_final_hdr-we_name && '",'
***   &&  '"address1":'        && '"' && lv_shipto_addrs1 && '",'
***   &&  '"address2":'        && '"' && lv_shipto_addrs2 && '",'
***   &&  '"location":'        && '"' && ls_final_hdr-we_city && '",'
***   &&  '"pincode":'         && '"' && ls_final_hdr-we_pin && '",'
***   &&  '"state_code":'      && '"' && ls_final_hdr-we_region && '"'
***   &&  '},'.
****    ENDIF.
******"EOC for ship details changes
***
***    miw_einv_json_hdr = miw_einv_json_hdr
***        && '"value_details": {'
***        && '"total_assessable_value":'    && lv_val_total_assess  && ','
***        && '"total_cgst_value":'          && lv_val_total_cgst  && ','
***        && '"total_sgst_value":'          && lv_val_total_sgst  && ','
***        && '"total_igst_value":'          && lv_val_total_igst  && ','
***        && '"total_cess_value":'          && lv_val_total_cess  && ','
***        && '"total_cess_nonadvol_value":' && lv_val_total_cess_non  && ','
***        && '"total_other_charge":'        && lv_val_other_charge  && ','
***        && '"total_invoice_value":'       && lv_val_total_invoice && ','
***        && '"total_cess_value_of_state":' && lv_val_total_cess_v_state  && ','
***        && '"round_off_amount":'          && lv_val_dt_roundoff  && ','
***        && '"total_invoice_value_additional_currency":' && lv_val_dt_tot_inv_ac  && ','
***        && '},'
***        && '"item_list": ['
***        && '{'.
***
***    CLEAR: itm_count.
***    LOOP AT it_final INTO DATA(ls_itm).
***      itm_count = itm_count + 1.
***
***      IF ls_itm-item_cgstrate IS NOT INITIAL.
***        lv_gst_rate = ls_itm-item_sgstrate.
***      ELSEIF ls_itm-item_igstrate IS NOT INITIAL.
***        lv_gst_rate = ls_itm-item_igstrate.
***      ENDIF.
***
***
***
***      miw_einv_json_itm = miw_einv_json_itm
***      && '"item_serial_number":'    && '"' && ls_itm-BillingDocumentItem && '",'
***      && '"product_description":'   && '"' && ls_itm-BillingDocumentItemText && '",'
***      && '"is_service": "N",'
***      && '"hsn_code":'              && '"' && ls_itm-hsn_code && '",'
***      && '"bar_code": "123497",'
***      && '"quantity":'              && ls_itm-BillingQuantity && ','
***      && '"free_quantity": 0,'
***      && '"unit":'                  && '"' && ls_itm-BaseUnit && '",'
***      && '"unit_price":'            && ls_itm-item_unitprice && ','
***      && '"total_amount":'          && ls_itm-item_totalamount_inr && ','
***      && '"pre_tax_value": 0,'
***      && '"discount": 0,'
***      && '"other_charge": 0,'
***      && '"assessable_value":'  && ls_itm-item_assessableamount_inr && ','
***      && '"gst_rate":'          && lv_gst_rate && ','
***      && '"igst_amount":'       && ls_itm-item_igstamount_inr && ','
***      && '"cgst_amount":'       && ls_itm-item_cgstamount_inr && ','
***      && '"sgst_amount":'       && ls_itm-item_sgstamount_inr && ','
***      && '"cess_rate": 0,'
***      && '"cess_amount": 0,'
***      && '"cess_nonadvol_value": 0,'
***      && '"state_cess_rate": 0,'
***      && '"state_cess_amount": 0,'
***      && '"state_cess_nonadvol_amount": 0,'
***      && '"total_item_value":'             && ls_itm-item_grandtotalamount_inr && ','
***      && '"country_origin": "IN",'
***      && '"order_line_reference":'         && '"' && ls_itm-BillingDocumentItem && '",'
***      && '"product_serial_number":'        && '"' && ls_itm-BillingDocumentItem && '",'
***
***      && '"batch_details": {'
***          && '"name": "TEST123",'
***          && '"expiry_date": "31/12/2021",'
***          && '"warranty_date": "31/12/2021"'
***      && '},'
***
***      && '"attribute_details": ['
***          && '{'
***              && '"item_attribute_details": "ATTR-1",'
***              && '"item_attribute_value": "Attribute Value"'
***          && '}'
***      && ']'.
***
***      IF line_itm EQ itm_count.
***        miw_einv_json_itm = miw_einv_json_itm &&  '}'.
***      ELSE.
***        miw_einv_json_itm = miw_einv_json_itm &&  '},{'.
***      ENDIF.
***
***      CLEAR: ls_itm.
***    ENDLOOP.
***
***    miw_string = miw_einv_json_hdr
***      && miw_einv_json_itm
***      &&  ']}'.
***
***    r_einv_json =  miw_string.

    miw_string = '{'
        && '"access_token":' && '"' && im_access_token && '",'
        && '"user_gstin": "06AABCE7582R1ZX",'
        && '"data_source": "erp",'
        && '"transaction_details": {'
            && '"supply_type": "B2B",'
            && '"charge_type": "N",'
            && '"ecommerce_gstin": ""'
        && '},'
        && '"document_details": {'
            && '"document_type": "INV",'
            && '"document_number":' && '"' && im_vbeln && '",'
            && '"document_date": "2024-05-30"'
        && '},'
        && '"seller_details": {'
            && '"gstin": "06AABCE7582R1ZX",'
            && '"legal_name": "VVDN - CP7Manesar",'
            && '"trade_name": "VVDN - CP7Manesar",'
            && '"address1": "Plot No: CP-07 Sector-8IMT Manesar",'
            && '"address2": "",'
            && '"location": "Gurugram",'
            && '"pincode": "122050",'
            && '"state_code": "06",'
            && '"phone_number": "",'
            && '"email": "procurement@vvdntech.in"'
        && '},'
        && '"buyer_details": {'
            && '"gstin": "06AAECS8719B2ZF",'
            && '"legal_name": "NItika Technology pvt.ltd",'
            && '"trade_name": "NItika Technology pvt.ltd",'
            && '"address1": "PLOT NUMBER CP7 SEC -8IMT MANESAR GURUGRAM",'
            && '"address2": "PLOT NUMBER CP7 SEC -8",'
            && '"location": "GURUGRAM",'
            && '"pincode": "122050",'
            && '"place_of_supply": "06",'
            && '"state_code": "06",'
            && '"phone_number": "",'
            && '"email": "nitin.fi@mawaimail.com"'
        && '},'
        && '"value_details": {'
            && '"total_assessable_value": 1000,'
            && '"total_cgst_value": 90,'
            && '"total_sgst_value": 90,'
            && '"total_igst_value": 0,'
            && '"total_cess_value": 0,'
            && '"total_cess_nonadvol_value": 0,'
            && '"total_other_charge": 0,'
            && '"total_invoice_value": 1180,'
            && '"total_cess_value_of_state": 0,'
            && '"round_off_amount": 0,'
            && '"total_invoice_value_additional_currency": 0'
        && '},'
        && '"item_list": ['
            && '{'
                && '"item_serial_number": "000010",'
                && '"product_description": "ANKIT ",'
                && '"is_service": "N",'
                && '"hsn_code": "85122090",'
                && '"bar_code": "123497",'
                && '"quantity": 1,'
                && '"free_quantity": 0,'
                && '"unit": "OTH",'
                && '"unit_price": 1000,'
                && '"total_amount": 1000,'
                && '"pre_tax_value": 0,'
                && '"discount": 0,'
                && '"other_charge": 0,'
                && '"assessable_value": 1000,'
                && '"gst_rate": 18,'
                && '"igst_amount": 0,'
                && '"cgst_amount": 90,'
                && '"sgst_amount": 90,'
                && '"cess_rate": 0,'
                && '"cess_amount": 0,'
                && '"cess_nonadvol_value": 0,'
                && '"state_cess_rate": 0,'
                && '"state_cess_amount": 0,'
                && '"state_cess_nonadvol_amount": 0,'
                && '"total_item_value": 1180,'
                && '"country_origin": "IN",'
                && '"order_line_reference": "000010",'
                && '"product_serial_number": "000010",'
                && '"batch_details": {'
                    && '"name": "TEST123",'
                    && '"expiry_date": "31/12/2021",'
                    && '"warranty_date": "31/12/2021"'
                && '},'
                && '"attribute_details": ['
                    && '{'
                        && '"item_attribute_details": "ATTR-1",'
                        && '"item_attribute_value": "Attribute Value"'
                    && '}'
                && ']'
            && '}'
        && ']'
    && '}'.

    r_einv_json =  miw_string.

  ENDMETHOD.


  METHOD save_irn_data_mi.

    DATA:
      miw_string TYPE string.

    TYPES: BEGIN OF gty_irn,
             ackno         TYPE string,
             ackdt         TYPE string,
             irn           TYPE string,
             signedinvoice TYPE string,
             signedqrcode  TYPE string,
             ewbno         TYPE string,
             ewbdt         TYPE string,
             ewbvalidtill  TYPE string,
             qrcodeurl     TYPE string,
             einvoicepdf   TYPE string,
             status        TYPE string,
             remarks       TYPE string,
             alert         TYPE string,
             error         TYPE string,
           END OF gty_irn.

    TYPES: BEGIN OF gty_msg_data,
             message      TYPE gty_irn,
             errormessage TYPE string,
             infodtls     TYPE string,
             status       TYPE string,
             code         TYPE string,
             requestid    TYPE string,
           END OF gty_msg_data.

    TYPES: BEGIN OF gty_resp,
             results TYPE gty_msg_data,
           END OF gty_resp.

    DATA:
      ls_resp TYPE gty_resp,
      gt_data TYPE TABLE OF zsd_einv_data,
      gs_data TYPE zsd_einv_data.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT * FROM zi_sale_reg
    WHERE BillingDocument     = @im_vbeln AND
          BillingDocumentDate = @im_date  AND
          Plant               = @im_plant AND
          CompanyCode         = @im_bukrs AND
          BillingDocumentIsCancelled = ''
    INTO TABLE @DATA(it_final) .              "#EC CI_ALL_FIELDS_NEEDED

    miw_string = im_einv_resp.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = miw_string
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_resp
    ).

    DATA(ls_res) = ls_resp-results.

    IF ls_res-code = '200' AND ls_res-status = 'Success'.

      DATA(ls_msg) = ls_res-message.

      IF ls_msg IS NOT INITIAL.

        READ TABLE it_final INTO DATA(ls_final) INDEX 1. "#EC CI_NOORDER
        gs_data-billingdocument   = im_vbeln.
        gs_data-companycode       = im_bukrs.
        gs_data-fiscalyear        = im_fyear.
        gs_data-plant_gstin       = '06AABCE7582R1ZX'. "ls_final-PLANT_GSTIN_NEW
        gs_data-docdate           = im_date.
        gs_data-plant             = im_plant.
        gs_data-status            = ls_res-status.
        gs_data-irn               = ls_msg-irn.
        gs_data-ackno             = ls_msg-ackno.
        gs_data-ackdt             = ls_msg-ackdt.
        gs_data-ewbno             = ls_msg-ewbno.
        gs_data-ewbdt             = ls_msg-ewbdt.
        gs_data-signedinvoice     = ls_msg-signedinvoice.
        gs_data-signedqrcode      = ls_msg-signedqrcode.
        gs_data-ewbvalidtill      = ls_msg-ewbvalidtill.
        gs_data-qrcodeurl         = ls_msg-qrcodeurl.
        gs_data-erdat             = sys_date.
        gs_data-uname             = sys_uname.
        gs_data-uzeit             = sys_time.
        gs_data-cancel_status     = ''.
        "*gs_data-cancel_date       = ''.
        "*gs_data-cancel_time       = ''.
        APPEND gs_data TO gt_data.

        IF gt_data[] IS NOT INITIAL.
          MODIFY zsd_einv_data FROM TABLE @gt_data.
          COMMIT WORK.
        ENDIF.

      ENDIF.
    ENDIF.

    r_saved_resp = im_einv_resp.

  ENDMETHOD.
ENDCLASS.
