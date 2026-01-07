CLASS zcl_eway_process_clrtx DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      gt_final  TYPE TABLE OF zstr_eway_data,
      gs_final  TYPE zstr_eway_data,
      lv_char10 TYPE c LENGTH 10,
      lv_char20 TYPE c LENGTH 20,
      lv_char4  TYPE c LENGTH 4.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20.

    METHODS:
      fetch_irn_data
        IMPORTING
                  im_vbeln           LIKE lv_char10
                  im_bukrs           LIKE lv_char4
                  im_fyear           LIKE lv_char4
                  im_plant           LIKE lv_char4
                  im_date            LIKE lv_char10
                  iv_action          LIKE lv_char10
                  im_etype           LIKE lv_char10
        RETURNING VALUE(r_einv_data) LIKE gt_final,

      get_token_from_clrtx
        IMPORTING
                  im_action             LIKE lv_char10
        RETURNING VALUE(r_access_token) TYPE string,

      prepare_eway_json_clrtx
        IMPORTING
                  im_vbeln           LIKE lv_char10
                  im_bukrs           LIKE lv_char4
                  im_fyear           LIKE lv_char4
                  im_plant           LIKE lv_char4
                  im_date            LIKE lv_char10
                  iv_action          LIKE lv_char10
                  im_access_token    TYPE string
                  im_input_str       TYPE string
        RETURNING VALUE(r_eway_json) TYPE string,

      gen_eway_from_clrtx
        IMPORTING
                  im_action         LIKE lv_char10
                  im_einv_json      TYPE string
                  im_input_str      TYPE string
        RETURNING VALUE(r_gen_eway) TYPE string,

      save_eway_data_clrtx
        IMPORTING
                  im_vbeln            LIKE lv_char10
                  im_bukrs            LIKE lv_char4
                  im_fyear            LIKE lv_char4
                  im_plant            LIKE lv_char4
                  im_date             LIKE lv_char10
                  im_eway_resp        TYPE string
                  im_input_str        TYPE string
        RETURNING VALUE(r_saved_resp) TYPE string,

      cancel_eway_clrtx
        IMPORTING
                  im_vbeln           LIKE lv_char10
                  im_bukrs           LIKE lv_char4
                  im_fyear           LIKE lv_char4
                  im_plant           LIKE lv_char4
                  im_plant_gstin     LIKE lv_char20
                  im_date            LIKE lv_char10
                  iv_action          LIKE lv_char10
                  im_access_token    TYPE string
                  im_input_str       TYPE string
        RETURNING VALUE(r_eway_canc) TYPE string,

      download_eway_clrtx
        IMPORTING
                  im_ewayno         LIKE lv_char20
                  im_vbeln          LIKE lv_char10
                  im_bukrs          LIKE lv_char4
                  im_fyear          LIKE lv_char4
                  im_plant          LIKE lv_char4
                  im_plant_gstin    LIKE lv_char20
                  im_date           LIKE lv_char10
                  iv_action         LIKE lv_char10
                  im_access_token   TYPE string
                  im_input_str      TYPE string
        RETURNING VALUE(r_eway_zip) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_EWAY_PROCESS_CLRTX IMPLEMENTATION.


  METHOD cancel_eway_clrtx.

    TYPES: BEGIN OF gty_resp,
             ownerid      TYPE string,
             gstin        TYPE string,
             irn          TYPE string,
             ewbnumber    TYPE string,
             ewbstatus    TYPE string,
             errordetails TYPE string,
           END OF gty_resp.
    DATA:
      ls_resp   TYPE gty_resp,
      cw_string TYPE string.

    DATA:
      gt_input TYPE TABLE OF zstr_eway_data,
      gs_input TYPE zstr_eway_data.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear,
          lv_doc_month(2) TYPE n.

    ""***Start: Data Preparation***************************
    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    lv_doc_date  = sys_date.
    lv_doc_month = lv_doc_date+4(2).
    lv_fis_year  = lv_doc_date+0(4).

    IF lv_doc_month LT 4.
      lv_fis_year = lv_fis_year - 1.
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gs_input
                                 ).

    IF gs_input IS INITIAL.
      gs_input-billingdocument =  im_vbeln.
      gs_input-companycode     =  im_bukrs.
      gs_input-plant           =  im_plant.
      gs_input-plant_gstin     =  im_plant_gstin.
    ENDIF.

    SELECT SINGLE * FROM zsd_einv_data
    WHERE billingdocument = @gs_input-billingdocument AND
          companycode     = @gs_input-companycode  AND
          fiscalyear      = @lv_fis_year AND
          plant           = @gs_input-plant AND
          plant_gstin     = @gs_input-plant_gstin AND
          ewbno           NE '' AND
          canc_status_eway EQ ''
          INTO @DATA(cs_einv) .               "#EC CI_ALL_FIELDS_NEEDED

    CLEAR: cw_string.
    cw_string = '{'
    && '"ewbNo":' && '"' && cs_einv-ewbno && '",'
    && '"cancelRsnCode": "DATA_ENTRY_MISTAKE",'
    && '"cancelRmrk": "DATA_ENTRY_MISTAKE"'
    && '}'.

    ""***End: Data Preparation***************************

    """"Start:Cancellation:*****************************************************************************************
    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string,
          lv_auth_token  TYPE string,
          lv_owner_id    TYPE string,
          lv_gstin       TYPE string.

    DATA:
      r_dq  TYPE RANGE OF sy-sysid,
      rw_dq LIKE LINE OF r_dq.

    rw_dq-low    = 'M1C' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    rw_dq-low    = 'PFE' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    IF sy-sysid IN r_dq.
      lv_gstin = '06AAFCD5862R017' ##NO_TEXT.
    ELSE.
      lv_gstin = gs_input-plant_gstin.
    ENDIF.

    SELECT SINGLE
      gstin,
      ownerid ,
      authtoken,
      url_auth_token,
      url_einv_gen,
      url_einv_canc,
      url_qrb2c,
      url_eway_gen,
      url_eway_irn,
      url_eway_partb,
      url_eway_canc
    FROM zsd_cred_clr
    WHERE gstin = @lv_gstin
    INTO @DATA(ls_clr_cred). "#EC WARNOK                 "#EC CI_ALL_FIELDS_NEEDED

    IF sy-subrc EQ 0.
      url           = ls_clr_cred-url_eway_canc.
      lv_auth_token = ls_clr_cred-authtoken.
      lv_gstin      = ls_clr_cred-gstin.
      lv_owner_id   = ls_clr_cred-ownerid.
    ENDIF.


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

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'X-Cleartax-Auth-Token' ##NO_TEXT
            i_value = lv_auth_token
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'X-Cleartax-Product' ##NO_TEXT
            i_value = 'EInvoice' ##NO_TEXT
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'owner_id' ##NO_TEXT
            i_value = lv_owner_id
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'gstin' ##NO_TEXT
            i_value = lv_gstin
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response1) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        DATA(response_body1) = lo_response1->get_text( ).
        r_eway_canc = response_body1.

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.
    """"End:Cancellation**********************************************************************

    """**Start: Update table for cancelled document*******************************************
    CLEAR: miw_string.
    miw_string = r_eway_canc.

    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = miw_string
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_resp
    ).

    IF ls_resp-ewbstatus = 'CANCELLED'.

      cs_einv-cancel_date_eway   = sys_date.
      cs_einv-canc_status_eway   = abap_true.
      cs_einv-cancel_time_eway   = sys_time.
      MODIFY zsd_einv_data FROM @cs_einv.
      COMMIT WORK.

    ENDIF.
    """**End: Update table for cancelled document*********************************************

  ENDMETHOD.


  METHOD download_eway_clrtx.

    TYPES: BEGIN OF gty_resp,
             ownerid      TYPE string,
             gstin        TYPE string,
             irn          TYPE string,
             ewbnumber    TYPE string,
             ewbstatus    TYPE string,
             errordetails TYPE string,
           END OF gty_resp.
    DATA:
      ls_resp   TYPE gty_resp,
      cw_string TYPE string.

    DATA:
      gt_input TYPE TABLE OF zstr_eway_data,
      gs_input TYPE zstr_eway_data.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear,
          lv_doc_month(2) TYPE n.

    ""***Start: Data Preparation***************************
    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    lv_doc_date  = sys_date.
    lv_doc_month = lv_doc_date+4(2).
    lv_fis_year  = lv_doc_date+0(4).

    IF lv_doc_month LT 4.
      lv_fis_year = lv_fis_year - 1.
    ENDIF.

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gs_input
                                 ).

    IF gs_input IS INITIAL.
      gs_input-billingdocument =  im_vbeln.
      gs_input-companycode     =  im_bukrs.
      gs_input-plant           =  im_plant.
      gs_input-plant_gstin     =  im_plant_gstin.
    ENDIF.

    SELECT SINGLE * FROM zsd_einv_data
    WHERE billingdocument = @gs_input-billingdocument AND
          companycode     = @gs_input-companycode  AND
          fiscalyear      = @lv_fis_year AND
          plant           = @gs_input-plant AND
          plant_gstin     = @gs_input-plant_gstin AND
          ewbno           NE '' AND
          canc_status_eway EQ ''
          INTO @DATA(cs_einv) .               "#EC CI_ALL_FIELDS_NEEDED

    CLEAR: cw_string.
    CONDENSE cs_einv-ewbno.
    cw_string = '{'
        && '"ewb_numbers": ['  &&  cs_einv-ewbno && '],'
        && '"print_type": "BASIC"'
    && '}'.

    ""***End: Data Preparation***************************

    """"Start:Download:*****************************************************************************************
    DATA:
      lo_zip TYPE REF TO cl_abap_zip.

    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string,
          lv_auth_token  TYPE string,
          lv_owner_id    TYPE string,
          lv_gstin       TYPE string.

    DATA:
      r_dq  TYPE RANGE OF sy-sysid,
      rw_dq LIKE LINE OF r_dq.

    rw_dq-low    = 'M1C' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    rw_dq-low    = 'PFE' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    IF sy-sysid IN r_dq.
      lv_gstin = '06AAFCD5862R017' ##NO_TEXT.
    ELSE.
      lv_gstin = gs_input-plant_gstin.
    ENDIF.

    SELECT SINGLE
      gstin,
      ownerid ,
      authtoken,
      url_auth_token,
      url_einv_gen,
      url_einv_canc,
      url_qrb2c,
      url_eway_gen,
      url_eway_irn,
      url_eway_partb,
      url_eway_canc
    FROM zsd_cred_clr
    WHERE gstin = @lv_gstin
    INTO @DATA(ls_clr_cred). "#EC WARNOK                 "#EC CI_ALL_FIELDS_NEEDED

    IF sy-subrc EQ 0.
      IF sy-sysid IN r_dq.
      url           = 'https://api-sandbox.clear.in/einv/v2/eInvoice/ewaybill/print' ##NO_TEXT. "ls_clr_cred-url_einv_gen.
      else.
      url           = 'https://api.clear.in/einv/v2/eInvoice/ewaybill/print' ##NO_TEXT. "ls_clr_cred-url_einv_gen.
      endif.
      lv_auth_token = ls_clr_cred-authtoken.
      lv_gstin      = ls_clr_cred-gstin.
      lv_owner_id   = ls_clr_cred-ownerid.
    ENDIF.


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

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'X-Cleartax-Auth-Token' ##NO_TEXT
            i_value = lv_auth_token
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'X-Cleartax-Product' ##NO_TEXT
            i_value = 'EInvoice' ##NO_TEXT
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'Schema-Type' ##NO_TEXT
            i_value = 'govt_schema_v1.1' ##NO_TEXT
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'owner_id' ##NO_TEXT
            i_value = lv_owner_id
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'gstin' ##NO_TEXT
            i_value = lv_gstin
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response1) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        DATA(response_body1) = lo_response1->get_text( ).

        DATA:
          lv_content   TYPE xstring,
          lv_responsex TYPE xstring.

        CREATE OBJECT lo_zip.

        lo_response1->get_binary(
          RECEIVING
            r_value  = lv_responsex
        ).

        lo_zip->load(
          EXPORTING
            zip             = lv_responsex
          EXCEPTIONS
            zip_parse_error = 1
            OTHERS          = 2
        ).

        lo_zip->get(
          EXPORTING
            index                   = 1
          IMPORTING
            content                 = lv_content
          EXCEPTIONS
            zip_index_error         = 1
            zip_decompression_error = 2
            OTHERS                  = 3
        ).

        r_eway_zip = lv_content.

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.
    """"End:Download**********************************************************************

  ENDMETHOD.


  METHOD fetch_irn_data.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear,
          lv_doc_month(2) TYPE n,
          r_vbeln         TYPE RANGE OF zi_sale_reg-billingdocument,
          r_date          TYPE RANGE OF zi_sale_reg-billingdocumentdate,
          r_plant         TYPE RANGE OF zi_sale_reg-plant,
          r_bukrs         TYPE RANGE OF zi_sale_reg-companycode,
          wr_bukrs        LIKE LINE OF r_bukrs,
          wr_vbeln        LIKE LINE OF r_vbeln,
          wr_date         LIKE LINE OF r_date,
          wr_plant        LIKE LINE OF r_plant.

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

    IF im_bukrs IS NOT INITIAL.
      wr_bukrs-low    = im_bukrs.
      wr_bukrs-high   = im_bukrs.
      wr_bukrs-sign   = 'I'.
      wr_bukrs-option = 'BT'.
      APPEND wr_bukrs TO r_bukrs.
    ENDIF.

    SELECT * FROM zsd_einv_data
    WHERE billingdocument  IN @r_vbeln AND
          companycode      IN @r_bukrs  AND
          plant            IN @r_plant AND
          erdat            IN @r_date AND
          fiscalyear       = @lv_fis_year AND
          businessmodule   = 'SD' AND
          cancel_status    = '' AND
          canc_status_eway = ''
    INTO TABLE @DATA(it_einv) .               "#EC CI_ALL_FIELDS_NEEDED


    LOOP AT it_einv INTO DATA(ls_einv).

      gs_final-billingdocument   = ls_einv-billingdocument.
      gs_final-companycode       = ls_einv-companycode.
      gs_final-fiscalyear        = ls_einv-fiscalyear.
      gs_final-plant_gstin       = ls_einv-plant_gstin.
      gs_final-billingtype       = ls_einv-billingtype.
      gs_final-irn               = ls_einv-irn.
      gs_final-plant             = ls_einv-plant.
      gs_final-ewbno             = ls_einv-ewbno.
      gs_final-ewbdt             = ls_einv-ewbdt.
      gs_final-transporterid     = ''.
      gs_final-modeoftransp      = ''.
      gs_final-transpdocnum      = ''.
      gs_final-transdocdate      = ''.
      gs_final-vehiclenum        = ''.
      gs_final-vehicletype       = ''.
      gs_final-transpname        = ''.
      gs_final-datasource        = ''.
      gs_final-distance          = ''.
      gs_final-status            = ''.
      APPEND gs_final TO gt_final.

      CLEAR: ls_einv.
    ENDLOOP.

    DELETE gt_final WHERE billingtype NE 'F2'.
    IF im_etype = 'new'.
      DELETE gt_final WHERE ewbno NE ''.
    ELSEIF im_etype = 'generated'.
      DELETE gt_final WHERE ewbno EQ ''.
    ENDIF.

    r_einv_data[] = gt_final[].

  ENDMETHOD.


  METHOD gen_eway_from_clrtx.

        """""*******************************
        TYPES: BEGIN OF gty_resp1,
                 success      TYPE string,
                 ackno        TYPE string,
                 ackdt        TYPE string,
                 irn          TYPE string,
                 ewbno        TYPE string,
                 ewbdt        TYPE string,
                 status       TYPE string,
                 ewbvalidtill TYPE string,
               END OF gty_resp1.

        DATA:
          eway_resp     TYPE gty_resp1,
          ls_resp_final TYPE gty_resp1.

        TYPES: BEGIN OF gty_resp,
                 transmode     TYPE c LENGTH 20,
                 vehno         TYPE c LENGTH 20,
                 vehtype       TYPE c LENGTH 20,
                 govt_response LIKE eway_resp,
               END OF gty_resp.

        DATA: ls_eway_resp TYPE gty_resp,
              lt_eway_resp TYPE TABLE OF gty_resp.
        ""******************************************************************

    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string,
          lv_auth_token  TYPE string,
          lv_owner_id    TYPE string,
          lv_gstin       TYPE string.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear,
          lv_doc_month(2) TYPE n.

    DATA:
      gt_input TYPE TABLE OF zstr_eway_data,
      gs_input TYPE zstr_eway_data,
      gv_json  TYPE string.

    DATA:
      r_dq  TYPE RANGE OF sy-sysid,
      rw_dq LIKE LINE OF r_dq.

    rw_dq-low    = 'M1C' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    rw_dq-low    = 'PFE' ##NO_TEXT.
    rw_dq-sign   = 'I' ##NO_TEXT.
    rw_dq-option = 'EQ' ##NO_TEXT.
    APPEND rw_dq TO r_dq.

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gs_input
                                 ).

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    lv_doc_date  = sys_date.
    lv_doc_month = lv_doc_date+4(2).
    lv_fis_year  = lv_doc_date+0(4).

    IF lv_doc_month LT 4.
      lv_fis_year = lv_fis_year - 1.
    ENDIF.

    SELECT SINGLE * FROM zsd_einv_data
    WHERE billingdocument  = @gs_input-billingdocument AND
          companycode      = @gs_input-companycode  AND
          fiscalyear       = @lv_fis_year AND
          plant            = @gs_input-plant AND
          plant_gstin      = @gs_input-plant_gstin AND
          ewbno            = '' AND
          cancel_status    = ''
    INTO @DATA(ls_einv) .                     "#EC CI_ALL_FIELDS_NEEDED

    IF sy-sysid IN r_dq.
      lv_gstin = '06AAFCD5862R017' ##NO_TEXT.
    ELSE.
      lv_gstin = gs_input-plant_gstin.
    ENDIF.

    SELECT SINGLE
      gstin,
      ownerid ,
      authtoken,
      url_auth_token,
      url_einv_gen,
      url_einv_canc,
      url_qrb2c,
      url_eway_gen,
      url_eway_irn,
      url_eway_partb,
      url_eway_canc
    FROM zsd_cred_clr
    WHERE gstin = @lv_gstin
    INTO @DATA(ls_clr_cred). "#EC WARNOK                "#EC CI_ALL_FIELDS_NEEDED

    IF sy-subrc EQ 0.
      url           = ls_clr_cred-url_eway_irn.
      lv_auth_token = ls_clr_cred-authtoken.
      lv_gstin      = ls_clr_cred-gstin.
      lv_owner_id   = ls_clr_cred-ownerid.
    ENDIF.

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

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'X-Cleartax-Auth-Token' ##NO_TEXT
            i_value = lv_auth_token
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'X-Cleartax-Product' ##NO_TEXT
            i_value = 'EInvoice' ##NO_TEXT
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'owner_id' ##NO_TEXT
            i_value = lv_owner_id
        ).

        lo_request1->set_header_field(
          EXPORTING
            i_name  = 'gstin' ##NO_TEXT
            i_value = lv_gstin
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response1) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        DATA(response_body1) = lo_response1->get_text( ).
        "*r_gen_eway = response_body1.
        """"*****************************************************************************************

        /ui2/cl_json=>deserialize(
                        EXPORTING json = response_body1
                           pretty_name = /ui2/cl_json=>pretty_mode-none
                           CHANGING data = lt_eway_resp
                     ).

        IF lt_eway_resp[] IS NOT INITIAL.

          READ TABLE lt_eway_resp INTO ls_eway_resp INDEX 1.

          IF ls_eway_resp-govt_response IS NOT INITIAL.

            ls_resp_final = CORRESPONDING #( ls_eway_resp-govt_response ).

            IF ls_resp_final-success = 'Y'.

              DATA(lv_json) = /ui2/cl_json=>serialize(
                  data             = ls_resp_final
                  pretty_name      = /ui2/cl_json=>pretty_mode-camel_case
                  ).

              r_gen_eway = lv_json.

            ELSE.

              r_gen_eway = response_body1.

            ENDIF.
          ENDIF.
        ENDIF.

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.


  METHOD get_token_from_clrtx.

  ENDMETHOD.


  METHOD prepare_eway_json_clrtx.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear,
          lv_doc_month(2) TYPE n.

    DATA:
      gt_input TYPE TABLE OF zstr_eway_data,
      gs_input TYPE zstr_eway_data,
      gv_json  TYPE string.

    DATA:
      lv_exp_adrs1 TYPE string,
      lv_exp_adrs2 TYPE string,
      lv_exp_adrs3 TYPE string,
      lv_exp_adrs4 TYPE string,
      lv_exp_adrs5 TYPE string,
      lv_kunnr     TYPE zi_customer_address-customer.


    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gs_input
                                 ).

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    lv_doc_date  = sys_date.
    lv_doc_month = lv_doc_date+4(2).
    lv_fis_year  = lv_doc_date+0(4).

    IF lv_doc_month LT 4.
      lv_fis_year = lv_fis_year - 1.
    ENDIF.

    SELECT SINGLE * FROM zsd_einv_data
    WHERE billingdocument  = @gs_input-billingdocument AND
          companycode      = @gs_input-companycode  AND
          fiscalyear       = @lv_fis_year AND
          plant            = @gs_input-plant AND
          plant_gstin      = @gs_input-plant_gstin AND
          ewbno            = '' AND
          cancel_status    = ''
    INTO @DATA(ls_einv) .                     "#EC CI_ALL_FIELDS_NEEDED

    """*****Prepare JSON*********************
    IF gs_input-portid IS INITIAL.
      gv_json = '['
      && '{'
      && '"Irn":'         && '"' && gs_input-irn && '",'
      && '"Distance":'    && '"' && gs_input-distance && '",'
      && '"TransMode":'   && '"' && gs_input-modeoftransp && '",'
      && '"TransId":'     && '"' && gs_input-transporterid && '",'
      && '"TransName":'   && '"' && gs_input-transpname && '",'
      && '"TransDocDt":'  && '"' && gs_input-transdocdate && '",'
      && '"TransDocNo":'  && '"' && gs_input-transpdocnum && '",'
      && '"VehNo":'       && '"' && gs_input-vehiclenum && '",'
      && '"VehType":'     && '"' && gs_input-vehicletype && '"'
      && '}'
      && ']'.
    ENDIF.

    IF gs_input-portid IS NOT INITIAL.

      CONDENSE gs_input-portid.
      lv_kunnr = gs_input-portid.

      SELECT SINGLE
customer,
customername,
customerfullname,
country,
taxnumber3,
addressid,
phonenumber1,
phonenumber2,
region AS regio,
addresseefullname,
organizationname1,
organizationname2,
streetprefixname1,
streetprefixname2,
streetname,
streetsuffixname1,
districtname,
cityname,
postalcode,
addressrepresentationcode,
region,
addresspersonid,
street,
housenumber,
formofaddress,
addresstimezone,
emailaddress
      FROM zi_customer_address
      WHERE customer = @lv_kunnr
      INTO @DATA(ls_port_adrs).

      lv_exp_adrs1  = ls_port_adrs-streetname.
      lv_exp_adrs2  = ls_port_adrs-streetprefixname1.
      lv_exp_adrs3  = ls_port_adrs-cityname.
      lv_exp_adrs4  = ls_port_adrs-postalcode.
      lv_exp_adrs5  = ls_port_adrs-housenumber.


      gv_json = '['
      && '{'
      && '"Irn":'         && '"' && gs_input-irn && '",'
      && '"Distance":'    && '"' && gs_input-distance && '",'
      && '"TransMode":'   && '"' && gs_input-modeoftransp && '",'
      && '"TransId":'     && '"' && gs_input-transporterid && '",'
      && '"TransName":'   && '"' && gs_input-transpname && '",'
      && '"TransDocDt":'  && '"' && gs_input-transdocdate && '",'
      && '"TransDocNo":'  && '"' && gs_input-transpdocnum && '",'
      && '"VehNo":'       && '"' && gs_input-vehiclenum && '",'
      && '"VehType":'     && '"' && gs_input-vehicletype && '",'
      && '"ExpShipDtls": {'
      && '"Addr1":' && '"' && lv_exp_adrs1 && '",'
      && '"Addr2":' && '"' && lv_exp_adrs2 && '",'
      && '"Loc":'   && '"' && lv_exp_adrs3 && '",'
      && '"Pin":'   && '"' && lv_exp_adrs4 && '",'
      && '"Stcd":'  && '"' && lv_exp_adrs5 && '"'
      && '}'
      && '}'
      && ']'.



    ENDIF.

    r_eway_json = gv_json.

  ENDMETHOD.


  METHOD save_eway_data_clrtx.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear,
          lv_doc_month(2) TYPE n.

    DATA:
      miw_string TYPE string.

    DATA:
      gt_input TYPE TABLE OF zstr_eway_data,
      gs_input TYPE zstr_eway_data.

    TYPES: BEGIN OF gty_resp,
             success      TYPE string,
             ackno        TYPE string,
             ackdt        TYPE string,
             irn          TYPE string,
             status       TYPE string,
             ewbno        TYPE string,
             ewbdt        TYPE string,
             ewbvalidtill TYPE string,
           END OF gty_resp.

    DATA:
      ls_resp TYPE gty_resp,
      gt_data TYPE TABLE OF zsd_einv_data,
      gs_data TYPE zsd_einv_data.

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = gs_input
                                 ).

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    lv_doc_date  = sys_date.
    lv_doc_month = lv_doc_date+4(2).
    lv_fis_year  = lv_doc_date+0(4).

    IF lv_doc_month LT 4.
      lv_fis_year = lv_fis_year - 1.
    ENDIF.

    SELECT SINGLE * FROM zsd_einv_data
    WHERE billingdocument = @gs_input-billingdocument AND
          companycode     = @gs_input-companycode  AND
          fiscalyear      = @lv_fis_year AND
          plant           = @gs_input-plant AND
          plant_gstin     = @gs_input-plant_gstin AND
          ewbno           EQ ''
          INTO @DATA(ls_einv) .               "#EC CI_ALL_FIELDS_NEEDED

    """*****Deserialize response******************************
    miw_string = im_eway_resp.
    /ui2/cl_json=>deserialize(
      EXPORTING
        json        = miw_string
        pretty_name = /ui2/cl_json=>pretty_mode-camel_case
      CHANGING
        data        = ls_resp
    ).

    IF ls_resp-success = 'Y'.

      ls_einv-transporterid = gs_input-transporterid.
      ls_einv-modeoftransp  = gs_input-modeoftransp.
      ls_einv-transpdocnum  = gs_input-transpdocnum.
      ls_einv-transdocdate  = gs_input-transdocdate.
      ls_einv-vehiclenum    = gs_input-vehiclenum.
      ls_einv-vehicletype   = gs_input-vehicletype.
      ls_einv-transpname    = gs_input-transpname.
      ls_einv-datasource    = gs_input-datasource.
      ls_einv-distance      = gs_input-distance.
      ls_einv-ewbno         = ls_resp-ewbno.
      ls_einv-ewbdt         = sys_date.
      ls_einv-ewayackno     = ls_resp-ackno.
      ls_einv-ewayackdt     = ls_resp-ackdt.

      MODIFY zsd_einv_data FROM @ls_einv.
      IF sy-subrc EQ 0.
        COMMIT WORK.
      ENDIF.
      CLEAR: ls_einv.

    ENDIF.

    r_saved_resp = im_eway_resp.

  ENDMETHOD.
ENDCLASS.
