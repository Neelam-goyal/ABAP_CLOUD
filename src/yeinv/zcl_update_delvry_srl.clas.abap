CLASS zcl_update_delvry_srl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20.

    DATA:
      gt_srl    TYPE TABLE OF zstr_dlvry_serial,
      lv_char10 TYPE c LENGTH 10,
      lv_char2  TYPE c LENGTH 2.

    METHODS:
      fetch_delvry_srl_data,

      fetch_csrf_token
        IMPORTING
                  im_action            LIKE lv_char10
        RETURNING VALUE(es_csrf_token) TYPE string,

      save_serial_data
        IMPORTING
                  im_action           LIKE lv_char10
                  it_srl              LIKE gt_srl
                  im_input_str        TYPE string
        RETURNING VALUE(es_save_stat) TYPE string,

      fetch_serial_data
        IMPORTING
                  im_action     LIKE lv_char10
                  im_date       LIKE lv_char10
        RETURNING VALUE(et_srl) LIKE gt_srl,

      update_serial_data
        IMPORTING
                  im_action            LIKE lv_char10
                  im_csrf_token        TYPE string
                  im_mode              LIKE lv_char10
                  xt_srl               LIKE gt_srl
        RETURNING VALUE(et_srl_status) LIKE gt_srl.

    INTERFACES if_oo_adt_classrun .
    INTERFACES if_apj_dt_exec_object .
    INTERFACES if_apj_rt_exec_object .

    CONSTANTS : default_inventory_id          TYPE c LENGTH 1 VALUE '1',
                wait_time_in_seconds          TYPE i VALUE 5,
                selection_name                TYPE c LENGTH 8   VALUE 'BATCHSRL' ##NO_TEXT,
                selection_description         TYPE c LENGTH 255 VALUE 'Batch Serial' ##NO_TEXT,
                application_log_object_name   TYPE if_bali_object_handler=>ty_object VALUE 'ZAPP_SRL_ALOG_01' ##NO_TEXT,
                application_log_sub_obj1_name TYPE if_bali_object_handler=>ty_object VALUE 'ZAPP_SRL_ALOGS_01' ##NO_TEXT.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_UPDATE_DELVRY_SRL IMPLEMENTATION.


  METHOD fetch_csrf_token.

*    "API endpoint for API sandbox
*    DATA: lv_url1     TYPE string,
*          lv_url      TYPE string,
*          lv_api_pass TYPE string,
*          response    TYPE string,
*          miw_string  TYPE string.
*
*    DATA: lv_sysid   TYPE zsd_sysid-sysid,
*          lv_obj_val TYPE zsd_sysid-objvalue,
*          lv_sys_url TYPE zsd_sysid-objvalue.
*
*    SELECT SINGLE * FROM zsd_sysid
*                    WHERE objcode = 'APIUSRPSS' AND sysid = @sy-sysid
*                    INTO @DATA(ls_sysid_pass).
*
*    IF sy-subrc EQ 0.
*      lv_obj_val = ls_sysid_pass-objvalue.
*    ENDIF.
*
*    SELECT SINGLE * FROM zsd_sysid
*                    WHERE objcode = 'SYSURL' AND sysid = @sy-sysid
*                    INTO @DATA(ls_sys_url).
*
*    IF sy-subrc EQ 0.
*      lv_sys_url = ls_sys_url-objvalue.
*      CONDENSE lv_sys_url.
*    ENDIF.
*
*    IF lv_sys_url IS NOT INITIAL.
*
*      lv_url1     = lv_sys_url && '/sap/opu/odata/sap/API_OUTBOUND_DELIVERY_SRV;v=2/A_OutbDeliveryHeader'.
*      lv_api_pass = lv_obj_val.
*
*    ENDIF.
*
*    TRY.
*
*        "create http destination by url; API endpoint for API sandbox
*        DATA(lo_http_destination) = cl_http_destination_provider=>create_by_url( lv_url1 ).
*
*        "create HTTP client by destination
*        DATA(lo_web_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ).
*
*        "adding headers with API Key for API Sandbox
*        DATA(lo_web_http_request) = lo_web_http_client->get_http_request( ).
*
*        lo_web_http_request->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.
*
*        lo_web_http_request->set_authorization_basic( i_username = 'api_user'
*                                              i_password = lv_api_pass ).
*
*        miw_string  =
*        '{'
*        && '"ActualGoodsMovementDate": "/Date(1492098664000)/"'
*        && '}'.
*
*        lo_web_http_request->append_text(
*          EXPORTING
*            data = miw_string
*        ).
*
*        lo_web_http_request->set_header_fields( VALUE #(
*        (  name = 'APIKey' value = 'auKKAVy7tpKvqcJq8JchjOflWWliK571' )
*        (  name = 'DataServiceVersion' value = '2.0' )
*        (  name = 'Accept' value = 'application/json' )
*        (  name = 'x-csrf-token' value = 'fetch' )
*        (  name = 'If-Match' value = '*' )
*         ) ) ##NO_TEXT.
*
*        "set request method and execute request
*        DATA(lo_web_http_response12) = lo_web_http_client->execute( if_web_http_client=>head ).
*        DATA(lv_response_csrf) = lo_web_http_response12->get_header_field(
*            i_name  = 'x-csrf-token'
*        ).
*
*        es_csrf_token = lv_response_csrf.
*
*      CATCH cx_http_dest_provider_error cx_web_http_client_error cx_web_message_error.
*        "error handling
*    ENDTRY.

  ENDMETHOD.


  METHOD fetch_delvry_srl_data.

  ENDMETHOD.


  METHOD fetch_serial_data.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    sys_date = im_date+6(4) && im_date+3(2) && im_date+0(2).

    SELECT * FROM zsd_batch_srl
             WHERE erdat EQ @sys_date AND
                   updatestatus EQ ''
             INTO TABLE @DATA(gt_srl_org).

    et_srl[] = CORRESPONDING #( gt_srl_org[] ).

  ENDMETHOD.


  METHOD if_apj_dt_exec_object~get_parameters.

    DATA:
      sys_date TYPE d.

    sys_date = cl_abap_context_info=>get_system_date( ).

    "Return the supported selection parameters here
    et_parameter_def = VALUE #(
      ( selname  = selection_name
        kind     = if_apj_dt_exec_object=>parameter
        datatype = 'C'
        length   =  8
        param_text = selection_description
        changeable_ind = abap_true )
    ).

    "Return the default parameters values here
    et_parameter_val = VALUE #(
      ( selname = selection_name
        kind = if_apj_dt_exec_object=>parameter
        sign = 'I'
        option = 'EQ'
        low = sys_date )
    ).

  ENDMETHOD.


  METHOD if_apj_rt_exec_object~execute.

    DATA:
      is_date TYPE d.

    is_date = cl_abap_context_info=>get_system_date( ). "sy-datum.

    DATA:
      lv_csrf_token   TYPE string,
      lv_action       TYPE c LENGTH 10,
      lv_srl_upd_stat TYPE string,
      gt_srl          TYPE TABLE OF zstr_dlvry_serial,
      xt_srl          TYPE TABLE OF zstr_dlvry_serial.

    update_serial_data(
      EXPORTING
        im_action     = lv_action
        im_csrf_token = lv_csrf_token
        im_mode       = 'BCG'
        xt_srl        = gt_srl
      RECEIVING
        et_srl_status = xt_srl

    ).

  ENDMETHOD.


  METHOD if_oo_adt_classrun~main.

    DATA  et_parameters TYPE if_apj_rt_exec_object=>tt_templ_val  .

    DATA:
      sys_date TYPE d.

    sys_date = cl_abap_context_info=>get_system_date( ).

    et_parameters = VALUE #(
        ( selname = selection_name
          kind = if_apj_dt_exec_object=>parameter
          sign = 'I'
          option = 'EQ'
          low = sys_date )
      ).

    TRY.

        if_apj_rt_exec_object~execute( it_parameters = et_parameters ).
        out->write( |Finished| ) ##NO_TEXT.

      CATCH cx_root INTO DATA(job_scheduling_exception) ##NO_HANDLER.

    ENDTRY.

  ENDMETHOD.


  METHOD save_serial_data.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20.

    DATA: bt_srl TYPE TABLE OF zsd_batch_srl,
          bs_srl TYPE zsd_batch_srl.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    IF it_srl[] IS NOT INITIAL.

      LOOP AT it_srl INTO DATA(ls_srl_1).

        bs_srl-deliverydocument        = ls_srl_1-deliverydocument.
        bs_srl-deliverydocumentitem    = ls_srl_1-deliverydocumentitem.
        bs_srl-batch                   = ls_srl_1-batch.
        bs_srl-serialnumber            = ls_srl_1-serialnumber.
        bs_srl-plant                   = ''.
        bs_srl-erdat                   = sys_date.
        bs_srl-uname                   = sys_uname.
        bs_srl-uzeit                   = sys_time.
        APPEND bs_srl TO bt_srl.

        CLEAR: ls_srl_1.
      ENDLOOP.

      MODIFY zsd_batch_srl FROM TABLE @bt_srl.
      IF sy-subrc EQ 0.
        es_save_stat = 'Data saved successfully in SAP' ##NO_TEXT.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD update_serial_data.

    "API endpoint for API sandbox
    DATA: lv_url      TYPE string,
          lv_url1     TYPE string,
          lv_url2     TYPE string,
          lv_api_pass TYPE string,
          response    TYPE string,
          miw_string  TYPE string.

    DATA: lv_sysid   TYPE zsd_sysid-sysid,
          lv_obj_val TYPE zsd_sysid-objvalue,
          lv_sys_url TYPE zsd_sysid-objvalue.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'APIUSRPSS' AND sysid = @sy-sysid
                    INTO @DATA(ls_sysid_pass).

    IF sy-subrc EQ 0.
      lv_obj_val = ls_sysid_pass-objvalue.
    ENDIF.

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'SYSURL' AND sysid = @sy-sysid
                    INTO @DATA(ls_sys_url).

    IF sy-subrc EQ 0.
      lv_sys_url = ls_sys_url-objvalue.
      CONDENSE lv_sys_url.
    ENDIF.



    DATA:
      lv_DeliveryDocument TYPE string,
      lv_DeliveryDocItem  TYPE string,
      lv_SerialNumber     TYPE string,
      it_srl              TYPE TABLE OF zstr_dlvry_serial.

    IF im_mode  = 'BCG'.

      SELECT * FROM zsd_batch_srl
               WHERE erdat EQ @sys_date
               INTO TABLE @DATA(gt_batch_srl). "#EC CI_ALL_FIELDS_NEEDED

      if gt_batch_srl[] is NOT INITIAL.
       it_srl[] =  CORRESPONDING #( gt_batch_srl[] ).
      ENDIF.

    else.

    it_srl[] = xt_srl[].

    ENDIF.

    IF it_srl[] IS NOT INITIAL.
      LOOP AT it_srl INTO DATA(ls_srl).

        IF lv_sys_url IS NOT INITIAL.

          lv_url1     = lv_sys_url && '/sap/opu/odata/sap/API_OUTBOUND_DELIVERY_SRV;v=2/AddSerialNumberToDeliveryItem?'.

          lv_DeliveryDocument = ls_srl-deliverydocument.     "'80000056'.
          lv_DeliveryDocItem  = ls_srl-deliverydocumentitem. "'900002'.
          lv_SerialNumber     = ls_srl-serialnumber.         "'10000250'.
          SHIFT lv_SerialNumber LEFT DELETING LEADING '0'.
          lv_url2 = |DeliveryDocument='{ lv_DeliveryDocument }'&DeliveryDocumentItem='{ lv_DeliveryDocItem }'&SerialNumber='{ lv_SerialNumber }'|.

          lv_url      = lv_url1 && lv_url2.
          lv_api_pass = lv_obj_val.

        ENDIF.

        TRY.

            "create http destination by url; API endpoint for API sandbox
            DATA(lo_http_destination) = cl_http_destination_provider=>create_by_url( lv_url ).

            "create HTTP client by destination
            DATA(lo_web_http_client) = cl_web_http_client_manager=>create_by_http_destination( lo_http_destination ).

            "adding headers with API Key for API Sandbox
            DATA(lo_web_http_request) = lo_web_http_client->get_http_request( ).

*        lo_web_http_request->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

            lo_web_http_request->set_authorization_basic( i_username = 'api_user'
                                                  i_password = lv_api_pass ).

            lo_web_http_request->set_header_fields( VALUE #(
            (  name = 'APIKey' value = 'auKKAVy7tpKvqcJq8JchjOflWWliK571' )
            (  name = 'DataServiceVersion' value = '2.0' )
            (  name = 'Accept' value = 'application/json' )
            (  name = 'x-csrf-token' value = 'fetch' )
            (  name = 'If-Match' value = '*' )
             ) ) ##NO_TEXT.

            "set request method and execute request
            DATA(lo_web_http_response12) = lo_web_http_client->execute( if_web_http_client=>head ).
            DATA(lv_response_csrf) = lo_web_http_response12->get_header_field(
                i_name  = 'x-csrf-token'
            ).

            lo_web_http_request->set_header_fields( VALUE #(
            (  name = 'APIKey' value = 'auKKAVy7tpKvqcJq8JchjOflWWliK571' )
            (  name = 'DataServiceVersion' value = '2.0' )
            (  name = 'Accept' value = 'application/json' )
            (  name = 'x-csrf-token' value = lv_response_csrf )
            (  name = 'If-Match' value = '*' )
             ) ) ##NO_TEXT.

*        CATCH cx_web_message_error.
            DATA(lo_web_http_response) = lo_web_http_client->execute( if_web_http_client=>post ).
            DATA(lv_response) = lo_web_http_response->get_text( ).

            ""***Start: Converting response in internal table*****************************
            DATA:
              lr_data     TYPE REF TO data.

            FIELD-SYMBOLS:
              <lt_table> TYPE STANDARD TABLE.

            FREE: lr_data.
            /ui2/cl_json=>deserialize(
                    EXPORTING json = lv_response
                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                       CHANGING data = lr_data
                 ).

            ASSIGN lr_data->* TO FIELD-SYMBOL(<ls_data>).
            " Map the ADD_TEXT field
            ASSIGN COMPONENT 'D' OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<ld_add>).

            IF <ld_add> IS ASSIGNED.

              ls_srl-updatestatus = 'Success' ##NO_TEXT.
              UNASSIGN <ld_add>.
              UNASSIGN <ls_data>.

              UPDATE zsd_batch_srl
              SET updatestatus = @abap_true,
                  update_date  = @sys_date,
                  update_time  = @sys_time
               WHERE deliverydocument     = @ls_srl-deliverydocument AND
                     deliverydocumentitem = @ls_srl-deliverydocumentitem AND
                     batch                = @ls_srl-batch AND
                     serialnumber         = @ls_srl-serialnumber.

            ELSE.
              ls_srl-updatestatus = 'Error in updation' ##NO_TEXT.
            ENDIF.
            ""***End: Converting response in internal table*****************************

          CATCH cx_http_dest_provider_error cx_web_http_client_error cx_web_message_error ##NO_HANDLER.
            "error handling
        ENDTRY.

        APPEND ls_srl TO et_srl_status.

        CLEAR: ls_srl, lv_response.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
