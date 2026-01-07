CLASS zcl_get_irn_detail DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA:
      im_subs_id TYPE string,
      im_gstin   TYPE string.

    METHODS:
      get_excelon_auth_token
        RETURNING VALUE(iv_access_token) TYPE string,

      get_excelon_app_key
        IMPORTING
                  im_access_token  TYPE string
        RETURNING VALUE(r_app_key) TYPE string,

      encrypt_logon_detail
        IMPORTING
                  im_app_key             TYPE string
                  im_auth_token          TYPE string
        RETURNING VALUE(r_encrypt_login) TYPE string,

      get_irp_token
        IMPORTING
                  im_auth_token     TYPE string
                  im_encrypt_login  TYPE string
        RETURNING VALUE(r_irp_data) TYPE string,

      get_encrypty_irn_detail
        IMPORTING
                  im_auth_token        TYPE string
                  im_irp_data          TYPE string
                  im_doc_num           TYPE string
                  im_doc_typ           TYPE string
                  im_doc_date          TYPE string
        RETURNING VALUE(r_irn_encrypt) TYPE string,

      get_decrypted_doc
        IMPORTING
                  im_auth_token     TYPE string
                  im_irp_data       TYPE string
                  im_irn_encrypt    TYPE string
                  im_app_key        TYPE string
        RETURNING VALUE(r_irn_data) TYPE string,

      get_encrypt_eway_detail
        IMPORTING
                  im_auth_token         TYPE string
                  im_irp_data           TYPE string
                  im_irn_num            TYPE string
                  im_app_key            TYPE string
        RETURNING VALUE(r_eway_encrypt) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GET_IRN_DETAIL IMPLEMENTATION.


  METHOD encrypt_logon_detail.

    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string.

    DATA:
      lv_secret TYPE string.

    DATA: lv_sysid    TYPE zsd_sysid-sysid.

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'IRN' AND sysid = @sy-sysid
                    INTO @DATA(ls_sysid).

    IF sy-subrc EQ 0.
      lv_sysid = ls_sysid-sysid.
    ENDIF.

    CLEAR : url.
    IF ( lv_sysid = 'ZSZ' OR lv_sysid = 'CCX' ).

      url = 'https://demoapi.exactgst.com/gstcore/api/EncryptLoginPayload'.
      im_subs_id = '9628bedc-f73a-4625-bf90-80632aa213a7'.
      im_gstin   = '06AAACI2419N1ZK'.

    ELSE.

      url = 'https://einv.exactgst.com/gstcore/api/EncryptLoginPayload'.
      im_subs_id = '11206265-6749-4a27-bd62-8dd8dd4ae5f5'.
      im_gstin   = '06AAACI2419N1ZK'.

    ENDIF.

    TRY.
        DATA(dest11) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest11 ).

        DATA(lo_request11) = lo_http_client->get_http_request( ).
        lo_request11->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        CLEAR: miw_string.
        IF ( lv_sysid = 'ZSZ' OR lv_sysid = 'CCX' ).

          miw_string = '{'
             && '"UserName":' && '"SIPL_06_1",'
             && '"Password":' && '"123@Excellon",'
             && '"AppKey":' &&  '"' && im_app_key && '",'
             && '"ForceRefreshAccessToken":' &&  '"false"'
             && '}'.

        ELSE.

          miw_string = '{'
             && '"UserName":' && '"API_SIPLIND",'
             && '"Password":' && '"SIPLind@123",'
             && '"AppKey":' &&  '"' && im_app_key && '",'
             && '"ForceRefreshAccessToken":' &&  '"false"'
             && '}'.

        ENDIF.

        lo_request11->append_text(
          EXPORTING
            data = miw_string
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'AuthenticationToken'
            i_value = im_auth_token
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'ExactSubscriptionId'
            i_value = im_subs_id
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response11) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        DATA(response_body11) = lo_response11->get_text( ).

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.

    "*REPLACE ALL OCCURRENCES OF '"' IN response_body11 WITH space.
    r_encrypt_login = response_body11.

  ENDMETHOD.


  METHOD get_decrypted_doc.

    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string.

    DATA:
      lv_secret TYPE string.

    TYPES: BEGIN OF lty_irp_data,
             ClientId    TYPE string,
             UserName    TYPE string,
             AuthToken   TYPE string,
             Sek         TYPE string,
             TokenExpiry TYPE string,
           END OF lty_irp_data.

    DATA:
      lt_irp TYPE TABLE OF lty_irp_data,
      ls_irp TYPE lty_irp_data.


    DATA: lv_sysid    TYPE zsd_sysid-sysid.

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'IRN' AND sysid = @sy-sysid
                    INTO @DATA(ls_sysid).

    IF sy-subrc EQ 0.
      lv_sysid = ls_sysid-sysid.
    ENDIF.

    /ui2/cl_json=>deserialize(
                    EXPORTING json = im_irp_data
                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                       CHANGING data = lt_irp
                 ).

    READ TABLE lt_irp INTO ls_irp INDEX 1.

    CLEAR : url, im_subs_id.
    IF ( lv_sysid = 'ZSZ' OR lv_sysid = 'CCX' ).

      url = 'https://demoapi.exactgst.com/gstcore/api/DecryptDataSEK'.
      im_subs_id = '9628bedc-f73a-4625-bf90-80632aa213a7'.
      im_gstin   = '06AAACI2419N1ZK'.

    ELSE.

      url = 'https://einv.exactgst.com/gstcore/api/DecryptDataSEK'.
      im_subs_id = '11206265-6749-4a27-bd62-8dd8dd4ae5f5'.
      im_gstin   = '06AAACI2419N1ZK'.

    ENDIF.

    TRY.
        DATA(dest11) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest11 ).

        DATA(lo_request11) = lo_http_client->get_http_request( ).
        lo_request11->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        CLEAR: miw_string.
        miw_string = '{'
           && '"Data":' && im_irn_encrypt && ','
           && '"rek":' && 'null,'
           && '"status":' && '"1"'
           && '}'.

        lo_request11->append_text(
          EXPORTING
            data = miw_string
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'AuthToken'
            i_value = ls_irp-authtoken
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'AuthenticationToken'
            i_value = im_auth_token
        ).


        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'ExactSubscriptionId'
            i_value = im_subs_id
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'sek'
            i_value = ls_irp-sek
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'AppKey'
            i_value = im_app_key
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response11) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        DATA(response_body11) = lo_response11->get_text( ).

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.

    r_irn_data = '[' && response_body11 && ']'.

  ENDMETHOD.


  METHOD get_encrypty_irn_detail.

    DATA: url            TYPE string,
          url1           TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string.

    DATA:
      lv_secret TYPE string.

    TYPES: BEGIN OF lty_irp_data,
             ClientId    TYPE string,
             UserName    TYPE string,
             AuthToken   TYPE string,
             Sek         TYPE string,
             TokenExpiry TYPE string,
           END OF lty_irp_data.

    DATA:
      lt_irp TYPE TABLE OF lty_irp_data,
      ls_irp TYPE lty_irp_data.

    DATA: lv_sysid    TYPE zsd_sysid-sysid.

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'IRN' AND sysid = @sy-sysid
                    INTO @DATA(ls_sysid).

    IF sy-subrc EQ 0.
      lv_sysid = ls_sysid-sysid.
    ENDIF.

    CLEAR : url.
    IF ( lv_sysid = 'ZSZ' OR lv_sysid = 'CCX' ).

      url = 'https://demoapi.exactgst.com/eicore/v1.03/Invoice/irnbydocdetails?'.
      im_subs_id = '9628bedc-f73a-4625-bf90-80632aa213a7'.
      im_gstin   = '06AAACI2419N1ZK'.

    ELSE.

      url = 'https://einv.exactgst.com/eicore/v1.03/Invoice/irnbydocdetails?'.
      im_subs_id = '11206265-6749-4a27-bd62-8dd8dd4ae5f5'.
      im_gstin   = '06AAACI2419N1ZK'.

    ENDIF.

    /ui2/cl_json=>deserialize(
                    EXPORTING json = im_irp_data
                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                       CHANGING data = lt_irp
                 ).

    READ TABLE lt_irp INTO ls_irp INDEX 1.

    url = url
          && 'doctype=' && im_doc_typ
          && '&docnum=' && im_doc_num
          && '&docdate=' && im_doc_date.

    TRY.
        DATA(dest11) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest11 ).

        DATA(lo_request11) = lo_http_client->get_http_request( ).
        lo_request11->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'AuthenticationToken'
            i_value = im_auth_token
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'ExactSubscriptionId'
            i_value = im_subs_id
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'user_name'
            i_value = ls_irp-username
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'authtoken'
            i_value = ls_irp-authtoken
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'gstin'
            i_value = im_gstin
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'sup_gstin'
            i_value = ''
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response11) = lo_http_client->execute( i_method = if_web_http_client=>get ).
        DATA(response_body11) = lo_response11->get_text( ).

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.

    SPLIT response_body11 AT '"Data":' INTO DATA(split_1) DATA(split_2) .
    SPLIT split_2 AT ',' INTO DATA(irn_encrypt) DATA(split_3) .
    r_irn_encrypt = irn_encrypt.

  ENDMETHOD.


  METHOD get_encrypt_eway_detail.

    DATA: url            TYPE string,
          url1           TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string.

    DATA:
      lv_secret TYPE string.

    TYPES: BEGIN OF lty_irp_data,
             ClientId    TYPE string,
             UserName    TYPE string,
             AuthToken   TYPE string,
             Sek         TYPE string,
             TokenExpiry TYPE string,
           END OF lty_irp_data.

    DATA:
      lt_irp TYPE TABLE OF lty_irp_data,
      ls_irp TYPE lty_irp_data.

    DATA: lv_sysid    TYPE zsd_sysid-sysid.

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'IRN' AND sysid = @sy-sysid
                    INTO @DATA(ls_sysid).

    IF sy-subrc EQ 0.
      lv_sysid = ls_sysid-sysid.
    ENDIF.

    CLEAR : url.
    IF ( lv_sysid = 'ZSZ' OR lv_sysid = 'CCX' ).

      url = 'https://demoapi.exactgst.com/eiewb/v1.03/ewaybill/?'.
      im_subs_id = '9628bedc-f73a-4625-bf90-80632aa213a7'.
      im_gstin   = '06AAACI2419N1ZK'.

    ELSE.

      url = 'https://einv.exactgst.com/eiewb/v1.03/ewaybill/?irn='.
      im_subs_id = '11206265-6749-4a27-bd62-8dd8dd4ae5f5'.
      im_gstin   = '06AAACI2419N1ZK'.

    ENDIF.

    /ui2/cl_json=>deserialize(
                    EXPORTING json = im_irp_data
                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                       CHANGING data = lt_irp
                 ).

    READ TABLE lt_irp INTO ls_irp INDEX 1.

    url = url
          && 'irn=' && im_irn_num.

    TRY.

        DATA(dest11) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest11 ).

        DATA(lo_request11) = lo_http_client->get_http_request( ).
        lo_request11->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'AuthenticationToken'
            i_value = im_auth_token
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'user_name'
            i_value = ls_irp-username
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'authtoken'
            i_value = ls_irp-authtoken
        ).


        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'gstin'
            i_value = im_gstin
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'ExactSubscriptionId'
            i_value = im_subs_id
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'sup_gstin'
            i_value = ''
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response11) = lo_http_client->execute( i_method = if_web_http_client=>get ).
        DATA(response_body11) = lo_response11->get_text( ).

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.

    SPLIT response_body11 AT '"Data":' INTO DATA(split_1) DATA(split_2) .
    SPLIT split_2 AT ',' INTO DATA(eway_encrypt) DATA(split_3) .
    r_eway_encrypt = eway_encrypt.

  ENDMETHOD.


  METHOD get_excelon_app_key.

    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string.

    DATA:
      lv_secret TYPE string.

    DATA: lv_sysid    TYPE zsd_sysid-sysid.

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'IRN' AND sysid = @sy-sysid
                    INTO @DATA(ls_sysid).

    IF sy-subrc EQ 0.
      lv_sysid = ls_sysid-sysid.
    ENDIF.

    CLEAR : url.
    IF ( lv_sysid = 'ZSZ' OR lv_sysid = 'CCX' ).

      url = 'https://demoapi.exactgst.com/gstcore/api/GenerateAppKeyString'.
      im_subs_id = '9628bedc-f73a-4625-bf90-80632aa213a7'.
      im_gstin   = '06AAACI2419N1ZK'.

    ELSE.

      url = 'https://einv.exactgst.com/gstcore/api/GenerateAppKeyString'.
      im_subs_id = '11206265-6749-4a27-bd62-8dd8dd4ae5f5'.
      im_gstin   = '06AAACI2419N1ZK'.

    ENDIF.

    TRY.
        DATA(dest11) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest11 ).

        DATA(lo_request11) = lo_http_client->get_http_request( ).
        lo_request11->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'AuthenticationToken'
            i_value = im_access_token
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'ExactSubscriptionId'
            i_value = im_subs_id
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response11) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        DATA(response_body11) = lo_response11->get_text( ).

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.

    REPLACE ALL OCCURRENCES OF '"' IN response_body11 WITH space.
    r_app_key = response_body11.

  ENDMETHOD.


  METHOD get_excelon_auth_token.

    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string.

    DATA:
      lv_secret TYPE string.

    DATA: lv_sysid    TYPE zsd_sysid-sysid.

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'IRN' AND sysid = @sy-sysid
                    INTO @DATA(ls_sysid).

    IF sy-subrc EQ 0.
      lv_sysid = ls_sysid-sysid.
    ENDIF.

    CLEAR : url.
    IF ( lv_sysid = 'ZSZ' OR lv_sysid = 'CCX' ).

      url = 'https://demoapi.exactgst.com/api/authentication/getAuthenticationToken'.
      im_subs_id = '9628bedc-f73a-4625-bf90-80632aa213a7'.
      im_gstin   = '06AAACI2419N1ZK'.

    ELSE.

      url = 'https://einv.exactgst.com/api/authentication/getAuthenticationToken'.
      im_subs_id = '11206265-6749-4a27-bd62-8dd8dd4ae5f5'.
      im_gstin   = '06AAACI2419N1ZK'.

    ENDIF.

    TRY.
        DATA(dest11) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest11 ).

        IF ( lv_sysid = 'ZSZ' OR lv_sysid = 'CCX' ).

          lv_secret = '"oLlLOEdr1kyEcOnudN0CHCoDm9Iw7BsRrZ8Gxn2oY0O+Y4Hlm23FD1+TQoBeN5kumm8+Xnue/NODhu01WcVSoyQzncmwgjOq'
          && 'jeMVCXuc4gUosgttY9OgJqG9CQBBsY69uJIC1n46D7zx5Q0imCAfed1cuNmuz+65+HDHaHz0pIyd0wCX7DEcXppMgFDsjduQNI60W'
          && 'i6/JrMkX2vGlMtg7t67uHq3Eum2CZOSpyk+5VBX22Mci0LW5MGexVfmmkLsizZgf48/v3xx8ttTt8+ykev1PehgL8uD52/34DB9WYJuJ'
          && '0irHQfKrqzPQb6g84Os0juhYBDtr02a/wNZG1u73A=="'.

          CLEAR: miw_string.
          miw_string = '{'
                  && '"ClientId":' && '"07fccf59-4f59-47d9-9dcd-40b6b6950e45",'
                  && '"ClientSecret":' && lv_secret
                  && '}'.

        ELSE.

          lv_secret = '"ZuHTuFn33g6Wg+7kcdZS1j88YF9vGZkpSNFBlOSDEZhIZvYdutllwjw00Ts5LYnI9L/u5xdvtYuDjUiNyRNtF28HQ0486hg8'
                   && 'Uf8v+ynaV1IUzaNcoNCvQakxKU6vxbRLYYRFNV3M1A6V9S7L/3xyKvpQCILt0sk/8i6ICq229kcuyM/619LMFi2VYQey1LskwnFstok'
                   && 'PJrL0V6qpcSj5nqZrlY0baW0QB22QibgnlooxMvdCEgWiKAPbraWRZuV95PIh1weW6sIfsySMD1yOGDpf97sZ1BN88DfHmXBOPY4GczT'
                   && 'PqwgE+vD/Y/QABCjEpLa4MEAddvdYRsrrOI+R4Q=="'.

          CLEAR: miw_string.
          miw_string = '{'
                  && '"ClientId":' && '"93e5081e-c1ee-44a3-b0b0-954d1ded561a",'
                  && '"ClientSecret":' && lv_secret
                  && '}'.

        ENDIF.

        DATA(lo_request11) = lo_http_client->get_http_request( ).
        lo_request11->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        lo_request11->append_text(
          EXPORTING
            data = miw_string
        ).

        DATA(lo_response11) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        DATA(response_body11) = lo_response11->get_text( ).

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.

    SPLIT response_body11 AT '"AuthenticationToken": ' INTO DATA(split_1) DATA(split_2) .
    SPLIT split_2 AT ',' INTO DATA(token) DATA(split_3) .
    REPLACE ALL OCCURRENCES OF '"' IN token WITH space.
    iv_access_token = token.

  ENDMETHOD.


  METHOD get_irp_token.

    DATA: url            TYPE string,
          lo_http_client TYPE REF TO if_web_http_client,
          miw_string     TYPE string.

    DATA:
      lv_secret TYPE string.

    DATA: lv_sysid    TYPE zsd_sysid-sysid.

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'IRN' AND sysid = @sy-sysid
                    INTO @DATA(ls_sysid).

    IF sy-subrc EQ 0.
      lv_sysid = ls_sysid-sysid.
    ENDIF.

    CLEAR : url.
    url = ''.

    IF ( lv_sysid = 'ZSZ' OR lv_sysid = 'CCX' ).

      url = 'https://demoapi.exactgst.com/eivital/v1.04/auth'.
      im_subs_id = '9628bedc-f73a-4625-bf90-80632aa213a7'.
      im_gstin   = '06AAACI2419N1ZK'.

    ELSE.

      url = 'https://einv.exactgst.com/eivital/v1.04/auth'.
      im_subs_id = '11206265-6749-4a27-bd62-8dd8dd4ae5f5'.
      im_gstin   = '06AAACI2419N1ZK'.

    ENDIF.

    TRY.
        DATA(dest11) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest11 ).

        DATA(lo_request11) = lo_http_client->get_http_request( ).
        lo_request11->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        CLEAR: miw_string.
        miw_string = '{'
           && '"Data":' && im_encrypt_login
           && '}'.

        lo_request11->append_text(
          EXPORTING
            data = miw_string
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'AuthenticationToken'
            i_value = im_auth_token
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'ExactSubscriptionId'
            i_value = im_subs_id
        ).

        lo_request11->set_header_field(
          EXPORTING
            i_name  = 'gstin'
            i_value = im_gstin
        ).

*    CATCH cx_web_message_error.
        DATA(lo_response11) = lo_http_client->execute( i_method = if_web_http_client=>post ).
        DATA(response_body11) = lo_response11->get_text( ).

      CATCH cx_web_http_client_error ##NO_HANDLER.

      CATCH cx_http_dest_provider_error ##NO_HANDLER.

    ENDTRY.

    SPLIT response_body11 AT '"Data":' INTO DATA(split_1) DATA(split_2) .
    SPLIT split_2 AT '},' INTO DATA(token) DATA(split_3) .
    r_irp_data = '[' && token && '}]'.

  ENDMETHOD.
ENDCLASS.
