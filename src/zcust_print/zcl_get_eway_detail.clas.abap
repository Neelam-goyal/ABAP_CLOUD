CLASS zcl_get_eway_detail DEFINITION
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
        RETURNING VALUE(r_irn_data) TYPE string.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_GET_EWAY_DETAIL IMPLEMENTATION.


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

      url = 'https://demoapiewb.exactgst.com/eway/api/EncryptLoginPayload'.
      im_subs_id = 'fbb34a5c-6a6f-4daf-bc9d-b7a9ff9a84b7'.
      im_gstin   = '06AAACI2419N1ZK'.

    ELSE.

      url = 'https://einv.exactgst.com/gstcore/api/EncryptLoginPayload'.
      im_subs_id = '09b9c7d5-7a8e-4e61-8ee1-9c6decd0c85d'.
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
             && '"username":' && '"SIPL_06_1",'
             && '"password":' && '"123@Excellon",'
             && '"app_key":' &&  '"' && im_app_key && '",'
             && '"action":' &&  '"ACCESSTOKEN"'
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
             status    TYPE string,
             authtoken TYPE string,
             sek       TYPE string,
           END OF lty_irp_data.

    TYPES: BEGIN OF lty_encypt_eway,
             status TYPE string,
             data   TYPE string,
             rek    TYPE string,
             hmac   TYPE string,
           END OF lty_encypt_eway.


    DATA:
      lt_irp         TYPE TABLE OF lty_irp_data,
      ls_irp         TYPE lty_irp_data,
      lt_encypt_eway TYPE TABLE OF lty_encypt_eway,
      ls_encypt_eway TYPE lty_encypt_eway.

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

    /ui2/cl_json=>deserialize(
                    EXPORTING json = im_irn_encrypt
                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                       CHANGING data = lt_encypt_eway
                 ).

    READ TABLE lt_irp INTO ls_irp INDEX 1.
    READ TABLE lt_encypt_eway INTO ls_encypt_eway INDEX 1.

    CLEAR : url, im_subs_id.
    IF ( lv_sysid = 'ZSZ' OR lv_sysid = 'CCX' ).

      url = 'https://demoapiewb.exactgst.com/eway/api/DecryptDataSEK'.
      im_subs_id = 'fbb34a5c-6a6f-4daf-bc9d-b7a9ff9a84b7'.
      im_gstin   = '06AAACI2419N1ZK'.

    ELSE.

      url = 'https://einv.exactgst.com/gstcore/api/DecryptDataSEK'.
      im_subs_id = '09b9c7d5-7a8e-4e61-8ee1-9c6decd0c85d'.
      im_gstin   = '06AAACI2419N1ZK'.

    ENDIF.

    TRY.
        DATA(dest11) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest11 ).

        DATA(lo_request11) = lo_http_client->get_http_request( ).
        lo_request11->set_content_type( 'application/json; charset=utf-8' ) ##NO_TEXT.

        CLEAR: miw_string.
        miw_string = '{'
           && '"Data":' && '"' && ls_encypt_eway-data && '",'
           && '"rek":' && '"' && ls_encypt_eway-rek && '",'
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
             status    TYPE string,
             authtoken TYPE string,
             sek       TYPE string,
           END OF lty_irp_data.

    DATA:
      lt_irp    TYPE TABLE OF lty_irp_data,
      ls_irp    TYPE lty_irp_data,
      user_name TYPE string.

    DATA: lv_sysid    TYPE zsd_sysid-sysid.

    SELECT SINGLE * FROM zsd_sysid
                    WHERE objcode = 'IRN' AND sysid = @sy-sysid
                    INTO @DATA(ls_sysid).

    IF sy-subrc EQ 0.
      lv_sysid = ls_sysid-sysid.
    ENDIF.

    CLEAR : url.
    IF ( lv_sysid = 'ZSZ' OR lv_sysid = 'CCX' ).

      url = 'https://demoapiewb.exactgst.com/ewaybillapi/v1.03/ewayapi/GetEwayBillGeneratedByConsigner?'.
      im_subs_id = 'fbb34a5c-6a6f-4daf-bc9d-b7a9ff9a84b7'.
      im_gstin   = '06AAACI2419N1ZK'.
      user_name  = 'AnandNVH_06_1'.

    ELSE.

      url = 'https://einv.exactgst.com/eicore/v1.03/Invoice/irnbydocdetails?'.
      im_subs_id = '09b9c7d5-7a8e-4e61-8ee1-9c6decd0c85d'.
      im_gstin   = '06AAACI2419N1ZK'.

    ENDIF.

    /ui2/cl_json=>deserialize(
                    EXPORTING json = im_irp_data
                       pretty_name = /ui2/cl_json=>pretty_mode-camel_case
                       CHANGING data = lt_irp
                 ).

    READ TABLE lt_irp INTO ls_irp INDEX 1.


    url = url
          && 'docType=' && im_doc_typ
          && '&docNo=' && im_doc_num.
    "&& '&docdate=' && im_doc_date.

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
            i_value = user_name
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

*    SPLIT response_body11 AT '"data":' INTO DATA(split_1) DATA(split_2) .
*    SPLIT split_2 AT ',' INTO DATA(irn_encrypt) DATA(split_3) .
    r_irn_encrypt = '[' && response_body11 && ']'. "irn_encrypt.

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

      url = 'https://demoapiewb.exactgst.com/eway/api/GenerateAppKeyString'.
      im_subs_id = 'fbb34a5c-6a6f-4daf-bc9d-b7a9ff9a84b7'.
      im_gstin   = '06AAACI2419N1ZK'.

    ELSE.

      url = 'https://einv.exactgst.com/gstcore/api/GenerateAppKeyString'.
      im_subs_id = '09b9c7d5-7a8e-4e61-8ee1-9c6decd0c85d'.
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

      url = 'https://demoapiewb.exactgst.com/api/authentication/getAuthenticationToken'.
      im_subs_id = 'fbb34a5c-6a6f-4daf-bc9d-b7a9ff9a84b7'.
      im_gstin   = '06AAACI2419N1ZK'.

    ELSE.

      url = 'https://einv.exactgst.com/api/authentication/getAuthenticationToken'.
      im_subs_id = '09b9c7d5-7a8e-4e61-8ee1-9c6decd0c85d'.
      im_gstin   = '06AAACI2419N1ZK'.

    ENDIF.

    TRY.
        DATA(dest11) = cl_http_destination_provider=>create_by_url( url ).
        lo_http_client = cl_web_http_client_manager=>create_by_http_destination( dest11 ).

        IF ( lv_sysid = 'ZSZ' OR lv_sysid = 'CCX' ).

          lv_secret = '"m9bJki6l0UhzQ2kTjHfbflH9T8LJfcQgfRPbwK1Lfewnt7swU5ld/UQRFFfwwZSgt2O2HAjoiUBPEiGC6WC8/etGNfcZNmu98vB'
                   && 'iVDS10MXjrk4f90AmvRtQ2+mxeeW4Lcv/UlLDOVitrwascRNNa0cnjGqyrMFPSiJzbjulqgjiOB8ciWT34f+t2qESkmfZJsFjoHS'
                   && 'Ql/HjGILE+gdqT+qLmUmCOpqx7D9DtzVmav6vHZjqIaDhFxOOQg6yq012qvKaT8Qb+yr8Xq2qjDB31GESjH0k3TPM2oGZRVBDY92N'
                   && 'c3UphKkhYfwxDpgeHgnP6Vcz6tWDzdyIrAcDMERQcw=="'.

          CLEAR: miw_string.
          miw_string = '{'
                  && '"ClientId":' && '"28e7dfb9-c10d-4071-805d-a1787aca9b3e",'
                  && '"ClientSecret":' && lv_secret
                  && '}'.

        ELSE.

          lv_secret = '"YYvfpcmrqfXRDj6dS8yIJRA9wlZovoYyL2wLqfU7A3vEn05CqLVblcAazVi2mPiVi7TvpFmzo7hR0Lbr5qLWCWq4ISWSGcU'
                   && 'z8LvILjlGKmlteND+Nusi/0RVk+BezGd51ZCVHwHhHXdxDPKicBNkSheMnQQ1fhGBAkKwwOdmzOoCU3FOM4H4fgLiimvv9Izz97'
                   && 'vktXTfe/UHaBUUt2XsIqHlChAODf6XlJhtRnnCPf0nrFA4EqrH2Hap8eI0Eo83NU0ukSO9oqSzmwwlTWpeRqiX1K2OvyZF0Ciqk'
                   && 'GGMuigiRktUMzbYTtoK7/ugHmbVnoqoDuUCArStcld8ZEWXTQ=="'.

          CLEAR: miw_string.
          miw_string = '{'
                  && '"ClientId":' && '"962fbc8d-10c7-42e9-9c5b-4767fe885b9b",'
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

      url = 'https://demoapiewb.exactgst.com/ewaybillapi/v1.03/Auth'.
      im_subs_id = 'fbb34a5c-6a6f-4daf-bc9d-b7a9ff9a84b7'.
      im_gstin   = '06AAACI2419N1ZK'.

    ELSE.

      url = 'https://einv.exactgst.com/eivital/v1.04/auth'.
      im_subs_id = '09b9c7d5-7a8e-4e61-8ee1-9c6decd0c85d'.
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

*    SPLIT response_body11 AT '"Data":' INTO DATA(split_1) DATA(split_2) .
*    SPLIT split_2 AT '},' INTO DATA(token) DATA(split_3) .
    r_irp_data = '[' && response_body11 && ']'.

  ENDMETHOD.
ENDCLASS.
