CLASS zcl_token_print DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      gt_final TYPE TABLE OF zi_token_data,
      nt_final TYPE TABLE OF zstr_wbridge.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sy_uname     TYPE c LENGTH 20.

    DATA:
      lv_char10  TYPE c LENGTH 10,
      lv_char4   TYPE c LENGTH 4,
      lv_char120 TYPE c LENGTH 120.

    METHODS:
      get_token_data
        IMPORTING
                  im_tokennum     LIKE lv_char10
                  im_action       LIKE lv_char10
        RETURNING VALUE(et_final) LIKE gt_final,

     prep_xml_token_print
        IMPORTING
                  it_final             LIKE gt_final
                  im_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_TOKEN_PRINT IMPLEMENTATION.


  METHOD get_token_data.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT * FROM ZI_TOKEN_DATA
    WHERE tokennum = @im_tokennum
    INTO TABLE @DATA(lt_tok).

    et_final[] = CORRESPONDING #( lt_tok[] ).

  ENDMETHOD.


  METHOD prep_xml_token_print.

    DATA : heading      TYPE c LENGTH 100,
           sub_heading  TYPE c LENGTH 200,
           lv_xml_final TYPE string,
           lv_header1   TYPE string,
           address      type string,
           lv_header2   TYPE string,
           lv_date      TYPE c LENGTH 10,
           lv_time      TYPE c LENGTH 10.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    READ TABLE it_final INTO DATA(ls_final) INDEX 1.
    IF ls_final-plant = '1001'.
       lv_header1 = 'Bhagwati Lacto Vegetarian Exports Pvt Ltd' ##NO_TEXT.
       address    = 'Firozepur Punjab' ##NO_TEXT.
    elseif ls_final-plant = '1002'.
       lv_header1 = 'HHF Kandla' ##NO_TEXT.
       address    = 'PLOT NO. 362, GIDC IND. ESTATE, VILL. MITHIROHAR, GANDHIDHAM (KANDLA) GUJRAT 370201' ##NO_TEXT.
    ls_final-SupplierGstin = '24AAFCH2454K1ZL' .
    elseif  ls_final-plant = '2001'.
        lv_header1 = 'Bhagwati Lacto Foods Pvt Ltd' ##NO_TEXT.
        address    = 'Firozepur Punjab' ##NO_TEXT .
    elseif  ls_final-plant = '3001'.
        lv_header1 = 'Healthy Harvested Foods Private Limited' ##NO_TEXT.
        address    = 'Kachchh Gujarat' ##NO_TEXT.
*    elseif  ls_final-plant = '4001'.
*        lv_header1 = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
*        address    = 'Narmadapuram M.P' ##NO_TEXT.
    elseif ls_final-plant = '4001'.
       lv_header1 = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
       address    = 'M-14A VILL. MOHASA BABAI INDSUTRIAL AREA MAKHAN NAGAR NARMADAPURAM M.P 461661' ##NO_TEXT.
    ls_final-SupplierGstin = '23AAJCB8215E1ZZ' .
    ENDIF.

    lv_header2 = 'Token Print' ##NO_TEXT.

    CONCATENATE ls_final-tokendate+6(2) ls_final-tokendate+4(2) ls_final-tokendate+0(4)
    into lv_date SEPARATED BY '.' .

    CONCATENATE ls_final-tokentime+0(2) ls_final-tokentime+2(2) ls_final-tokentime+4(2)
    into lv_time SEPARATED BY ':' .

    DATA(lv_xml) =  |<Form>| &&
                    |<TokenPrintNode>| &&
                    |<plant_name>{ lv_header1 }</plant_name>| &&
                    |<adrs>{  address    }</adrs>| &&
                    |<Token_No>{ ls_final-tokennum }</Token_No>| &&
                    |<Supplier_code>{ ls_final-supplier }</Supplier_code>| &&
                    |<Supplier_name>{ ls_final-SupplierName }</Supplier_name>| &&
                    |<GSTIN_No>{ ls_final-SupplierGstin }</GSTIN_No>| &&
                    |<Vehicle_No>{ ls_final-vehicleno }</Vehicle_No>| &&
                    |<In_Date>{ lv_date }</In_Date>| &&
                    |<In_Time>{ lv_time }</In_Time>| &&
                    |<Driver_name>{ ls_final-drivername }</Driver_name>| &&
                    |<Driver_Mobile>{ ls_final-SupplierMobile }</Driver_Mobile>| &&
                    |<Vehicle_type>{ ls_final-vehicletype }</Vehicle_type>| &&
                    |<Remarks>{ ls_final-remark }</Remarks>| &&
                    |<ItemData>|  ##NO_TEXT.

    DATA : lv_item TYPE string .

    LOOP AT it_final INTO data(wa_final) .
    lv_item = |{ lv_item }| && |<ItemDataNode>| &&
              |<PO_No>{ wa_final-ponum }</PO_No>| &&
              |<Material>{ wa_final-material }</Material>| &&
              |<Material_desc>{ wa_final-MaterialDesc }</Material_desc>| &&
              |<INS_LOT_NO>{ wa_final-insplotno }</INS_LOT_NO>| &&
              |</ItemDataNode>|  ##NO_TEXT.

     clear: wa_final.
     endloop.

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                       |</TokenPrintNode>| &&
                       |</Form>|.

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.

  ENDMETHOD.
ENDCLASS.
