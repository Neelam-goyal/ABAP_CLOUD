CLASS ycl_gate_print DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      gt_final TYPE TABLE OF zstr_unload_hdr,
      w_final  TYPE zstr_unload_hdr.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sy_uname     TYPE c LENGTH 20.

    DATA:
      lv_char10  TYPE c LENGTH 10,
      lv_char4   TYPE c LENGTH 4,
      lv_char120 TYPE c LENGTH 120,
      lv_qty     TYPE c LENGTH 10,
      lv_date    TYPE c LENGTH 10.

    METHODS:
      get_gate_entry_data
        IMPORTING
                  im_genum        LIKE lv_char10
                  im_geyear       LIKE lv_char4
                  im_action       LIKE lv_char10
        RETURNING VALUE(et_final) LIKE gt_final,

      get_gate_out_data
        IMPORTING
                  im_genum        LIKE lv_char10
                  im_geyear       LIKE lv_char4
                  im_action       LIKE lv_char10
        RETURNING VALUE(et_final) LIKE gt_final,

      prep_xml_gate_entry_print
        IMPORTING
                  it_final             LIKE gt_final
                  im_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_GATE_PRINT IMPLEMENTATION.


  METHOD get_gate_entry_data. "

    DATA:
      ns_final TYPE zstr_unload_hdr,
      nt_item  TYPE TABLE OF zstr_unload_itm,
      ns_item  TYPE zstr_unload_itm.

    DATA : sl_no TYPE c LENGTH 10.



    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).
    "----

    SELECT * FROM zi_token_ge
    WHERE  gentry_num = @im_genum
    INTO TABLE @DATA(lt_tok).

    et_final[] = CORRESPONDING #( lt_tok[] ).

    IF lt_tok IS INITIAL .
      SELECT *
       FROM zi_ge_data
      WHERE  gentry_num = @im_genum
      INTO TABLE @DATA(lt_tok1).

      et_final[] = CORRESPONDING #( lt_tok1[] ).
    ENDIF.

    ""Item Data
    IF  lt_tok IS NOT INITIAL.
      LOOP AT lt_tok INTO DATA(wa_final1).
        sl_no = sl_no + 1 .
        ns_item-sl_num = sl_no  .
        ns_item-ponum = wa_final1-ponum.
        ns_item-material = wa_final1-material_desc . "material.
        ns_item-qty_bag = wa_final1-invqtybag.
        ns_item-weight = wa_final1-invqty.
        lv_qty = lv_qty + wa_final1-invqtybag .
        CONDENSE : lv_qty .
        APPEND ns_item TO nt_item.
        CLEAR : ns_item .
      ENDLOOP.
    ELSEIF lt_tok1 IS NOT INITIAL.
      LOOP AT lt_tok1 INTO DATA(wa_final2).
        sl_no = sl_no + 1 .
        ns_item-sl_num = sl_no  .
        ns_item-ponum = wa_final2-ponum.
        ns_item-material = wa_final2-maktx. "wa_final2-matnr.
        ns_item-qty_bag = wa_final2-uom.
        ns_item-weight = wa_final2-challnqty.
        IF  ns_item-qty_bag = 'ST'.
          ns_item-qty_bag = 'PC' .
        ENDIF.
        CONDENSE ns_item-weight  .
        APPEND ns_item TO nt_item.
        CLEAR : ns_item .
      ENDLOOP.
    ENDIF.
    INSERT LINES OF nt_item INTO TABLE ns_final-gt_itm.
    APPEND ns_final TO et_final.

    IF lt_tok1 IS NOT INITIAL .
      READ TABLE  et_final ASSIGNING FIELD-SYMBOL(<ff>) INDEX 1 . "#EC CI_NOORDER
      <ff>-lifnr = |{ <ff>-lifnr ALPHA = IN }| .
      IF <ff> IS ASSIGNED .
        SELECT SINGLE suppliername
        FROM  i_supplier
        WHERE supplier = @<ff>-lifnr
        INTO @<ff>-supplier_name.
        " Boc by neelam on 03.07.2025
        SELECT SINGLE driver_num
        FROM  zi_ge_data
        WHERE gentry_num = @<ff>-gentry_num
        INTO @<ff>-mobileno .

        SELECT SINGLE cityname
       FROM  zi_ge_data
       WHERE gentry_num = @<ff>-gentry_num
       INTO @<ff>-city_name.

        SELECT SUM( challnqty ) FROM zi_ge_data WHERE gentry_num = @<ff>-gentry_num INTO @DATA(lv_qty1).
        lv_qty = lv_qty1.
        " Eoc by neelam on 03.07.2025
        CONCATENATE <ff>-created_on+6(2) <ff>-created_on+4(2) <ff>-created_on+0(4)  INTO lv_date SEPARATED BY ':' .
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_gate_out_data.
    DATA:
      ns_final TYPE zstr_unload_hdr,
      nt_item  TYPE TABLE OF zstr_unload_itm,
      ns_item  TYPE zstr_unload_itm,
      lv_qty   TYPE c LENGTH 10.

    DATA : sl_no TYPE c LENGTH 10.

    SELECT * FROM zi_gout_rep
    WHERE gentrynum = @im_genum
    INTO TABLE @DATA(lt_gout).

    lv_qty = REDUCE #( INIT x = 0 FOR wa IN lt_gout NEXT x +=  wa-itemqty )  .

    LOOP AT lt_gout INTO DATA(ww) .
      w_final-gentry_num     = ww-gentrynum .
      w_final-supplier_name  = ww-custname .
      w_final-station        = ww-ewaybillnum .
      w_final-driver_name    = ww-drivername .
      w_final-transporter    = ww-transporter .
      w_final-wb_in_date     = ww-createdon .
      w_final-whmt_charges   = ww-whmgtchar.
      w_final-invqtybag      = lv_qty .
      w_final-vechnum        = ww-vechnum .
      w_final-mobileno       = ww-drivercontact .
      w_final-gross_wt       = ww-grosswgt .
      w_final-tare_wt        = ww-tarewgt .
      w_final-net_wt         = ww-netwgt .
      CONCATENATE ww-uzeit+0(2) ww-uzeit+2(2) ww-uzeit+4(2) INTO w_final-wb_in_time SEPARATED BY ':' .
      w_final-werks          = ww-werks .
      APPEND w_final TO et_final  .
      CLEAR: ww, w_final.
    ENDLOOP.

    CLEAR lv_qty .

  ENDMETHOD.


  METHOD prep_xml_gate_entry_print.

    DATA : heading      TYPE c LENGTH 100,
           sub_heading  TYPE c LENGTH 200,
           lv_xml_final TYPE string,
           lv_header1   TYPE string.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    DATA: lv_time TYPE t.

    lv_time = sys_time.
    DATA(lv_time_string) = |{ lv_time+0(2) }:{ lv_time+2(2) }:{ lv_time+4(2) }|.

    READ TABLE it_final INTO DATA(ls_final) INDEX 1.
    IF im_action EQ 'gateoutsli'.
      ls_final-wb_in_date = |{ sys_date+6(2) }.{ sys_date+4(2) }.{ sys_date+0(4) }|.
    ENDIF.
    " Boc by neelam on 03.07.2025
    IF im_action EQ 'unloadslip'.

      IF ls_final-city_name IS INITIAL.
        SELECT SINGLE cityname
            FROM  zi_ge_data
            WHERE gentry_num = @ls_final-gentry_num
            INTO @ls_final-city_name.
      ENDIF.
    ENDIF.

    IF ls_final-wb_in_date IS INITIAL.
      ls_final-wb_in_date = ls_final-unload_date.
    ELSE.
      ls_final-wb_in_date = ls_final-wb_in_date.
    ENDIF.

    " Boc by neelam on 03.07.2025
    IF ls_final-plant = '1001'  OR ls_final-werks = '1001'.
      ls_final-heading = 'Bhagwati Lacto Vegetarian Exports Pvt Ltd' ##NO_TEXT.
      ls_final-address = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001 Firozepur Punjab' ##NO_TEXT.
    ELSEIF ls_final-plant  = '2001' OR ls_final-werks = '2001'..
      ls_final-heading = 'Bhagwati Lacto Foods Pvt Ltd' ##NO_TEXT.
      ls_final-address = 'VILL. RUKNA MUNGLA, FARIDKOT ROAD, FEROZEPUR, Ferozepur, Punjab, 152001' ##NO_TEXT.
    ELSEIF ls_final-plant  = '3001' OR ls_final-werks = '3001'..
      ls_final-heading = 'Healthy Harvested Foods Private Limited' ##NO_TEXT.
      ls_final-address = 'PLOT NO100, SECTOR 1/A, GANDHIDHAM, Kachchh, Gujarat, 370201' ##NO_TEXT.
    ELSEIF ls_final-plant  = '4001' OR ls_final-werks = '4001'..
      ls_final-heading = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
      ls_final-address = 'Building No./Flat No.: M-14A Road/Street: VILL-Mohasa Babai city Makhan Nagar Distr-Narmadapuram (MP Postal code-461661' ##NO_TEXT.
    ELSEIF ls_final-plant  = '1002' OR ls_final-werks = '1002'..
      ls_final-heading = 'HHF Kandla' ##NO_TEXT.
      ls_final-address = 'PLOT NO.362,GIDC IND.ESTATE,VILL. MITHIROHAR,GANDHIDHAM (KANDLA) GUJRAT 370201,AADCB4295Q ,  24AAFCH2454K1ZL' ##NO_TEXT.
*   elseif ls_final-plant  = '4001' OR ls_final-WERKS = '4001'..
*    ls_final-heading = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
*    ls_final-Address = 'M-14A VILL. MOHASA BABAI INDSUTRIAL AREA MAKHAN NAGAR NARMADAPURAM M.P 461661 ,AAJCB8215E1,23AAJCB8215E1ZZ' ##NO_TEXT.
    ENDIF.

    IF im_action = 'weighslip'.
      ls_final-sub_heading   = 'Weighment Slip' ##NO_TEXT.
      IF ls_final-vehicle_no IS NOT INITIAL.
        ls_final-vehicle_no = ls_final-vehicle_no.
      ELSE.
        ls_final-vehicle_no = ls_final-vechnum.
      ENDIF.
    ELSEIF im_action = 'gateoutsli'.
      ls_final-sub_heading   = 'Out Pass' ##NO_TEXT.
      IF ls_final-vehicle_no IS NOT INITIAL.
        ls_final-vehicle_no = ls_final-vehicle_no.
      ELSE.
        ls_final-vehicle_no = ls_final-vechnum.
      ENDIF.
    ELSEIF im_action = 'unloadslip'.
      ls_final-sub_heading   = 'Unloading Pass' ##NO_TEXT.
    ELSEIF im_action = 'sdweighslp'.
      ls_final-sub_heading   = 'Weighment Slip' ##NO_TEXT.
    ENDIF.
    "  lv_header1 = '1'.

    IF im_action NE 'sdweighslp'   .

      DATA(lv_xml) =  |<Form>| &&
                      |<UploadingPass>| &&
                      |<heading>{ ls_final-heading }</heading>| &&
                      |<Address>{ ls_final-address }</Address>| &&
                      |<Sub_heading>{ ls_final-sub_heading }</Sub_heading>| &&
                      |<Gate_no>{ ls_final-gentry_num }</Gate_no>| &&
                      |<Veh_no>{ ls_final-vehicle_no }</Veh_no>| &&
                      |<vechnum>{ ls_final-vechnum }</vechnum>| &&
                      |<slip_no>{ ls_final-gentry_num }</slip_no>| &&
                      |<transporter>{ ls_final-transporter }</transporter>| &&
                      |<mob_no>{ ls_final-mobileno }</mob_no>| &&
                      |<Supplier>{ ls_final-supplier_name }</Supplier>| &&
                      |<Driver>{ ls_final-driver_name }</Driver>| &&
                      |<Date>{ ls_final-wb_in_date }</Date>| &&
                      |<created_on>{ lv_date }</created_on>| &&
                      |<City_name>{ ls_final-city_name }</City_name>| &&
                      |<station>{ ls_final-city_name }</station>| &&
                      |<Charges>{ ls_final-whmt_charges }</Charges>| &&
                      |<quantity>{ lv_qty }</quantity>| &&
                      |<Remarks>{ ls_final-remarks }</Remarks>| &&
                      |<Supervisor>{ ls_final-supervisor }</Supervisor>| &&
                      |<Gate_incharge>{ ls_final-gate_inch }</Gate_incharge>| &&
                      |<Location>{ ls_final-location }</Location>| &&
                      |<Bardana_qty>{ ls_final-bardana_qty }</Bardana_qty>| &&
                      |<Stack>{ ls_final-stack }</Stack>| &&
                      |<Bags>{ ls_final-bags }</Bags>| &&
                      |<Pkg>{ ls_final-pkg }</Pkg>| &&
                      |<Quality>{ ls_final-quality }</Quality>| &&
                      |<Short_Bag>{ ls_final-short_bag }</Short_Bag>| &&
                      |<Palledar>{ ls_final-palledar }</Palledar>| &&
                      |<Supervisor_sign>{ ls_final-supervisor_sign }</Supervisor_sign>| &&
                      |<Haudi_katai>{ ls_final-haudi_katai }</Haudi_katai>| &&
                      |<gross_wt>{ ls_final-gross_wt }</gross_wt>| &&
                      |<tare_wt>{ ls_final-tare_wt }</tare_wt>| &&
                      |<net_wt>{ ls_final-net_wt }</net_wt>| &&
                      |<wb_in_date>{ ls_final-wb_in_date }</wb_in_date>| &&
                      |<wb_in_time>{ ls_final-wb_in_time }</wb_in_time>| &&
                      |<wb_out_date>{ ls_final-wb_out_date }</wb_out_date>| &&
                      |<wb_out_time>{ ls_final-wb_out_time }</wb_out_time>| &&
                      |<current_time>{ lv_time_string }</current_time>| &&
                      |<ItemData>|  ##NO_TEXT.

      DATA : lv_item TYPE string .
      LOOP AT it_final INTO DATA(ls_final1) .

        LOOP AT ls_final1-gt_itm INTO DATA(ls_item).

          lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                    |<SL_NUM>{ ls_item-sl_num }</SL_NUM>| &&
                    |<PO_NO>{ ls_item-ponum }</PO_NO>| &&
                    |<Material>{ ls_item-material }</Material>| &&
                    |<Qty_bag>{ ls_item-qty_bag }</Qty_bag>| &&
                    |<Weight>{ ls_item-weight }</Weight>| &&
                    |</ItemDataNode>|  ##NO_TEXT.

          CLEAR: ls_item.
        ENDLOOP.
      ENDLOOP.


      lv_xml = |{ lv_xml }{ lv_item }| &&
                         |</ItemData>| &&
                         |</UploadingPass>| &&
                         |</Form>|.

    ELSEIF im_action = 'sdweighslp'   .

      lv_xml =     |<Form>| &&
                   |<WeighSlipNode>| &&
                   |<plant_name>{ ls_final-heading }</plant_name>| &&
                   |<adrs>{ ls_final-address }</adrs>| &&
                   |<sub_heading>{ ls_final-sub_heading }</sub_heading>| &&
                   |<Slip_No>{ ls_final-gentry_num }</Slip_No>| &&
                   |<Date>{ ls_final-wb_in_date }</Date>| &&                     "ls_final-Created_On
                   |<Party_Name>{ ls_final-supplier_name }</Party_Name>| &&
                   |<Station>{ ls_final-station }</Station>| &&
                   |<Charges>{ ls_final-whmt_charges }</Charges>| &&
                   |<Quantity>{ ls_final-invqtybag }</Quantity>| &&
                   |<Driver_name>{ ls_final-driver_name }</Driver_name>| &&
                   |<Mobile_No>{ ls_final-mobileno }</Mobile_No>| &&
                   |<Vehicle_No>{ ls_final-vechnum }</Vehicle_No>| &&
                   |<Transporter>{ ls_final-transporter }</Transporter>| &&
                   |<ItemData>|  ##NO_TEXT.


      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<Gross_wt>{ ls_final-gross_wt }</Gross_wt>| &&
                |<Tare_wt>{ ls_final-tare_wt }</Tare_wt>| &&
                |<Net_wt>{ ls_final-net_wt }</Net_wt>| &&
                |<gross_time>{ ls_final-wb_in_time }</gross_time>| &&
                |<tare_time>{ ls_final-wb_in_time }</tare_time>| &&
                |<Net_time>{ ls_final-wb_in_time }</Net_time>| &&
                |</ItemDataNode>|  ##NO_TEXT.

      lv_xml = |{ lv_xml }{ lv_item }| &&
                         |</ItemData>| &&
                         |</WeighSlipNode>| &&
                         |</Form>|.


    ENDIF.

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.

  ENDMETHOD.
ENDCLASS.
