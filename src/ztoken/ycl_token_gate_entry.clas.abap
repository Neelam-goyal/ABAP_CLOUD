CLASS ycl_token_gate_entry DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA:
      xt_getoken TYPE TABLE OF zstr_token_gate_entry,
      xs_getoken TYPE zstr_token_gate_entry,
      gt_final   TYPE TABLE OF zmm_ge_token,
      gs_final   TYPE zmm_ge_token.

    DATA:
      lv_char10  TYPE c LENGTH 10,
      lv_char120 TYPE c LENGTH 120,
      lv_char4   TYPE c LENGTH 4.

    METHODS:
      get_token_data
        IMPORTING
                  im_tokennum    LIKE lv_char10
        RETURNING VALUE(et_data) LIKE xt_getoken,

      save_data_get_genum
        IMPORTING
                  xt_gedata        LIKE xt_getoken
                  im_action        LIKE lv_char10
        RETURNING VALUE(rv_ge_num) LIKE lv_char120,

      get_ge_change_data
        IMPORTING
                  im_genum       LIKE lv_char10
        RETURNING VALUE(et_data) LIKE xt_getoken.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_TOKEN_GATE_ENTRY IMPLEMENTATION.


  METHOD get_ge_change_data.

    DATA:
      gs_change TYPE zstr_token_gate_entry,
      it_item   TYPE TABLE OF zstr_token_item,
      is_item   TYPE zstr_token_item.

    SELECT * FROM zmm_ge_token WHERE gentry_num = @im_genum AND gedeleted = @abap_false
             INTO TABLE @DATA(gt_gedata).

    DATA(gt_pitem) = gt_gedata[].
    SORT gt_gedata BY gentry_num.
    DELETE ADJACENT DUPLICATES FROM gt_gedata COMPARING gentry_num.

    LOOP AT gt_gedata INTO DATA(ls_gedata).

      gs_change =  CORRESPONDING #( ls_gedata ).

      LOOP AT gt_pitem INTO DATA(gs_item).
        is_item-ebeln         = gs_item-ponum.
        is_item-ebelp         = gs_item-poitem.
        is_item-opqty         = gs_item-opqty.
        is_item-docdate       = gs_item-docdate.
        is_item-netprice      = gs_item-netprice.
        is_item-matnr         = gs_item-material.
        is_item-productgroup  = gs_item-material_group.
        is_item-producttype   = gs_item-material_type.
        is_item-uom           = gs_item-material_uom.
        is_item-maktx         = gs_item-material_desc .
        is_item-supplier      = gs_item-supplier.

        is_item-bagtype        = gs_item-bagtype.
        is_item-bardanawht     = gs_item-bardanawht.
        is_item-invqty         = gs_item-invqty.
        is_item-invqtybag      = gs_item-invqtybag.
        is_item-noofbag        = gs_item-noofbag.
        is_item-totalbardana   = gs_item-totalbardana.
        is_item-totaldeduc     = gs_item-totaldeduc.
        is_item-actualqty      = gs_item-actualqty.
        is_item-deduction      = gs_item-deduction.

        APPEND is_item TO it_item.
        CLEAR: gs_item.
      ENDLOOP.

      INSERT LINES OF it_item INTO TABLE gs_change-item_data.
      APPEND gs_change TO et_data.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_token_data.

    DATA:
      it_item TYPE TABLE OF zstr_token_item,
      is_item TYPE zstr_token_item.

    SELECT
tokennum      ,
plant         ,
ponum         ,
poitem        ,
insplotno         ,
tokeninsp         ,
gatenetryno       ,
gatenetryyear     ,
material          ,
material_desc     ,
material_uom      ,
material_group    ,
material_type     ,
opqty             ,
docdate           ,
netprice          ,
supplier          ,
supplier_name     ,
supplier_gstin    ,
supplier_pan      ,
supplier_mobile   ,
drivername        ,
vehicleno         ,
vehicletype       ,
vehiclname        ,
tokendate         ,
tokentime         ,
tokenstatus       ,
transporter       ,
remark            ,
token_type        ,
token_createdby   ,
token_createdon   ,
token_createdtime ,
tokenchanged      ,
token_changedby   ,
token_changedon   ,
token_changedtime ,
tokendeleted      ,
token_deletedby   ,
token_deletedon   ,
token_deletedtime
    FROM zmm_token_data WHERE tokennum     = @im_tokennum AND
                                       tokendeleted = @abap_false
                                       INTO TABLE @DATA(gt_token). "#EC CI_NO_TRANSFORM

    IF gt_token[] IS NOT INITIAL.

    SELECT SINGLE
    gentry_num   ,
    gentry_year  ,
    ponum        ,
    poitem       ,
    tokennum     ,
    mblnr            ,
    mjahr            ,
    material         ,
    material_desc    ,
    plant            ,
    material_group   ,
    material_uom     ,
    material_type    ,
    opqty            ,
    docdate          ,
    netprice         ,
    supplier         ,
    supplier_name    ,
    supplier_gstin   ,
    supplier_pan     ,
    token_in_date    ,
    token_in_time    ,
    driver_name      ,
    city_name        ,
    part_netprice    ,
    vehicle_no       ,
    remark           ,
    token_type ,
    mobileno         ,
    invoice_no       ,
    invoice_date     ,
    invoice_qty      ,
    invoice_qty_uom  ,
    ewaybill_no      ,
    transporter      ,
    lr_no            ,
    lr_date          ,
    total_bag_rec    ,
    per_bag_wt       ,
    total_bag_wt     ,
    excess_qty       ,
    total_bag_rec_uom,
    per_bag_wt_uom   ,
    total_bag_wt_uom ,
    excess_qty_uom   ,
    grain_vech_stat  ,
    gross_wt         ,
    tare_wt          ,
    net_wt           ,
    net_wt_rec       ,
    net_rec_qty      ,
    gross_wt_uom     ,
    tare_wt_uom      ,
    net_wt_uom       ,
    net_wt_rec_uom   ,
    net_rec_qty_uom  ,
    wb_in_date       ,
    wb_in_time       ,
    wb_out_date      ,
    wb_out_time      ,
    unload_date      ,
    unload_time      ,
    wb_remark        ,
    whmt_charges     ,
    supp_loadwht     ,
    bagtype          ,
    bardanawht       ,
    invqty           ,
    invqtybag        ,
    noofbag          ,
    totalbardana     ,
    totaldeduc       ,
    actualqty        ,
    deduction        ,
    erdat            ,
    uzeit            ,
    cuname           ,
    gedeleted
    FROM zmm_ge_token
    WHERE tokennum   = @im_tokennum
    INTO @DATA(ls_token_ge). "#EC CI_NOORDER

      READ TABLE gt_token INTO DATA(gs_token) INDEX 1.  "#EC CI_NOORDER

      xs_getoken-tokennum          = gs_token-tokennum.
      xs_getoken-gentry_num        = ls_token_ge-gentry_num.
      xs_getoken-gentry_year       = ls_token_ge-gentry_year.
      xs_getoken-material          = gs_token-material.
      xs_getoken-material_desc     = gs_token-material_desc.
      xs_getoken-plant             = gs_token-plant.
      xs_getoken-material_group    = gs_token-material_group.
      xs_getoken-supplier          = gs_token-supplier.
      xs_getoken-supplier_name     = gs_token-supplier_name.
      xs_getoken-supplier_gstin    = gs_token-supplier_gstin.
      xs_getoken-supplier_pan      = gs_token-supplier_pan.
      xs_getoken-token_in_date     = gs_token-tokendate.
      xs_getoken-token_in_time     = gs_token-tokentime.
      xs_getoken-driver_name       = gs_token-drivername.
      xs_getoken-vehicle_no        = gs_token-vehicleno.
      xs_getoken-remark            = gs_token-remark.
      xs_getoken-mobileno          = gs_token-supplier_mobile.
      xs_getoken-token_type        = gs_token-token_type.
      xs_getoken-ponum             = ''.
      xs_getoken-poitem            = ''.
      xs_getoken-invoice_no        = ''.
      xs_getoken-invoice_date      = ''.
      xs_getoken-invoice_qty       = ''.
      xs_getoken-invoice_qty_uom   = ''.
      xs_getoken-ewaybill_no       = ''.
      xs_getoken-transporter       = gs_token-transporter.
      xs_getoken-lr_no             = ''.
      xs_getoken-lr_date           = ''.
      xs_getoken-total_bag_rec     = ''.
      xs_getoken-per_bag_wt        = ''.
      xs_getoken-total_bag_wt      = ''.
      xs_getoken-excess_qty        = ''.
      xs_getoken-total_bag_rec_uom = ''.
      xs_getoken-per_bag_wt_uom    = ''.
      xs_getoken-total_bag_wt_uom  = ''.
      xs_getoken-excess_qty_uom    = ''.
      xs_getoken-grain_vech_stat   = ''.
      xs_getoken-gross_wt          = ''.
      xs_getoken-tare_wt           = ''.
      xs_getoken-net_wt            = ''.
      xs_getoken-net_wt_rec        = ''.
      xs_getoken-net_rec_qty       = ''.
      xs_getoken-gross_wt_uom      = ''.
      xs_getoken-tare_wt_uom       = ''.
      xs_getoken-net_wt_uom        = ''.
      xs_getoken-net_wt_rec_uom    = ''.
      xs_getoken-net_rec_qty_uom   = ''.
      xs_getoken-wb_in_date        = ''.
      xs_getoken-wb_in_time        = ''.
      xs_getoken-wb_out_date       = ''.
      xs_getoken-wb_out_time       = ''.
      xs_getoken-unload_date       = ''.
      xs_getoken-unload_time       = ''.
      xs_getoken-wb_remark         = ''.

      LOOP AT gt_token INTO DATA(gs_item).

        is_item-ebeln         = gs_item-ponum.
        is_item-ebelp         = gs_item-poitem.
        is_item-supplier      = gs_item-supplier.
        is_item-opqty         = gs_item-opqty.
        is_item-docdate       = gs_item-docdate.
        is_item-netprice      = gs_item-netprice.
        is_item-matnr         = gs_item-material.
        is_item-productgroup  = gs_item-material_group.
        is_item-producttype   = gs_item-material_type.
        is_item-uom           = gs_item-material_uom.
        is_item-maktx         = gs_item-material_desc .
        is_item-insplotno     = gs_item-insplotno.
        APPEND is_item TO it_item.

        CLEAR: gs_final.
      ENDLOOP.

      INSERT LINES OF it_item INTO TABLE xs_getoken-item_data.
      APPEND xs_getoken TO et_data.

    ENDIF.

  ENDMETHOD.


  METHOD save_data_get_genum.

    DATA:
      gt_ofinal TYPE TABLE OF zmm_ge_data,
      gs_ofinal TYPE zmm_ge_data.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sys_uname    TYPE c LENGTH 20.

    DATA: lv_doc_date     TYPE d,
          lv_fis_year     TYPE zi_dc_note-fiscalyear,
          lv_doc_month(2) TYPE n.

    sys_date  = cl_abap_context_info=>get_system_date( ).
    sys_time  = cl_abap_context_info=>get_system_time( ).
    sys_uname = cl_abap_context_info=>get_user_technical_name( ).

    READ TABLE xt_gedata INTO DATA(xs_gedata_new) INDEX 1.

    IF xt_gedata[] IS NOT INITIAL.

      lv_doc_date  = sys_date.
      lv_doc_month = lv_doc_date+4(2).
      lv_fis_year  = lv_doc_date+0(4).

      IF lv_doc_month LT 4. "( v_month = '01' OR v_month = '02' OR v_month = '03' ).
        lv_fis_year = lv_fis_year - 1.
      ENDIF.

      IF im_action = 'create'.

        TRY.

            IF xs_gedata_new-plant = '1001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '01'
                  object      = 'ZGATE_NUM'
                IMPORTING
                  number      = DATA(ge_num)
                  returncode  = DATA(rcode).

            ELSEIF xs_gedata_new-plant = '1002'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '02'
                  object      = 'ZGATE_NUM'
                IMPORTING
                  number      = ge_num
                  returncode  = rcode.

            ELSEIF xs_gedata_new-plant = '2001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '03'
                  object      = 'ZGATE_NUM'
                IMPORTING
                  number      = ge_num
                  returncode  = rcode.

            ELSEIF xs_gedata_new-plant = '3001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '04'
                  object      = 'ZGATE_NUM'
                IMPORTING
                  number      = ge_num
                  returncode  = rcode.

            ELSEIF xs_gedata_new-plant = '4001'.

              CALL METHOD cl_numberrange_runtime=>number_get
                EXPORTING
                  nr_range_nr = '05'
                  object      = 'ZGATE_NUM'
                IMPORTING
                  number      = ge_num
                  returncode  = rcode.

            ENDIF.

          CATCH cx_nr_object_not_found ##NO_HANDLER.
          CATCH cx_number_ranges ##NO_HANDLER.
        ENDTRY.

      ENDIF.

      LOOP AT xt_gedata INTO DATA(xs_gedata).

        MOVE-CORRESPONDING xs_gedata TO gs_final.
        IF im_action = 'change'.
          ge_num = gs_final-gentry_num.
        ENDIF.

        LOOP AT xs_gedata-item_data INTO DATA(ls_item).

          SHIFT ge_num LEFT DELETING LEADING '0'.
          gs_final-gentry_num  = ge_num.
          gs_final-gentry_year = lv_fis_year.

          gs_final-ponum           = ls_item-ebeln.
          gs_final-poitem          = ls_item-ebelp.
          gs_final-supplier        = ls_item-supplier.
          gs_final-material        = ls_item-matnr.
          gs_final-material_desc   = ls_item-maktx.
          gs_final-material_group  = ls_item-productgroup.
          gs_final-material_uom    = ls_item-uom.
          gs_final-material_type   = ls_item-producttype.
          gs_final-opqty           = ls_item-opqty.
          gs_final-docdate         = ls_item-docdate.
          gs_final-netprice        = ls_item-netprice.

          gs_final-bagtype         = ls_item-bagtype.
          gs_final-bardanawht      = ls_item-bardanawht.
          gs_final-invqty          = ls_item-invqty.
          gs_final-invqtybag       = ls_item-invqtybag.
          gs_final-noofbag         = ls_item-noofbag.
          gs_final-totalbardana    = ls_item-totalbardana.
          gs_final-totaldeduc      = ls_item-totaldeduc.
          gs_final-actualqty       = ls_item-actualqty.
          gs_final-deduction       = ls_item-deduction.

          gs_final-erdat       = sys_date.
          gs_final-uzeit       = sys_time.
          gs_final-cuname      = sys_uname.
          APPEND gs_final TO gt_final.

          """***Start:Data Preperation for gate entry table(PO-Based)
          gs_ofinal-gentry_num     = gs_final-gentry_num.
          gs_ofinal-gentry_year    = gs_final-gentry_year.
          gs_ofinal-ponum          = gs_final-ponum.
          gs_ofinal-poitem         = gs_final-poitem.
          gs_ofinal-mblnr          = gs_final-mblnr.
          gs_ofinal-mjahr          = gs_final-mjahr.
          gs_ofinal-created_on     = sys_date.
          gs_ofinal-created_time   = sys_time.
          "*gs_ofinal-out_date       = ''.
          "*gs_ofinal-out_time       = ''.
          gs_ofinal-werks          = gs_final-plant .
          gs_ofinal-lifnr          = gs_final-supplier .
          gs_ofinal-supplier       = gs_final-supplier .
          gs_ofinal-vechnum        = gs_final-vehicle_no.
          gs_ofinal-transporter    = gs_final-transporter.
          gs_ofinal-driver_name    = gs_final-driver_name.
          gs_ofinal-lr_num         = gs_final-lr_no.

          gs_ofinal-lr_date        = gs_final-lr_date+0(4) &&
                                     gs_final-lr_date+5(2) &&
                                     gs_final-lr_date+8(2).

          gs_ofinal-token_type     = gs_final-token_type.
          gs_ofinal-gross_wgt      = gs_final-gross_wt.
          gs_ofinal-tare_wgt       = gs_final-tare_wt.
          gs_ofinal-net_wgt        = gs_final-net_wt.
          gs_ofinal-matnr          = gs_final-material.
          gs_ofinal-maktx          = gs_final-material_desc.
          gs_ofinal-erdat          = sys_date.
          gs_ofinal-uzeit          = sys_time.
          gs_ofinal-cuname         = ''.
          gs_ofinal-gedeleted      = ''.

          gs_ofinal-created_on    = sys_date.
          gs_ofinal-created_time  = sys_time.
          "*gs_ofinal-out_date      = ''.
          "*gs_ofinal-out_time      = ''.
          gs_ofinal-billnum       = gs_final-invoice_no.
          gs_ofinal-billdate      = gs_final-invoice_date+0(4) && gs_final-invoice_date+5(2) && gs_final-invoice_date+8(2).
          gs_ofinal-ewaybill_num  = ''.
          gs_ofinal-driver_num    = ''.
          gs_ofinal-trans_mode    = ''.
          gs_ofinal-weight_uom    = ''.
          gs_ofinal-check_rc      = ''.
          gs_ofinal-check_pollt   = ''.
          gs_ofinal-check_tripal  = ''.
          gs_ofinal-check_insur   = ''.
          gs_ofinal-check_dl      = ''.
          gs_ofinal-uname         = ''.
          gs_ofinal-itemdesc      = ''.
          "*gs_ofinal-docdate       = ''.
          gs_ofinal-poqty         = ''.
          gs_ofinal-uom           = ''.
          gs_ofinal-ovrtol        = ''.
          gs_ofinal-netprice      = ''.
          gs_ofinal-currcy        = ''.
          gs_ofinal-perqty        = ''.
          gs_ofinal-openqty       = ''.
          gs_ofinal-valtyp        = ''.
          gs_ofinal-delnoteqty    = ''.
          gs_ofinal-challnqty     = gs_final-invqty.
          gs_ofinal-actualqty     = gs_final-actualqty.
          gs_ofinal-sloc          = ''.
          gs_ofinal-mweight       = ''.

          APPEND gs_ofinal TO gt_ofinal.
          """***End:Data Preperation for gate entry table(PO-Based)

          CLEAR: ls_item.
        ENDLOOP.

        CLEAR: xs_gedata, gs_ofinal.
      ENDLOOP.

      IF gt_final[] IS NOT INITIAL.

        IF im_action = 'create'.

          INSERT zmm_ge_token FROM TABLE @gt_final.
          IF sy-subrc EQ 0.
            INSERT zmm_ge_data FROM TABLE @gt_ofinal.
            CONCATENATE 'Gate entry number' ge_num 'generated successfully' INTO rv_ge_num SEPARATED BY space ##NO_TEXT.
          ENDIF.

        ELSEIF im_action = 'change'.

*          DATA:
*            lv_index TYPE sy-tabix.
*
*          SELECT * FROM zmm_ge_token
*                   WHERE gentry_num = @gs_final-gentry_num AND gedeleted = @abap_false AND mblnr NE ''
*                   INTO TABLE @DATA(lt_ge_data). "#EC CI_ALL_FIELDS_NEEDED
*
*          IF lt_ge_data[] IS NOT INITIAL.
*
*            SELECT * FROM zi_grn_detail
*                     FOR ALL ENTRIES IN @lt_ge_data
*                     WHERE reversedmaterialdocument = @lt_ge_data-mblnr AND
*                           goodsmovementtype = '102'
*                     INTO TABLE @DATA(gt_mdoc). "#EC CI_ALL_FIELDS_NEEDED
*
*            LOOP AT lt_ge_data INTO DATA(ls_ge_data).
*              lv_index = sy-tabix.
*
*              READ TABLE gt_mdoc INTO DATA(wa_mkpf1)
*                             WITH KEY reversedmaterialdocument = ls_ge_data-mblnr
*                                             goodsmovementtype = '102'.
*              IF sy-subrc EQ 0.
*                DELETE lt_ge_data INDEX lv_index.
*              ENDIF.
*
*              CLEAR: ls_ge_data.
*            ENDLOOP.
*
*          ENDIF.
*
*          IF lt_ge_data[] IS INITIAL.

          MODIFY zmm_ge_token FROM TABLE @gt_final.
          IF sy-subrc EQ 0.
            MODIFY zmm_ge_data FROM TABLE @gt_ofinal.
            CONCATENATE 'Gate entry number' ge_num 'updated successfully' INTO rv_ge_num SEPARATED BY space ##NO_TEXT.
          ENDIF.

*          ELSE.
*
*            CONCATENATE 'Changes not allowed, MIGO alreday posted against gate entry' ge_num INTO rv_ge_num SEPARATED BY space ##NO_TEXT.
*
*          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
