CLASS zcl_custom_pp_print DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      gt_final TYPE TABLE OF zstr_insplot_print_hdr,
      lt_item  TYPE TABLE OF zstr_insplot_print_itm,
      ls_item  TYPE zstr_insplot_print_itm.

    DATA:
      sys_date     TYPE d  , " VALUE cl_abap_context_info ,
      sys_time     TYPE t  , "  VALUE  cl_abap_context_info=>get_system_time( 8,0 ),
      sys_timezone TYPE timezone,
      sy_uname     TYPE c LENGTH 20.

    DATA:
      lv_char10  TYPE c LENGTH 10,
      lv_char120 TYPE c LENGTH 120.

    METHODS:
      get_insplot_data
        IMPORTING
                  im_lotnum       TYPE zi_lot_print_data-InspectionLot
                  im_sel_scr      TYPE zstr_lot_print_sel
        RETURNING VALUE(et_final) LIKE gt_final,

      prep_xml_insplot_print
        IMPORTING
                  it_final             LIKE gt_final
                  iv_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CUSTOM_PP_PRINT IMPLEMENTATION.


  METHOD get_insplot_data.

    DATA: gt_insplot     TYPE TABLE OF zstr_insplot_print_hdr,
          gs_insplot     TYPE zstr_insplot_print_hdr,
          gt_lotitem     TYPE TABLE OF zstr_insplot_print_itm,
          gs_lotitem     TYPE zstr_insplot_print_itm,
          lv_res_val     TYPE p LENGTH 16 DECIMALS 2,
          lv_val_char    TYPE c LENGTH 20,

          lv_lower_val1  TYPE string,
          lv_upper_val1  TYPE string,
          lv_lower_val   TYPE string,
          lv_upper_val   TYPE string,
          lv_decimal_val TYPE i,

          lv_upr_val     TYPE p LENGTH 16 DECIMALS 2,
          lv_lwr_val     TYPE p LENGTH 16 DECIMALS 2.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT * FROM zi_insp_lot_rej WHERE InspectionLot = @im_lotnum INTO TABLE @DATA(gt_lot).
    IF gt_lot[] IS NOT INITIAL.

      SELECT
      InspectionLot,
      InspPlanOperationInternalID,
      InspectionCharacteristic,
      InspectionMethod,
      InspectionMethodPlant,
      InspCharcCreatedBy,
      InspectionSpecification,
      InspectionCharacteristicText,
      InspSpecUpperLimit,
      InspSpecLowerLimit,
      InspSpecDecimalPlaces,
      inspspechaslowerlimit,
      inspspechasUpperlimit
      FROM I_InspectionCharacteristic
      FOR ALL ENTRIES IN @gt_lot
      WHERE InspectionLot = @gt_lot-InspectionLot
      INTO TABLE @DATA(gt_lot_spec). "#EC CI_NO_TRANSFORM

      SELECT
        InspectionLot,
        InspPlanOperationInternalID,
        InspectionCharacteristic,
        InspResultValueInternalID,
        InspectionResultAttribute,
        InspResultItemInternalID,
        InspectionSubsetInternalID,
        InspectionResultMeasuredValue,
        InspResultHasMeasuredValue,
        InspectionResultOriginalValue,
        InspectionValuationResult,
        Inspector,
        InspectionStartDate,
        InspectionStartTime,
        InspectionEndDate,
        InspectionEndTime,
        InspectionNumberOfDefects,
        DefectClass,
        InspResultNrOfAddlDcmlsPlaces
        FROM I_InspectionResultValue
      FOR ALL ENTRIES IN @gt_lot
      WHERE InspectionLot = @gt_lot-InspectionLot
      INTO TABLE @DATA(gt_lot_res). "#EC CI_NO_TRANSFORM

      SELECT
          InspectionLot,
          InspPlanOperationInternalID,
          InspectionCharacteristic,
          Inspector,
          InspectionResultStatus,
          InspResultDynModifValuation,
          InspectionResultMeanValue   ,
          InspectionResultHasMeanValue,
          InspectionResultMaximumValue,
          InspResultHasMaximumValue,
          InspectionResultMinimumValue,
          InspResultHasMinimumValue,
          InspectionResultOriginalValue,
          InspResultValidValuesNumber,
          InspResultNmbrOfRecordedRslts,
          InspectionResultText,
          InspectionResultHasLongText,
          InspectionValuationResult,
          CharacteristicAttributeCode
          FROM I_InspectionResult
        FOR ALL ENTRIES IN @gt_lot
        WHERE InspectionLot = @gt_lot-InspectionLot
        INTO TABLE @DATA(gt_lot_resn). "#EC CI_NO_TRANSFORM


      SELECT
      InspectionLot,
      InspectionOperation,
      InspectionCharacteristic,
      InspectionSpecificationText
        FROM I_InspectionLotValueHelp
      FOR ALL ENTRIES IN @gt_lot
      WHERE InspectionLot = @gt_lot-InspectionLot
      INTO TABLE @DATA(gt_lot_help). "#EC CI_NO_TRANSFORM

    if gt_lot_spec[] is NOT INITIAL.
      SELECT
      InspectionMethod,
      InspectionMethodPlant,
      InspectionMethodSearchField
      FROM I_InspectionMethodVersion
      FOR ALL ENTRIES IN @gt_lot_spec
      WHERE InspectionMethod = @gt_lot_spec-InspectionMethod AND
            InspectionMethodPlant = @gt_lot_spec-InspectionMethodPlant
      INTO TABLE @DATA(gt_lot_ver). "#EC CI_NO_TRANSFORM
    endif.

    ENDIF.


    ""***Header Data
    READ TABLE gt_lot INTO DATA(ls_lot) INDEX 1.
    IF sy-subrc EQ 0.
      READ TABLE gt_lot_spec INTO DATA(gs_lot_spec) WITH KEY InspectionLot = ls_lot-InspectionLot.
    ENDIF.


    gs_insplot-part_no          = ls_lot-material. "ManufacturerPartNmbr.
    gs_insplot-part_name        = ls_lot-ProductDescription.
    gs_insplot-plant            = ls_lot-Plant.
    gs_insplot-supplier         = ls_lot-SupplierName.

    if ls_lot-Plant = '4001'.
     gs_insplot-header       = 'BHAGWATI AGROCHEM PVT. LTD.'.
     gs_insplot-header_text  = 'M-14A INDUSTRIAL AREA MPIDC MOHASA BABAI NARMDAPURAM-461661 (MADHYA PARDESH)'.
    else.
     gs_insplot-header       = 'BHAGWATI LACTO VEGETARIAN EXPORTS PVT. LTD.'.
     gs_insplot-header_text  = '18 Old Anaj Mandi, Ferozepur Cantt. - 152001 Punjab, India.'.
    ENDIF.

    IF ls_lot-Customer IS NOT INITIAL.

      SELECT SINGLE Customer,
                    CustomerName
                    FROM I_Customer WHERE Customer = @ls_lot-Customer
                    INTO @DATA(ls_cust).

      gs_insplot-customer         = ls_cust-CustomerName.

    ENDIF.

    gs_insplot-invoice_no       = ls_lot-DeliveryDocument.

    gs_insplot-date             = ls_lot-InspLotCreatedOnLocalDate+6(2) && '.'
                                    && ls_lot-InspLotCreatedOnLocalDate+4(2) && '.'
                                    && ls_lot-InspLotCreatedOnLocalDate+0(4).

    gs_insplot-sheet            = ''.

    gs_insplot-r_recvng_insp    = im_sel_scr-r_recvng_insp.
    gs_insplot-r_new_devlmnt    = im_sel_scr-r_new_devlmnt.
    gs_insplot-r_design_chng    = im_sel_scr-r_design_chng.
    gs_insplot-r_tooling_chng   = im_sel_scr-r_tooling_chng.

    gs_insplot-remark           = im_sel_scr-remark.
    gs_insplot-insp_lot_qty     = ls_lot-InspectionLotQuantity.
    IF ls_lot-InspectionLotUsageDecisionCode = 'R1'.
      gs_insplot-rejected         = ls_lot-InspectionLotUsageDecisionCode.
    ELSEIF ls_lot-InspectionLotUsageDecisionCode = 'A1'.
      gs_insplot-accepted        = ls_lot-InspectionLotUsageDecisionCode.
    ELSEIF ls_lot-InspectionLotUsageDecisionCode = 'A2'.
      gs_insplot-cond_acceptd     = ls_lot-InspectionLotUsageDecisionCode.
    ELSEIF ls_lot-InspectionLotUsageDecisionCode = 'A3'.
      gs_insplot-cond_acceptd     = ls_lot-InspectionLotUsageDecisionCode.
    ELSEIF ls_lot-InspectionLotUsageDecisionCode = 'A4'.
      gs_insplot-cond_acceptd     = ls_lot-InspectionLotUsageDecisionCode.
    ENDIF.

*    SELECT SINGLE
*    UserID,
*    FullName
*    FROM I_Userdetails WHERE UserID = @gs_lot_spec-InspCharcCreatedBy
*    INTO @DATA(ls_user).

    gs_insplot-inspected_by     = gs_lot_spec-InspCharcCreatedBy.
    SELECT SINGLE UserID , userdescription  FROM zi_user
                    WHERE UserID = @gs_lot_spec-InspCharcCreatedBy
                    INTO @DATA(ls_user_req).
    IF sy-subrc EQ 0.
      gs_insplot-inspected_by     = ls_user_req-UserDescription.
    ENDIF.

    gs_insplot-approved_by      = ls_lot-InspectionLotUsageDecidedBy.
    IF ls_lot-InspectionLotType = '01'  ##NO_TEXT.
      gs_insplot-approved_by      = ''  ##NO_TEXT.
    ELSEIF ls_lot-InspectionLotType = '04'  ##NO_TEXT.
      gs_insplot-approved_by      = ''  ##NO_TEXT.
    ENDIF.

    gs_insplot-insp_lot_no      = ls_lot-InspectionLot.
    gs_insplot-batch_no         = ls_lot-Batch.
    gs_insplot-grn_no           = ls_lot-MaterialDocument.

    """Item Data
    CLEAR: gs_lot_spec.
    LOOP AT gt_lot_spec INTO gs_lot_spec.

      gs_lotitem-sr_num           = gs_lot_spec-InspectionCharacteristic.
      gs_lotitem-characteristics  = gs_lot_spec-InspectionCharacteristicText."gs_lot_spec-InspectionCharacteristic.


      READ TABLE gt_lot_help INTO DATA(gs_lot_help) WITH KEY
                                  InspectionLot             = gs_lot_spec-InspectionLot
                                  InspectionCharacteristic  = gs_lot_spec-InspectionCharacteristic.
*      IF sy-subrc EQ 0.
*        gs_lotitem-specification    = gs_lot_help-InspectionSpecificationText.
*      ENDIF.

      lv_decimal_val    = 5. "gs_lot_spec-InspSpecDecimalPlaces.

      lv_lower_val      = gs_lot_spec-InspSpecLowerLimit.
      lv_lower_val1     = lv_lower_val+20(2).
      lv_lower_val      = lv_lower_val+0(lv_decimal_val).


      lv_upper_val      = gs_lot_spec-InspSpecUpperLimit.
      lv_upper_val1     = lv_upper_val+20(2).
      lv_upper_val      = lv_upper_val+0(lv_decimal_val).

      CONDENSE: lv_lower_val, lv_upper_val, lv_lower_val1, lv_upper_val1.

      IF lv_lower_val1 = '01'.
        lv_lwr_val  = lv_lower_val * 10.
      ELSEIF lv_lower_val1 = '02'.
        lv_lwr_val  = lv_lower_val * 100.
      else.
        lv_lwr_val  = lv_lower_val * 1.
      ENDIF.

      IF lv_upper_val1 = '01'.
        lv_upr_val  = lv_upper_val * 10.
      ELSEIF lv_upper_val1 = '02'.
        lv_upr_val  = lv_upper_val * 100.
      else.
        lv_upr_val  = lv_upper_val * 1.
      ENDIF.

      CLEAR: lv_lower_val, lv_upper_val.
      lv_lower_val = lv_lwr_val.
      lv_upper_val = lv_upr_val.

      "gs_lotitem-specification    = |{ lv_lower_val+0(lv_decimal_val) } - { lv_upper_val+0(lv_decimal_val) }|.
      gs_lotitem-specification    = |{ lv_lower_val } - { lv_upper_val }|.

      IF gs_lot_spec-InspSpecHasLowerLimit IS INITIAL AND gs_lot_spec-InspSpecHasUpperLimit IS INITIAL.
        gs_lotitem-specification    = 'Quality lnspection Result'  ##NO_TEXT.
      ENDIF.

      READ TABLE gt_lot_ver INTO DATA(gs_lot_ver) WITH KEY
                                 InspectionMethod      = gs_lot_spec-InspectionMethod
                                 InspectionMethodPlant = gs_lot_spec-InspectionMethodPlant.
      IF sy-subrc EQ 0.
        gs_lotitem-method_of_insp   = gs_lot_ver-InspectionMethodSearchField.
      ENDIF.

      DATA(lt_lot_res) = gt_lot_res[].
      DELETE lt_lot_res WHERE InspPlanOperationInternalID NE gs_lot_spec-InspPlanOperationInternalID.
      DELETE lt_lot_res WHERE InspectionCharacteristic NE gs_lot_spec-InspectionCharacteristic.

      DATA: l_decfloat TYPE decfloat34.
      TYPES ty_char8 TYPE c LENGTH 8.

      SORT lt_lot_res BY InspectionStartDate InspectionStartTime DESCENDING.
      LOOP AT lt_lot_res INTO DATA(ls_lot_res).

        l_decfloat = ls_lot_res-InspectionResultMeasuredValue.
        DATA(l_character) = CONV ty_char8( CONV string( l_decfloat ) ).
        lv_res_val  = l_character.
        lv_val_char = lv_res_val.
        CONDENSE lv_val_char.

        IF sy-tabix EQ 1.
          gs_lotitem-insp_1           = lv_val_char.
        ELSEIF sy-tabix EQ 2.
          gs_lotitem-insp_2           = lv_val_char.
        ELSEIF sy-tabix EQ 3.
          gs_lotitem-insp_3           = lv_val_char.
        ELSEIF sy-tabix EQ 4.
          gs_lotitem-insp_4           = lv_val_char.
        ELSEIF sy-tabix EQ 5.
          gs_lotitem-insp_5           = lv_val_char.
        ENDIF.

        CLEAR: ls_lot_res, lv_val_char, lv_res_val, l_character, l_decfloat.
      ENDLOOP.

      READ TABLE gt_lot_resn INTO DATA(gs_lot_resn) WITH KEY
                                  InspectionLot               = gs_lot_spec-InspectionLot
                                  InspPlanOperationInternalID = gs_lot_spec-InspPlanOperationInternalID
                                  InspectionCharacteristic    = gs_lot_spec-InspectionCharacteristic.

      IF sy-subrc EQ 0 AND gs_lot_resn-InspectionValuationResult   = 'R'.
        gs_lotitem-disposition      = 'NG'.
      ELSEIF gs_lot_resn-InspectionValuationResult   = 'A'.
        gs_lotitem-disposition      = 'OK'.
      ENDIF.

      CLEAR: gs_lot_resn.
      READ TABLE gt_lot_resn INTO gs_lot_resn WITH KEY
                                  InspectionLot               = gs_lot_spec-InspectionLot
                                  InspPlanOperationInternalID = gs_lot_spec-InspPlanOperationInternalID
                                  InspectionCharacteristic    =  gs_lot_spec-InspectionCharacteristic
                                  CharacteristicAttributeCode = '01'.
      IF sy-subrc EQ 0.
        gs_lotitem-insp_1           = 'OK'.
        gs_lotitem-insp_2           = 'OK'.
        gs_lotitem-insp_3           = 'OK'.
        gs_lotitem-insp_4           = 'OK'.
        gs_lotitem-insp_5           = 'OK'.
      ENDIF.

      CLEAR: gs_lot_resn.
      READ TABLE gt_lot_resn INTO gs_lot_resn WITH KEY
                                  InspectionLot               = gs_lot_spec-InspectionLot
                                  InspPlanOperationInternalID = gs_lot_spec-InspPlanOperationInternalID
                                  InspectionCharacteristic    =  gs_lot_spec-InspectionCharacteristic
                                  CharacteristicAttributeCode = '02'.
      IF sy-subrc EQ 0.
        gs_lotitem-insp_1           = 'NG'.
        gs_lotitem-insp_2           = 'NG'.
        gs_lotitem-insp_3           = 'NG'.
        gs_lotitem-insp_4           = 'NG'.
        gs_lotitem-insp_5           = 'NG'.
      ENDIF.

      CLEAR: gs_lot_resn.
      loop at gt_lot_resn INTO gs_lot_resn WHERE
                                  InspectionLot               = gs_lot_spec-InspectionLot AND
                                  InspPlanOperationInternalID = gs_lot_spec-InspPlanOperationInternalID AND
                                  InspectionCharacteristic    =  gs_lot_spec-InspectionCharacteristic.

      if gs_lot_resn-CharacteristicAttributeCode = '01' or
         gs_lot_resn-CharacteristicAttributeCode = '02'.

      else.
        gs_lotitem-insp_1           = gs_lot_resn-InspectionResultOriginalValue.
        gs_lotitem-insp_2           = gs_lot_resn-InspectionResultOriginalValue.
        gs_lotitem-insp_3           = gs_lot_resn-InspectionResultOriginalValue.
        gs_lotitem-insp_4           = gs_lot_resn-InspectionResultOriginalValue.
        gs_lotitem-insp_5           = gs_lot_resn-InspectionResultOriginalValue.

      ENDIF.

      CLEAR: gs_lot_resn.
      ENDLOOP.

      APPEND gs_lotitem TO gt_lotitem.
      CLEAR: gs_lot_spec, lt_lot_res, gs_lot_resn, gs_lot_help, gs_lot_ver, gs_lotitem.
    ENDLOOP.

    INSERT LINES OF gt_lotitem INTO TABLE gs_insplot-lot_item.
    APPEND gs_insplot TO gt_insplot.

    et_final[] = gt_insplot[].

  ENDMETHOD.


  METHOD prep_xml_insplot_print.

    DATA : heading      TYPE c LENGTH 100,
           sub_heading  TYPE c LENGTH 200,
           lv_xml_final TYPE string.

    READ TABLE it_final INTO DATA(ls_final) INDEX 1.
    "REPLACE ALL OCCURRENCES OF '&' IN ls_final-suppl_name WITH 'AND' ##NO_TEXT .
    SHIFT ls_final-insp_lot_qty LEFT DELETING LEADING ''.

    DATA(lv_xml) =  |<Form>| &&
                    |<InspectionNode>| &&
                    |<header>{ ls_final-header }</header>| &&
                    |<header_text>{ ls_final-header_text }</header_text>| &&
                    |<part_no>{ ls_final-part_no }</part_no>| &&
                    |<part_name>{ ls_final-part_name }</part_name>| &&
                    |<plant>{ ls_final-plant  }</plant>| &&
                    |<supplier>{ ls_final-supplier }</supplier>| &&
                    |<customer>{ ls_final-customer }</customer>| &&
                    |<invoice_no>{ ls_final-invoice_no }</invoice_no>| &&
                    |<date>{ ls_final-date }</date>| &&
                    |<sheet>{ ls_final-sheet }</sheet>| &&
                    |<r_recvng_insp>{ ls_final-r_recvng_insp }</r_recvng_insp>| &&
                    |<r_new_devlmnt>{ ls_final-r_new_devlmnt }</r_new_devlmnt>| &&
                    |<r_design_chng>{ ls_final-r_design_chng }</r_design_chng>| &&
                    |<r_tooling_chng>{ ls_final-r_tooling_chng }</r_tooling_chng>| &&
                    |<insp_lot_qty>{ ls_final-insp_lot_qty }</insp_lot_qty>| &&
                    |<remark>{ ls_final-remark }</remark>| &&
                    |<accepted>{ ls_final-accepted }</accepted>| &&
                    |<rejected>{ ls_final-rejected }</rejected>| &&
                    |<Cond_accepted>{ ls_final-cond_acceptd }</Cond_accepted>| &&
                    |<inspected_by>{ ls_final-inspected_by }</inspected_by>| &&
                    |<approved_by>{ ls_final-approved_by }</approved_by>| &&
                    |<insp_lot_no>{ ls_final-insp_lot_no }</insp_lot_no>| &&
                    |<batch_no>{ ls_final-batch_no }</batch_no>| &&
                    |<grn_no>{ ls_final-grn_no }</grn_no>| &&
                    |<ItemData>|  ##NO_TEXT.

    DATA : lv_item TYPE string .
    DATA : srn TYPE c LENGTH 3 .
    CLEAR : lv_item , srn .

    LOOP AT ls_final-lot_item INTO DATA(ls_item).

      srn = srn + 1 .

      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<sr_num>{ srn }</sr_num>| &&
                |<characteristics>{ ls_item-characteristics }</characteristics>| &&
                |<specification>{ ls_item-specification }</specification>| &&
                |<method_of_insp>{ ls_item-method_of_insp }</method_of_insp>| &&
                |<insp_1>{ ls_item-insp_1 }</insp_1>| &&
                |<insp_2>{ ls_item-insp_2 }</insp_2>| &&
                |<insp_3>{ ls_item-insp_3 }</insp_3>| &&
                |<insp_4>{ ls_item-insp_4 }</insp_4>| &&
                |<insp_5>{ ls_item-insp_5 }</insp_5>| &&
                |<disposition>{ ls_item-disposition }</disposition>| &&
                |</ItemDataNode>|  ##NO_TEXT .

    ENDLOOP.

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                       |</InspectionNode>| &&
                       |</Form>| ##NO_TEXT .

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.

  ENDMETHOD.
ENDCLASS.
