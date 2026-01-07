CLASS zcl_mrm_badi_cloud DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_ex_mrm_check_invoice_cloud .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MRM_BADI_CLOUD IMPLEMENTATION.


  METHOD if_ex_mrm_check_invoice_cloud~check_invoice.

    DATA: ls_msg LIKE LINE OF messages,
          lv_msg TYPE string.

    IF action = if_ex_mrm_check_invoice_cloud=>c_action_simulate OR
       "action = if_ex_mrm_check_invoice_cloud=>c_action_check OR
       action = if_ex_mrm_check_invoice_cloud=>c_action_post.

      LOOP AT itemswithporeference INTO DATA(ls_itm).

        SELECT SINGLE
        InspectionLot,
        Material,
        Plant
        FROM i_inspectionlot WITH PRIVILEGED ACCESS
        WHERE PurchasingDocument = @ls_itm-purchaseorder AND
        PurchasingDocumentItem = @ls_itm-purchaseorderitem AND
        MaterialDocument = @ls_itm-referencedocument AND
        MaterialDocumentYear = @ls_itm-referencedocumentfiscalyear AND
        Material             = @ls_itm-purchaseorderitemmaterial
        INTO @DATA(ls_insplot).

        IF sy-subrc EQ 0.

          SELECT SINGLE
          InspectionLot,
          inspectionlotusagedecisioncode
          FROM i_insplotusagedecision WITH PRIVILEGED ACCESS
          WHERE InspectionLot = @ls_insplot-InspectionLot
          INTO @DATA(ls_lot_desc).

          IF ls_lot_desc-InspectionLotUsageDecisionCode IS INITIAL.

            CONCATENATE 'Quality not cleared for material' ls_itm-purchaseorderitemmaterial INTO lv_msg SEPARATED BY space ##NO_TEXT.
            APPEND VALUE #( messagetype = if_ex_mrm_check_invoice_cloud=>c_messagetype_error
            messageid        = 'MRM_BADI_CLOUD'
            messagenumber    = '001'
            messagevariable1 = lv_msg ) TO messages.

          ENDIF.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
