CLASS zcl_edoc_badi_impl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES if_edoc_adaptor_cloud .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_EDOC_BADI_IMPL IMPLEMENTATION.


  METHOD if_edoc_adaptor_cloud~change_edocument_type.

  ENDMETHOD.


  METHOD if_edoc_adaptor_cloud~change_form.
  ENDMETHOD.


  METHOD if_edoc_adaptor_cloud~get_variable_key.
  ENDMETHOD.


  METHOD if_edoc_adaptor_cloud~is_relevant.
*   CV_RELEVANT = 'X'.
  ENDMETHOD.


  METHOD if_edoc_adaptor_cloud~restrict_cancel.
  ENDMETHOD.


  METHOD if_edoc_adaptor_cloud~set_output_data.

    ASSIGN COMPONENT 'REQUEST'  OF STRUCTURE cs_output_data TO FIELD-SYMBOL(<ld_add>).
    ASSIGN COMPONENT 'DOC_DATA' OF STRUCTURE <ld_add> TO FIELD-SYMBOL(<ld_doc_data>).
    ASSIGN COMPONENT 'DOC_DTLS' OF STRUCTURE <ld_doc_data> TO FIELD-SYMBOL(<ld_doc_dtls>).
    ASSIGN COMPONENT 'NO'   OF STRUCTURE <ld_doc_dtls> TO FIELD-SYMBOL(<ld_doc_no>).
    ASSIGN COMPONENT 'TYP' OF STRUCTURE <ld_doc_dtls> TO FIELD-SYMBOL(<ld_doc_type>).

    ASSIGN COMPONENT 'VAL_DTLS' OF STRUCTURE <ld_doc_data> TO FIELD-SYMBOL(<ld_val_dtls>).
    ASSIGN COMPONENT 'TOT_INV_VAL'   OF STRUCTURE <ld_val_dtls> TO FIELD-SYMBOL(<ld_tot_inv_val>).

    IF <ld_doc_no> IS ASSIGNED.
      IF <ld_doc_type> = 'CRN'.

        SELECT SINGLE
               BillingDocument,
               doc_type,
               DocumentReferenceID
               FROM zi_sale_reg
               WHERE DocumentReferenceID = @<ld_doc_no>
               INTO @DATA(ls_sal_reg).

        IF sy-subrc EQ 0.
          <ld_doc_type> = ls_sal_reg-doc_type.
        ENDIF.

      ENDIF.
    ENDIF.

    IF <ld_tot_inv_val> IS ASSIGNED.

      DATA:
        tot_qty   TYPE p LENGTH 16 DECIMALS 2.

      SELECT SINGLE
         BillingDocument,
         SUM( item_grandtotalamount_inr ) AS part_grandtotalamount_inr
         FROM zi_sale_reg
         WHERE DocumentReferenceID = @<ld_doc_no>
         GROUP BY BillingDocument
         INTO @DATA(ls_sal).

      tot_qty = ls_sal-part_grandtotalamount_inr.
      <ld_tot_inv_val> = tot_qty.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
