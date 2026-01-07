CLASS zcl_po_process DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      xt_final TYPE TABLE OF zstr_po_create_hdr,
      xt_hdr   TYPE TABLE OF zstr_po_create_hdr,
      xs_hdr   TYPE zstr_po_create_hdr,
      xt_item  TYPE TABLE OF zstr_po_create_item,
      xs_item  TYPE zstr_po_create_item.

    METHODS:
      create_purchase_order
        IMPORTING
                  im_input_str      TYPE string
        RETURNING VALUE(et_po_stat) LIKE xt_hdr.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_PO_PROCESS IMPLEMENTATION.


  METHOD create_purchase_order.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sy_uname     TYPE c LENGTH 20.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    /ui2/cl_json=>deserialize(
      EXPORTING json = im_input_str
         pretty_name = /ui2/cl_json=>pretty_mode-camel_case
         CHANGING data = xt_final
                                 ).


    LOOP AT xt_final ASSIGNING FIELD-SYMBOL(<lfs_final>).
     <lfs_final>-postatus      = 'Sucess' ##NO_TEXT.
     <lfs_final>-purchaseorder = '1234567890' ##NO_TEXT.
    ENDLOOP.

    et_po_stat[] = CORRESPONDING #( xt_final[] ).

*    DATA(n) = 1.
*
*    DATA(purchase_order) = VALUE i_purchaseordertp_2(
*      purchaseordertype = 'NB'
*      companycode = '2000'
*      purchasingorganization = '2000'
*      purchasinggroup = '001'
*      supplier = '1000088'
*    ).
*
*    DATA(purchase_order_item) = VALUE  i_purchaseorderitemtp_2(
*        material = '10005'
*        plant = '2001'
*        orderquantity = '50'
*        purchaseorderitem = '10'
*        netpriceamount = '4995'
*    ).
*
*    MODIFY  ENTITIES OF i_purchaseordertp_2
*      ENTITY purchaseorder
*        CREATE SET FIELDS WITH VALUE #( (
*          VALUE #( BASE CORRESPONDING #( purchase_order CHANGING CONTROL ) %cid = |My%CID_{ n }| )
*        ) )
*        CREATE BY \_purchaseorderitem SET FIELDS WITH VALUE #( (
*          %cid_ref = |My%CID_{ n }|
*          %target = VALUE #( ( VALUE #( BASE CORRESPONDING #( purchase_order_item CHANGING CONTROL ) %cid = |My%CIDitem_{ n }| ) ) )
*        ) )
*     REPORTED FINAL(reported)
*     FAILED FINAL(failed)
*     MAPPED FINAL(mapped).
*
*    LOOP AT reported-purchaseorderitem ASSIGNING FIELD-SYMBOL(<reported>).
*      DATA(lv_result) = <reported>-%msg->if_message~get_text( ).
*      "*out->write( lv_result ).
*    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
