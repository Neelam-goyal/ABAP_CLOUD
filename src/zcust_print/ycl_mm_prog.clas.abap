CLASS ycl_mm_prog DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    DATA:
      gt_final TYPE TABLE OF zstr_schd_line_print,
      gs_final TYPE zstr_schd_line_print,
      lt_item  TYPE TABLE OF zstr_schd_line_item,
      ls_item  TYPE zstr_schd_line_item,
      gt_rfq_item TYPE table of zstr_rfq_hdr.

    DATA:
      sys_date     TYPE d,
      sys_time     TYPE t,
      sys_timezone TYPE timezone,
      sy_uname     TYPE c LENGTH 20.

    DATA:
      lv_char10  TYPE c LENGTH 10,
      lv_char120 TYPE c LENGTH 120.

    METHODS:
      get_sa_data
        IMPORTING
                  iv_ebeln        LIKE lv_char10
                  iv_action       LIKE lv_char10
        RETURNING VALUE(et_final) LIKE gt_final,

      prep_xml_schdl_print
        IMPORTING
                  it_final             LIKE gt_final
                  iv_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string,

      get_rfq_data
        IMPORTING
                  im_rfqnum       TYPE zi_rfq_data-requestforquotation
                  im_partcntr     TYPE zi_rfq_data-partnercounter
                  im_supplier     TYPE zi_rfq_data-supplier
                  im_action       LIKE lv_char10
        RETURNING VALUE(et_final) LIKE gt_rfq_item,

      prep_xml_rfq_print
        IMPORTING
                  it_final             LIKE gt_rfq_item
                  im_action            LIKE lv_char10
        RETURNING VALUE(iv_xml_base64) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YCL_MM_PROG IMPLEMENTATION.


  METHOD get_rfq_data.
  data : gt_rfq_hdr type table of zstr_rfq_hdr,
         gs_rfq_hdr TYPE zstr_rfq_hdr,
         gt_rfq_itm TYPE table of zstr_rfq_itm,
         gs_rfq_itm type zstr_rfq_itm.

  SELECT * from I_RfqItem_Api01 where RequestForQuotation = @im_rfqnum
   into TABLE @data(it_rfq_no). "#EC CI_NO_TRANSFORM

 if it_rfq_no is not INITIAL.

  SELECT
  RequestForQuotation,
  CompanyCode,
CreationDate,
IncotermsClassification
  from  I_Requestforquotation_Api01
      FOR ALL ENTRIES IN @it_rfq_no
      where RequestForQuotation = @it_rfq_no-RequestForQuotation
      into TABLE @data(it_rfq_typ). "#EC CI_NO_TRANSFORM

  SELECT
  RequestForQuotation,
  supplier
  from  I_RfqBidder_Api01
    FOR ALL ENTRIES IN @it_rfq_no
    where RequestForQuotation = @it_rfq_no-RequestForQuotation
      and supplier = @im_supplier
   into TABLE @data(it_suplir). "#EC CI_NO_TRANSFORM

if it_suplir[] is NOT INITIAL.
  SELECT
  supplier,
  suppliername,
SupplierFullName,
BPAddrStreetName,
BPAddrCityName,
DistrictName,
PostalCode,
region,
TaxNumber3,
BusinessPartnerPanNumber,
PhoneNumber1
  from I_Supplier FOR ALL ENTRIES IN @it_suplir
    where supplier = @it_suplir-Supplier
     into TABLE  @data(it_suply_data). "#EC CI_NO_TRANSFORM

endif.

  SELECT
  plant,
  plantname
  from I_Plant FOR ALL ENTRIES IN @it_rfq_no
      where plant = @it_rfq_no-plant
      into TABLE @data(it_delv). "#EC CI_NO_TRANSFORM

SELECT
RequestForQuotation,
RequestForQuotationItem,
ScheduleLine,
DelivDateCategory,
ScheduleLineDeliveryDate,
SchedLineStscDeliveryDate,
ScheduleLineDeliveryTime,
ScheduleLineOrderQuantity,
RoughGoodsReceiptQty,
OrderQuantityUnit
from I_RfqScheduleLine_Api01
   FOR ALL ENTRIES IN @it_rfq_no
     where RequestForQuotation = @it_rfq_no-RequestForQuotation
       and RequestForQuotationItem = @it_rfq_no-RequestForQuotationItem
     into TABLE @data(it_itm). "#EC CI_NO_TRANSFORM

 ENDIF.

 data(it_rfq1) = it_rfq_no[].
 sort it_rfq_no by RequestForQuotation.
 delete ADJACENT DUPLICATES FROM it_rfq_no COMPARING RequestForQuotation.
 loop at it_rfq_no into data(wa_rfq_no).
  if wa_rfq_no-Plant = '1001'.
   gs_rfq_hdr-header_1 = 'Request for Quotation' ##NO_TEXT.
   gs_rfq_hdr-header_2 = 'Bhagwati Lacto Vegetarian Exports Pvt Ltd' ##NO_TEXT.
   gs_rfq_hdr-plant_adr1 = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
   gs_rfq_hdr-plant_adr2 = 'Works : Rural Focal Point Mana Singh wala , Firozepur Moga Road' ##NO_TEXT.
   gs_rfq_hdr-plant_adr3 = 'GSTIN : 03AADCB4295Q1ZA , Pan No : AADCB4295Q' ##NO_TEXT.
 ELSEIF wa_rfq_no-Plant = '2001'.
   gs_rfq_hdr-header_1 = 'Request for Quotation' ##NO_TEXT.
   gs_rfq_hdr-header_2 = 'Bhagwati Lacto Foods Pvt Ltd' ##NO_TEXT.
   gs_rfq_hdr-plant_adr1 = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
   gs_rfq_hdr-plant_adr2 = 'Works : VILL. RUKNA MUNGLA, FARIDKOT ROAD, FEROZEPUR, Ferozepur, Punjab, 152001' ##NO_TEXT.
   gs_rfq_hdr-plant_adr3 = 'GSTIN : 03AADCB6737H1ZU , Pan No : AADCB6737H' ##NO_TEXT.
 ELSEIF wa_rfq_no-Plant = '3001'.
   gs_rfq_hdr-header_1 = 'Request for Quotation' ##NO_TEXT.
   gs_rfq_hdr-header_2 = 'Bealthy Harvested Foods Private Limited' ##NO_TEXT.
   gs_rfq_hdr-plant_adr1 = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001' ##NO_TEXT.
   gs_rfq_hdr-plant_adr2 = 'Works : PLOT NO100, SECTOR 1/A, GANDHIDHAM, Kachchh, Gujarat, 370201' ##NO_TEXT.
   gs_rfq_hdr-plant_adr3 = 'GSTIN : 24AAFCH2454K1ZL , Pan No : AAFCH2454K' ##NO_TEXT.
  endif.
 READ TABLE it_rfq_typ into data(wa_rfq_typ) WITH KEY RequestForQuotation = wa_rfq_no-RequestForQuotation.
   gs_rfq_hdr-rfq_no = wa_rfq_typ-RequestForQuotation.
   gs_rfq_hdr-rfq_dat = |{ wa_rfq_typ-CreationDate+6(2) }.{ wa_rfq_typ-CreationDate+4(2) }.{ wa_rfq_typ-CreationDate+0(4) }|.
   gs_rfq_hdr-rfqtyp = wa_rfq_typ-IncotermsClassification.
  READ TABLE it_suplir into data(wa_suplir) WITH KEY RequestForQuotation = wa_rfq_no-RequestForQuotation.
  READ TABLE it_suply_data into data(wa_suply_data) WITH KEY supplier = wa_suplir-Supplier.
   gs_rfq_hdr-suplier = wa_suply_data-supplier.
   gs_rfq_hdr-suply_adr1 = wa_suply_data-SupplierFullName.
   gs_rfq_hdr-suply_adr2 = wa_suply_data-BPAddrStreetName.
   gs_rfq_hdr-suply_adr3 = |{ wa_suply_data-BPAddrCityName } { wa_suply_data-DistrictName } { wa_suply_data-PostalCode } { wa_suply_data-region }|.
   gs_rfq_hdr-suply_gst = wa_suply_data-TaxNumber3.
   gs_rfq_hdr-suply_pan = wa_suply_data-BusinessPartnerPanNumber.
   gs_rfq_hdr-suply_mobile = wa_suply_data-PhoneNumber1.
   gs_rfq_hdr-suply_email = 'www.com' ##NO_TEXT.

   READ TABLE it_delv into data(wa_del) WITH KEY plant = wa_rfq_no-plant .
   if wa_del-Plant = '1001'.
   gs_rfq_hdr-delivery_adr = 'Delivery Address' ##NO_TEXT.
   gs_rfq_hdr-delivery_adr1 = 'Bhagwati Lacto Vegetarian Exports Pvt Ltd ' ##NO_TEXT.
   gs_rfq_hdr-delivery_ad2 = 'Rural Focal Point Mana Singh wala, Firozepur' ##NO_TEXT.
   gs_rfq_hdr-delivery_adr3 = 'Moga Road 152001' ##NO_TEXT.
   gs_rfq_hdr-delivery_mobile = '12345' ##NO_TEXT.
   gs_rfq_hdr-delivery_gst = '03AADCB4295Q1ZA' ##NO_TEXT.
   gs_rfq_hdr-delivery_pan = 'AADCB4295Q' ##NO_TEXT.
   gs_rfq_hdr-delivery_cin = 'U15100UR2003PLC028002' ##NO_TEXT.
   ELSEIF wa_del-Plant = '2001'.
   gs_rfq_hdr-delivery_adr = 'Delivery Address' ##NO_TEXT.
   gs_rfq_hdr-delivery_adr1 = 'Bhagwati Lacto Foods Pvt Ltd' ##NO_TEXT.
   gs_rfq_hdr-delivery_ad2 = 'VILL. RUKNA MUNGLA, FARIDKOT ROAD, FEROZEPUR' ##NO_TEXT.
   gs_rfq_hdr-delivery_adr3 = 'Ferozepur, Punjab, 152001' ##NO_TEXT.
   gs_rfq_hdr-delivery_mobile = '12345' ##NO_TEXT.
   gs_rfq_hdr-delivery_gst = '03AADCB6737H1ZU' ##NO_TEXT.
   gs_rfq_hdr-delivery_pan = 'AADCB6737H' ##NO_TEXT.
   gs_rfq_hdr-delivery_cin = 'U15100UR2003PLC028002' ##NO_TEXT.
   ELSEIF wa_del-Plant = '3001'.
   gs_rfq_hdr-delivery_adr = 'Delivery Address' ##NO_TEXT.
   gs_rfq_hdr-delivery_adr1 = 'Healthy Harvested Foods Private Limited' ##NO_TEXT.
   gs_rfq_hdr-delivery_ad2 = 'PLOT NO100, SECTOR 1/A, GANDHIDHAM' ##NO_TEXT.
   gs_rfq_hdr-delivery_adr3 = 'Kachchh, Gujarat, 370201' ##NO_TEXT.
   gs_rfq_hdr-delivery_mobile = '12345'.
   gs_rfq_hdr-delivery_gst = '24AAFCH2454K1Z' ##NO_TEXT.
   gs_rfq_hdr-delivery_pan = 'AAFCH2454K' ##NO_TEXT.
   gs_rfq_hdr-delivery_cin = 'U15100UR2003PLC028002' ##NO_TEXT.
   ELSEIF wa_del-Plant = '4001'.
   gs_rfq_hdr-delivery_adr = 'Delivery Address' ##NO_TEXT.
   gs_rfq_hdr-delivery_adr1 = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
   gs_rfq_hdr-delivery_ad2 = 'Building No./Flat No.: M-14A Road/Street: VILL-Mohasa Babai city' ##NO_TEXT.
   gs_rfq_hdr-delivery_adr3 = 'Makhan Nagar Distr-Narmadapuram M.P 461661' ##NO_TEXT.
   gs_rfq_hdr-delivery_mobile = '12345' ##NO_TEXT.
   gs_rfq_hdr-delivery_gst = '24AAFCH2454K1Z' ##NO_TEXT.
   gs_rfq_hdr-delivery_pan = 'AAFCH2454K' ##NO_TEXT.
   gs_rfq_hdr-delivery_cin = 'U15100UR2003PLC028002' ##NO_TEXT.
   ENDIF.
*****************itm data********************

 loop at it_rfq1 into data(wa_rfq) where RequestForQuotation = wa_rfq_no-RequestForQuotation.
   gs_rfq_itm-part_cod = wa_rfq-Material.
   gs_rfq_itm-description = wa_rfq-PurchasingDocumentItemText.
 READ TABLE it_itm into  data(wa_itm) WITH KEY RequestForQuotation = wa_rfq-RequestForQuotation
                                               RequestForQuotationItem = wa_rfq-RequestForQuotationItem.
   gs_rfq_itm-del_dat = |{ wa_itm-ScheduleLineDeliveryDate+6(2) }.{ wa_itm-ScheduleLineDeliveryDate+4(2) }.{ wa_itm-ScheduleLineDeliveryDate+0(4) }|.
   gs_rfq_itm-quantity = wa_itm-ScheduleLineOrderQuantity.
   if wa_itm-OrderQuantityUnit = 'ST'.
     gs_rfq_itm-uom = 'PC'.
   ELSE.
   gs_rfq_itm-uom = wa_itm-OrderQuantityUnit.
   ENDIF.
   gs_rfq_itm-remarks = 'Remark' ##NO_TEXT.
 APPEND gs_rfq_itm to gt_rfq_itm.
 clear : gs_rfq_itm.
  endloop.
 INSERT LINES OF gt_rfq_itm INTO TABLE gs_rfq_hdr-del_itm.
 APPEND gs_rfq_hdr to gt_rfq_hdr.
 clear : gs_rfq_hdr.
 ENDLOOP.

 et_final[] = gt_rfq_hdr[].
  ENDMETHOD.


  METHOD get_sa_data.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    SELECT * FROM zi_schedgagrmt_po
             WHERE schedulingagreement = @iv_ebeln
             INTO TABLE @DATA(lt_sa).         "#EC CI_ALL_FIELDS_NEEDED

    IF sy-subrc EQ 0.

      SELECT * FROM i_schdagrschdlnenhcdapi01
      WHERE schedulingagreement = @iv_ebeln
      INTO TABLE @DATA(lt_schedule). "#EC CI_ALL_FIELDS_NEEDED

      DATA(lt_hdr) = lt_sa[].
      SORT lt_hdr BY schedulingagreement.
      DELETE ADJACENT DUPLICATES FROM lt_hdr COMPARING schedulingagreement.

      READ TABLE lt_hdr INTO DATA(lcs_hdr) INDEX 1.

      SELECT SINGLE * FROM zi_plant_address
               WHERE plant = @lcs_hdr-plant
               INTO @DATA(ls_plant_adrs).     "#EC CI_ALL_FIELDS_NEEDED

      SELECT SINGLE * FROM zi_supplier_address
               WHERE supplier = @lcs_hdr-supplier
               INTO @DATA(ls_suppliert_adrs). "#EC CI_ALL_FIELDS_NEEDED

      SELECT SINGLE * FROM i_purchasinggroup
               WHERE purchasinggroup = @lcs_hdr-purchasinggroup
               INTO @DATA(ls_pur_group).      "#EC CI_ALL_FIELDS_NEEDED

      LOOP AT lt_hdr INTO DATA(ls_hdr).

        gs_final-schedulingagreement  = ls_hdr-schedulingagreement.

        gs_final-schdl_date        = ls_hdr-creationdate+6(2) && '.' &&
                                     ls_hdr-creationdate+4(2) && '.' &&
                                     ls_hdr-creationdate+0(4).

        gs_final-plant_code        = ls_hdr-plant.
        gs_final-plant_name        = ls_plant_adrs-plantname.
        gs_final-plant_adrs1       = ls_plant_adrs-streetprefixname1 && ',' && ls_plant_adrs-streetprefixname2.
        gs_final-plant_adrs2       = ls_plant_adrs-streetname &&  ',' && ls_plant_adrs-streetsuffixname1.
        gs_final-plant_adrs3       = ls_plant_adrs-cityname &&  ','  && ls_plant_adrs-postalcode .

        gs_final-header_1          = 'SCHEDULE AGREEMENT' ##NO_TEXT.
        gs_final-header_2          = ls_plant_adrs-plantname. "'ELIN ELECTRONICS LTD, GHAZIABAD'.
        gs_final-header_3          = gs_final-plant_adrs1. "'GZB/ELIN/PUR/010/03 REV.11'.
        gs_final-header_4          = gs_final-plant_adrs2. "'Site No.1, C-142,143,144,144/1,144/2'.
        gs_final-header_5          = gs_final-plant_adrs3. "'C-158, B.S. Road, Industrial Area'.
        gs_final-header_6          = |Email: { ls_plant_adrs-emailaddress } Phone: { ls_plant_adrs-phoneareacodesubscribernumber }| ##NO_TEXT. "'Email: elingoa@gmail.com Phone: 6690934/38/26'.
        gs_final-page_no           = ''.
        gs_final-tot_page          = ''.
        gs_final-contact_person    = ls_pur_group-faxnumber.
        gs_final-contact_phone     = ls_pur_group-phonenumber.
        gs_final-contact_email     = ls_pur_group-emailaddress.
        gs_final-pay_terms         = ls_hdr-paymenttermsname.
        gs_final-plant_adrs4       = ''.
        gs_final-plant_phone       = ''.
        gs_final-plant_email       = ''.
        gs_final-plant_gstin       = '30AAACE6449G1ZY' ##NO_TEXT.
        gs_final-plant_pan         = 'AAACE6449G' ##NO_TEXT.
        gs_final-plant_vat         = ''.
        gs_final-suppl_code        = ls_hdr-supplier.
        gs_final-suppl_name        = ls_suppliert_adrs-suppliername.
        gs_final-suppl_adrs1       = ls_suppliert_adrs-streetprefixname1 && ',' && ls_suppliert_adrs-streetprefixname2..
        gs_final-suppl_adrs2       = ls_suppliert_adrs-streetname &&  ',' && ls_suppliert_adrs-streetsuffixname1 &&  ',' && ls_suppliert_adrs-districtname.
        gs_final-suppl_adrs3       = ls_suppliert_adrs-cityname &&  ',' && ls_suppliert_adrs-postalcode &&  ',' && ls_suppliert_adrs-country.
        gs_final-suppl_adrs4       = ''.
        gs_final-suppl_phone       = ls_suppliert_adrs-phonenumber1.
        gs_final-suppl_email       = ls_suppliert_adrs-emailaddress.
        gs_final-suppl_gstin       = ls_suppliert_adrs-taxnumber3.
        gs_final-suppl_pan         = ls_suppliert_adrs-businesspartnerpannumber.

        LOOP AT lt_sa INTO DATA(ls_sa) WHERE schedulingagreement = ls_hdr-schedulingagreement.

          READ TABLE lt_schedule INTO DATA(ls_schedule) WITH KEY
          schedulingagreement     = ls_sa-schedulingagreement
          schedulingagreementitem = ls_sa-schedulingagreementitem
          scheduleline            = ls_sa-scheduleline.

          IF ls_schedule-schedulelineopenquantity GT 0.

            ls_item-schedulingagreement     = ls_sa-schedulingagreement.
            ls_item-schedulingagreementitem = ls_sa-schedulingagreementitem.
            ls_item-item_code               = ls_sa-material.
            ls_item-item_name               = ls_sa-productdescription.
            ls_item-item_qty                = ls_schedule-schedulelineopenquantity. "ls_sa-ScheduleLineOrderQuantity.

            ls_item-delivery_date           =  ls_sa-schedulelinedeliverydate+6(2) && '.' &&
                                               ls_sa-schedulelinedeliverydate+4(2) && '.' &&
                                               ls_sa-schedulelinedeliverydate+0(4).

            ls_item-net_value               = ls_sa-netpriceamount.

            ls_item-rate                    = ''.
            ls_item-cgst_amt                = ''.
            ls_item-sgst_amt                = ''.
            ls_item-igst_amt                = ''.
            ls_item-tax_value               = ''.
            ls_item-tot_val                 = ''.
            ls_item-unit                    = ''.
            APPEND ls_item TO lt_item.

            gs_final-sum_qty       = gs_final-sum_qty + ls_item-item_qty.
            gs_final-sum_netpr     = gs_final-sum_netpr + ls_item-net_value.

          ENDIF.

          CLEAR: ls_sa, ls_schedule.
        ENDLOOP.

        INSERT LINES OF lt_item INTO TABLE gs_final-sa_item.
        APPEND gs_final TO gt_final.

        CLEAR: ls_hdr.
      ENDLOOP.

    ENDIF.

  ENDMETHOD.


  METHOD prep_xml_rfq_print.

 data : lv_xml_final TYPE string.

    READ TABLE it_final INTO DATA(ls_final) INDEX 1.

    DATA(lv_xml) =  |<Form>| &&
                    |<RequestforQuotation>| &&
                    |<header_1>{ ls_final-header_1 }</header_1>| &&
                    |<header_2>{ ls_final-header_2 }</header_2>| &&
                    |<plant_adr1>{ ls_final-plant_adr1 }</plant_adr1>| &&
                    |<plant_adr2>{ ls_final-plant_adr2 }</plant_adr2>| &&
                    |<plant_adr3>{ ls_final-plant_adr3 }</plant_adr3>| &&
                    |<Suplier>{ ls_final-Suplier }</Suplier>| &&
                    |<suply_adr1>{ ls_final-suply_adr1 }</suply_adr1>| &&
                    |<suply_adr2>{ ls_final-suply_adr2 }</suply_adr2>| &&
                    |<suply_adr3>{ ls_final-suply_adr3 }</suply_adr3>| &&
                    |<suply_gst>{ ls_final-suply_gst }</suply_gst>| &&
                    |<suply_pan>{ ls_final-suply_pan }</suply_pan>| &&
                    |<suply_email>{ ls_final-suply_email }</suply_email>| &&
                    |<suply_Mobile>{ ls_final-suply_Mobile }</suply_Mobile>| &&
                    |<Delivery_adr>{ ls_final-delivery_adr }</Delivery_adr>| &&
                    |<Delivery_adr1>{ ls_final-delivery_adr1 }</Delivery_adr1>| &&
                    |<Delivery_adr2>{ ls_final-delivery_ad2 }</Delivery_adr2>| &&
                    |<Delivery_adr3>{ ls_final-delivery_adr3 }</Delivery_adr3>| &&
                    |<Delivery_mobile>{ ls_final-delivery_mobile }</Delivery_mobile>| &&
                    |<Delivery_gst>{ ls_final-delivery_gst }</Delivery_gst>| &&
                    |<Delivery_pan>{ ls_final-delivery_pan }</Delivery_pan>| &&
                    |<Delivery_cin>{ ls_final-delivery_cin }</Delivery_cin>| &&
                    |<rfq_no>{ ls_final-rfq_no }</rfq_no>| &&
                    |<rfq_dat>{ ls_final-rfq_dat }</rfq_dat>| &&
                    |<rfq_typ>{ ls_final-rfqtyp }</rfq_typ>| &&
                    |<ItemData>| .

    DATA : lv_item TYPE string .
    DATA : srn TYPE c LENGTH 3.
    CLEAR : lv_item , srn .

    LOOP AT ls_final-del_itm INTO DATA(ls_item).

      srn = srn + 1 .

      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<sl_num>{ srn }</sl_num>| &&
                |<part_cod>{ ls_item-part_cod }</part_cod>| &&
                |<description>{ ls_item-description }</description> | &&
                |<brand>{ ls_item-brand }</brand>| &&
                |<del_dat>{ ls_item-del_dat }</del_dat>| &&
                |<quantity>{ ls_item-quantity }</quantity>| &&
                |<uom>{ ls_item-uom }</uom>| &&
                |<remarks>{ ls_item-remarks }</remarks>| &&
                |</ItemDataNode>|  .

    ENDLOOP.

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                       |</RequestforQuotation>| &&
                       |</Form>|.

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.

  ENDMETHOD.


  METHOD prep_xml_schdl_print.

    sys_date = cl_abap_context_info=>get_system_date( ).
    sys_time = cl_abap_context_info=>get_system_time( ).
    sy_uname = cl_abap_context_info=>get_user_technical_name( ).

    CLEAR: gt_final.
    gt_final[] = it_final[].

    READ TABLE gt_final INTO gs_final INDEX 1.

    DATA(lv_xml) =  |<Form>| &&
                    |<ScheduleAgrementNode>| &&
                    |<header_1>{ gs_final-header_1 }</header_1>| &&
                    |<header_2>{ gs_final-header_2 }</header_2>| &&
                    |<header_3>{ gs_final-header_3 }</header_3>| &&
                    |<header_4>{ gs_final-header_4 }</header_4>| &&
                    |<header_5>{ gs_final-header_5 }</header_5>| &&
                    |<header_6>{ gs_final-header_6 }</header_6>| &&
                    |<schdl_agrno>{ gs_final-schedulingagreement }</schdl_agrno>| &&
                    |<schdl_date>{ gs_final-schdl_date }</schdl_date>| &&
                    |<page_no>{ gs_final-page_no }</page_no>| &&
                    |<tot_page>{ gs_final-tot_page }</tot_page>| &&
                    |<contact_person>{ gs_final-contact_person }</contact_person>| &&
                    |<contact_phone>{ gs_final-contact_phone }</contact_phone>| &&
                    |<contact_email>{ gs_final-contact_email }</contact_email>| &&
                    |<pay_terms>{ gs_final-pay_terms }</pay_terms>| &&
                    |<plant_code>{ gs_final-plant_code }</plant_code>| &&
                    |<plant_name>{ gs_final-plant_name }</plant_name>| &&
                    |<plant_adrs1>{ gs_final-plant_adrs1 }</plant_adrs1>| &&
                    |<plant_adrs2>{ gs_final-plant_adrs2 }</plant_adrs2>| &&
                    |<plant_adrs3>{ gs_final-plant_adrs3 }</plant_adrs3>| &&
                    |<plant_adrs4>{ gs_final-plant_adrs4 }</plant_adrs4>| &&
                    |<plant_phone>{ gs_final-plant_phone }</plant_phone>| &&
                    |<plant_email>{ gs_final-plant_email }</plant_email>| &&
                    |<plant_gstin>{ gs_final-plant_gstin }</plant_gstin>| &&
                    |<plant_pan>{ gs_final-plant_pan }</plant_pan>| &&
                    |<plant_vat>{ gs_final-plant_vat }</plant_vat>| &&
                    |<suppl_code>{ gs_final-suppl_code }</suppl_code>| &&
                    |<suppl_name>{ gs_final-suppl_name }</suppl_name>| &&
                    |<suppl_adrs1>{ gs_final-suppl_adrs1 }</suppl_adrs1>| &&
                    |<suppl_adrs2>{ gs_final-suppl_adrs2 }</suppl_adrs2>| &&
                    |<suppl_adrs3>{ gs_final-suppl_adrs3 }</suppl_adrs3>| &&
                    |<suppl_adrs4>{ gs_final-suppl_adrs4 }</suppl_adrs4>| &&
                    |<suppl_phone>{ gs_final-suppl_phone }</suppl_phone>| &&
                    |<suppl_email>{ gs_final-suppl_email }</suppl_email>| &&
                    |<suppl_gstin>{ gs_final-suppl_gstin }</suppl_gstin>| &&
                    |<suppl_pan>{ gs_final-suppl_pan }</suppl_pan>| &&
                    |<sum_qty>{ gs_final-sum_qty }</sum_qty>| &&
                    |<sum_netpr>{ gs_final-sum_netpr }</sum_netpr>| &&
                    |<ItemData>|  ##NO_TEXT.


    DATA : lv_item TYPE string,
           srn     TYPE c LENGTH 3.

    CLEAR : lv_item , srn .

    SORT gs_final-sa_item BY delivery_date ASCENDING.
    LOOP AT gs_final-sa_item INTO DATA(ls_item).
      srn = srn + 1 .

      SHIFT ls_item-item_code LEFT DELETING LEADING '0'.
      lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                |<sl_num>{ srn }</sl_num>| &&
                |<item_code>{ ls_item-item_code }</item_code>| &&
                |<item_name>{ ls_item-item_name }</item_name>| &&
                |<item_qty>{ ls_item-item_qty }</item_qty>| &&
                |<delivery_date>{ ls_item-delivery_date }</delivery_date>| &&
                |<net_value>{ ls_item-net_value }</net_value>| &&
                |</ItemDataNode>|  ##NO_TEXT .

      CLEAR: ls_item.
    ENDLOOP.

    lv_xml = |{ lv_xml }{ lv_item }| &&
                       |</ItemData>| &&
                       |</ScheduleAgrementNode>| &&
                       |</Form>| ##NO_TEXT .

    DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
    iv_xml_base64 = ls_data_xml_64.

  ENDMETHOD.
ENDCLASS.
