    CLASS zcl_sd_custom_print DEFINITION
      PUBLIC
      FINAL
      CREATE PUBLIC .

      PUBLIC SECTION.
        DATA:
          gt_final  TYPE TABLE OF zi_sale_reg,
          gt_final1 TYPE TABLE OF zsd_pack_adv,
          lv_char10 TYPE c LENGTH 10.

        METHODS:
          get_billing_data
            IMPORTING
                      iv_vbeln        LIKE lv_char10
                      iv_action       LIKE lv_char10
            RETURNING VALUE(et_final) LIKE gt_final,

          prep_xml_tax_inv
            IMPORTING
                      it_final             LIKE gt_final
                      iv_action            LIKE lv_char10
                      im_prntval           LIKE lv_char10
            RETURNING VALUE(iv_xml_base64) TYPE string,

          get_packing_data
            IMPORTING
                      im_pack         LIKE lv_char10
                      iv_action       LIKE lv_char10
            RETURNING VALUE(et_final) LIKE gt_final,


          prep_xml_pack_inv
            IMPORTING
                      it_final             LIKE gt_final
                      iv_action            LIKE lv_char10
                      im_prntval           LIKE lv_char10
                      im_pack              LIKE lv_char10
            RETURNING VALUE(iv_xml_base64) TYPE string,

          """"""""""""""""""""""""
          get_packing_advice_data
            IMPORTING
                      im_pack          LIKE lv_char10
                      iv_action        LIKE lv_char10
            RETURNING VALUE(et_final1) LIKE gt_final1,


          prep_xml_pack_adv
            IMPORTING
                      it_final1            LIKE gt_final1
                      iv_action            LIKE lv_char10
                      im_prntval           LIKE lv_char10
                      im_pack              LIKE lv_char10
            RETURNING VALUE(iv_xml_base64) TYPE string,

          """"""""""""""""""""""""



          prep_xml_shipping_inv
            IMPORTING
                      it_final             LIKE gt_final
                      iv_action            LIKE lv_char10
                      im_prntval           LIKE lv_char10
            RETURNING VALUE(iv_xml_base64) TYPE string.

      PROTECTED SECTION.
      PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SD_CUSTOM_PRINT IMPLEMENTATION.


      METHOD get_billing_data.

        """*****************Start: Fetch & Prepare Data******************************

        DATA : lv_billtype TYPE RANGE OF zi_sale_reg-billingdocumenttype,
               wa_billtype LIKE LINE OF  lv_billtype,
               lv_distchnl TYPE RANGE OF zi_sale_reg-distributionchannel,
               wa_distchnl LIKE LINE  OF lv_distchnl.


        SELECT * FROM zi_sale_reg  WHERE billingdocument = @iv_vbeln
                                    AND billingdocumenttype IN @lv_billtype
                                    AND distributionchannel IN @lv_distchnl
                                        INTO TABLE @DATA(it_final) .

        IF it_final IS NOT INITIAL.

          et_final[] = it_final[].

        ENDIF.

      ENDMETHOD.


      METHOD get_packing_data.

        """*****************Start: Fetch & Prepare Data******************************


        SELECT * FROM zsd_pack_data WHERE pack_num = @im_pack ORDER BY PRIMARY KEY
         INTO @DATA(wa_pack_data1)
         UP TO 1 ROWS .                       "#EC CI_ALL_FIELDS_NEEDED
        ENDSELECT.

        SELECT * FROM zi_sale_reg
        WHERE billingdocument    = @wa_pack_data1-vbeln AND
              "BillingDocumentType = 'F2' AND
              bill_to_party NE '' AND
              billingdocumentiscancelled = ''
        INTO TABLE @DATA(it_final) .          "#EC CI_ALL_FIELDS_NEEDED


        et_final[] = it_final[].

        """*****************End: Fetch & Prepare Data********************************

      ENDMETHOD.


      METHOD get_packing_advice_data.



        SELECT * FROM zsd_pack_adv WHERE packadvnum = @im_pack ORDER BY PRIMARY KEY
         INTO TABLE @DATA(it_final1) .        "#EC CI_ALL_FIELDS_NEEDED


        et_final1[] = it_final1[].



      ENDMETHOD.


      METHOD prep_xml_pack_adv.



        DATA : party_name   TYPE c LENGTH 50,
               comp_name    TYPE c LENGTH 100,
               comp_address TYPE c LENGTH 250,
               plant        TYPE c LENGTH 4.


        IF it_final1[] IS NOT INITIAL.
          READ TABLE it_final1 INTO DATA(wa) INDEX 1.
        ENDIF.

*        wa-saleorderno = |{ wa-saleorderno ALPHA = IN }|.

        wa-saleorderno = '00' && wa-saleorderno.

        SELECT
           so~salesdocument,
           so~salesdocumenttype,
           item~salesdocumentitem,
           item~material,
           item~plant
         FROM i_salesdocument AS so
         INNER JOIN i_salesdocumentitem AS item
         ON item~salesdocument = so~salesdocument
         WHERE so~salesdocument = @wa-saleorderno
         INTO TABLE @DATA(it_so_plant).

        READ  TABLE it_so_plant INTO DATA(pl) INDEX 1.

        IF pl-plant = '1001'.
          comp_name = 'Bhagwati Lacto Vegetarian Exports Pvt. Ltd.'.
          comp_address = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001,FEROZEPUR(Punjab)'.
          plant = pl-plant .
        ELSEIF pl-plant = '2001'.
          comp_name = 'BHAGWATI LACTO FOODS PVT LTD'.
          comp_address = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001,FEROZEPUR(Punjab)'.
          plant = pl-plant .
        ELSEIF pl-plant = '3001'.
          comp_name = 'HEALTHY HARVESTED FOODS PVT'.
          comp_address = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001,FEROZEPUR(Punjab)'.
          plant = pl-plant .
        ELSEIF pl-plant = '4001'.
          comp_name = 'BHAGWATI AGROCHEM PVT LTD'.
          comp_address = 'Regd. Office : 18-1/2 Anaj Mandi Firozepur Cantt-152001,FEROZEPUR(Punjab)'.
          plant = pl-plant .
        ENDIF.


        DATA : odte_text TYPE c LENGTH 20 , """"original duplicate triplicate ....
               tot_qty   TYPE p LENGTH 16 DECIMALS 2,
               tot_amt   TYPE p LENGTH 16 DECIMALS 2,
               tot_dis   TYPE p LENGTH 16 DECIMALS 2.

        IF im_prntval = 'Original' ##NO_TEXT.
          "odte_text = odte_text = |White-Original            Pink-Duplicate          Yellow-Triplicate|  ##NO_TEXT.
          odte_text = 'Original' ##NO_TEXT.
        ELSEIF im_prntval = 'Duplicate' ##NO_TEXT.
          odte_text = 'Duplicate' ##NO_TEXT.
        ELSEIF im_prntval = 'Triplicate' ##NO_TEXT.
          odte_text = 'Triplicate'  ##NO_TEXT.
        ELSEIF im_prntval = 'Extra' ##NO_TEXT.
          odte_text = 'Extra Copy' ##NO_TEXT.
        ENDIF.


        SELECT SINGLE a~customername
        FROM i_customer AS a
        LEFT JOIN i_salesdocument AS b ON a~customer = b~soldtoparty
        WHERE salesdocument = @wa-saleorderno
        INTO @party_name.


        ""*****Start: Item XML*****************************************************
        DATA : lv_item      TYPE string,
               lv_pallet_no TYPE string,
               srn          TYPE c LENGTH 3,
               lv_anp_part  TYPE string.



        DATA(lv_xml) = |<Form>| &&
                       |<BillingDocumentNode>| &&
                       |<Advice_no>{ wa-packadvnum }</Advice_no>| &&
                       |<plant>{ plant }</plant>| &&
                       |<comp_address>{ comp_address }</comp_address>| &&
                       |<comp_name>{ comp_name }</comp_name >| &&
                       |<Date>{ wa-entrydate }</Date>| &&
                       |<party_name>{ party_name }</party_name>| &&
                       |<vechicle_no>{ wa-vehicleno }</vechicle_no>| &&
                       |<Mobile_no>{ wa-mobileno }</Mobile_no>| &&
                       |<Destinations>{ wa-destination }</Destinations>| &&
                       |<Dim_claim>{ wa-dimclaim }</Dim_claim>| &&
                       |<Broker_name>{ wa-brokername }</Broker_name>| &&
                       |<ItemData>| ##NO_TEXT.

        ""*****End: Header XML*****************************************************



        LOOP AT it_final1 INTO DATA(w_item).

          w_item-saleorderitem = |{ w_item-saleorderitem ALPHA = OUT }|.


          lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                    |<item_code>{ w_item-saleorderitem }</item_code>| &&
                    |<item_saleorder>{  w_item-saleorderno }</item_saleorder>| &&
                    |<item_desc>{ w_item-materialdesc }</item_desc>| &&
                    |<item_brand>{ w_item-custmateril }</item_brand>| &&
                    |<item_qty>{ w_item-quantity }</item_qty>| &&
                    |<item_unit_wt>{ w_item-unitweight }</item_unit_wt>| &&
                    |<item_unit>{ w_item-unit }</item_unit>| &&
                    |<item_book_wt>{ w_item-bookwt }</item_book_wt>| &&
                    |<item_rate>{ w_item-rate }</item_rate>| &&
                    |</ItemDataNode>|.


        ENDLOOP .


        """****Merging Header & Item XML
        lv_xml = |{ lv_xml }{ lv_item }| &&
                           |</ItemData>| &&
                           |</BillingDocumentNode>| &&
                           |</Form>|.

        DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
        iv_xml_base64 = ls_data_xml_64.



      ENDMETHOD.


      METHOD prep_xml_pack_inv.

        DATA: lv_vbeln_n TYPE c LENGTH 10,
              lv_posnr   TYPE zstr_billing_text-billingdocumentitem.

        DATA:
*          tot_amt   TYPE p LENGTH 16 DECIMALS 2,
*          tot_dis   TYPE p LENGTH 16 DECIMALS 2,
          tot_oth         TYPE p LENGTH 16 DECIMALS 2,
          grand_tot       TYPE p LENGTH 16 DECIMALS 2,
          lv_unit_price   TYPE p LENGTH 16 DECIMALS 2,
          lv_unit_pkg     TYPE p LENGTH 16 DECIMALS 3,
          lv_unit_pkg_new TYPE p LENGTH 16 DECIMALS 4,
          gw_per_unit     TYPE p LENGTH 16 DECIMALS 4,
          gross_wgt       TYPE p LENGTH 16 DECIMALS 3,
          lv_tot_qty      TYPE p LENGTH 16 DECIMALS 2,
          lv_no_pkg       TYPE c LENGTH 100,
          lv_po_numbers   TYPE string.

        IF it_final[] IS NOT INITIAL.

          LOOP AT it_final INTO DATA(wa_final).

            IF sy-tabix = 1.
              lv_po_numbers = lv_po_numbers && wa_final-purchaseorderbycustomer.
            ELSE.
              lv_po_numbers = lv_po_numbers && '/' && wa_final-purchaseorderbycustomer.
            ENDIF.

            CLEAR: wa_final.
          ENDLOOP.

          READ TABLE it_final INTO DATA(w_final) INDEX 1 .

          lv_vbeln_n = w_final-billingdocument.
          lv_vbeln_n = |{ lv_vbeln_n ALPHA = IN }| .
          lv_posnr   = w_final-billingdocumentitem.
          lv_posnr   = |{ lv_posnr ALPHA = IN }| .
          """    SHIFT lv_vbeln_n LEFT DELETING LEADING '0'.

          IF im_pack IS NOT INITIAL.

            SELECT * FROM zsd_pack_data WHERE pack_num = @im_pack
                     INTO TABLE @DATA(lt_pack).

            SELECT SINGLE * FROM zsd_pack_data WHERE pack_num =  @im_pack INTO @DATA(wa_pack_data). "#EC WARNOK

          ENDIF.

          REPLACE ALL OCCURRENCES OF '&' IN  w_final-re_name WITH '' .
          REPLACE ALL OCCURRENCES OF '&' IN  w_final-we_name WITH '' .

          DATA : odte_text TYPE c LENGTH 20 , """"original duplicate triplicate ....
                 tot_qty   TYPE p LENGTH 16 DECIMALS 2,
                 tot_amt   TYPE p LENGTH 16 DECIMALS 2,
                 tot_dis   TYPE p LENGTH 16 DECIMALS 2.

          IF im_prntval = 'Original' ##NO_TEXT.
            "odte_text = odte_text = |White-Original            Pink-Duplicate          Yellow-Triplicate|  ##NO_TEXT.
            odte_text = 'Original' ##NO_TEXT.
          ELSEIF im_prntval = 'Duplicate' ##NO_TEXT.
            odte_text = 'Duplicate' ##NO_TEXT.
          ELSEIF im_prntval = 'Triplicate' ##NO_TEXT.
            odte_text = 'Triplicate'  ##NO_TEXT.
          ELSEIF im_prntval = 'Extra' ##NO_TEXT.
            odte_text = 'Extra Copy' ##NO_TEXT.
          ENDIF.

          DATA : heading     TYPE c LENGTH 100,
                 sub_heading TYPE c LENGTH 100,
                 for_sign    TYPE c LENGTH 100.


          IF iv_action = 'export'  ##NO_TEXT.
            heading = 'COMMERCIAL INVOICE' ##NO_TEXT.
          ELSEIF iv_action = 'packls'  ##NO_TEXT.
            heading = 'PACKING LIST' ##NO_TEXT.
          ELSEIF iv_action = 'shipinv'  ##NO_TEXT.
            heading = 'SHIPPING INVOICE CUM PACKING LIST' ##NO_TEXT.
          ENDIF .


          for_sign  = 'BHAGWATI LACTO VEGETARIAN EXPORTS (P) LTD. ' ##NO_TEXT.

          DATA : lv_bill_adr1   TYPE c LENGTH 250,
                 lv_bill_adr2   TYPE c LENGTH 100,
                 lv_bill_adr3   TYPE c LENGTH 100,
                 lv_shp_adr1    TYPE c LENGTH 250,
                 lv_shp_adr2    TYPE c LENGTH 250,
                 lv_shp_adr3    TYPE c LENGTH 250,
                 lv_sp_adr1     TYPE c LENGTH 100,
                 lv_sp_adr2     TYPE c LENGTH 100,
                 lv_sp_adr3     TYPE c LENGTH 100,
                 lv_es_adr1     TYPE c LENGTH 100,
                 lv_es_adr2     TYPE c LENGTH 100,
                 lv_es_adr3     TYPE c LENGTH 100,
                 lv_plant_name  TYPE string,
                 lv_plant_email TYPE string,
                 lv_plant_adrs1 TYPE string,
                 lv_plant_adrs2 TYPE string,
                 lv_plant_adrs3 TYPE string,
                 lv_plant_cin   TYPE string,
                 lv_iec_no      TYPE c LENGTH 40.

          """"""" bill address set """"""""
          IF w_final-re_house_no IS NOT INITIAL .
            lv_bill_adr1 = |{ w_final-re_house_no }| .
          ENDIF .

          IF w_final-re_street IS NOT INITIAL .
            IF lv_bill_adr1 IS NOT INITIAL   .
              lv_bill_adr1 = |{ lv_bill_adr1 } , { w_final-re_street }, { w_final-re_streetprefixname1 }, { w_final-re_streetprefixname2 }, { w_final-re_streetsuffixname1 }| .
            ELSE .
              lv_bill_adr1 = |{ w_final-re_street }, { w_final-re_streetprefixname1 }, { w_final-re_streetprefixname1 }, { w_final-re_streetprefixname2 }, { w_final-re_streetsuffixname1 }| .
            ENDIF .
          ENDIF .

          IF w_final-re_street1 IS NOT INITIAL .
            IF lv_bill_adr1 IS NOT INITIAL   .
              lv_bill_adr1 = |{ lv_bill_adr1 }, { w_final-re_street1 }, { w_final-re_streetprefixname1 }, { w_final-re_streetprefixname2 }, { w_final-re_streetsuffixname1 }| .
            ELSE .
              lv_bill_adr1 = |{ w_final-re_street1 }, { w_final-re_streetprefixname1 }, { w_final-re_streetprefixname2 }, { w_final-re_streetsuffixname1 }| .
            ENDIF .
          ENDIF .

          DATA(len) = strlen( lv_bill_adr1 ) .
          len = len - 40.
          IF strlen( lv_bill_adr1 ) GT 40 .
            lv_bill_adr2 = |{ lv_bill_adr1+40(len) },| .
            lv_bill_adr1 = lv_bill_adr1+0(40) .
          ENDIF .
          """"""" eoc bill address set """"""""


          """"""" ship address set """"""""
          IF w_final-we_house_no IS NOT INITIAL .
            lv_shp_adr1 = |{ w_final-we_house_no }| .
          ENDIF .

          IF w_final-we_street IS NOT INITIAL .
            IF lv_shp_adr1 IS NOT INITIAL   .
              lv_shp_adr1 = |{ lv_shp_adr1 } , { w_final-we_street }, { w_final-we_streetprefixname1 }, { w_final-we_streetprefixname2 }, { w_final-we_streetsuffixname1 }| .
            ELSE .
              lv_shp_adr1 = |{ w_final-we_street }, { w_final-we_streetprefixname1 }, { w_final-we_streetprefixname2 }, { w_final-we_streetsuffixname1 }| .
            ENDIF .
          ENDIF .

          IF w_final-we_street1 IS NOT INITIAL .
            IF lv_shp_adr1 IS NOT INITIAL   .
              lv_shp_adr1 = |{ lv_shp_adr1 } , { w_final-we_street1 }, { w_final-we_streetprefixname1 }, { w_final-we_streetprefixname2 }, { w_final-we_streetsuffixname1 }| .
            ELSE .
              lv_shp_adr1 = |{ w_final-we_street1 }, { w_final-we_streetprefixname1 }, { w_final-we_streetprefixname2 }, { w_final-we_streetsuffixname1 }| .
            ENDIF .
          ENDIF .

          len = strlen( lv_shp_adr1 ) .
          len = len - 40.
          IF strlen( lv_shp_adr1 ) GT 40 .
            lv_shp_adr2 = |{ lv_shp_adr1+40(len) },| .
            lv_shp_adr1 = lv_shp_adr1+0(40) .
          ENDIF .
          CONDENSE lv_shp_adr1.
          CONDENSE lv_shp_adr2.

          len = strlen( lv_es_adr1 ) .
          IF len GT 40 .
            lv_es_adr2 = |{ lv_es_adr1+40(len) },| .
            lv_es_adr1 = lv_es_adr1+0(40) .
          ENDIF .

          ""****Start:Logic to read text of Billing Header************
          DATA:
            lo_text           TYPE REF TO zcl_read_text,
            gt_text           TYPE TABLE OF zstr_billing_text,
            gt_item_text      TYPE TABLE OF zstr_billing_text,
            lo_amt_words      TYPE REF TO zcl_amt_words,
            lv_grand_tot_word TYPE string.

          DATA:
            inst_hsn_code     TYPE string,
            inst_sbno         TYPE string,
            inst_sb_date      TYPE string,
            inst_rcno         TYPE string,
            trans_mode        TYPE string,
            inst_date_accpt   TYPE string,
            inst_delv_date    TYPE string,
            inst_transipment  TYPE string,
            inst_no_orginl    TYPE string,
            inst_frt_amt      TYPE string,
            inst_frt_pay_at   TYPE string,
            inst_destination  TYPE string,
            inst_particular   TYPE string,
            inst_collect      TYPE string,
            lv_tot_box_bags   TYPE p LENGTH 13,
            lv_tot_net_wt     TYPE c LENGTH 20,
            lv_tot_gross_wt   TYPE c LENGTH 20,
            lv_tot_net_wt_3   TYPE p LENGTH 13 DECIMALS 3,
            lv_tot_gross_wt_3 TYPE p LENGTH 13 DECIMALS 3,
            hsn_code_new      TYPE c LENGTH 10.

          CREATE OBJECT lo_text.
          CREATE OBJECT lo_amt_words.

          ""****End:Logic to read text of Billing Header************

          lo_text->read_text_billing_header(
             EXPORTING
               iv_billnum = lv_vbeln_n
             RECEIVING
               xt_text    = gt_text "This will contain all text IDs data of given billing document
           ).


          DATA : lv_no_pck      TYPE c LENGTH 100,
                 lv_gross       TYPE c LENGTH 100,
                 lv_other_ref   TYPE c LENGTH 100,
                 lv_des_goods   TYPE c LENGTH 100,
                 lv_bank1       TYPE c LENGTH 100,
                 lv_bank2       TYPE c LENGTH 100,
                 lv_bank3       TYPE c LENGTH 100,
                 lv_bank4       TYPE c LENGTH 100,
                 lv_bank5       TYPE c LENGTH 100,
                 lv_bank6       TYPE c LENGTH 100,
                 lv_buyer       TYPE c LENGTH 10,
                 lv_notify1     TYPE c LENGTH 10,
                 lv_notify2     TYPE c LENGTH 10,
                 lv_notify3     TYPE c LENGTH 10,
                 lv_notify4     TYPE c LENGTH 10,
                 lv_notify1_1   TYPE string,
                 lv_notify1_2   TYPE string,
                 lv_notify1_3   TYPE string,
                 lv_notify2_1   TYPE string,
                 lv_notify2_2   TYPE string,
                 lv_notify2_3   TYPE string,
                 lv_notify3_1   TYPE string,
                 lv_notify3_2   TYPE string,
                 lv_notify3_3   TYPE string,
                 lv_notify4_1   TYPE string,
                 lv_notify4_2   TYPE string,
                 lv_notify4_3   TYPE string,
                 lv_remark      TYPE string,
                 lv_tot_pcs     TYPE c LENGTH 100,
                 lv_pre_carrige TYPE c LENGTH 100,
                 lv_pre_carrier TYPE c LENGTH 100,
                 lv_cntry_fdest TYPE c LENGTH 100,
                 lv_vessal_flt  TYPE c LENGTH 100,
                 lv_port_loding TYPE c LENGTH 100,
                 lv_final_dest  TYPE c LENGTH 100,
                 lv_port_dischg TYPE c LENGTH 100,
                 container      TYPE c LENGTH 50.

          CLEAR : lv_vessal_flt , lv_no_pck , lv_gross .
          READ TABLE gt_text INTO DATA(w_text) WITH KEY longtextid = 'Z025' .
          IF sy-subrc = 0 .
            lv_buyer = w_text-longtext .
          ENDIF .

          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z021' .
          IF sy-subrc = 0 .
            lv_notify1 = w_text-longtext .
          ENDIF .

          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z022' .
          IF sy-subrc = 0 .
            lv_notify2 = w_text-longtext .
          ENDIF .

          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z023' .
          IF sy-subrc = 0 .
            lv_notify3 = w_text-longtext .
          ENDIF .

          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z024' .
          IF sy-subrc = 0 .
            lv_notify4 = w_text-longtext .
          ENDIF .

          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z011' .
          IF sy-subrc = 0 .
            lv_gross = w_text-longtext .
          ENDIF .

          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'TX05' .
          IF sy-subrc = 0 .
            lv_other_ref = w_text-longtext .
          ENDIF .

          """***For Shipping Instruction****************
          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z005' .
          IF sy-subrc = 0 .
            inst_sbno = w_text-longtext .
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z002' .
          IF sy-subrc = 0 .
            trans_mode = w_text-longtext .
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z012' .
          IF sy-subrc = 0 .
            lv_des_goods = w_text-longtext .
          ENDIF .
          """***For Shipping Instruction****************


          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z008' .
          IF sy-subrc = 0 .
            lv_pre_carrige = w_text-longtext .
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z009' .
          IF sy-subrc = 0 .
            lv_pre_carrier = w_text-longtext .
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z015' .
          IF sy-subrc = 0 .
            lv_cntry_fdest = w_text-longtext .
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z004' .
          IF sy-subrc = 0 .
            lv_vessal_flt = w_text-longtext .
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z006' .
          IF sy-subrc = 0 .
            lv_port_loding = w_text-longtext .
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z014' .
          IF sy-subrc = 0 .
            lv_final_dest = w_text-longtext .
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z013' .
          IF sy-subrc = 0 .
            lv_port_dischg = w_text-longtext .
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z007' .
          IF sy-subrc = 0 .
            lv_tot_pcs = w_text-longtext .
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z017' .
          IF sy-subrc = 0 .
            lv_remark = w_text-longtext .
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z034' .
          IF sy-subrc = 0 .
            container = w_text-longtext .
          ENDIF .

          DATA:
             lv_bill_date TYPE c LENGTH 10.

          lv_bill_date = w_final-billingdocumentdate+6(2) && '.' && w_final-billingdocumentdate+4(2) && '.' && w_final-billingdocumentdate+0(4).
          wa_pack_data-iec = ''.
*          w_final-phoneareacodesubscribernumber = '+91-124-4710100' ##NO_TEXT.
          wa_pack_data-country_org = 'INDIA' ##NO_TEXT.
          wa_pack_data-country_of_fdest = w_final-re_country.

          wa_pack_data-mark_no_of_cont = w_final-materialbycustomer.
          ""*****Start: Item XML*****************************************************
          DATA : lv_item      TYPE string,
                 lv_pallet_no TYPE string,
                 srn          TYPE c LENGTH 3,
                 lv_anp_part  TYPE string.

          IF w_final-item_igstrate EQ 0.
            sub_heading = 'SUPPLY MEANT FOR EXPORT UNDER LUT WITHOUT PAYMENT OF INTEGRATED TAX' ##NO_TEXT.
          ELSE.
            sub_heading = 'SUPPLY MEANT FOR EXPORT WITH PAYMENT OF IGST' ##NO_TEXT.
          ENDIF.

          IF iv_action = 'export'  ##NO_TEXT.

            DATA(xt_pack) = lt_pack[].
            SORT lt_pack BY vbeln posnr.
            DELETE ADJACENT DUPLICATES FROM lt_pack COMPARING vbeln posnr.

            LOOP AT lt_pack ASSIGNING FIELD-SYMBOL(<lfs_pack>).

              IF <lfs_pack> IS ASSIGNED.
                CLEAR: <lfs_pack>-qty_in_pcs, <lfs_pack>-pkg_vol, <lfs_pack>-pkg_length.
                LOOP AT xt_pack INTO DATA(xs_pack) WHERE vbeln = <lfs_pack>-vbeln AND posnr = <lfs_pack>-posnr.
                  "<lfs_pack>-qty_in_pcs = <lfs_pack>-qty_in_pcs + xs_pack-qty_in_pcs.
                  <lfs_pack>-pkg_vol    = <lfs_pack>-pkg_vol + xs_pack-pkg_vol.
                  <lfs_pack>-pkg_length = <lfs_pack>-pkg_length + xs_pack-pkg_length.
                  CLEAR: xs_pack.
                ENDLOOP.

                READ TABLE it_final INTO DATA(xw_final) WITH KEY billingdocument = <lfs_pack>-vbeln billingdocumentitem = <lfs_pack>-posnr.
                IF sy-subrc EQ 0.
                  <lfs_pack>-qty_in_pcs = xw_final-billingquantity.
                ENDIF.

              ENDIF.

            ENDLOOP.

          ENDIF.

          IF iv_action = 'packls'.
            SORT lt_pack BY pallet_no.
          ENDIF.

          CLEAR : lv_item , srn .
          CLEAR: tot_amt, tot_dis, tot_oth, grand_tot.

          IF iv_action = 'shipinv'.

            LOOP AT it_final INTO DATA(w_item_new).

              hsn_code_new =  w_item_new-hsn.

              lv_vbeln_n = w_item_new-billingdocument.
              lv_posnr   = w_item_new-billingdocumentitem.

              lv_vbeln_n = |{ lv_vbeln_n ALPHA = IN }| .
              lv_posnr   = |{ lv_posnr ALPHA = IN }| .

              CLEAR: gt_item_text.
              lo_text->read_text_billing_item(
                EXPORTING
                  im_billnum  = lv_vbeln_n
                  im_billitem = lv_posnr
                RECEIVING
                  xt_text     = gt_item_text
              ).

              IF gt_item_text[] IS NOT INITIAL.

                READ TABLE gt_item_text INTO DATA(gs_item_text) WITH KEY longtextid = 'Z006' .
                IF sy-subrc = 0 .
                  lv_pallet_no = gs_item_text-longtext .
                ENDIF .

                CLEAR: gs_item_text.
                READ TABLE gt_item_text INTO gs_item_text WITH KEY longtextid = 'Z007' .
                IF sy-subrc = 0 .
                  lv_no_pkg = gs_item_text-longtext .
                  CONDENSE lv_no_pkg.
                ENDIF .

              ENDIF.

              CLEAR: w_item_new-billingdocumentitemtext.
              SELECT SINGLE productdescription FROM i_productdescription_2 WHERE product = @w_item_new-product
              INTO @w_item_new-billingdocumentitemtext.

              lv_anp_part = w_item_new-materialbycustomer.
              "w_item-billingdocumentitemtext = ''.
              lv_unit_pkg = w_item_new-billingquantity.
              lv_unit_price = w_item_new-item_unitprice.
              w_item_new-item_totalamount = w_item_new-billingquantity * lv_unit_price.

              lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                        |<hsn_code> {  hsn_code_new }</hsn_code>| &&
                        |<cust_pono> { lv_pallet_no }</cust_pono>| &&
                        |<anp_part>{ lv_anp_part }</anp_part>| &&
                        |<no_of_pkg>{ lv_no_pkg }</no_of_pkg>| &&
                        |<item_desc>{ w_item_new-billingdocumentitemtext }</item_desc>| &&
                        |<qty>{ lv_unit_pkg }</qty>| &&
                        |<rate>{ lv_unit_price }</rate>| &&
                        |<amount>{ w_item_new-item_totalamount }</amount>| &&
                        |</ItemDataNode>|.

              tot_amt         = tot_amt + w_item_new-item_totalamount.
              tot_oth         = tot_oth + w_item_new-item_othercharge.
              tot_dis         = tot_dis + w_item_new-item_discountamount.
              inst_frt_amt    = inst_frt_amt + w_item_new-item_freight.

              lv_tot_net_wt   = lv_tot_net_wt   + w_item_new-netweight.
              lv_tot_gross_wt = lv_tot_gross_wt + w_item_new-grossweight.

              CLEAR: w_item_new, lv_pallet_no, lv_no_pkg, lv_unit_pkg, lv_unit_price , hsn_code_new.
            ENDLOOP.

            grand_tot       = tot_amt + tot_oth - tot_dis + inst_frt_amt.
            wa_pack_data-total_pcs     = lv_tot_pcs.
            wa_pack_data-tot_net_wgt   = lv_tot_net_wt.
            lv_tot_net_wt_3   = lv_tot_net_wt.
            wa_pack_data-tot_gross_wgt = lv_tot_gross_wt.
            lv_tot_gross_wt_3 = lv_tot_gross_wt.

            SELECT SINGLE * FROM zi_countrytext   WHERE country = @w_final-we_country AND language = 'E'
            INTO @DATA(lv_cn_name_we1).       "#EC CI_ALL_FIELDS_NEEDED

          ELSE.

            LOOP AT lt_pack INTO DATA(w_pack) .

              lv_vbeln_n = w_pack-vbeln.
              lv_posnr   = w_pack-posnr.

              lv_vbeln_n = |{ lv_vbeln_n ALPHA = IN }| .
              lv_posnr   = |{ lv_posnr ALPHA = IN }| .

              READ TABLE it_final INTO DATA(w_item) WITH KEY
                                  billingdocument     = lv_vbeln_n billingdocumentitem = lv_posnr.

              CLEAR: gt_item_text.
              lo_text->read_text_billing_item(
                EXPORTING
                  im_billnum  = lv_vbeln_n
                  im_billitem = lv_posnr
                RECEIVING
                  xt_text     = gt_item_text
              ).

              IF gt_item_text[] IS NOT INITIAL.

                CLEAR: gs_item_text.
                READ TABLE gt_item_text INTO gs_item_text WITH KEY longtextid = 'Z006' .
                IF sy-subrc = 0 .
                  lv_pallet_no = gs_item_text-longtext .
                ENDIF .

              ENDIF.

              srn = srn + 1 .
*            lv_pallet_no =  |{ w_item-purchaseorderbycustomer } / { w_item-customerpurchaseorderdate+6(2) }.{ w_item-customerpurchaseorderdate+4(2) }.{ w_item-customerpurchaseorderdate+0(4) } / { gs_item_text-longtext }| .

              IF w_pack-type_pkg IS NOT INITIAL AND w_pack-qty_in_pcs IS NOT INITIAL.
                lv_tot_qty    =  w_pack-qty_in_pcs * w_pack-type_pkg.
              ELSE.
                lv_tot_qty    =  w_pack-type_pkg.
              ENDIF.

              IF iv_action = 'export'.
                wa_pack_data-total_pcs      = wa_pack_data-total_pcs + w_pack-type_pkg. "w_pack-qty_in_pcs.
              ELSE.
                wa_pack_data-total_pcs      = wa_pack_data-total_pcs + lv_tot_qty.
              ENDIF.

*              wa_pack_data-tot_net_wgt    = wa_pack_data-tot_net_wgt + w_pack-pkg_vol.
              lv_tot_net_wt_3    = lv_tot_net_wt_3 + w_pack-pkg_vol.
*              wa_pack_data-tot_gross_wgt  = wa_pack_data-tot_gross_wgt +  w_pack-pkg_length.
              lv_tot_gross_wt_3  = lv_tot_gross_wt_3 +  w_pack-pkg_length.

              lv_anp_part  = w_item-materialbycustomer.

              """""""""""""""""""""""""""""""""""
              SELECT SINGLE * FROM zi_regiontext  WHERE region = @w_final-region AND language = 'E' AND country = @w_final-country
               INTO @DATA(lv_st_nm1).         "#EC CI_ALL_FIELDS_NEEDED

              SELECT SINGLE * FROM zi_regiontext  WHERE region = @w_final-re_region AND language = 'E' AND country = @w_final-re_country
              INTO @DATA(lv_st_name_re1).     "#EC CI_ALL_FIELDS_NEEDED

              SELECT SINGLE * FROM zi_regiontext  WHERE region = @w_final-we_region AND language = 'E' AND country = @w_final-we_country
              INTO @DATA(lv_st_name_we1).     "#EC CI_ALL_FIELDS_NEEDED


              SELECT SINGLE * FROM zi_countrytext   WHERE country = @w_final-country AND language = 'E'
              INTO @DATA(lv_cn_nm1).          "#EC CI_ALL_FIELDS_NEEDED

              SELECT SINGLE * FROM zi_countrytext   WHERE country = @w_final-re_country AND language = 'E'
              INTO @DATA(lv_cn_name_re1).     "#EC CI_ALL_FIELDS_NEEDED

              SELECT SINGLE * FROM zi_countrytext   WHERE country = @w_final-we_country AND language = 'E'
              INTO @lv_cn_name_we1.           "#EC CI_ALL_FIELDS_NEEDED

              SELECT SINGLE * FROM zi_countrytext   WHERE country = @wa_pack_data-country_of_fdest
               AND language = 'E'  INTO @DATA(lv_cn_name_fdes). "#EC CI_ALL_FIELDS_NEEDED

              CLEAR: w_item-billingdocumentitemtext.
              SELECT SINGLE productdescription FROM i_productdescription_2 WHERE product = @w_item-product
              INTO @w_item-billingdocumentitemtext. "#EC CI_ALL_FIELDS_NEEDED

              REPLACE ALL OCCURRENCES OF '&' IN  w_item-materialbycustomer WITH '' .
              REPLACE ALL OCCURRENCES OF '&' IN  lv_anp_part WITH '' .
              REPLACE ALL OCCURRENCES OF '&' IN  w_item-billingdocumentitemtext WITH '' .

              IF w_item-conditionquantity IS NOT INITIAL .
                lv_unit_price = w_item-item_unitprice / w_item-conditionquantity.
              ELSE.
                lv_unit_price = w_item-item_unitprice.
              ENDIF.

              w_item-item_totalamount = w_item-billingquantity * lv_unit_price. "w_item-item_unitprice.
              IF w_pack-type_pkg IS NOT INITIAL.
                lv_unit_pkg  = w_item-billingquantity / w_pack-type_pkg. "w_pack-pkg_vol / w_pack-type_pkg.
                lv_unit_pkg_new  = w_item-billingquantity / w_pack-type_pkg. "w_pack-pkg_vol / w_pack-type_pkg.
              ELSE.
                lv_unit_pkg  = 0.
                lv_unit_pkg_new  = 0.
              ENDIF.

              tot_amt = tot_amt + w_item-item_totalamount.
              tot_dis = tot_dis + w_item-item_discountamount.
              tot_oth = tot_oth + w_item-item_othercharge.
              inst_frt_amt = inst_frt_amt + w_item-item_freight.

              grand_tot = tot_amt + tot_oth - tot_dis + inst_frt_amt.

              lv_no_pkg = w_pack-type_pkg.
              CONDENSE lv_no_pkg.
              CLEAR : gw_per_unit, gross_wgt.
              IF lv_no_pkg IS NOT INITIAL.
                gw_per_unit = ( w_pack-pkg_length / lv_no_pkg ) / 1000.
              ENDIF.
              gross_wgt = w_pack-pkg_length / 1000.

              lv_item = |{ lv_item }| && |<ItemDataNode>| &&

                        |<cust_pono> { lv_pallet_no }</cust_pono>| &&
                        |<pallet_no>{ w_pack-pallet_no }</pallet_no>| &&
                        |<pkgs_from_to>{ w_pack-pkg_no }</pkgs_from_to>| &&
                        |<buyer_code>{ w_pack-kdmat }</buyer_code>| &&
                        |<anp_part>{ lv_anp_part }</anp_part>| &&
                        |<item_code>{ w_pack-matnr }</item_code>| &&
                        |<item_desc>{  w_item-billingdocumentitemtext }</item_desc>| &&
                        |<hsn_code>{  w_item-hsn }</hsn_code>| &&
                        |<qty>{ lv_unit_pkg }</qty>| &&
                        |<qtynew>{ lv_unit_pkg_new }</qtynew>| &&
                        |<qty_pcs>{ w_pack-qty_in_pcs }</qty_pcs>| &&
                        |<net_wgt>{ w_item-billingquantity }</net_wgt>| &&
                        |<gross_wgt>{ gross_wgt }</gross_wgt>| &&
                        |<rate>{ lv_unit_price }</rate>| &&
                        |<amount>{ w_item-item_totalamount }</amount>| &&
                        |<no_of_pkg>{ lv_no_pkg }</no_of_pkg>| &&
                        |<tot_qty>{ lv_tot_qty }</tot_qty>| &&
                        |<grss_wt_per_unit>{ gw_per_unit }</grss_wt_per_unit>| &&
                        |<box_size>{ w_pack-box_size }</box_size>| &&
*                     |<item_code>{ w_item-MaterialDescriptionByCustomer }</item_code>| &&
                        |</ItemDataNode>|.

              CLEAR : w_pack, w_item, lv_pallet_no, lv_anp_part, lv_unit_pkg, gross_wgt, lv_unit_price, lv_no_pkg,
                      lv_tot_qty, gw_per_unit , lv_unit_pkg_new .

            ENDLOOP .

          ENDIF.

          IF iv_action = 'shpinst'.

            heading = 'SLI'.

            inst_delv_date     = ''.
            inst_transipment   = ''.
            inst_frt_amt       = ''.
            inst_frt_pay_at    = ''.
            inst_destination   = ''.

            IF w_final-incotermsclassification = 'FOB' OR w_final-incotermsclassification = 'FCA'.
              inst_collect       = 'FREIGHT COLLECT' ##NO_TEXT.
            ELSE.
              inst_collect       = 'IHC COLLECT' ##NO_TEXT.
            ENDIF.

            DATA(lt_inst) = it_final[].
            SORT lt_inst BY hsn.
            DELETE ADJACENT DUPLICATES FROM lt_inst COMPARING hsn.

            LOOP AT lt_inst INTO DATA(ls_inst).
              inst_hsn_code  = inst_hsn_code && ls_inst-hsn.
            ENDLOOP.

            CLEAR: lv_item.
            lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                      |<cust_pono> { lv_pallet_no }</cust_pono>| &&
                      |</ItemDataNode>|  .

          ENDIF.

          lv_grand_tot_word  = grand_tot.
          lo_amt_words->number_to_words_export(
           EXPORTING
             iv_num   = lv_grand_tot_word
           RECEIVING
             rv_words = DATA(grand_tot_amt_words)
         ).

          IF w_final-transactioncurrency EQ 'USD'.

          ELSEIF w_final-transactioncurrency EQ 'EUR'.
            REPLACE ALL OCCURRENCES OF 'Dollars' IN grand_tot_amt_words WITH 'Euro' ##NO_TEXT.
          ENDIF.
          CONDENSE grand_tot_amt_words.

          DATA : lv_declaration1 TYPE string .
          DATA : lv_declaration3 TYPE string .
          DATA : lv_declaration4 TYPE string .
          DATA : lv_declaration5 TYPE string .
          DATA : lv_declaration6 TYPE string .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z016' .
          IF sy-subrc = 0 .
            lv_declaration1 = w_text-longtext .
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z030' .
          IF sy-subrc = 0 .
            lv_declaration3 = w_text-longtext . "BATCH NO.
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z032' .
          IF sy-subrc = 0 .
            lv_declaration4 = w_text-longtext .  "PKG DATE
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z031' .
          IF sy-subrc = 0 .
            lv_declaration5 = w_text-longtext .   "EXP DATE
          ENDIF .

          CLEAR: w_text.
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z033' .
          IF sy-subrc = 0 .
            lv_declaration6 = w_text-longtext .   "CROP YEAR
          ENDIF .

          ""*****End: Item XML*****************************************************
          CLEAR : odte_text .
          ""*****Start: Header XML*****************************************************


          IF w_final-plant = '1001'.

            lv_plant_name   = 'Bhagwati Lacto Vegetarian Exports Pvt. Ltd.' ##NO_TEXT.
            lv_plant_email = 'rice@blvexports.com'.
            w_final-phoneareacodesubscribernumber = '+917009234323'.

            lv_bank1 = 'UCO BANK'.
            lv_bank2 = 'MIDCORPORATE BRANCH, INDUSTRIAL AREA, NEAR CHEEMA CHOWK, R.K. ROAD, LUDHIANA-141001 (PB) INDIA'.
            lv_bank3 = '20660510000250'.
            lv_bank4 = 'UCBAINBB216'.
            lv_bank5 = ''.
            lv_bank6 = ''.

          ELSEIF w_final-plant = '2001'.

            lv_plant_name   = 'Bhagwati Lacto Foods Pvt Ltd' ##NO_TEXT.
            w_final-plant_email = ''.
            w_final-phoneareacodesubscribernumber = '+91 1632 244765'.

            lv_bank1 = 'UCO BANK'.
            lv_bank2 = 'MIDCORPORATE BRANCH, INDUSTRIAL AREA, NEAR CHEEMA CHOWK, R.K. ROAD, LUDHIANA-141001 (PB) INDIA'.
            lv_bank3 = '20660210001981'.
            lv_bank4 = 'UCBAINBB216'.
            lv_bank5 = ''.
            lv_bank6 = ''.

          ELSEIF w_final-plant = '3001'.

            lv_plant_name   = 'Healthy Harvested Foods Private Limited' ##NO_TEXT.
            w_final-plant_email = ''.

          ELSEIF w_final-plant = '4001'.
            lv_plant_name   = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
            w_final-plant_email = ''.
          ENDIF.

          IF w_final-plant = '1001'.
            lv_plant_adrs1 = '18-1/2 Anaj Mandi,'.
            lv_plant_adrs2 = 'Ferozepur Cantt.-152001,'.
            lv_plant_adrs3 = 'Punjab, India'.
          ELSE.
            SELECT SINGLE * FROM zi_plant_address
                            WHERE plant = @w_final-plant
                            INTO @DATA(ls_plant_adrs).

            lv_plant_adrs1 = ls_plant_adrs-streetname.
            lv_plant_adrs2 = |{ ls_plant_adrs-cityname } - { ls_plant_adrs-postalcode }|.
            lv_plant_adrs3 = |{ ls_plant_adrs-region } - { ls_plant_adrs-addresstimezone }|.
          ENDIF.

          lv_plant_cin   = ''.
          wa_pack_data-ad_code = ''.

          lv_iec_no           = '3008002838'.
          wa_pack_data-ex_pan = 'AADCB4295Q'.

          lv_po_numbers = w_final-purchaseorderbycustomer.

          lv_notify1 = |{ lv_notify1 ALPHA = IN }| .
          lv_notify2 = |{ lv_notify2 ALPHA = IN }| .
          lv_notify3 = |{ lv_notify3 ALPHA = IN }| .
          lv_notify4 = |{ lv_notify4 ALPHA = IN }| .
          lv_buyer   = |{ lv_buyer ALPHA = IN }| .

          SELECT SINGLE *
          FROM zi_customer_address
          WHERE customer = @lv_notify1
          INTO @DATA(ls_notify_adrs).

          lv_notify1_1 = ls_notify_adrs-customername.
          lv_notify1_2 = ls_notify_adrs-streetname && ls_notify_adrs-streetprefixname1 && ls_notify_adrs-streetprefixname2
          && ls_notify_adrs-streetsuffixname1 && '-' && ls_notify_adrs-postalcode.
          lv_notify1_3 = 'Phone No :' && ls_notify_adrs-phonenumber1 && cl_abap_char_utilities=>newline &&
                         'E-mail :' && ls_notify_adrs-emailaddress.

          CLEAR: ls_notify_adrs.
          SELECT SINGLE *
          FROM zi_customer_address
          WHERE customer = @lv_notify2
          INTO @ls_notify_adrs.

          lv_notify2_1 = ls_notify_adrs-customername.
          lv_notify2_2 = ls_notify_adrs-streetname && ls_notify_adrs-streetprefixname1 && ls_notify_adrs-streetprefixname2
          && ls_notify_adrs-streetsuffixname1 && '-' && ls_notify_adrs-postalcode.
          lv_notify2_3 = 'Phone No :' && ls_notify_adrs-phonenumber1 && cl_abap_char_utilities=>newline &&
                         'E-mail :' && ls_notify_adrs-emailaddress.

          CLEAR: ls_notify_adrs.
          SELECT SINGLE *
          FROM zi_customer_address
          WHERE customer = @lv_notify3
          INTO @ls_notify_adrs.

          lv_notify3_1 = ls_notify_adrs-customername.
          lv_notify3_2 = ls_notify_adrs-streetname && ls_notify_adrs-streetprefixname1 && ls_notify_adrs-streetprefixname2
          && ls_notify_adrs-streetsuffixname1 && '-' && ls_notify_adrs-postalcode.
          lv_notify3_3 = 'Phone No :' && ls_notify_adrs-phonenumber1 && cl_abap_char_utilities=>newline &&
                         'E-mail :' && ls_notify_adrs-emailaddress.

          CLEAR: ls_notify_adrs.
          SELECT SINGLE *
          FROM zi_customer_address
          WHERE customer = @lv_notify4
          INTO @ls_notify_adrs.

          lv_notify4_1 = ls_notify_adrs-customername.
          lv_notify4_2 = ls_notify_adrs-streetname && ls_notify_adrs-streetprefixname1 && ls_notify_adrs-streetprefixname2
          && ls_notify_adrs-streetsuffixname1 && '-' && ls_notify_adrs-postalcode.
          lv_notify4_3 = 'Phone No :' && ls_notify_adrs-phonenumber1 && cl_abap_char_utilities=>newline &&
                         'E-mail :' && ls_notify_adrs-emailaddress.


          CLEAR: ls_notify_adrs.
          SELECT SINGLE
          customername,
          streetname,
          cityname,
          postalcode
          FROM zi_customer_address
          WHERE customer = @lv_buyer
          INTO @ls_notify_adrs.

          w_final-re_name = ls_notify_adrs-customername.
          lv_bill_adr1    = ls_notify_adrs-streetname && ls_notify_adrs-cityname && '-' && ls_notify_adrs-postalcode.
          lv_bill_adr2      = ''.
          w_final-re_phone4 = ''.

*          wa_pack_data-tot_net_wgt    = wa_pack_data-tot_net_wgt / 1000.
          lv_tot_net_wt_3    = lv_tot_net_wt_3 / 1000.
*          wa_pack_data-tot_gross_wgt  = wa_pack_data-tot_gross_wgt / 1000.
          lv_tot_gross_wt_3  = lv_tot_gross_wt_3 / 1000.

          REPLACE ALL OCCURRENCES OF '&' IN w_final-incotermslocation1 WITH ''.

          lv_tot_box_bags = wa_pack_data-total_pcs.
*          SELECT SINGLE * FROM
*          i_customerpaymenttermstext
*          WHERE customerpaymentterms = @w_final-customerpaymentterms AND language = 'E'
*          INTO @DATA(ls_payterm_text).

*          w_final-CustomerPaymentTermsName = ls_payterm_text-CustomerPaymentTermsName.

          SELECT SINGLE
          salesdocument,
          purchaseorderbycustomer,
          customerpurchaseorderdate,
          creationdate,
          referencesddocument
          FROM i_salesdocument
          WHERE salesdocument = @w_final-salesdocument
          INTO @DATA(ls_sale_doc).

          DATA(lv_po_date) = ls_sale_doc-creationdate+6(2) && '.'
          && ls_sale_doc-creationdate+4(2) && '.'
          && ls_sale_doc-creationdate+0(4).

**********************************Logic Add for Buyer order date in Shipping Invoice****************Changes done by Madhur******

          SELECT SINGLE salesdocument , salesdocumentdate
          , salesdocumenttype , referencesddocument
           FROM i_salesdocument
           WHERE salesdocument = @w_final-salesdocument
*        and salesdocumenttype = 'AG'
           INTO @DATA(ls_sales_doc1).

          SELECT SINGLE salesdocument , salesdocumentdate
        , salesdocumenttype , referencesddocument
         FROM i_salesdocument
         WHERE salesdocument = @ls_sales_doc1-referencesddocument
         AND salesdocumenttype = 'AG'
         INTO @DATA(ls_sales_doc2).


          DATA(lv_bill_date1) = ls_sales_doc2-salesdocumentdate+6(2) && '.'
           && ls_sales_doc2-salesdocumentdate+4(2) && '.'
           && ls_sales_doc2-salesdocumentdate+0(4).

***********************************************************************************************************************


          IF iv_action = 'shipinv'.
            wa_pack_data-pre_carig_by   = lv_pre_carrige.
            wa_pack_data-pre_carrier    = lv_pre_carrier.
            lv_cn_name_fdes-countryname = lv_cntry_fdest.
            wa_pack_data-port_of_load   = lv_port_loding.
            wa_pack_data-final_dest     = lv_final_dest.
            wa_pack_data-port_of_discg  = lv_port_dischg.
          ELSE.
            lv_vessal_flt = wa_pack_data-vessel.
          ENDIF.

          CONDENSE w_final-we_pin.
          CONDENSE lv_cn_name_we1-countryname.
          TRANSLATE lv_cn_name_fdes-countryname TO UPPER CASE.

          CLEAR w_final-customerpaymenttermsname.
          SELECT SINGLE paymenttermsdescription FROM i_paymenttermstext
          WHERE paymentterms = @w_final-customerpaymentterms
          INTO @DATA(lv_paymenttermsname).
"""add by sb 23122025""
        if w_final-DistributionChannel = '20' and w_final-Plant = '1001'.
        w_final-plant_gstin = '24AADCB4295Q1Z6'.
        ENDIF.
"""add by sb 23122025""
          DATA(lv_xml) = |<Form>| &&
                         |<BillingDocumentNode>| &&
                         |<heading>{ heading }</heading>| &&
                         |<sub_heading>{ sub_heading }</sub_heading>| &&
                         |<for_sign>{ for_sign }</for_sign>| &&
                         |<odte_text>{ odte_text }</odte_text>| &&
                         |<po_num>{ ls_sale_doc-referencesddocument }</po_num>| &&
*                         |<po_date>{ lv_po_date }</po_date>| &&
                         |<po_date>{ lv_bill_date1 }</po_date>| &&
                          |<plant_code>{ w_final-plant }</plant_code>| &&
                          |<plant_name>{ lv_plant_name }</plant_name>| &&
                          |<plant_address_l1>{ lv_plant_adrs1 }</plant_address_l1>| &&
                          |<plant_address_l2>{ lv_plant_adrs2 }</plant_address_l2>| &&
                          |<plant_address_l3>{ lv_plant_adrs3 }</plant_address_l3>| &&
                          |<plant_cin>{ lv_plant_cin }</plant_cin>| &&
                          |<plant_gstin>{ w_final-plant_gstin }</plant_gstin>| &&
                          |<plant_pan>{ w_final-plant_gstin+2(10) }</plant_pan>| &&
                          |<plant_state_code>{ w_final-region }</plant_state_code>| &&
                          |<plant_state_name>{ w_final-plantname }</plant_state_name>| &&
                          |<plant_phone>{ w_final-phoneareacodesubscribernumber }</plant_phone>| &&
                          |<plant_email>{ lv_plant_email }</plant_email>| &&
                          |<consignee_code>{ w_final-ship_to_party }</consignee_code>| &&
                          |<consignee_name>{ w_final-we_name }</consignee_name>| &&
                          |<consignee_address_l1>{ lv_shp_adr1 }</consignee_address_l1>| &&
                          |<consignee_address_l2>{ lv_shp_adr2 }</consignee_address_l2>| &&
                          |<consignee_address_l3>{ w_final-we_pin } ({ lv_cn_name_we1-countryname })</consignee_address_l3>| &&
                          |<consignee_cin>{ w_final-plantname }</consignee_cin>| &&
                          |<consignee_gstin>{ w_final-we_tax }</consignee_gstin>| &&
                          |<consignee_pan>{ w_final-we_pan }</consignee_pan>| &&
                          |<consignee_state_code>{ w_final-we_region } ({ lv_st_name_we1-regionname })</consignee_state_code>| &&
                          |<consignee_state_name>{ w_final-we_city }</consignee_state_name>| &&
                          |<consignee_place_suply>{ w_final-we_region }</consignee_place_suply>| &&
                          |<consignee_phone>{ w_final-we_phone4 }</consignee_phone>| &&
                          |<consignee_email>{ w_final-we_email }</consignee_email>| &&


                            |<notify1_1>{ lv_notify1_1 }</notify1_1>| &&
                            |<notify1_2>{ lv_notify1_2 }</notify1_2>| &&
                            |<notify1_3>{ lv_notify1_3 }</notify1_3>| &&

                            |<notify2_1>{ lv_notify2_1 }</notify2_1>| &&
                            |<notify2_2>{ lv_notify2_2 }</notify2_2>| &&
                            |<notify2_3>{ lv_notify2_3 }</notify2_3>| &&

                            |<notify3_1>{ lv_notify3_1 }</notify3_1>| &&
                            |<notify3_2>{ lv_notify3_2 }</notify3_2>| &&
                            |<notify3_3>{ lv_notify3_3 }</notify3_3>| &&

                            |<notify4_1>{ lv_notify4_1 }</notify4_1>| &&
                            |<notify4_2>{ lv_notify4_2 }</notify4_2>| &&
                            |<notify4_3>{ lv_notify4_3 }</notify4_3>| &&

*                          |<shipto_code>{ w_final-sp_code }</shipto_code>| &&
*                          |<shipto_name>{ w_final-sp_name }</shipto_name>| &&
*                          |<shipto_addrs1>{ lv_sp_adr1 }</shipto_addrs1>| &&
*                          |<shipto_addrs2>{ lv_sp_adr2 }</shipto_addrs2>| &&
*                          |<shipto_addrs3>{ w_final-sp_pin }</shipto_addrs3>| &&

*                          |<secnd_ntfy_code>{ w_final-es_code }</secnd_ntfy_code>| &&
*                          |<secnd_ntfy_name>{ w_final-es_name }</secnd_ntfy_name>| &&
*                          |<secnd_ntfy_addrs1>{ lv_es_adr1 }</secnd_ntfy_addrs1>| &&
*                          |<secnd_ntfy_addrs2>{ lv_es_adr2 }</secnd_ntfy_addrs2>| &&
*                          |<secnd_ntfy_addrs3>{ w_final-es_pin }</secnd_ntfy_addrs3>| &&


                          |<buyer_code>{ w_final-bill_to_party }</buyer_code>| &&
                          |<buyer_name>{ w_final-re_name }</buyer_name>| &&
                          |<buyer_address_l1>{ lv_bill_adr1 }</buyer_address_l1>| &&
                          |<buyer_address_l2>{ lv_bill_adr2 }</buyer_address_l2>| &&
                          |<buyer_address_l3>{ '' }</buyer_address_l3>| &&
                          |<buyer_cin>{ '' }</buyer_cin>| &&
                          |<buyer_gstin>{ w_final-re_tax }</buyer_gstin>| &&
                          |<buyer_pan>{ w_final-re_pan }</buyer_pan>| &&
                          |<buyer_state_code>{ w_final-re_region } ({ lv_st_name_re1-regionname })</buyer_state_code>| &&
                          |<buyer_state_name>{ w_final-re_city }</buyer_state_name>| &&
                          |<buyer_place_suply>{ w_final-re_region }</buyer_place_suply>| &&
                          |<buyer_phone>{ w_final-re_phone4 }</buyer_phone>| &&
                          |<buyer_email>{ w_final-re_email }</buyer_email>| &&

                          |<inv_no>{ w_final-documentreferenceid }</inv_no>| &&
                          |<inv_date>{ lv_bill_date }</inv_date>| &&

                          |<iec_num>{ lv_iec_no }</iec_num>| &&
                          |<pan_num>{ wa_pack_data-ex_pan }</pan_num>| &&
                          |<ad_code>{ wa_pack_data-ad_code }</ad_code>| &&
                          |<pre_carig_by>{ wa_pack_data-pre_carig_by }</pre_carig_by>| &&
                          |<vessel>{ lv_vessal_flt }</vessel>| &&
                          |<port_of_discg>{ wa_pack_data-port_of_discg }</port_of_discg>| &&
                          |<mark_no_of_cont>{ wa_pack_data-mark_no_of_cont }</mark_no_of_cont>| &&
                          |<pre_carrier>{ wa_pack_data-pre_carrier }</pre_carrier>| &&
                          |<port_of_load>{ wa_pack_data-port_of_load }</port_of_load>| &&
                          |<final_dest>{ wa_pack_data-final_dest }</final_dest>| &&
                          |<country_org>{ wa_pack_data-country_org }</country_org>| &&
                          |<country_of_fdest>{ lv_cn_name_fdes-countryname }</country_of_fdest>| &&

                          |<pay_term>{ w_final-incotermsclassification } ({ w_final-incotermslocation1 })</pay_term>| &&
                          |<payment>{ lv_paymenttermsname }</payment>| &&

                          |<des_of_goods>{ lv_des_goods }</des_of_goods>| &&
                          |<no_kind_pkg>{ wa_pack_data-no_kind_pkg }</no_kind_pkg>| &&

                          |<total_pcs>{ lv_tot_box_bags }</total_pcs>| &&
                          |<tot_net_wgt>{ lv_tot_net_wt_3 }</tot_net_wgt>| &&
                          |<tot_gross_wgt>{ lv_tot_gross_wt_3 }</tot_gross_wgt>| &&
                          |<total_vol>{ 'C' }</total_vol>| &&

                          |<other_ref> { lv_other_ref }</other_ref>| &&

                          |<lut_urn> { 'AD060323015122V' }</lut_urn>| &&
                          |<lut_date> { '09/03/2023' }</lut_date>| &&
                          |<end_use_code> { lv_po_numbers }</end_use_code>| &&
                          |<plant_website> { 'www.blv.com' }</plant_website>| &&

                          |<total_amt>{ tot_amt }</total_amt>| &&
                          |<other_charges>{ tot_oth }</other_charges>| &&
                          |<discount>{ tot_dis }</discount>| &&
                          |<grand_total>{ grand_tot }</grand_total>| &&
                          |<insurance_amt>{ '1' }</insurance_amt>| &&

                          |<bank1>{ lv_bank1 }</bank1>| &&
                          |<bank2>{ lv_bank2 }</bank2>| &&
                          |<bank3>{ lv_bank3 }</bank3>| &&
                          |<bank4>{ lv_bank4 }</bank4>| &&
                          |<bank5>{ lv_bank5 }</bank5>| &&
                          |<bank6>{ lv_bank6 }</bank6>| &&

                          |<lv_dec1>{ lv_declaration1 }</lv_dec1>| &&
                          |<lv_dec2>{ lv_remark }</lv_dec2>| &&
                          |<lv_dec3>{ lv_declaration3 }</lv_dec3>| &&
                          |<lv_dec4>{ lv_declaration4 }</lv_dec4>| &&
                          |<lv_dec5>{ lv_declaration5 }</lv_dec5>| &&
                          |<lv_dec6>{ lv_declaration6 }</lv_dec6>| &&
                          |<container>{ container }</container>| &&

                          |<rate_curr>{ w_final-transactioncurrency }</rate_curr>| &&
                          |<amt_words>{ grand_tot_amt_words }</amt_words>| &&

                        |<inst_hsn_code>{ inst_hsn_code }</inst_hsn_code>| &&
                        |<inst_sbno>{ inst_sbno }</inst_sbno>| &&
                        |<inst_collect>{ inst_collect  }</inst_collect>| &&
                        |<inst_sb_date>{ inst_sb_date }</inst_sb_date>| &&
                        |<inst_rcno>{ inst_rcno }</inst_rcno>| &&
                        |<trans_mode>{ trans_mode }</trans_mode>| &&
                        |<inst_date_accpt>{ inst_date_accpt }</inst_date_accpt>| &&
                        |<inst_delv_date>{ inst_delv_date }</inst_delv_date>| &&
*                       |<inst_transipment>{ inst_transipment }</inst_transipment| &&
                        |<inst_no_orginl>{ inst_no_orginl }</inst_no_orginl>| &&
                        |<inst_frt_amt>{ inst_frt_amt }</inst_frt_amt>| &&
                        |<inst_frt_pay_at>{ inst_frt_pay_at }</inst_frt_pay_at>| &&
                        |<inst_destination>{ inst_destination }</inst_destination>| &&
                        |<inst_particular>{ inst_particular }</inst_particular>| &&

                         |<ItemData>| ##NO_TEXT.

          ""*****End: Header XML*****************************************************

          """****Merging Header & Item XML
          lv_xml = |{ lv_xml }{ lv_item }| &&
                             |</ItemData>| &&
                             |</BillingDocumentNode>| &&
                             |</Form>|.

          DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
          iv_xml_base64 = ls_data_xml_64.

        ENDIF.

      ENDMETHOD.


      METHOD prep_xml_shipping_inv.

      ENDMETHOD.


      METHOD prep_xml_tax_inv.

        DATA: lv_vbeln_n   TYPE c LENGTH 10,
              lv_qr_code   TYPE string,
              lv_irn_num   TYPE c LENGTH 64, "w_irn-irnno
              lv_ack_no    TYPE c LENGTH 20, "w_irn-ackno
              lv_ack_date  TYPE c LENGTH 10, "w_irn-ackdat
              lv_ref_sddoc TYPE c LENGTH 20, "w_item-ReferenceSDDocument
              value        TYPE string.     " Roundoff

        ""****Start:Logic to convert amount in Words************
        DATA:
          lo_amt_words TYPE REF TO zcl_amt_words.
        CREATE OBJECT lo_amt_words.
        ""****End:Logic to convert amount in Words************

        ""****Start:Logic to read text of Billing Header************
        DATA:
          lo_text TYPE REF TO zcl_read_text,
          gt_text TYPE TABLE OF zstr_billing_text.

        CREATE OBJECT lo_text.


        ""****End:Logic to read text of Billing Header************

        lv_qr_code = |This is a demo QR code. So please keep patience... And do not scan it with bar code scanner till i say to scan #sumit| ##NO_TEXT.

        READ TABLE it_final INTO DATA(w_final) INDEX 1 .
        lv_vbeln_n = w_final-billingdocument.


        lo_text->read_text_billing_header(
           EXPORTING
             iv_billnum = lv_vbeln_n
           RECEIVING
             xt_text    = gt_text "This will contain all text IDs data of given billing document
         ).

        SHIFT lv_vbeln_n LEFT DELETING LEADING '0'.

        DATA : odte_text      TYPE string , """" original duplicate triplicate ....
               lv_broker_name TYPE c LENGTH 40,
               lv_plant_name  TYPE c LENGTH 255,
               lv_plant_email TYPE c LENGTH 255,
               tot_qty        TYPE p LENGTH 16 DECIMALS 2,
               tot_amt        TYPE p LENGTH 16 DECIMALS 2,
               tot_dis        TYPE p LENGTH 16 DECIMALS 2.

        REPLACE ALL OCCURRENCES OF '&' IN  w_final-re_name WITH '' .
        REPLACE ALL OCCURRENCES OF '&' IN  w_final-we_name WITH '' .

        """"""""""""""""""" for total ...
        DATA : lv_qty             TYPE p LENGTH 16 DECIMALS 2,
               lv_netwt           TYPE p LENGTH 16 DECIMALS 2,
               lv_grosswt         TYPE p LENGTH 16 DECIMALS 2,
               lv_dis             TYPE p LENGTH 16 DECIMALS 2,
               lv_tot_amt         TYPE p LENGTH 16 DECIMALS 2,
               lv_tax_amt         TYPE p LENGTH 16 DECIMALS 2,
               lv_tax_amt1        TYPE p LENGTH 16 DECIMALS 2,
               lv_tot_sgst        TYPE p LENGTH 16 DECIMALS 2,
               lv_tot_cgst        TYPE p LENGTH 16 DECIMALS 2,
               lv_tot_igst        TYPE p LENGTH 16 DECIMALS 2,
               lv_tot_igst1       TYPE p LENGTH 16 DECIMALS 2,
               lv_tot_cgst1       TYPE p LENGTH 16 DECIMALS 2,
               lv_tot_amort       TYPE p LENGTH 16 DECIMALS 2,
               lv_tot_sgst1       TYPE p LENGTH 16 DECIMALS 2,
               lv_tot_pkgchrg     TYPE p LENGTH 16 DECIMALS 2,
               lv_no_of_package   TYPE p LENGTH 16 DECIMALS 2,
               lv_no_of_bag       TYPE p LENGTH 16 DECIMALS 2,
               lv_tcs             TYPE p LENGTH 16 DECIMALS 2,
               lv_other_chrg      TYPE p LENGTH 16 DECIMALS 2,
               sum_other_chrg     TYPE p LENGTH 16 DECIMALS 2,
               lv_round_off       TYPE p LENGTH 16 DECIMALS 2,
               lv_tot_gst         TYPE p LENGTH 16 DECIMALS 2,
               lv_grand_tot       TYPE p LENGTH 16 DECIMALS 2,
               lv_item_urate      TYPE p LENGTH 16 DECIMALS 2,
               lv_item_amtinr     TYPE p LENGTH 16 DECIMALS 2,
               lv_item_amtexp     TYPE p LENGTH 16 DECIMALS 2,
               lv_mrp_of_goods    TYPE p LENGTH 16 DECIMALS 2,
               lv_amt_expcurr     TYPE p LENGTH 16 DECIMALS 2,
               lv_net             TYPE p LENGTH 16 DECIMALS 2,
               lv_gross           TYPE p LENGTH 16 DECIMALS 2,
               lv_exchng_rate     TYPE p LENGTH 16 DECIMALS 2,
               lv_gst_rate        TYPE p LENGTH 16 DECIMALS 2,
               lv_total_insur_amt TYPE p LENGTH 16 DECIMALS 2,
               total_item_wt      TYPE p LENGTH 16 DECIMALS 2,
               lv_certify_1       TYPE string,
               lv_certify_2       TYPE string,
               lv_certify_3       TYPE string,
               lv_place_supply    TYPE string,
               lv_fssai_lic_no    TYPE string,
               lv_insur_policy_no TYPE string,
               lv_billto_fssai_no TYPE string,
               lv_comp_name       TYPE string,
               lv_gr_number       TYPE string,
               lv_bank_name       TYPE string,
               lv_bank_accno      TYPE string,
               lv_bank_branch     TYPE string,
               lv_bank_ifsc       TYPE string,
               footer1            TYPE c LENGTH 200,
               footer2            TYPE c LENGTH 200,
               footer3            TYPE c LENGTH 200,
               footer4            TYPE c LENGTH 200,
               footer5            TYPE c LENGTH 200.

        LOOP               AT it_final INTO DATA(w_sum).
          lv_qty = lv_qty + w_sum-billingquantity .
          lv_dis = lv_dis + w_sum-item_discountamount .
          lv_tot_amt = lv_tot_amt + w_sum-item_totalamount_inr .
          lv_tax_amt = lv_tax_amt + w_sum-item_assessableamount .
          lv_tot_igst = lv_tot_igst + w_sum-item_igstamount .
          lv_tot_igst1 = lv_tot_igst1 + w_sum-item_igstamount .
          lv_tot_sgst = lv_tot_sgst + w_sum-item_sgstamount .
          lv_tot_cgst = lv_tot_cgst + w_sum-item_cgstamount .
          lv_tcs = lv_tcs + w_sum-item_othercharge .
          lv_other_chrg = lv_other_chrg + w_sum-item_freight .
          lv_round_off = lv_round_off + w_sum-item_roundoff .
          lv_gross = lv_gross + w_sum-grossweight .
          lv_net   = lv_net + w_sum-netweight .
        ENDLOOP. .

        DATA : sale_order TYPE c LENGTH 10.
        sale_order = w_sum-salesdocument.


        lv_tot_amt = lv_tot_amt - lv_other_chrg .
        lv_tax_amt = lv_tax_amt - lv_other_chrg .

        lv_grand_tot =  lv_tax_amt + lv_tot_sgst + lv_tot_cgst + lv_tot_igst
                        + lv_other_chrg + lv_tcs + lv_round_off .
        lv_tot_gst = lv_tot_sgst + lv_tot_cgst + lv_tot_igst .

        """ IF w_final-DistributionChannel = '30' .
        CLEAR : lv_qty , lv_dis , lv_tot_amt , lv_tax_amt ,lv_tot_igst , lv_tot_igst1 ,lv_tot_gst ,
         lv_tcs , lv_other_chrg , lv_round_off ,  lv_tot_amt ,lv_tax_amt ,lv_grand_tot , lv_tot_sgst , lv_tot_cgst.
        "" ENDIF .

        """""""""""""""""""""

*        IF w_final-re_tax  = 'URP' .
*          CLEAR : w_final-re_tax .
*        ENDIF .
*        IF w_final-we_tax  = 'URP' .
*          CLEAR : w_final-we_tax .
*        ENDIF .

        DATA : lv_remarks TYPE c LENGTH 100.
        DATA : lv_flag_pol TYPE c LENGTH 1.
        DATA : lv_gsdb TYPE c LENGTH 100 .
        DATA : lv_cus_pl TYPE c LENGTH 500 .
        DATA :  vcode TYPE c LENGTH 100 .
        DATA : lv_vehicle TYPE c LENGTH 15 .
        DATA : lv_eway TYPE c LENGTH 15.
        DATA : lv_eway_dt TYPE c LENGTH 10 .
        DATA : lv_transmode       TYPE c LENGTH 10 ,  """lv_exp_no
               lv_tranporter_name TYPE c LENGTH 60.
        DATA : lv_exp_no TYPE c LENGTH 100.
        DATA : lv_no_pck TYPE c LENGTH 100.
        DATA : head_lut TYPE c LENGTH 100.
        CLEAR : lv_remarks , lv_no_pck .

        READ TABLE gt_text INTO DATA(w_text) WITH KEY longtextid = 'Z001' .
        IF sy-subrc = 0 .
          lv_vehicle = w_text-longtext .
        ENDIF .

        READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z002' .
        IF sy-subrc = 0 .
          lv_transmode = w_text-longtext .
        ENDIF .

        READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z003' .
        IF sy-subrc = 0 .
          lv_tranporter_name = w_text-longtext .
        ENDIF .

        READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z012' .
        IF sy-subrc = 0 .
          lv_remarks = w_text-longtext .
        ENDIF .

        READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z007' .
        IF sy-subrc = 0 .
          lv_no_pck = w_text-longtext .
        ENDIF .

*        READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z011' .
*        IF sy-subrc = 0 .
*          lv_gross = w_text-longtext .
*        ENDIF .

*        READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z016' .
*        IF sy-subrc = 0 .
*          vcode = w_text-longtext .
*        ENDIF .

        READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z017' .
        IF sy-subrc = 0 .
          lv_cus_pl = w_text-longtext .
        ENDIF .

        READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z010' .
        IF sy-subrc = 0 .
          lv_exp_no = w_text-longtext .
        ENDIF .

        READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z016'.
        IF sy-subrc = 0 .
          head_lut = w_text-longtext .
        ENDIF .

        READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z020'.
        IF sy-subrc = 0 .
          lv_comp_name = w_text-longtext .
        ENDIF .

        READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z019'.
        IF sy-subrc = 0 .
          lv_insur_policy_no = w_text-longtext .
        ENDIF.

        READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z018'.
        IF sy-subrc = 0 .
          lv_gr_number = w_text-longtext .
        ENDIF.

* add by sb 11032025
        IF w_final-plant = '4001'.
          lv_comp_name = 'TATA AIG GENERAL INSURANCE CO. LTD'.
          lv_insur_policy_no = 'NI6520007733'.
        ELSEIF w_final-plant = '2001' AND w_final-distributionchannel = '50' .
          lv_comp_name = 'TATAAIG GENERAL INSURANCE COMPANY LIMITED.'.
          lv_insur_policy_no = '6520010817'.
        ELSEIF w_final-plant = '2001'.
          lv_comp_name = 'THE ORIENTAL INSURANCE CO. LTD.'.
          lv_insur_policy_no = '233700/21/2023/14'.
        ELSEIF w_final-plant = '3001'.
          lv_comp_name = 'TATAAIG GENERAL INSURANCE COMPANY LIMITED.'.
          lv_insur_policy_no = '6520010817'.
        ELSE.
          lv_comp_name = 'Tata AIG general Insurance company Limited'.
          lv_insur_policy_no = '0865100768'.
        ENDIF.
* add by sb 11032025
        SELECT SINGLE customer,
               plant,
               vendor
               FROM zsd_vendor_code
               WHERE customer = @w_final-soldtoparty AND plant = @w_final-plant
               INTO @DATA(ls_vcode).

        vcode = ls_vcode-vendor.

        DATA : lv_bill_adr1 TYPE c LENGTH 100.
        DATA : lv_bill_adr2 TYPE c LENGTH 100.
        DATA : lv_bill_adr3 TYPE c LENGTH 100.

        DATA : lv_shp_adr1 TYPE c LENGTH 100.
        DATA : lv_shp_adr2 TYPE c LENGTH 100.
        DATA : lv_shp_adr3 TYPE c LENGTH 100.

        """"""" bill address set """"""""
        IF w_final-re_house_no IS NOT INITIAL .
          lv_bill_adr1 = |{ w_final-re_house_no }| .
        ENDIF .

        IF w_final-re_street IS NOT INITIAL .
          IF lv_bill_adr1 IS NOT INITIAL   .
            lv_bill_adr1 = |{ lv_bill_adr1 } , { w_final-re_street }, { w_final-re_streetprefixname1 }, { w_final-re_streetprefixname2 }, { w_final-re_streetsuffixname1 }| .
          ELSE .
            lv_bill_adr1 = |{ w_final-re_street }, { w_final-re_streetprefixname1 }, { w_final-re_streetprefixname2 }, { w_final-re_streetsuffixname1 }| .
          ENDIF .
        ENDIF .

        IF w_final-re_street1 IS NOT INITIAL .
          IF lv_bill_adr1 IS NOT INITIAL   .
            lv_bill_adr1 = |{ lv_bill_adr1 } , { w_final-re_street1 }, { w_final-re_streetprefixname1 }, { w_final-re_streetprefixname2 }, { w_final-re_streetsuffixname1 }| .
          ELSE .
            lv_bill_adr1 = |{ w_final-re_street1 }, { w_final-re_streetprefixname1 }, { w_final-re_streetprefixname2 }, { w_final-re_streetsuffixname1 }| .
          ENDIF .
        ENDIF .

        DATA(len) = strlen( lv_bill_adr1 ) .
        len = len - 40.
        IF strlen( lv_bill_adr1 ) GT 40 .
          lv_bill_adr2 = |{ lv_bill_adr1+40(len) },| .
          lv_bill_adr1 = lv_bill_adr1+0(40) .
        ENDIF .
        """""""eoc bill address set""""""""


        """"""" ship address set """"""""

        IF w_final-we_house_no IS NOT INITIAL .
          lv_shp_adr1 = |{ w_final-we_house_no }| .
        ENDIF .

        IF w_final-we_street IS NOT INITIAL .
          IF lv_shp_adr1 IS NOT INITIAL   .
            lv_shp_adr1 = |{ lv_shp_adr1 } , { w_final-we_street }, { w_final-we_streetprefixname1 }, { w_final-we_streetprefixname2 }, { w_final-we_streetsuffixname1 }| .
          ELSE .
            lv_shp_adr1 = |{ w_final-we_street }, { w_final-we_streetprefixname1 }, { w_final-we_streetprefixname2 }, { w_final-we_streetsuffixname1 }| .
          ENDIF .
        ENDIF .

        IF w_final-we_street1 IS NOT INITIAL .
          IF lv_shp_adr1 IS NOT INITIAL   .
            lv_shp_adr1 = |{ lv_shp_adr1 } , { w_final-we_street1 }, { w_final-we_streetprefixname1 }, { w_final-we_streetprefixname2 }, { w_final-we_streetsuffixname1 }| .
          ELSE .
            lv_shp_adr1 = |{ w_final-we_street1 }, { w_final-we_streetprefixname1 }, { w_final-we_streetprefixname2 }, { w_final-we_streetsuffixname1 }| .
          ENDIF .
        ENDIF .

        len = strlen( lv_shp_adr1 ) .
        len = len - 40.
        IF strlen( lv_shp_adr1 ) GT 40 .
          lv_shp_adr2 = |{ lv_shp_adr1+40(len) },| .
          lv_shp_adr1 = lv_shp_adr1+0(40) .
        ENDIF .

        """"""" ship bill address set """"""""
        DATA : heading      TYPE c LENGTH 100,
               sub_heading  TYPE c LENGTH 255,
               for_sign     TYPE c LENGTH 100,
               curr         TYPE c LENGTH 100,
               exp_curr     TYPE c LENGTH 100,
               exc_rt       TYPE c LENGTH 100,
               lv_delv_term TYPE c LENGTH 100.

        DATA : lv_dt_bill TYPE c LENGTH 10.
        DATA : lv_dt_po TYPE c LENGTH 10.
        DATA : lv_dt_ack TYPE c LENGTH 10.

        lv_exchng_rate = w_final-accountingexchangerate .
        exc_rt = lv_exchng_rate.
        SELECT SINGLE * FROM zsd_einv_data WHERE billingdocument = @w_final-billingdocument
          INTO @DATA(w_einvvoice) .           "#EC CI_ALL_FIELDS_NEEDED
        CLEAR : lv_qr_code , lv_irn_num   , lv_ack_no ,lv_ack_date .

        lv_qr_code = w_einvvoice-signedqrcode .
        lv_irn_num = w_einvvoice-irn .
        lv_ack_no =  w_einvvoice-ackno .
        lv_ack_date =  w_einvvoice-ackdt+8(2) && '/' && w_einvvoice-ackdt+5(2) && '/' && w_einvvoice-ackdt+0(4) .
        lv_eway     = w_einvvoice-ewbno.
        lv_eway_dt  = w_einvvoice-ewbdt+6(2) && '/' && w_einvvoice-ewbdt+4(2) && '/' &&  w_einvvoice-ewbdt+0(4)." && '/'  ." 2024-04-27
        """sub_heading = '(Issued Under Section 31 of Central Goods & Service Tax Act 2017 and HARYANA State Goods & Service Tax Act 2017)' .
        sub_heading = '' .
*        for_sign  = 'Bhagwati Lacto Vegetarian Exports (Pvt.) Ltd.'  ##NO_TEXT. comment by sb03112025
*          for_sign  = lv_plant_name. "add by sb110325
        """""" Date conversion """"
        lv_dt_bill  = w_final-billingdocumentdate+6(2) && '/' && w_final-billingdocumentdate+4(2) && '/' && w_final-billingdocumentdate+0(4).
        lv_dt_ack = lv_ack_date."+6(2) && '/' && lv_ack_date+4(2) && '/' && lv_ack_date+0(4).
        lv_dt_po = w_final-customerpurchaseorderdate+6(2) && '/' && w_final-customerpurchaseorderdate+4(2) && '/' && w_final-customerpurchaseorderdate+0(4).
        """""" Date Conversion """"

        IF im_prntval = 'Original' ##NO_TEXT.
          ""odte_text = |Original                                   Duplicate                                 Triplicate                                      Extra| ##NO_TEXT.
          odte_text = 'Original' ##NO_TEXT.
        ELSEIF im_prntval = 'Duplicate' ##NO_TEXT.
          odte_text = 'Duplicate' ##NO_TEXT.
        ELSEIF im_prntval = 'Triplicate' ##NO_TEXT.
          odte_text = 'Triplicate' ##NO_TEXT.
        ELSEIF im_prntval = 'Extra' ##NO_TEXT.
          odte_text = 'Extra Invoice Copy' ##NO_TEXT.
        ENDIF.

        IF iv_action = 'taxinv' .

          heading = 'TAX INVOICE' ##NO_TEXT.

          IF w_final-distributionchannel = '30' .
            "heading = 'EXPORT INVOICE'  .
            IF w_final-item_igstrate IS INITIAL .
              sub_heading = 'Issued Under Section 31 of Central Goods and Service Tax Act 2017 and HARYANA State Goods and Service Tax Act 2017' ##NO_TEXT.
              "*head_lut = 'Against LUT No.(ARN No. AD060323015122V DT. 29/03/23'.
            ELSE .
              sub_heading = 'Issued Under Section 31 of Central Goods and Service Tax Act 2017 and HARYANA State Goods and Service Tax Act 2017' ##NO_TEXT.
              "" head_lut = 'Against LUT No.(ARN No. AD060323015122V DT. 21/03/23'.
            ENDIF .
          ELSE .
            "heading = 'TAX INVOICE'  .
            sub_heading = 'Under Section 31 of CGST Act and SGST Act read with section 20 of IGST Act' ##NO_TEXT.
          ENDIF .

        ELSEIF iv_action = 'oeminv' .

          heading = 'TAX INVOICE'  ##NO_TEXT.
          sub_heading = 'Issued Under Section 31 of Central Goods and Service Tax Act 2017 and HARYANA State Goods and Service Tax Act 2017' ##NO_TEXT.

        ELSEIF iv_action = 'dchlpr' ##NO_TEXT.

          heading     = 'DELIVERY CHALLAN' ##NO_TEXT.
*          IF w_final-billingdocumenttype = 'JSN' ##NO_TEXT. comment by sb 15102025 arvind
*            heading     = 'JOB WORK CHALLAN' ##NO_TEXT.
*add by sb 31102025
          IF w_final-distributionchannel = '50'.
            footer1 = '1.Packing Material dispatched in this vehicle is for packing of Paddy/Rice only not for Sale.'.
            footer2 = '2.Packing Material dispatched in this vehicle is for packing of Rice for Export purpose only not for Domestic Sales'.
            footer3 = '3.Goods dispatched in this vehicle to hired godown for Repacking and Exports only not to be sold in domestic Market.'.
            footer4 = '4.Machinery part dispatched in this vehicle is for Repair purpose only not for sales.'.
            footer5 = '5.All disputes are subject to FIROZPUR Jurisdiction only.'.
          ELSE.
            footer1 = '1. Goods once sold not be taken back after 3 month'.
            footer2 = '2. Interest @ 24%p.a. will be charged if payment not made within 30 days'.
            footer3 = '3. Our responsibility cases once goods leave our promises'.
            footer4 = '4. If payment will not received within 2-3 days, amount will be charged as per invoice against 2%'.
            footer5 = '5. Our sale bill should contain followinf declaration- goods sold bear unregistered brand names excempt supply under. All disputes are subject to FIROZPUR Jurisdiction only. '.
          ENDIF.

*          ENDIF.
          IF im_prntval = 'Original' ##NO_TEXT.
            odte_text = |Original                                   Duplicate                                 Triplicate                                      Extra| ##NO_TEXT.
          ELSEIF im_prntval = 'Duplicate' ##NO_TEXT.
            odte_text = 'Duplicate Challan' ##NO_TEXT.
          ELSEIF im_prntval = 'Triplicate' ##NO_TEXT.
            odte_text = 'Triplicate Challan' ##NO_TEXT.
          ELSEIF im_prntval = 'Extra' ##NO_TEXT.
            odte_text = 'Extra Challan Copy' ##NO_TEXT.
          ENDIF.

          """"""""""""""""""" bill to party equals ship to party in challan case .
          w_final-ship_to_party = w_final-bill_to_party .
          w_final-we_name  = w_final-re_name .
          lv_shp_adr1 = lv_bill_adr1 .
          lv_shp_adr2 = lv_bill_adr2 .
          w_final-we_city  = w_final-re_city .
          w_final-we_pin = w_final-re_pin   .
          w_final-we_tax  = w_final-re_tax  .
          w_final-we_pan = w_final-re_pan  .
          w_final-we_region = w_final-re_region  .
          w_final-we_city = w_final-re_city .
          w_final-we_city = w_final-re_city  .
          w_final-we_phone4  = w_final-re_phone4  .
          w_final-we_email = w_final-re_email .

          "w_final-PurchaseOrderByCustomer = w_final-PurchaseOrder .

          lv_dt_po = lv_dt_bill .

          CLEAR : exc_rt .  """" will add read text of nature of work ...
          READ TABLE gt_text INTO w_text WITH KEY longtextid = 'Z018' .
          IF sy-subrc = 0 .
            exc_rt = w_text-longtext .
          ENDIF .
          """"""""""""""""""" bill to party equals ship to party in challan case .

        ELSEIF iv_action = 'dcnote' ##NO_TEXT.
          IF w_final-billingdocumenttype = 'G2' OR w_final-billingdocumenttype = 'CBRE'.
            heading = 'CREDIT NOTE' ##NO_TEXT.
            sub_heading = 'Issued Under Section 31 of Central Goods and Service Tax Act 2017 and HARYANA State Goods and Service Tax Act 2017' ##NO_TEXT.
          ELSEIF w_final-billingdocumenttype = 'L2' ##NO_TEXT.
            heading = 'DEBIT NOTE' ##NO_TEXT.
            sub_heading = 'Issued Under Section 31 of Central Goods and Service Tax Act 2017 and HARYANA State Goods and Service Tax Act 2017' ##NO_TEXT.
          ENDIF .

        ELSEIF iv_action = 'aftinv' ##NO_TEXT.

          heading = 'TAX INVOICE' ##NO_TEXT.
          sub_heading = 'Issued Under Section 31 of Central Goods and Service Tax Act 2017 and HARYANA State Goods and Service Tax Act 2017' ##NO_TEXT.

        ELSEIF iv_action = 'bsupply' ##NO_TEXT.

          heading = 'Bill Of Supply' ##NO_TEXT.

        ENDIF .

        curr     = w_final-transactioncurrency .
        exp_curr = w_final-transactioncurrency.

        CONDENSE : exc_rt , curr , heading , sub_heading , head_lut , for_sign ,  lv_shp_adr1 , lv_shp_adr2, exp_curr.


        SELECT SINGLE * FROM zi_regiontext WHERE region = @w_final-region AND language = 'E' AND country = @w_final-country
         INTO @DATA(lv_st_nm).                "#EC CI_ALL_FIELDS_NEEDED

        SELECT SINGLE * FROM zi_regiontext  WHERE region = @w_final-re_region AND language = 'E' AND country = @w_final-re_country
        INTO @DATA(lv_st_name_re).            "#EC CI_ALL_FIELDS_NEEDED

        SELECT SINGLE * FROM zi_regiontext  WHERE region = @w_final-we_region AND language = 'E' AND country = @w_final-we_country
        INTO @DATA(lv_st_name_we).            "#EC CI_ALL_FIELDS_NEEDED

        SELECT SINGLE * FROM zi_countrytext  WHERE country = @w_final-country AND language = 'E'
        INTO @DATA(lv_cn_nm).                 "#EC CI_ALL_FIELDS_NEEDED

        SELECT SINGLE * FROM zi_countrytext   WHERE country = @w_final-re_country AND language = 'E'
        INTO @DATA(lv_cn_name_re).            "#EC CI_ALL_FIELDS_NEEDED

        SELECT SINGLE * FROM zi_countrytext   WHERE country = @w_final-we_country AND language = 'E'
        INTO @DATA(lv_cn_name_we).            "#EC CI_ALL_FIELDS_NEEDED

        IF iv_action = 'dchlpr' .
          IF w_final-billingdocumenttype = 'F8'.
            SELECT SINGLE salesdocument FROM i_billingdocumentitem
                 WHERE billingdocument = @w_final-billingdocument
                   AND billingdocumentitem = @w_final-billingdocumentitem
                  INTO @w_final-purchaseorderbycustomer.
          ELSE.
            w_final-purchaseorderbycustomer = w_final-purchaseorder .
          ENDIF.
          CLEAR: exc_rt.
        ENDIF.

        DATA:
          lv_plant_addrs1 TYPE string,
          lv_plant_addrs2 TYPE string,
          lv_plant_addrs3 TYPE string,
          lv_plant_cin    TYPE string.

        SELECT SINGLE * FROM zi_plant_address
                        WHERE plant = @w_final-plant
                        INTO @DATA(ls_plant_adrs).

        lv_plant_addrs1 = ls_plant_adrs-streetname.
        lv_plant_addrs2 = |{ ls_plant_adrs-cityname } - { ls_plant_adrs-postalcode }|.
        lv_plant_addrs3 = |{ ls_plant_adrs-region } - { ls_plant_adrs-addresstimezone }|.

        lv_delv_term = |{ w_final-incotermsclassification } ({ w_final-incotermslocation1 })|.
        """ add by sb 11/12/2025"""
        IF w_final-plant = '1001'.
          lv_plant_cin = 'U35106PB2007PTC031212' ##NO_TEXT.
        ELSEIF w_final-plant = '2001'.
          lv_plant_cin = 'U15203PB2009PTC032538' ##NO_TEXT.
        ELSEIF w_final-plant = '3001'.
          lv_plant_cin = 'U01100GJ2020PTC113206' ##NO_TEXT.
        ELSEIF w_final-plant = '4001'.
          lv_plant_cin = 'U10612PB2021PTC053299' ##NO_TEXT.
        ENDIF.
        """ add by sb 11/12/2025"""
        IF w_final-plant = '1001'.

          lv_plant_name   = 'Bhagwati Lacto Vegetarian Exports Pvt. Ltd.' ##NO_TEXT.
          lv_plant_email = 'sales@blvexports.com'.
          lv_fssai_lic_no = '12121241000020'.

        ELSEIF w_final-plant = '2001'.

          lv_plant_name   = 'Bhagwati Lacto Foods Pvt Ltd' ##NO_TEXT.
          w_final-plant_email = ''.

        ELSEIF w_final-plant = '3001'.

          lv_plant_name   = 'Healthy Harvested Foods Private Limited' ##NO_TEXT.
          w_final-plant_email = ''.

        ELSEIF w_final-plant = '4001'.
          lv_plant_name   = 'Bhagwati Agrochem Private Limited' ##NO_TEXT.
          w_final-plant_email = ''.
        ENDIF.

        for_sign  = lv_plant_name. "add by sb110325
        w_final-phoneareacodesubscribernumber = ''.
        lv_place_supply = w_final-we_city && '-' && lv_st_name_we-regionname. "lv_cn_name_we-CountryName.
        REPLACE ALL OCCURRENCES OF '&' IN lv_bill_adr1 WITH ''.
        REPLACE ALL OCCURRENCES OF '&' IN lv_shp_adr1 WITH ''.

        IF w_final-distributionchannel = '30' OR w_final-distributionchannel = '40'.
          head_lut = |LUT No: { head_lut }| ##NO_TEXT.
        ELSE.
          CLEAR: head_lut.
        ENDIF.

        lv_broker_name = ''.

        SELECT SINGLE
        salesdocument,
        supplier
        FROM i_salesdocumentpartner
                WHERE salesdocument = @w_final-salesdocument
                  AND partnerfunction = 'SP'
                INTO @DATA(is_sale).

        SELECT SINGLE
        businesspartner,
        businesspartnername
        FROM i_businesspartner
        WHERE businesspartner = @is_sale-supplier
        INTO @DATA(is_brokr).

        lv_broker_name = is_brokr-businesspartnername.

        CLEAR w_final-customerpaymenttermsname.
        SELECT SINGLE paymenttermsdescription FROM i_paymenttermstext
        WHERE paymentterms = @w_final-customerpaymentterms INTO @w_final-customerpaymenttermsname.

  """add by sb 23122025""
        if w_final-DistributionChannel = '20' and w_final-Plant = '1001'.
        w_final-plant_gstin = '24AADCB4295Q1Z6'.
        ENDIF.
"""add by sb 23122025""

        DATA(lv_xml) = |<Form>| &&
                       |<BillingDocumentNode>| &&
                       |<heading>{ heading }</heading>| &&
                       |<sub_heading>{ sub_heading }</sub_heading>| &&
                       |<head_lut>{ head_lut }</head_lut>| &&
                       |<for_sign>{ for_sign }</for_sign>| &&
                       |<odte_text>{ odte_text }</odte_text>| &&
                       |<broker_name>{ lv_broker_name }</broker_name>| &&
                       |<doc_curr>{ curr }</doc_curr>| &&
                       |<exp_curr>{ exp_curr }</exp_curr>| &&
                       |<plant_code>{ w_final-plant }</plant_code>| &&
                       |<plant_name>{ lv_plant_name }</plant_name>| &&
                       |<plant_address_l1>{ lv_plant_addrs1 }</plant_address_l1>| &&
                       |<plant_address_l2>{ lv_plant_addrs2 }</plant_address_l2>| &&
                       |<plant_address_l3>{ lv_plant_addrs3 }</plant_address_l3>| &&
                        |<plant_cin>{ lv_plant_cin }</plant_cin>| &&
                        |<plant_gstin>{ w_final-plant_gstin }</plant_gstin>| &&
                        |<plant_pan>{ w_final-plant_gstin+2(10) }</plant_pan>| &&
                        |<plant_state_code>{ w_final-region } ({ ls_plant_adrs-regionname })</plant_state_code>| &&
                        |<plant_state_name></plant_state_name>| &&
                        |<plant_phone>{ w_final-phoneareacodesubscribernumber }</plant_phone>| &&
                        |<plant_email>{ lv_plant_email }</plant_email>| &&
                        |<billto_code>{ w_final-bill_to_party }</billto_code>| &&
                        |<billto_name>{ w_final-re_name }</billto_name>| &&
                        |<billto_address_l1>{ lv_bill_adr1 }</billto_address_l1>| &&
                        |<billto_address_l2>{ lv_bill_adr2 }{ w_final-re_city }</billto_address_l2>| &&
                        |<billto_address_l3>{ w_final-re_pin } ({ lv_cn_name_re-countryname })</billto_address_l3>| &&
*                        |<billto_cin>{ W_FINAL-re }</billto_cin>| &&
                        |<billto_gstin>{ w_final-re_tax }</billto_gstin>| &&
                        |<billto_pan>{ w_final-re_pan }</billto_pan>| &&
                        |<billto_state_code>{ w_final-re_region } ({ lv_st_name_re-regionname })</billto_state_code>| &&
                        |<billto_state_name></billto_state_name>| &&
                        |<billto_place_suply>{ w_final-re_region }</billto_place_suply>| &&
                        |<billto_phone>{ w_final-re_phone4 }</billto_phone>| &&
                        |<billto_email>{ w_final-re_email }</billto_email>| &&

                        |<shipto_code>{ w_final-ship_to_party }</shipto_code>| &&
                        |<shipto_name>{ w_final-we_name }</shipto_name>| &&
                        |<shipto_address_l1>{ lv_shp_adr1 }</shipto_address_l1>| &&
                        |<shipto_address_l2>{ lv_shp_adr2 }{ w_final-we_city }</shipto_address_l2>| &&
                        |<shipto_address_l3>{ w_final-we_pin } ({ lv_cn_name_we-countryname })</shipto_address_l3>| &&
*                        |<shipto_cin>{ W_FINAL-PlantName }</shipto_cin>| &&
                        |<shipto_gstin>{ w_final-we_tax }</shipto_gstin>| &&
                        |<shipto_pan>{ w_final-we_pan }</shipto_pan>| &&
                        |<shipto_state_code>{ w_final-we_region } ({ lv_st_name_we-regionname })</shipto_state_code>| &&
                        |<shipto_state_name>{ lv_st_name_we-regionname }</shipto_state_name>| &&
                        |<shipto_place_suply>{ lv_place_supply }</shipto_place_suply>| &&
                        |<shipto_phone>{ w_final-we_phone4 }</shipto_phone>| &&
                        |<shipto_email>{ w_final-we_email }</shipto_email>| &&

                        |<inv_no>{  w_final-billingdocument }  </inv_no>| &&
                        |<inv_date>{ lv_dt_bill }</inv_date>| &&
                        |<inv_ref>{ w_final-documentreferenceid }</inv_ref>| &&
                        |<exchange_rate>{ exc_rt }</exchange_rate>| &&
                        |<currency>{ w_final-transactioncurrency }</currency>| &&
                        |<Exp_Inv_No>{ lv_exp_no }</Exp_Inv_No>| &&       """""""
                        |<IRN_num>{ lv_irn_num }</IRN_num>| &&
                        |<IRN_ack_No>{ lv_ack_no }</IRN_ack_No>| &&
                        |<irn_ack_date>{ lv_dt_ack }</irn_ack_date>| &&
                        |<irn_doc_type></irn_doc_type>| &&     """"""
                        |<irn_category></irn_category>| &&     """"""
                        |<qrcode>{ lv_qr_code }</qrcode>| &&
                        |<vcode>{ vcode }</vcode>| &&    """"" USING ZTABLE DATA TO BE MAINTAINED ...
                        |<vplant>{ lv_cus_pl }</vplant>| &&

                        |<sale_order>{ sale_order }</sale_order>| &&

                        |<pur_odr_no>{ w_final-purchaseorderbycustomer }</pur_odr_no>| &&
                        |<pur_odr_date>{ lv_dt_po }</pur_odr_date>| &&
                        |<Pay_term>{ w_final-customerpaymenttermsname }</Pay_term>| &&  """"
                        |<Delivery_term>{ lv_delv_term }</Delivery_term>| &&  """"
                        |<Veh_no>{ lv_vehicle }</Veh_no>| &&    """"" badi to save ewaybill & einvoice data from DRC
                        |<Trans_mode>{ lv_transmode }</Trans_mode>| &&
                        |<tranporter_name>{ lv_tranporter_name }</tranporter_name>| &&
                        |<Ewaybill_no>{ lv_eway }</Ewaybill_no>| &&
                        |<Ewaybill_date>{ lv_eway_dt }</Ewaybill_date>| &&

                        |<footer1>{ footer1 }</footer1>| &&
                        |<footer2>{ footer2 }</footer2>| &&
                        |<footer3>{ footer3 }</footer3>| &&
                        |<footer4>{ footer4 }</footer4>| &&
                        |<footer5>{ footer5 }</footer5>| &&

                       |<ItemData>| .

        DATA : lv_item TYPE string .
        DATA : srn      TYPE c LENGTH 3,
               lv_matnr TYPE c LENGTH 120.
        CLEAR : lv_item , srn .

        LOOP AT it_final INTO DATA(w_item) .
          SELECT COUNT( * ) FROM i_product WHERE product = @w_item-product AND producttype IN ( 'KMAT', 'LEIH','NLAG','SBPD','SBRE','SERV','UNBW','ZCJW','ZNVL','ZRTP','ZSER' ).
          IF sy-subrc IS INITIAL.
            lv_flag_pol = 'X'.
          ENDIF.
          srn = srn + 1 .

*          IF iv_action = 'dchlpr' .
*            w_item-MaterialByCustomer = w_item-product .
*
*            IF w_item-item_pcip_amt IS NOT INITIAL .
*              w_item-item_unitprice = w_item-item_pcip_amt .
*            ELSEIF w_item-item_unitprice IS NOT INITIAL .
*              w_item-item_unitprice = w_item-item_unitprice .
*            ENDIF .
*
*          ENDIF .

          """          IF w_final-DistributionChannel = '30' .
*          IF iv_action NE 'aftinv' .
*            IF w_item-ConditionQuantity IS NOT INITIAL .
*              w_item-item_unitprice = w_item-item_unitprice / w_item-ConditionQuantity .
*            ENDIF .
*          ENDIF .

          IF iv_action = 'dchlpr' AND w_item-item_unitprice IS INITIAL.  ""IV_ACTION
            w_item-item_unitprice  = w_item-item_pcip_amt.
            w_item-item_totalamount_inr = w_item-item_pcip_amt * w_item-billingquantity.
          ENDIF.

          w_item-item_amotization = w_item-item_amotization  *   w_final-accountingexchangerate  .
          w_item-item_totalamount_inr = w_item-billingquantity * w_final-accountingexchangerate * w_item-item_unitprice .
          w_item-item_discountamount = w_item-item_discountamount *   w_final-accountingexchangerate  .

*          w_item-item_assessableamount = w_item-item_totalamount_inr -  w_item-item_discountamount.

          lv_tax_amt      = lv_tax_amt + w_item-item_assessableamount .

          IF w_item-distributionchannel = '80'.

            w_item-item_assessableamount = w_item-item_assessableamount +
                                           w_item-item_fert_oth +
                                           w_item-item_freight +
                                           w_item-item_pkg_chrg +
                                           w_item-item_amotization.

          ELSE.

            w_item-item_assessableamount = w_item-item_assessableamount +
*                                           w_item-item_fert_oth +
*                                           w_item-item_freight +
                                           w_item-item_othercharge +
                                           w_item-item_pkg_chrg +
                                           w_item-item_amotization.
          ENDIF.

          w_item-item_sgstamount = w_item-item_assessableamount  *     w_item-item_cgstrate / 100  .
          w_item-item_cgstamount = w_item-item_assessableamount  *    w_item-item_cgstrate / 100    .
          w_item-item_igstamount = w_item-item_assessableamount  *   w_item-item_igstrate / 100   .


          lv_total_insur_amt = lv_total_insur_amt + w_item-item_insurance_amt.
          lv_qty         = lv_qty  +   w_item-billingquantity .
          lv_dis         = lv_dis + w_item-item_discountamount .
          lv_tot_cgst    = lv_tot_cgst  + w_item-item_cgstamount .
          lv_tot_sgst    = lv_tot_sgst  + w_item-item_sgstamount .
          lv_tcs         = lv_tcs +  w_item-item_othercharge .
          lv_other_chrg  = lv_other_chrg + w_item-item_freight .
          lv_round_off   = lv_round_off +  w_item-item_roundoff .
          sum_other_chrg = sum_other_chrg + w_item-item_fert_oth.
          lv_tax_amt1    = lv_tax_amt1 + w_item-item_assessableamount .
          """"       ENDIF

          DATA : lv_item_text TYPE string .
          CLEAR : lv_item_text .

          IF w_item-materialdescriptionbycustomer IS INITIAL.
            lv_item_text = w_item-billingdocumentitemtext.
          ELSE.
            lv_item_text = w_item-materialdescriptionbycustomer.
          ENDIF.

          SELECT SINGLE
          salesdocument,
          salesdocumentitem,
          materialbycustomer
          FROM i_salesdocumentitem
          WHERE salesdocument = @w_item-salesdocument AND salesdocumentitem = @w_item-salesdocumentitem
          INTO @DATA(ls_sale_doc).

          IF ls_sale_doc-materialbycustomer IS NOT INITIAL.
            w_item-materialbycustomer = ls_sale_doc-materialbycustomer.
          ENDIF.

          IF w_item-materialbycustomer IS NOT INITIAL. "w_item-ProductOldID IS NOT INITIAL.
            lv_matnr = w_item-materialbycustomer.       "w_item-ProductOldID.
          ELSE.
            lv_matnr = w_item-product.
          ENDIF.

          lv_ref_sddoc = w_item-materialbycustomer.

          SELECT SINGLE
          pricelisttype
          FROM i_billingdocument
          WHERE billingdocument = @w_item-billingdocument
          INTO @DATA(price_type).

          REPLACE ALL OCCURRENCES OF '&' IN lv_item_text WITH '' .
          REPLACE ALL OCCURRENCES OF '&' IN lv_ref_sddoc WITH '' .

          IF w_item-conditionquantity IS NOT INITIAL .
            lv_item_urate  = w_item-item_unitprice / w_item-conditionquantity .
            lv_item_amtinr = w_item-item_totalamount_inr / w_item-conditionquantity.
            lv_item_amtexp = lv_item_urate * w_item-billingquantity.
            w_item-item_igstamount       = w_item-item_igstamount / w_item-conditionquantity.
          ELSE.
            lv_item_urate  = w_item-item_unitprice.
            lv_item_amtinr = w_item-item_totalamount_inr.
            lv_item_amtexp = w_item-item_unitprice * w_item-billingquantity.
          ENDIF.

          IF iv_action = 'oeminv' OR iv_action = 'bsupply' OR iv_action = 'dcnote' OR iv_action = 'dchlpr'.
********************************
            IF w_item-billingdocumenttype = 'JSN' OR w_item-billingdocumenttype = 'F8'.
              SELECT SINGLE billingquantity FROM i_billingdocumentitem
                   WHERE billingdocument = @w_item-billingdocument
                     AND billingdocumentitem = @w_item-billingdocumentitem
                    INTO @w_item-netweight.
            ELSE.
              w_item-netweight = w_item-netweight / 100.
            ENDIF.
********************************
*            w_item-netweight = w_item-netweight / 100.
            IF price_type = 'S1' OR price_type = ' '.
              lv_item_amtinr   = lv_item_urate * w_item-netweight.
            ELSEIF price_type = 'S2'.
              lv_item_amtinr   = lv_item_urate * w_item-billingquantity.
            ENDIF.
          ENDIF.

          IF iv_action = 'taxinv'.
            lv_item_amtinr   = lv_item_urate * ( w_item-netweight * w_final-accountingexchangerate ).
            w_item-netweight = w_item-billingquantity.
            w_item-item_assessableamount = w_item-item_assessableamount * w_final-accountingexchangerate.
          ENDIF.

          IF iv_action = 'bsupply' AND w_item-netweight IS INITIAL.
            SELECT SINGLE netweight,
                          weightunit,
                          grossweight
                          FROM i_product WHERE product = @lv_matnr     "zmat_wt_data = Table
                         INTO @DATA(mat_wt).

            w_item-netweight = mat_wt-netweight * w_item-billingquantity.
            IF mat_wt-weightunit = 'KG'.
              w_item-netweight = w_item-netweight / 100.
            ENDIF.
            lv_gross         = lv_gross + ( mat_wt-grossweight * w_item-billingquantity ).
            lv_net           = lv_net   + ( mat_wt-netweight * w_item-billingquantity ).
            lv_item_amtinr = lv_item_urate * ( w_item-netweight * w_final-accountingexchangerate ).
            CLEAR: mat_wt.
          ENDIF.

          lv_amt_expcurr  = lv_amt_expcurr + lv_item_amtexp.
          lv_mrp_of_goods = w_item-item_zmrp_amount.
          lv_tot_amt      = lv_tot_amt +   lv_item_amtinr. "w_item-item_totalamount_inr .
          lv_tot_igst     = lv_tot_igst  + w_item-item_igstamount .

          IF w_item-billingquantityunit EQ 'ST'.
            w_item-billingquantityunit = 'NOS'.
          ENDIF.

          IF iv_action = 'dchlpr'.

            SELECT SINGLE product,
                   alternativeunit,
                   quantitynumerator,
                   quantitydenominator
                   FROM i_productunitsofmeasure
                   WHERE product = @w_item-product
                   AND   alternativeunit = 'BAG'
                   INTO @DATA(ls_bags).            "#EC CI_NO_TRANSFORM


            IF ls_bags-quantitydenominator IS NOT INITIAL.
              w_item-billingquantity = w_item-billingquantity / ( ls_bags-quantitynumerator / ls_bags-quantitydenominator ).
              IF w_item-billingdocumenttype = 'F8'.
                value = w_item-billingquantity.
                SPLIT value  AT '.' INTO: DATA(var) DATA(var1).
                IF var1+0(1) LE '5'.
                  w_item-billingquantity = floor( w_item-billingquantity ).
                ELSE.
                  w_item-billingquantity = ceil( w_item-billingquantity ).
                ENDIF.
              ENDIF.
            ENDIF.

          ENDIF.

          IF  w_item-item_igstamount IS NOT INITIAL.
            lv_gst_rate =  w_item-item_igstrate .
          ELSE.
            lv_gst_rate = w_item-item_sgstrate.
          ENDIF.
          lv_item = |{ lv_item }| && |<ItemDataNode>| &&
                    |<sno>{ srn }</sno>| &&
                    |<item_code>{ lv_matnr }</item_code>| &&
                    |<item_cust_refno>{ lv_ref_sddoc }</item_cust_refno>| &&
                    |<item_desc>{ lv_item_text }</item_desc>| &&
                    |<item_hsn_code>{ w_item-hsn }</item_hsn_code>| &&
                    |<mrp_of_goods>{ lv_mrp_of_goods }</mrp_of_goods>| &&
                    |<item_uom>{ w_item-billingquantityunit }</item_uom>| &&
                    |<item_qty>{ w_item-billingquantity }</item_qty>| &&
                    |<item_unit_rate>{ lv_item_urate }</item_unit_rate>| &&
                    |<item_amt_inr>{ lv_item_amtinr }</item_amt_inr>| &&
                    |<item_amt_expcurr>{ lv_item_amtexp }</item_amt_expcurr>| &&
                    |<item_discount>{ w_item-item_discountamount }</item_discount>| &&
                    |<item_taxable_amt>{ w_item-item_assessableamount }</item_taxable_amt>| &&
                    |<item_sgst_rate>{ lv_gst_rate }</item_sgst_rate>| &&
                    |<item_sgst_amt>{ w_item-item_sgstamount }</item_sgst_amt>| &&
                    |<item_cgst_amt>{ w_item-item_cgstamount }</item_cgst_amt>| &&
                    |<item_cgst_rate>{ w_item-item_cgstrate }</item_cgst_rate>| &&
                    |<item_igst_amt>{ w_item-item_igstamount }</item_igst_amt>| &&
                    |<item_igst_rate>{ w_item-item_igstrate }</item_igst_rate>| &&
                    |<item_amort_amt>{ w_item-item_amotization }</item_amort_amt>| &&
                    |<item_weight>{ w_item-netweight }</item_weight>| &&
                    |<item_brand>{ w_item-materialbycustomer }</item_brand>| &&
                    |<item_nobag>{ w_item-billingquantity }</item_nobag>| &&
                    |</ItemDataNode>|  .

          lv_tot_pkgchrg = lv_tot_pkgchrg + w_item-item_pkg_chrg.
          lv_tot_amort = lv_tot_amort + w_item-item_amotization.
          lv_no_of_package  = lv_no_of_package + w_item-billingquantity.
          lv_no_of_bag  = lv_no_of_bag + w_item-billingquantity.
          total_item_wt = total_item_wt + w_item-netweight.

*          "lv_tot_igst1 = lv_tot_igst1 + ( ( w_item-item_assessableamount + w_item-item_freight + w_item-item_fert_oth ) * w_item-item_igstrate / 100 ) .
*          "lv_tot_cgst1 = lv_tot_cgst1 + ( ( w_item-item_assessableamount + w_item-item_freight + w_item-item_fert_oth ) * w_item-item_cgstrate / 100 ) .
*          "lv_tot_sgst1 = lv_tot_sgst1 + ( ( w_item-item_assessableamount + w_item-item_freight + w_item-item_fert_oth ) * w_item-item_sgstrate / 100 ) .

          lv_tot_sgst1 = lv_tot_sgst1 + ( ( w_item-item_assessableamount ) * w_item-item_sgstrate / 100 ) .
          lv_tot_cgst1 = lv_tot_cgst1 + ( ( w_item-item_assessableamount ) * w_item-item_cgstrate / 100 ) .
          lv_tot_igst1 = lv_tot_igst1 + ( ( w_item-item_assessableamount ) * w_item-item_igstrate / 100 ) .

        ENDLOOP .

        IF w_final-distributionchannel = '30' .

          lv_other_chrg  = lv_other_chrg * w_final-accountingexchangerate .
          sum_other_chrg = sum_other_chrg * w_final-accountingexchangerate .

          ""lv_tot_igst1 = ( lv_tax_amt + lv_other_chrg ) * w_item-item_igstrate / 100  .

          "" lv_tot_igst  = lv_tot_igst * w_final-AccountingExchangeRate .

          lv_grand_tot =  lv_tax_amt + lv_tot_sgst + lv_tot_cgst + lv_tot_igst1 + lv_total_insur_amt
                          + lv_other_chrg  + lv_round_off + sum_other_chrg + ( lv_dis * 1 ) + lv_tot_amort.  "" + lv_tcs

          lv_tot_gst = lv_tot_sgst + lv_tot_cgst + lv_tot_igst1 .
        ELSE .

          lv_other_chrg  = lv_other_chrg * w_final-accountingexchangerate.
          sum_other_chrg = sum_other_chrg * w_final-accountingexchangerate.

          "       lv_tot_igst1 = ( lv_tax_amt + lv_other_chrg ) * w_item-item_igstrate / 100  .
          "       lv_tot_cgst1 = ( lv_tax_amt + lv_other_chrg ) * w_item-item_cgstrate / 100  .
          "       lv_tot_sgst1 = ( lv_tax_amt + lv_other_chrg ) * w_item-item_sgstrate / 100  .
          "" lv_tot_igst  = lv_tot_igst * w_final-AccountingExchangeRate .

          lv_grand_tot =  lv_tax_amt + lv_tot_sgst1 + lv_tot_cgst1 + lv_tot_igst1
                          + lv_other_chrg  + lv_round_off  + lv_tcs + sum_other_chrg + lv_tot_amort + lv_tot_pkgchrg.

          lv_tot_gst = lv_tot_sgst1 + lv_tot_cgst1 + lv_tot_igst1 .

        ENDIF .

        DATA : lv_grand_tot_word TYPE string,
               lv_gst_tot_word   TYPE string.
        lv_grand_tot_word = lv_grand_tot .
        lv_gst_tot_word = lv_tot_gst .

        lo_amt_words->number_to_words(
         EXPORTING
           iv_num   = lv_grand_tot_word
         RECEIVING
           rv_words = DATA(grand_tot_amt_words)
       ).

        grand_tot_amt_words = |{ grand_tot_amt_words } Only| ##NO_TEXT.

        lo_amt_words->number_to_words(
          EXPORTING
            iv_num   = lv_gst_tot_word
          RECEIVING
            rv_words = DATA(gst_tot_amt_words)
        ).

        gst_tot_amt_words = |{ gst_tot_amt_words } Only| ##NO_TEXT.

        IF iv_action = 'dchlpr' .
          IF w_final-billingdocumenttype = 'F8'.

            lv_certify_1 = 'It is Certified that the particulars given above are true and correct and'
                        && 'amount indicated represents the price actually changed and that there'
                        && 'is no flow of additional consideration directly or indirectly from the buyer' ##NO_TEXT.

          ELSEIF w_final-billingdocumenttype = 'JSN'.

            lv_certify_1 = 'Tax is payable under reverse charge: Yes / No' ##NO_TEXT.
            lv_certify_2 = 'For JOB WORK / RETURNABLE MATERIAL DELIVERY CHALLAN, THE MATERIAL MUST BE SENT BACK'
                        && 'WITHIN 1 YEAR FOR CAPITAL GOODS LIKE FIXTURES, THE GOODS MUST BE SENT BACK WITHIN 3 YEAR' ##NO_TEXT.

          ENDIF.
        ENDIF.
        " add by sb 11032025
        IF w_final-plant = '4001'.
          lv_bank_name    = 'Indian Bank'.
          lv_bank_accno   = '8064847477'.
          lv_bank_branch  = 'LUDHIANA'.
          lv_bank_ifsc    = 'IDIB0M0363'.
        ELSEIF w_final-plant = '2001'.
          lv_bank_name    = 'State Bank of India'.
          lv_bank_accno   = '65176337258'.
          lv_bank_branch  = 'FIROZEPUR CANTT'.
          lv_bank_ifsc    = 'SBIN0050627'.
        ELSEIF w_final-plant = '3001'.
          lv_bank_name    = 'Indian Bank'.
          lv_bank_accno   = '7111281156'.
          lv_bank_branch  = 'Near Cheema Chowk'.
          lv_bank_ifsc    = 'IDIB0M0363'.
        ELSE.
          lv_bank_name    = 'PUNJAB NATIONAL BANK'.
          lv_bank_accno   = '0171008700006312'.
          lv_bank_branch  = 'FIROZEPUR CANTT'.
          lv_bank_ifsc    = 'PUNB0017100'.
        ENDIF.

        IF iv_action = 'oeminv' OR iv_action = 'bsupply' OR iv_action = 'dcnote' OR iv_action = 'dchlpr'.
          lv_gross = lv_gross / 100.
          lv_net   = lv_net / 100.
        ENDIF.
        IF lv_flag_pol = 'X'.
          CLEAR: lv_insur_policy_no,lv_comp_name.
        ENDIF.

        ""add by sb 12/04/2025""""
        IF iv_action = 'oeminv'.
          IF w_item-customerpricegroup = 'C1'.
            CLEAR: lv_insur_policy_no,
                   lv_comp_name.
          ENDIF.
        ENDIF.
        ""add by sb 12/04/2025""""

        lv_xml = |{ lv_xml }{ lv_item }| &&
                           |</ItemData>| &&

                        |<total_amount_words>(INR) { grand_tot_amt_words }</total_amount_words>| &&
                        |<gst_amt_words>(INR) { gst_tot_amt_words }</gst_amt_words>| &&
                        |<no_of_package>{ lv_no_of_package }</no_of_package>| &&
                        |<no_of_bag>{ lv_no_of_bag }</no_of_bag>| &&
*                        |<remark_if_any>{ lv_remarks }</remark_if_any>| &&
                        |<remark_if_any>{ lv_cus_pl }</remark_if_any>| &&
                        |<no_of_package>{ lv_no_pck }</no_of_package>| &&
                        |<total_Weight>{ lv_qty }</total_Weight>| &&
                        |<total_item_Weight>{ total_item_wt }</total_item_Weight>| &&
                        |<gross_Weight>{ lv_gross }</gross_Weight>| &&
                        |<net_Weight>{ lv_net }</net_Weight>| &&
                        |<total_insur_amt>{ lv_total_insur_amt }</total_insur_amt>| &&
                        |<tot_qty>{ lv_qty }</tot_qty>| &&  """ line item total quantity
                        |<total_amount>{ lv_tot_amt }</total_amount>| &&
                        |<total_discount>{ lv_dis }</total_discount>| &&
                        |<total_taxable_value>{ lv_tax_amt }</total_taxable_value>| &&
                        |<total_taxable_value1>{ lv_tax_amt1 }</total_taxable_value1>| &&
                        |<total_cgst>{ lv_tot_cgst }</total_cgst>| &&
                        |<total_sgst>{ lv_tot_sgst }</total_sgst>| &&
                        |<total_igst>{ lv_tot_igst }</total_igst>| &&
                        |<total_igst1>{ lv_tot_igst1 }</total_igst1>| &&  """ printing in total
                        |<total_sgst1>{ lv_tot_sgst1 }</total_sgst1>| &&  """ printing in total
                        |<total_cgst1>{ lv_tot_cgst1 }</total_cgst1>| &&  """ printing in total
                        |<total_amort>{ lv_tot_amort }</total_amort>| &&
                        |<sum_packing_chrg>{ lv_tot_pkgchrg }</sum_packing_chrg>| &&
                        |<total_tcs>{ lv_tcs }</total_tcs>| &&
                        |<total_other_chrg>{ lv_other_chrg }</total_other_chrg>| &&
                        |<sum_other_chrg>{ sum_other_chrg }</sum_other_chrg>| &&
                        |<round_off>{ lv_round_off }</round_off>| &&
                        |<grand_total>{ lv_grand_tot }</grand_total>| &&
                        |<total_amt_expcurr>{ lv_amt_expcurr }</total_amt_expcurr>| &&
                        |<certify_1>{ lv_certify_1 }</certify_1>| &&
                        |<certify_2>{ lv_certify_2 }</certify_2>| &&
                        |<certify_3>{ lv_certify_3 }</certify_3>| &&
                        |<fssai_lic_no>{ lv_fssai_lic_no }</fssai_lic_no>| &&
                        |<insur_policy_no>{ lv_insur_policy_no }</insur_policy_no>| &&
                        |<billto_fssai_no>{ lv_billto_fssai_no }</billto_fssai_no>| &&
                        |<gr_number>{ lv_gr_number }</gr_number>| &&
                        |<comp_name>{ lv_comp_name }</comp_name>| &&
                        |<bank_name>{ lv_bank_name }</bank_name>| &&
                        |<bank_accno>{ lv_bank_accno }</bank_accno>| &&
                        |<bank_branch>{ lv_bank_branch }</bank_branch>| &&
                        |<bank_ifsc>{ lv_bank_ifsc }</bank_ifsc>| &&
                        |</BillingDocumentNode>| &&
                        |</Form>|.

        DATA(ls_data_xml_64) = cl_web_http_utility=>encode_base64( lv_xml ).
        iv_xml_base64 = ls_data_xml_64.

      ENDMETHOD.
ENDCLASS.
