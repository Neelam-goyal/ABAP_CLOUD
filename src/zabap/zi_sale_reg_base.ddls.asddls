@AbapCatalog.sqlViewName: 'ZV_SREG_BASE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sale registerr'

@ClientHandling.algorithm: #SESSION_VARIABLE
@Analytics: {
dataCategory: #CUBE,
internalName:#LOCAL
}
@ObjectModel: {
usageType: {
dataClass: #MIXED,
serviceQuality: #D,
sizeCategory: #XXL
},
supportedCapabilities: [ #ANALYTICAL_PROVIDER, #SQL_DATA_SOURCE, #CDS_MODELING_DATA_SOURCE ],
modelingPattern: #ANALYTICAL_CUBE
}

define root view ZI_SALE_REG_BASE
  as select distinct from I_BillingDocumentItem      as b_item

    left outer join       I_BillingDocument          as bl          on bl.BillingDocument = b_item.BillingDocument

    
   
    left outer join       I_BillingDocumentTypeText as bltxt        on bltxt.BillingDocumentType = bl.BillingDocumentType
                                                                    and bltxt.Language = 'E'
                                                                        
    left outer join       I_DistributionChannelText as distbtxt     on distbtxt.DistributionChannel = bl.DistributionChannel
                                                                    and distbtxt.Language           = 'E'
    
    left outer join       I_MaterialDocumentItem_2   as md          on md.MaterialDocument = b_item.ReferenceSDDocument

    left outer join       I_SalesDocument            as so_head     on so_head.SalesDocument = b_item.SalesDocument


    left outer join       ZI_PARTNER_ADDRESS         as add_id      on add_id.BillingDocument = b_item.BillingDocument



    left outer join       I_CustomerMaterial_2       as custmat     on  custmat.SalesOrganization   = bl.SalesOrganization
                                                                    and custmat.DistributionChannel = bl.DistributionChannel
                                                                    and custmat.Customer            = bl.SoldToParty
                                                                    and custmat.Product             = b_item.Product
  //and custmat.Customer = bl.SoldToParty

  // left outer join I_PaymentTermsText as payterm on payterm.PaymentTerms = bl.CustomerPaymentTerms
    left outer join       I_CustomerPaymentTermsText as payterm     on  payterm.CustomerPaymentTerms = bl.CustomerPaymentTerms
                                                                    and payterm.Language             = 'E'


    left outer join       ZI_PRCD_ELEMENTS           as PRCD        on  PRCD.BillingDocument     = b_item.BillingDocument
                                                                    and PRCD.BillingDocumentItem = b_item.BillingDocumentItem
  
  
  //              and PRCD.ITEM_GRANDTOTALAMOUNT > 0
    left outer join       zsd_einvoice               as EINV        on EINV.billingdocument = b_item.BillingDocument

    left outer join       I_Plant                    as PLANT       on PLANT.Plant = b_item.Plant

    left outer join       I_OrganizationAddress      as plant_add   on plant_add.AddressID = PLANT.AddressID

    left outer join       I_ProductPlantBasic        as ITEM_DETAIL on  b_item.Product = ITEM_DETAIL.Product
                                                                    and b_item.Plant   = ITEM_DETAIL.Plant

    left outer join       I_SalesOrderItem           as so_item     on  b_item.SalesDocument     = so_item.SalesOrder
                                                                    and b_item.SalesDocumentItem = so_item.SalesOrderItem

      left outer join     I_DeliveryDocument       as del_data          on del_data.DeliveryDocument = so_item.ReferenceSDDocument
                                                                      
  
   
    left outer join       I_Product                  as item_det2   on item_det2.Product = b_item.Product

  //////// ADDED NEW COLUMNS AS PER REQUIRED .....

    left outer join       I_CreditMemoRequestItem    as cr_mri      on  cr_mri.CreditMemoRequest     = b_item.SalesDocument
                                                                    and cr_mri.CreditMemoRequestItem = b_item.SalesDocumentItem

    left outer join       I_DebitMemoRequestItem     as dm_mri      on  dm_mri.DebitMemoRequest     = b_item.SalesDocument
                                                                    and dm_mri.DebitMemoRequestItem = b_item.SalesDocumentItem

    left outer join       I_CustomerReturnItem       as cust_ri     on  cust_ri.CustomerReturn     = b_item.SalesDocument
                                                                    and cust_ri.CustomerReturnItem = b_item.SalesDocumentItem

    left outer join       I_SalesSchedgAgrmtItem     as s_sch_ag    on  s_sch_ag.SalesSchedulingAgreement     = b_item.SalesDocument
                                                                    and s_sch_ag.SalesSchedulingAgreementItem = b_item.SalesDocumentItem                                                                   
  ////////Item.DeliveryDocument
    
    left outer join        I_SalesDocumentPartner     as _saleitem  on _saleitem.SalesDocument = b_item.SalesDocument
                                                                    and _saleitem.PartnerFunction = 'SP'
                                                                    
                                                                    
      left outer join  I_BillingDocumentItemPrcgElmnt as _condtyp  on b_item.BillingDocument = _condtyp.BillingDocument   
      and b_item.BillingDocumentItem = _condtyp.BillingDocument                                                           

{

      @EndUserText.label: 'Company Code' //
  key bl.CompanyCode,
      @EndUserText.label: 'SAP Invoice Number' //
  key b_item.BillingDocument,
      @EndUserText.label: 'Billing Item' //
  key b_item.BillingDocumentItem,
      @EndUserText.label: 'Cancelled Invoice'
      bl.BillingDocumentIsCancelled,
      @EndUserText.label: 'Billing Type' //
      bl.BillingDocumentType,
      @EndUserText.label: 'Currency' //
      bl.TransactionCurrency,
      
      @EndUserText.label: 'Vechicle No for Non Valuated Mat'
      bl.YY1_VehicleNumber_BDH,
      
      @EndUserText.label: 'Transporter Name'
      bl.YY1_TRANSPORTERNAME_BDH,
      
      @EndUserText.label: 'GR Number'
      bl.YY1_GRNUMBER_BDH,

      bltxt.BillingDocumentTypeName,     
       
      @Semantics.currencyCode: true
      cast(case bl.TransactionCurrency 
      when 'INR' then 'INR' //then cast( 'INR' as abap.cuky( 5 )
      else 'INR'
      end as abap.cuky( 5 ) ) as TransCurr,

      @EndUserText.label: 'Exchange Rate'
      bl.AccountingExchangeRate,

      @EndUserText.label: 'Purchase Order'
      md.PurchaseOrder,

      bl.IncotermsLocation1,
      bl.IncotermsLocation2,
      bl.CustomerPriceGroup,

      // custmat.MaterialByCustomer ,
      custmat.MaterialDescriptionByCustomer,

      //case b_item.SalesSDDocumentCategory
      // when 'C' .

      @EndUserText.label: 'Customer Material Code'
      case bl.BillingDocumentType
      when 'G2' then cr_mri.MaterialByCustomer
      when 'L2' then dm_mri.MaterialByCustomer
      when 'CBRE' then cust_ri.MaterialByCustomer
       when 'F8' then
         case b_item.SalesSDDocumentCategory
         when 'C' then  so_item.MaterialByCustomer
         else s_sch_ag.MaterialByCustomer end
      when 'F2' then
         case b_item.SalesSDDocumentCategory
         when 'C' then  so_item.MaterialByCustomer
         else s_sch_ag.MaterialByCustomer end
      when 'F5' then   so_item.MaterialByCustomer
      // s_sch_ag.MaterialByCustomer  // C
      else '' end                                            as MaterialByCustomer,

      payterm.CustomerPaymentTermsName,
      // item_det2.GrossWeight ,
      item_det2.WeightUnit,
      // item_det2.BaseUnit ,
      // item_det2.NetWeight ,

      b_item.ItemGrossWeight                                as GrossWeight,
//    cast(  ( b_item.ItemNetWeight / 100  )   as abap.fltp  )       as NetWeight,
      b_item.NetAmount    as NetAmount,
      
     
      
       b_item.ItemNetWeight     as NetWeight,
       
   cast( cast( b_item.ItemNetWeight as abap.fltp ) / cast( 100 as abap.fltp ) as abap.fltp ) as NetWeight1,

      item_det2.SizeOrDimensionText,
      item_det2.ProductOldID,

      @EndUserText.label: 'Po_Number'
      so_head.PurchaseOrderByCustomer,

      @EndUserText.label: 'SalesDocumentType'
      so_head.SalesDocumentType,
      
      @EndUserText.label: 'Po_date'
      so_head.CustomerPurchaseOrderDate,

      @EndUserText.label: 'SO_date'
      so_head.SalesDocumentDate,
      
      @EndUserText.label: 'Vehicle Number'
      del_data.YY1_VehicleNumber_DLH,
      
      @EndUserText.label: 'Supervisor Name'
      del_data.YY1_SupervisorName_DLH,
      
       @EndUserText.label: 'Labour Contractor'
       del_data.YY1_LabourContractor_DLH,
      
       @EndUserText.label: 'Packing By'
       del_data.YY1_PackingBy_DLH,
      
       @EndUserText.label: 'Stitching By'
       del_data.YY1_StitchingBy_DLH,

      @EndUserText.label: 'SO Creation Date' //
      so_head.CreationDate,

      b_item._ReferenceDeliveryDocumentItem.DeliveryDocument,
      b_item._ReferenceDeliveryDocumentItem.DeliveryDocumentItem,
      
      @EndUserText.label: 'Tax Invoice Date' //
      bl.BillingDocumentDate,
      @EndUserText.label: 'Tax Invoice Creation Date' //
      bl.CreationDate                                        as billcreationdate,
      bl.SDDocumentCategory,
      bl.BillingDocumentCategory,
      //      bl.BillingDocumentType,
      bl.SalesOrganization,
      bl.DistributionChannel,
      distbtxt.DistributionChannelName,
      bl.Division,
      bl.SoldToParty,
      bl.IncotermsClassification,
      bl.CustomerPaymentTerms,

      @EndUserText.label: 'Tax Invoice Number' //
      bl.DocumentReferenceID,
      
      
//      @EndUserText.label: 'Export Invoice Number'
//      bl.YY1_ExportInvoiceNum_BDH,

      case bl.BillingDocumentType
        when 'F2' then 'INV'
        when 'G2' then 'CRN'
        when 'L2' then 'DBN'
        when 'F8' then 'INV'
        when 'CBRE' then 'CRN'
        else ' ' end                                         as DOC_TYPE,

      @EndUserText.label: 'IRN' //
      EINV.irn,
      @EndUserText.label: 'Ewaybill Number' //
      EINV.ewbno,

      @EndUserText.label: 'Ack Number' //
      EINV.ackno,
      @EndUserText.label: 'Ack Date' //
      EINV.ackdt,

      @EndUserText.label: 'Accounting Document Number' //
      bl.AccountingDocument,

      PLANT.PlantName,
      PLANT.AddressID,

      plant_add.AddresseeFullName,
      plant_add.CityName,
      @EndUserText.label: 'PLANT_POSTAL'
      plant_add.PostalCode,
      @EndUserText.label: 'PLANT_STREET'
      plant_add.Street,
      @EndUserText.label: 'PLANT_STREET1'
      plant_add.StreetName,
      @EndUserText.label: 'PLANT_HOUSENO'
      plant_add.HouseNumber,
      @EndUserText.label: 'PLANT_COUNTRY'
      plant_add.Country,
      @EndUserText.label: 'PLANT_REGION'
      plant_add.Region,
      @EndUserText.label: 'PLANT_FROMOFADDRESS'
      plant_add.FormOfAddress,
      @EndUserText.label: 'PLANT_PHONE'
      plant_add._PhoneNumber.PhoneAreaCodeSubscriberNumber,

      @EndUserText.label: 'PLANT_GSTIN_NEW'
      case b_item.Plant
         when '1001' then '03AADCB4295Q1ZA'
         when '2001' then '03AADCB6737H1ZU'
         when '3001' then '24AAFCH2454K1ZL'
         when '4001' then '23AAJCB8215E1ZZ'         
         else '03AADCB4295Q1ZA' end                          as PLANT_GSTIN,

      @EndUserText.label: 'PLANT_EMAIL_NEW'
      case b_item.Plant
      when '1001' then 'rice@blvexports.com'
      else  'rice@blvexports.com' end                       as PLANT_EMAIL,

      ITEM_DETAIL.ConsumptionTaxCtrlCode                     as hsn,
      b_item.Product                                         as HSN_CODE,
      b_item.BillingDocumentItemText,

      @Semantics.quantity.unitOfMeasure: ''
      @EndUserText.label: 'Sale Quantity'
      b_item.BillingQuantity,

      b_item.BillingQuantityUnit,
      b_item.SalesDocument,
      b_item.SalesDocumentItem,
      b_item.ItemGrossWeight,
      b_item.StorageLocation,
      b_item.Plant,
      b_item.BaseUnit,
      b_item.TaxCode,
      b_item.SalesGroup,
      b_item.ShippingPoint,
      b_item.Product                                         as product,

      @EndUserText.label: 'Ship to Party'
      add_id.ship_to_party,
      @EndUserText.label: 'Ship to NAME'
      add_id.WE_NAME,
      @EndUserText.label: 'Ship to CITY'
      add_id.WE_CITY,
      @EndUserText.label: 'Ship to PIN'
      add_id.WE_PIN,
      @EndUserText.label: 'Ship to STREET'
      add_id.WE_STREET,
      @EndUserText.label: 'Ship to STREET1'
      add_id.WE_STREET1,
      @EndUserText.label: 'Ship to HOUSE_NO'
      add_id.WE_HOUSE_NO,
      @EndUserText.label: 'Ship to COUNTRY'
      add_id.WE_COUNTRY,
      @EndUserText.label: 'Ship to REGION'
      add_id.WE_REGION,
      @EndUserText.label: 'Ship to FROM_OF_ADD'
      add_id.WE_FROM_OF_ADD,
      @EndUserText.label: 'Ship to GSTIN'
      add_id.WE_TAX,
      @EndUserText.label: 'Ship to PAN'
      add_id.WE_PAN,
      @EndUserText.label: 'Ship to EMAIL'
      add_id.WE_EMAIL,
      @EndUserText.label: 'Ship to PHONE'
      add_id.WE_PHONE4,
      @EndUserText.label: 'Ship to STREET2'
      add_id.WE_StreetPrefixName1,
      @EndUserText.label: 'Ship to STREET3'
      add_id.WE_StreetPrefixName2,
      @EndUserText.label: 'Ship to STREET4'
      add_id.WE_StreetSuffixName1,

      @EndUserText.label: 'Sold to PARTY'
      add_id.sold_to_party,
      @EndUserText.label: 'Sold to NAME'
      add_id.AG_NAME,
      @EndUserText.label: 'Sold to CITY'
      add_id.AG_CITY,
      @EndUserText.label: 'Sold to PIN'
      add_id.AG_PIN,
      @EndUserText.label: 'Sold to STREET'
      add_id.AG_STREET,
      @EndUserText.label: 'Sold to STREET1'
      add_id.AG_STREET1,
      @EndUserText.label: 'Sold to HOUSE_NO'
      add_id.AG_HOUSE_NO,
      @EndUserText.label: 'Sold to COUNTRY'
      add_id.AG_COUNTRY,
      @EndUserText.label: 'Sold to REGION'
      add_id.AG_REGION,
      @EndUserText.label: 'Sold to FROM_OF_ADD'
      add_id.AG_FROM_OF_ADD,
      @EndUserText.label: 'Sold to GSTIN'
      add_id.AG_TAX,
      @EndUserText.label: 'Sold to PAN'
      add_id.AG_PAN,
      @EndUserText.label: 'Sold to EMAIL'
      add_id.AG_EMAIL,
      @EndUserText.label: 'Sold to PHONE'
      add_id.AG_PHONE4,
      @EndUserText.label: 'Sold to STREET2'
      add_id.AG_StreetPrefixName1,
      @EndUserText.label: 'Sold to STREET3'
      add_id.AG_StreetPrefixName2,
      @EndUserText.label: 'Sold to STREET4'
      add_id.AG_StreetSuffixName1,

      @EndUserText.label: 'Payer Code'
      add_id.payer_code,
      @EndUserText.label: 'Payer NAME'
      add_id.RG_NAME,
      @EndUserText.label: 'Payer CITY'
      add_id.RG_CITY,
      @EndUserText.label: 'Payer PIN'
      add_id.RG_PIN,
      @EndUserText.label: 'Payer STREET'
      add_id.RG_STREET,
      @EndUserText.label: 'Payer STREET1'
      add_id.RG_STREET1,
      @EndUserText.label: 'Payer HOUSE_NO'
      add_id.RG_HOUSE_NO,
      @EndUserText.label: 'Payer COUNTRY'
      add_id.RG_COUNTRY,
      @EndUserText.label: 'Payer REGION'
      add_id.RG_REGION,
      @EndUserText.label: 'Payer FROM_OF_ADD'
      add_id.RG_FROM_OF_ADD,
      @EndUserText.label: 'Payer GSTIN'
      add_id.RG_TAX,
      @EndUserText.label: 'Payer PAN'
      add_id.RG_PAN,
      @EndUserText.label: 'Payer EMAIL'
      add_id.RG_EMAIL,
      @EndUserText.label: 'Payer PHONE'
      add_id.RG_PHONE4,
      @EndUserText.label: 'Payer STREET2'
      add_id.RG_StreetPrefixName1,
      @EndUserText.label: 'Payer STREET3'
      add_id.RG_StreetPrefixName2,
      @EndUserText.label: 'Payer STREET4'
      add_id.RG_StreetSuffixName1,

      @EndUserText.label: 'Bill to Code'
      add_id.bill_to_party,
      @EndUserText.label: 'Bill to NAME'
      add_id.RE_NAME,
      @EndUserText.label: 'Bill to CITY'
      add_id.RE_CITY,
      @EndUserText.label: 'Bill to PIN'
      add_id.RE_PIN,
      @EndUserText.label: 'Bill to STREET'
      add_id.RE_STREET,
      @EndUserText.label: 'Bill to STREET1'
      add_id.RE_STREET1,
      @EndUserText.label: 'Bill to HOUSE_NO'
      add_id.RE_HOUSE_NO,
      @EndUserText.label: 'Bill to COUNTRY'
      add_id.RE_COUNTRY,
      @EndUserText.label: 'Bill to REGION'
      add_id.RE_REGION,
      @EndUserText.label: 'Bill to FROM_OF_ADD'
      add_id.RE_FROM_OF_ADD,
      @EndUserText.label: 'Bill to GSTIN'
      add_id.RE_TAX,
      @EndUserText.label: 'Bill to PAN'
      add_id.RE_PAN,
      @EndUserText.label: 'Bill to EMAIL'
      add_id.RE_EMAIL,
      @EndUserText.label: 'Bill to PHONE'
      add_id.RE_PHONE4,
      @EndUserText.label: 'Bill to STREET2'
      add_id.RE_StreetPrefixName1,
      @EndUserText.label: 'Bill to STREET3'
      add_id.RE_StreetPrefixName2,
      @EndUserText.label: 'Bill to STREET4'
      add_id.RE_StreetSuffixName1,


      /// NOTIFY PARTY 1 .
      @EndUserText.label: 'SP_CODE'
      add_id.SP_code,
      @EndUserText.label: 'SP_NAME'
      add_id.SP_NAME,
      
//      @EndUserText.label: 'Supplier Name'
       _saleitem.FullName,
      @EndUserText.label: 'SP_CITY'
      add_id.SP_CITY,
      @EndUserText.label: 'SP_PIN'
      add_id.SP_PIN,
      @EndUserText.label: 'SP_STREET'
      add_id.SP_STREET,
      @EndUserText.label: 'SP_STREET1'
      add_id.SP_STREET1,
      @EndUserText.label: 'SP_HOUSE_NO'
      add_id.SP_HOUSE_NO,
      @EndUserText.label: 'SP_COUNTRY'
      add_id.SP_COUNTRY,
      @EndUserText.label: 'SP_REGION'
      add_id.SP_REGION,
      @EndUserText.label: 'SP_FROM_OF_ADD'
      add_id.SP_FROM_OF_ADD,
      @EndUserText.label: 'SP_GSTIN'
      add_id.SP_TAX,
      @EndUserText.label: 'SP_PAN'
      add_id.SP_PAN,
      @EndUserText.label: 'SP_EMAIL'
      add_id.SP_EMAIL,
      @EndUserText.label: 'SP_PHONE'
      add_id.SP_PHONE4,
      @EndUserText.label: 'SP_STREET2'
      add_id.SP_StreetPrefixName1,
      @EndUserText.label: 'SP_STREET3'
      add_id.SP_StreetPrefixName2,
      @EndUserText.label: 'SP_STREET4'
      add_id.SP_StreetSuffixName1,
      ///

      /// NOTIFY PARTY 2 .
      @EndUserText.label: 'ES_CODE'
      add_id.ES_code,
      @EndUserText.label: 'ES_NAME'
      add_id.ES_NAME,
      @EndUserText.label: 'ES_CITY'
      add_id.ES_CITY,
      @EndUserText.label: 'ES_PIN'
      add_id.ES_PIN,
      @EndUserText.label: 'ES_STREET'
      add_id.ES_STREET,
      @EndUserText.label: 'ES_STREET1'
      add_id.ES_STREET1,
      @EndUserText.label: 'ES_HOUSE_NO'
      add_id.ES_HOUSE_NO,
      @EndUserText.label: 'ES_COUNTRY'
      add_id.ES_COUNTRY,
      @EndUserText.label: 'ES_REGION'
      add_id.ES_REGION,
      @EndUserText.label: 'ES_FROM_OF_ADD'
      add_id.ES_FROM_OF_ADD,
      @EndUserText.label: 'ES_GSTIN'
      add_id.ES_TAX,
      @EndUserText.label: 'ES_PAN'
      add_id.ES_PAN,
      @EndUserText.label: 'ES_EMAIL'
      add_id.ES_EMAIL,
      @EndUserText.label: 'ES_PHONE'
      add_id.ES_PHONE4,
      @EndUserText.label: 'ES_STREET2'
      add_id.ES_StreetPrefixName1,
      @EndUserText.label: 'ES_STREET3'
      add_id.ES_StreetPrefixName2,
      @EndUserText.label: 'ES_STREET4'
      add_id.ES_StreetSuffixName1,
      ///

      @EndUserText.label: 'Condition Quantity'
      PRCD.ConditionQuantity,

      @EndUserText.label: 'PART_ASSESSABLEAMOUNT'
      @Aggregation.default: #SUM
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_ASSESSABLEAMOUNT,


      @EndUserText.label: 'PART_ASSESSABLEAMOUNT_INR'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      cast( ( PRCD.ITEM_ASSESSABLEAMOUNT * bl.AccountingExchangeRate ) as abap.dec( 12, 2 )) as ITEM_ASSESSABLEAMOUNT_INR,
      
      @EndUserText.label: 'PACKING_CHARG'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_PKG_CHRG,
      
      @EndUserText.label: 'Sale Unit Price' //
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      cast( ( case bl.DistributionChannel
      when '00' then division( PRCD.ITEM_UNITPRICE  ,  b_item.BillingQuantity , 2 )
      else PRCD.ITEM_UNITPRICE end   ) as abap.dec( 12, 2 ) )        as ITEM_UNITPRICE,
      
      // @EndUserText.label: 'ITEM_UNITPRICE_INR'
      //      case bl.DistributionChannel
      //      when '20' then division( PRCD.ITEM_UNITPRICE  ,  b_item.BillingQuantity , 2 )
      //      else PRCD.ITEM_UNITPRICE end as ITEM_UNITPRICE_INR ,

      @EndUserText.label: 'PART_DISCOUNTAMOUNT'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_DISCOUNTAMOUNT,

      @EndUserText.label: 'PART_DISCOUNTAMOUNT_INR'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      cast( ( PRCD.ITEM_DISCOUNTAMOUNT * bl.AccountingExchangeRate ) as abap.dec( 12, 2 ) )  as ITEM_DISCOUNTAMOUNT_INR,

      @EndUserText.label: 'PART_FRIEGHT'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_FREIGHT,
      
      @EndUserText.label: 'PART_AMOTIZATION'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_AMOTIZATION,

      @EndUserText.label: 'PART_OTHERCHARGE'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_OTHERCHARGE,

      @EndUserText.label: 'PART_INSURANCE_AMT'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_INSURANCE_AMT,
            
      @EndUserText.label: 'PART_SGSTRATE'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_SGSTRATE,
      
      @EndUserText.label: 'PART_CGSTRATE'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_CGSTRATE,
      
      @EndUserText.label: 'PART_IGSTRATE'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_IGSTRATE,
      
      @EndUserText.label: 'PART_SGSTAMOUNT'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_SGSTAMOUNT,
      
      @EndUserText.label: 'PART_CGSTAMOUNT'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_CGSTAMOUNT,
      
      @EndUserText.label: 'PART_IGSTAMOUNT'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_IGSTAMOUNT,
      
      @EndUserText.label: 'Sub Total Amount'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      
      case when _condtyp.ConditionType = 'JOSG' or _condtyp.ConditionType = 'JOCG' then 
      
     cast( ( PRCD.ITEM_SGSTAMOUNT + PRCD.ITEM_CGSTAMOUNT ) as abap.dec( 12, 2 ) )
     
      when  _condtyp.ConditionType = 'JOIG' then
     
        PRCD.ITEM_IGSTAMOUNT end  as Subtotal_Amount,
   
      
      @EndUserText.label: 'PART_IGSTAMOUNT_INR'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      cast( ( PRCD.ITEM_IGSTAMOUNT * bl.AccountingExchangeRate ) as abap.dec( 12, 2 ) ) as ITEM_IGSTAMOUNT_INR,

      @EndUserText.label: 'PART_CGSTAMOUNT_INR'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      cast( ( PRCD.ITEM_CGSTAMOUNT * bl.AccountingExchangeRate ) as abap.dec( 12, 2 ) ) as ITEM_CGSTAMOUNT_INR,

      @EndUserText.label: 'PART_SGSTAMOUNT_INR'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      cast( ( PRCD.ITEM_SGSTAMOUNT * bl.AccountingExchangeRate ) as abap.dec( 12, 2 ) ) as ITEM_SGSTAMOUNT_INR,

      @EndUserText.label: 'PART_TOTALAMOUNT'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      case bl.DistributionChannel
      when '20' then PRCD.ITEM_ZBAS_UNIT 
      else
      PRCD.ITEM_TOTALAMOUNT end                              as ITEM_TOTALAMOUNT,

      @EndUserText.label: 'PART_TOTALAMOUNT_INR'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      cast((case bl.DistributionChannel
      when '20' then PRCD.ITEM_ZBAS_UNIT * bl.AccountingExchangeRate 
      else
      PRCD.ITEM_TOTALAMOUNT * bl.AccountingExchangeRate end ) as abap.dec( 12, 2 ) ) as ITEM_TOTALAMOUNT_INR,
      
      @EndUserText.label: 'PART_GRANDTOTALAMOUNT'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_GRANDTOTALAMOUNT                             as ITEM_GRANDTOTALAMOUNT,
      
      @EndUserText.label: 'PART_GRANDTOTALAMOUNT_INR'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      cast( ( PRCD.ITEM_GRANDTOTALAMOUNT * bl.AccountingExchangeRate  ) as abap.dec( 12, 2 ) ) as ITEM_GRANDTOTALAMOUNT_INR,
      
      @EndUserText.label: 'PART_LESS_AMMORTIZATION'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_LESS_AMMORTIZATION,
      
      @EndUserText.label: 'PART_PCIP_AMT'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_PCIP_AMT,
      
      @EndUserText.label: 'PART_ROUNDOFF'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_ROUNDOFF,
      
      @EndUserText.label: 'PART_ZMRP_AMOUNT'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_ZMRP_AMOUNT,

      @EndUserText.label: 'PART_ZBAS_UNIT'
      PRCD.ITEM_ZBAS_UNIT,

      @EndUserText.label: 'PART_FERT_OTH'
      @Semantics.amount.currencyCode: 'TransactionCurrency'
      PRCD.ITEM_FERT_OTH,

      b_item.ProfitCenter,

      case bl.BillingDocumentType
        when 'F2' then
                      case add_id.WE_TAX when '' then 'B2C'
                        when 'URP' then  case  when PRCD.ITEM_IGSTAMOUNT is initial then 'EXPWOP'
                                       when  PRCD.ITEM_IGSTAMOUNT is not initial then 'EXPWP' end
                                       else 'B2B' end
        when 'G2' then 'B2B'
        when 'L2' then 'B2B'
        when 'F8' then 'B2B'
        when 'JSTO' then 'B2B'
        
        when 'CBRE' 
        then case bl.DistributionChannel 
        when '30' 
        then case when PRCD.ITEM_IGSTAMOUNT  is initial then 'EXPWOP'
                  when  PRCD.ITEM_IGSTAMOUNT is not initial then 'EXPWP'
                  end
        when '40' 
        then case when PRCD.ITEM_IGSTAMOUNT  is initial then 'EXPWOP'
                  when  PRCD.ITEM_IGSTAMOUNT is not initial then 'EXPWP'
                  end 
                  else 'B2B' end               
                                                                 
        else 'B2C' end                                       as SUPPLY_TYPE

}
where
  b_item.BillingQuantity > 0 
  
