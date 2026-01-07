@AbapCatalog.sqlViewName: 'ZV_SALE_REGF2'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sale Register for F2 Data'
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

define root view ZI_SALE_REG_F2
  as select distinct from I_BillingDocumentItem      as b_item

  left outer join       I_BillingDocument          as bl          on bl.BillingDocument = b_item.BillingDocument
                                                          
{

      @EndUserText.label: 'Company Code' 
  key bl.CompanyCode,
      @EndUserText.label: 'SAP Invoice Number' 
  key b_item.BillingDocument,
      @EndUserText.label: 'Billing Item' 
  key b_item.BillingDocumentItem,
  
      @EndUserText.label: 'Tax Invoice Number' 
      bl.DocumentReferenceID,

      @EndUserText.label: 'Accounting Document Number' 
      bl.AccountingDocument,
      
      bl.BillingDocumentType,

      @EndUserText.label: 'Tax Invoice Date' 
      bl.BillingDocumentDate,

      @EndUserText.label: 'Currency' 
      bl.TransactionCurrency,

      @EndUserText.label: 'Exchange Rate'
      bl.AccountingExchangeRate,
      
      b_item._ReferenceDeliveryDocumentItem.DeliveryDocument,
      b_item._ReferenceDeliveryDocumentItem.DeliveryDocumentItem

}

 where
  b_item.BillingQuantity > 0 and
  bl.BillingDocumentType = 'F2'

