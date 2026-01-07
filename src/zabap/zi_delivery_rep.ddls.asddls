@AbapCatalog.sqlViewName: 'ZV_DEL_REP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Delivery Report'

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


//i_sddocumentprocessflow  .....
define view ZI_DELIVERY_REP as select distinct from I_OutboundDeliveryItem as oditem
left outer join I_OutboundDelivery as od on oditem.OutboundDelivery = od.OutboundDelivery

left outer join I_BillingDocItemPartner as bp_item on bp_item.Customer = od.SoldToParty
left outer join I_Address_2 as add on add.AddressID  = bp_item.AddressID

// left outer join I_Address_2 as add on add.BusinessPartner = od.SoldToParty
//left outer join I_SDDocumentProcessFlow as pdoc on pdoc.PrecedingDocument = od.OutboundDelivery 
//left outer join I_BillingDocument as bill on bill.BillingDocument = pdoc.SubsequentDocument

left outer join I_BillingDocumentItem as billitem on billitem.ReferenceSDDocument = od.OutboundDelivery
left outer join I_BillingDocument as bill on bill.BillingDocument = billitem.BillingDocument
left outer join I_MaterialDocumentItem_2 as matdoc on matdoc.DeliveryDocument = od.OutboundDelivery
left outer join  ZI_PRCD_ELEMENTS  as PRCD on  PRCD.BillingDocument     = billitem.BillingDocument
                                      and PRCD.BillingDocumentItem = billitem.BillingDocumentItem
left outer join I_JournalEntry as JE on  JE.DocumentReferenceID     = bill.BillingDocument
left outer join I_OperationalAcctgDocItem as OEI on OEI.AccountingDocument = JE.AccountingDocument
                                                     and OEI.CompanyCode = JE.CompanyCode
                                                     and OEI.FiscalYear = JE.FiscalYear
                                                     and OEI.FinancialAccountType = 'S'                                        
{
    @EndUserText.label: 'PGI Number'
    key od.OutboundDelivery ,
    @EndUserText.label: 'PGI Date' 
    key od.ActualGoodsMovementDate ,
    @EndUserText.label: 'PGI Item' 
    key oditem.OutboundDeliveryItem ,
    @EndUserText.label: 'Customer Code' 
    od.SoldToParty ,
    @EndUserText.label: 'Customer Name'
    add.AddresseeFullName ,
    @EndUserText.label: 'Billing Doc' 
    bill.BillingDocument ,
    @EndUserText.label: 'Billing Doc Ref' 
    bill.DocumentReferenceID ,
    @EndUserText.label: 'Billing Doc Date' 
    bill.BillingDocumentDate ,
    @EndUserText.label: 'PGI Part Number' 
    oditem.Material ,
    @EndUserText.label: 'PGI Part Description'
    oditem.DeliveryDocumentItemText ,
    
    @EndUserText.label: 'Billing Qty'
    billitem.BillingQuantity ,

    @EndUserText.label: 'QTY Unit'
    oditem.DeliveryQuantityUnit , 
        @DefaultAggregation: #SUM
      @Semantics.quantity.unitOfMeasure: 'DeliveryQuantityUnit'
      @EndUserText.label: 'QTY as per PGI'
    oditem.ActualDeliveryQuantity ,
    
    oditem.StorageLocation ,
    
    @EndUserText.label: 'Currency'   
    bill.TransactionCurrency ,
    
    //@EndUserText.label: 'Currency Unit'
    //matdoc.CompanyCodeCurrency ,
  //  @EndUserText.label: 'COGS Amount'
  //  @Semantics.amount.currencyCode: 'TransactionCurrency'
  //  matdoc.GdsMvtExtAmtInCoCodeCrcy ,
     
     @EndUserText.label: 'COGS Amount Value'
    @Semantics.amount.currencyCode: 'TransactionCurrency'
    OEI.AmountInTransactionCurrency , 
    
    @EndUserText.label: 'Unit Price' 
    @Semantics.amount.currencyCode: ''
     case when billitem.BillingQuantity is not initial then case bill.DistributionChannel 
      when '20' then division( PRCD.ITEM_UNITPRICE  ,  billitem.BillingQuantity , 2 )    
      else PRCD.ITEM_UNITPRICE end end as ITEM_UNITPRICE ,
    
    @EndUserText.label: 'Exchange Rate' 
    bill.AccountingExchangeRate ,
    
    @EndUserText.label: 'Billing Amount'
    bill.TotalNetAmount ,
    @EndUserText.label: 'Billing Tax'
    bill.TotalTaxAmount ,   
    
    @EndUserText.label: 'Status'
    od.OverallSDProcessStatus
    
}










