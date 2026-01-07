@AbapCatalog.sqlViewName: 'ZI_SCDDV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Schedule vs Dispatch1'

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

define view ZI_SCD_DP_DATA as select from I_SalesDocumentItem as a
left outer join ZI_SBILLING_DATA as b on a.SalesDocument = b.SalesDocument and a.SalesDocumentItem
= b.SalesDocumentItem
left outer join ZI_SBILLING_ADATA as F on a.SalesDocument = F.SalesDocument and a.SalesDocumentItem
= F.SalesDocumentItem
inner join I_SalesDocument as c on a.SalesDocument = c.SalesDocument 
inner join I_Customer_VH as d on c.SoldToParty = d.Customer
inner join I_ProductText as e on a.Product = e.Product and e.Language = 'E'
inner join I_Product as g on a.Product = g.Product 
left outer join C_SlsOrdSchdLnWthCalInfoDEX as H on  a.SalesDocument = H.SalesOrder and a.SalesDocumentItem
= H.SalesOrderItem

{

key a.SalesDocument,
key a.SalesDocumentItem,
c.SalesDocumentType,
c.SoldToParty,
c.PriceDetnExchangeRate,
c.DistributionChannel,
g.ProductOldID,
d.CustomerName,
a.CreationDate,
a.Product,
e.ProductName,
a.MaterialByCustomer,
a.MaterialGroup,
a.PurchaseOrderByCustomer,
a.CustomerPurchaseOrderDate,
@EndUserText.label: 'Commited Delivery Date'
a.CommittedDeliveryDate,
@EndUserText.label: 'Requested Delivery Date'
H.RequestedDeliveryDate,
@EndUserText.label: 'Rejection Reason'
a.SalesDocumentRjcnReason,
@Semantics.unitOfMeasure: true
a.OrderQuantityUnit,
@DefaultAggregation: #SUM
@Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
a.OrderQuantity,
@Semantics.amount.currencyCode: 'TransactionCurrency'
a.NetAmount,
@EndUserText.label: 'Net Amount in INR'
//@Semantics.amount.currencyCode: 'TransactionCurrency'
cast(a.NetAmount * c.PriceDetnExchangeRate as abap.curr(13,2))  as NetAmountInr, 
@Semantics.currencyCode: true
a.TransactionCurrency,
@Semantics.amount.currencyCode: 'TransactionCurrency'
a.NetPriceAmount,
@EndUserText.label: 'Net Price in INR'
//@Semantics.amount.currencyCode: 'TransactionCurrency'
cast(a.NetPriceAmount * c.PriceDetnExchangeRate as abap.curr(13,2)) as NetPriceInr, 
@Semantics.quantity.unitOfMeasure: 'NetPriceQuantityUnit'
a.NetPriceQuantity,
a.NetPriceQuantityUnit,
@Semantics.amount.currencyCode: 'TransactionCurrency'
a.TaxAmount,
a.ShippingPoint,
a.Plant,
@EndUserText.label: 'Billed Quantity'
@DefaultAggregation: #SUM
@Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
b.BillingQty,
case c.SalesDocumentType
when 'LZ'
then
a.TargetQuantityUnit 
else '' end as targetqtyunit,
case c.SalesDocumentType
when 'LZ'
then a.TargetQuantity 
else cast( 0 as abap.quan(13,3)) end as targetqty,
F.currency,
@EndUserText.label: 'Billed Amount'
@DefaultAggregation: #SUM
@Semantics.amount.currencyCode: 'currency'
F.BNetAmount,
@EndUserText.label: 'Billed Amount in INR'
//@Semantics.amount.currencyCode: 'currency'
cast(F.BNetAmount * c.PriceDetnExchangeRate as abap.curr(13,2)) as BilledAmtInr, 
@EndUserText.label: 'Pending Amount'
@DefaultAggregation: #SUM
@Semantics.amount.currencyCode: 'currency'
case when F.BNetAmount is not null then
( a.NetAmount -  F.BNetAmount ) 
else ( a.NetAmount ) end as PendingAmt,
@EndUserText.label: 'Pending Amount in INR'
cast(case when F.BNetAmount is not null then
( a.NetAmount -  F.BNetAmount ) * c.PriceDetnExchangeRate 
else ( a.NetAmount * c.PriceDetnExchangeRate ) end as abap.curr(13,2))  as PendingAmtinInr,
@EndUserText.label: 'Pending Quantity'
@DefaultAggregation: #SUM
@Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
case when b.BillingQty is not null then
( a.OrderQuantity -  b.BillingQty ) 
else ( a.OrderQuantity ) end as pendingQty
} where (c.SalesDocumentType= 'TA' or c.SalesDocumentType = 'LZ' or c.SalesDocumentType = 'OR')
