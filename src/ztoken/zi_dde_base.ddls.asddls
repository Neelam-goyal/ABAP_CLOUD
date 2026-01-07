@AbapCatalog.sqlViewName: 'ZV_DDE_BASE'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Daily dispatch ethanol base'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_DDE_BASE

  as select distinct from I_BillingDocumentItem as b_item

    left outer join I_BillingDocument     as bl      on bl.BillingDocument = b_item.BillingDocument

    left outer join I_SalesDocument       as so_head on so_head.SalesDocument = b_item.SalesDocument

left outer join ZI_CUSTOMER_ADDRESS   as cust    on cust.Customer = bl.SoldToParty    
{

  key b_item.Plant,
  key bl.SoldToParty,
  key so_head.PurchaseOrderByCustomer,
      so_head.CustomerPurchaseOrderDate,
      bl.BillingDocumentDate,
      cust.Customer,
      cust.CustomerName,
      cust.CityName,
      substring(bl.BillingDocumentDate,7,2) as daycount
}
