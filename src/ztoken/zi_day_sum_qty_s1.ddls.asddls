@AbapCatalog.sqlViewName: 'ZDAY_SUM_QTY_S1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DDE day sum qty'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_DAY_SUM_QTY_S1 
  as select distinct from I_BillingDocumentItem as b_item

    left outer join       I_BillingDocument     as bl      on bl.BillingDocument = b_item.BillingDocument

    left outer join       I_SalesDocument       as so_head on so_head.SalesDocument = b_item.SalesDocument

    left outer join       ZI_CUSTOMER_ADDRESS   as cust    on cust.Customer = bl.SoldToParty

{

  key b_item.Plant,
  key bl.SoldToParty,
  key so_head.PurchaseOrderByCustomer,
      bl.BillingDocumentDate,
      bl.BillingDocumentType,
      
      case bl.BillingDocumentType
      when 'S1'
      then sum( b_item.BillingQuantity )
      else 0 end as totcancelledqty
      
}
where bl.BillingDocumentType = 'S1'
group by
  b_item.Plant,
  bl.SoldToParty,
  so_head.PurchaseOrderByCustomer,
  bl.BillingDocumentType,
  bl.BillingDocumentDate
