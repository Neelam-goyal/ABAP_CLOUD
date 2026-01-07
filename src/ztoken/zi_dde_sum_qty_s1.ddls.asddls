@AbapCatalog.sqlViewName: 'ZV_DDE_SUM_S1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'DDE day sum qty s1'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_DDE_SUM_QTY_S1 

  as select distinct from I_BillingDocumentItem as b_item

    left outer join       I_BillingDocument     as bl      on bl.BillingDocument = b_item.BillingDocument

    left outer join       I_SalesDocument       as so_head on so_head.SalesDocument = b_item.SalesDocument

    left outer join       ZI_CUSTOMER_ADDRESS   as cust    on cust.Customer = bl.SoldToParty

    left outer join       I_SalesOrderItem      as so_item on  b_item.SalesDocument     = so_item.SalesOrder
                                                           and b_item.SalesDocumentItem = so_item.SalesOrderItem
{

  key b_item.Plant,
  key bl.SoldToParty,
  key so_head.PurchaseOrderByCustomer,

      case bl.BillingDocumentType
      when 'S1'
      then sum( b_item.BillingQuantity )
      else 0 end as totcancelledqty,
      
      sum( so_item.OrderQuantity ) as soqtys1

}where bl.BillingDocumentType = 'S1'
group by
  b_item.Plant,
  bl.SoldToParty,
  so_head.PurchaseOrderByCustomer,
  bl.BillingDocumentType
