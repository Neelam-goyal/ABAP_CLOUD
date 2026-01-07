@AbapCatalog.sqlViewName: 'ZV_PURDOC_F4'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase doc F4'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_PURDOC_F4
  as select from ZI_PO_DATA as po
{

  key po.PurchaseOrder,
  key po.PurchaseOrderItem,
      po.PurchaseOrderDate,
      po.Material

}

union select from ZI_Schedgagrmt_PO as sapo
{

  key sapo.SchedulingAgreement      as PurchaseOrder,
  key sapo.SchedulingAgreementItem  as PurchaseOrderItem,
      sapo.ScheduleLineDeliveryDate as PurchaseOrderDate,
      sapo.Material

}
