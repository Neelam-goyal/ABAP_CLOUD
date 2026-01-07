@AbapCatalog.sqlViewName: 'ZV_SUM_DELV'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sum Batch Qty'
define view ZI_SUM_DELV_BATCH_QTY 
as select from I_DeliveryDocumentItem as ditem
{

key ditem.DeliveryDocument,
key ditem.HigherLvlItmOfBatSpltItm,
ditem.Material,
sum(ditem.ActualDeliveryQuantity) as ActualDeliveryQuantity    
}
group by
ditem.DeliveryDocument,
ditem.HigherLvlItmOfBatSpltItm,
ditem.Material
