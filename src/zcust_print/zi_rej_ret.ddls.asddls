@AbapCatalog.sqlViewName: 'ZV_REJ_RET'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Rejection Returned'
define view ZI_REJ_RET as select from ZI_GRN_DETAIL as vend
{
 
 key vend.MaterialDocument, 
 key vend.MaterialDocumentYear,
 key vend.PurchaseOrder,
 key vend.PurchaseOrderItem,
 key vend.Supplier,
 key vend.ReferenceDocument,
 key vend.Batch,
 sum(  vend.QuantityInEntryUnit ) as ret_qty
    
} where vend.GoodsMovementType = '122'

group by
vend.MaterialDocument, 
vend.MaterialDocumentYear,
vend.PurchaseOrder,
vend.PurchaseOrderItem,
vend.Supplier,
vend.ReferenceDocument,
vend.Batch
