@AbapCatalog.sqlViewName: 'ZV_PR_PRINT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'PR Print'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_PR_PRINT
  as select from I_PurchaseRequisitionAPI01 as pr
{

  key pr.PurchaseRequisition,
      pr.PurReqnDescription,
      pr.PurchaseRequisitionType,
      pr.LastChangeDateTime      
}
