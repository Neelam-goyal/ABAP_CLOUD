@AbapCatalog.sqlViewName: 'ZV_BDOC_F4'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Billing Document F4'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_BDOC_F4 as select from I_BillingDocument as bl
{
    key bl.BillingDocument,
    bl.BillingDocumentType,
    bl.BillingDocumentDate,
    bl.DistributionChannel

}
