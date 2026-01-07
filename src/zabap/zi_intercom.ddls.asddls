@AbapCatalog.sqlViewName: 'ZI_INTERCOMS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'IncotermsClassification'
@Metadata.ignorePropagatedAnnotations: true
define view zi_intercom as select from I_PurchaseOrderAPI01
as a left outer join I_IncotermsClassificationText  as b on a.IncotermsClassification = b.IncotermsClassification
{ key a.PurchaseOrder,
b.IncotermsClassificationName
    
}
