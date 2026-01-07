@AbapCatalog.sqlViewName: 'ZI_PARTNER1'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'BROKER'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_PARTNER as select from I_PurchaseOrderPartnerAPI01
as a
left outer join I_Supplier                as b on a.Supplier = b.Supplier

{key a.PurchaseOrder,
    b.SupplierName as broker
   
}
