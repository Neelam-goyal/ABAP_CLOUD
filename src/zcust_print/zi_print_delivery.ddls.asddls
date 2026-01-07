@AbapCatalog.sqlViewName: 'ZV_PRINT_DLVRY'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Delivery print data'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_PRINT_DELIVERY
  as select distinct from I_DeliveryDocument dlvry
    left outer join       I_DeliveryDocumentItem as dlvit on dlvit.DeliveryDocument = dlvry.DeliveryDocument
{

  key dlvry.DeliveryDocument,
      dlvry.DeliveryDate,
      dlvry.DeliveryDocumentType,
      dlvit.Plant

}
