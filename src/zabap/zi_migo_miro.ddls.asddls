@AbapCatalog.sqlViewName: 'ZV_MIGO_MIRO'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'MIGO to Miro Data'
define view ZI_MIGO_MIRO
  as select from ZI_PURCHASE_REG as preg
{

  key preg.MigoNumber,
  key preg.MigoYear,
  key preg.MiroNumber,
  key preg.MiroYear,
      preg.PurchasingDocument,
      preg.PurchasingDocumentItem,
      preg.Product,
      preg.MiroQty,
      preg.TaxableValue,
      preg.MiroUnit,
      preg.PostingDate,
      preg.InvoiceValue

}
where preg.AccountingDocumentType = 'RE'
