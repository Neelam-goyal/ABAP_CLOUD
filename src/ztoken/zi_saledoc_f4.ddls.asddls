@AbapCatalog.sqlViewName: 'ZV_SALEDOC_F4'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales document F4 help'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_SALEDOC_F4
  as select from I_SalesDocument as sd
  left outer join ZI_CUSTOMER_ADDRESS as cadd
  on cadd.Customer  = sd.SoldToParty
{

  key sd.SalesDocument,
      sd.SDDocumentCategory,
      sd.SalesDocumentType,
      sd.SalesOrganization,
      sd.DistributionChannel,
      sd.OrganizationDivision,
      sd.SalesGroup,
      sd.SalesOffice,
      sd.SoldToParty,
      cadd.CustomerName,
      cadd.DistrictName,
      sd.CustomerGroup,
      sd.SalesDistrict,
      sd.CustomerPaymentTerms,
      sd.SalesDocApprovalStatus

}
