@AbapCatalog.sqlViewName: 'ZV_PRINT_QUOT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Quotation print'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_PRINT_QUOT 
as select from I_SalesDocument as so
{
 
 key so.SalesDocument,
 so.SalesDocumentType,
 so.CreationDate,
 so.SalesOrganization,
 so.DistributionChannel,
 so.OrganizationDivision,
 so.SalesGroup,
 so.SalesOffice,
 so.SDDocumentCategory
    
}
where so.SDDocumentCategory = 'B'
 