@AbapCatalog.sqlViewName: 'ZV_SCN_PRINT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'SCN Print'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_SCN_PRINT 
as select distinct from I_OperationalAcctgDocItem as bkpf
{
  
  key bkpf.CompanyCode,
  key bkpf.AccountingDocument,
  key bkpf.FiscalYear,
      bkpf.PostingDate,
      bkpf.DocumentDate,
      bkpf.AccountingDocumentType
    
}
