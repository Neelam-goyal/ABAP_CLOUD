@AbapCatalog.sqlViewName: 'ZI_CHQ_SLIP'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Chq slip'
@Metadata.ignorePropagatedAnnotations: true
define view ZI_CHQ_SLIP_PRINT 
as select distinct from I_OperationalAcctgDocItem as bkpf
{
  
  key bkpf.CompanyCode,
  key bkpf.AccountingDocument,
  key bkpf.FiscalYear,
      bkpf.PostingDate,
      bkpf.DocumentDate,
      bkpf.AccountingDocumentType
    
}
