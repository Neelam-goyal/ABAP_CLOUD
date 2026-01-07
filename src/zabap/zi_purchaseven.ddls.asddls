@AbapCatalog.sqlViewName: 'ZI_PURCHASEVV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase Vendor Data'
define view ZI_PURCHASEVEN
  as select from    I_OperationalAcctgDocItem as a
    left outer join I_Supplier                as b on a.Supplier = b.Supplier
    left outer join I_JournalEntry            as c on  a.CompanyCode        = c.CompanyCode
                                                   and a.AccountingDocument = c.AccountingDocument
                                                   and a.FiscalYear         = c.FiscalYear
    left outer join I_CountryText             as d on  b.Country  = d.Country
                                                   and d.Language = 'E'
{
  key  a.CompanyCode,
  key  a.AccountingDocument,
  key  a.FiscalYear,
       a.InvoiceReference,
       a.InvoiceReferenceFiscalYear,
       a.InvoiceItemReference,
       b.Supplier,
       b.SupplierName,
       b.TaxNumber3,
       b.Region,
       b.SupplierAccountGroup,
       b.Country,
       d.CountryName,
       b.IN_GSTSupplierClassification,
       cast(c.AbsoluteExchangeRate as abap.curr(13,2)) as AbsoluteExchangeRate,
       @EndUserText.label: 'Miro Number'
       substring(a.OriginalReferenceDocument,1,10)     as MiroNumber,
       @EndUserText.label: 'Miro Year'
       substring(a.OriginalReferenceDocument,11,4 )    as MiroYear,
       a.AssignmentReference
}
where
  a.FinancialAccountType = 'K'
