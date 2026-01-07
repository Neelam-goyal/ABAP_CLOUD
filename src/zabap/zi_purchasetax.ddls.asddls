@AbapCatalog.sqlViewName: 'ZI_PURCHASETAXV'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Purchase tax data'
define view ZI_PURCHASETAX as select from I_OperationalAcctgDocItem 
{
key  CompanyCode,
key  AccountingDocument,    
key  FiscalYear,
key  TaxItemGroup,
@Semantics.currencyCode: true
CompanyCodeCurrency,
@Semantics.amount.currencyCode: 'CompanyCodeCurrency'
sum( case TransactionTypeDetermination when 'JIC' then AmountInCompanyCodeCurrency end ) as jocg,
@Semantics.amount.currencyCode: 'CompanyCodeCurrency'
sum( case TransactionTypeDetermination when 'JRC' then AmountInCompanyCodeCurrency * -1 end ) as Rjocg,
@Semantics.amount.currencyCode: 'CompanyCodeCurrency'
sum( case TransactionTypeDetermination when 'JIS' then AmountInCompanyCodeCurrency end ) as josg,
@Semantics.amount.currencyCode: 'CompanyCodeCurrency'
sum( case TransactionTypeDetermination when 'JRS' then AmountInCompanyCodeCurrency * -1 end ) as Rjosg,
@Semantics.amount.currencyCode: 'CompanyCodeCurrency'
sum( case TransactionTypeDetermination when 'JII' then AmountInCompanyCodeCurrency
when 'JIM' 
 then AmountInCompanyCodeCurrency
when '' then AmountInCompanyCodeCurrency end ) as joig,
@Semantics.amount.currencyCode: 'CompanyCodeCurrency'
sum( case TransactionTypeDetermination when 'JRI' then AmountInCompanyCodeCurrency * -1 end ) as Rjoig
}where AccountingDocumentItemType = 'T'
group by CompanyCode, AccountingDocument, FiscalYear, TaxItemGroup,CompanyCodeCurrency
