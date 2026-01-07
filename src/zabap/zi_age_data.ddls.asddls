@AbapCatalog.sqlViewName: 'ZV_AGE_DATA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Ageing Data'
define view ZI_AGE_DATA

  with parameters
    @Environment.systemField: #SYSTEM_DATE
    P_KeyDate : sydate
  as select from ZI_VEND_AGE(P_KeyDate : $parameters.P_KeyDate) as vage
{

  key vage.CompanyCode,
  key vage.AccountingDocument,
  key vage.FiscalYear,
  vage.AssignmentReference,
  vage.DocumentItemText,
  vage.OperationalGLAccount,
  vage.GLAccount,
  vage.Supplier,
  vage.SupplierName,
  vage.ProfitCenter,
  vage.BusinessPlace,
  vage.PostingDate,
  vage.DocumentDate,
  vage.AccountingDocumentType,
  vage.PaymentTerms,
  vage.PaymentTermsDescription,
  vage.NetDueDate,
  vage.CompanyCodeCurrency,
  vage.AmountInCompanyCodeCurrency,
  vage.TransactionCurrency,
  vage.AmountInTransactionCurrency,
  vage.DocumentReferenceID,
  vage.PurchasingDocument,
  vage.nodays,
  
  @EndUserText.label: 'days_0_30'
  vage.days_1_30,
  
  @EndUserText.label: 'days_31_60'
  vage.days_31_60,
  
  @EndUserText.label: 'days_61_90'
  vage.days_61_90,
  
  @EndUserText.label: 'days_91_120'
  vage.days_91_120,
  
  @EndUserText.label: 'days_121_150'
  vage.days_121_150,
  
  @EndUserText.label: 'days_151_180'
  vage.days_151_180,
  
  @EndUserText.label: 'days_ge_180'
  vage.days_ge_180,
  
  @EndUserText.label: 'Not due amount'
  vage.not_due_amount,
 
case 
when ( vage.days_1_30  is not null and 
       vage.days_31_60 is not null and
       vage.days_61_90  is not null and
       vage.days_91_120 is not null and
       vage.days_121_150 is not null and
       vage.days_151_180 is not null and
       vage.days_ge_180  is not null and
       vage.not_due_amount is not null ) 
       then 
       vage.days_1_30 + vage.days_31_60 + vage.days_61_90 + vage.days_91_120 + vage.days_121_150 + vage.days_151_180 + vage.days_ge_180
       //vage.days_1_30 + vage.days_31_60 + vage.days_61_90 + vage.days_91_120 + vage.days_121_150 + vage.days_151_180 + vage.days_ge_180 + vage.not_due_amount
        
when ( vage.days_1_30  is not null and 
       vage.days_31_60   is null and
       vage.days_61_90   is null and
       vage.days_91_120  is null and
       vage.days_121_150 is null and
       vage.days_151_180 is null and
       vage.days_ge_180  is null and
       vage.not_due_amount is null ) 
       then 
       vage.days_1_30

//when ( vage.days_1_30  is not null and 
//       vage.days_31_60   is null and
//       vage.days_61_90   is null and
//       vage.days_91_120  is null and
//       vage.days_121_150 is null and
//       vage.days_151_180 is null and
//       vage.days_ge_180  is null and
//       vage.not_due_amount is not null ) 
//       then 
//       vage.days_1_30 + vage.not_due_amount
       
when ( vage.days_31_60   is not null and 
       vage.days_1_30    is null and
       vage.days_61_90   is null and
       vage.days_91_120  is null and
       vage.days_121_150 is null and
       vage.days_151_180 is null and
       vage.days_ge_180  is null and
       vage.not_due_amount is null ) 
       then 
       vage.days_31_60

//when ( vage.days_31_60   is not null and 
//       vage.days_1_30    is null and
//       vage.days_61_90   is null and
//       vage.days_91_120  is null and
//       vage.days_121_150 is null and
//       vage.days_151_180 is null and
//       vage.days_ge_180  is null and
//       vage.not_due_amount is not null ) 
//       then 
//       vage.days_31_60 + vage.not_due_amount
       
when ( vage.days_61_90   is not null and 
       vage.days_1_30    is null and
       vage.days_31_60   is null and
       vage.days_91_120  is null and
       vage.days_121_150 is null and
       vage.days_151_180 is null and
       vage.days_ge_180  is null and
       vage.not_due_amount is null ) 
       then 
       vage.days_61_90

//when ( vage.days_61_90   is not null and 
//       vage.days_1_30    is null and
//       vage.days_31_60   is null and
//       vage.days_91_120  is null and
//       vage.days_121_150 is null and
//       vage.days_151_180 is null and
//       vage.days_ge_180  is null and
//       vage.not_due_amount is not null ) 
//       then 
//       vage.days_61_90 + vage.not_due_amount

       
when ( vage.days_91_120   is not null and 
       vage.days_1_30    is null and
       vage.days_31_60   is null and
       vage.days_61_90  is null and
       vage.days_121_150 is null and
       vage.days_151_180 is null and
       vage.days_ge_180  is null and
       vage.not_due_amount is null ) 
       then 
       vage.days_91_120

//when ( vage.days_91_120   is not null and 
//       vage.days_1_30    is null and
//       vage.days_31_60   is null and
//       vage.days_61_90  is null and
//       vage.days_121_150 is null and
//       vage.days_151_180 is null and
//       vage.days_ge_180  is null and
//       vage.not_due_amount is not null ) 
//       then 
//       vage.days_91_120 + vage.not_due_amount
       
when ( vage.days_121_150   is not null and 
       vage.days_1_30    is null and
       vage.days_31_60   is null and
       vage.days_61_90  is null and
       vage.days_91_120 is null and
       vage.days_151_180 is null and
       vage.days_ge_180  is null and
       vage.not_due_amount is null ) 
       then 
       vage.days_121_150

//when ( vage.days_121_150   is not null and 
//       vage.days_1_30    is null and
//       vage.days_31_60   is null and
//       vage.days_61_90  is null and
//       vage.days_91_120 is null and
//       vage.days_151_180 is null and
//       vage.days_ge_180  is null and
//       vage.not_due_amount is not null ) 
//       then 
//       vage.days_121_150 + vage.not_due_amount
       
when ( vage.days_151_180   is not null and 
       vage.days_1_30    is null and
       vage.days_31_60   is null and
       vage.days_61_90  is null and
       vage.days_91_120 is null and
       vage.days_121_150 is null and
       vage.days_ge_180  is null and
       vage.not_due_amount is null ) 
       then 
       vage.days_151_180

//when ( vage.days_151_180   is not null and 
//       vage.days_1_30    is null and
//       vage.days_31_60   is null and
//       vage.days_61_90  is null and
//       vage.days_91_120 is null and
//       vage.days_121_150 is null and
//       vage.days_ge_180  is null and
//       vage.not_due_amount is not null ) 
//       then 
//       vage.days_151_180 + vage.not_due_amount
       
when ( vage.days_ge_180   is not null and 
       vage.days_1_30    is null and
       vage.days_31_60   is null and
       vage.days_61_90  is null and
       vage.days_91_120 is null and
       vage.days_121_150 is null and
       vage.days_151_180  is null and
       vage.not_due_amount is null ) 
       then 
       vage.days_ge_180

//when ( vage.days_ge_180   is not null and 
//       vage.days_1_30    is null and
//       vage.days_31_60   is null and
//       vage.days_61_90  is null and
//       vage.days_91_120 is null and
//       vage.days_121_150 is null and
//       vage.days_151_180  is null and
//       vage.not_due_amount is not null ) 
//       then 
//       vage.days_ge_180 + vage.not_due_amount
       
//when ( vage.not_due_amount   is not null and 
//       vage.days_1_30    is null and
//       vage.days_31_60   is null and
//       vage.days_61_90  is null and
//       vage.days_91_120 is null and
//       vage.days_121_150 is null and
//       vage.days_151_180  is null and
//       vage.days_ge_180 is null ) 
//       then 
//       vage.not_due_amount
                            
end as overdue_amount 

}



