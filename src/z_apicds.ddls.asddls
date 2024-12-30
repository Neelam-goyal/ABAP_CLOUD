@EndUserText.label: 'Custom Payment Advice with CUD'
@AccessControl.authorizationCheck: #NOT_REQUIRED
define root view entity Z_APICDS
as select from I_PaymentAdvice
{
    key CompanyCode,
    key PaymentAdviceAccountType,
    key PaymentAdviceAccount,
    key PaymentAdvice
    // Include additional fields as needed
}
