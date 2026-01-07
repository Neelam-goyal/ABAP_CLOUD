@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Lot Email'
@Metadata.allowExtensions: true
define root view entity ZE_INSP_LOT_EMAIL as select from yinsp_lot_email as email
{

  key email.lot_type,
  key email.plant,
  key email.emailid,
  email.to_cc
      
}
