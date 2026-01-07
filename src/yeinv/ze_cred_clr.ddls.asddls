@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View entity'
@Metadata.allowExtensions: true
define root view entity ZE_CRED_CLR
  as select from zsd_cred_clr as clr
{
  key clr.gstin,
  key clr.ownerid,
  key clr.authtoken,
      clr.url_auth_token,
      clr.url_einv_gen,
      clr.url_einv_canc,
      clr.url_qrb2c,
      clr.url_eway_gen,
      clr.url_eway_irn,
      clr.url_eway_partb,
      clr.url_eway_canc
}
