@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'System ID'
@Metadata.allowExtensions: true
define root view entity ZE_SYSID as select from zsd_sysid as sid
{

  key sid.objcode,
  key sid.sysid,
  sid.objvalue
  
}
