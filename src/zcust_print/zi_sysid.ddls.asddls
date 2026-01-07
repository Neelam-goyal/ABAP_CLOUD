@AbapCatalog.sqlViewName: 'ZV_SYSID'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'System ID'
@Metadata.allowExtensions: true
define root view ZI_SYSID as select from zsd_sysid as sid
{

  key sid.objcode,
  key sid.sysid,
  sid.objvalue
}
