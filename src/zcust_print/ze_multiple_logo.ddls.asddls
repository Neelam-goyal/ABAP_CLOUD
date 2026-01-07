@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View entity'
@Metadata.allowExtensions: true
define root view entity ZE_MULTIPLE_LOGO 
  as select from zmulti_logo as logo
{

  key logo.logoname,
      logo.logovalue,
      local_created_by,  
      local_created_at,
      local_last_changed_by,
      local_last_changed_at,
      last_change_at
}
