@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'View entity'
@Metadata.allowExtensions: true
define root view entity ZE_NUM_TO_STRING as select from zsd_num2string as NUM
{
    key NUM.num as Num,
    NUM.word as Word
}


