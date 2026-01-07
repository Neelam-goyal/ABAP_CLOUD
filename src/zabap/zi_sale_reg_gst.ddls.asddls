@AbapCatalog.sqlViewName: 'ZV_SALE_REG_GST'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sale registerr'

@ClientHandling.algorithm: #SESSION_VARIABLE
@Analytics: {
dataCategory: #CUBE,
internalName:#LOCAL
}
@ObjectModel: {
usageType: {
dataClass: #MIXED,
serviceQuality: #D,
sizeCategory: #XXL
},
supportedCapabilities: [ #ANALYTICAL_PROVIDER, #SQL_DATA_SOURCE, #CDS_MODELING_DATA_SOURCE ],
modelingPattern: #ANALYTICAL_CUBE
}

define root view ZI_SALE_REG_GST
as select distinct from ZI_SALE_REG as SALE_REG
{
@EndUserText.label: 'Company Code'
key CompanyCode,
key BillingDocument,
key BillingDocumentItem,
@EndUserText.label: 'Cancelled Invoice'
BillingDocumentIsCancelled,
BillingDocumentType,
//@EndUserText.label: 'Posting Status'
//postingstatus,
TransactionCurrency,
@EndUserText.label: 'Exchange Rate'
AccountingExchangeRate,
@EndUserText.label: 'Purchase Order'
PurchaseOrder,
IncotermsLocation1,
IncotermsLocation2,
MaterialDescriptionByCustomer,
MaterialByCustomer,
CustomerPaymentTermsName,
WeightUnit,
GrossWeight,
NetWeight,
SizeOrDimensionText,
ProductOldID,
//@EndUserText.label: 'Product Type'
//ProductType,
@EndUserText.label: 'Po_Number'
PurchaseOrderByCustomer,
@EndUserText.label: 'Po_Date'
CustomerPurchaseOrderDate,
@EndUserText.label: 'SO_date'
SalesDocumentDate,
CreationDate,
DeliveryDocument,
DeliveryDocumentItem,
BillingDocumentDate,
SDDocumentCategory,
BillingDocumentCategory,
SalesOrganization,
DistributionChannel,
Division,
SoldToParty,
IncotermsClassification,
CustomerPaymentTerms,
DocumentReferenceID,
DOC_TYPE,
PlantName,
AddressID,
AddresseeFullName,
CityName,
@EndUserText.label: 'PLANT_POSTAL'
PostalCode,
@EndUserText.label: 'PLANT_STREET'
Street,
@EndUserText.label: 'PLANT_STREET1'
StreetName,
@EndUserText.label: 'PLANT_HOUSENO'
HouseNumber,
@EndUserText.label: 'PLANT_COUNTRY'
Country,
@EndUserText.label: 'PLANT_REGION'
Region,
@EndUserText.label: 'PLANT_FROMOFADDRESS'
FormOfAddress,
@EndUserText.label: 'PLANT_PHONE'
PhoneAreaCodeSubscriberNumber,
@EndUserText.label: 'PLANT_GSTIN'
PLANT_GSTIN,
@EndUserText.label: 'PLANT_EMAIL'
PLANT_EMAIL,
@EndUserText.label: 'HSN Code_N'
hsn,
@EndUserText.label: 'HSN Code'
HSN_CODE,
product,
BillingDocumentItemText,
@Semantics.quantity.unitOfMeasure: ''
case BillingDocumentType when 'G2' then BillingQuantity * - 1 
                          when 'CBRE'  then BillingQuantity * - 1
                          when 'S1' then  BillingQuantity * - 1  
                          else BillingQuantity end as BillingQuantity ,  
BillingQuantityUnit,
SalesDocument,
SalesDocumentItem,
ItemGrossWeight,
StorageLocation,
Plant,
BaseUnit,
TaxCode,
SalesGroup,
ShippingPoint,
@EndUserText.label: 'Ship_to_Party'
ship_to_party,
@EndUserText.label: 'Ship to Name'
WE_NAME,
@EndUserText.label: 'Ship to City'
WE_CITY,
@EndUserText.label: 'Ship to Pin'
WE_PIN,
@EndUserText.label: 'Ship to Street'
WE_STREET,
@EndUserText.label: 'Ship to Street1'
WE_STREET1,
@EndUserText.label: 'Ship to House No'
WE_HOUSE_NO,
@EndUserText.label: 'Ship to Country'
WE_COUNTRY,
@EndUserText.label: 'Ship to Region'
WE_REGION,
@EndUserText.label: 'Ship to From of add'
WE_FROM_OF_ADD,
@EndUserText.label: 'Ship to GSTIN'
WE_TAX,
@EndUserText.label: 'Ship to Pan'
WE_PAN,
@EndUserText.label: 'Ship to Email'
WE_EMAIL,
@EndUserText.label: 'Ship to Phone'
WE_PHONE4,
@EndUserText.label: 'Ship to Street2'
WE_StreetPrefixName1,
@EndUserText.label: 'Ship to Street3'
WE_StreetPrefixName2,
@EndUserText.label: 'Ship to Street4'
WE_StreetSuffixName1,
@EndUserText.label: 'Sold to Party'
sold_to_party,
@EndUserText.label: 'Sold to Name'
AG_NAME,
@EndUserText.label: 'Sold to City'
AG_CITY,
@EndUserText.label: 'Sold to Pin'
AG_PIN,
@EndUserText.label: 'Sold to Street'
AG_STREET,
@EndUserText.label: 'Sold to Street1'
AG_STREET1,
@EndUserText.label: 'Sold to House No'
AG_HOUSE_NO,
@EndUserText.label: 'Sold to Country'
AG_COUNTRY,
@EndUserText.label: 'Sold to Region'
AG_REGION,
@EndUserText.label: 'Sold to From of Add'
AG_FROM_OF_ADD,
@EndUserText.label: 'Sold to Tax'
AG_TAX,
@EndUserText.label: 'Sold to Pan'
AG_PAN,
@EndUserText.label: 'Sold to Email'
AG_EMAIL,
@EndUserText.label: 'Sold to Phone'
AG_PHONE4,
@EndUserText.label: 'Sold to Street2'
AG_StreetPrefixName1,
@EndUserText.label: 'Sold to Street3'
AG_StreetPrefixName2,
@EndUserText.label: 'Sold to Street4'
AG_StreetSuffixName1,
@EndUserText.label: 'Payer Code'
payer_code,
@EndUserText.label: 'Payer Name'
RG_NAME,
@EndUserText.label: 'Payer City'
RG_CITY,
@EndUserText.label: 'Payer Pin'
RG_PIN,
@EndUserText.label: 'Payer Street'
RG_STREET,
@EndUserText.label: 'Payer Street1'
RG_STREET1,
@EndUserText.label: 'Payer House No'
RG_HOUSE_NO,
@EndUserText.label: 'Payer Country'
RG_COUNTRY,
@EndUserText.label: 'Payer Region'
RG_REGION,
@EndUserText.label: 'Payer FROM_OF_ADD'
RG_FROM_OF_ADD,
@EndUserText.label: 'Payer GSTIN'
RG_TAX,
@EndUserText.label: 'Payer Pan'
RG_PAN,
@EndUserText.label: 'Payer Email'
RG_EMAIL,
@EndUserText.label: 'Payer Phone'
RG_PHONE4,
@EndUserText.label: 'Payer Street2'
RG_StreetPrefixName1,
@EndUserText.label: 'Payer Street3'
RG_StreetPrefixName2,
@EndUserText.label: 'Payer Street4'
RG_StreetSuffixName1,
@EndUserText.label: 'Bill to Party'
bill_to_party,
RE_NAME,
RE_CITY,
RE_PIN,
RE_STREET,
RE_STREET1,
RE_HOUSE_NO,
RE_COUNTRY,
RE_REGION,
RE_FROM_OF_ADD,
RE_TAX,
RE_PAN,
RE_EMAIL,
RE_PHONE4,
RE_StreetPrefixName1,
RE_StreetPrefixName2,
RE_StreetSuffixName1,
SP_code,
SP_NAME,
SP_CITY,
SP_PIN,
SP_STREET,
SP_STREET1,
SP_HOUSE_NO,
SP_COUNTRY,
SP_REGION,
SP_FROM_OF_ADD,
SP_TAX,
SP_PAN,
SP_EMAIL,
SP_PHONE4,
SP_StreetPrefixName1,
SP_StreetPrefixName2,
SP_StreetSuffixName1,
ES_code,
ES_NAME,
ES_CITY,
ES_PIN,
ES_STREET,
ES_STREET1,
ES_HOUSE_NO,
ES_COUNTRY,
ES_REGION,
ES_FROM_OF_ADD,
ES_TAX,
ES_PAN,
ES_EMAIL,
ES_PHONE4,
ES_StreetPrefixName1,
ES_StreetPrefixName2,
ES_StreetSuffixName1,
ConditionQuantity,
@EndUserText.label: 'ITEM_ASSESSABLEAMOUNT'
case BillingDocumentType when 'G2' then ITEM_ASSESSABLEAMOUNT * - 1 
                          when 'CBRE'  then ITEM_ASSESSABLEAMOUNT * - 1
                          when 'S1' then  ITEM_ASSESSABLEAMOUNT * - 1  
                          else ITEM_ASSESSABLEAMOUNT end as ITEM_ASSESSABLEAMOUNT ,

@EndUserText.label: 'ITEM_ASSESSABLEAMOUNT_INR'
case BillingDocumentType when 'G2' then ITEM_ASSESSABLEAMOUNT_INR * - 1 
                          when 'CBRE'  then ITEM_ASSESSABLEAMOUNT_INR * - 1
                          when 'S1' then  ITEM_ASSESSABLEAMOUNT_INR * - 1  
                          else ITEM_ASSESSABLEAMOUNT_INR end as ITEM_ASSESSABLEAMOUNT_INR ,
@EndUserText.label: 'ITEM_UNITPRICE'
ITEM_UNITPRICE as ITEM_UNITPRICE,
@EndUserText.label: 'ITEM_DISCOUNTAMOUNT'
case BillingDocumentType when 'G2' then ITEM_DISCOUNTAMOUNT * - 1 
                          when 'CBRE'  then ITEM_DISCOUNTAMOUNT * - 1
                          when 'S1' then  ITEM_DISCOUNTAMOUNT * - 1  
                          else ITEM_DISCOUNTAMOUNT end as ITEM_DISCOUNTAMOUNT ,
                          
@EndUserText.label: 'ITEM_DISCOUNTAMOUNT_INR'

case BillingDocumentType when 'G2' then ITEM_DISCOUNTAMOUNT_INR * - 1 
                          when 'CBRE'  then ITEM_DISCOUNTAMOUNT_INR * - 1
                          when 'S1' then  ITEM_DISCOUNTAMOUNT_INR * - 1  
                          else ITEM_DISCOUNTAMOUNT_INR end as ITEM_DISCOUNTAMOUNT_INR ,
@EndUserText.label: 'ITEM_FREIGHT'
case BillingDocumentType when 'G2' then ITEM_FREIGHT * - 1 
                          when 'CBRE'  then ITEM_FREIGHT * - 1
                          when 'S1' then  ITEM_FREIGHT * - 1  
                          else ITEM_FREIGHT end as ITEM_FREIGHT ,
@EndUserText.label: 'ITEM_AMOTIZATION'
case BillingDocumentType when 'G2' then ITEM_AMOTIZATION * - 1 
                          when 'CBRE'  then ITEM_AMOTIZATION * - 1
                          when 'S1' then  ITEM_AMOTIZATION * - 1  
                          else ITEM_AMOTIZATION end as ITEM_AMOTIZATION ,
                          
@EndUserText.label: 'ITEM_OTHERCHARGE'
case BillingDocumentType when 'G2' then ITEM_OTHERCHARGE * - 1 
                          when 'CBRE'  then ITEM_OTHERCHARGE * - 1
                          when 'S1' then  ITEM_OTHERCHARGE * - 1  
                          else ITEM_OTHERCHARGE end as ITEM_OTHERCHARGE ,
                          
@EndUserText.label: 'ITEM_SGSTRATE'
ITEM_SGSTRATE as ITEM_SGSTRATE,
@EndUserText.label: 'ITEM_CGSTRATE'
ITEM_CGSTRATE as ITEM_CGSTRATE,
@EndUserText.label: 'ITEM_IGSTRATE'
ITEM_IGSTRATE as ITEM_IGSTRATE,
@EndUserText.label: 'ITEM_SGSTAMOUNT'
case BillingDocumentType when 'G2' then ITEM_SGSTAMOUNT * - 1 
                          when 'CBRE'  then ITEM_SGSTAMOUNT * - 1
                          when 'S1' then  ITEM_SGSTAMOUNT * - 1  
                          else ITEM_SGSTAMOUNT end as ITEM_SGSTAMOUNT ,
                          
@EndUserText.label: 'ITEM_CGSTAMOUNT'
case BillingDocumentType when 'G2' then ITEM_CGSTAMOUNT * - 1 
                          when 'CBRE'  then ITEM_CGSTAMOUNT * - 1
                          when 'S1' then  ITEM_CGSTAMOUNT * - 1  
                          else ITEM_CGSTAMOUNT end as ITEM_CGSTAMOUNT ,
@EndUserText.label: 'ITEM_IGSTAMOUNT'

case BillingDocumentType when 'G2' then ITEM_IGSTAMOUNT * - 1 
                          when 'CBRE'  then ITEM_IGSTAMOUNT * - 1
                          when 'S1' then  ITEM_IGSTAMOUNT * - 1  
                          else ITEM_IGSTAMOUNT end as ITEM_IGSTAMOUNT ,
@EndUserText.label: 'ITEM_IGSTAMOUNT_INR'
case BillingDocumentType when 'G2' then ITEM_IGSTAMOUNT_INR * - 1 
                          when 'CBRE'  then ITEM_IGSTAMOUNT_INR * - 1
                          when 'S1' then  ITEM_IGSTAMOUNT_INR * - 1  
                          else ITEM_IGSTAMOUNT_INR end as ITEM_IGSTAMOUNT_INR ,
@EndUserText.label: 'ITEM_CGSTAMOUNT_INR'

case BillingDocumentType when 'G2' then ITEM_CGSTAMOUNT_INR * - 1 
                          when 'CBRE'  then ITEM_CGSTAMOUNT_INR * - 1
                          when 'S1' then  ITEM_CGSTAMOUNT_INR * - 1  
                          else ITEM_CGSTAMOUNT_INR end as ITEM_CGSTAMOUNT_INR ,
@EndUserText.label: 'ITEM_SGSTAMOUNT_INR'

case BillingDocumentType when 'G2' then ITEM_SGSTAMOUNT_INR * - 1 
                          when 'CBRE'  then ITEM_SGSTAMOUNT_INR * - 1
                          when 'S1' then  ITEM_SGSTAMOUNT_INR * - 1  
                          else ITEM_SGSTAMOUNT_INR end as ITEM_SGSTAMOUNT_INR ,
@EndUserText.label: 'ITEM_TOTALAMOUNT'

case BillingDocumentType when 'G2' then ITEM_TOTALAMOUNT * - 1 
                          when 'CBRE'  then ITEM_TOTALAMOUNT * - 1
                          when 'S1' then  ITEM_TOTALAMOUNT * - 1  
                          else ITEM_TOTALAMOUNT end as ITEM_TOTALAMOUNT ,
@EndUserText.label: 'ITEM_TOTALAMOUNT_INR'

case BillingDocumentType when 'G2' then ITEM_TOTALAMOUNT_INR * - 1 
                          when 'CBRE'  then ITEM_TOTALAMOUNT_INR * - 1
                          when 'S1' then  ITEM_TOTALAMOUNT_INR * - 1  
                          else ITEM_TOTALAMOUNT_INR end as ITEM_TOTALAMOUNT_INR ,
@EndUserText.label: 'ITEM_GRANDTOTALAMOUNT'
case BillingDocumentType when 'G2' then ITEM_GRANDTOTALAMOUNT * - 1 
                          when 'CBRE'  then ITEM_GRANDTOTALAMOUNT * - 1
                          when 'S1' then  ITEM_GRANDTOTALAMOUNT * - 1  
                          else ITEM_GRANDTOTALAMOUNT end as ITEM_GRANDTOTALAMOUNT,
                          
                          
@EndUserText.label: 'ITEM_GRANDTOTALAMOUNT_INR'
case BillingDocumentType when 'G2' then ITEM_GRANDTOTALAMOUNT_INR * - 1 
                          when 'CBRE'  then ITEM_GRANDTOTALAMOUNT_INR * - 1
                          when 'S1' then  ITEM_GRANDTOTALAMOUNT_INR * - 1  
                          else ITEM_GRANDTOTALAMOUNT_INR end as ITEM_GRANDTOTALAMOUNT_INR ,
@EndUserText.label: 'ITEM_LESS_AMMORTIZATION'
case BillingDocumentType when 'G2' then ITEM_LESS_AMMORTIZATION * - 1 
                          when 'CBRE'  then ITEM_LESS_AMMORTIZATION * - 1
                          when 'S1' then  ITEM_LESS_AMMORTIZATION * - 1  
                          else ITEM_LESS_AMMORTIZATION end as ITEM_LESS_AMMORTIZATION ,
@EndUserText.label: 'ITEM_PCIP_AMT'
case BillingDocumentType when 'G2' then  ITEM_PCIP_AMT * - 1 
                          when 'CBRE'  then  ITEM_PCIP_AMT * - 1
                          when 'S1' then   ITEM_PCIP_AMT * - 1  
                          else  ITEM_PCIP_AMT end as  ITEM_PCIP_AMT ,
@EndUserText.label: 'ITEM_ROUNDOFF'
case BillingDocumentType when 'G2' then ITEM_ROUNDOFF * - 1 
                          when 'CBRE'  then ITEM_ROUNDOFF * - 1
                          when 'S1' then  ITEM_ROUNDOFF * - 1  
                          else ITEM_ROUNDOFF end as ITEM_ROUNDOFF ,
@EndUserText.label: 'ITEM_ZMRP_AMOUNT'
case BillingDocumentType when 'G2' then ITEM_ZMRP_AMOUNT * - 1 
                          when 'CBRE'  then ITEM_ZMRP_AMOUNT * - 1
                          when 'S1' then  ITEM_ZMRP_AMOUNT * - 1  
                          else ITEM_ZMRP_AMOUNT end as ITEM_ZMRP_AMOUNT ,
@EndUserText.label: 'ITEM_ZBAS_UNIT'
ITEM_ZBAS_UNIT,
@EndUserText.label: 'ITEM_FERT_OTH'
case BillingDocumentType when 'G2' then ITEM_FERT_OTH * - 1 
                          when 'CBRE'  then ITEM_FERT_OTH * - 1
                          when 'S1' then  ITEM_FERT_OTH * - 1  
                          else ITEM_FERT_OTH end as ITEM_FERT_OTH ,
@EndUserText.label: 'SUPPLY_TYPE'
SUPPLY_TYPE
} 
