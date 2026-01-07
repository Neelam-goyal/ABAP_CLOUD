@AbapCatalog.sqlViewName: 'ZV_PRCD_ELEMENTS'
@AbapCatalog.compiler.compareFilter: true
//@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Tax Detail'
define view ZI_PRCD_ELEMENTS as select from I_BillingDocumentItemPrcgElmnt as COND
{
     key COND.BillingDocument,
     key COND.BillingDocumentItem,
     
     @Semantics.currencyCode
     COND.TransactionCurrency, 

      sum( case ConditionType when 'PPR0' then COND.ConditionQuantity 
       when 'PMP0' then COND.ConditionQuantity end ) as ConditionQuantity ,
           
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'ZMRP' then COND.ConditionRateValue  end ) as ITEM_ZMRP_AMOUNT ,
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'ZGST' then COND.ConditionAmount  end ) as ITEM_ASSESSABLEAMOUNT ,
   
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    
    // COND.ConditionQuantity , 
    @Semantics.amount.currencyCode: ''
    sum(
     case ConditionType 
       when 'PPR0' then  COND.ConditionRateValue
       when 'PMP0' then  COND.ConditionRateValue
       when 'ZBAS' then  COND.ConditionAmount // COND.ConditionRateValue
       
         //case when COND.ConditionQuantity is not initial then cast(COND.ConditionAmount as abap.fltp)  / cast(COND.ConditionQuantity as abap.fltp)
         // when  COND.ConditionQuantity is initial then COND.ConditionAmount  end
          
       // when 'ZBAS' then  COND.ConditionRateValue 
       //case when COND.ConditionQuantity is not initial then cast(COND.ConditionRateValue as abap.fltp)  / cast(COND.ConditionQuantity as abap.fltp)
         //   when COND.ConditionQuantity is initial then COND.ConditionAmount end
                              end  
                              )
                               as ITEM_UNITPRICE ,
                               
           // ConditionRateValue                    
                              //cast(ekpo.umren as abap.fltp) as umren
                              
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''                      
    sum( case ConditionType when 'ZDIS' then  COND.ConditionAmount * - 1  
                            when 'ZTRD' then  COND.ConditionAmount * - 1
                            when 'ZTR1' then  COND.ConditionAmount * - 1
                            when 'ZTR2' then  COND.ConditionAmount * - 1
                               end ) as ITEM_DISCOUNTAMOUNT ,
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'ZAMD' then  COND.ConditionAmount  end ) as ITEM_AMOTIZATION ,
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'ZBAS' then  COND.ConditionAmount  end ) as ITEM_ZBAS_UNIT ,
    
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'ZFRT' then  COND.ConditionAmount  end ) as ITEM_FREIGHT ,
    //@Semantics.amount.currencyCode: 'TransactionCurrency'

    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'ZPAC' then  COND.ConditionAmount  end ) as ITEM_PKG_CHRG,
        
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType 
 //   when 'ZFRT' then COND.ConditionAmount
//    when 'ZOTH' then COND.ConditionAmount
    when 'ZINS' then COND.ConditionAmount  
    end ) as ITEM_FERT_OTH ,
     
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType 
                            when 'ZOTH' then  COND.ConditionAmount  
                            end ) as ITEM_OTHERCHARGE ,
    
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''                       
    sum( case ConditionType when 'JOSG' then  COND.ConditionRateValue  end ) as  ITEM_SGSTRATE ,
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'JOCG' then  COND.ConditionRateValue  end ) as ITEM_CGSTRATE ,
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'JOIG' then  COND.ConditionRateValue  end ) as ITEM_IGSTRATE   ,
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'JOSG' then  COND.ConditionAmount  end ) as  ITEM_SGSTAMOUNT ,
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'JOCG' then  COND.ConditionAmount  end ) as ITEM_CGSTAMOUNT,
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'JOIG' then  COND.ConditionAmount  end ) as ITEM_IGSTAMOUNT ,
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'ZAMR' then  COND.ConditionAmount end ) as ITEM_LESS_AMMORTIZATION ,
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'PMP0' then  COND.ConditionAmount  
                            //when 'PCIP' then  COND.ConditionAmount
                             end ) as ITEM_TOTALAMOUNT ,
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''                        
    sum( case ConditionType when 'ZGRD' then  COND.ConditionAmount  end ) as ITEM_GRANDTOTALAMOUNT ,
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: ''
    sum( case ConditionType when 'DRD1' then  COND.ConditionAmount  end ) as ITEM_ROUNDOFF ,
    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: '' 
    sum( case ConditionType when 'PCIP' then  COND.ConditionRateValue  end ) as ITEM_PCIP_AMT,  

    //@Semantics.amount.currencyCode: 'TransactionCurrency' 
    @Semantics.amount.currencyCode: '' 
    sum( case ConditionType when 'ZINS' then  COND.ConditionAmount  end ) as ITEM_INSURANCE_AMT
           
  }

  
group by
COND.BillingDocument,
COND.BillingDocumentItem , 
COND.TransactionCurrency 

