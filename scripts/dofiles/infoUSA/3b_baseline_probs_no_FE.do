
sum same_race  if choice==1
sum same_race  if choice==1 & Minority==1
sum same_race  if choice==1 & Minority==0
sum same_race  if choice==0
sum same_race  if choice==0 & Minority==1
sum same_race  if choice==0 & Minority==0



reghdfe  same_race  choice ,    cl(CBSA_downtown) level(90) 

di (_b[_cons]  )     

di (_b[_cons]  +  _b[choice] ) 



nlcom (RR_choice   : (_b[_cons]  )     / (_b[_cons] + _b[choice] )  )    


reghdfe  same_race  Minority_choice Minority White_choice ,    cl(CBSA_downtown) level(90) 

di (_b[_cons]  +  _b[Minority] )     
di (_b[_cons] + _b[Minority_choice] +  _b[Minority])  
di (_b[_cons]  )                  
di (_b[_cons] + _b[White_choice])  

nlcom (RR_Minority   : (_b[_cons]  +  _b[Minority] )     / (_b[_cons] + _b[Minority_choice] +  _b[Minority])  )     ///
      (RR_White :  (_b[_cons]  )                  / (_b[_cons] + _b[White_choice])   ) 
      