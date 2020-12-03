N= 15
P= 55
a=0.01
h=0.005
m=0.5
 #### P1 ###
P1_a= a*(N/P^m) # FORMULA QUE ESTAVAMOS USANDO
P1_b= a * (N / P ^ m) # FORMULA LUCA

P1_a
P1_b
#### P2 ###
P2_a= a*(N/P^m)/(1+a*h*(N/P^m)) # FORMULA QUE ESTAVAMOS USANDO
P2_b= (a * N) / ((P ^ m) + (a * h * N)) # FORMULA LUCA

P2_a
P2_b
### P3 ####
P3_a = a*(N/P^m)^2/(1+a*h*(N/P^m)^2) # FORMULA QUE ESTAVAMOS USANDO
P3_b =  (a * N^2) / (P^(2*m) + (a * h * N ^ 2)) # FORMULA LUCA
             
P3_a
P3_b

               