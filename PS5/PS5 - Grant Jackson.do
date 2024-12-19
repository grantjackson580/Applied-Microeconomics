// Clear memory and set system
clear
cls

// Read the dataset
cd "C:/Users/gmj011/Documents"
use "production_function.dta"

// Declare panel data structure
xtset id year


// ---------- Question 1 ----------


// Question 1(a): OLS estimation with time dummies
reg y lp rt k ik i.year

// Store coefficients for lambda calculations
nlcom (lambda_L: -_b[rt]/_b[lp]) ///
      (lambda_K: -_b[ik]/_b[k])
	  
// Question 1(b): Test lambda_L = 1
// H0: -_b[rt]/_b[lp] = 1
nlcom (-_b[rt]/_b[lp] - 1)

// Question 1(c): Test lambda_K = 1
// H0: -_b[ik]/_b[k] = 1
nlcom (-_b[ik]/_b[k] - 1)

// Question 1(d): Test for CRS
// H0: alpha_L + alpha_K = 1
test (_b[lp] + _b[k] = 1)


// ---------- Question 2 ----------


// Question 2(a): Fixed Effects estimation
xtreg y lp rt k ik i.year, fe

// Store FE coefficients for lambda calculations
nlcom (lambda_L: -_b[rt]/_b[lp]) ///
      (lambda_K: -_b[ik]/_b[k])
	  
// Question 2(b): Same tests as Q1 but for FE
// Test lambda_L = 1
nlcom (-_b[rt]/_b[lp] - 1)

// Test lambda_K = 1
nlcom (-_b[ik]/_b[k] - 1)

// Test for CRS
test (_b[lp] + _b[k] = 1)


// ---------- Question 3 ----------


// Question 3(b): Fixed Effects + C-0
xtreg y L.y lp L.lp rt L.rt k L.k ik L.ik i.year, fe

// Calculate lambdas for FE+CO
nlcom (lambda_L: -_b[rt]/_b[lp]) ///
      (lambda_K: -_b[ik]/_b[k])
	  
// Question 3(c): Test CO restrictions
// Test each restriction separately
testnl _b[L.y] + _b[L.lp]/_b[lp] = 0
testnl _b[L.y] + _b[L.rt]/_b[rt] = 0
testnl _b[L.y] + _b[L.k]/_b[k] = 0
testnl _b[L.y] + _b[L.ik]/_b[ik] = 0

// Test all restrictions jointly
testnl (_b[L.y] + _b[L.lp]/_b[lp] = 0) ///
       (_b[L.y] + _b[L.rt]/_b[rt] = 0) ///
       (_b[L.y] + _b[L.k]/_b[k] = 0) ///
       (_b[L.y] + _b[L.ik]/_b[ik] = 0)

	   
// ---------- Question 4 ----------


// Arellano-Bond without serial correlation
xtabond2 y lp rt k ik i.year, ///
         gmm(y lp rt k ik, lag(2 .)) ///
         iv(i.year) robust noleveleq


// ---------- Question 5 ----------	

	 
// Arellano-Bond with AR(1)
xtabond2 y L.y lp L.lp rt L.rt k L.k ik L.ik i.year, ///
         gmm(y lp rt k ik, lag(2 .)) ///
         iv(i.year) robust noleveleq

// Test CO restrictions
testnl _b[L.y] + _b[L.lp]/_b[lp] = 0
testnl _b[L.y] + _b[L.rt]/_b[rt] = 0
testnl _b[L.y] + _b[L.k]/_b[k] = 0
testnl _b[L.y] + _b[L.ik]/_b[ik] = 0

// Test all restrictions jointly
testnl (_b[L.y] + _b[L.lp]/_b[lp] = 0) ///
       (_b[L.y] + _b[L.rt]/_b[rt] = 0) ///
       (_b[L.y] + _b[L.k]/_b[k] = 0) ///
       (_b[L.y] + _b[L.ik]/_b[ik] = 0)