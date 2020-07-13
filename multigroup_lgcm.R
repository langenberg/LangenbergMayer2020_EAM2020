library(lavaan)

load("dy11y25_resc.RData")

model_mg_lgca <- "

################################################################################
############################# model specification ##############################
################################################################################

  # group_weights
    group % c(gw00,gw01,gw10,gw11)*w
    N := exp(gw00) + exp(gw01) + exp(gw10) + exp(gw11)
    relfreq00 := exp(gw00)/N # Men.NotMarried
    relfreq01 := exp(gw01)/N # Men.Married
    relfreq10 := exp(gw10)/N # Women.NotMarried
    relfreq11 := exp(gw11)/N # Women.Married
  # measurement model
    eta1 =~ 1*y11 + 1*y21
    eta2 =~ 1*y12 + 1*y22
    eta3 =~ 1*y13 + 1*y23
    eta4 =~ 1*y14 + 1*y24
    eta5 =~ 1*y15 + 1*y25
    mf =~ 1*y21 + 1*y22 + 1*y23 + 1*y24 + 1*y25
  # variances
    eta1 ~~ 0*eta1
    eta2 ~~ 0*eta2 + 0*eta1
    eta3 ~~ 0*eta3 + 0*eta2 + 0*eta1
    eta4 ~~ 0*eta4 + 0*eta3 + 0*eta2 + 0*eta1
    eta5 ~~ 0*eta5 + 0*eta4 + 0*eta3 + 0*eta2 + 0*eta1
  # intercepts
    y11 + y12 + y13 + y14 + y15 ~ 0*1
    y21 + y22 + y23 + y24 + y25 ~ 0*1
    eta1 + eta2 + eta3 + eta4 + eta5 ~ 0*1
    mf ~ c(a_mf, a_mf, a_mf, a_mf)*1
    
    pi0  ~ c(a000, a001, a010, a011)*1
    pi1  ~ c(a100, a101, a110, a111)*1
    pi2  ~ c(a200, a201, a210, a211)*1
    pi3  ~ c(a300, a301, a310, a311)*1
    pi4  ~ c(a400, a401, a410, a411)*1
    
    # pi0  ~ c(    b_0_pi0_11,     b_0_pi0_12,     b_0_pi0_21,     b_0_pi0_22)*1
    # pi1  ~ c(    b_0_pi1_11,     b_0_pi1_12,     b_0_pi1_21,     b_0_pi1_22)*1
    # pi2  ~ c(    b_0_pi2_11,     b_0_pi2_12,     b_0_pi2_21,     b_0_pi2_22)*1
    # pi3  ~ c(    b_0_pi3_11,     b_0_pi3_12,     b_0_pi3_21,     b_0_pi3_22)*1
    # pi4  ~ c(    b_0_pi4_11,     b_0_pi4_12,     b_0_pi4_21,     b_0_pi4_22)*1
  # struc_coeff
    pi0 =~ 1*eta1 +    0*eta2 +  0.5*eta3 + 0*eta4 +    1*eta5
    pi1 =~ 0*eta1 +    1*eta2 +  0.5*eta3 + 0*eta4 +    1*eta5
    pi2 =~ 0*eta1 +    0*eta2 +  0.5*eta3 + 0*eta4 +    0*eta5
    pi3 =~ 0*eta1 + (-2)*eta2 + (-1)*eta3 + 0*eta4 + (-1)*eta5
    pi4 =~ 0*eta1 +    1*eta2 +  0.5*eta3 + 1*eta4 +    0*eta5


################################################################################
################################ eta variables #################################
################################################################################


eta_1_11 :=   1*a000 +   0*a100 +   0*a200 +  0*a300 +   0*a400
eta_2_11 :=   0*a000 +   1*a100 +   0*a200 + -2*a300 +   1*a400
eta_3_11 := 0.5*a000 + 0.5*a100 + 0.5*a200 + -1*a300 + 0.5*a400
eta_4_11 :=   0*a000 +   0*a100 +   0*a200 +  0*a300 +   1*a400
eta_5_11 :=   1*a000 +   1*a100 +   0*a200 + -1*a300 +   0*a400

eta_1_12 :=   1*a001 +   0*a101 +   0*a201 +  0*a301 +   0*a401
eta_2_12 :=   0*a001 +   1*a101 +   0*a201 + -2*a301 +   1*a401
eta_3_12 := 0.5*a001 + 0.5*a101 + 0.5*a201 + -1*a301 + 0.5*a401
eta_4_12 :=   0*a001 +   0*a101 +   0*a201 +  0*a301 +   1*a401
eta_5_12 :=   1*a001 +   1*a101 +   0*a201 + -1*a301 +   0*a401

eta_1_21 :=   1*a010 +   0*a110 +   0*a210 +  0*a310 +   0*a410
eta_2_21 :=   0*a010 +   1*a110 +   0*a210 + -2*a310 +   1*a410
eta_3_21 := 0.5*a010 + 0.5*a110 + 0.5*a210 + -1*a310 + 0.5*a410
eta_4_21 :=   0*a010 +   0*a110 +   0*a210 +  0*a310 +   1*a410
eta_5_21 :=   1*a010 +   1*a110 +   0*a210 + -1*a310 +   0*a410

eta_1_22 :=   1*a011 +   0*a111 +   0*a211 +  0*a311 +   0*a411
eta_2_22 :=   0*a011 +   1*a111 +   0*a211 + -2*a311 +   1*a411
eta_3_22 := 0.5*a011 + 0.5*a111 + 0.5*a211 + -1*a311 + 0.5*a411
eta_4_22 :=   0*a011 +   0*a111 +   0*a211 +  0*a311 +   1*a411
eta_5_22 :=   1*a011 +   1*a111 +   0*a211 + -1*a311 +   0*a411


################################################################################
############################### regression model ###############################
################################################################################

  # Regression model:
    # E(pi_j | women, married)
    # = bj00
    #   bj1_*I_G=2 +
    #   bj_1*I_M=2 +
    #   bj11*I_G=2*I_M=2
    #   
    # bj00 = (  b_0_11                           )*1
    # bj1_ = (- b_0_11          + b_0_21         )*1
    # bj_1 = (- b_0_11 + b_0_12                  )*1
    # bj11 = (  b_0_11 - b_0_12 - b_0_21 + b_0_22)*1


################################################################################
################################ Probabilities #################################
################################################################################

    P_G2    := relfreq10 + relfreq11
    P_M1    := relfreq00 + relfreq10
    P_M2    := relfreq01 + relfreq11
    P_G2_M2 := relfreq01

################################################################################
############################### effects for pi0 ################################
################################################################################
  
  # definition of regression parameters
    b000 :=   a000
    
    b01_ := - a000        + a010
    
    b0_1 := - a000 + a001 
    
    b011 :=   a000 - a001 - a010 + a011

                   
  # E(E(pi0 | GENDER, MARITALSTATUS))
    exp_pi0         := b000 * 1 +
                       b01_ * P_G2 +
                       b0_1 * P_M2 +
                       b011 * P_G2_M2
                       
  # # E(E(pi0 | GENDER=0, MARITALSTATUS))
  #   exp_pi0_G1      := b000 * 1 + 
  #                      b0_1 * P_M2
  #                
  # # E(E(y21 | GENDER=1, MARITALSTATUS))
  #   exp_pi0_G2      := b000 * 1 +
  #                      b01_ * 1 +
  #                      b0_1 * P_M2 +
  #                      b011 * P_M2
                       
  # avg effect GENDER pi0
    # E(E(pi0 | GENDER=2, MARITALSTATUS) -
    #   E(pi0 | GENDER=1, MARITALSTATUS))
    avg_gender_pi0  := b01_ * 1 +
                       b011 * P_M2

  # avg effect GENDER pi0
    # E(E(pi0 | GENDER, MARITALSTATUS=2) -
    #   E(pi0 | GENDER, MARITALSTATUS=1))
    avg_married_pi0 := b0_1 * 1 +
                       b011 * P_G2

################################################################################
############################### effects for pi1 ################################
################################################################################

  
  # definition of regression parameters
    b100 :=   a100
    
    b11_ := - a100        + a110
    
    b1_1 := - a100 + a101 
    
    b111 :=   a100 - a101 - a110 + a111

                   
  # E(E(pi1 | GENDER, MARITALSTATUS))
    exp_pi1         := b100 * 1 +
                       b11_ * P_G2 +
                       b1_1 * P_M2 +
                       b111 * P_G2_M2
  
  # # E(E(pi1 | GENDER=0, MARITALSTATUS))
  #   exp_pi1_G1      := b100 * 1 + 
  #                      b1_1 * P_M2
  #                
  # # E(E(y21 | GENDER=1, MARITALSTATUS))
  #   exp_pi1_G2      := b100 * 1 +
  #                      b11_ * 1 +
  #                      b1_1 * P_M2 +
  #                      b111 * P_M2
                       
  # avg effect GENDER pi1
    # E(E(pi1 | GENDER=2, MARITALSTATUS) -
    #   E(pi1 | GENDER=1, MARITALSTATUS))
    avg_gender_pi1  := b11_ * 1 +
                       b111 * P_M2

  # avg effect GENDER pi1
    # E(E(pi1 | GENDER, MARITALSTATUS=2) -
    #   E(pi1 | GENDER, MARITALSTATUS=1))
    avg_married_pi1 := b1_1 * 1 +
                       b111 * P_G2

################################################################################
############################### effects for pi2 ################################
################################################################################

  
  # definition of regression parameters
    b200 :=   a200
    
    b21_ := - a200        + a210
    
    b2_1 := - a200 + a201 
    
    b211 :=   a200 - a201 - a210 + a211

                   
  # E(E(pi2 | GENDER, MARITALSTATUS))
    exp_pi2         := b200 * 1 +
                       b21_ * P_G2 +
                       b2_1 * P_M2 +
                       b211 * P_G2_M2
  
  # # E(E(pi2 | GENDER=0, MARITALSTATUS))
  #   exp_pi2_G1      := b200 * 1 + 
  #                      b2_1 * P_M2
  #                
  # # E(E(y21 | GENDER=1, MARITALSTATUS))
  #   exp_pi2_G2      := b200 * 1 +
  #                      b21_ * 1 +
  #                      b2_1 * P_M2 +
  #                      b211 * P_M2
                       
  # avg effect GENDER pi2
    # E(E(pi2 | GENDER=2, MARITALSTATUS) -
    #   E(pi2 | GENDER=1, MARITALSTATUS))
    avg_gender_pi2  := b21_ * 1 +
                       b211 * P_M2

  # avg effect GENDER pi2
    # E(E(pi2 | GENDER, MARITALSTATUS=2) -
    #   E(pi2 | GENDER, MARITALSTATUS=1))
    avg_married_pi2 := b2_1 * 1 +
                       b211 * P_G2

################################################################################
############################### effects for pi3 ################################
################################################################################

  
  # definition of regression parameters
    b300 :=   a300
    
    b31_ := - a300        + a310
    
    b3_1 := - a300 + a301 
    
    b311 :=   a300 - a301 - a310 + a311

                   
  # E(E(pi3 | GENDER, MARITALSTATUS))
    exp_pi3         := b300 * 1 +
                       b31_ * P_G2 +
                       b3_1 * P_M2 +
                       b311 * P_G2_M2
  # # E(E(pi3 | GENDER=0, MARITALSTATUS))
  #   exp_pi3_G1      := b300 * 1 + 
  #                      b3_1 * P_M2
  #                
  # # E(E(y21 | GENDER=1, MARITALSTATUS))
  #   exp_pi3_G2      := b300 * 1 +
  #                      b31_ * 1 +
  #                      b3_1 * P_M2 +
  #                      b311 * P_M2
                       
  # avg effect GENDER pi3
    # E(E(pi3 | GENDER=2, MARITALSTATUS) -
    #   E(pi3 | GENDER=1, MARITALSTATUS))
    avg_gender_pi3  := b31_ * 1 +
                       b311 * P_M2

  # avg effect GENDER pi3
    # E(E(pi3 | GENDER, MARITALSTATUS=2) -
    #   E(pi3 | GENDER, MARITALSTATUS=1))
    avg_married_pi3 := b3_1 * 1 +
                       b311 * P_G2

################################################################################
############################### effects for pi4 ################################
################################################################################

  
  # definition of regression parameters
    b400 :=   a400
    
    b41_ := - a400        + a410
    
    b4_1 := - a400 + a401 
    
    b411 :=   a400 - a401 - a410 + a411

                   
  # E(E(pi4 | GENDER, MARITALSTATUS))
    exp_pi4         := b400 * 1 +
                       b41_ * P_G2 +
                       b4_1 * P_M2 +
                       b411 * P_G2_M2
  
  # # E(E(pi4 | GENDER=0, MARITALSTATUS))
  #   exp_pi4_G1      := b400 * 1 + 
  #                      b4_1 * P_M2
  #                
  # # E(E(y21 | GENDER=1, MARITALSTATUS))
  #   exp_pi4_G2      := b400 * 1 +
  #                      b41_ * 1 +
  #                      b4_1 * P_M2 +
  #                      b411 * P_M2
                       
  # avg effect GENDER pi4
    # E(E(pi4 | GENDER=2, MARITALSTATUS) -
    #   E(pi4 | GENDER=1, MARITALSTATUS))
    avg_gender_pi4  := b41_ * 1 +
                       b411 * P_M2

  # avg effect GENDER pi4
    # E(E(pi4 | GENDER, MARITALSTATUS=2) -
    #   E(pi4 | GENDER, MARITALSTATUS=1))
    avg_married_pi4 := b4_1 * 1 +
                       b411 * P_G2

"

fit_mg_lgca <-
    sem(
        model_mg_lgca,
        data = dy11y25_resc,
        group = "group",
        missing = "ml",
        estimator="MLR",
        group.label = c(
            "Men.NotMarried",
            "Men.Married",
            "Women.NotMarried",
            "Women.Married"
        )
    )

summary(fit_mg_lgca, fit.measures = T)
