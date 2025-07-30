# -------------------------------------- #
# Loading the necessary data from NHANES
# -------------------------------------- #

# 2011-12

# Demographics
demo1112 <-nhanes("DEMO_G")
# Body Measures (for height and weight to calculate BMI)
body1112 <- nhanes("BMX_G")
# Glycohemoglobin (HbA1c)
HbA1c1112 <- nhanes("GHB_G")
# DXA data (for appendicular lean mass)
dexa1112 <- nhanes("DXX_G")
# Physical Activity
phys1112 <- nhanes("PAQ_G")
# Alcohol consumption
alcohol1112 <- nhanes("ALQ_G")
# Cigarette smoking data
smoking1112 <- nhanes("SMQ_G")
# Healthy Eating Score
hei1112 <- score(method = "simple", years = "1112", component = "total score")
# Sleep data
sleep1112 <- nhanes("SLQ_G")

# 2013-14

demo1314 <-nhanes("DEMO_H")
body1314 <- nhanes("BMX_H")
HbA1c1314 <- nhanes("GHB_H")
dexa1314 <- nhanes("DXX_H")
phys1314 <- nhanes("PAQ_H")
alcohol1314 <- nhanes("ALQ_H")
smoking1314 <- nhanes("SMQ_H")
hei1314 <- score(method = "simple", years = "1314", component = "total score")
sleep1314 <- nhanes("SLQ_H")

# 2015-16

demo1516 <-nhanes("DEMO_I")
body1516 <- nhanes("BMX_I")
HbA1c1516 <- nhanes("GHB_I")
dexa1516 <- nhanes("DXX_I")
phys1516 <- nhanes("PAQ_I")
alcohol1516 <- nhanes("ALQ_I")
smoking1516 <- nhanes("SMQ_I")
hei1516 <- score(method = "simple", years = "1516", component = "total score")
sleep1516 <- nhanes("SLQ_I")

# 2017-18

# Demographics
demo1718 <-nhanes("DEMO_J")
body1718 <- nhanes("BMX_J")
HbA1c1718 <- nhanes("GHB_J")
dexa1718 <- nhanes("DXX_J")
phys1718 <- nhanes("PAQ_J")
alcohol1718 <- nhanes("ALQ_J")
smoking1718 <- nhanes("SMQ_J")
hei1718 <- score(method = "simple", years = "1718", component = "total score")
sleep1718 <- nhanes("SLQ_J")