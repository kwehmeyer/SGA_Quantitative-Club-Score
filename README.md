# SGA_Quantitative-Club-Score


https://kahlilwehmeyer.shinyapps.io/SGA_Quantitative-Club-Score/

Creator: Kahlil Wehmeyer

***
# Weights
*Note* Use a desktop markdown renderer or something like https://stackedit.io to view equations properly

Please read this first so you can understand how score are calculated. You will better understand how to adjust weights and how your adjustments will affect the scoring.

## Club Performance Score
This is a punitive score. Meaning that a club can have a maximum score of 0 for this category. 

### Budget Percentage Usage

**Calculation**
$$ - (\frac{Percentage\,Of\,BudgetUsed}{Punitive\,Threshold})  \times Weight $$

Adjustable Weights:
- Punitive Threshold (How much we expect them to have spent)
- Weight (How harshly we want budget to matter)

### Missing /  Late Paperwork

**Calculation**
$$ Missing/Late\,Paperwork \times Weight$$

Adjustable Weights:
- Weight

### Monthly Meetings Missed

**Calculation**
$$ \frac{Meetings\,Missed}{Total\,Meetings} $$

Adjustable Weights:
- None

### Missing / Broken Assets

**Calculation**
$$ Bad\; Assets \times Weight
$$
Adjustable Weights:
- Weight

### Final Club Performance Score

**Calculation**
$$ Budget - Missing/Late\, Paperwork - Monthly\, Meetings\, Missed - Bad/Broken\, Assets$$

## Club Social Score
This is an additive score - the more campus and student involvement the better score the club will yield.

### General Meetings

**Calculation**
$$ No.\,General\,Meetings \times (\frac{General\,Meeting\,Attendance}{Attendance\,Weight}) \times Weight $$

Adjustable Weights:
- Attendance Weight
- Weight

### Events

**Calculation**

$$ No.\,Events \times (\frac{Event\,Attendance}{Attendance\,Weight}) \times Weight $$

Adjustable Weights:
- Attendance Weight
- Weight

### E-Board Meetings

$$ No.\:E-Board\:Meetings  \times Weight $$

Adjustable Weights:
- Weight

## Final Club Social Score

$$ General\, Meetings + Events + E-Board\, Meetings$$

# Final Score 

$$ \frac{Social\,Score + Performance\, Score}{2} + Category\, Bonus $$
