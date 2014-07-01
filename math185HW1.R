#Math 185, HW 1
# Spencer Ochs

# Problem 1
# a)

# make table
letter_grades <- matrix(c(51,82,79,51,76,72,48,78,66,66,71,85,46,72,74), ncol = 3)
colnames(letter_grades) <- c('A','B','C')
rownames(letter_grades) <- c('year1','year2','year3','year4','year5')
grade_tab <- as.table(letter_grades)

# Graphs

# overall proportions
overall_tab <- matrix(c(sum(grade_tab[,'A']), sum(grade_tab[,'B']), sum(grade_tab[,'C'])),ncol=3)
colnames(overall_tab) <- c('A','B', 'C')
pie(overall_tab, labels= c('A','B','C'), radius=1, main='Pie Chart', xlab='Proportion of grades over all 5 years')
barplot(overall_tab, names.arg= c('A','B','C'), xlab="Grades", ylab="# of grades given over 5 years", main='Barplot')

# proportions for each year
par(mfrow = c(2,5), mai=rep(0.1, 4), xaxt='n', yaxt='n')
for(i in c('year1','year2','year3','year4','year5')){
  year_tab <- grade_tab[i,] 
  barplot(year_tab, names.arg= c('A','B','C'), xlab=paste("Grades for year ",i), ylab=paste("# of grades given in year ", i), main=paste(i)) 
}
for(i in c('year1','year2','year3','year4','year5')){
  year_tab <- grade_tab[i,] 
  pie(year_tab, labels= c('A','B','C'), radius=1, xlab=paste("Grades for year ",i), main=paste(i)) 
}
# reset plots window
dev.off()

# Hypothesis testing problem:
# H_0: P('A')=.25, P('B')=.35, P('C')=.4 "Prof follows guidelines"
# H_1: P('A')!=.25 OR P('B')!=.35 OR P('C')!=.4 "Prof does not follow guidelines"

# test if overall observed grade counts match expected counts
test <- chisq.test(overall_tab/sum(overall_tab), p=c(.25,.35,.4))
test$stat
# gives p-value of 0.03799505, so reject hypothesis that Prof is following guidelines

# b) 
# establish independence between the years, ie not getting harsher, using 
# want to know whether there is an association between year and grades
# H_0: the grades in each year are independent
# H_1: the grades in each year are dependent (decreasing portion of A's)
addmargins(grade_tab)
test_ind <- chisq.test(grade_tab)
test_ind
# p-value given is 0.0001685, so definately not independent, but looking at proportion of 
# grades each year it does not appear that there is a trend toward less A's and more C's
# given in each year (as seen in the plots below).
freq_grade_tab <- grade_tab # convert to grade frequencies per year
for(i in c('year1','year2','year3','year4','year5')){
  freq_grade_tab[i,] <- grade_tab[i,]/sum(grade_tab[i,])
}
barplot(freq_grade_tab[,'A'], names.arg= c('year1','year2','year3','year4','year5'), xlab="years 1 through 5", ylab="frequency of grades in each year", main="A's per year") 
barplot(freq_grade_tab[,'B'], names.arg= c('year1','year2','year3','year4','year5'), xlab="years 1 through 5", ylab="frequency of grades in each year", main="B's per year") 
barplot(freq_grade_tab[,'C'], names.arg= c('year1','year2','year3','year4','year5'), xlab="years 1 through 5", ylab="frequency of grades in each year", main="C's per year") 


# c) ideally we would like to see the scores for the class before the grades are "curved"
# which would tell us if the class is getting lower scores overall from year to year.


# Problem 2
# test for association

# extract data by date and gender, don't show totals
# Natality data taken from http://wonder.cdc.gov/natality.html 
nat_dat = read.table('Natality, 2007-2010.txt', skip=1, nrows=102)
colnames(nat_dat) <- c('State', 'State Code', 'Gender', 'Gender Code', 'Births')
# create matrix from natality data
natality <- matrix(sapply(nat_dat['Births'], as.numeric), nrow=51, ncol=2, byrow=TRUE)
#states <- unique(nat_dat['State']) Cant get this to work...
# Note: I am completely ashamed of the following code, but I was fed up with R at this point and just needed to do something braindead
states <- c('Alabama','Alaska','Arizona','Arkansas','California','Colorado','Connecticut','Delaware', 'District of Columbia', 'Florida','Georgia','Hawaii','Idaho','Illinois','Indiana','Iowa','Kansas','Kentucky','Louisiana','Maine','Maryland','Massachusetts','Michigan','Minnesota','Mississippi','Missouri','Montana','Nebraska','Nevada','New Hampshire','New Jersey','New Mexico','New York','North Carolina','North Dakota','Ohio','Oklahoma','Oregon','Pennsylvania','Rhode Island','South Carolina','South Dakota','Tennessee','Texas','Utah','Vermont','Virginia','Washington','West Virginia','Wisconsin','Wyoming')
dimnames(natality) <- list(states, c("Female","Male"))

# now to test for an association simply use test for independence, like before, between 
# state and gender
test_nat <- chisq.test(natality)
test_nat # give p-value of 0.07431 so we don't reject the null that state and the gender of 
# the baby born in that state are independent.
