sets
hour / 1*10 /
type / a,b /
;

parameters
value(hour,type)
value2(hour,type)
count(type)
sums(type)
average(type)
;
alias (type,type2);


value('1','a') = 10;
value('2','a') = 20;
value('1','b') = 1;
value('2','b') = 2;
value('3','b') = 5;


value2(hour,type)$value(hour,type) = 1;


count(type) = sum(hour,value2(hour,type));
