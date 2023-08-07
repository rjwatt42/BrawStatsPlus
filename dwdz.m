
syms z n
assume(n>3)

alpha=0.05;

w1=normcdf(norminv(alpha/2)+z*sqrt(n-3));
w2=normcdf(norminv(alpha/2)-z*sqrt(n-3));

dw=diff(w1,z)+diff(w2,z);

eval(dw)
