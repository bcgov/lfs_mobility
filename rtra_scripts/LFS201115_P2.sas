DATA work.LFS_Retirement;
SET RTRAdata.LFS201115 (keep= ID SYEAR AGE NOC_5);

length age10 $10;

if 15 <= AGE < 25 then age10 = "15-24";
    else if 25 <= AGE < 35 then age10 = "25-34";
    else if 35 <= AGE < 45 then age10 = "35-44";
    else if 45 <= AGE < 55 then age10 = "45-54";
    else if 55 <= AGE < 65 then age10 = "55-64";
    else age10 = "nwa";

/*Second half of the NOCS... missing in other half*/

if NOC_5 >= 50000;

run;

%RTRAFreq(
     InputDataset=work.LFS_Retirement,
     OutputName=age1115p2,
     ClassVarList= SYEAR NOC_5 age10,
     UserWeight=FINALWT);
run;
