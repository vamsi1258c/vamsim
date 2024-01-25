     FDtedsp2   CF   E             Workstn
     DDatenew          S              8  0
     Ddates            S              6A
      /Free
           DoU *In03 = *On;
           Exfmt Format1;
            Test(DE) *mdy Date;
             If %Error;
               Msg = 'Entered Date format is in Wronfg format';
             Else;
           Datenew = %Dec(%char(%date(date:*Mdy/):*iso):7:0);
               Msg = 'Entered Date is in correct format';
             Endif;
             Enddo;
           *Inlr = *On;
