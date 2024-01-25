     FDatevalidaCF   E             Workstn
      /Free
           DoU *In03 = *On;
           Exfmt Format1;
            Test(DE) *mdy Date;
             If %Error;
               Msg = 'Entered Date format is in Wronfg format';
             Else;
               Msg = 'Entered Date is in correct format';
             Endif;
             Enddo;
           *Inlr = *On;
