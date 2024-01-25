     FTestlf    if   e             disk
     DEmpdoj2          S              6S 0
     C     *ENTRY        PLIST
     C                   PARM                    EMPDOJ1           6
      /Free
           Empdoj2 = %dec(Empdoj1);
           Setll *start Testlf;
             Dow Not %Eof(Testlf);
              Read Testlf;
                If Empdoj2 = Empdoj;
                    Dsply Empid;
                Else;
                 Leave;
                Endif;
              Read Testlf;
                Enddo;
            *inlr = *On;
