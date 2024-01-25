     FTstdsp2   cf   e             workstn
     DResult           S              3S 0
     DString1          S             40
     DStringNew        S             40
     C*    *Entry        Plist
     C*                  Parm                    String1          40
      /Free
              Dou *In03 = *On;
              Exfmt Format1;
              String1 = string;
              Result = %Scan('A123' : String1);
               If Result <> 0;
           StringNew = %Replace('D456':String1:Result+1:4);
               Else;
               Msg = 'String Not found';
               Endif;
               Enddo;
          *inlr = *On;
      /End-Free
