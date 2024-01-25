     DResult           S              3S 0
     C     *Entry        Plist
     C                   Parm                    String1          40
      /Free
              Result = %Scan('A123' : String1);
               If Result <> 0;
     C     'String Found'dsply
     C                   Else
     C     'String notfo'Dsply
     C                   Endif
          *inlr = *On;
      /End-Free
