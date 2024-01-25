     Hoption(*NOXREF:*NODEBUGIO:*SRCSTMT)
     Hthread(*SERIALIZE)
     H DFTACTGRP(*NO) ACTGRP(*NEW)

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Input Files
       dcl-pi TESTCONST;
          var1     char(10)        const;
       end-pi;

         processLoop('test'+var1);

       *inlr = *On;
       // *********************************************************************
       // processLoop: process a html 'LOOP' variable
       // *********************************************************************
       dcl-proc processLoop;
       dcl-pi *n;
          var2     char(15)        const;
       end-pi;
         processLoop2(','+%trim(var2)+',');
       end-proc processLoop;
       // *********************************************************************
       // processLoop: process a html 'LOOP' variable
       // *********************************************************************
       dcl-proc processLoop2;
       dcl-pi *n;
          var3     char(20)        const;
       end-pi;
         dsply var3;
       end-proc processLoop2;
