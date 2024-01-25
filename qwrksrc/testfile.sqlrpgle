     Hoption(*NOXREF:*NODEBUGIO:*SRCSTMT)
     Hthread(*SERIALIZE)
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('CIBINDDIR':'QC2LE')

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Input Files
     f*dvinstitf1uf   e           k disk    usropn
     fDVCLIENTF1if   e           k disk    usropn
     f
     f
       if not %open(DVCLIENTF1);
          open DVCLIENTF1;
       endif;
       *inlr = *On;
