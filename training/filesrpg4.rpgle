     H option(*nodebugio)
     FACCTDTA   IF   E           K DISK
      *
      *
     C                   dow       not %eof(ACCTDTA)
     C                   read      ACCTDTA
     C                   if        %eof(ACCTDTA)
     C                   leave
     C                   endif
     C                   enddo
     C                   eval      *inlr = *on
