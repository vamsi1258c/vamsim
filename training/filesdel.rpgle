     H option(*nodebugio)
     FACCTDTA   UF   E           K DISK
      *
      *
     C                   dow       not %eof(ACCTDTA)
     C                   read      ACCTDTA
     C                   if        %eof(ACCTDTA)
     C                   leave
     C                   endif
     C                   if        lname = 's'
     C                   delete    ACCTDTA
     C                   endif
     C                   enddo
     C                   eval      *inlr = *on
