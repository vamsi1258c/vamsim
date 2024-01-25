     H option(*nodebugio)
     FACCTDTA   UF   E           K DISK
      *
      *
     C                   dow       not %eof(ACCTDTA)
     C                   read      ACCTDTA
     C                   if        %eof(ACCTDTA)
     C                   leave
     C                   endif
     C                   if        lname = 'mallempalli'
     C                   eval      lname = 'm'
     C                   update    ACCTDTA1
     C                   endif
     C                   enddo
     C                   eval      *inlr = *on
