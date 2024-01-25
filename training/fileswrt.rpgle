     H option(*nodebugio)
     FACCTDTA   O    E           K DISK
      *
      *
     C                   clear                   ACCTDTA1
     C                   eval      FNAME = 'Karan'
     C                   eval      LNAME = 'Singh'
     C                   eval      ACCNO = 5465544576
     C                   eval      BALANCE = 5674865
     C                   eval      CITY = 'hyd'
     C                   write     acctdta1
     C                   eval      *inlr = *on
