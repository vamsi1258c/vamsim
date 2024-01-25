     D arr_name        S             20    dim(10)
     D idx             s              2S 0
     C                   for       idx = 1 to 10 by 1
     C                   eval      arr_name(idx) = 'Mahesh'
     C                   eval      arr_name(idx) = 'Suresh'
     C                   eval      arr_name(idx) = 'Ramesh'
     C                   eval      arr_name(idx) = 'Naresh'
     C                   endfor
     C                   for       idx = 1 to 10 by 1
      *
     C                   if        arr_name(idx) <> *blanks
     C     arr_name(idx) dsply
     C                   endif
     C                   endfor
     C                   eval      *inlr = *on
