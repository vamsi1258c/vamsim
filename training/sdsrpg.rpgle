     D PROG_INFO      SDS
     D pgmname                 1     10a
     D pgmsts                 11     15s 0
     D pgmprvsts              16     20s 0
     D pgmsrcstmt             21     28a
     D pgmroutine             29     36a
     D pgmparms               37     39s 0
     D pgmmsgid               40     46a
     D pgmmi#                 47     50a
     D pgmwork                51     80a
     D pgmlib                 81     90a
     D pgmerrdta              91    170a
     D pgmrpgmsg             171    174a
     D pgmjob                244    253a
     D pgmuser               254    263a
     D pgmjobnum             264    269s 0
     D pgmjobdate            270    275s 0
     D pgmrundate            276    281s 0
     D pgmruntime            282    287s 0
      *
     C     pgmname       dsply
     C     pgmuser       dsply
     C     pgmjob        dsply
     C                   eval      *inlr = *on
