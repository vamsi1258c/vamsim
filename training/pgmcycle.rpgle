     FORDER     IPE  E           K DISK
     D SUMQTY          S              7  0
     D SUMPRC          S              7  0
     IORDERRC
     I                                          ODNUM         L1
     I*                                         ODITM         L2
     C                   ADD       ODQTY         SUMQTY
     C                   ADD       ODPRC         SUMPRC
     CL1   'Tot Qty:'    DSPLY
     CL1   SUMQTY        DSPLY
     CL1   'Tot Prc:'    DSPLY
     CL1   SUMPRC        DSPLY
     CL1                 Clear                   SUMQTY
     CL1                 Clear                   SUMPRC
