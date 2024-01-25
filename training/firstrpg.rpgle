     D EMPLOYEE        DS
     D Name                    1     20A
     D Age                    21     25S 0
     D Location               26     35A
     D DOB                            8A   inz('20211222')
     D Year                           4A   OVERLAY(DOB)
     D Month                          2A   OVERLAY(DOB:5)
     D Day                            2A   OVERLAY(DOB:7)
      *
     C     Year          Dsply
     C     Month         Dsply
     C     Day           Dsply
     C                   EVAL      *INLR = *ON
