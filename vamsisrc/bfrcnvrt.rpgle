     F*
     F* BVM User ID Creation Screen
     FBUA001FM  CF   E             WORKSTN SFILE(BUA001B1:S0SFRN)
     F*
     F* BVM User ID creation - LOG file
     FBUUALOGP  IF   E           K DISK
     F*
     F* BVM User ID creation - LOG file Logical
     FBUUALOGL1 IF   E           K DISK    RENAME(BUUALOGRC:BUUALOGR1)
     F                                     PREFIX(B1:2)
     F*
     F* Plant master file
     FPLMSTP    IF   E           K DISK
     F*
     F* Origin Parameters File
     FTFOPP     IF   E           K DISK
     F*
     D*
     D W0CMD1          C                   CONST('CHKOBJ (')
     D W0CMD2          C                   CONST(') OBJTYPE(*USRPRF)')
     D W0ANUM          C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ012-
     D                                     3456789')
     D W0ALPH          C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
     D W0NUM           C                   CONST('0123456789')
     D*
     D W1POS           S              2P 0 INZ(*ZEROS)
     D W1WCNT          S              2P 0 INZ(*ZEROS)
     D W1NUSR          S             10A
     D W2NUSR          S             10A
     D W3NUSR          S              1A   DIM(10)
     D I               S              2P 0 INZ(*ZEROS)
     D W1NUSL          S              2P 0 INZ(*ZEROS)
     D W1RRN           S              4P 0
     D W1CHK           S              2P 0 INZ(*ZEROS)
     D*
     D P0ACT           S              1A
     D*
     D P0CMD           S             50A   INZ(*BLANKS)
     D P0LEN           S             15P 5 INZ(*ZEROS)
     D*
     D                SDS
     D W1USER                254    263                                         User name
     D*
     C* Mainline
     C*
     C                   SELECT
     C                   WHEN      P0ACT = 'A'
     C                   EXSR      $PROMADD
     C*
     C                   WHEN      P0ACT = 'D'
     C                   EXSR      $PROMDEL
     C*
     C                   OTHER
     C*
     C* Do Nothing
     C                   ENDSL
     C*
     C                   EVAL      *INLR = *ON
     C*
     C******************************************************************
     C* $PROMADD : Prompt Screen To display BVM User ID -Add Screen    *
     C******************************************************************
     C     $PROMADD      BEGSR
     C*
     C                   DOU       *IN03 = *ON
     C*
     C* Display Prompt Screen
     C                   EXFMT     BUA001S1
     C*
     C* Reset Indicators
     C                   EXSR      $RESETIN
     C*
     C* If F3 is taken
     C* - Skip Validation and Exit
     C                   IF        *IN03 = *ON
     C                   LEAVE
     C                   ENDIF
     C* If F5 is taken
     C* - Refresh Screen (with initialized values)
     C                   IF        *IN05 = *ON
     C                   EXSR      $REFRESH
     C                   ITER
     C                   ENDIF
     C*
     C* If F7 option is taken
     C* - Display User Log Screen
     C                   IF        *IN07 = *ON
     C                   EXSR      $USRLOG
     C                   ITER
     C                   ENDIF
     C*
     C* Validations
     C                   EXSR      $VALIDADD
     C*
     C* If Any validation fails
     C* - Display Prompt Screen with Corresponding Error/Warning
     C                   IF        *IN99 = *ON OR
     C                             (*IN98 = *ON AND W1WCNT = 1)
     C                   ITER
     C                   ENDIF
     C*
     C* If All validations are successful
     C* - Update Details in LDA
     C* - Intialize Screen Variables
     C                   EXSR      $LOADLDA
     C*
     C* Refresh Screen (with initialized values)
     C                   EXSR      $REFRESH
     C                   ENDDO
     C*
     C     $PROMADDE     ENDSR
     C*
     C******************************************************************
     C* $PROMDEL : Prompt Screen To display BVM User ID -Del Screen    *
     C******************************************************************
     C     $PROMDEL      BEGSR
     C*
     C                   DOU       *IN03 = *ON
     C*
     C* Display Prompt Screen
     C                   EXFMT     BUA001S2
     C*
     C* Reset Indicators
     C                   EXSR      $RESETIN
     C*
     C* If F3 is taken
     C* - Skip Validation and Exit
     C                   IF        *IN03 = *ON
     C                   LEAVE
     C                   ENDIF
     C* If F5 is taken
     C* - Refresh Screen (with initialized values)
     C                   IF        *IN05 = *ON
     C                   EXSR      $REFRESH
     C                   ITER
     C                   ENDIF
     C*
     C* If F7 option is taken
     C* - Display User Log Screen
     C                   IF        *IN07 = *ON
     C                   EXSR      $USRLOG
     C                   ITER
     C                   ENDIF
     C*
     C* Validations
     C                   EXSR      $VALIDDEL
     C*
     C* If Any validation fails
     C* - Display Prompt Screen with Corresponding Error/Warning
     C                   IF        *IN99 = *ON OR
     C                             (*IN98 = *ON AND W1WCNT = 1)
     C                   ITER
     C                   ENDIF
     C*
     C* If All validations are successful
     C* - Update Details in LDA
     C* - Intialize Screen Variables
     C                   EXSR      $LOADLDA
     C*
     C* Refresh Screen (with initialized values)
     C                   EXSR      $REFRESH
     C                   ENDDO
     C*
     C     $PROMDELE     ENDSR
     C*
     C******************************************************************
     C* $RESETIN : Reset Indicators                                    *
     C******************************************************************
     C     $RESETIN      BEGSR
     C*
     C* Initialize all Screen Indicators
     C                   IF        P0ACT = 'A'
     C                   EVAL      *IN30 = *OFF
     C                   EVAL      *IN31 = *OFF
     C                   EVAL      *IN32 = *OFF
     C                   EVAL      *IN33 = *OFF
     C                   EVAL      *IN34 = *OFF
     C                   EVAL      *IN35 = *OFF
     C                   EVAL      *IN36 = *OFF
     C                   EVAL      *IN98 = *OFF
     C*
     C                   ELSEIF    P0ACT = 'D'
     C                   EVAL      *IN37 = *OFF
     C                   EVAL      *IN38 = *OFF
     C                   ENDIF
     C*
     C                   EVAL      *IN99 = *OFF
     C*
     C     $RESETINE     ENDSR
     C*
     C******************************************************************
     C* $REFRESH : Refresh Prompt Screen Variables                     *
     C******************************************************************
     C     $REFRESH      BEGSR
     C*
     C* Initialize all screen fields with 'Blanks'
     C                   IF        P0ACT = 'A'
     C                   EVAL      #SFUSR = *BLANKS
     C                   EVAL      #SNUSR = *BLANKS
     C                   EVAL      #SPLCD = *BLANKS
     C                   EVAL      #SORG  = *BLANKS
     C                   EVAL      #SFNAM = *BLANKS
     C                   EVAL      #SLNAM = *BLANKS
     C                   EVAL      #SPNUM = *BLANKS
     C                   EVAL      #SMAIL = *BLANKS
     C                   EVAL      #STKTR = *BLANKS
     C                   EVAL      #SSOXR = *BLANKS
     C                   EVAL      #SDESC = *BLANKS
     C                   EVAL      #SEMSG = *BLANKS
     C                   EVAL      #SWMSG = *BLANKS
     C*
     C                   ELSEIF    P0ACT = 'D'
     C                   EVAL      #SUSRP = *BLANKS
     C                   EVAL      #STKTR = *BLANKS
     C                   EVAL      #SSOXR = *BLANKS
     C                   EVAL      #SDESC = *BLANKS
     C                   EVAL      #SEMSG = *BLANKS
     C                   ENDIF
     C*
     C* Initialize Warning Count as '0'
     C                   IF        W1WCNT <> 0
     C                   EVAL      *IN30 = *OFF
     C                   EVAL      W1WCNT = 0
     C                   ENDIF
     C*
     C     $REFRESHE     ENDSR
     C******************************************************************
     C* $USRLOG  : Display User Log Screen                             *
     C******************************************************************
     C     $USRLOG       BEGSR
     C*
     C                   EVAL      *IN03 = *OFF
     C                   EVAL      *IN12 = *OFF
     C*
     C                   DOW       *IN03 = *OFF AND *IN12 =*OFF
     C*
     C* Clear Subfile
     C                   EVAL      *IN04 = *ON
     C                   WRITE     BUA001SC1
     C                   EVAL      *IN04 = *OFF
     C*
     C                   EVAL      #SUSER = W1USER
     C                   EVAL      W1RRN = 0
     C*
     C* Load Subfile
     C     W1USER        SETLL     BUUALOGRC
     C     W1USER        READE     BUUALOGRC
     C*
     C                   DOW       NOT %EOF(BUUALOGP)
     C*
     C                   EVAL      *IN08 = *ON
     C                   EVAL      W1RRN = W1RRN+1
     C                   EVAL      S0SFRN = W1RRN
     C*
     C                   EVAL      #SUSRF = BUUSRF
     C                   EVAL      #SUSRD = BUUSRD
     C                   EVAL      #SPLOV = BUPLOV
     C                   EVAL      #SATYP = BUATYP
     C                   EVAL      #SWMOV = BUWMOV
     C                   EVAL      #SREF1# = BUREF1#
     C                   EVAL      #SREF2# = BUREF2#
     C                   EVAL      #SDATE = BUCRDT
     C                   EVAL      #STIME = BUCRTM
     C                   EVAL      #SUDES = BUUDES
     C*
     C                   WRITE     BUA001B1
     C*
     C     W1USER        READE     BUUALOGRC
     C                   ENDDO
     C*
     C* If No records to display
     C* - Display Message 'No User log found'
     C                   IF        W1RRN = 0
     C                   EVAL      *IN39 = *ON
     C                   ENDIF
     C*
     C* Display Subfile Screen
     C                   WRITE     BUA001F1
     C                   EXFMT     BUA001SC1
     C                   READ      BUA001F1
     C*
     C                   EVAL      *IN39 = *OFF
     C                   ENDDO
     C*
     C     $USRLOGE      ENDSR
     C*
     C******************************************************************
     C* $VALIDADD: Validations of Prompt screen variables              *
     C******************************************************************
     C     $VALIDADD     BEGSR
     C*
     C* If 'From User' is 'Blanks'
     C* - Display an Error Message
     C                   IF        #SFUSR = *BLANKS
     C                   EVAL      *IN30 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: From User can not be Blanks'
     C                   LEAVESR
     C                   ENDIF
     C*
     C* If 'From User' is not an existing user
     C* - Display an Error Message
     C                   EVAL      P0CMD  = W0CMD1 + %TRIM(#SFUSR) + W0CMD2
     C                   EVAL      P0LEN  = %LEN(%TRIM(P0CMD))
     C*
     C                   CALL(E)   'QCMDEXC'     P0API
     C                   IF        %ERROR()
     C                   EVAL      *IN30 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: From User Profile is not an-
     C                              existing User'
     C                   LEAVESR
     C                   ENDIF
     C*
     C* If 'From User' is already present in 'BUUALOGP' file
     C* - Display a Warning Message Before Proceeding
     C     #SFUSR        CHAIN     BUUALOGL1
     C                   IF        %FOUND(BUUALOGL1)
     C                   EVAL      W1WCNT = W1WCNT + 1
     C                   EVAL      *IN30 = *ON
     C                   EVAL      *IN98 = *ON
     C                   EVAL      #SWMSG = 'Warning: From User Profile is -
     C                             Present in User ID creation File'
     C                   ENDIF
     C*
     C* If 'New User' is 'Blanks'
     C* - Display an Error Message
     C                   IF        #SNUSR = *BLANKS
     C                   EVAL      *IN31 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: New User can not be Blanks'
     C                   LEAVESR
     C                   ENDIF
     C*
     C* If 'New User' is An existing user
     C* - Display an Error Message
     C                   EVAL      P0CMD  = W0CMD1 + %TRIM(#SNUSR) + W0CMD2
     C                   EVAL      P0LEN  = %LEN(%TRIM(P0CMD))
     C*
     C                   CALL(E)   'QCMDEXC'     P0API
     C                   IF        NOT %ERROR()
     C                   EVAL      *IN31 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: New User Profile is already-
     C                              an existing User in system'
     C                   LEAVESR
     C                   ENDIF
     C*
     C* If 'New User' is not valid user profile
     C* - Display an Error Message
     C                   EVAL      W1CHK = 0
     C                   EVAL      W1CHK = %CHECK(W0ANUM:%TRIM(#SNUSR):1)
     C*
     C                   IF        W1CHK > 0
     C                   EVAL      *IN31 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: Invalid New User Profile -
     C                             (Use only A-Z, 0-9 for user profile)'
     C                   LEAVESR
     C                   ENDIF
     C*
     C                   EVAL      I = I+1
     C*
     C* If First 2 Positions of User Profile are not numeric
     C* - Display an Error Message
     C                   EVAL      W1CHK = 0
     C                   EVAL      W1CHK = %CHECK(W0ALPH:
     C                                     %TRIM(%SUBST(#SNUSR:1:2)) :1)
     C                   IF        W1CHK > 0
     C                   EVAL      *IN31 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: Invalid New User Profile -
     C                             (Use only A-Z for first 2chars of User prf)'
     C                   LEAVESR
     C                   ENDIF
     C*
     C                   IF        #SPLCD <> *BLANKS
     C*
     C* If Plant Code is not valid (Not present in PLMSTP)
     C* - Display an Error Message
     C     #SPLCD        CHAIN     PLMSTP
     C                   IF        NOT %FOUND(PLMSTP)
     C                   EVAL      *IN32 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: Invalid Plant code (Not -
     C                             Present in Plant master file'
     C                   LEAVESR
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   IF        #SORG <> *BLANKS
     C*
     C* If Origin is not valid (Not present in TFOPP)
     C* - Display an Error Message
     C     #SORG         CHAIN     TFOPP
     C                   IF        NOT %FOUND(TFOPP)
     C                   EVAL      *IN33 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: Invalid Origin (Not -
     C                             Present in Origin Parameters File'
     C                   LEAVESR
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   IF        #SPNUM <> *BLANKS
     C*
     C* If Phone Number is not Numeric
     C* - Display an Error Message
     C                   EVAL      W1CHK = 0
     C                   EVAL      W1CHK = %CHECK(W0NUM:%TRIM(#SPNUM):1)
     C*
     C                   IF        W1CHK > 0
     C                   EVAL      *IN34 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: Invalid Phone Number -
     C                             (Phone Number should be Numeric)'
     C                   LEAVESR
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   IF        #SMAIL <> *BLANKS
     C*
     C* If '@' is not present in Mail ID
     C* - Display an Error Message
     C                   EVAL      W1POS = %SCAN('@':#SMAIL:1)
     C*
     C                   IF        W1POS = 0
     C                   EVAL      *IN35 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: Invalid Email-ID'
     C                   LEAVESR
     C                   ENDIF
     C*
     C* If '@' is repeated again in Mail ID (or) If '.com' is not present in Mail ID after '@'
     C* - Display an Error Message
     C                   IF        %SCAN('@':#SMAIL:W1POS+1) > 0 OR
     C                             %SCAN('.COM':#SMAIL:W1POS+1) = 0
     C                   EVAL      *IN35 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: Invalid Email-ID'
     C                   LEAVESR
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C* If Both Ticket Reference and SOX Audit Reference are maintained as 'Blanks'
     C* - Display an Error Message
     C                   IF        #STKTR = *BLANKS AND #SSOXR = *BLANKS
     C                   EVAL      *IN36 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: Either Ticket Ref or -
     C                             SOX Audit Ref should be entered'
     C                   ENDIF
     C*
     C*
     C     $VALIDADDE    ENDSR
     C*
     C******************************************************************
     C* $VALIDDEL: Validations of Prompt screen variables              *
     C******************************************************************
     C     $VALIDDEL     BEGSR
     C*
     C* If 'User Profile' is Blanks
     C* - Display an Error Message
     C                   IF        #SUSRP = *BLANKS
     C                   EVAL      *IN37 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: User Profile can not -
     C                             be Blanks'
     C                   LEAVESR
     C                   ENDIF
     C*
     C* If 'User Profile' is not an existing user
     C* - Display an Error Message
     C                   EVAL      P0CMD  = W0CMD1 + %TRIM(#SUSRP) + W0CMD2
     C                   EVAL      P0LEN  = %LEN(%TRIM(P0CMD))
     C*
     C                   CALL(E)   'QCMDEXC'     P0API
     C                   IF        %ERROR()
     C                   EVAL      *IN37 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: From User Profile is not an-
     C                              existing User'
     C                   LEAVESR
     C                   ENDIF
     C*
     C* If Both Ticket Reference and SOX Audit Reference are maintained as 'Blanks'
     C* - Display an Error Message
     C                   IF        #STKTR = *BLANKS AND #SSOXR = *BLANKS
     C                   EVAL      *IN38 = *ON
     C                   EVAL      *IN99 = *ON
     C                   EVAL      #SEMSG = 'Error: Either Ticket Ref or -
     C                             SOX Audit Ref should be entered'
     C                   ENDIF
     C*
     C     $VALIDDELE    ENDSR
     C*
     C******************************************************************
     C* $LOADLDA : Update Details in LDA                               *
     C******************************************************************
     C     $LOADLDA      BEGSR
     C*
     C     $LOADLDAE     ENDSR
     C*
     C******************************************************************
     C* INZSR    : Initialization Subroutine                           *
     C******************************************************************
     C     INZSR         BEGSR
     C*
     C* Entry Parameter List
     C     *ENTRY        PLIST
     C                   PARM                    P0ACT
     C*
     C* Parameter list of API
     C     P0API         PLIST
     C                   PARM                    P0CMD
     C                   PARM                    P0LEN
     C*
     C     INZSRE        ENDSR
     C*
