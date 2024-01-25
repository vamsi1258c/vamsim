/*    RTVRPGLES - RETRIEVE RPGLE SOURCE                                */
/*    CREATED BY JIM FRIEDMAN 01/26/04                                 */

             CMD        PROMPT('RETRIEVE RPGLE SOURCE')
             PARM       KWD(PGMNAME) TYPE(QUAL) MIN(1) CHOICE('NAME +
                          REQUIRED') PROMPT('PROGRAM NAME')
             PARM       KWD(OBJTYPE) TYPE(*NAME) DFT(*PGM) +
                          SPCVAL((*PGM)) CHOICE('NAME REQUIRED') +
                          PROMPT('OBJECT TYPE')
             PARM       KWD(SRCFIL) TYPE(QUAL) MIN(1) CHOICE('NAME +
                          REQUIRED') PROMPT('SOURCE FILE')
             PARM       KWD(MBRNAME) TYPE(*NAME) DFT(*PGM) +
                          SPCVAL((*PGM)) CHOICE('NAME REQUIRED') +
                          PROMPT('RETRIEVED MEMBER NAME')
QUAL:       QUAL       TYPE(*NAME) MIN(1) CHOICE('NAME REQUIRED')
             QUAL       TYPE(*NAME) MIN(1) CHOICE('NAME REQUIRED') +
                          PROMPT('LIBRARY')
