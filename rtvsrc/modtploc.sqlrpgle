      **************************************************************************
      *  (C) Copyright 2016, Credentials Solutions, LLC
      *  All rights reserved
      *
      *  * * * * * * *  CREDENTIALS CONFIDENTIAL  * * * * * * *
      *  This program is an unpublished work fully protected by the
      *  United States copyright laws and is considered a trade secret
      *  belonging to the copyright holder.
      *
      *\B Standard Backout Instructions:
      *\B   Re-compile prior version of program in PGMCOMPARE/RPG_PREV1
      *
      *  Prototype: /copy prototypes,TPLOC
      *
      *  Description : MOD to get records from TPLOC1
      *
      *  Written by  : TDR
      *  Date Written: 02/02/2016
      **************************************************************************
      *  Module Name. . . . . . .: MODTPLOC
      *
      *  Subprocedure names . . .: tploc_canEdit
      *                          : tploc_isUploadable
      *                          : tploc_getBatchCode
      *                          : tploc_getDescription
      *                          : tploc_getLocations
      *                          : tploc_getLocation
      *                          : tploc_getSchVal
      *                          : tploc_getTmWord
      *                          : tploc_getFormatCd
      **************************************************************************
      *  Change History:
      * -------- --- -----------------------------------------------------------
      * 05/07/19 KMK added getFormatCd
      * 05/03/17 TDR removed PRT from isUploadable check
      * 09/01/16 KMK added mode constant to isuploadable check
      * 02/18/16 TDR added tploc_canEdit, tploc_isUploadable, tploc_getBatchCode
      * 02/02/16 TDR New Module for TPLOC1
      **************************************************************************
     h nomain

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Files
     ftploc1    if   e           k disk    usropn extfile('CLLCFILE/TPLOC1')

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Constants
     d#false           c                   '0'
     d#true            c                   '1'

     dLOCATION_MAX     c                   10

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Data Structures

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Local Variables

       //***********************************************************************
       // tploc_canEdit - check if the location code can be changed
       //   Parameters:
       //     - Fice
       //     - Online Code
       //   Returns:
       //     - true if the location can be changed
       //     - false if it cannot be changed
       //***********************************************************************
       dcl-proc tploc_canEdit export;
       dcl-pi tploc_canEdit ind;
          dcl-parm fice char(6) const;
          dcl-parm onlineCode char(1) const;
       end-pi;

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Constants

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Data Structures

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Local Variables
     dreturnValue      s               n

      **************************************************************************
      * Start of Processing
      **************************************************************************
       if not %open(tploc1);
          open tploc1;
       endif;

       chain (fice:onlineCode) tplocrec;

       close tploc1;

       if %found(tploc1);
          if ledt_loc = 'Y';
             returnValue = #true;
          else;
             returnValue = #false;
          endif;
       else;
          returnValue = #false;
       endif;

       return returnValue;

       end-proc;

       //***********************************************************************
       // tploc_isUploadable - check if the locations transcipts are uploaded
       //   Parameters:
       //     - Fice
       //     - Online Code
       //     - PDF or ERM constant
       //   Returns:
       //     - true if the location is uploaded for either PDF or ERM
       //     - false if not uploadable
       //***********************************************************************
       dcl-proc tploc_isUploadable export;
       dcl-pi tploc_isUploadable ind;
          dcl-parm fice char(6) const;
          dcl-parm onlineCode char(1) const;
          dcl-parm mode char(3) const;
       end-pi;

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Constants

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Data Structures

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Local Variables
     dreturnValue      s               n

      **************************************************************************
      * Start of Processing
      **************************************************************************
       if not %open(tploc1);
          open tploc1;
       endif;

       chain (fice:onlineCode) tplocrec;

       close tploc1;

       if %found(tploc1);
          select;
          when ltm_upl_cd = 'Y';
             returnValue = #true;
          when ltm_upl_cd = ' ';
             returnValue = #false;
          when ltm_upl_cd = 'A' and mode = 'PDF';
             returnValue = #true;
          other;
             returnValue = #false;
          endsl;
       else;
          returnValue = #false;
       endif;

       return returnValue;

       end-proc;

       //***********************************************************************
       // tploc_getBatchCode - get the batch code for the transcript location
       //   Parameters:
       //     - Fice
       //     - Online Code
       //   Returns:
       //     - lbatchcd from the location record
       //***********************************************************************
       dcl-proc tploc_getBatchCode export;
       dcl-pi tploc_getBatchCode char(1);
          dcl-parm fice char(6) const;
          dcl-parm onlineCode char(1) const;
       end-pi;

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Constants

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Data Structures

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Local Variables
     dreturnValue      s              1a

      **************************************************************************
      * Start of Processing
      **************************************************************************
       if not %open(tploc1);
          open tploc1;
       endif;

       chain (fice:onlineCode) tplocrec;

       close tploc1;

       if %found(tploc1);
          returnValue = lbatchcd;
       else;
          returnValue = *blanks;
       endif;

       return returnValue;

       end-proc;

       //***********************************************************************
       // tploc_getDescription - get the description from the location record
       //   Parameters:
       //     - Fice
       //     - Online Code
       //   Returns:
       //     - ldescrp from the location record
       //***********************************************************************
       dcl-proc tploc_getDescription export;
       dcl-pi tploc_getDescription char(50);
          dcl-parm fice char(6) const;
          dcl-parm onlineCode char(1) const;
       end-pi;

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Constants

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Data Structures

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Local Variables
     dtms_onlin        s              1a
     dreturnValue      s             50a
     didx              s              5p 0
     daltInd           s               n

      **************************************************************************
      * Start of Processing
      **************************************************************************
       if not %open(tploc1);
          open tploc1;
       endif;

       tms_onlin = onlineCode;
       altInd = #false;
       idx = %scan(tms_onlin:'1234567890');
       if idx = 0;
          altInd = #true;
          select;
          when tms_onlin = 'A';
             tms_onlin = '1';
          when tms_onlin = 'B';
             tms_onlin = '2';
          when tms_onlin = 'C';
             tms_onlin = '3';
          endsl;
       endif;

       chain (fice:tms_onlin) tplocrec;

       close tploc1;

       if %found(tploc1);
          if altInd;
             returnValue = laltdescrp;
          else;
             returnValue = ldescrp;
          endif;
       else;
          returnValue = 'No Window Title Setup';
       endif;

       return returnValue;

       end-proc;

       //***********************************************************************
       // tploc_getLocation - get a single location record
       //   Parameters:
       //     - Fice
       //     - Online Code
       //   Returns:
       //     - TPLOC1 record
       //***********************************************************************
       dcl-proc tploc_getLocation export;
       dcl-pi tploc_getLocation likerec(tplocrec);
          dcl-parm fice char(6) const;
          dcl-parm onlineCode char(1) const;
       end-pi;

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Constants

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Data Structures
     dreturnDs         ds                  likerec(tplocrec)

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Local Variables

      **************************************************************************
      * Start of Processing
      **************************************************************************
       if not %open(tploc1);
          open tploc1;
       endif;

       chain (fice:onlineCode) tplocrec returnDs;

       close tploc1;

       return returnDs;

       end-proc;

       //***********************************************************************
       // tploc_getLocations - get all of the locations for a fice
       //   Parameters:
       //     - Fice
       //     - Number of Locations Returned
       //   Returns:
       //     - All TPLOC1 records for fice
       //***********************************************************************
       dcl-proc tploc_getLocations export;
       dcl-pi tploc_getLocations likerec(tplocrec) dim(LOCATION_MAX);
          dcl-parm fice char(6) const;
          dcl-parm numLocations zoned(3);
       end-pi;

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Constants

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Data Structures
     dreturnDs         ds                  likerec(tplocrec) dim(LOCATION_MAX)

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Local Variables
     dsql#rows         s             10i 0
     dsqlNum           s             10i 0

      **************************************************************************
      * Start of Processing
      **************************************************************************
          sqlNum = LOCATION_MAX;

          exec sql declare locationCur cursor for
             SELECT *
               FROM cllcfile/tploc1
              WHERE lfice = :fice
              ORDER BY ltms_onlin;

          exec sql open locationCur;

          exec sql fetch locationCur for :sqlNum rows into :returnDs;

          exec sql get diagnostics :sql#rows = row_count;

          numLocations = sql#rows;

          exec sql close locationCur;

          return returnDs;

       end-proc;

       //***********************************************************************
       // tploc_getSchVal - get the TMS Word from the location record
       //   Parameters:
       //     - Fice
       //     - Online Code
       //   Returns:
       //     - lsch_val from the location record
       //***********************************************************************
       dcl-proc tploc_getSchVal export;
       dcl-pi tploc_getSchVal char(30);
          dcl-parm fice char(6) const;
          dcl-parm onlineCode char(1) const;
       end-pi;

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Constants

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Data Structures

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Local Variables
     dreturnValue      s             30a

      **************************************************************************
      * Start of Processing
      **************************************************************************
       if not %open(tploc1);
          open tploc1;
       endif;

       chain (fice:onlineCode) tplocrec;

       close tploc1;

       if %found(tploc1);
          returnValue = lsch_val;
       else;
          returnValue = 'N/A';
       endif;

       return returnValue;

       end-proc;

       //***********************************************************************
       // tploc_getTmWord - get the TMS Word from the location record
       //   Parameters:
       //     - Fice
       //     - Online Code
       //   Returns:
       //     - ltm_word from the location record
       //***********************************************************************
       dcl-proc tploc_getTmWord export;
       dcl-pi tploc_getTmWord char(15);
          dcl-parm fice char(6) const;
          dcl-parm onlineCode char(1) const;
       end-pi;

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Constants

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Data Structures

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Local Variables
     dreturnValue      s             15a

      **************************************************************************
      * Start of Processing
      **************************************************************************
       if not %open(tploc1);
          open tploc1;
       endif;

       chain (fice:onlineCode) tplocrec;

       close tploc1;

       if %found(tploc1);
          returnValue = ltm_word;
       else;
          returnValue = 'N/A';
       endif;

       return returnValue;

       end-proc;

       //***********************************************************************
       // tploc_getFormatCd - get the FormatCd from the location record
       //   Parameters:
       //     - Fice
       //     - Online Code
       //   Returns:
       //     - lformatcd from the location record
       //***********************************************************************
       dcl-proc tploc_getFormatCd export;
       dcl-pi tploc_getFormatCd char(1);
          dcl-parm fice char(6) const;
          dcl-parm onlineCode char(1) const;
       end-pi;

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Constants

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Data Structures

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Local Variables
     dreturnValue      s              1a

      **************************************************************************
      * Start of Processing
      **************************************************************************
       if not %open(tploc1);
          open tploc1;
       endif;

       chain (fice:onlineCode) tplocrec;

       close tploc1;

       if %found(tploc1);
          returnValue = lformatcd;
       else;
          returnValue = '-';
       endif;

       return returnValue;

       end-proc;
