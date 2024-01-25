      ********************************************************************
      *  (C) Copyright 2005 Credentials Solutions, LLC
      *  All rights reserved
      *
      *  * * * * * * *  CREDENTIALS CONFIDENTIAL  * * * * * * *
      *  This program is an unpubFished work fully protected by the
      *  United States copyright laws and is considered a trade secret
      *  belonging to the copyright holder.
      *
     *\B Standard Backout Instructions:
     *\B   Re-compile prior version of program in PGMCOMPARE/RPG_PREV1
      *
      *  Module Name : SERVER_GT
      *
      *  Description : DTAQ Program to provide Add/Retrieve Capability
      *                to GAORDER3 for all Applications
      *
      *
      *  Written by  : KMK
      *  Date Written: 06/03/2005
      ********************************************************************
      * Change History
      * -------- --- --------------------------------------------------
      * 07/12/19 RPR Renamed XTEXT_280V to xtextv in GAORDER3
      * 07/09/19 RPR Expanding XTEXT_280V to 500.
      * 05/08/19 LCC Trimming #GT_textv to remove whitespace
      * 05/12/10 KMK Removed old commented code from obligation upgrade
      *              from 8A to 24A long fields.
      * 04/02/10 MRB Updated comment retreive to use fice + order# instead
      *              of just order#, Addition of Muni comments, U + user id,
      *              means order# is no longer unique
      *              Also added Delete comment command - 'GD'
      ********************************************************************
     Hoption(*NOXREF:*NODEBUGIO)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('CIBINDDIR':'QC2LE')
     Fgaorder3  uf a e           k disk    usropn
     F*SG01                                extfile('CLLCFILE/GAORDER3')
     fcitp_obg1 if   e           k disk    usropn
     F*SG01                                extfile('CLLCFILE/CITP_OBG1')
      ************************************************************************

      *  program status data structure
     d/copy rpgcopy,cllcpsds
     d/copy rpgcopy,$srvrgt_ds
     d bufin                   1    600a

      * - - - - - - - - - - - - - - - - - - -
      *     local variables
      * - - - - - - - - - - - - - - - - - - -
     d                 ds
     dcycle_date               1      8S 0 inz
     dcycle_time               9     14S 0 inz

     d                 ds
     d td14                    1     14  0
     d curtime                 1      6  0
     d td14mmdd                7     10  0
     d td14yyyy               11     14  0

     ddate8ds          ds
     d today                   1      8  0
     d date8yyyy               1      4  0
     d date8mmdd               5      8  0
     Dadate8yyyy               1      4a
     Dadate8mm                 5      6a
     Dadate8dd                 7      8a
     Dadate8_fld               1      8a

     Ddsply_date       ds            10
     D dsply_mm                1      2
     D dsply_s1                3      3
     D dsply_dd                4      5
     D dsply_s2                6      6
     D dsply_yyyy              7     10

      * - - - - - - - - - - - - - - - - - - -
      *   local fields
      * - - - - - - - - - - - - - - - - - - -

     dallocate         ds            70
     d                         1     25    inz('ALCOBJ OBJ((CI_PROCESS/PC')
     d                        26     50    inz('_SRVR_GT *DTAARA *EXCL)) ')
     d                        51     70    inz('WAIT(0)')

     d #false          s              1a   inz('0')
     d #true           s              1a   inz('1')

     d ei              s              5p 0 INZ
     d errs            s             90A   DIM(40)

     d have_txt        s              1a
     d have_obs        s              1a
     d idx             s              3p 0 inz
     d ntimes          s              2s 0 inz
     d ordtxt_dat      s              8a
     d ordtxt_tim      s              6a
     d ordtxt_usr      s             10a
     d ordtxt_typ      s              1a
     d kntimes         s              2s 0 inz

     d the_cod3        s              3s 0 inz
     d the_code        s              1a
     d the_fice        s              6a
     d the_recid       s              2a

     d                 ds
     d wob_code2                     24a
     d wob8_3a                        3a   dim(8) overlay(wob_code2:1)


      *-------------------------------------------------------------------
      *  if we had a fatal error handled by *PSSR write a record and end
      *-------------------------------------------------------------------
    c                   IF        panel       = '#FATALERR '
     c                   exsr      wrtpanel
     c                   goto      end_pgm
    c                   ENDIF

      *----------------------------------------------------------
      *    If INZSR allocate failed, end program
      *----------------------------------------------------------

    c                   IF        *in85      = *on
     c                   eval      *inlr      = *on
     c                   goto      end_pgm
    c                   ENDIF

      *----------------------------------------------------------
      *    Top of program loop - get data q input
      *----------------------------------------------------------

     c     top           tag

     c                   call      'QRCVDTAQ'
     c                   parm                    #GT_name_i
     c                   parm                    #GT_lib_i
     c                   parm                    #GT_len_i
     c                   parm                    #GT_dqin
     c                   parm      -1            #GT_wait

      *-------------------------------------------------------------------
      *    Get cycle date and time
      *-------------------------------------------------------------------
     c                   time                    td14
     c                   move      td14yyyy      date8yyyy
     c                   move      td14mmdd      date8mmdd
     c                   eval      cycle_date  = today
     c                   eval      cycle_time  = curtime

      *-------------------------------------------------------------------
      *    Process the Request
      *-------------------------------------------------------------------

    c                   SELECT

      *                                      GAORDER3 - Get GAORDER3
    c                   WHEN      #GT_func    = 'GG'
     c                   exsr      get_ga3_1

      *                                      GAORDER3 - Add GAORDER3
    c                   WHEN      #GT_func    = 'GA'
     c                   exsr      add_ga3

      *                                      GAORDER3 - Read Comments
    c                   WHEN      #GT_func    = 'GC'
     c                   exsr      get_ga3

      *                                      GAORDER3 - DELETE Comments
    c                   WHEN      #GT_func    = 'GD'
     c                   exsr      delete_ga3

      *                                      File Close Request
    c                   WHEN      #GT_func    = 'FC'
     c                   close     *all

      *                                      End Program
    c                   WHEN      #GT_func    = 'EP'
     c                   goto      end_pgm

    c                   ENDSL

      *-------------------------------------------------------------------
      *    return the record to the caller
      *-------------------------------------------------------------------

     c                   eval      #GT_dq_kyo  = #GT_dq_kyi

SGTSTC                   Eval      #GT_len_o = %len(#GT_dqout)
     c                   call      'QSNDDTAQ'
     c                   parm                    #GT_name_o
     c                   parm                    #GT_lib_o
     c                   parm                    #GT_len_o
     c                   parm                    #GT_dqout
     c                   parm      26            #GT_keylen
     c                   parm                    #GT_dq_kyo

     c                   clear                   #GT_dqout
     c                   goto      top

     c     end_pgm       tag
     c                   close     *all
     c                   seton                                        LR


      ********************************************************************
      *     *INZSR:  initialization subroutine
      ********************************************************************
     csr   *inzsr        begsr

     c                   call      'QCMDEXC'                            85
     c                   parm                    allocate
     c                   parm      70            cmdlen           15 5

     c*                  call      'QCLRDTAQ'
     c*                  parm                    #GT_name_o
     c*                  parm                    #GT_lib_o

     c*                  call      'QCLRDTAQ'
     c*                  parm                    #GT_name_i
     c*                  parm                    #GT_lib_i

     c     key_inst5     klist
     c                   kfld                    the_fice
     c                   kfld                    the_recid
     c                   kfld                    the_code

     c     txtadd_key    klist
     c                   kfld                    #GT_fice
     c                   kfld                    #GT_ord#
     c                   kfld                    kntimes

     c     txtget_key    klist
     c                   kfld                    #GT_fice
     c                   kfld                    #GT_ord#

     csr                 endsr

     d/copy rpgcopy,$PSSR_SR

     ********************************************************************
     *    add_ga3     GAORDER3 "ADD" record
     *                Adds Records xntimes - 1
     ********************************************************************
    csr   add_ga3       begsr

    c                   IF        not %OPEN(gaorder3)
     c                   open      gaorder3
    c                   ENDIF

     c                   eval      kntimes     = 99
     c                   eval      ntimes      = 99

      *---Find last record in order to get next ntimes value
     c     txtadd_key    setll     gaorder3
    c                   IF        NOT %EQUAL
     c                   goto      add_rec
    c                   ELSE
     c                   eval      kntimes     = 1
     c     txtadd_key    setll     gaorder3
     c                   read(n)   textrec
     c                   eval      ntimes      = xntimes - 1

     c     add_rec       tag
     c                   eval      xntimes     = ntimes
     c                   eval      xfice       = #GT_fice
     c                   eval      xapplid     = #GT_applid
     c                   eval      xorder#     = #GT_ord#
     c                   eval      xdate       = cycle_date
     c                   eval      xtime       = cycle_time
     c                   eval      xrec_type   = #GT_rectyp
     c                   eval      xobl_code2  = #GT_ob24
     c                   eval      xtextv  = %trim(#GT_textv)
     c                   eval      xenteredby  = #GT_entby
     c                   write     textrec
     c                   eval      #GT_status  = 'OK'
    c                   ENDIF

     c                   move      #GT_ord#      #GT_ord#_o

     csr                 endsr

     ********************************************************************
     *    delete_ga3  Delete comments from GAORDER3
     ********************************************************************
    csr   delete_ga3    begsr

    c                   IF        not %OPEN(gaorder3)
     c                   open      gaorder3
    c                   ENDIF

     c     txtget_key    setll     textrec
    c                   IF        %EQUAL
    c                   dow       NOT %EOF(gaorder3)
     c     txtget_key    reade     textrec
    c                   if        %EOF
     c                   leave
    c                   endif

     c                   delete    textrec
    c                   enddo
     c                   eval      #GT_status  = 'OK'
    c                   ENDIF

     c                   move      #GT_ord#      #GT_ord#_o

     csr                 endsr

     ********************************************************************
     *    get_ga3_1   GAORDER3 "GET" records only
     *                gets MOST RECENT RECORD ONLY
     ********************************************************************
    csr   get_ga3_1     begsr

     c                   eval      have_txt    = #FALSE
     c                   eval      have_obs    = #FALSE
     c                   clear                   wob_code2

    c                   IF        not %OPEN(gaorder3)
     c                   open      gaorder3
    c                   ENDIF

     c     txtget_key    setll     textrec
    c                   IF        %EQUAL
     c     read_again    tag
     c     txtget_key    reade(n)  textrec
     c                   if        %EOF
     c                   goto      e_ga3_1
     c                   endif

    c                   if        %FOUND
     c
      *   Read first record that is not a comment
    c                   if        xrec_type   = 'C'
     c                   goto      read_again
    c                   endif

     c                   movel     xdate         ordtxt_dat
     c                   movel     xtime         ordtxt_tim
     c                   movel     xenteredby    ordtxt_usr
     c                   movel     xrec_type     ordtxt_typ

      * Save Text in return array
     c                   eval      have_txt    = #TRUE
    c                   if        xtextv <> *blanks
     c                   eval      #GT_ary(1)  = ordtxt_dat +
     c                             ordtxt_tim + ordtxt_usr +
     c                             ordtxt_typ +
     c                             %trim(xtextv)
    c                   endif
     c                   eval      wob_code2   = xobl_code2

    c                   endif
     c                   eval      #GT_status  = 'OK'
    c                   ENDIF

      * If we have obligation blocks, save them in index 101-108
    c                   IF        wob_code2  <> *blanks
     c                   eval      have_obs    = #TRUE
     c                   eval      the_fice    = xfice
    c     1             DO        8             idx
    c                   if        wob8_3a(idx)<> *blanks
     c                   move      wob8_3a(idx)  the_cod3
     c                   exsr      get_OB3
     c                   eval      #GT_ary(100+idx) = ordtxt_dat +
     c                             ordtxt_tim + ordtxt_usr + ordtxt_typ +
     c                             ohold_text
    c                   endif
    c                   ENDDO
    c                   ENDIF

     c     e_ga3_1       tag

     c                   move      #GT_ord#      #GT_ord#_o
     c                   if        have_obs    = #TRUE
     c                   eval      #GT_ob_flg  = 'Y'
     c                   endif

     csr                 endsr

     ********************************************************************
     *    get_ga3     GAORDER3 "GET" records for an order
     ********************************************************************
    csr   get_ga3       begsr

     c                   eval      have_txt    = #FALSE
     c                   eval      have_obs    = #FALSE
     c*                  clear                   wob_codes
     c                   clear                   wob_code2

    c                   IF        not %OPEN(gaorder3)
     c                   open      gaorder3
    c                   ENDIF

     c     txtget_key    setll     textrec
    c                   IF        %EQUAL
     c                   eval      idx         = 0
    c                   dow       NOT %EOF(gaorder3)
     c                   eval      idx         = idx + 1
     c     txtget_key    reade(n)  textrec
    c                   if        %EOF
     c                   leave
    c                   endif

     c                   movel     xdate         ordtxt_dat
     c                   movel     xtime         ordtxt_tim
     c                   movel     xenteredby    ordtxt_usr
     c                   movel     xrec_type     ordtxt_typ

      * Save Text in return array
     c                   eval      have_txt    = #TRUE
    c                   if        xtextv <> *blanks
     c                   eval      #GT_ary(idx) =  ordtxt_dat +
     c                             ordtxt_tim + ordtxt_usr +
     c                             ordtxt_typ +
     c                             %trim(xtextv)
    c                   endif
     c                   eval      wob_code2   = xobl_code2

    c                   enddo
     c                   eval      #GT_status  = 'OK'
    c                   ENDIF

     c                   move      #GT_ord#      #GT_ord#_o

     csr                 endsr

      /free
       //*****************************************************************
       //   get_OB3:  - Get Obliation Block from CITP_OBG1
       //*****************************************************************
       begsr get_OB3;

      if not %open(citp_obg1);
          open citp_obg1;
      endif;

       chain (xfice:the_cod3) oblg_rec;
      if not %FOUND(citp_obg1);
          ohold_text  = 'Obligation block not defined';
      endif;

       endsr;

      /end-free

      ********************************************************************
      *     wrtpanel - dummy subroutine
      ********************************************************************
     csr   wrtpanel      begsr
     c                   movel     ' '           panel            10
     c                   movel     ' '           hscrn_next        3
     csr                 endsr
