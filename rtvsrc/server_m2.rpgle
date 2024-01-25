      ********************************************************************
      *
      *  (C) Copyright 2009 Credentials Inc.
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
      *  Module Name : SERVER_M2
      *
      *  Description : Server Program to provide Master File data to
      *                various applications via dataQ interface
      *
      *
      *  Written by  : JJG
      *  Date Written: 05/26/2009
      ********************************************************************
      * 07/09/16 JJG added function of 'CQ' to clear outbound data queue
      *              so orphaned messages don't cause a blowup
      * 05/02/15 JJG added function of 'HB' to do a heartbeat reply so we
      *              can successfully end all instances of SERVER_M2 caused
      *              by external vulnerability scanners
      * 03/26/15 KMK created EX to check existence of a file passed in
      * 03/19/15 JJG change pgm to always close GATEWAY1 on every cycle
      *              so DAILY_STOP can reorg file.  All the SERVER_M2 jobs
      *              launched during vuln scans prevent the *M2CLOSE from
      *              working successfully in DAILY_STOP
      * 10/28/14 JJG change <br> to <p> in cimessage handler
      * 10/11/14 JJG add function 'MR' to retrieve CIMESSAG1 records
      * 05/09/14 JJG added block(*NO) to ccn,ssn,secmontr1 DBs
      *              added close func for ssn and ccamontr1 dbs
      * 06/21/12 KMK changed auth upload submitted user
      * 06/03/12 JJG added RTGDTA(CLS35) to do_sj SBMJOB command so
      *              auth upload image convert will run better
      * 03/11/12 JJG added code for trantype = SJ perform SBMJOB function
      *              The SJ function is restricted to specific commands and
      *              specific users and source functions
      * 07/19/11 JJG added code for trantype = S1 to write SECMONTR1 rec
      * 07/19/11 JJG added code for trantype = S1 to write SECMONTR1 rec
      * 05/14/11 JJG rewrote Kiosk lkogic to return two arrays instead
      *              of one big text field.  Reduced array sizes from
      *              200 down to 50
      * 05/13/11 JWG converted to rotating array
      * 05/12/11 JWG added get_IP routine to pgm to retrieve kiosk ips
      *              added init_ip to create kiosk arrays for retrieval
      * 07/16/10 JJG added new do_GW routine to pgm to write a gateway1
      *              record for epay storage of tokens
      * 07/11/10 JJG added cibinddir and actgrp(*NEW)
      * 07/05/10 JJG Changed BNDDIR so compiles with 14
      * 06/26/10 JJG add function to move a source member from cllcpgm
      *                  to working file in cllcwwwpgm
      * 12/20/09 JJG added Routine DO_CM to log CCS access to Credit card
      *                    in the CCA_MONTR1 database.  We do not store
      *                    an actual card number just the fact that the
      *                    operator might have seen the card on Order#
      ********************************************************************
     Hoption(*NOXREF:*NODEBUGIO)
     H DFTACTGRP(*NO) ACTGRP(*CALLER)
     H BNDDIR('CIBINDDIR':'QC2LE')

     ftpfields1 if   e           k disk    usropn
     f                                     extfile('CLLCFILE/TPFIELDS1')

     fcimessag1aif   e           k disk    usropn
     f                                     extfile('CIDEVFILE/CIMESSAG1A')

     fkioskip1  if   e           k disk    usropn
     f                                     extfile('CLLCFILE/KIOSKIP1')

     Fssnmontr1 o    e           k disk    usropn block(*no)
     F                                     extfile('CLLCFILE/SSNMONTR1')

     Fccamontr1 o    e           k disk    usropn block(*no)
     F                                     extfile('CLLCFILE/CCAMONTR1')

     Fgateway1  o    e           k disk    usropn
     F                                     extfile('CLLCFILE/GATEWAY1')

     Fsecmontr1 o    e           k disk    usropn block(*no)
     F                                     extfile('CLLCFILE/SECMONTR1')

      /copy prototypes,dataqueues
      /copy prototypes,exists_ifs
     d/copy rpgcopy,cllcpsds
     d/copy rpgcopy,dvdtaara
     d/copy rpgcopy,cidtaara
     d/copy rpgcopy,$srvrm2_ds
     d bufin                   1    100a

     **-- Run system command:
     D system          Pr            10i 0 ExtProc( 'system' )
     D  command                        *   Value  Options( *String )
     D command         s            500a   inz
     D dec#            S             17p15
     D random#         S              5P 0 inz
     D r#_1sttime      S              1a   inz

      * - - - - - - - - - - - - - - - - - - -
      *     local variables
      * - - - - - - - - - - - - - - - - - - -

     d                 ds
     damt_alpha                1      4a
     damt_packed               1      4p 2

     d                 ds
     dcycle_date               1      8S 0 inz
     dcycle_time               9     14S 0 inz

      *---------------------------------------------------------------
      *   KIOSKIP1 Record Retrieval Fields
     d ar_kskkey       s              8a   dim(50) inz
     d ar_ksk_ip       s            450a   dim(50) inz
     d ar_ksk_ctl      s             30a   dim(50) inz

     d kiosk_info      ds
     d area_ips                     450a   inz
     d area_ctl                      30a   inz
     d ar_ips                        15a   overlay(area_ips:1) dim(30)
     d ar_ctl                         1a   overlay(area_ctl:1) dim(30)

     dlidx_kiosk       s              3p 0 inz
     dkiosk_key        s              8a   inz

      * counter for rotating array - kiosk only
     dksk_cnt          s              3p 0 inz

      *---------------------------------------------------------------
      *   TPFIELDS1 Record Retrieval Fields

     didx_tpkeys       s              3p 0 inz
     dar_tpkeys        s              7a   inz dim(4)
     dget_tpkey        s              7a   inz

      *  message retrieval data field
     d mr_text         s           6000a

     dtpflds1_dz     e ds                  extname(tpfields1)
     d                                     prefix(Z:1)
     d                                     occurs(4)

     dtpflds1_dy     e ds                  extname(tpfields1)
      * - - - - - - - - - - - - - - - - - - -
      *   local fields
      * - - - - - - - - - - - - - - - - - - -

     dallocate         ds            70
     d                         1     25    inz('ALCOBJ OBJ((CI_PROCESS/PC')
     d                        26     50    inz('_SRVR_M2 *DTAARA *EXCL)) ')
     d                        51     70    inz('WAIT(0)')

     d exist_file      s            512A
     d exist_rc        s              1A

     d i1              s              5p 0 inz
     d n30             s              3p 0 inz
     d sm_count        s              5p 0 inz

     d                 ds
     d values                        36a   inz('ABCDEFGHIJKLMNOPQRSTUVWXYZ+
     d                                     1234567890')
     d vals                           1a   dim(36) overlay(values)

     d work_fice       s              6a   inz
     d work_lang       s              1a   inz

     Daddlibl2         ds            70
     D                         1     25    inz('ADDLIBLE LIB(CLLCPGM)    ')
     D                        26     70    inz('POSITION(*LAST)')

     Daddlibl4         ds            70
     D                         1     25    inz('ADDLIBLE LIB(CLLCWWWPGM) ')
     D                        26     70    inz('POSITION(*LAST)')

      /free
       if *in85 = *on;    // If INZSR allocate failed, end program
          *inlr = *on;
       else;
          exsr do_detail;
       endif;

       close *all;


       // -------------------------------------------------------
       //  Detail Subroutine
       // -------------------------------------------------------

       begsr do_detail;
       dow *inlr <> *on;

       callp qrcvdtaq(#M2_name_i:#M2_lib_i:#M2_len_i:#M2_dqin:#M2_wait);

       //  Get cycle date and time

       cycle_date = %dec(%date(): *ISO);
       cycle_time = %dec(%time(): *ISO);

       //  Process the Request

       select;

       when #M2_func = 'TP';             // tpfields1 retrieval
         exsr get_tp;                    // most frequent so make it first

       when #M2_func = 'MR';             // CIMESSAG1 Retrieval Request
         exsr get_mr;                    // 2nd most frequent so make it second

       when #M2_func = 'IP';             // kiosk ip retrieval
          if #m2_fice = 'RELOAD';
             clear ar_kskkey;
             clear ar_ksk_ip;
             clear ar_ksk_ctl;
             ksk_cnt = 0;
             iter;
          else;
             exsr get_ip;
          endif;

       when #M2_func = 'SJ';             // SBMJOB function requested
         exsr do_sj;
         iter;

       when #M2_func = 'CM';             // CC Monitor Audit Request
         exsr do_cm;                     // from CCS auth form function
         iter;                           // no data going back

       when #M2_func = 'GW';             // Request to write a GATEWAY1
         exsr do_gw;                     // record for EPay-type charge
         iter;                           // no data going back

       when #M2_func = 'MM';             // move a member
         exsr mov_mbr;

       when #M2_func = 'SM';             // SSN Monitor Audit Request
         sm_count += 1;
         exsr do_sm;
         iter;

       when #M2_func = 'S1';             // SECMONTR1 record request
         exsr do_s1;
         iter;

       when #M2_func = 'FC';             // close files request
         close *all;
         clear ar_tpkeys;
         for i1 = 1 to 4;
            %occur(tpflds1_dz) = i1;
            clear  tpflds1_dz;
         endfor;
         iter;                           // no data going back

       when #M2_func = 'EX';             // Check if file exists
         exsr chk_exists;

       when #M2_func = 'HB';             // Heartbeat check
         #M2_status = 'AL';
         #M2_result = 'I AM ALIVE';

       when #M2_func = 'CQ';             // clear outbound DtaQ
         callp qclrdtaq(#M2_name_o:#M2_lib_o);
         iter;

       when #M2_func = 'EP';             // end program
         *inlr = *on;
         iter;

       endsl;

       #M2_dq_kyo  = #M2_dq_kyi; //  return the data to the caller

       callp qsnddtaq(#M2_name_o:#M2_lib_o:#M2_len_o:#M2_dqout:
                      #M2_keylen:#M2_dq_kyo);
       clear #M2_dqout;
       enddo;
       endsr;

       // ****************************************************************
       //    *INZSR:  initialization subroutine
       // ****************************************************************
       begsr *inzsr;
       #M2_wait = -1;
       *in85 = *off;

       monitor;
       // system(%trim(allocate));
       on-error *all;
          *in85 = *on;
       endmon;

       monitor;
       system(%trim(addlibl2));
       system(%trim(addlibl4));
       on-error *all;
       endmon;

       endsr;

       // ****************************************************************
       //    chk_exists:  Check if a file exists
       // ****************************************************************
       begsr chk_exists;

       exist_file = %trim(#m2_filenm);
       callp exists_ifs(exist_file:exist_rc);

       // File already exists, delete existing one first
       if exist_rc  = '1';
          #M2_status = 'OK';
       else;
          #M2_status = 'NO';
       endif;

       endsr;

       // ****************************************************************
       //    do_CM      Store a CCA audit monitor request
       // ****************************************************************
       begsr do_cm;
       if not %open(ccamontr1);
          open ccamontr1;
       endif;


       auserid   = #M2_ccausr;
       areason   = #M2_ccarc1;
       aviewdate = %dec(%date(): *ISO);
       aviewtime = %dec(%time(): *ISO);
       acardnume = #M2_ccacc#;
       aorder#   = #M2_ccaord;
       if #M2_ccatim = *blanks;
          #M2_ccatim = '000000';
       endif;
       atime     = %int(#M2_ccatim);
       ajobname  = #M2_jobn_i;
       ajobuser  = #M2_jobu_i;
       ajob#     = #M2_job#_i;

       write cca_log;
       close ccamontr1;

       // CM' request is an asyncronous request to SERVER_M2
       // The calling program does not wait for a response

       endsr;

       // ****************************************************************
       //    do_GW      Write a GATEWAY1 record for storing an EPay
       //               credit card transaction number
       // ****************************************************************
       begsr do_gw;
       if not %open(gateway1);
          open gateway1;
       endif;


       gorder#    = #M2_gword#;
       grectype   = 'C';
       gstatus    = 'C';

       amt_alpha  = #M2_gwamtp;
       gamt_total = amt_packed;

       gcardtype  = #M2_gwcard;
       gdate      = %dec(%date(): *ISO);
       gtime      = %dec(%time(): *ISO);
       gcc_au_tx# = #M2_gw_au#;
       gorbitl_ID = #M2_gwfice;
       gcc_cp_tx# = #M2_gw_cp#;

       write gateway;
       close gateway1;     // so DAILY_STOP can reorg

       // SM' request is an asyncronous request to SERVER_M2
       // The calling program does not wait for a response

       endsr;

       // ****************************************************************
       //    do_SJ      run SBMJOB command to start a batch job
       //               an SJ function is validated before being allowed
       //               to run.  The request must have come from a valid
       //               process ID.  The cmd string itself must contain a
       //               recognized and allowed iSeries command
       //
       //               All programs using this function must accept an
       //               additional parameter as the last parm on the CALL
       //               that will be added by this program to the parms on the
       //               that will be added by this program to the parms
       //               supplied in #M2_SJ_cmd.  THis last parm will be:
       //
       //               #M2_sj_key       (32 bytes)
       //                 #M2_sj_pid      3-bytes
       //                 #M2_sj_ord      9-bytes
       //                 #M2_sj_dat      8-bytes
       //                 #M2_sj_tim     12-bytes
       //               #M2_sj_cmd       (235-bytes)
       //
       //****************************************************************
       begsr do_sj;

       if #M2_sj_pid = 'CRI';    // Crop Image Process
       else;
          leavesr;
       endif;

       // no commands allowed, only call pgm stmts
       if %scan('CALL PGM(':#m2_sj_cmd:1) = 1;
       else;
          leavesr;
       endif;

       // positions 10-19 must be the program name (trailing spaces if needed)
       if %scan('AUTUPLSCNC':#m2_sj_cmd:10) = 10;
          command = 'SBMJOB CMD(' +
                    %trim(#m2_sj_cmd);
          n30 = %len(%trim(command));     // locate the last )
          %subst(command:n30:1) = ' ';    // remove the last )
          command = %trim(command) + ' ' + '''' + #M2_sj_key +
                    ''')) JOB(' + %trim(%subst(#m2_sj_cmd:10:10)) +
                    ') USER(CIBATCH) RTGDTA(CLS35)';
          system(%trim(command));
       else;
          leavesr;
       endif;

       endsr;

       // ****************************************************************
       //    do_SM      Store an SSN audit monitor request
       // ****************************************************************
       begsr do_sm;
       if not %open(ssnmontr1);
          open ssnmontr1;
       endif;


       afice     = #M2_fice;
       assn_enc  = #M2_ssnenc;
       auserid   = #M2_ssnusr;
       areason   = #M2_ssnrc1;
       areason2  = #M2_ssnrc2;
       aviewdate = %dec(%date(): *ISO);
       aviewtime = %dec(%time(): *ISO);
       aorder#   = #M2_ssnord;
       ajobname  = #M2_jobn_i;
       ajobuser  = #M2_jobu_i;
       ajob#     = #M2_job#_i;

       write ssn_log;
       close ssnmontr1;

       // SM' request is an asyncronous request to SERVER_M2
       // The calling program does not wait for a response

       endsr;

       // ****************************************************************
       //    do_S1      Write a SECMONTR1 record
       // ****************************************************************
       begsr do_s1;
       if not %open(secmontr1);
          open secmontr1;
       endif;

       clear secmonrec;

       sdate = %dec(%date(): *ISO);
       stime = %dec(%time(): *ISO);

       in *lock cidtaara;
       secmonseq += 1;
       out cidtaara;

       srecord#  = secmonseq;
       ssource   = #M2_s1_src;
       scategry  = #M2_s1_cat;
       sstatus   = #M2_s1_sta;
       suserid   = #M2_s1_usr;
       sjobname  = #M2_jobn_i;
       sjobuser  = #M2_jobu_i;
       sjobnbr   = #M2_job#_i;
       sprogram  = #M2_s1_pgm;
       slibrary  = #M2_s1_lib;
       sseverity = #M2_s1_sev;
       slogtype  = #M2_s1_lgt;
       sfice     = #M2_fice;
       sipv6_src = #M2_s1_ip;

       if sstatus = 'A';
          sreviewdby = '*AUTO';
          sreviewddt = sdate;
          sreviewdtm = stime;
       elseif sstatus = ' ';
          sstatus = 'O';
       endif;

       surl  = #M2_s1_url;
       stext = #M2_s1_txt;

       write secmonrec;
       close secmontr1;

       // S1 request is an asyncronous request to SERVER_M2
       // The calling program does not wait for a response

       endsr;

       // ****************************************************************
       //    get_IP     Get the Kiosk IP based on fice/applid passed
       // ****************************************************************
       begsr get_IP;

       kiosk_key = #m2_ipfice + #m2_ipappl;

       lidx_kiosk = %lookup(kiosk_key:ar_kskkey);
       if lidx_kiosk > 0;   // FICE/APPLID Kiosk data in arrays
          area_ips =  ar_ksk_ip(lidx_kiosk);
          area_ctl =  ar_ksk_ctl(lidx_kiosk);
       else;
          if not %open(kioskip1);
             open kioskip1;
          endif;
          clear area_ips;
          clear area_ctl;
          clear n30;
          setll (#m2_ipfice:#m2_ipappl) kioskip1;
          if %equal(kioskip1);
             reade (#m2_ipfice:#m2_ipappl) kioskrec;
             dow not %eof(kioskip1);
                if kenabled <> 'Y';     // do not fill if IP addr not enabled
                   reade (#m2_ipfice:#m2_ipappl) kioskrec;
                   iter;
                endif;
                n30 += 1;
                if n30 < 31;
                   ar_ips(n30) = kkiosk_ip;
                   ar_ctl(n30) = kkioskctrl;
                endif;
                reade (#m2_ipfice:#m2_ipappl) kioskrec;
             enddo;

             // Add List of Kiosk IPs for FICE/APPLID to rotating array
             ksk_cnt += 1;
             if ksk_cnt > %elem(ar_kskkey);
                ksk_cnt = 1;
             endif;
             ar_kskkey(ksk_cnt) = kiosk_key;
             ar_ksk_ip(ksk_cnt) = area_ips;
             ar_ksk_ctl(ksk_cnt) = area_ctl;
          endif;
       endif;

       #M2_status = 'OK';
       #M2_result = area_ips + area_ctl;

       endsr;

       // ****************************************************************
       //    get_MR     Get the CIMESSAG1 records for a requested
       //               see if the record has been set up in the
       // ****************************************************************
       begsr get_MR;
       if not %open(cimessag1a);
          open cimessag1a;
       endif;

       clear mr_text;
       setll (#M2_fice:#M2_mrappl:#M2_mrscrn:#M2_mrsect:#M2_mrlang) messagerec;

       if not %equal(cimessag1a);
          #M2_status = 'NM';           // no messages
          #M2_fice_o = #m2_fice;
          leavesr;
       endif;

       // OK, we have records so read them and concatenate

       reade (#M2_fice:#M2_mrappl:#M2_mrscrn:#M2_mrsect:#M2_mrlang) messagerec;
       dow not %eof(cimessag1a);
        mr_text = %trim(mr_text) + %trim(jm_text) + '<p>';
        reade (#M2_fice:#M2_mrappl:#M2_mrscrn:#M2_mrsect:#M2_mrlang) messagerec;
       enddo;

       #M2_status = 'OK';
       #M2_result = mr_text;
       #M2_fice_o = #m2_fice;
       endsr;

       // ****************************************************************
       //    get_TP     Get the TPFIELDS1 rec for a fice/language
       //               see if the record has been set up in the
       //               ds_tpkeys occurs data structure else
       //               set it up
       // ****************************************************************
       begsr get_TP;
       if not %open(tpfields1);
          open tpfields1;
       endif;

       work_fice = #m2_fice;
       work_lang = #m2_lang;
       get_tpkey = work_fice + work_lang;

       i1 = %lookup(get_tpkey:ar_tpkeys);
       if i1 > 0;
         %occur(tpflds1_dz) = i1;
       else;
         idx_tpkeys += 1;
         if idx_tpkeys > 4;
            idx_tpkeys = 1;
         endif;
         i1 = idx_tpkeys;
         ar_tpkeys(i1) = get_tpkey;

         %occur(tpflds1_dz) = i1;

         chain ('000000':work_lang) tpfields;
         if %found(tpfields1);
            tpflds1_dz = tpflds1_dy;
         else;
            clear tpflds1_dz;
         endif;

         if work_fice <> '000000';
            chain (work_fice:work_lang) tpfields;
            if %found;
               exsr dotp_flds;
            endif;
         endif;
       endif;

       #M2_status = 'OK';
       #M2_result = tpflds1_dz;
       #M2_fice_o = #m2_fice;
       endsr;

       // ****************************************************************
       //    mov_mbr    Move source member from cllcpgm to cllcwwwpgm
       // ****************************************************************
       begsr mov_mbr;

       command = 'CPYF FROMFILE(' +
                  %trim(#M2_mmlib) + '/' + %trim(#M2_mmfile) +
                  ') TOFILE(CLLCFILE/SOURCEFILE) FROMMBR(' +
                  %trim(#M2_mmmbr) +
                  ') TOMBR(AMEMBER) MBROPT(*REPLACE)';

       #M2_status = 'OK';

       monitor;
       system(%trim(command));
       on-error *all;
          #M2_status = 'NG';
       endmon;

       endsr;

      /end-free
     c*/copy rpgcopy,dotp_flds
      /free
       begsr DOTP_FLDS;
       if     YD_SIDOPT   = '*BLANK';
        clear ZD_SIDOPT ;
       elseif YD_SIDOPT  <> *blanks;
              ZD_SIDOPT   = YD_SIDOPT ;
       endif;
       if     YD_SIDREQ   = '*BLANK';
        clear ZD_SIDREQ ;
       elseif YD_SIDREQ  <> *blanks;
              ZD_SIDREQ   = YD_SIDREQ ;
       endif;
       if     YD_SSNOPT   = '*BLANK';
        clear ZD_SSNOPT ;
       elseif YD_SSNOPT  <> *blanks;
              ZD_SSNOPT   = YD_SSNOPT ;
       endif;
       if     YD_SSNORID  = '*BLANK';
        clear ZD_SSNORID;
       elseif YD_SSNORID <> *blanks;
              ZD_SSNORID  = YD_SSNORID;
       endif;
       if     YD_SSNREQ   = '*BLANK';
        clear ZD_SSNREQ ;
       elseif YD_SSNREQ  <> *blanks;
              ZD_SSNREQ   = YD_SSNREQ ;
       endif;
       if     YFICE       = '*BLANK';
        clear ZFICE     ;
       elseif YFICE      <> *blanks;
              ZFICE       = YFICE     ;
       endif;
       if     YH_#TRANA   = '*BLANK';
        clear ZH_#TRANA ;
       elseif YH_#TRANA  <> *blanks;
              ZH_#TRANA   = YH_#TRANA ;
       endif;
       if     YH_#TRANS   = '*BLANK';
        clear ZH_#TRANS ;
       elseif YH_#TRANS  <> *blanks;
              ZH_#TRANS   = YH_#TRANS ;
       endif;
       if     YH_ACTN_H   = '*BLANK';
        clear ZH_ACTN_H ;
       elseif YH_ACTN_H  <> *blanks;
              ZH_ACTN_H   = YH_ACTN_H ;
       endif;
       if     YH_ADDRC    = '*BLANK';
        clear ZH_ADDRC  ;
       elseif YH_ADDRC   <> *blanks;
              ZH_ADDRC    = YH_ADDRC  ;
       endif;
       if     YH_ADDR1    = '*BLANK';
        clear ZH_ADDR1  ;
       elseif YH_ADDR1   <> *blanks;
              ZH_ADDR1    = YH_ADDR1  ;
       endif;
       if     YH_AT_#PGS  = '*BLANK';
        clear ZH_AT_#PGS;
       elseif YH_AT_#PGS <> *blanks;
              ZH_AT_#PGS  = YH_AT_#PGS;
       endif;
       if     YH_ATTFR    = '*BLANK';
        clear ZH_ATTFR  ;
       elseif YH_ATTFR   <> *blanks;
              ZH_ATTFR    = YH_ATTFR  ;
       endif;
       if     YH_ATTTO    = '*BLANK';
        clear ZH_ATTTO  ;
       elseif YH_ATTTO   <> *blanks;
              ZH_ATTTO    = YH_ATTTO  ;
       endif;
       if     YH_BIRTH    = '*BLANK';
        clear ZH_BIRTH  ;
       elseif YH_BIRTH   <> *blanks;
              ZH_BIRTH    = YH_BIRTH  ;
       endif;
       if     YH_BUTADD   = '*BLANK';
        clear ZH_BUTADD ;
       elseif YH_BUTADD  <> *blanks;
              ZH_BUTADD   = YH_BUTADD ;
       endif;
       if     YH_BUTBAS   = '*BLANK';
        clear ZH_BUTBAS ;
       elseif YH_BUTBAS  <> *blanks;
              ZH_BUTBAS   = YH_BUTBAS ;
       endif;
       if     YH_BUTCC    = '*BLANK';
        clear ZH_BUTCC  ;
       elseif YH_BUTCC   <> *blanks;
              ZH_BUTCC    = YH_BUTCC  ;
       endif;
       if     YH_BUTCLR   = '*BLANK';
        clear ZH_BUTCLR ;
       elseif YH_BUTCLR  <> *blanks;
              ZH_BUTCLR   = YH_BUTCLR ;
       endif;
       if     YH_BUTCOD   = '*BLANK';
        clear ZH_BUTCOD ;
       elseif YH_BUTCOD  <> *blanks;
              ZH_BUTCOD   = YH_BUTCOD ;
       endif;
       if     YH_BUTCXL   = '*BLANK';
        clear ZH_BUTCXL ;
       elseif YH_BUTCXL  <> *blanks;
              ZH_BUTCXL   = YH_BUTCXL ;
       endif;
       if     YH_BUTEDT   = '*BLANK';
        clear ZH_BUTEDT ;
       elseif YH_BUTEDT  <> *blanks;
              ZH_BUTEDT   = YH_BUTEDT ;
       endif;
       if     YH_BUTFREE  = '*BLANK';
        clear ZH_BUTFREE;
       elseif YH_BUTFREE <> *blanks;
              ZH_BUTFREE  = YH_BUTFREE;
       endif;
       if     YH_BUTNFND  = '*BLANK';
        clear ZH_BUTNFND;
       elseif YH_BUTNFND <> *blanks;
              ZH_BUTNFND  = YH_BUTNFND;
       endif;
       if     YH_BUTNXT   = '*BLANK';
        clear ZH_BUTNXT ;
       elseif YH_BUTNXT  <> *blanks;
              ZH_BUTNXT   = YH_BUTNXT ;
       endif;
       if     YH_BUTRFND  = '*BLANK';
        clear ZH_BUTRFND;
       elseif YH_BUTRFND <> *blanks;
              ZH_BUTRFND  = YH_BUTRFND;
       endif;
       if     YH_BUTSRCH  = '*BLANK';
        clear ZH_BUTSRCH;
       elseif YH_BUTSRCH <> *blanks;
              ZH_BUTSRCH  = YH_BUTSRCH;
       endif;
       if     YH_BUTUPD   = '*BLANK';
        clear ZH_BUTUPD ;
       elseif YH_BUTUPD  <> *blanks;
              ZH_BUTUPD   = YH_BUTUPD ;
       endif;
       if     YH_BUTVUP   = '*BLANK';
        clear ZH_BUTVUP ;
       elseif YH_BUTVUP  <> *blanks;
              ZH_BUTVUP   = YH_BUTVUP ;
       endif;
       if     YH_CARDADR  = '*BLANK';
        clear ZH_CARDADR;
       elseif YH_CARDADR <> *blanks;
              ZH_CARDADR  = YH_CARDADR;
       endif;
       if     YH_CARDCIT  = '*BLANK';
        clear ZH_CARDCIT;
       elseif YH_CARDCIT <> *blanks;
              ZH_CARDCIT  = YH_CARDCIT;
       endif;
       if     YH_CARDCTY  = '*BLANK';
        clear ZH_CARDCTY;
       elseif YH_CARDCTY <> *blanks;
              ZH_CARDCTY  = YH_CARDCTY;
       endif;
       if     YH_CARDCVV  = '*BLANK';
        clear ZH_CARDCVV;
       elseif YH_CARDCVV <> *blanks;
              ZH_CARDCVV  = YH_CARDCVV;
       endif;
       if     YH_CARDEXM  = '*BLANK';
        clear ZH_CARDEXM;
       elseif YH_CARDEXM <> *blanks;
              ZH_CARDEXM  = YH_CARDEXM;
       endif;
       if     YH_CARDEXY  = '*BLANK';
        clear ZH_CARDEXY;
       elseif YH_CARDEXY <> *blanks;
              ZH_CARDEXY  = YH_CARDEXY;
       endif;
       if     YH_CARDNAM  = '*BLANK';
        clear ZH_CARDNAM;
       elseif YH_CARDNAM <> *blanks;
              ZH_CARDNAM  = YH_CARDNAM;
       endif;
       if     YH_CARDNMF  = '*BLANK';
        clear ZH_CARDNMF;
       elseif YH_CARDNMF <> *blanks;
              ZH_CARDNMF  = YH_CARDNMF;
       endif;
       if     YH_CARDNML  = '*BLANK';
        clear ZH_CARDNML;
       elseif YH_CARDNML <> *blanks;
              ZH_CARDNML  = YH_CARDNML;
       endif;
       if     YH_CARDNMM  = '*BLANK';
        clear ZH_CARDNMM;
       elseif YH_CARDNMM <> *blanks;
              ZH_CARDNMM  = YH_CARDNMM;
       endif;
       if     YH_CARDNUM  = '*BLANK';
        clear ZH_CARDNUM;
       elseif YH_CARDNUM <> *blanks;
              ZH_CARDNUM  = YH_CARDNUM;
       endif;
       if     YH_CARDSTA  = '*BLANK';
        clear ZH_CARDSTA;
       elseif YH_CARDSTA <> *blanks;
              ZH_CARDSTA  = YH_CARDSTA;
       endif;
       if     YH_CARDTYP  = '*BLANK';
        clear ZH_CARDTYP;
       elseif YH_CARDTYP <> *blanks;
              ZH_CARDTYP  = YH_CARDTYP;
       endif;
       if     YH_CARDZIP  = '*BLANK';
        clear ZH_CARDZIP;
       elseif YH_CARDZIP <> *blanks;
              ZH_CARDZIP  = YH_CARDZIP;
       endif;
       if     YH_CELL#    = '*BLANK';
        clear ZH_CELL#  ;
       elseif YH_CELL#   <> *blanks;
              ZH_CELL#    = YH_CELL#  ;
       endif;
       if     YH_CELL#2   = '*BLANK';
        clear ZH_CELL#2 ;
       elseif YH_CELL#2  <> *blanks;
              ZH_CELL#2   = YH_CELL#2 ;
       endif;
       if     YH_CELLCO   = '*BLANK';
        clear ZH_CELLCO ;
       elseif YH_CELLCO  <> *blanks;
              ZH_CELLCO   = YH_CELLCO ;
       endif;
       if     YH_CITY     = '*BLANK';
        clear ZH_CITY   ;
       elseif YH_CITY    <> *blanks;
              ZH_CITY     = YH_CITY   ;
       endif;
       if     YH_CNTRY    = '*BLANK';
        clear ZH_CNTRY  ;
       elseif YH_CNTRY   <> *blanks;
              ZH_CNTRY    = YH_CNTRY  ;
       endif;
       if     YH_COLLWD   = '*BLANK';
        clear ZH_COLLWD ;
       elseif YH_COLLWD  <> *blanks;
              ZH_COLLWD   = YH_COLLWD ;
       endif;
       if     YH_DELAD1   = '*BLANK';
        clear ZH_DELAD1 ;
       elseif YH_DELAD1  <> *blanks;
              ZH_DELAD1   = YH_DELAD1 ;
       endif;
       if     YH_DELAD2   = '*BLANK';
        clear ZH_DELAD2 ;
       elseif YH_DELAD2  <> *blanks;
              ZH_DELAD2   = YH_DELAD2 ;
       endif;
       if     YH_DELAD3   = '*BLANK';
        clear ZH_DELAD3 ;
       elseif YH_DELAD3  <> *blanks;
              ZH_DELAD3   = YH_DELAD3 ;
       endif;
       if     YH_DELATN   = '*BLANK';
        clear ZH_DELATN ;
       elseif YH_DELATN  <> *blanks;
              ZH_DELATN   = YH_DELATN ;
       endif;
       if     YH_DELCITY  = '*BLANK';
        clear ZH_DELCITY;
       elseif YH_DELCITY <> *blanks;
              ZH_DELCITY  = YH_DELCITY;
       endif;
       if     YH_DELINS   = '*BLANK';
        clear ZH_DELINS ;
       elseif YH_DELINS  <> *blanks;
              ZH_DELINS   = YH_DELINS ;
       endif;
       if     YH_DELSTAT  = '*BLANK';
        clear ZH_DELSTAT;
       elseif YH_DELSTAT <> *blanks;
              ZH_DELSTAT  = YH_DELSTAT;
       endif;
       if     YH_DELTEL   = '*BLANK';
        clear ZH_DELTEL ;
       elseif YH_DELTEL  <> *blanks;
              ZH_DELTEL   = YH_DELTEL ;
       endif;
       if     YH_EMAIL    = '*BLANK';
        clear ZH_EMAIL  ;
       elseif YH_EMAIL   <> *blanks;
              ZH_EMAIL    = YH_EMAIL  ;
       endif;
       if     YH_EMAIL2   = '*BLANK';
        clear ZH_EMAIL2 ;
       elseif YH_EMAIL2  <> *blanks;
              ZH_EMAIL2   = YH_EMAIL2 ;
       endif;
       if     YH_EMPLID   = '*BLANK';
        clear ZH_EMPLID ;
       elseif YH_EMPLID  <> *blanks;
              ZH_EMPLID   = YH_EMPLID ;
       endif;
       if     YH_FAX#     = '*BLANK';
        clear ZH_FAX#   ;
       elseif YH_FAX#    <> *blanks;
              ZH_FAX#     = YH_FAX#   ;
       endif;
       if     YH_FAX#2    = '*BLANK';
        clear ZH_FAX#2  ;
       elseif YH_FAX#2   <> *blanks;
              ZH_FAX#2    = YH_FAX#2  ;
       endif;
       if     YH_FAXATN   = '*BLANK';
        clear ZH_FAXATN ;
       elseif YH_FAXATN  <> *blanks;
              ZH_FAXATN   = YH_FAXATN ;
       endif;
       if     YH_FAX2CC   = '*BLANK';
        clear ZH_FAX2CC ;
       elseif YH_FAX2CC  <> *blanks;
              ZH_FAX2CC   = YH_FAX2CC ;
       endif;
       if     YH_FAX2NO   = '*BLANK';
        clear ZH_FAX2NO ;
       elseif YH_FAX2NO  <> *blanks;
              ZH_FAX2NO   = YH_FAX2NO ;
       endif;
       if     YH_FAX2NV   = '*BLANK';
        clear ZH_FAX2NV ;
       elseif YH_FAX2NV  <> *blanks;
              ZH_FAX2NV   = YH_FAX2NV ;
       endif;
       if     YH_FULLNM   = '*BLANK';
        clear ZH_FULLNM ;
       elseif YH_FULLNM  <> *blanks;
              ZH_FULLNM   = YH_FULLNM ;
       endif;
       if     YH_HDRBOI   = '*BLANK';
        clear ZH_HDRBOI ;
       elseif YH_HDRBOI  <> *blanks;
              ZH_HDRBOI   = YH_HDRBOI ;
       endif;
       if     YH_HDRCTCT  = '*BLANK';
        clear ZH_HDRCTCT;
       elseif YH_HDRCTCT <> *blanks;
              ZH_HDRCTCT  = YH_HDRCTCT;
       endif;
       if     YH_HDRFAX   = '*BLANK';
        clear ZH_HDRFAX ;
       elseif YH_HDRFAX  <> *blanks;
              ZH_HDRFAX   = YH_HDRFAX ;
       endif;
       if     YH_HDRPICK  = '*BLANK';
        clear ZH_HDRPICK;
       elseif YH_HDRPICK <> *blanks;
              ZH_HDRPICK  = YH_HDRPICK;
       endif;
       if     YH_HDRRECI  = '*BLANK';
        clear ZH_HDRRECI;
       elseif YH_HDRRECI <> *blanks;
              ZH_HDRRECI  = YH_HDRRECI;
       endif;
       if     YH_HDRSTUI  = '*BLANK';
        clear ZH_HDRSTUI;
       elseif YH_HDRSTUI <> *blanks;
              ZH_HDRSTUI  = YH_HDRSTUI;
       endif;
       if     YH_HDRSUM$  = '*BLANK';
        clear ZH_HDRSUM$;
       elseif YH_HDRSUM$ <> *blanks;
              ZH_HDRSUM$  = YH_HDRSUM$;
       endif;
       if     YH_HLPADDL  = '*BLANK';
        clear ZH_HLPADDL;
       elseif YH_HLPADDL <> *blanks;
              ZH_HLPADDL  = YH_HLPADDL;
       endif;
       if     YH_HLPHFCO  = '*BLANK';
        clear ZH_HLPHFCO;
       elseif YH_HLPHFCO <> *blanks;
              ZH_HLPHFCO  = YH_HLPHFCO;
       endif;
       if     YH_HLPHFIO  = '*BLANK';
        clear ZH_HLPHFIO;
       elseif YH_HLPHFIO <> *blanks;
              ZH_HLPHFIO  = YH_HLPHFIO;
       endif;
       if     YH_HLPHFKO  = '*BLANK';
        clear ZH_HLPHFKO;
       elseif YH_HLPHFKO <> *blanks;
              ZH_HLPHFKO  = YH_HLPHFKO;
       endif;
       if     YH_HLPHFOE  = '*BLANK';
        clear ZH_HLPHFOE;
       elseif YH_HLPHFOE <> *blanks;
              ZH_HLPHFOE  = YH_HLPHFOE;
       endif;
       if     YH_HLPPERO  = '*BLANK';
        clear ZH_HLPPERO;
       elseif YH_HLPPERO <> *blanks;
              ZH_HLPPERO  = YH_HLPPERO;
       endif;
       if     YH_HLPPERR  = '*BLANK';
        clear ZH_HLPPERR;
       elseif YH_HLPPERR <> *blanks;
              ZH_HLPPERR  = YH_HLPPERR;
       endif;
       if     YH_HLPPERT  = '*BLANK';
        clear ZH_HLPPERT;
       elseif YH_HLPPERT <> *blanks;
              ZH_HLPPERT  = YH_HLPPERT;
       endif;
       if     YH_HLPWFOR  = '*BLANK';
        clear ZH_HLPWFOR;
       elseif YH_HLPWFOR <> *blanks;
              ZH_HLPWFOR  = YH_HLPWFOR;
       endif;
       if     YH_HLPWNOC  = '*BLANK';
        clear ZH_HLPWNOC;
       elseif YH_HLPWNOC <> *blanks;
              ZH_HLPWNOC  = YH_HLPWNOC;
       endif;
       if     YH_HLPWVAR  = '*BLANK';
        clear ZH_HLPWVAR;
       elseif YH_HLPWVAR <> *blanks;
              ZH_HLPWVAR  = YH_HLPWVAR;
       endif;
       if     YH_KIND_A   = '*BLANK';
        clear ZH_KIND_A ;
       elseif YH_KIND_A  <> *blanks;
              ZH_KIND_A   = YH_KIND_A ;
       endif;
       if     YH_KIND_D   = '*BLANK';
        clear ZH_KIND_D ;
       elseif YH_KIND_D  <> *blanks;
              ZH_KIND_D   = YH_KIND_D ;
       endif;
       if     YH_KIND_E   = '*BLANK';
        clear ZH_KIND_E ;
       elseif YH_KIND_E  <> *blanks;
              ZH_KIND_E   = YH_KIND_E ;
       endif;
       if     YH_KIND_P   = '*BLANK';
        clear ZH_KIND_P ;
       elseif YH_KIND_P  <> *blanks;
              ZH_KIND_P   = YH_KIND_P ;
       endif;
       if     YH_KINDHD   = '*BLANK';
        clear ZH_KINDHD ;
       elseif YH_KINDHD  <> *blanks;
              ZH_KINDHD   = YH_KINDHD ;
       endif;
       if     YH_M2SCIT   = '*BLANK';
        clear ZH_M2SCIT ;
       elseif YH_M2SCIT  <> *blanks;
              ZH_M2SCIT   = YH_M2SCIT ;
       endif;
       if     YH_M2SNAM   = '*BLANK';
        clear ZH_M2SNAM ;
       elseif YH_M2SNAM  <> *blanks;
              ZH_M2SNAM   = YH_M2SNAM ;
       endif;
       if     YH_M2SSTA   = '*BLANK';
        clear ZH_M2SSTA ;
       elseif YH_M2SSTA  <> *blanks;
              ZH_M2SSTA   = YH_M2SSTA ;
       endif;
       if     YH_NAMEF    = '*BLANK';
        clear ZH_NAMEF  ;
       elseif YH_NAMEF   <> *blanks;
              ZH_NAMEF    = YH_NAMEF  ;
       endif;
       if     YH_NAMEFO   = '*BLANK';
        clear ZH_NAMEFO ;
       elseif YH_NAMEFO  <> *blanks;
              ZH_NAMEFO   = YH_NAMEFO ;
       endif;
       if     YH_NAMEL    = '*BLANK';
        clear ZH_NAMEL  ;
       elseif YH_NAMEL   <> *blanks;
              ZH_NAMEL    = YH_NAMEL  ;
       endif;
       if     YH_NAMELO   = '*BLANK';
        clear ZH_NAMELO ;
       elseif YH_NAMELO  <> *blanks;
              ZH_NAMELO   = YH_NAMELO ;
       endif;
       if     YH_NAMEM    = '*BLANK';
        clear ZH_NAMEM  ;
       elseif YH_NAMEM   <> *blanks;
              ZH_NAMEM    = YH_NAMEM  ;
       endif;
       if     YH_NAMEMI   = '*BLANK';
        clear ZH_NAMEMI ;
       elseif YH_NAMEMI  <> *blanks;
              ZH_NAMEMI   = YH_NAMEMI ;
       endif;
       if     YH_NAMES    = '*BLANK';
        clear ZH_NAMES  ;
       elseif YH_NAMES   <> *blanks;
              ZH_NAMES    = YH_NAMES  ;
       endif;
       if     YH_NET_C    = '*BLANK';
        clear ZH_NET_C  ;
       elseif YH_NET_C   <> *blanks;
              ZH_NET_C    = YH_NET_C  ;
       endif;
       if     YH_NET_D    = '*BLANK';
        clear ZH_NET_D  ;
       elseif YH_NET_D   <> *blanks;
              ZH_NET_D    = YH_NET_D  ;
       endif;
       if     YH_NET_E    = '*BLANK';
        clear ZH_NET_E  ;
       elseif YH_NET_E   <> *blanks;
              ZH_NET_E    = YH_NET_E  ;
       endif;
       if     YH_NET_F    = '*BLANK';
        clear ZH_NET_F  ;
       elseif YH_NET_F   <> *blanks;
              ZH_NET_F    = YH_NET_F  ;
       endif;
       if     YH_NET_M    = '*BLANK';
        clear ZH_NET_M  ;
       elseif YH_NET_M   <> *blanks;
              ZH_NET_M    = YH_NET_M  ;
       endif;
       if     YH_NET_O    = '*BLANK';
        clear ZH_NET_O  ;
       elseif YH_NET_O   <> *blanks;
              ZH_NET_O    = YH_NET_O  ;
       endif;
       if     YH_NET_S    = '*BLANK';
        clear ZH_NET_S  ;
       elseif YH_NET_S   <> *blanks;
              ZH_NET_S    = YH_NET_S  ;
       endif;
       if     YH_NET_X    = '*BLANK';
        clear ZH_NET_X  ;
       elseif YH_NET_X   <> *blanks;
              ZH_NET_X    = YH_NET_X  ;
       endif;
       if     YH_NET_1    = '*BLANK';
        clear ZH_NET_1  ;
       elseif YH_NET_1   <> *blanks;
              ZH_NET_1    = YH_NET_1  ;
       endif;
       if     YH_PIN      = '*BLANK';
        clear ZH_PIN    ;
       elseif YH_PIN     <> *blanks;
              ZH_PIN      = YH_PIN    ;
       endif;
       if     YH_PORS     = '*BLANK';
        clear ZH_PORS   ;
       elseif YH_PORS    <> *blanks;
              ZH_PORS     = YH_PORS   ;
       endif;
       if     YH_PUNAME   = '*BLANK';
        clear ZH_PUNAME ;
       elseif YH_PUNAME  <> *blanks;
              ZH_PUNAME   = YH_PUNAME ;
       endif;
       if     YH_PYBYCC   = '*BLANK';
        clear ZH_PYBYCC ;
       elseif YH_PYBYCC  <> *blanks;
              ZH_PYBYCC   = YH_PYBYCC ;
       endif;
       if     YH_PYINPER  = '*BLANK';
        clear ZH_PYINPER;
       elseif YH_PYINPER <> *blanks;
              ZH_PYINPER  = YH_PYINPER;
       endif;
       if     YH_QTY      = '*BLANK';
        clear ZH_QTY    ;
       elseif YH_QTY     <> *blanks;
              ZH_QTY      = YH_QTY    ;
       endif;
       if     YH_RECIP#   = '*BLANK';
        clear ZH_RECIP# ;
       elseif YH_RECIP#  <> *blanks;
              ZH_RECIP#   = YH_RECIP# ;
       endif;
       if     YH_RECIPH   = '*BLANK';
        clear ZH_RECIPH ;
       elseif YH_RECIPH  <> *blanks;
              ZH_RECIPH   = YH_RECIPH ;
       endif;
       if     YH_RECIPIS  = '*BLANK';
        clear ZH_RECIPIS;
       elseif YH_RECIPIS <> *blanks;
              ZH_RECIPIS  = YH_RECIPIS;
       endif;
       if     YH_RECIPT   = '*BLANK';
        clear ZH_RECIPT ;
       elseif YH_RECIPT  <> *blanks;
              ZH_RECIPT   = YH_RECIPT ;
       endif;
       if     YH_RECIP1Q  = '*BLANK';
        clear ZH_RECIP1Q;
       elseif YH_RECIP1Q <> *blanks;
              ZH_RECIP1Q  = YH_RECIP1Q;
       endif;
       if     YH_RECIP2Q  = '*BLANK';
        clear ZH_RECIP2Q;
       elseif YH_RECIP2Q <> *blanks;
              ZH_RECIP2Q  = YH_RECIP2Q;
       endif;
       if     YH_RECIP3Q  = '*BLANK';
        clear ZH_RECIP3Q;
       elseif YH_RECIP3Q <> *blanks;
              ZH_RECIP3Q  = YH_RECIP3Q;
       endif;
       if     YH_RECIP4Q  = '*BLANK';
        clear ZH_RECIP4Q;
       elseif YH_RECIP4Q <> *blanks;
              ZH_RECIP4Q  = YH_RECIP4Q;
       endif;
       if     YH_RECIP5Q  = '*BLANK';
        clear ZH_RECIP5Q;
       elseif YH_RECIP5Q <> *blanks;
              ZH_RECIP5Q  = YH_RECIP5Q;
       endif;
       if     YH_RPYMTH   = '*BLANK';
        clear ZH_RPYMTH ;
       elseif YH_RPYMTH  <> *blanks;
              ZH_RPYMTH   = YH_RPYMTH ;
       endif;
       if     YH_RTYPE_A  = '*BLANK';
        clear ZH_RTYPE_A;
       elseif YH_RTYPE_A <> *blanks;
              ZH_RTYPE_A  = YH_RTYPE_A;
       endif;
       if     YH_RTYPE_B  = '*BLANK';
        clear ZH_RTYPE_B;
       elseif YH_RTYPE_B <> *blanks;
              ZH_RTYPE_B  = YH_RTYPE_B;
       endif;
       if     YH_RTYPE_E  = '*BLANK';
        clear ZH_RTYPE_E;
       elseif YH_RTYPE_E <> *blanks;
              ZH_RTYPE_E  = YH_RTYPE_E;
       endif;
       if     YH_RTYPE_G  = '*BLANK';
        clear ZH_RTYPE_G;
       elseif YH_RTYPE_G <> *blanks;
              ZH_RTYPE_G  = YH_RTYPE_G;
       endif;
       if     YH_RTYPE_H  = '*BLANK';
        clear ZH_RTYPE_H;
       elseif YH_RTYPE_H <> *blanks;
              ZH_RTYPE_H  = YH_RTYPE_H;
       endif;
       if     YH_RTYPE_N  = '*BLANK';
        clear ZH_RTYPE_N;
       elseif YH_RTYPE_N <> *blanks;
              ZH_RTYPE_N  = YH_RTYPE_N;
       endif;
       if     YH_RTYPE_P  = '*BLANK';
        clear ZH_RTYPE_P;
       elseif YH_RTYPE_P <> *blanks;
              ZH_RTYPE_P  = YH_RTYPE_P;
       endif;
       if     YH_RTYPE_S  = '*BLANK';
        clear ZH_RTYPE_S;
       elseif YH_RTYPE_S <> *blanks;
              ZH_RTYPE_S  = YH_RTYPE_S;
       endif;
       if     YH_RTYPE_U  = '*BLANK';
        clear ZH_RTYPE_U;
       elseif YH_RTYPE_U <> *blanks;
              ZH_RTYPE_U  = YH_RTYPE_U;
       endif;
       if     YH_RTYPE_Z  = '*BLANK';
        clear ZH_RTYPE_Z;
       elseif YH_RTYPE_Z <> *blanks;
              ZH_RTYPE_Z  = YH_RTYPE_Z;
       endif;
       if     YH_SID      = '*BLANK';
        clear ZH_SID    ;
       elseif YH_SID     <> *blanks;
              ZH_SID      = YH_SID    ;
       endif;
       if     YH_SSN      = '*BLANK';
        clear ZH_SSN    ;
       elseif YH_SSN     <> *blanks;
              ZH_SSN      = YH_SSN    ;
       endif;
       if     YH_SSN_4    = '*BLANK';
        clear ZH_SSN_4  ;
       elseif YH_SSN_4   <> *blanks;
              ZH_SSN_4    = YH_SSN_4  ;
       endif;
       if     YH_SSN2     = '*BLANK';
        clear ZH_SSN2   ;
       elseif YH_SSN2    <> *blanks;
              ZH_SSN2     = YH_SSN2   ;
       endif;
       if     YH_SSN2_4   = '*BLANK';
        clear ZH_SSN2_4 ;
       elseif YH_SSN2_4  <> *blanks;
              ZH_SSN2_4   = YH_SSN2_4 ;
       endif;
       if     YH_STATE    = '*BLANK';
        clear ZH_STATE  ;
       elseif YH_STATE   <> *blanks;
              ZH_STATE    = YH_STATE  ;
       endif;
       if     YH_SUMELEC  = '*BLANK';
        clear ZH_SUMELEC;
       elseif YH_SUMELEC <> *blanks;
              ZH_SUMELEC  = YH_SUMELEC;
       endif;
       if     YH_SUMFREE  = '*BLANK';
        clear ZH_SUMFREE;
       elseif YH_SUMFREE <> *blanks;
              ZH_SUMFREE  = YH_SUMFREE;
       endif;
       if     YH_SUMGNED  = '*BLANK';
        clear ZH_SUMGNED;
       elseif YH_SUMGNED <> *blanks;
              ZH_SUMGNED  = YH_SUMGNED;
       endif;
       if     YH_SUMHNDL  = '*BLANK';
        clear ZH_SUMHNDL;
       elseif YH_SUMHNDL <> *blanks;
              ZH_SUMHNDL  = YH_SUMHNDL;
       endif;
       if     YH_SUMOPER  = '*BLANK';
        clear ZH_SUMOPER;
       elseif YH_SUMOPER <> *blanks;
              ZH_SUMOPER  = YH_SUMOPER;
       endif;
       if     YH_SUMSCHL  = '*BLANK';
        clear ZH_SUMSCHL;
       elseif YH_SUMSCHL <> *blanks;
              ZH_SUMSCHL  = YH_SUMSCHL;
       endif;
       if     YH_SUMSHIP  = '*BLANK';
        clear ZH_SUMSHIP;
       elseif YH_SUMSHIP <> *blanks;
              ZH_SUMSHIP  = YH_SUMSHIP;
       endif;
       if     YH_SUMTOTL  = '*BLANK';
        clear ZH_SUMTOTL;
       elseif YH_SUMTOTL <> *blanks;
              ZH_SUMTOTL  = YH_SUMTOTL;
       endif;
       if     YH_SUMXTRA  = '*BLANK';
        clear ZH_SUMXTRA;
       elseif YH_SUMXTRA <> *blanks;
              ZH_SUMXTRA  = YH_SUMXTRA;
       endif;
       if     YH_SUM800   = '*BLANK';
        clear ZH_SUM800 ;
       elseif YH_SUM800  <> *blanks;
              ZH_SUM800   = YH_SUM800 ;
       endif;
       if     YH_TEL_CC   = '*BLANK';
        clear ZH_TEL_CC ;
       elseif YH_TEL_CC  <> *blanks;
              ZH_TEL_CC   = YH_TEL_CC ;
       endif;
       if     YH_TEL#     = '*BLANK';
        clear ZH_TEL#   ;
       elseif YH_TEL#    <> *blanks;
              ZH_TEL#     = YH_TEL#   ;
       endif;
       if     YH_WDTRNTO  = '*BLANK';
        clear ZH_WDTRNTO;
       elseif YH_WDTRNTO <> *blanks;
              ZH_WDTRNTO  = YH_WDTRNTO;
       endif;
       if     YH_ZIPCD    = '*BLANK';
        clear ZH_ZIPCD  ;
       elseif YH_ZIPCD   <> *blanks;
              ZH_ZIPCD    = YH_ZIPCD  ;
       endif;
       if     YLANGUAGE   = '*BLANK';
        clear ZLANGUAGE ;
       elseif YLANGUAGE  <> *blanks;
              ZLANGUAGE   = YLANGUAGE ;
       endif;
       if     YT_#TRANA   = '*BLANK';
        clear ZT_#TRANA ;
       elseif YT_#TRANA  <> *blanks;
              ZT_#TRANA   = YT_#TRANA ;
       endif;
       if     YT_#TRANS   = '*BLANK';
        clear ZT_#TRANS ;
       elseif YT_#TRANS  <> *blanks;
              ZT_#TRANS   = YT_#TRANS ;
       endif;
       if     YT_ACTN_H   = '*BLANK';
        clear ZT_ACTN_H ;
       elseif YT_ACTN_H  <> *blanks;
              ZT_ACTN_H   = YT_ACTN_H ;
       endif;
       if     YT_ADDRC    = '*BLANK';
        clear ZT_ADDRC  ;
       elseif YT_ADDRC   <> *blanks;
              ZT_ADDRC    = YT_ADDRC  ;
       endif;
       if     YT_ADDR1    = '*BLANK';
        clear ZT_ADDR1  ;
       elseif YT_ADDR1   <> *blanks;
              ZT_ADDR1    = YT_ADDR1  ;
       endif;
       if     YT_AT_#PGS  = '*BLANK';
        clear ZT_AT_#PGS;
       elseif YT_AT_#PGS <> *blanks;
              ZT_AT_#PGS  = YT_AT_#PGS;
       endif;
       if     YT_ATTFR    = '*BLANK';
        clear ZT_ATTFR  ;
       elseif YT_ATTFR   <> *blanks;
              ZT_ATTFR    = YT_ATTFR  ;
       endif;
       if     YT_ATTTO    = '*BLANK';
        clear ZT_ATTTO  ;
       elseif YT_ATTTO   <> *blanks;
              ZT_ATTTO    = YT_ATTTO  ;
       endif;
       if     YT_BIRTH    = '*BLANK';
        clear ZT_BIRTH  ;
       elseif YT_BIRTH   <> *blanks;
              ZT_BIRTH    = YT_BIRTH  ;
       endif;
       if     YT_BUTADD   = '*BLANK';
        clear ZT_BUTADD ;
       elseif YT_BUTADD  <> *blanks;
              ZT_BUTADD   = YT_BUTADD ;
       endif;
       if     YT_BUTBAS   = '*BLANK';
        clear ZT_BUTBAS ;
       elseif YT_BUTBAS  <> *blanks;
              ZT_BUTBAS   = YT_BUTBAS ;
       endif;
       if     YT_BUTCC    = '*BLANK';
        clear ZT_BUTCC  ;
       elseif YT_BUTCC   <> *blanks;
              ZT_BUTCC    = YT_BUTCC  ;
       endif;
       if     YT_BUTCLR   = '*BLANK';
        clear ZT_BUTCLR ;
       elseif YT_BUTCLR  <> *blanks;
              ZT_BUTCLR   = YT_BUTCLR ;
       endif;
       if     YT_BUTCOD   = '*BLANK';
        clear ZT_BUTCOD ;
       elseif YT_BUTCOD  <> *blanks;
              ZT_BUTCOD   = YT_BUTCOD ;
       endif;
       if     YT_BUTCXL   = '*BLANK';
        clear ZT_BUTCXL ;
       elseif YT_BUTCXL  <> *blanks;
              ZT_BUTCXL   = YT_BUTCXL ;
       endif;
       if     YT_BUTEDT   = '*BLANK';
        clear ZT_BUTEDT ;
       elseif YT_BUTEDT  <> *blanks;
              ZT_BUTEDT   = YT_BUTEDT ;
       endif;
       if     YT_BUTFREE  = '*BLANK';
        clear ZT_BUTFREE;
       elseif YT_BUTFREE <> *blanks;
              ZT_BUTFREE  = YT_BUTFREE;
       endif;
       if     YT_BUTNFND  = '*BLANK';
        clear ZT_BUTNFND;
       elseif YT_BUTNFND <> *blanks;
              ZT_BUTNFND  = YT_BUTNFND;
       endif;
       if     YT_BUTNXT   = '*BLANK';
        clear ZT_BUTNXT ;
       elseif YT_BUTNXT  <> *blanks;
              ZT_BUTNXT   = YT_BUTNXT ;
       endif;
       if     YT_BUTRFND  = '*BLANK';
        clear ZT_BUTRFND;
       elseif YT_BUTRFND <> *blanks;
              ZT_BUTRFND  = YT_BUTRFND;
       endif;
       if     YT_BUTSRCH  = '*BLANK';
        clear ZT_BUTSRCH;
       elseif YT_BUTSRCH <> *blanks;
              ZT_BUTSRCH  = YT_BUTSRCH;
       endif;
       if     YT_BUTUPD   = '*BLANK';
        clear ZT_BUTUPD ;
       elseif YT_BUTUPD  <> *blanks;
              ZT_BUTUPD   = YT_BUTUPD ;
       endif;
       if     YT_BUTVUP   = '*BLANK';
        clear ZT_BUTVUP ;
       elseif YT_BUTVUP  <> *blanks;
              ZT_BUTVUP   = YT_BUTVUP ;
       endif;
       if     YT_CARDADR  = '*BLANK';
        clear ZT_CARDADR;
       elseif YT_CARDADR <> *blanks;
              ZT_CARDADR  = YT_CARDADR;
       endif;
       if     YT_CARDCIT  = '*BLANK';
        clear ZT_CARDCIT;
       elseif YT_CARDCIT <> *blanks;
              ZT_CARDCIT  = YT_CARDCIT;
       endif;
       if     YT_CARDCTY  = '*BLANK';
        clear ZT_CARDCTY;
       elseif YT_CARDCTY <> *blanks;
              ZT_CARDCTY  = YT_CARDCTY;
       endif;
       if     YT_CARDCVV  = '*BLANK';
        clear ZT_CARDCVV;
       elseif YT_CARDCVV <> *blanks;
              ZT_CARDCVV  = YT_CARDCVV;
       endif;
       if     YT_CARDEXM  = '*BLANK';
        clear ZT_CARDEXM;
       elseif YT_CARDEXM <> *blanks;
              ZT_CARDEXM  = YT_CARDEXM;
       endif;
       if     YT_CARDEXY  = '*BLANK';
        clear ZT_CARDEXY;
       elseif YT_CARDEXY <> *blanks;
              ZT_CARDEXY  = YT_CARDEXY;
       endif;
       if     YT_CARDNAM  = '*BLANK';
        clear ZT_CARDNAM;
       elseif YT_CARDNAM <> *blanks;
              ZT_CARDNAM  = YT_CARDNAM;
       endif;
       if     YT_CARDNMF  = '*BLANK';
        clear ZT_CARDNMF;
       elseif YT_CARDNMF <> *blanks;
              ZT_CARDNMF  = YT_CARDNMF;
       endif;
       if     YT_CARDNML  = '*BLANK';
        clear ZT_CARDNML;
       elseif YT_CARDNML <> *blanks;
              ZT_CARDNML  = YT_CARDNML;
       endif;
       if     YT_CARDNMM  = '*BLANK';
        clear ZT_CARDNMM;
       elseif YT_CARDNMM <> *blanks;
              ZT_CARDNMM  = YT_CARDNMM;
       endif;
       if     YT_CARDNUM  = '*BLANK';
        clear ZT_CARDNUM;
       elseif YT_CARDNUM <> *blanks;
              ZT_CARDNUM  = YT_CARDNUM;
       endif;
       if     YT_CARDSTA  = '*BLANK';
        clear ZT_CARDSTA;
       elseif YT_CARDSTA <> *blanks;
              ZT_CARDSTA  = YT_CARDSTA;
       endif;
       if     YT_CARDTYP  = '*BLANK';
        clear ZT_CARDTYP;
       elseif YT_CARDTYP <> *blanks;
              ZT_CARDTYP  = YT_CARDTYP;
       endif;
       if     YT_CARDZIP  = '*BLANK';
        clear ZT_CARDZIP;
       elseif YT_CARDZIP <> *blanks;
              ZT_CARDZIP  = YT_CARDZIP;
       endif;
       if     YT_CELL#    = '*BLANK';
        clear ZT_CELL#  ;
       elseif YT_CELL#   <> *blanks;
              ZT_CELL#    = YT_CELL#  ;
       endif;
       if     YT_CELL#2   = '*BLANK';
        clear ZT_CELL#2 ;
       elseif YT_CELL#2  <> *blanks;
              ZT_CELL#2   = YT_CELL#2 ;
       endif;
       if     YT_CELLCO   = '*BLANK';
        clear ZT_CELLCO ;
       elseif YT_CELLCO  <> *blanks;
              ZT_CELLCO   = YT_CELLCO ;
       endif;
       if     YT_CITY     = '*BLANK';
        clear ZT_CITY   ;
       elseif YT_CITY    <> *blanks;
              ZT_CITY     = YT_CITY   ;
       endif;
       if     YT_CNTRY    = '*BLANK';
        clear ZT_CNTRY  ;
       elseif YT_CNTRY   <> *blanks;
              ZT_CNTRY    = YT_CNTRY  ;
       endif;
       if     YT_COLLWD   = '*BLANK';
        clear ZT_COLLWD ;
       elseif YT_COLLWD  <> *blanks;
              ZT_COLLWD   = YT_COLLWD ;
       endif;
       if     YT_DELAD1   = '*BLANK';
        clear ZT_DELAD1 ;
       elseif YT_DELAD1  <> *blanks;
              ZT_DELAD1   = YT_DELAD1 ;
       endif;
       if     YT_DELAD2   = '*BLANK';
        clear ZT_DELAD2 ;
       elseif YT_DELAD2  <> *blanks;
              ZT_DELAD2   = YT_DELAD2 ;
       endif;
       if     YT_DELAD3   = '*BLANK';
        clear ZT_DELAD3 ;
       elseif YT_DELAD3  <> *blanks;
              ZT_DELAD3   = YT_DELAD3 ;
       endif;
       if     YT_DELATN   = '*BLANK';
        clear ZT_DELATN ;
       elseif YT_DELATN  <> *blanks;
              ZT_DELATN   = YT_DELATN ;
       endif;
       if     YT_DELCITY  = '*BLANK';
        clear ZT_DELCITY;
       elseif YT_DELCITY <> *blanks;
              ZT_DELCITY  = YT_DELCITY;
       endif;
       if     YT_DELINS   = '*BLANK';
        clear ZT_DELINS ;
       elseif YT_DELINS  <> *blanks;
              ZT_DELINS   = YT_DELINS ;
       endif;
       if     YT_DELSTAT  = '*BLANK';
        clear ZT_DELSTAT;
       elseif YT_DELSTAT <> *blanks;
              ZT_DELSTAT  = YT_DELSTAT;
       endif;
       if     YT_DELTEL   = '*BLANK';
        clear ZT_DELTEL ;
       elseif YT_DELTEL  <> *blanks;
              ZT_DELTEL   = YT_DELTEL ;
       endif;
       if     YT_EMAIL    = '*BLANK';
        clear ZT_EMAIL  ;
       elseif YT_EMAIL   <> *blanks;
              ZT_EMAIL    = YT_EMAIL  ;
       endif;
       if     YT_EMAIL2   = '*BLANK';
        clear ZT_EMAIL2 ;
       elseif YT_EMAIL2  <> *blanks;
              ZT_EMAIL2   = YT_EMAIL2 ;
       endif;
       if     YT_EMPLID   = '*BLANK';
        clear ZT_EMPLID ;
       elseif YT_EMPLID  <> *blanks;
              ZT_EMPLID   = YT_EMPLID ;
       endif;
       if     YT_FAX#     = '*BLANK';
        clear ZT_FAX#   ;
       elseif YT_FAX#    <> *blanks;
              ZT_FAX#     = YT_FAX#   ;
       endif;
       if     YT_FAX#2    = '*BLANK';
        clear ZT_FAX#2  ;
       elseif YT_FAX#2   <> *blanks;
              ZT_FAX#2    = YT_FAX#2  ;
       endif;
       if     YT_FAXATN   = '*BLANK';
        clear ZT_FAXATN ;
       elseif YT_FAXATN  <> *blanks;
              ZT_FAXATN   = YT_FAXATN ;
       endif;
       if     YT_FAX2CC   = '*BLANK';
        clear ZT_FAX2CC ;
       elseif YT_FAX2CC  <> *blanks;
              ZT_FAX2CC   = YT_FAX2CC ;
       endif;
       if     YT_FAX2NO   = '*BLANK';
        clear ZT_FAX2NO ;
       elseif YT_FAX2NO  <> *blanks;
              ZT_FAX2NO   = YT_FAX2NO ;
       endif;
       if     YT_FAX2NV   = '*BLANK';
        clear ZT_FAX2NV ;
       elseif YT_FAX2NV  <> *blanks;
              ZT_FAX2NV   = YT_FAX2NV ;
       endif;
       if     YT_FULLNM   = '*BLANK';
        clear ZT_FULLNM ;
       elseif YT_FULLNM  <> *blanks;
              ZT_FULLNM   = YT_FULLNM ;
       endif;
       if     YT_HDRBOI   = '*BLANK';
        clear ZT_HDRBOI ;
       elseif YT_HDRBOI  <> *blanks;
              ZT_HDRBOI   = YT_HDRBOI ;
       endif;
       if     YT_HDRCTCT  = '*BLANK';
        clear ZT_HDRCTCT;
       elseif YT_HDRCTCT <> *blanks;
              ZT_HDRCTCT  = YT_HDRCTCT;
       endif;
       if     YT_HDRFAX   = '*BLANK';
        clear ZT_HDRFAX ;
       elseif YT_HDRFAX  <> *blanks;
              ZT_HDRFAX   = YT_HDRFAX ;
       endif;
       if     YT_HDRPICK  = '*BLANK';
        clear ZT_HDRPICK;
       elseif YT_HDRPICK <> *blanks;
              ZT_HDRPICK  = YT_HDRPICK;
       endif;
       if     YT_HDRRECI  = '*BLANK';
        clear ZT_HDRRECI;
       elseif YT_HDRRECI <> *blanks;
              ZT_HDRRECI  = YT_HDRRECI;
       endif;
       if     YT_HDRSTUI  = '*BLANK';
        clear ZT_HDRSTUI;
       elseif YT_HDRSTUI <> *blanks;
              ZT_HDRSTUI  = YT_HDRSTUI;
       endif;
       if     YT_HDRSUM$  = '*BLANK';
        clear ZT_HDRSUM$;
       elseif YT_HDRSUM$ <> *blanks;
              ZT_HDRSUM$  = YT_HDRSUM$;
       endif;
       if     YT_HLPADDL  = '*BLANK';
        clear ZT_HLPADDL;
       elseif YT_HLPADDL <> *blanks;
              ZT_HLPADDL  = YT_HLPADDL;
       endif;
       if     YT_HLPHFCO  = '*BLANK';
        clear ZT_HLPHFCO;
       elseif YT_HLPHFCO <> *blanks;
              ZT_HLPHFCO  = YT_HLPHFCO;
       endif;
       if     YT_HLPHFIO  = '*BLANK';
        clear ZT_HLPHFIO;
       elseif YT_HLPHFIO <> *blanks;
              ZT_HLPHFIO  = YT_HLPHFIO;
       endif;
       if     YT_HLPHFKO  = '*BLANK';
        clear ZT_HLPHFKO;
       elseif YT_HLPHFKO <> *blanks;
              ZT_HLPHFKO  = YT_HLPHFKO;
       endif;
       if     YT_HLPHFOE  = '*BLANK';
        clear ZT_HLPHFOE;
       elseif YT_HLPHFOE <> *blanks;
              ZT_HLPHFOE  = YT_HLPHFOE;
       endif;
       if     YT_HLPPERO  = '*BLANK';
        clear ZT_HLPPERO;
       elseif YT_HLPPERO <> *blanks;
              ZT_HLPPERO  = YT_HLPPERO;
       endif;
       if     YT_HLPPERR  = '*BLANK';
        clear ZT_HLPPERR;
       elseif YT_HLPPERR <> *blanks;
              ZT_HLPPERR  = YT_HLPPERR;
       endif;
       if     YT_HLPPERT  = '*BLANK';
        clear ZT_HLPPERT;
       elseif YT_HLPPERT <> *blanks;
              ZT_HLPPERT  = YT_HLPPERT;
       endif;
       if     YT_HLPWFOR  = '*BLANK';
        clear ZT_HLPWFOR;
       elseif YT_HLPWFOR <> *blanks;
              ZT_HLPWFOR  = YT_HLPWFOR;
       endif;
       if     YT_HLPWNOC  = '*BLANK';
        clear ZT_HLPWNOC;
       elseif YT_HLPWNOC <> *blanks;
              ZT_HLPWNOC  = YT_HLPWNOC;
       endif;
       if     YT_HLPWVAR  = '*BLANK';
        clear ZT_HLPWVAR;
       elseif YT_HLPWVAR <> *blanks;
              ZT_HLPWVAR  = YT_HLPWVAR;
       endif;
       if     YT_KIND_A   = '*BLANK';
        clear ZT_KIND_A ;
       elseif YT_KIND_A  <> *blanks;
              ZT_KIND_A   = YT_KIND_A ;
       endif;
       if     YT_KIND_D   = '*BLANK';
        clear ZT_KIND_D ;
       elseif YT_KIND_D  <> *blanks;
              ZT_KIND_D   = YT_KIND_D ;
       endif;
       if     YT_KIND_E   = '*BLANK';
        clear ZT_KIND_E ;
       elseif YT_KIND_E  <> *blanks;
              ZT_KIND_E   = YT_KIND_E ;
       endif;
       if     YT_KIND_P   = '*BLANK';
        clear ZT_KIND_P ;
       elseif YT_KIND_P  <> *blanks;
              ZT_KIND_P   = YT_KIND_P ;
       endif;
       if     YT_KINDHD   = '*BLANK';
        clear ZT_KINDHD ;
       elseif YT_KINDHD  <> *blanks;
              ZT_KINDHD   = YT_KINDHD ;
       endif;
       if     YT_M2SCIT   = '*BLANK';
        clear ZT_M2SCIT ;
       elseif YT_M2SCIT  <> *blanks;
              ZT_M2SCIT   = YT_M2SCIT ;
       endif;
       if     YT_M2SNAM   = '*BLANK';
        clear ZT_M2SNAM ;
       elseif YT_M2SNAM  <> *blanks;
              ZT_M2SNAM   = YT_M2SNAM ;
       endif;
       if     YT_M2SSTA   = '*BLANK';
        clear ZT_M2SSTA ;
       elseif YT_M2SSTA  <> *blanks;
              ZT_M2SSTA   = YT_M2SSTA ;
       endif;
       if     YT_NAMEF    = '*BLANK';
        clear ZT_NAMEF  ;
       elseif YT_NAMEF   <> *blanks;
              ZT_NAMEF    = YT_NAMEF  ;
       endif;
       if     YT_NAMEFO   = '*BLANK';
        clear ZT_NAMEFO ;
       elseif YT_NAMEFO  <> *blanks;
              ZT_NAMEFO   = YT_NAMEFO ;
       endif;
       if     YT_NAMEL    = '*BLANK';
        clear ZT_NAMEL  ;
       elseif YT_NAMEL   <> *blanks;
              ZT_NAMEL    = YT_NAMEL  ;
       endif;
       if     YT_NAMELO   = '*BLANK';
        clear ZT_NAMELO ;
       elseif YT_NAMELO  <> *blanks;
              ZT_NAMELO   = YT_NAMELO ;
       endif;
       if     YT_NAMEM    = '*BLANK';
        clear ZT_NAMEM  ;
       elseif YT_NAMEM   <> *blanks;
              ZT_NAMEM    = YT_NAMEM  ;
       endif;
       if     YT_NAMEMI   = '*BLANK';
        clear ZT_NAMEMI ;
       elseif YT_NAMEMI  <> *blanks;
              ZT_NAMEMI   = YT_NAMEMI ;
       endif;
       if     YT_NAMES    = '*BLANK';
        clear ZT_NAMES  ;
       elseif YT_NAMES   <> *blanks;
              ZT_NAMES    = YT_NAMES  ;
       endif;
       if     YT_NET_C    = '*BLANK';
        clear ZT_NET_C  ;
       elseif YT_NET_C   <> *blanks;
              ZT_NET_C    = YT_NET_C  ;
       endif;
       if     YT_NET_D    = '*BLANK';
        clear ZT_NET_D  ;
       elseif YT_NET_D   <> *blanks;
              ZT_NET_D    = YT_NET_D  ;
       endif;
       if     YT_NET_E    = '*BLANK';
        clear ZT_NET_E  ;
       elseif YT_NET_E   <> *blanks;
              ZT_NET_E    = YT_NET_E  ;
       endif;
       if     YT_NET_F    = '*BLANK';
        clear ZT_NET_F  ;
       elseif YT_NET_F   <> *blanks;
              ZT_NET_F    = YT_NET_F  ;
       endif;
       if     YT_NET_M    = '*BLANK';
        clear ZT_NET_M  ;
       elseif YT_NET_M   <> *blanks;
              ZT_NET_M    = YT_NET_M  ;
       endif;
       if     YT_NET_O    = '*BLANK';
        clear ZT_NET_O  ;
       elseif YT_NET_O   <> *blanks;
              ZT_NET_O    = YT_NET_O  ;
       endif;
       if     YT_NET_S    = '*BLANK';
        clear ZT_NET_S  ;
       elseif YT_NET_S   <> *blanks;
              ZT_NET_S    = YT_NET_S  ;
       endif;
       if     YT_NET_X    = '*BLANK';
        clear ZT_NET_X  ;
       elseif YT_NET_X   <> *blanks;
              ZT_NET_X    = YT_NET_X  ;
       endif;
       if     YT_NET_1    = '*BLANK';
        clear ZT_NET_1  ;
       elseif YT_NET_1   <> *blanks;
              ZT_NET_1    = YT_NET_1  ;
       endif;
       if     YT_PIN      = '*BLANK';
        clear ZT_PIN    ;
       elseif YT_PIN     <> *blanks;
              ZT_PIN      = YT_PIN    ;
       endif;
       if     YT_PORS     = '*BLANK';
        clear ZT_PORS   ;
       elseif YT_PORS    <> *blanks;
              ZT_PORS     = YT_PORS   ;
       endif;
       if     YT_PUNAME   = '*BLANK';
        clear ZT_PUNAME ;
       elseif YT_PUNAME  <> *blanks;
              ZT_PUNAME   = YT_PUNAME ;
       endif;
       if     YT_PYBYCC   = '*BLANK';
        clear ZT_PYBYCC ;
       elseif YT_PYBYCC  <> *blanks;
              ZT_PYBYCC   = YT_PYBYCC ;
       endif;
       if     YT_PYINPER  = '*BLANK';
        clear ZT_PYINPER;
       elseif YT_PYINPER <> *blanks;
              ZT_PYINPER  = YT_PYINPER;
       endif;
       if     YT_QTY      = '*BLANK';
        clear ZT_QTY    ;
       elseif YT_QTY     <> *blanks;
              ZT_QTY      = YT_QTY    ;
       endif;
       if     YT_RECIP#   = '*BLANK';
        clear ZT_RECIP# ;
       elseif YT_RECIP#  <> *blanks;
              ZT_RECIP#   = YT_RECIP# ;
       endif;
       if     YT_RECIPH   = '*BLANK';
        clear ZT_RECIPH ;
       elseif YT_RECIPH  <> *blanks;
              ZT_RECIPH   = YT_RECIPH ;
       endif;
       if     YT_RECIPIS  = '*BLANK';
        clear ZT_RECIPIS;
       elseif YT_RECIPIS <> *blanks;
              ZT_RECIPIS  = YT_RECIPIS;
       endif;
       if     YT_RECIPT   = '*BLANK';
        clear ZT_RECIPT ;
       elseif YT_RECIPT  <> *blanks;
              ZT_RECIPT   = YT_RECIPT ;
       endif;
       if     YT_RECIP1Q  = '*BLANK';
        clear ZT_RECIP1Q;
       elseif YT_RECIP1Q <> *blanks;
              ZT_RECIP1Q  = YT_RECIP1Q;
       endif;
       if     YT_RECIP2Q  = '*BLANK';
        clear ZT_RECIP2Q;
       elseif YT_RECIP2Q <> *blanks;
              ZT_RECIP2Q  = YT_RECIP2Q;
       endif;
       if     YT_RECIP3Q  = '*BLANK';
        clear ZT_RECIP3Q;
       elseif YT_RECIP3Q <> *blanks;
              ZT_RECIP3Q  = YT_RECIP3Q;
       endif;
       if     YT_RECIP4Q  = '*BLANK';
        clear ZT_RECIP4Q;
       elseif YT_RECIP4Q <> *blanks;
              ZT_RECIP4Q  = YT_RECIP4Q;
       endif;
       if     YT_RECIP5Q  = '*BLANK';
        clear ZT_RECIP5Q;
       elseif YT_RECIP5Q <> *blanks;
              ZT_RECIP5Q  = YT_RECIP5Q;
       endif;
       if     YT_RPYMTH   = '*BLANK';
        clear ZT_RPYMTH ;
       elseif YT_RPYMTH  <> *blanks;
              ZT_RPYMTH   = YT_RPYMTH ;
       endif;
       if     YT_RTYPE_A  = '*BLANK';
        clear ZT_RTYPE_A;
       elseif YT_RTYPE_A <> *blanks;
              ZT_RTYPE_A  = YT_RTYPE_A;
       endif;
       if     YT_RTYPE_B  = '*BLANK';
        clear ZT_RTYPE_B;
       elseif YT_RTYPE_B <> *blanks;
              ZT_RTYPE_B  = YT_RTYPE_B;
       endif;
       if     YT_RTYPE_E  = '*BLANK';
        clear ZT_RTYPE_E;
       elseif YT_RTYPE_E <> *blanks;
              ZT_RTYPE_E  = YT_RTYPE_E;
       endif;
       if     YT_RTYPE_G  = '*BLANK';
        clear ZT_RTYPE_G;
       elseif YT_RTYPE_G <> *blanks;
              ZT_RTYPE_G  = YT_RTYPE_G;
       endif;
       if     YT_RTYPE_H  = '*BLANK';
        clear ZT_RTYPE_H;
       elseif YT_RTYPE_H <> *blanks;
              ZT_RTYPE_H  = YT_RTYPE_H;
       endif;
       if     YT_RTYPE_N  = '*BLANK';
        clear ZT_RTYPE_N;
       elseif YT_RTYPE_N <> *blanks;
              ZT_RTYPE_N  = YT_RTYPE_N;
       endif;
       if     YT_RTYPE_P  = '*BLANK';
        clear ZT_RTYPE_P;
       elseif YT_RTYPE_P <> *blanks;
              ZT_RTYPE_P  = YT_RTYPE_P;
       endif;
       if     YT_RTYPE_S  = '*BLANK';
        clear ZT_RTYPE_S;
       elseif YT_RTYPE_S <> *blanks;
              ZT_RTYPE_S  = YT_RTYPE_S;
       endif;
       if     YT_RTYPE_U  = '*BLANK';
        clear ZT_RTYPE_U;
       elseif YT_RTYPE_U <> *blanks;
              ZT_RTYPE_U  = YT_RTYPE_U;
       endif;
       if     YT_RTYPE_Z  = '*BLANK';
        clear ZT_RTYPE_Z;
       elseif YT_RTYPE_Z <> *blanks;
              ZT_RTYPE_Z  = YT_RTYPE_Z;
       endif;
       if     YT_SID      = '*BLANK';
        clear ZT_SID    ;
       elseif YT_SID     <> *blanks;
              ZT_SID      = YT_SID    ;
       endif;
       if     YT_SSN      = '*BLANK';
        clear ZT_SSN    ;
       elseif YT_SSN     <> *blanks;
              ZT_SSN      = YT_SSN    ;
       endif;
       if     YT_SSN_4    = '*BLANK';
        clear ZT_SSN_4  ;
       elseif YT_SSN_4   <> *blanks;
              ZT_SSN_4    = YT_SSN_4  ;
       endif;
       if     YT_SSN2     = '*BLANK';
        clear ZT_SSN2   ;
       elseif YT_SSN2    <> *blanks;
              ZT_SSN2     = YT_SSN2   ;
       endif;
       if     YT_SSN2_4   = '*BLANK';
        clear ZT_SSN2_4 ;
       elseif YT_SSN2_4  <> *blanks;
              ZT_SSN2_4   = YT_SSN2_4 ;
       endif;
       if     YT_STATE    = '*BLANK';
        clear ZT_STATE  ;
       elseif YT_STATE   <> *blanks;
              ZT_STATE    = YT_STATE  ;
       endif;
       if     YT_SUMELEC  = '*BLANK';
        clear ZT_SUMELEC;
       elseif YT_SUMELEC <> *blanks;
              ZT_SUMELEC  = YT_SUMELEC;
       endif;
       if     YT_SUMFREE  = '*BLANK';
        clear ZT_SUMFREE;
       elseif YT_SUMFREE <> *blanks;
              ZT_SUMFREE  = YT_SUMFREE;
       endif;
       if     YT_SUMGNED  = '*BLANK';
        clear ZT_SUMGNED;
       elseif YT_SUMGNED <> *blanks;
              ZT_SUMGNED  = YT_SUMGNED;
       endif;
       if     YT_SUMHNDL  = '*BLANK';
        clear ZT_SUMHNDL;
       elseif YT_SUMHNDL <> *blanks;
              ZT_SUMHNDL  = YT_SUMHNDL;
       endif;
       if     YT_SUMOPER  = '*BLANK';
        clear ZT_SUMOPER;
       elseif YT_SUMOPER <> *blanks;
              ZT_SUMOPER  = YT_SUMOPER;
       endif;
       if     YT_SUMSCHL  = '*BLANK';
        clear ZT_SUMSCHL;
       elseif YT_SUMSCHL <> *blanks;
              ZT_SUMSCHL  = YT_SUMSCHL;
       endif;
       if     YT_SUMSHIP  = '*BLANK';
        clear ZT_SUMSHIP;
       elseif YT_SUMSHIP <> *blanks;
              ZT_SUMSHIP  = YT_SUMSHIP;
       endif;
       if     YT_SUMTOTL  = '*BLANK';
        clear ZT_SUMTOTL;
       elseif YT_SUMTOTL <> *blanks;
              ZT_SUMTOTL  = YT_SUMTOTL;
       endif;
       if     YT_SUMXTRA  = '*BLANK';
        clear ZT_SUMXTRA;
       elseif YT_SUMXTRA <> *blanks;
              ZT_SUMXTRA  = YT_SUMXTRA;
       endif;
       if     YT_SUM800   = '*BLANK';
        clear ZT_SUM800 ;
       elseif YT_SUM800  <> *blanks;
              ZT_SUM800   = YT_SUM800 ;
       endif;
       if     YT_TEL_CC   = '*BLANK';
        clear ZT_TEL_CC ;
       elseif YT_TEL_CC  <> *blanks;
              ZT_TEL_CC   = YT_TEL_CC ;
       endif;
       if     YT_TEL#     = '*BLANK';
        clear ZT_TEL#   ;
       elseif YT_TEL#    <> *blanks;
              ZT_TEL#     = YT_TEL#   ;
       endif;
       if     YT_WDTRNTO  = '*BLANK';
        clear ZT_WDTRNTO;
       elseif YT_WDTRNTO <> *blanks;
              ZT_WDTRNTO  = YT_WDTRNTO;
       endif;
       if     YT_ZIPCD    = '*BLANK';
        clear ZT_ZIPCD  ;
       elseif YT_ZIPCD   <> *blanks;
              ZT_ZIPCD    = YT_ZIPCD  ;
       endif;
       endsr;
      /end-free
