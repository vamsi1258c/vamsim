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
     f                                     extfile('CLLCFILE/CIMESSAG1A')

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
      /IF NOT DEFINED(dataqueues)
      *  QRCVDTAQ      Keyed receive

     D qrcvdtaq        pr                  extpgm('QRCVDTAQ')
     D p$DtaQ                        10a   Const
     D p$DtaQLib                     10a   Const
     D p$EntryLen                     5p 0 Const
     D p$Data                     32767a   Const Options(*VarSize)
     D p$Wait                         5p 0 Const
     D p$KeyOrd                       2a   Const Options(*NoPass)
     D p$KeyLen                       3P 0 Const Options(*NoPass)
     D p$KeyData                    256A   Const Options(*VarSize:*NoPass)
     D p$Sndr_l                       3p 0 Const Options(*NoPass)
     D p$Sndr_i                      10a   Const Options(*NoPass)


      * Prototype for Send Data Queue (QSNDDTAQ)
     D qsnddtaq        pr                  ExtPgm('QSNDDTAQ')
     D p$DtaQ                        10A   Const
     D p$DtaQLib                     10A   Const
     D p$EntryLen                     5P 0 Const
     D p$Data                     32767A   Const Options(*VarSize)
     D p$KeyLen                       3P 0 Const Options(*NoPass)
     D p$KeyData                    256A   Const Options(*VarSize:*NoPass)
     D p$AsyncRqs                    10A   Const Options(*NoPass)

      *  QCLRDTAQ

     D qclrdtaq        pr                  ExtPgm('QCLRDTAQ')
     D   dq_name                     10a   Const
     D   dq_lib                      10a   Const

      /DEFINE dataqueues
      /ENDIF
      /copy prototypes,exists_ifs
       dcl-pr exists_ifs extpgm('EXISTS_IFS');
          prot_path char(512) const;
          prot_rc char(1);
       end-pr;

     **-- Run system command:
     D system          Pr            10i 0 ExtProc( 'system' )
     D  command                        *   Value  Options( *String )

      *  program status data structure
     d/copy rpgcopy,cllcpsds
      /IF NOT DEFINED(cllcpsds)
      *------------------------------------------------------------
      * Program status data structure
      *------------------------------------------------------------
      * 02/12/13 MRB Added compilier directive to ensure copybook is
      *              not added twice to a program
      * 06/03/12 JJG Added ds name of ##psds to data structure
      *              so that individual pgms can do overlay(##psds...
      *              to add references for other psds fields
      *          JJG put psds as last ds in this member so overlay
      *              will work
      *------------------------------------------------------------
     Dprogerrsds       ds
     D  QQ_pgmname                   10a
     D  QQ_status                     5S 0
     D  QQ_lineno                     8a
     D  QQ_message                    7a
     D  QQ_library                   10a
     D  QQ_member                    10a
     D  QQ_jobname                   10a
     D  QQ_userid                    10a
     D  QQ_jobnbr                     6S 0
     D  QQ_routine                   10a
     D  QQ_buffer                 10000a
     D  QQ_parms                  10086a   overlay(progerrsds)

     D ##psds         sds
     D  pgm_name               1     10a
     D  pgm_status            11     15S 0
     D  ps_lineno             21     28a
     D  pgm_#parms            37     39S 0
     D  ps_message            40     46a
     D  ps_library            81     90a
     D  ps_except             91    170a
     D  pgm_jobname          244    253a
     D  pgm_userid           254    263a
     D  pgm_jobnbr           264    269S 0
     D  ps_member            324    333a
     D  ps_routine       *routine
     D  ps_proc          *proc

      /DEFINE cllcpsds
      /ENDIF
     d/copy rpgcopy,dvdtaara
      *  CLLC Degree Verification Data Area DS
      ****************************************************************
      * 05/13/16 DDZ Freed $pdfv_seq
      * 03/29/16 DDZ Added essTod_ym for releasing ESS invoices
      * 03/06/16 JJG Added DSTOP_DLY1, 2, 3 for tuning DAILY_STOP
      * 07/22/15 SAS Added cruise_docID for Career Cruisings document ID
      * 02/17/15 JJG Added ess_seq for ESS cicharge2 sequence numbers
      * 11/19/14 SAS Added tod_seq for ess Transcript On Demand order#
      *              UPDATED free spaces available to reflect true avail space
      * 09/26/14 MRB Added print group location codes
      * 06/03/14 TDR Added cite_seq field to be used by GALPRCGI
      * 05/04/14 JJG Added wk_oddeven field to be used by AUTOSAVDLY to
      *                    save taoes between 101-107 and 121-127
      * 04/27/14 JJG Added srvrcc_ctl field to cause MONERIS and BAMS_GGE4
      *                    programs to run parallel write of CCs to SERVER_CC
      * 08/27/13 KMK Added impaired field for BAMS! "Just Because"
      * 08/23/13 MRB Removed fields now in use by USPS data area
      * 07/11/13 KMK added ccstrkseq for new CCSTRACK1 database
      * 05/15/13 JJG added LAST_ARCH field to contain A or B to
      *                    designate the most recent backup cycle
      *                    for ARCHIVES
      *          JJG added LAST_NOBU field to contain A or B to
      *                    designate the most recent backup cycle
      *                    for ARCH_NOBU
      * 09/27/12 JJG added texch_seq in position 54-57 packed
      *                    values range from 400000 to 599999
      * 08/22/12 JJG Added csmailcntr for sequencing CS MAILHLD msgs
      * 06/15/12 JJG Added robo_aptdn flag to indicate that ROBOAPT is
      *                    not functioning
      * 11/15/11 MRB Added tp_postbal, tp_postmin
      * 10/20/11 MRB Added ppal_imprd
      * 08/12/11 MRB Added TP IMB Sequence
      * 02/20/11 JJG Removed dv-dbase field (pos 312)
      ****************************************************************
     ddvdtaara         ds           600    dtaara(dvdtaara)
      *   GA IMB Tracking Sequence # - MOVED TO USPSDTAARA
     d free9p0                 1      5p 0
      *   epay 6-digit seq for void/refund IOTPTransId
     d epay_seq                6      9p 0
      *   contact1 databse sequence control field (7 digits)
     d contact_sq             10     13p 0
      *   TP Print Group Location Codes
      *   'A' = Anywhere, 'N' = Northfield Only, 'V' = Vegas Only
     d prntgrp_d              14     14a
     d prntgrp_l              15     15a
     d prntgrp_m              16     16a
     d prntgrp_o              17     17a
     d prntgrp_x              18     18a
     d prntgrp_z              19     19a
      *   TP Postage Meter Balance - MOVED TO USPSDTAARA
     d free6s2                20     25s 2
      *    Post Office Minimum Balance for alert e-mails - MOVED TO USPSDTAARA
     d free7p2                26     29p 2
      *   ROBOAPTdn flag 'D' = down, any other = up
     d roboaptdn              30     30a
      *   last_arch contains A or B to indicate last b/u cycle
     d last_arch              31     31a
      *   last_nobu contains A or B to indicate last b/u cycle
     d last_nobu              32     32a
      *   control code for MONERIS & BAMS_GGE4 to send to SERVER_CC DTAQ
     d srvrcc_ctl             33     33a
      *   week odd/even control for backup tapes
      *   1 = odd weeks     2 =  even week
     d wk_oddeven             34     34a
      *   old std_colors, mapp_down & Audit date
     d free19a                35     53a
      *   Transcript exchange Seq nos. 4xxxxx and 5xxxxx
      *   starts at 400000 and ends at 599999
     d texch_seq              54     57p 0
      *   Transcript Order sequence number
     d dvtmsseq               58     62s 0
      *   Cust Service email seuence counter
     d csmailcntr             63     68s 0
      *   TOD order sequence number
     d tod_seq                69     73s 0
      *   ESS CICHARGE2 Sequence number
     d ess_seq                74     77p 0
     d spare                  78     93a
     d ppal_imprd             94     94a
     d epay_imprd             95     95a
     d Std_PWEXP              96     97s 0
     d ShutDown               98    100a
     d EM_Addr_PF            101    150a

      *    CCS Track1 Database Sequence Numbers
     d ccstrkseq             151    155p 0
      *    daily stop delay variables
     d dstop_dly1            156    157s 0
     d dstop_dly2            158    159s 0
     d dstop_dly3            160    161s 0
      *    Old EM_Addr_PW
     d free39a               162    200a
      *   old referred sequence
     d free7a                201    207a
      *   old $fax_dialp
     d free2a                208    209a
      *   old $dv_seqno
     d free5a                210    214a
      *   old $dv_curpfx
     d free8a                215    216a
      *  old evdtaseq
     d essTod_ym             217    222a
      *  Career Cruising Document ID
     d Cruise_docID          223    228p 0

      *  D - OBOR is down so skip monitoring in MONSYSSTS
     d $obor_down            229    229a

      *  Y - turn on RTN_EMAIL trace for one cycle, else N
      *  D - RTN_EMAIL down so skip MONSYSSTS
     d rmail_trc             230    230a

      *  CI Sequence no. for manual credit card processes
     d $rfnd_seq             231    235s 0

      *  old $pdfv_seq
     d free8p0               236    243p 0

      *  CREDINC and CICOPS333TTP server status codes.
      *   ' ' = Server status is OK to process
      *   'M' = Server status is is Maintenance - Display Maint Screen
     d CINC_svr              293    293a
     d COPSI_svr             294    294a

     d $masterPWE            295    310a
     d $masterPW             301    310a
     d isp_down              311    311a

      *  old  dv_dbase
     d free1b                312    312a

     d rr01_ym               313    318a
     d last_post#            319    327s 0
     d pque_seq#             328    334s 0
     d maint_zip4            336    336a
     d ci_holiday            337    337a
     d ctl_sesstp            338    338a

      *   GA (Generic Application) Controls  350-400

     d ctl_sessga            350    350a
     d ga_seqnum             351    355s 0

      * GA post office balance - MOVED TO USPSDTAARA
     d free7b                356    362s 2
      *    ga veh/pet sequence number control for records keys
     d ga_vp_seq#            363    368p 0
      *    ga veh deleted sequence number
     d ga_vd_seq#            369    374p 0
      *    towerdata NCOA file seq number
     d ga_ncoaseq            375    377s 0
      *    ga res unique instance number
     d ga_unqres#            378    383p 0
      *    ga VinPower Usage Counter
     d ga_vinPcnt            384    389p 0
      *    Server_MF flag to allow duplicate instance
     d srvrMF_dup            390    390a
      *    Citation Sequence number used in LPRCITE1
     d cite_seq              391    395p 0

      *   BAMS Impaired field
     d bam_prob              400    400a

      *   Orbital Gateway Control fields
     d orb_tracer            401    416S 0
     d orb_down              417    417a
     d orb_prob              418    418a
      *        Current ORBITAL JOBNAME/USER/JOB#
     d orb_jobnam            419    428a
     d orb_userid            429    438a
     d orb_job#              439    444a
      *        Current blended credit card rate
     d cc_blendrt            445    449S 5

      *   offline airbill order sequence number 484-488

     d ab_seqnum             484    488s 0

      *   Audit Flag to turn on auditing of Authorization Forms
     d auditauthz            490    490a

      *   Y = turn on browser tracing to cgitrace3 in any pgms that support it
     d browsr_trc            593    593a

      *   Y = do a backup of ARCHIVES lib during next AUTOSAVDLY run
     d bu_archive            594    594a

      *  Cutoff time for rollover to next day on Parking Appl
     d PP_shutoff            595    600s 0
     d/copy rpgcopy,cidtaara

      *  CI System Functions Control Data Area
      ****************************************************************
      * 05/20/16 MRB Restored deleted source
      ****************************************************************
     dcidtaara         ds           600    DTAARA('CIDTAARA')
     d secmonseq               1      6P 0
     d @Project#               7     12P 0

     D command         s            500a   inz
     D dec#            S             17p15
     D random#         S              5P 0 inz
     D r#_1sttime      S              1a   inz

     d/copy rpgcopy,$srvrm2_ds
      ************************************************************************
      * Variables used for sending and retrieving from SERVER_M2 Appl       **
      * NOTE... Stand-alone field definitions must precede data structures  **
      ************************************************************************
      * 03/11/12 JJG Added support for SJ function (SBMJOB)                 **
      * 05/12/11 KMK Added ability to grab Kiosk IP data                    **
      * 10/14/10 JJG corrected #m2_len_i from 200 to 300                    **
      ************************************************************************

     d#M2_len_i        s              5P 0 inz(300)
     d#M2_len_o        s              5P 0 inz(30000)
     d#M2_lib_i        s             10A   inz('CLLCFILE')
     d#M2_lib_o        s             10A   inz('CLLCFILE')
     d#M2_name_i       s             10A   inz('DTAQ_M2_I')
     d#M2_name_o       s             10A   inz('DTAQ_M2_O')
     d#M2_wait         s              5P 0 inz(15)
     d#M2_keylen       s              3P 0 inz(26)
     d#M2_sndr_i       s             10a   inz
     d#M2_sndr_l       s              3P 0 inz
     d#M2_retrys       s              3P 0 inz

      *  from the server_m2 appl
     d#M2_dqout        ds         30000
     d #M2_dq_kyo              1     26a
     d  #M2_jobn_o             1     10a
     d  #M2_jobu_o            11     20a
     d  #M2_job#_o            21     26S 0
     d #M2_status             31     32a
     d #M2_fice_o             33     38a
     d #M2_user_o             33     42a
     d #M2_duns_o             33     41a
     d #M2_duss_o             42     42a
     d #M2_result            101  30000a
      *   SPECIAL subfield of result so %trim() is faster
     d #M2_R6000             101   6100a

      *  to the server_m2 appl
     d#M2_dqin         ds           300
     d #M2_dq_kyi              1     26a
     d  #M2_jobn_i             1     10a
     d  #M2_jobu_i            11     20a
     d  #M2_job#_i            21     26S 0

      * Function Codes:
      *                 CM - Request to Store CCA monitor data
      *                 EP - End Program
      *                 FC - Files Close request
      *                 GW - Write a GATEWAY1 record for EPAY
      *                 IP - KIOSKIP1 retrieval
      *                 SJ - SBMJOB to batch request
      *                 SM - Request to Store SSN monitor data
      *                 MM - Move a Member
      *                 MR - CIMESSAG1 message retrieval
      *                 SM - SSN Monitor record
      *                 S1 - SECMONTR1 record
      *                 TP - TPFIELDS1 retrieval
      *                 IP - KIOSKIP1 Retrieval

     d #M2_func               29     30a

      *     for TPFIELDS1 retrieval
     d #M2_fice               31     36a
     d #M2_lang               37     37a

      *     for SSNMONTR1 audit record
     d*#M2_fice               31     36a
     d #M2_ssnenc             37     52a
     d #M2_ssnusr             53     62a
     d #M2_ssnrc1             63     64a
     d #M2_ssnrc2             65     66a
     d #M2_ssnord             67     75a

      *     for CCAMONTR1 audit record
     d*#M2_fice               31     36a
     d #M2_ccacc#             37     68a
     d #M2_ccausr             69     78a
     d #M2_ccarc1             79     80a
     d*#M2_ccarc2             81     82a       not implemented in file
     d #M2_ccaord             83     91a
     d #M2_ccatim             92     97a

      *     for MM Move member function
     d #M2_mmlib              31     40a
     d #M2_mmfile             41     50a
     d #M2_mmmbr              51     60a

      *     for GATEWAY1 record
     d #M2_gwfice             31     36a
     d #M2_gword#             37     45a
     d #M2_gwamtp             46     49a
     d #M2_gwcard             50     53a
     d #M2_gw_au#             61    124a
     d #M2_gw_cp#            125    188a

      *     for Kiosk IP retrieval
     d #M2_ipfice             31     36a
     d #M2_ipappl             37     38a

      *     for SECMONTR1 audit record
     d*#M2_fice               31     36a
     d #M2_s1_src             37     39a
     d #M2_s1_cat             40     42a
     d #M2_s1_sev             43     44a
     d #M2_s1_lib             45     54a
     d #M2_s1_pgm             55     64a
     d #M2_s1_usr             65     74a
     d #M2_s1_sta             75     75a
     d #M2_s1_lgt             76     76a
     d #M2_s1_ip              77    126a
     d #M2_s1_url            127    226a
     d #M2_s1_txt            227    300a

      *     for SBMJOB request
     d #M2_sj_key             31     62a
     d   #M2_sj_pid           31     33a
     d   #M2_sj_ord           34     42a
     d   #M2_sj_dat           43     50a
     d   #M2_sj_tim           51     62a
     d #M2_sj_cmd             66    300a

      *     for Message Retrieval request
     d*#M2_fice               31     36a
     d #M2_mrappl             37     39a
     d #M2_mrscrn             40     42a
     d #M2_mrsect             43     44a
     d #M2_mrlang             45     45a
     d #M2_mrkeyf             31     45a

      *     for Check File existence
     d #M2_filenm             31    300a

     d bufin                   1    100a

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
     c/copy rpgcopy,dotp_flds
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
     MAIN PROCEDURE EXIT >>Format Record         Tµ          Ë{







 å                        â   â           ä   ä           à   à           á   á           ã   ã
      ñ           ¢   ¢           .   .           <   <           (   (           +   +           |
 ì        ê   ê           ë   ë           è   è           í   í           î   î           ï   ï
      !           $   $           *   *           )   )           ;   ;           ¬   ¬           -
 Ñ        Ä   Ä           À   À           Á   Á           Ã   Ã           Å   Å           Ç   Ç
      ,           %   %           _   _           >   >           ?   ?           ø   ø           É
 :        È   È           Í   Í           Î   Î           Ï   Ï           Ì   Ì           `   `
      @           '   '           =   =           "   "                           a   a           b
 »        e   e           f   f           g   g           h   h           i   i           «   «
      ý           þ   þ           ±   ±           °   °           j   j           k   k           l
 æ        o   o           p   p           q   q           r   r           ª   ª           º   º
      Æ           ¤   ¤           µ   µ           ~   ~           s   s           t   t           u
 Ý        x   x           y   y           z   z           ¡   ¡           ¿   ¿           Ð   Ð
      ®           ^   ^           £   £           ¥   ¥           ·   ·           ©   ©           §
 ´        ½   ½           ¾   ¾           [   [           ]   ]           ¯   ¯           ¨   ¨
      {           A   A           B   B           C   C           D   D           E   E           F
 õ        I   I           ­   ­           ô   ô           ö   ö           ò   ò           ó   ó
      J           K   K           L   L           M   M           N   N           O   O           P
 \        ¹   ¹           û   û           ü   ü           ù   ù           ú   ú           ÿ   ÿ
      S           T   T           U   U           V   V           W   W           X   X           Y
 1        Ô   Ô           Ö   Ö           Ò   Ò           Ó   Ó           Õ   Õ           0   0
      3           4   4           5   5           6   6           7   7           8   8           9
          Ü   Ü           Ù   Ù           Ú   Ú







 ã                                        â   â           ä   ä           à   à           á   á
      ç           ñ   ñ           ¢   ¢           .   .           <   <           (   (           +
 ï        é   é           ê   ê           ë   ë           è   è           í   í           î   î
      ß           !   !           $   $           *   *           )   )           ;   ;           ¬
 Ç        Â   Â           Ä   Ä           À   À           Á   Á           Ã   Ã           Å   Å
      !           ,   ,           %   %           _   _           >   >           ?   ?           ø
 `        Ë   Ë           È   È           Í   Í           Î   Î           Ï   Ï           Ì   Ì
      #           @   @           '   '           =   =           "   "                           a
 «        d   d           e   e           f   f           g   g           h   h           i   i
      ð           ý   ý           þ   þ           ±   ±           °   °           j   j           k
 º        n   n           o   o           p   p           q   q           r   r           ª   ª
      ¸           Æ   Æ           ¤   ¤           µ   µ           ~   ~           s   s           t
 Ð        w   w           x   x           y   y           z   z           ¡   ¡           ¿   ¿
      Þ           ®   ®           ^   ^           £   £           ¥   ¥           ·   ·           ©
 ¨        ¼   ¼           ½   ½           ¾   ¾           [   [           ]   ]           ¯   ¯
      ×           {   {           A   A           B   B           C   C           D   D           E
 ó        H   H           I   I           ­   ­           ô   ô           ö   ö           ò   ò
      }           J   J           K   K           L   L           M   M           N   N           O
 ÿ        R   R           ¹   ¹           û   û           ü   ü           ù   ù           ú   ú
      ÷           S   S           T   T           U   U           V   V           W   W           X
 0        ²   ²           Ô   Ô           Ö   Ö           Ò   Ò           Ó   Ó           Õ   Õ
      2           3   3           4   4           5   5           6   6           7   7           8
          Û   Û           Ü   Ü           Ù   Ù           Ú   Ú







 á                                                        â   â           ä   ä           à   à
      å           ç   ç           ñ   ñ           ¢   ¢           .   .           <   <           (
 î        &   &           é   é           ê   ê           ë   ë           è   è           í   í
      ì           ß   ß           !   !           $   $           *   *           )   )           ;
 Å        /   /           Â   Â           Ä   Ä           À   À           Á   Á           Ã   Ã
      Ñ           !   !           ,   ,           %   %           _   _           >   >           ?
 Ì        Ê   Ê           Ë   Ë           È   È           Í   Í           Î   Î           Ï   Ï
      :           #   #           @   @           '   '           =   =           "   "
 i        c   c           d   d           e   e           f   f           g   g           h   h
      »           ð   ð           ý   ý           þ   þ           ±   ±           °   °           j
 ª        m   m           n   n           o   o           p   p           q   q           r   r
      æ           ¸   ¸           Æ   Æ           ¤   ¤           µ   µ           ~   ~           s
 ¿        v   v           w   w           x   x           y   y           z   z           ¡   ¡
      Ý           Þ   Þ           ®   ®           ^   ^           £   £           ¥   ¥           ·
 ¯        ¶   ¶           ¼   ¼           ½   ½           ¾   ¾           [   [           ]   ]
      ´           ×   ×           {   {           A   A           B   B           C   C           D
 ò        G   G           H   H           I   I           ­   ­           ô   ô           ö   ö
      õ           }   }           J   J           K   K           L   L           M   M           N
 ú        Q   Q           R   R           ¹   ¹           û   û           ü   ü           ù   ù
      \           ÷   ÷           S   S           T   T           U   U           V   V           W
 Õ        Z   Z           ²   ²           Ô   Ô           Ö   Ö           Ò   Ò           Ó   Ó
      1           2   2           3   3           4   4           5   5           6   6           7
          ³   ³           Û   Û           Ü   Ü           Ù   Ù           Ú   Ú







 à                                                                        â   â           ä   ä
      ã           å   å           ç   ç           ñ   ñ           ¢   ¢           .   .           <
 í        |   |           &   &           é   é           ê   ê           ë   ë           è   è
      ï           ì   ì           ß   ß           !   !           $   $           *   *           )
 Ã        -   -           /   /           Â   Â           Ä   Ä           À   À           Á   Á
      Ç           Ñ   Ñ           !   !           ,   ,           %   %           _   _           >
 Ï        É   É           Ê   Ê           Ë   Ë           È   È           Í   Í           Î   Î
      `           :   :           #   #           @   @           '   '           =   =           "
 h        b   b           c   c           d   d           e   e           f   f           g   g
      «           »   »           ð   ð           ý   ý           þ   þ           ±   ±           °
 r        l   l           m   m           n   n           o   o           p   p           q   q
      º           æ   æ           ¸   ¸           Æ   Æ           ¤   ¤           µ   µ           ~
 ¡        u   u           v   v           w   w           x   x           y   y           z   z
      Ð           Ý   Ý           Þ   Þ           ®   ®           ^   ^           £   £           ¥
 ]        §   §           ¶   ¶           ¼   ¼           ½   ½           ¾   ¾           [   [
      ¨           ´   ´           ×   ×           {   {           A   A           B   B           C
 ö        F   F           G   G           H   H           I   I           ­   ­           ô   ô
      ó           õ   õ           }   }           J   J           K   K           L   L           M
 ù        P   P           Q   Q           R   R           ¹   ¹           û   û           ü   ü
      ÿ           \   \           ÷   ÷           S   S           T   T           U   U           V
 Ó        Y   Y           Z   Z           ²   ²           Ô   Ô           Ö   Ö           Ò   Ò
      0           1   1           2   2           3   3           4   4           5   5           6
          9   9           ³   ³           Û   Û           Ü   Ü           Ù   Ù           Ú   Ú







 ä                                                                                        â   â
      á           ã   ã           å   å           ç   ç           ñ   ñ           ¢   ¢           .
 è        +   +           |   |           &   &           é   é           ê   ê           ë   ë
      î           ï   ï           ì   ì           ß   ß           !   !           $   $           *
 Á        ¬   ¬           -   -           /   /           Â   Â           Ä   Ä           À   À
      Å           Ç   Ç           Ñ   Ñ           !   !           ,   ,           %   %           _
 Î        ø   ø           É   É           Ê   Ê           Ë   Ë           È   È           Í   Í
      Ì           `   `           :   :           #   #           @   @           '   '           =
 g        a   a           b   b           c   c           d   d           e   e           f   f
      i           «   «           »   »           ð   ð           ý   ý           þ   þ           ±
 q        k   k           l   l           m   m           n   n           o   o           p   p
      ª           º   º           æ   æ           ¸   ¸           Æ   Æ           ¤   ¤           µ
 z        t   t           u   u           v   v           w   w           x   x           y   y
      ¿           Ð   Ð           Ý   Ý           Þ   Þ           ®   ®           ^   ^           £
 [        ©   ©           §   §           ¶   ¶           ¼   ¼           ½   ½           ¾   ¾
      ¯           ¨   ¨           ´   ´           ×   ×           {   {           A   A           B
 ô        E   E           F   F           G   G           H   H           I   I           ­   ­
      ò           ó   ó           õ   õ           }   }           J   J           K   K           L
 ü        O   O           P   P           Q   Q           R   R           ¹   ¹           û   û
      ú           ÿ   ÿ           \   \           ÷   ÷           S   S           T   T           U
 Ò        X   X           Y   Y           Z   Z           ²   ²           Ô   Ô           Ö   Ö
      Õ           0   0           1   1           2   2           3   3           4   4           5
 Ú        8   8           9   9           ³   ³           Û   Û           Ü   Ü           Ù   Ù







 â                                                                                             
      à           á   á           ã   ã           å   å           ç   ç           ñ   ñ           ¢
 ë        (   (           +   +           |   |           &   &           é   é           ê   ê
      í           î   î           ï   ï           ì   ì           ß   ß           !   !           $
 À        ;   ;           ¬   ¬           -   -           /   /           Â   Â           Ä   Ä
      Ã           Å   Å           Ç   Ç           Ñ   Ñ           !   !           ,   ,           %
 Í        ?   ?           ø   ø           É   É           Ê   Ê           Ë   Ë           È   È
      Ï           Ì   Ì           `   `           :   :           #   #           @   @           '
 f                        a   a           b   b           c   c           d   d           e   e
      h           i   i           «   «           »   »           ð   ð           ý   ý           þ
 p        j   j           k   k           l   l           m   m           n   n           o   o
      r           ª   ª           º   º           æ   æ           ¸   ¸           Æ   Æ           ¤
 y        s   s           t   t           u   u           v   v           w   w           x   x
      ¡           ¿   ¿           Ð   Ð           Ý   Ý           Þ   Þ           ®   ®           ^
 ¾        ·   ·           ©   ©           §   §           ¶   ¶           ¼   ¼           ½   ½
      ]           ¯   ¯           ¨   ¨           ´   ´           ×   ×           {   {           A
 ­        D   D           E   E           F   F           G   G           H   H           I   I
      ö           ò   ò           ó   ó           õ   õ           }   }           J   J           K
 û        N   N           O   O           P   P           Q   Q           R   R           ¹   ¹
      ù           ú   ú           ÿ   ÿ           \   \           ÷   ÷           S   S           T
 Ö        W   W           X   X           Y   Y           Z   Z           ²   ²           Ô   Ô
      Ó           Õ   Õ           0   0           1   1           2   2           3   3           4
 Ù        7   7           8   8           9   9           ³   ³           Û   Û           Ü   Ü







  
      ä           à   à           á   á           ã   ã           å   å           ç   ç           ñ
 ê        <   <           (   (           +   +           |   |           &   &           é   é
      è           í   í           î   î           ï   ï           ì   ì           ß   ß           !
 Ä        )   )           ;   ;           ¬   ¬           -   -           /   /           Â   Â
      Á           Ã   Ã           Å   Å           Ç   Ç           Ñ   Ñ           !   !           ,
 È        >   >           ?   ?           ø   ø           É   É           Ê   Ê           Ë   Ë
      Î           Ï   Ï           Ì   Ì           `   `           :   :           #   #           @
 e        "   "                           a   a           b   b           c   c           d   d
      g           h   h           i   i           «   «           »   »           ð   ð           ý
 o        °   °           j   j           k   k           l   l           m   m           n   n
      q           r   r           ª   ª           º   º           æ   æ           ¸   ¸           Æ
 x        ~   ~           s   s           t   t           u   u           v   v           w   w
      z           ¡   ¡           ¿   ¿           Ð   Ð           Ý   Ý           Þ   Þ           ®
 ½        ¥   ¥           ·   ·           ©   ©           §   §           ¶   ¶           ¼   ¼
      [           ]   ]           ¯   ¯           ¨   ¨           ´   ´           ×   ×           {
 I        C   C           D   D           E   E           F   F           G   G           H   H
      ô           ö   ö           ò   ò           ó   ó           õ   õ           }   }           J
 ¹        M   M           N   N           O   O           P   P           Q   Q           R   R
      ü           ù   ù           ú   ú           ÿ   ÿ           \   \           ÷   ÷           S
 Ô        V   V           W   W           X   X           Y   Y           Z   Z           ²   ²
      Ò           Ó   Ó           Õ   Õ           0   0           1   1           2   2           3
 Ü        6   6           7   7           8   8           9   9           ³   ³           Û   Û
      Ú







      â           ä   ä           à   à           á   á           ã   ã           å   å           ç
 é        .   .           <   <           (   (           +   +           |   |           &   &
      ë           è   è           í   í           î   î           ï   ï           ì   ì           ß
 Â        *   *           )   )           ;   ;           ¬   ¬           -   -           /   /
      À           Á   Á           Ã   Ã           Å   Å           Ç   Ç           Ñ   Ñ           !
 Ë        _   _           >   >           ?   ?           ø   ø           É   É           Ê   Ê
      Í           Î   Î           Ï   Ï           Ì   Ì           `   `           :   :           #
 d        =   =           "   "                           a   a           b   b           c   c
      f           g   g           h   h           i   i           «   «           »   »           ð
 n        ±   ±           °   °           j   j           k   k           l   l           m   m
      p           q   q           r   r           ª   ª           º   º           æ   æ           ¸
 w        µ   µ           ~   ~           s   s           t   t           u   u           v   v
      y           z   z           ¡   ¡           ¿   ¿           Ð   Ð           Ý   Ý           Þ
 ¼        £   £           ¥   ¥           ·   ·           ©   ©           §   §           ¶   ¶
      ¾           [   [           ]   ]           ¯   ¯           ¨   ¨           ´   ´           ×
 H        B   B           C   C           D   D           E   E           F   F           G   G
      ­           ô   ô           ö   ö           ò   ò           ó   ó           õ   õ           }
 R        L   L           M   M           N   N           O   O           P   P           Q   Q
      û           ü   ü           ù   ù           ú   ú           ÿ   ÿ           \   \           ÷
 ²        U   U           V   V           W   W           X   X           Y   Y           Z   Z
      Ö           Ò   Ò           Ó   Ó           Õ   Õ           0   0           1   1           2
 Û        5   5           6   6           7   7           8   8           9   9           ³   ³
      Ù           Ú   Ú







                  â   â           ä   ä           à   à           á   á           ã   ã           å
 &        ¢   ¢           .   .           <   <           (   (           +   +           |   |
      ê           ë   ë           è   è           í   í           î   î           ï   ï           ì
 /        $   $           *   *           )   )           ;   ;           ¬   ¬           -   -
      Ä           À   À           Á   Á           Ã   Ã           Å   Å           Ç   Ç           Ñ
 Ê        %   %           _   _           >   >           ?   ?           ø   ø           É   É
      È           Í   Í           Î   Î           Ï   Ï           Ì   Ì           `   `           :
 c        '   '           =   =           "   "                           a   a           b   b
      e           f   f           g   g           h   h           i   i           «   «           »
 m        þ   þ           ±   ±           °   °           j   j           k   k           l   l
      o           p   p           q   q           r   r           ª   ª           º   º           æ
 v        ¤   ¤           µ   µ           ~   ~           s   s           t   t           u   u
      x           y   y           z   z           ¡   ¡           ¿   ¿           Ð   Ð           Ý
 ¶        ^   ^           £   £           ¥   ¥           ·   ·           ©   ©           §   §
      ½           ¾   ¾           [   [           ]   ]           ¯   ¯           ¨   ¨           ´
 G        A   A           B   B           C   C           D   D           E   E           F   F
      I           ­   ­           ô   ô           ö   ö           ò   ò           ó   ó           õ
 Q        K   K           L   L           M   M           N   N           O   O           P   P
      ¹           û   û           ü   ü           ù   ù           ú   ú           ÿ   ÿ           \
 Z        T   T           U   U           V   V           W   W           X   X           Y   Y
      Ô           Ö   Ö           Ò   Ò           Ó   Ó           Õ   Õ           0   0           1
 ³        4   4           5   5           6   6           7   7           8   8           9   9
      Ü           Ù   Ù           Ú   Ú







                                  â   â           ä   ä           à   à           á   á           ã
 |        ñ   ñ           ¢   ¢           .   .           <   <           (   (           +   +
      é           ê   ê           ë   ë           è   è           í   í           î   î           ï
 -        !   !           $   $           *   *           )   )           ;   ;           ¬   ¬
      Â           Ä   Ä           À   À           Á   Á           Ã   Ã           Å   Å           Ç
 É        ,   ,           %   %           _   _           >   >           ?   ?           ø   ø
      Ë           È   È           Í   Í           Î   Î           Ï   Ï           Ì   Ì           `
 b        @   @           '   '           =   =           "   "                           a   a
      d           e   e           f   f           g   g           h   h           i   i           «
 l        ý   ý           þ   þ           ±   ±           °   °           j   j           k   k
      n           o   o           p   p           q   q           r   r           ª   ª           º
 u        Æ   Æ           ¤   ¤           µ   µ           ~   ~           s   s           t   t
      w           x   x           y   y           z   z           ¡   ¡           ¿   ¿           Ð
 §        ®   ®           ^   ^           £   £           ¥   ¥           ·   ·           ©   ©
      ¼           ½   ½           ¾   ¾           [   [           ]   ]           ¯   ¯           ¨
 F        {   {           A   A           B   B           C   C           D   D           E   E
      H           I   I           ­   ­           ô   ô           ö   ö           ò   ò           ó
 P        J   J           K   K           L   L           M   M           N   N           O   O
      R           ¹   ¹           û   û           ü   ü           ù   ù           ú   ú           ÿ
 Y        S   S           T   T           U   U           V   V           W   W           X   X
      ²           Ô   Ô           Ö   Ö           Ò   Ò           Ó   Ó           Õ   Õ           0
 9        3   3           4   4           5   5           6   6           7   7           8   8
      Û           Ü   Ü           Ù   Ù           Ú   Ú







                                                  â   â           ä   ä           à   à           á
 +        ç   ç           ñ   ñ           ¢   ¢           .   .           <   <           (   (
      &           é   é           ê   ê           ë   ë           è   è           í   í           î
 ¬        ß   ß           !   !           $   $           *   *           )   )           ;   ;
      /           Â   Â           Ä   Ä           À   À           Á   Á           Ã   Ã           Å
 ø        !   !           ,   ,           %   %           _   _           >   >           ?   ?
      Ê           Ë   Ë           È   È           Í   Í           Î   Î           Ï   Ï           Ì
 a        #   #           @   @           '   '           =   =           "   "
      c           d   d           e   e           f   f           g   g           h   h           i
 k        ð   ð           ý   ý           þ   þ           ±   ±           °   °           j   j
      m           n   n           o   o           p   p           q   q           r   r           ª
 t        ¸   ¸           Æ   Æ           ¤   ¤           µ   µ           ~   ~           s   s
      v           w   w           x   x           y   y           z   z           ¡   ¡           ¿
 ©        Þ   Þ           ®   ®           ^   ^           £   £           ¥   ¥           ·   ·
      ¶           ¼   ¼           ½   ½           ¾   ¾           [   [           ]   ]           ¯
 E        ×   ×           {   {           A   A           B   B           C   C           D   D
      G           H   H           I   I           ­   ­           ô   ô           ö   ö           ò
 O        }   }           J   J           K   K           L   L           M   M           N   N
      Q           R   R           ¹   ¹           û   û           ü   ü           ù   ù           ú
 X        ÷   ÷           S   S           T   T           U   U           V   V           W   W
      Z           ²   ²           Ô   Ô           Ö   Ö           Ò   Ò           Ó   Ó           Õ
 8        2   2           3   3           4   4           5   5           6   6           7   7
      ³           Û   Û           Ü   Ü           Ù   Ù           Ú   Ú







                                                                  â   â           ä   ä           à
 (        å   å           ç   ç           ñ   ñ           ¢   ¢           .   .           <   <
      |           &   &           é   é           ê   ê           ë   ë           è   è           í
 ;        ì   ì           ß   ß           !   !           $   $           *   *           )   )
      -           /   /           Â   Â           Ä   Ä           À   À           Á   Á           Ã
 ?        Ñ   Ñ           !   !           ,   ,           %   %           _   _           >   >
      É           Ê   Ê           Ë   Ë           È   È           Í   Í           Î   Î           Ï
          :   :           #   #           @   @           '   '           =   =           "   "
      b           c   c           d   d           e   e           f   f           g   g           h
 j        »   »           ð   ð           ý   ý           þ   þ           ±   ±           °   °
      l           m   m           n   n           o   o           p   p           q   q           r
 s        æ   æ           ¸   ¸           Æ   Æ           ¤   ¤           µ   µ           ~   ~
      u           v   v           w   w           x   x           y   y           z   z           ¡
 ·        Ý   Ý           Þ   Þ           ®   ®           ^   ^           £   £           ¥   ¥
      §           ¶   ¶           ¼   ¼           ½   ½           ¾   ¾           [   [           ]
 D        ´   ´           ×   ×           {   {           A   A           B   B           C   C
      F           G   G           H   H           I   I           ­   ­           ô   ô           ö
 N        õ   õ           }   }           J   J           K   K           L   L           M   M
      P           Q   Q           R   R           ¹   ¹           û   û           ü   ü           ù
 W        \   \           ÷   ÷           S   S           T   T           U   U           V   V
      Y           Z   Z           ²   ²           Ô   Ô           Ö   Ö           Ò   Ò           Ó
 7        1   1           2   2           3   3           4   4           5   5           6   6
      9           ³   ³           Û   Û           Ü   Ü           Ù   Ù           Ú   Ú







                                                                                  â   â           ä
 <        ã   ã           å   å           ç   ç           ñ   ñ           ¢   ¢           .   .
      +           |   |           &   &           é   é           ê   ê           ë   ë           è
 )        ï   ï           ì   ì           ß   ß           !   !           $   $           *   *
      ¬           -   -           /   /           Â   Â           Ä   Ä           À   À           Á
 >        Ç   Ç           Ñ   Ñ           !   !           ,   ,           %   %           _   _
      ø           É   É           Ê   Ê           Ë   Ë           È   È           Í   Í           Î
 "        `   `           :   :           #   #           @   @           '   '           =   =
      a           b   b           c   c           d   d           e   e           f   f           g
 °        «   «           »   »           ð   ð           ý   ý           þ   þ           ±   ±
      k           l   l           m   m           n   n           o   o           p   p           q
 ~        º   º           æ   æ           ¸   ¸           Æ   Æ           ¤   ¤           µ   µ
      t           u   u           v   v           w   w           x   x           y   y           z
 ¥        Ð   Ð           Ý   Ý           Þ   Þ           ®   ®           ^   ^           £   £
      ©           §   §           ¶   ¶           ¼   ¼           ½   ½           ¾   ¾           [
 C        ¨   ¨           ´   ´           ×   ×           {   {           A   A           B   B
      E           F   F           G   G           H   H           I   I           ­   ­           ô
 M        ó   ó           õ   õ           }   }           J   J           K   K           L   L
      O           P   P           Q   Q           R   R           ¹   ¹           û   û           ü
 V        ÿ   ÿ           \   \           ÷   ÷           S   S           T   T           U   U
      X           Y   Y           Z   Z           ²   ²           Ô   Ô           Ö   Ö           Ò
 6        0   0           1   1           2   2           3   3           4   4           5   5
      8           9   9           ³   ³           Û   Û           Ü   Ü           Ù   Ù           Ú







                                                                                                  â
 .        á   á           ã   ã           å   å           ç   ç           ñ   ñ           ¢   ¢
      (           +   +           |   |           &   &           é   é           ê   ê           ë
 *        î   î           ï   ï           ì   ì           ß   ß           !   !           $   $
      ;           ¬   ¬           -   -           /   /           Â   Â           Ä   Ä           À
 _        Å   Å           Ç   Ç           Ñ   Ñ           !   !           ,   ,           %   %
      ?           ø   ø           É   É           Ê   Ê           Ë   Ë           È   È           Í
 =        Ì   Ì           `   `           :   :           #   #           @   @           '   '
                  a   a           b   b           c   c           d   d           e   e           f
 ±        i   i           «   «           »   »           ð   ð           ý   ý           þ   þ
      j           k   k           l   l           m   m           n   n           o   o           p
 µ        ª   ª           º   º           æ   æ           ¸   ¸           Æ   Æ           ¤   ¤
      s           t   t           u   u           v   v           w   w           x   x           y
 £        ¿   ¿           Ð   Ð           Ý   Ý           Þ   Þ           ®   ®           ^   ^
      ·           ©   ©           §   §           ¶   ¶           ¼   ¼           ½   ½           ¾
 B        ¯   ¯           ¨   ¨           ´   ´           ×   ×           {   {           A   A
      D           E   E           F   F           G   G           H   H           I   I           ­
 L        ò   ò           ó   ó           õ   õ           }   }           J   J           K   K
      N           O   O           P   P           Q   Q           R   R           ¹   ¹           û
 U        ú   ú           ÿ   ÿ           \   \           ÷   ÷           S   S           T   T
      W           X   X           Y   Y           Z   Z           ²   ²           Ô   Ô           Ö
 5        Õ   Õ           0   0           1   1           2   2           3   3           4   4
      7           8   8           9   9           ³   ³           Û   Û           Ü   Ü           Ù






R                                                                                      >>>>TEXT DESC
 q    y   Q   -   Y   ø   8           °       µ       ^       {   ç   }   ì   \   Ç   0   Ì       h
 y    -   Y   ø   8           °       µ       ^       {   ç   }   ì   \   Ç   0   Ì       h       q
 ½    ø   8           °       µ       ^       {   ç   }   ì   \   Ç   0   Ì       h       q       y
ãH  å         °       µ       ^       {   ç   }   ì   \   Ç   0   Ì  â   âh  ä   äq  à   ày  á   á½
ìQ  ß.°  <   <µ  (   (^  +   +{  |ç  |}  &ì  &\  éÇ  é0  êÌ  ë   ëh  è   èq  í   íy  î   î½  ï   ïH
!Y  ,)µ  ;   ;^  ¬   ¬{  -ç  -}  /ì  /\  ÂÇ  Â0  ÄÌ  À   Àh  Á   Áq  Ã   Ãy  Å   Å½  Ç   ÇH  Ñ&  ÑQ
@8  '?^  ø   ø{  Éç  É}  Êì  Ê\  ËÇ  Ë0  ÈÌ  Í   Íh  Î   Îq  Ï   Ïy  Ì   Ì½  `   `H  :&  :Q  #-  #Y
±   ±a{  bç  b}  cì  c\  dÇ  d0  eÌ  f   fh  g   gq  h   hy  i   i½  «   «H  »&  »Q  ð-  ðY  ýø  ý8
~   ~l}  mì  m\  nÇ  n0  oÌ  p   ph  q   qq  r   ry  ª   ª½  º   ºH  æ&  æQ  ¸-  ¸Y  Æø  Æ8  ¤   µ
·   ·v\  wÇ  w0  xÌ  y   yh  z   zq  ¡   ¡y  ¿   ¿½  Ð   ÐH  Ý&  ÝQ  Þ-  ÞY  ®ø  ®8  ^   £   £°  ¥
E   E¼0  ½Ì  ¾   ¾h  [   [q  ]   ]y  ¯   ¯½  ¨   ¨H  ´&  ´Q  ×-  ×Y  {ø  {8  A   B   B°  C   Cµ  D
Pç  P­   ­h  ô   ôq  ö   öy  ò   ò½  ó   óH  õ&  õQ  }-  }Y  Jø  J8  K   L   L°  M   Mµ  N   N^  O
Zì  Zü   üq  ù   ùy  ú   ú½  ÿ   ÿH  \&  \Q  ÷-  ÷Y  Sø  S8  T   U   U°  V   Vµ  W   W^  X   X{  Yç
ÛÇ  ÛÓ   Óy  Õ   Õ½  0   0H  1&  1Q  2-  2Y  3ø  38  4   5   5°  6   6µ  7   7^  8   8{  9ç  9}  ³ì
 Ì        ½       H   &   Q   -   Y   ø   8           °       µ       ^       {   ç   }   ì   \   Ç
 h        H   &   Q   -   Y   ø   8           °       µ       ^       {   ç   }   ì   \   Ç   0   Ì
 q    &   Q   -   Y   ø   8           °       µ       ^       {   ç   }   ì   \   Ç   0   Ì       h
äy  à -   Y   ø   8           °       µ       ^       {   ç   }   ì   \   Ç   0   Ì       h  â   âq
í½  îçø  ç8  ñ   ¢   ¢°  .   .µ  <   <^  (   ({  +ç  +}  |ì  |\  &Ç  &0  éÌ  ê   êh  ë   ëq  è   èy
ÅH  Ç!   $   $°  *   *µ  )   )^  ;   ;{  ¬ç  ¬}  -ì  -\  /Ç  /0  ÂÌ  Ä   Äh  À   Àq  Á   Áy  Ã   Ã½
`Q  :%°  _   _µ  >   >^  ?   ?{  øç  ø}  Éì  É\  ÊÇ  Ê0  ËÌ  È   Èh  Í   Íq  Î   Îy  Ï   Ï½  Ì   ÌH
»Y  ð=µ  "   "^       {  aç  a}  bì  b\  cÇ  c0  dÌ  e   eh  f   fq  g   gy  h   h½  i   iH  «&  «Q
¸8  Æ°^  j   j{  kç  k}  lì  l\  mÇ  m0  nÌ  o   oh  p   pq  q   qy  r   r½  ª   ªH  º&  ºQ  æ-  æY
^   ^s{  tç  t}  uì  u\  vÇ  v0  wÌ  x   xh  y   yq  z   zy  ¡   ¡½  ¿   ¿H  Ð&  ÐQ  Ý-  ÝY  Þø  Þ8
B   B©}  §ì  §\  ¶Ç  ¶0  ¼Ì  ½   ½h  ¾   ¾q  [   [y  ]   ]½  ¯   ¯H  ¨&  ¨Q  ´-  ´Y  ×ø  ×8  {   A
M   MF\  GÇ  G0  HÌ  I   Ih  ­   ­q  ô   ôy  ö   ö½  ò   òH  ó&  óQ  õ-  õY  }ø  }8  J   K   K°  L
W   WQ0  RÌ  ¹   ¹h  û   ûq  ü   üy  ù   ù½  ú   úH  ÿ&  ÿQ  \-  \Y  ÷ø  ÷8  S   T   T°  U   Uµ  V
8ç  8Ô   Ôh  Ö   Öq  Ò   Òy  Ó   Ó½  Õ   ÕH  0&  0Q  1-  1Y  2ø  28  3   4   4°  5   5µ  6   6^  7
 ì   Ù   Ùq  Ú   Úy       ½       H   &   Q   -   Y   ø   8           °       µ       ^       {   ç
 Ç        y       ½       H   &   Q   -   Y   ø   8           °       µ       ^       {   ç   }   ì
 Ì        ½       H   &   Q   -   Y   ø   8           °       µ       ^       {   ç   }   ì   \   Ç
 h        H   &   Q   -   Y   ø   8           °       µ       ^       {   ç   }   ì   \   Ç   0   Ì
êq  ëá&  áQ  ã-  ãY  åø  å8  ç   ñ   ñ°  ¢   ¢µ  .   .^  <   <{  (ç  (}  +ì  +\  |Ç  |0  &Ì  é   éh
Ày  Áï-  ïY  ìø  ì8  ß   !   !°  $   $µ  *   *^  )   ){  ;ç  ;}  ¬ì  ¬\  -Ç  -0  /Ì  Â   Âh  Ä   Äq
Î½  ÏÑø  Ñ8  !   ,   ,°  %   %µ  _   _^  >   >{  ?ç  ?}  øì  ø\  ÉÇ  É0  ÊÌ  Ë   Ëh  È   Èq  Í   Íy
hH  i#   @   @°  '   'µ  =   =^  "   "{   ç   }  aì  a\  bÇ  b0  cÌ  d   dh  e   eq  f   fy  g   g½
ªQ  ºý°  þ   þµ  ±   ±^  °   °{  jç  j}  kì  k\  lÇ  l0  mÌ  n   nh  o   oq  p   py  q   q½  r   rH
ÐY  Ý¤µ  µ   µ^  ~   ~{  sç  s}  tì  t\  uÇ  u0  vÌ  w   wh  x   xq  y   yy  z   z½  ¡   ¡H  ¿&  ¿Q
´8  ×£^  ¥   ¥{  ·ç  ·}  ©ì  ©\  §Ç  §0  ¶Ì  ¼   ¼h  ½   ½q  ¾   ¾y  [   [½  ]   ]H  ¯&  ¯Q  ¨-  ¨Y
J   JC{  Dç  D}  Eì  E\  FÇ  F0  GÌ  H   Hh  I   Iq  ­   ­y  ô   ô½  ö   öH  ò&  òQ  ó-  óY  õø  õ8
T   TN}  Oì  O\  PÇ  P0  QÌ  R   Rh  ¹   ¹q  û   ûy  ü   ü½  ù   ùH  ú&  úQ  ÿ-  ÿY  \ø  \8  ÷   S
5   5X\  YÇ  Y0  ZÌ  ²   ²h  Ô   Ôq  Ö   Öy  Ò   Ò½  Ó   ÓH  Õ&  ÕQ  0-  0Y  1ø  18  2   3   3°  4
     90  ³Ì  Û   Ûh  Ü   Üq  Ù   Ùy  Ú   Ú½       H   &   Q   -   Y   ø   8           °       µ
 ç        h       q       y       ½       H   &   Q   -   Y   ø   8           °       µ       ^
 ì        q       y       ½       H   &   Q   -   Y   ø   8           °       µ       ^       {   ç
 Ç        y       ½       H   &   Q   -   Y   ø   8           °       µ       ^       {   ç   }   ì
|Ì  &â   â½  ä   äH  à&  àQ  á-  áY  ãø  ã8  å   ç   ç°  ñ   ñµ  ¢   ¢^  .   .{  <ç  <}  (ì  (\  +Ç
/h  Âè   èH  í&  íQ  î-  îY  ïø  ï8  ì   ß   ß°  !   !µ  $   $^  *   *{  )ç  )}  ;ì  ;\  ¬Ç  ¬0  -Ì
Ëq  ÈÃ&  ÃQ  Å-  ÅY  Çø  Ç8  Ñ   !   !°  ,   ,µ  %   %^  _   _{  >ç  >}  ?ì  ?\  øÇ  ø0  ÉÌ  Ê   Êh
ey  fÌ-  ÌY  `ø  `8  :   #   #°  @   @µ  '   '^  =   ={  "ç  "}   ì   \  aÇ  a0  bÌ  c   ch  d   dq
p½  q«ø  «8  »   ð   ð°  ý   ýµ  þ   þ^  ±   ±{  °ç  °}  jì  j\  kÇ  k0  lÌ  m   mh  n   nq  o   oy
zH  ¡æ   ¸   ¸°  Æ   Æµ  ¤   ¤^  µ   µ{  ~ç  ~}  sì  s\  tÇ  t0  uÌ  v   vh  w   wq  x   xy  y   y½
]Q  ¯Þ°  ®   ®µ  ^   ^^  £   £{  ¥ç  ¥}  ·ì  ·\  ©Ç  ©0  §Ì  ¶   ¶h  ¼   ¼q  ½   ½y  ¾   ¾½  [   [H
òY  ó{µ  A   A^  B   B{  Cç  C}  Dì  D\  EÇ  E0  FÌ  G   Gh  H   Hq  I   Iy  ­   ­½  ô   ôH  ö&  öQ
ÿ8  \K^  L   L{  Mç  M}  Nì  N\  OÇ  O0  PÌ  Q   Qh  R   Rq  ¹   ¹y  û   û½  ü   üH  ù&  ùQ  ú-  úY
2   2U{  Vç  V}  Wì  W\  XÇ  X0  YÌ  Z   Zh  ²   ²q  Ô   Ôy  Ö   Ö½  Ò   ÒH  Ó&  ÓQ  Õ-  ÕY  0ø  08
     6}  7ì  7\  8Ç  80  9Ì  ³   ³h  Û   Ûq  Ü   Üy  Ù   Ù½  Ú   ÚH   &   Q   -   Y   ø   8
      \   Ç   0   Ì       h       q       y       ½       H   &   Q   -   Y   ø   8           °
      0   Ì       h       q       y       ½       H   &   Q   -   Y   ø   8           °       µ
 ç        h       q       y       ½       H   &   Q   -   Y   ø   8           °       µ       ^
<ì  <     q       y       ½  â   âH  ä&  äQ  à-  àY  áø  á8  ã   å   å°  ç   çµ  ñ   ñ^  ¢   ¢{  .ç
;Ç  ;é   éy  ê   ê½  ë   ëH  è&  èQ  í-  íY  îø  î8  ï   ì   ì°  ß   ßµ  !   !^  $   ${  *ç  *}  )ì
øÌ  ÉÄ   Ä½  À   ÀH  Á&  ÁQ  Ã-  ÃY  Åø  Å8  Ç   Ñ   Ñ°  !   !µ  ,   ,^  %   %{  _ç  _}  >ì  >\  ?Ç
bh  cÍ   ÍH  Î&  ÎQ  Ï-  ÏY  Ìø  Ì8  `   :   :°  #   #µ  @   @^  '   '{  =ç  =}  "ì  "\   Ç   0  aÌ
mq  ng&  gQ  h-  hY  iø  i8  «   »   »°  ð   ðµ  ý   ý^  þ   þ{  ±ç  ±}  °ì  °\  jÇ  j0  kÌ  l   lh
wy  xr-  rY  ªø  ª8  º   æ   æ°  ¸   ¸µ  Æ   Æ^  ¤   ¤{  µç  µ}  ~ì  ~\  sÇ  s0  tÌ  u   uh  v   vq
½½  ¾¿ø  ¿8  Ð   Ý   Ý°  Þ   Þµ  ®   ®^  ^   ^{  £ç  £}  ¥ì  ¥\  ·Ç  ·0  ©Ì  §   §h  ¶   ¶q  ¼   ¼y
­H  ô¨   ´   ´°  ×   ×µ  {   {^  A   A{  Bç  B}  Cì  C\  DÇ  D0  EÌ  F   Fh  G   Gq  H   Hy  I   I½
üQ  ùõ°  }   }µ  J   J^  K   K{  Lç  L}  Mì  M\  NÇ  N0  OÌ  P   Ph  Q   Qq  R   Ry  ¹   ¹½  û   ûH
ÓY  Õ÷µ  S   S^  T   T{  Uç  U}  Vì  V\  WÇ  W0  XÌ  Y   Yh  Z   Zq  ²   ²y  Ô   Ô½  Ö   ÖH  Ò&  ÒQ
 8   3^  4   4{  5ç  5}  6ì  6\  7Ç  70  8Ì  9   9h  ³   ³q  Û   Ûy  Ü   Ü½  Ù   ÙH  Ú&  ÚQ   -   Y
      {   ç   }   ì   \   Ç   0   Ì       h       q       y       ½       H   &   Q   -   Y   ø   8
      }   ì   \   Ç   0   Ì       h       q       y       ½       H   &   Q   -   Y   ø   8
      \   Ç   0   Ì       h       q       y       ½       H   &   Q   -   Y   ø   8           °
ñ   ñ 0   Ì       h       q       y       ½       H  â&  âQ  ä-  äY  àø  à8  á   ã   ã°  å   åµ  ç
$ç  $+   +h  |   |q  &   &y  é   é½  ê   êH  ë&  ëQ  è-  èY  íø  í8  î   ï   ï°  ì   ìµ  ß   ß^  !
_ì  _-   -q  /   /y  Â   Â½  Ä   ÄH  À&  ÀQ  Á-  ÁY  Ãø  Ã8  Å   Ç   Ç°  Ñ   Ñµ  !   !^  ,   ,{  %ç
"Ç  "Ê   Êy  Ë   Ë½  È   ÈH  Í&  ÍQ  Î-  ÎY  Ïø  Ï8  Ì   `   `°  :   :µ  #   #^  @   @{  'ç  '}  =ì
jÌ  kd   d½  e   eH  f&  fQ  g-  gY  hø  h8  i   «   «°  »   »µ  ð   ð^  ý   ý{  þç  þ}  ±ì  ±\  °Ç
th  uo   oH  p&  pQ  q-  qY  rø  r8  ª   º   º°  æ   æµ  ¸   ¸^  Æ   Æ{  ¤ç  ¤}  µì  µ\  ~Ç  ~0  sÌ
§q  ¶y&  yQ  z-  zY  ¡ø  ¡8  ¿   Ð   Ð°  Ý   Ýµ  Þ   Þ^  ®   ®{  ^ç  ^}  £ì  £\  ¥Ç  ¥0  ·Ì  ©   ©h
Gy  H[-  [Y  ]ø  ]8  ¯   ¨   ¨°  ´   ´µ  ×   ×^  {   {{  Aç  A}  Bì  B\  CÇ  C0  DÌ  E   Eh  F   Fq
R½  ¹öø  ö8  ò   ó   ó°  õ   õµ  }   }^  J   J{  Kç  K}  Lì  L\  MÇ  M0  NÌ  O   Oh  P   Pq  Q   Qy
ÔH  Öú   ÿ   ÿ°  \   \µ  ÷   ÷^  S   S{  Tç  T}  Uì  U\  VÇ  V0  WÌ  X   Xh  Y   Yq  Z   Zy  ²   ²½
ÙQ  Ú0°  1   1µ  2   2^  3   3{  4ç  4}  5ì  5\  6Ç  60  7Ì  8   8h  9   9q  ³   ³y  Û   Û½  Ü   ÜH
 Y    µ       ^       {   ç   }   ì   \   Ç   0   Ì       h       q       y       ½       H   &   Q
 8    ^       {   ç   }   ì   \   Ç   0   Ì       h       q       y       ½       H   &   Q   -   Y
      {   ç   }   ì   \   Ç   0   Ì       h       q       y       ½       H   &   Q   -   Y   ø   8
ã   ã }   ì   \   Ç   0   Ì       h       q       y       ½       H   &   Q  â-  âY  äø  ä8  à   á
ì   ì¢\  .Ç  .0  <Ì  (   (h  +   +q  |   |y  &   &½  é   éH  ê&  êQ  ë-  ëY  èø  è8  í   î   î°  ï
!   !*0  )Ì  ;   ;h  ¬   ¬q  -   -y  /   /½  Â   ÂH  Ä&  ÄQ  À-  ÀY  Áø  Á8  Ã   Å   Å°  Ç   Çµ  Ñ
@ç  @?   ?h  ø   øq  É   Éy  Ê   Ê½  Ë   ËH  È&  ÈQ  Í-  ÍY  Îø  Î8  Ï   Ì   Ì°  `   `µ  :   :^  #
þì  þa   aq  b   by  c   c½  d   dH  e&  eQ  f-  fY  gø  g8  h   i   i°  «   «µ  »   »^  ð   ð{  ýç
µÇ  µl   ly  m   m½  n   nH  o&  oQ  p-  pY  qø  q8  r   ª   ª°  º   ºµ  æ   æ^  ¸   ¸{  Æç  Æ}  ¤ì
¥Ì  ·v   v½  w   wH  x&  xQ  y-  yY  zø  z8  ¡   ¿   ¿°  Ð   Ðµ  Ý   Ý^  Þ   Þ{  ®ç  ®}  ^ì  ^\  £Ç
Dh  E¼   ¼H  ½&  ½Q  ¾-  ¾Y  [ø  [8  ]   ¯   ¯°  ¨   ¨µ  ´   ´^  ×   ×{  {ç  {}  Aì  A\  BÇ  B0  CÌ
Oq  PI&  IQ  ­-  ­Y  ôø  ô8  ö   ò   ò°  ó   óµ  õ   õ^  }   }{  Jç  J}  Kì  K\  LÇ  L0  MÌ  N   Nh
Yy  Zû-  ûY  üø  ü8  ù   ú   ú°  ÿ   ÿµ  \   \^  ÷   ÷{  Sç  S}  Tì  T\  UÇ  U0  VÌ  W   Wh  X   Xq
³½  ÛÒø  Ò8  Ó   Õ   Õ°  0   0µ  1   1^  2   2{  3ç  3}  4ì  4\  5Ç  50  6Ì  7   7h  8   8q  9   9y
 H            °       µ       ^       {   ç   }   ì   \   Ç   0   Ì       h       q       y       ½
 Q    °       µ       ^       {   ç   }   ì   \   Ç   0   Ì       h       q       y       ½       H
 Y    µ       ^       {   ç   }   ì   \   Ç   0   Ì       h       q       y       ½       H   &   Q
â8  ä ^       {   ç   }   ì   \   Ç   0   Ì       h       q       y       ½       H   &   Q   -   Y
í   íå{  çç  ç}  ñì  ñ\  ¢Ç  ¢0  .Ì  <   <h  (   (q  +   +y  |   |½  &   &H  é&  éQ  ê-  êY  ëø  ë8
Å   Åß}  !ì  !\  $Ç  $0  *Ì  )   )h  ;   ;q  ¬   ¬y  -   -½  /   /H  Â&  ÂQ  Ä-  ÄY  Àø  À8  Á   Ã
`   `,\  %Ç  %0  _Ì  >   >h  ?   ?q  ø   øy  É   É½  Ê   ÊH  Ë&  ËQ  È-  ÈY  Íø  Í8  Î   Ï   Ï°  Ì
»   »'0  =Ì  "   "h       q  a   ay  b   b½  c   cH  d&  dQ  e-  eY  fø  f8  g   h   h°  i   iµ  «
R    ° >>>>FILE DESCRIPTOR        &          Ý             RPG       CLLCPGM   SERVER_M2 >>FILE DESC
LCPGMG       CLLCPGM   SERVER_M2 PROTOTYPESCLLCPGM   DATAQUEUESPROTOTYPESCLLCPGM   EXISTS_IFSRPGCOPY
S For  DVDTAARA  RPGCOPY   CLLCPGM   CIDTAARA  RPGCOPY   CLLCPGM   $SRVRM2_DSRPGCOPY   CLLCPGM   DOT
