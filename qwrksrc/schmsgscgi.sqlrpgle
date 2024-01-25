      *************************************************************************
      *  (C) Copyright 2014 Credentials Solutions, LLC
      *  All Rights Reserved
      *
      *     * * * *  Credentials Confidential Source * * * * * * *
      *  THIS PROGRAM IS AN UNPUBLISHED WORK FULLY PROTECTED BY THE
      *  UNITED STATES COPYRIGHT LAWS AND IS CONSIDERED A TRADE
      *  SECRET BELONGING TO THE COPYRIGHT HOLDER.
      *
      *\B Standard Backout Instructions:
      *\B   Re-compile prior version of program in PGMCOMPARE/RPG_PREV1
      *************************************************************************
      *
      *  PROGRAM-SCHMSGSCGI is a CGI for school staff to maintain order
      *                     form messages for both TP and GA
      *
      *  Author      : JWG
      *  Date Written: 09/18/2014
      *************************************************************************
      *  Change History:
      * -------- -----------------------------------------------------------
      * 07/07/20 SG  Allow JSON requests and Responses
      * 06/10/20 JWG Update rss help center links in wf and added test internal check
      * 02/18/20 BXM Added JavaScript for wordcount/notification CKEditor plugins
      *              to message_ADUP.html (the wordcount plugin is being used to
      *              display the character count and limit messages to 600 characters
      *              including HTML and spaces), added Go To Test/Prod link to footer
      *              for 'IT' and 'DOC' groups in message_ADUP.html, messageList.html,
      *              invalidPanel.html
      * 10/24/19 RPR Updated labels in HTML
      * 09/07/19 TDR Updated Privacy Policy Link
      *              Updated jQuery and Bootstrap Versions
      * 09/17/18 LCC Updated Company name, updated crm_name to equal the length of credUser.name
      * 08/30/18 BXM Changed Manage Messages screen title to Message Management
      * 05/07/18 JWG Added .org as on ok url type
      * 03/13/18 SAS Updating to jquery 3.3.1, bootstrap 4, rssFunt-1.1, RSSmain-1.1
      * 02/20/18 TDR Updating 'PANEL' to use web_processFile
      * 02/07/18 SAS Removing hard coded libraries
      * 01/04/18 SAS Adding staging information for compile
      * 12/08/17 JWG Removed it436test references and implement jquery 3.2.1 in WF
      * 10/18/17 LCC Removed the source ckeditor tool for non-IT users, HTML revisions
      * 08/23/17 SAS New UI, fixed how messages are going out to page
      *               added CRM information function
      * 04/17/17 SG  SenecaGlobal Inc. code delivery
      * 02/16/17 SG  1) Externalised html data from compile time arrays
      *                 into IFS
      *              2) Removed hardcoding of library names
      *              3) Added MODWEB function calls to facilitate processing
      *                 of IFS files
      * 01/27/17 DDZ Added support for D2 messages
      * 08/02/16 TDR Extended length of anchorTag to 200
      *              Added error message when hyperlink is too long
      * 04/11/16 HAD Added a button for viewing order form
      * 03/07/16 HAD Removed checkTextCount Message usage
      * 02/26/16 HAD Added .mil and credentials-inc.com to the Url extn
      *              Whitelist
      * 01/03/16 HAD Added a Sub-Routine validateHtml
      * 12/22/15 HAD Added a new Sub-Routine sanitizeHtml for validating
      *              and stripping invalid HTML Tags
      * 12/21/15 HAD Changed the School Banner Message edit from textarea
      *              to CKEditor
      * 12/11/14 JWG Fixed date on and off edits to either have dates or
      *              not have dates
      * 11/13/14 JWG Added re-seq of messages after a delete is performed
      *              Fixed up some of the ui for ease of use
      * 09/18/14 JWG New CGI Program
      *************************************************************************
     Hoption(*NOXREF:*NODEBUGIO:*SRCSTMT)
     Hthread(*SERIALIZE)
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('CIBINDDIR':'QC2LE':'YAJL')

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Files

     fcimessag1 uf a e           k disk    usropn
     fdvclientf1uf   e           k disk    usropn
     fdvclientf2if   e           k disk    usropn
     fdvinstitf1if   e           k disk    usropn

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Copy Books (Declaration)

       // These constants profile the fields in parsinp_ds
       dcl-c HTML_NAME_LEN 30;
       dcl-c HTML_DATA_LEN 600;
       dcl-c MAX_INPUT_DIM 50;
       dcl-c BUFIN_LEN 5120;
       dcl-c STAGING_DIR 's/';

      /copy qsysinc/qrpglesrc,jni
      /copy rpgcopy,cllcpsds
      /copy rpgcopy,dvdtaara
      /copy rpgcopy,parsinp_ds
      /copy rpgcopy,messageds
      /copy rpgcopy,parsedsbas
      /copy rpgcopy,parseds999
      /copy rpgcopy,$srvrem_ds
      /copy rpgcopy,zhskpg_ds
      /copy rpgcopy,ifshead
      /copy rpgcopy,java

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Prototypes

      /copy prototypes,APIGetEnv
      /copy prototypes,APIStdIn
      /copy prototypes,APIStdOut
      /copy prototypes,crmfunc
      /copy prototypes,DATE_FMTS
      /copy prototypes,java
      /include prototypes,web
      /copy prototypes,setenvvar
      /include prototypes,zsystemcmd
      /copy prototypes,zmhrcvpm

     d proto_ver       s            200a
     d ssl_cipher      s            200a
     d httpsonoff      s            200a
     d httpscipher     s            200a

     d plen            s              3p 0 dim(65) inz

     d CImsgKeys       ds                  likerec(messagerec:*key)
     d CImsgs          ds                  likerec(messagerec) dim(30)

     d MsgSeq          ds                  qualified dim(3)
     d  msgsect                       2a
     d  msgseqnce                     3p 0

      * - - - - - - - - - - - - - ------ - - - - - - - - - - - - - - - - - - -
      * Java Variables

     d jData           s                   like(jString)
     d jRegex          s                   like(jString)
     d jReplace        s                   like(jString)
     d jResult         s                   like(jString)
     d jPattern        s               o   CLASS(*JAVA
     d                                         : 'java.util.regex.Pattern')
     d jMatcher        s               o   CLASS(*JAVA
     d                                         : 'java.util.regex.Matcher')

     d jCharSeq        s               o   CLASS(*JAVA
     d                                         : 'java.lang.CharSequence')

     d HtmlParser_getIllegalTagIndex...
     d                 pr            10i 0
     d                                     EXTPROC(*JAVA:
     d                                      'HtmlParser':
     d                                      'getIllegalTagIndex')
     d                                     static
     d htmlStr                   200000a   const varying

     d HtmlParser_hasIllegalTags...
     d                 pr              n
     d                                     EXTPROC(*JAVA:
     d                                      'HtmlParser':
     d                                      'hasIllegalTags')
     d                                     static
     d htmlStr                   200000a   const varying

     d HtmlParser_sanitizeHtml...
     d                 pr        200000a   varying
     d                                     EXTPROC(*JAVA:
     d                                      'HtmlParser':
     d                                      'sanitizeHtml')
     d                                     static
     d htmlStr                   200000a   const varying

      * options for all <select> tags
     dscrn_size        c                   2
     dscrn_id          s              3a   dim(scrn_size) ctdata
     dscrn_txt         s             30a   dim(scrn_size) alt(scrn_id)

     dsect_size        c                   1
     dsect_id          s              2a   dim(sect_size) ctdata
     dsect_txt         s             30a   dim(sect_size) alt(sect_id)

     dlang_size        c                   1
     dlang_id          s              1a   dim(lang_size) ctdata
     dlang_txt         s             20a   dim(lang_size) alt(lang_id)

     dstat_size        c                   2
     dstat_id          s              1a   dim(stat_size) ctdata
     dstat_txt         s             30a   dim(stat_size) alt(stat_id)

     dctgy_size        c                   3
     dctgy_id          s              1a   dim(ctgy_size) ctdata
     dctgy_txt         s             30a   dim(ctgy_size) alt(ctgy_id)

      * - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - -
      * Error and Confirmation Variables

     d ei              s              5p 0 inz
     d errs            s             90a   dim(40) inz
     d errs_sect       s              2a   dim(40) inz

     d e1              s              3p 0 inz
     d ci              s              3p 0 inz
     d conf            s             96a   dim(40) inz

      * - - - - - - - ------- - - - - - - - - - - - - - - - - - - - - - - - - -
      * HTML specific variables

     d hchguser        s             10a
     d hdateoff        s             10a
     d hdateon         s             10a
     d hdescrp         s             50a
     d heditMode       s              3a
     d hmsg_seq        s              3a
     d hscrn_id        s              3a   inz
     d hscrn_sect      s              2a   inz
     d htext           s            999a

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * CGI specific variables

      * Variables for the CGI interface API for QtmhWrStout.
     dbufOut           s          10240a   inz
     dbufOutln         s              9b 0
     dbufrTrip         s             10i 0 inz(1700)

      * Define line feed that is required when writing data to std output.
     dLINEFEED         c                   x'15'
     dbreak            c                   '<br>'

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Data Structures

     d@status          ds                  likeds(statusDs)
        dcl-ds credUser likeds(credUser_t);

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Constants

     dFALSE            c                   '0'
     dTRUE             c                   '1'

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Filepath variables used for locating html files

     drootFilePath     s            100a
     dpanel            s             10a
     dpanelFile        s             25a

     dDOT_HTML         c                   '.html'
     dDOT_JSON         c                   '.json'

     dWF_FILEPATH      c                   '/wf/'
     dPROD_DIR         c                   'p/'
     dTEST_DIR         c                   't/'
     dSLASH            c                   '/'

     dPROD_NAME        c                   'SCHMSGSCGI'
       dcl-c TEST_NAME 'SCHMSGCGIT';

     dcrtd_yyyy        c                   '2014'

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Local Variables

     dcondition        s             10a
     dpageLang         s              2a   inz(' ')

     danchorTag        s            200a
     d#dateon          s              9p 0 inz
     d#dateoff         s              9p 0 inz

     d@appl_name       s             10a   inz
     d@prog_name       s             14a   inz

     daccessD2         s               n   inz(FALSE)
     daccessTp         s               n   inz(FALSE)

     dbyte3            s              3a
     dbyte5            s              5a
     dbyte1            s              1a
     dbyte10           s             10a
     dbyte999          s            999a

     dcopyRight        s             10a
     dcrm_id           s             10a
     dcrm_info         s            175a
     dcrm_name         s             40a
     dcrm_tel#         s             12a
     dcrm_email        s             29a

     d                 ds                  inz
     d cycle_date              1      8s 0
     d cycle_y4mm              1      6
     d cycle_time      s              6s 0 inz

     derrMsg           s          50000a

     dhasIllegalTag    s               n   inz
     dhex0D0A          s              2a   inz(x'0D25')
     dhits#_sql        s              3p 0 inz

     dhtmlTitle        s             75a   varying inz

     dhtmlDS           ds             9    inz
     d htmlPos                 1      3a
     d htmlLen                 4      6a
     d htmlStr                 7      9a

     d                 ds
     d htmlstmt                     100a   inz
     d html_1to8                      8a   overlay(htmlstmt:1)
     d html_1to12                    12a   overlay(htmlstmt:1)
     d html_msgs                     13a   overlay(htmlstmt:9)

       dcl-s httpAccept char(10) inz(*blank);

     didx              s              9p 0 inz
     didx2             s              9p 0 inz
     dillegalTags      s               n
     disTest           s               n
     di1               s              3p 0 inz

     dJSfunc           s            100a   inz
     djsIdx            s              9p 0 inz

     dlabelID          s             50a
     dlen              s              3p 0 inz
     dlidx             s              3p 0 inz
     dloopcnt          s              3p 0 inz

     dmessgCnt         s              3p 0 inz
     dmsg_seq          s              3p 0 inz
     dMsgAddSeq        s              3p 0 inz
     dm1               s              3p 0 inz

     dnewSeq           s              3p 0 inz
     doldSeq           s              3p 0 inz
     dorderFormUrl     s                   like(htmlStmt)

     dpanel_ttl        s             25a
     dParmCnt          s              3p 0 inz
       dcl-s point2Staging ind inz(FALSE);
     dpos              s              3p 0 inz
     dprev_sect        s              3a

     dregex            s            100a

     dsanitizedText    s                   like(htext)
     dscrnIdx          s              5p 0 inz
     dsectIdx          s              3p 0 inz
     dslctIdx          s              3p 0 inz
     dskip_it          s              1a
     dslct_type        s             10a
     dslct_size        s              3p 0 inz
     dslct_vals        s             10a   dim(20)
     dslct_text        s             50a   dim(20)
     dStartIdx         s              3p 0 inz
     dstrtTag          s            200a
     dstrt             s              3p 0 inz(0)
     dend              s              3p 0 inz(0)
     ds1               s              3p 0 inz

     dtagLen           s              3p 0 inz
     dtagStrt          s              3p 0 inz
     dtagStart         s             10i 0
     dtempMsgTxt       s                   like(field_data)
     dtd1              s           1000a
     dtd2              s            800a
     dtheValue         s             10a

     dinvldChar        s               n

     dwhattodo         ds            75
     d whattodo6               1      6a

     dwrkSeq           s              3p 0 inz

     dyschname         s             50a

     donlydst          s              3a   inz('DDZ')
     dhtmlErrPos       s              2a   inz

       // *********************************************************************
       // Start of CGI Program execution section...
       // *********************************************************************

       // if coming from *PSSR write a panel and end
       if panel = '#FATALERR ';
          panelFile = 'FatalError.html';
          ei += 1;
          errs(ei) = 'FATAL EXCEPTION OCCURRED';
          condition = 'FATALERR';
       else;
          exsr zcgihskpg;
          // Get the Accept HTTP Header option (set for JSON responses).
          httpAccept = getHttpAccept();
          clear condition;

          if %subst(EResp:1:3) = 'GET';
             exsr do_get;
          else;
             exsr do_post;
          endif;
       endif;

       clear h_action;

       if hscrn_next = '303'; // redirected to a different URL
       else;
          exsr setrootpath;
          // if client accepts json, change panelFile to json suffix
          if httpAccept = 'json';
             panelFile = setPanelToJSON(panelFile:httpAccept);
          endif;
          web_processFile(%trim(rootfilepath):
                          panelFile:
                          %paddr('GETVARVALUES'):
                          %paddr('PROCESSIF'):
                          %paddr('PROCESSLOOP'):
                          %paddr('PROCESSSWITCH'));
       endif;

       close *all;
       return;

       // *********************************************************************
       // Handle "GET" == Process Whattodo LIST
       // *********************************************************************
       begsr do_get;

       whattodo = bufin;
       whattodo = %xlate(x'00':' ':whattodo);
       whattodo = %xlate('*':' ':whattodo);

       select;
       when whattodo6 = 'MSGLST';       // Coming from DVCGIRSS
          h_applid = %subst(whattodo:7:2);
          hfice = %subst(whattodo:9:6);

          hsignonkey = %subst(whattodo:15:12);
          huserid = %subst(whattodo:27:10);

          panelFile = 'messageList' + DOT_HTML;
          hscrn_next = 'SML';
          exsr rtn_MsgList;

          exsr checksign;
          if ei > 0;
             panelFile = 'invalidPanel' + DOT_HTML;
             hscrn_next = 'INV';
          endif;

          exsr get_schnam;

          exsr setMessageVars;
          hlanguage = 'E';

          clear heditmode;
          CImsgKeys.jm_fice = hfice;
          CImsgKeys.jm_applid = h_applid;
          CImsgKeys.jm_scrn_id = hscrn_id;
          CImsgKeys.jm_scrsect = hscrn_sect;

       other;

       endsl;

       endsr;

       // *********************************************************************
       // Handle "POST" == Run parseinp and process screens
       // *********************************************************************
       begsr do_post;

       %subst(parea:2) = bufin;
       exsr parseInp;

       clear ei;
       clear errs;
       clear errs_sect;

       exsr checkSign;
       if ei > 0;
          panelFile = 'invalidPanel' + DOT_HTML;
          hscrn_next = 'INV';
          leavesr;
       endif;

       exsr get_schnam;

       select;

       // Actions performed on all screens are checked first
       when h_action = 'SML';          // School Message List
          panelFile = 'messageList' + DOT_HTML;
          exsr rtn_MsgList;
          hscrn_next = 'SML';
          clear heditmode;

          exsr setMessageVars;

       // Screen Panel Processing
       when hscrn_type = 'SML';        // School Message List
          panelFile = 'messageList' + DOT_HTML;
          exsr rtn_MsgList;
          hscrn_next = 'SML';

          select;
          when h_action = 'ANM';
             panelFile = 'message_ADUP' + DOT_HTML;
             hscrn_next = 'ANM';
             heditMode = 'ADD';
             hdateon = '0';
             hdateoff = '0';

          when h_action = 'UPM';
             panelFile = 'message_ADUP' + DOT_HTML;
             hscrn_next = 'UPM';
             heditMode = 'UPD';
             exsr get_CImsg;

          when h_action = 'UMS';       // Update status of message
             exsr CI_UpdMsg;

          when h_action = 'DLT';       // Delete Message
             exsr CI_DltMsg;
             if ci > 0;
                // exsr CI_ReSeqMsgs;
             endif;

          endsl;

       when hscrn_type = 'ANM';        // Add New School Message
          panelFile = 'message_ADUP' + DOT_HTML;
          hscrn_next = 'ANM';

          if h_action = 'ADD';
             exsr edit_msg;
             if ei = 0;
                exsr CI_AddMsg;
                if ei = 0;
                   panelFile = 'messageList' + DOT_HTML;
                   exsr rtn_MsgList;

                   hscrn_next = 'SML';
                endif;
             endif;
          endif;

       when hscrn_type = 'UPM';        // Update School Message
          panelFile = 'message_ADUP' + DOT_HTML;
          hscrn_next = 'UPM';
          if h_action = 'UPD';
             exsr edit_msg;
             if ei = 0;
                exsr CI_UpdMsg;
                if ei = 0;
                   panelFile = 'messageList' + DOT_HTML;
                   exsr rtn_MsgList;

                   hscrn_next = 'SML';
                   leavesr;
                endif;
             endif;
          endif;

       endsl;

       endsr;

       /copy rpgcopy,$PSSR_SR
       /copy rpgcopy,$srvrem_sp

       // *********************************************************************
       // *INZSR: Program initialization Routine
       // *********************************************************************
       begsr *inzsr;

       @appl_name = %trim(pgm_name);
       @prog_name = %trim(@appl_name) + '.pgm';

       in dvdtaara;

       msg_defalt = 'SM@';
       msg_file = ('DVMESSAGES*LIBL     ') ;

       cycle_date = %dec(%date():*iso);
       cycle_time = %dec(%time():*iso);

       if pgm_name = PROD_NAME;
          isTest = FALSE;
       else;
          isTest = TRUE;
       endif;

       // DO NOT REMOVE: required for calling java
       //   set environment variable for java classpath
       /copy rpgcopy,putenvcall

       endsr;

       // *********************************************************************
       // SetRootPath: Construct according to page langauge criteria
       // *********************************************************************
       begsr setRootPath;

       // if user is signed for first time, set page language to user
       // preferance option

       //!! below code is commented out as multi language POC changes
       //   are not required for this delivery
       //if pageLang = *blanks;
       //   pageLang = ccontract#;
       //endif;

       if pageLang = ' ';
          pageLang = 'EN';
       endif;

       //this boolean gets set in crtcipgm2 on a CP of the program
       if point2Staging;
          rootFilepath = WF_FILEPATH + STAGING_DIR + PROD_NAME + SLASH +
                         pageLang + SLASH;

       elseif isTest = TRUE;
          rootFilePath = WF_FILEPATH + TEST_DIR + PROD_NAME + SLASH + pageLang +
                         SLASH;
       else;
          rootFilePath = WF_FILEPATH + PROD_DIR + PROD_NAME + SLASH + pageLang +
                         SLASH;
       endif;

       endsr;

       // **********************************************************************
       // CI_AddMsg: Add a New Message
       // **********************************************************************
       begsr CI_AddMsg;

       exsr get_MsgCnt;

       messgCnt += 1;
       if messgCnt > 10;
          leavesr;
       endif;

       if not %open(cimessag1);
          open cimessag1;
       endif;

       msg_seq = messgCnt * 10;
       CImsgKeys.jm_msg_seq = msg_seq;

       chain(n) %kds(CImsgKeys:6) messagerec;
       if %found(cimessag1);
          leavesr;
       endif;

       jm_fice = hfice;
       jm_applid = h_applid;
       jm_scrn_id = hscrn_id;
       jm_scrsect = hscrn_sect;
       jm_msg_seq = msg_seq;
       jm_lang = hlanguage;
       jm_status = hstatus;
       jm_dateon = %int(hdateon);
       jm_dateoff = %int(hdateoff);
       jm_categry = %trim(hcategry);
       jm_descrp = %trim(hdescrp);
       jm_text = %trim(%scanrpl('%0D%0A':'<br>':htext));
       jm_chgdate = cycle_date;
       jm_chguser = huserid;

       write(e) messagerec;
       if %error;
          ei += 1;
          errs(ei) = 'Error Adding Message';
          leavesr;
       endif;

       ci += 1;
       conf(ci) = %trim(hdescrp) + ' Message Added Successfully';

       endsr;

       // **********************************************************************
       // CI_DltMsg: Update a Message
       // **********************************************************************
       begsr CI_DltMsg;

       if not %open(cimessag1);
          open cimessag1;
       endif;

       chain %kds(CImsgKeys:6) messagerec;
       if %found(cimessag1);
          hdescrp = jm_descrp;
          delete messagerec;

          ci += 1;
          conf(ci) = %trim(hdescrp) + ' Message Deleted Successfully';
       endif;

       exsr get_MsgCnt;

       if messgCnt > 0;
          exec sql
             update cimessag1
                set jm_msg_seq = (incr(0) * 10)
                where jm_fice = :hfice and jm_applid = :h_applid and
                   jm_scrn_id = :hscrn_id and jm_scrsect = :hscrn_sect and
                   jm_lang = :hlanguage;
       endif;

       endsr;

       // **********************************************************************
       // CI_ReSeqMsgs: Re-Sequence messages after a delete is performed
       // **********************************************************************
       begsr CI_ReSeqMsgs;

       if not %open(cimessag1);
          open cimessag1;
       endif;

       setll %kds(CImsgKeys:4) messagerec;
       if not %equal(cimessag1);
          leavesr;
       endif;

       newSeq = 0;
       i1 = 0;
       reade(n) %kds(CImsgKeys:4) messagerec;
       dow not %eof(cimessag1);
          i1 += 1;
          newSeq = (i1 * 10);
          oldSeq = jm_msg_seq;

          if oldSeq <> newSeq;
             exec sql
                update cimessag1
                   set jm_msg_seq = :newseq
                   where jm_fice = :hfice and jm_applid = :h_applid and
                      jm_scrn_id = :hscrn_id and jm_scrsect = :hscrn_sect and
                      jm_msg_seq = :oldseq and jm_lang = :hlanguage;
          endif;

       reade(n) %kds(CImsgKeys:4) messagerec;
       enddo;

       endsr;

       // **********************************************************************
       // CI_UpdMsg: Update a Message
       // **********************************************************************
       begsr CI_UpdMsg;

       if not %open(cimessag1);
          open cimessag1;
       endif;

       chain %kds(CImsgKeys:6) messagerec;
       if %found(cimessag1);
          // these fields are ALWAYS updated
          jm_chgdate = cycle_date;
          jm_chguser = huserid;

          // Screen based update
          select;
          when hscrn_type = 'SML';

             select;
             when jm_dateoff >= cycle_date and hstatus = 'Y';
                jm_dateon = cycle_date;

             when hstatus = 'N' and
                  ((jm_dateon <= cycle_date and jm_dateoff >= cycle_date) or
                   (jm_dateoff <= cycle_date));
                jm_dateoff = 0;
                jm_dateon = 0;

             when hstatus = 'N';
                jm_dateoff = 0;
                jm_dateon = 0;

             endsl;

             ci += 1;
             if hstatus = 'Y';
                conf(ci) = %trim(jm_descrp) + ' has been turned ON';
             else;
                conf(ci) = %trim(jm_descrp) + ' has been turned OFF';
             endif;
             jm_status = hstatus;

          when hscrn_type = 'UPM';
             jm_status = hstatus;
             jm_categry = %trim(hcategry);
             jm_descrp = %trim(hdescrp);
             jm_text = %trim(%scanrpl('%0D%0A':'<br>':htext));
             //jm_text = %trim(%scanrpl('%0D%0A':'':htext));
             jm_dateon = %int(hdateon);
             jm_dateoff = %int(hdateoff);

          endsl;

          update messagerec;

       endif;

       endsr;

       // **********************************************************************
       // CheckSign: Check the SIGNONKEY value vs. the dvclientf1
       // **********************************************************************
       begsr CheckSign;

       if not %open(dvclientf1);
          open dvclientf1;
       endif;

       chain (huserid) client;
       select;
       when not %found(dvclientf1);
          ei += 1;
          errs(ei)  = '#7801';

       //if they are not the same and the csignonkey is blank, then
       //the user was inactive for more than x minutes so give a
       //different message for this than for a mismatch.
       when hsignonkey <> csignonkey and csignonkey = *blanks;
          unlock dvclientf1;
          ei += 1;
          errs(ei)  = '#7802';

       //if they are not the same and the csignonkey is not blank,
       //then something else is going on.  Either the user has backed
       //up into a prior session or someone else is trying to get on.
       when hsignonkey <> csignonkey;
          unlock dvclientf1;
          ei += 1;
          errs(ei) = '#7802';

       other;
          clastactd = cycle_date;
          clastactt = cycle_time;
          update client;

       endsl;

       if not %open(dvclientf2);
          open dvclientf2;
       endif;

       chain (huserid) authority;
       if not %found(dvclientf2);
          clear cau_tp;
       endif;

       endsr;

       // **********************************************************************
       // Edit_Dates:
       // **********************************************************************
       begsr Edit_Dates;

       #dateon = %int(hdateon);
       #dateoff = %int(hdateoff);

       // Date on requires a date off
       if #dateon > 0 and #dateoff = 0;
          ei += 1;
          errs(ei) = '#7607';
          errs_sect(ei) = 'MO';
       endif;

       select;
       when hstatus = 'Y';
       // Both these conditions force the date on to be today
       // 1.) date on and off are set to future dates
       // 2.) date off is set to a future date or today and date on was not set
          if (#dateon > cycle_date and #dateoff > #dateon) or
             (#dateon = 0 and #dateoff >= cycle_date);
             hdateon = %char(cycle_date);
          endif;

       when hstatus = 'N';
          if #dateon > cycle_date and #dateoff > #dateon;
          // Do nothing, this is allowed because the message either has never
          // been turned on yet, or the dates started out as 0 and the user
          // is setting up a new message to be turned on at a later date
          elseif #dateon = 0 and #dateoff >= cycle_date;
          // if turning the msg to off or leaving it off with a future off
          // date will require an on date
             ei += 1;
             errs(ei) = '#7608';
             errs_sect(ei) = 'MO';

          else;
          // Always turn dates off when user turns a msg off when date on is
          // set to today or prior
             hdateon = '0';
             hdateoff = '0';
          endif;

       endsl;

       // if the date on is set to today, force message to be on
       if #dateon = cycle_date;
          hstatus = 'Y';
       endif;

       endsr;

       // **********************************************************************
       // Edit_Msg:
       // **********************************************************************
       begsr Edit_Msg;

       if hstatus = ' ';
          ei += 1;
          errs(ei) = '#7605';
          errs_sect(ei) = 'MO';
       endif;

       if hcategry = ' ';
          ei += 1;
          errs(ei) = '#7601';
          errs_sect(ei) = 'MO';
       endif;

       exsr edit_Dates;

       if hdescrp = *blanks;
          ei += 1;
          errs(ei) = '#7606';
          errs_sect(ei) = 'MO';
       endif;

       if htext = *blanks;
          ei += 1;
          errs(ei) = '#7611';
          errs_sect(ei) = 'MT';
          leavesr;
       endif;

       htext = %scanrpl('&#60;':'&lt;':htext);
       htext = %scanrpl('&#62;':'&gt;':htext);

       htext = %scanrpl('&lt;':'<':htext);
       htext = %scanrpl('&gt;':'>':htext);

       exsr validateLinkTags;

       if invldChar = TRUE;
          if not %open(cimessag1);
             open cimessag1;
          endif;

          chain %kds(CImsgKeys:6) messagerec;
          if %found(cimessag1);
             htext = jm_text;
          endif;

          ei += 1;
          errs(ei) = '#7616';
          errs_sect(ei) = 'MT';
          htext = %scanrpl('%0D%0A':'':htext);
          leavesr;
       endif;

       exsr validateHtml;

       if illegalTags;

          exsr sanitizeHtml;
          htext = %trim(sanitizedText);
          ei += 1;
          errs(ei) = '#7612';
          errs_sect(ei) = 'MT';
          htext = %scanrpl('%0D%0A':'':htext);
          leavesr;

       endif;

       //Translate '--' to '' because '--' is used for commenting SQL Stmts
       if %scan('--':htext) > 0;
          htext = %scanrpl('--':'':htext);
          invldChar = TRUE;
       endif;

       if invldChar = TRUE;
          ei += 1;
          errs(ei) = '#7612';
          errs_sect(ei) = 'MT';
       endif;

       exsr validateAnchorTag;
       if ei > 1;
          leavesr;
       endif;

       htext = %scanrpl('%0D%0A':'':htext);

       endsr;

       // *********************************************************************
       // get_CImsg:
       // *********************************************************************
       begsr get_CImsg;

       if not %open(cimessag1);
          open cimessag1;
       endif;

       chain %kds(CImsgKeys:6) messagerec;
       if %found(cimessag1);
          hstatus = jm_status;
          hdateon = %trim(%char(jm_dateon));
          hdateoff = %trim(%char(jm_dateoff));
          hcategry = jm_categry;
          hdescrp = jm_descrp;
          htext = %scanrpl('<br>':hex0D0A:jm_text);
         // htext = %scanrpl('':hex0D0A:jm_text);
          hchgdate = %char(jm_chgdate);
          hchguser = jm_chguser;

       endif;

       endsr;

       // *********************************************************************
       // get_MsgCnt:
       // *********************************************************************
       begsr get_MsgCnt;

       exec sql
          select count(*)
             into :messgcnt
             from cimessag1
             where jm_fice = :hfice and jm_applid = :h_applid and jm_scrn_id
                = :hscrn_id and jm_scrsect = :hscrn_sect and jm_lang =
                :hlanguage;

       endsr;

       // *********************************************************************
       // get_schnam:   Get the School Name
       // *********************************************************************
       begsr get_schnam;

          if not %open(dvinstitf1);
             open dvinstitf1;
          endif;

          chain (hfice) instrec;
          if not %found(dvinstitf1);
             clear ithe_featr;
             clear ischname;
             clear iteam_id;
          endif;

          if ithe_featr = 'T';
             yschname = 'The ' + %trim(ischname);
          else;
             yschname = ischname;
          endif;

          crm_id = iteam_id + 'CCS';

          if ichargetm <> 'G' and ichargetm <> ' ';
             accessTp = TRUE;
          endif;

          if icharged2 <> 'G' and icharged2 <> ' ';
             accessD2 = TRUE;
          endif;

          if hfice = '009998' or hfice = '009999' or hfice = 'PPBJW1';
             accessD2 = TRUE;
             accessTp = TRUE;
          endif;

          // get CRM information
          //SG01 credUser = crmfunc_getUser(iteam_id);
          if credUser.name = *blanks;
             //SG01 credUser = crmfunc_getUser(itech_id);
          endif;

          crm_name  = credUser.name;
          crm_tel#  = credUser.phone;
          crm_email = credUser.email;

          if crm_name = *blanks;
             crm_name = 'Credentials Solutions, LLC';
          endif;

          if crm_tel# = *blanks;
             crm_tel# = '847-716-3005';
          endif;

       endsr;

       // *********************************************************************
       // subroutine to parse the input buffer into PAR array
       // and then load working "h" variables with initial values.
       // Those fields that are processed in Upper Case only are
       // converted from lower case to upper case by this routine.
       // *********************************************************************
       begsr parseinp;

       clear htmlForm;

       if httpAccept = 'json';
          // parseJSON will use the ContentType. of text/json
           if parseJSON(htmlForm : bufin : bufinLn : MAX_INPUT_DIM) < 0;
              // This is an error parsing

           endif;
       else;
          htmlForm = parseForm(bufIn:curContentType);
       endif;

       for pw = 1 to htmlForm.numInputs;
          field_name = htmlForm.input(pw).name;

          //! the sanitize does nothing - just a return
          //! the if to else should be removed.
       //   if field_name <> 'MSG_DATEON' and
       //      field_name <> 'MSG_DATEOFF' and
       //      field_name <> 'MSG_TEXT';
       //
       //      field_data = htmlForm.input(pw).data;
       //      field_data = %scanrpl('"':'':field_data);
       //      field_data = sanitize(field_data);
       //   else;
             field_data = htmlForm.input(pw).data;
             field_data = %scanrpl('"':'':field_data);
       //   endif;

          //! field_len not used
          field_len  = %len(%trim(field_data));

          select;

          when field_name = *blanks;

          when field_name = 'FORMID';
             hscrn_type = field_data;

          when field_name = 'ACTION';
             h_action = field_data;

          when field_name = 'SK';
             hsignonkey = field_data;

          when field_name = 'USERID';
             huserid = %xlate(xlc:xuc:field_data);

          when field_name = 'FICE';
             hfice = %xlate(xlc:xuc:field_data);
             CImsgKeys.jm_fice = hfice;

          when field_name = 'APPLID';
             h_applid = %xlate(xlc:xuc:field_data);
             CImsgKeys.jm_applid = h_applid;

          when field_name = 'MODE';
             heditmode = %xlate(xlc:xuc:field_data);

          when %subst(field_name:1:6) = 'STATUS';
             wrkSeq = %int(%subst(field_name:7:3));
             //! This requires the variable MSG_SEQ to be read before
             //! the STATUS
             if h_action = 'UMS' and wrkSeq = CImsgKeys.jm_msg_seq;
                hstatus = %xlate(xlc:xuc:field_data);
             endif;

          when field_name = 'MSG_SCRNID';
             hscrn_id = %xlate(xlc:xuc:field_data);
             CImsgKeys.jm_scrn_id = hscrn_id;

          when field_name = 'MSG_SCRSECT';
             hscrn_sect = field_data;
             CImsgKeys.jm_scrsect = hscrn_sect;

          when field_name = 'MSG_LANG';
             hlanguage = field_data;
             CImsgKeys.jm_lang = hlanguage;

          when field_name = 'MSG_SEQ';
             hmsg_seq = field_data;
             if %check('0123456789':hmsg_seq) > 0;
                hmsg_seq = '000';
             endif;
             CImsgKeys.jm_msg_seq = %int(hmsg_seq);

          when field_name = 'MSG_STATUS';
             hstatus = %xlate(xlc:xuc:field_data);

          when field_name = 'MSG_CATEGRY';
             hcategry = %xlate(xlc:xuc:field_data);

          when field_name = 'MSG_DATEON';
             byte10 = %trim(field_data);
             hdateon = FrmtDate(byte10:'MM/DD/YYYY':'YYYYMMDD');
             if hdateon = 'ERROR';
                hdateon = '0';
             endif;

          when field_name = 'MSG_DATEOFF';
             byte10 = %trim(field_data);
             hdateoff = FrmtDate(byte10:'MM/DD/YYYY':'YYYYMMDD');
             if hdateoff = 'ERROR';
                hdateoff = '0';
             endif;

          when field_name = 'MSG_DESCRP';
             hdescrp = web_sanitize(%trim(field_data));
             hdescrp = %scanrpl('--':'':hdescrp);

          when field_name = 'MSG_TEXT';
             htext = %trim(field_data);

          other;

          endsl;

       endfor;

       endsr;

       // *********************************************************************
       // rtn_MsgList:
       // *********************************************************************
       begsr rtn_MsgList;

       lidx = %lookup(hscrn_id:scrn_id);
       if lidx = 0;
          leavesr;
       endif;

       exsr get_MsgCnt;

       endsr;

       // *********************************************************************
       // santizizeHtml: Sub-Routine to sanitize Html Tags
       // *********************************************************************
       begsr sanitizeHtml;

       monitor;
          sanitizedText = HtmlParser_sanitizeHtml(%trim(htext));
       on-error;
          // Java exception is returned in message RNX0301
          callp qmhrcvpm(MsgInfo: %size(MsgInfo): 'RCVM0100':
                         '*': *zero: '*LAST': *blanks: *zero: '*REMOVE':
                         MsgErrCode);
          errMsg = MsgInfo;
       endmon;

       endsr;

       // *********************************************************************
       //  setMessageVars: Set the variables for messages based on applid
       // *********************************************************************
       begsr setMessageVars;

       select;
       when h_applid = 'TP' and accessTp;
          hscrn_id = 'HLP';
          hscrn_sect = '01';
       when h_applid = 'D2' and accessD2;
          hscrn_id = 'ORD';
          hscrn_sect = '01';
       other;
          panelFile = 'invalidPanel' + DOT_HTML;
          hscrn_next = 'INV';
          ei += 1;
          if h_applid = 'TP';
             errs(ei) = 'Access to messages for transcripts is unavailable';
          elseif h_applid = 'D2';
             errs(ei) = 'Access to messages for eduCheck is unavailable';
          else;
             errs(ei) = 'Access to messages is unavailable';
          endif;
       endsl;

       endsr;

       // *********************************************************************
       // validateAnchorTag - edit <a> tag in the Message Text to add target
       //                           attribute
       // *********************************************************************
       begsr validateAnchorTag;

       strt = %scan('<a ': %trim(htext));

       if htext = *blanks or strt = 0;
          leavesr;//Does not contain an <a> Tag
       endif;

       dow strt > 0;
          end  = %scan('</a>':%trim(htext):strt);

          if end = 0;
             ei += 1;
             errs(ei) = '#7615';
             errs_sect(ei) = 'MT';
             %subst(htext:strt) = *blanks;
             leavesr;
          endif;

          tagLen = end + 4 - strt;

          if tagLen > %len(anchorTag);
             ei += 1;
             errs(ei) = '#7617';
             errs_sect(ei) = 'MT';
             htext = %subst(htext:1:strt-1) + %subst(htext:end+4);
             leavesr;
          endif;

          anchorTag = %subst(htext:strt:tagLen);

          tagLen = %scan('>':%trim(anchorTag)) -
                   %scan('<a ': %trim(anchorTag)) + 1;

          strtTag = %subst(anchorTag:1:tagLen);

          strtTag = %xlate(xuc:xlc:strtTag);

          if %scan('target="_blank"':strtTag) > 0 and
             (%scan('https://':strtTag) > 0 or %scan('http://':strtTag) > 0) and
             %scan('href="':strtTag) = 0;

             ei += 1;
             errs(ei) = '#7613';
             errs_sect(ei) = 'MT';
             htext = %scanrpl(%trim(anchorTag):'':htext:strt);
             leavesr;

          endif;

          if %scan('.edu':strtTag) = 0 and
             %scan('.org':strtTag) = 0 and
             %scan('.mil':strtTag) = 0 and
             %scan('credentials-inc.com':strtTag) = 0;

             ei += 1;
             errs(ei) = '#7614';
             errs_sect(ei) = 'MT';
             htext = %scanrpl(%trim(anchorTag):'':htext:strt);
             leavesr;

          endif;

          strt = %scan('<a ':%trim(htext):end + 1);

       enddo;

       endsr;

       //**********************************************************************
       // validateHtml: Sub-Routine to validate Html Tags
       //**********************************************************************
       begsr validateHtml;

       monitor;
          illegalTags = HtmlParser_hasIllegalTags(%trim(htext));
       on-error;
          // Java exception is returned in message RNX0301
          callp qmhrcvpm(MsgInfo: %size(MsgInfo): 'RCVM0100':
                         '*': *zero: '*LAST': *blanks: *zero: '*REMOVE':
                         MsgErrCode);
          errMsg = MsgInfo;
       endmon;

       endsr;

       // *********************************************************************
       // validateLinkTags: Validate link tags <script> and <style>
       // *********************************************************************
       begsr validateLinkTags;

       monitor;
          tagStart = HtmlParser_getIllegalTagIndex(%trim(htext));
          tagStart += 1;
       on-error;
          // Java exception is returned in message RNX0301
          callp qmhrcvpm(MsgInfo: %size(MsgInfo): 'RCVM0100':
                         '*': *zero: '*LAST': *blanks: *zero: '*REMOVE':
                         MsgErrCode);
          errMsg = MsgInfo;
       endmon;

       if tagStart > 0;
          invldChar = TRUE;
       else;
          invldChar = FALSE;
       endif;

       endsr;

       /copy rpgcopy,zcgihskpg
       /copy rpgcopy,messagesr

       /copy rpgcopy,web_sp

       // *********************************************************************
       // FrmtDate - format a date
       // *********************************************************************
       dcl-proc FrmtDate export;
       dcl-pi *n char(32);
          dateIn char(32) const;
          picIn  char(16) const;
          picOut char(16) const;
       end-pi;

     d DateOut         s             32a
     d Lil             s             10i 0

       if DateIn <> *blanks and DateIn <> '0';
          monitor;
             ceedays(DateIn:picIn:Lil:*omit);
             ceedate(Lil:picOut:DateOut:*omit);
          on-error;
             DateOut = 'ERROR';
          endmon;
       else;
          DateOut = '0';
       endif;

       return DateOut;

       end-proc;

       // *********************************************************************
       // Sanitize - convert HTML special chars to encoded chars
       // *********************************************************************
       dcl-proc Sanitize export;
       dcl-pi *n char(1000);
          theHTML char(1000) value;
       end-pi;

     d wrkhtml         s           1000a
     d idx             s              3p 0 inz

       if theHtml = *blanks;
          return theHTML;
       endif;

       //theHTML = %scanrpl('/*':'':theHTML);
       //theHTML = %scanrpl('*/':'':theHTML);
       //theHTML = %scanrpl('#':'&#35;':theHTML);
       //theHTML = %scanrpl('&':'&amp;':theHTML);
       //theHTML = %scanrpl('&amp;#35;':'&amp;':theHTML);
       //theHTML = %scanrpl(';':'&#59;':theHTML);
       //theHTML = %scanrpl('&amp&#59;':'&amp;':theHTML);
       //theHTML = %scanrpl('&#35&#59;':'&#35;':theHTML);
       //theHTML = %scanrpl('--':'&#45;&#45;':theHtml);
       //theHTML = %scanrpl('''':'&#39;':theHTML);
       //theHTML = %scanrpl('>':'&gt;':theHTML);
       //theHTML = %scanrpl('<':'&lt;':theHTML);
       //theHTML = %scanrpl(':':'&#58;':theHTML);
       //theHTML = %scanrpl('(':'&#40;':theHTML);
       //theHTML = %scanrpl(')':'&#41;':theHTML);
       //theHTML = %scanrpl('"':'&quot;':theHTML);

       return theHTML;

       end-proc;

       // *********************************************************************
       // decodeHTML - convert HTML special chars to encoded chars
       // *********************************************************************
       dcl-proc decodeHTML export;
       dcl-pi *n char(1000);
          theHTML char(1000) value;
       end-pi;

       if theHtml = *blanks;
          return theHTML;
       endif;

       //theHTML = %scanrpl('&amp;':'&':theHTML);
       //theHTML = %scanrpl('&semi;':';':theHTML);
       //theHTML = %scanrpl('&ndash;&ndash;':'--':theHtml);
       //theHTML = %scanrpl('&apos;':'''':theHTML);
       //theHTML = %scanrpl('&gt;':'>':theHTML);
       //theHTML = %scanrpl('&lt;':'<':theHTML);
       //theHTML = %scanrpl('&quot;':'"':theHTML);
       //theHTML = %scanrpl('&frasl;':'/':theHTML);
       //theHTML = %scanrpl('&num;':'#':theHTML);
       //theHTML = %scanrpl('&colon;':':':theHTML);
       //theHTML = %scanrpl('&lpar;':'(':theHTML);
       //theHTML = %scanrpl('&rpar;':')':theHTML);
       //theHTML = %scanrpl('&#35;':'#':theHTML);
       //theHTML = %scanrpl('&#38;':'&':theHTML);
       //theHTML = %scanrpl('&#59;':';':theHTML);
       //theHTML = %scanrpl('&#45;&#45;':'--':theHtml);
       //theHTML = %scanrpl('&#39;':'''':theHTML);
       //theHTML = %scanrpl('&#34;':'"':theHTML);
       //theHTML = %scanrpl('&#58;':':':theHTML);
       //theHTML = %scanrpl('&#40;':'(':theHTML);
       //theHTML = %scanrpl('&#41;':')':theHTML);

       return theHTML;

       end-proc;

       // *********************************************************************
       // Function: FindHTML - find html tag positions to substring
       // *********************************************************************
       dcl-proc FindHTML;
       dcl-pi *n char(9);
          theHTML char(999) const;
          theStart packed(9) const;
       end-pi;

      * Variables
     dwrkText          s            999a

     drtrnds           ds             9
     d rtrnpos                 1      3a
     d rtrnlen                 4      6a
     d rtrnstr                 7      9a

     dtagName          s              6a
     dendTag           s             25a
     dstrIdx           s              3p 0 inz
     dpos2             s              3p 0 inz

       wrkText = %xlate(xlc:xuc:theHTML);
       strIdx = theStart;
       if theStart = 0;
          strIdx = 1;
       endif;

       pos = %scan('&LT;':wrkText:strIdx);
       if pos = 0;
          rtrnds = *blanks;
          return rtrnds;
       endif;

       tagName = %subst(wrkText:pos+4:6);
       select;
       when tagName = 'SCRIPT';
          // Do nothing
       when %subst(tagName:1:5) = 'STYLE';
          tagName = 'STYLE';
       when %subst(tagName:1:4) = 'FORM';
          tagName = 'FORM';
       when %subst(tagName:1:1) <> ' ';
          clear tagName;
       other;
          pos += 1;
          rtrnpos = '000';
          rtrnlen = '000';
          rtrnstr = %editc(pos:'X');
          return rtrnds;
       endsl;

       if tagName <> *blanks;
          endTag = '&LT;/' + %trim(tagName) + '&GT;';
       else;
          endTag = '&GT;';
       endif;

       pos2 = %scan(%trim(endTag):wrkText:pos);
       if pos2 > 0;
          len = (pos2+%len(%trim(endTag))) - pos;
       else;
          len = 0;
       endif;

       rtrnpos = %editc(pos:'X');
       rtrnlen = %editc(len:'X');
       rtrnstr = %editc(strIdx:'X');

       return rtrnds;

       end-proc;

       // *********************************************************************
       // getVarValues: process html 'VAR', 'PANEL', 'MSG' , 'SELECTED'
       //                variables
       // *********************************************************************
       dcl-proc getVarValues;
       dcl-pi *n  like(#wp_out.result);
          varType like(HTML_VAR) const;
          varName like(HTML_PARM) dim(HTML_PARM_DIM) const options(*nopass);
       end-pi;

      * Variables
     dreturnVal        s                    like(#wp_out.result)
     dvariable         s                    like(HTML_PARM)
     ddatechar         s             10a
     ddate8            s              8s 0
     derridx           s              3s 0
     dbyte6            s              6a
     derrMsgid         s              4a

     derrline          ds            94
     d errtext                 1     90
     d err_indic               1      1
     d err_msg#                2      5
     d err_vf                  6     11
     d err_mo                 11     11
      *d errbr                  91     94    inz('<br>')

     dmsg              ds                  likeds(msgDs_t) inz

       variable = varName(1);
       clear returnVal;

       // get value for panel substitution
       if varType = 'PANEL';
          select;
          endsl;

          web_processFile(%trim(rootfilepath):
                          %trim(panelFile):
                          %paddr('GETVARVALUES'):
                          %paddr('PROCESSIF'):
                          %paddr('PROCESSLOOP'):
                          %paddr('PROCESSSWITCH'));

          return returnVal;
       endif;

       // get value SELECTED input substitution
       if varType = 'SELECTED';

          select;
          when variable = 'CATGRY';
            return hcategry;

          when variable = 'SCRNID';
            return hscrn_id;

          when variable = 'LANG';
            return hlanguage;

          other;
            clear returnVal;
          endsl;

          return returnVal;
       endif;

       // get messages for display
       if varType = 'MSG';

          select;

          when variable = 'CONF';
            if ci > 0;
               for erridx = 1 to ci;
                 errtext = conf(erridx);

                 select;
                 when err_indic = '#';
                    errMsgid = err_msg#;
                    msg = web_zGetMessage(Msg_defalt + errMsgid
                                         :msg_file);

                    returnVal += msg.text;
                 other;
                    returnVal += errline;
                 endsl;

               endfor;
            else;
               clear returnVal;
            endif;

          when %subst(variable:1:5) = 'ERROR';
             if %len(%trim(variable)) > 5;
                htmlErrPos = %subst(variable:6:2);
                if htmlErrPos <> *blanks and %lookup(htmlErrPos:errs_sect) = 0;
                   clear returnVal;
                endif;
             endif;

             if ei > 0;
                for erridx = 1 to ei;
                   errtext = errs(erridx);

                   if errs_sect(erridx) <> htmlErrPos;
                      iter;
                   endif;

                   select;
                   when err_indic = '@' or err_indic = '#';
                      errMsgid = err_msg#;
                      msg = web_zGetMessage(Msg_defalt + errMsgid
                                           :msg_file);
                      returnVal += msg.text;
                   other;
                      returnVal += errline;
                   endsl;

                endfor;
             else;
                clear returnVal;
             endif;

          other;
            clear returnVal;
          endsl;

          return returnval;
       endif;

       // get value for resource substitution
       if (varType = 'RESOURCE');
          returnVal = getResource(variable:isTest);
       endif;

       // get value for variable substitution
       if varType = 'VAR';

          select;

          when variable = 'ACTION';
             returnVal = %trim(h_action);

          when variable = 'APINF';
             returnVal = happl_inf;

          when variable = 'NT';
             returnVal = %char(hntimes);

          when variable = 'MENU';
             returnVal = hmenuitem;

          when variable = 'TIME';
             returnVal = %char(cycle_time);

          when variable = 'USERID';
             returnVal = %trim(huserid);

          when variable = 'FICE';
             returnVal = %trim(hfice);

          when variable = 'SK';
             returnVal = %trim(hsignonkey);

          when variable = 'FORMID';
             returnVal = %trim(hscrn_next);

          when variable = 'PGMNAME';
             returnVal = %trim(pgm_Name);

          when variable = 'PROD_PGM_NAME';
             return PROD_NAME;

          when variable = 'TEST_PGM_NAME';
             return TEST_NAME;

          when variable = 'APPLNAME';
             returnVal = %trim(@prog_name);

          when variable = 'APPLID';
             returnVal = %trim(h_applid);

          when variable = 'CPYREND';
             copyRight = %trim(%subst(%char(cycle_Date):1:4));
             if CopyRight > crtd_yyyy;
                CopyRight = crtd_yyyy + '-' + CopyRight;
             endif;

             returnVal = CopyRight;

          when variable = 'PNLTITLE';
             select;
             when panelFile = 'messageList' + DOT_HTML;
                returnVal = 'RSS - Message Management';
             when panelFile = 'message_ADUP' + DOT_HTML;
                select;
                when heditmode = 'ADD';
                   returnVal = 'RSS - Add New Message';
                when heditmode = 'UPD';
                   returnVal = 'RSS - Edit Message';
                other;
                   returnVal = 'RSS - Message Management';
                endsl;
             other;
                returnVal = 'Registrar Support';
             endsl;

          when variable = 'MSG_SCRNID';
             returnVal = %trim(hscrn_id);

          when variable = 'MSG_LANG';
             returnVal = %trim(hlanguage);

          when variable = 'HTMLTITLE';
             returnVal = %trim(htmlTitle);

          when variable = 'JM_CHGUSR';
             returnVal = %trim(CImsgs(m1).jm_chguser);

          when variable = 'JM_CHGDATE';
             returnVal = %trim(%char(%date(CImsgs(m1).jm_chgdate):*usa));

          when variable = 'JM_DATEON';
             returnVal = %char(%date(CImsgs(m1).jm_dateon):*usa);

          when variable = 'JM_DATEOFF';
             returnVal = %char(%date(CImsgs(m1).jm_dateoff):*usa);

          when variable = 'JM_MSG_SEQ';
             returnVal = %editc(CImsgs(m1).jm_msg_seq:'X');

          when variable = 'JM_TEXT';
             returnVal = %trim(CImsgs(m1).jm_text);

          when variable = 'JM_STATUS';
             select;
             when CImsgs(m1).jm_status <> 'Y' and CImsgs(m1).jm_dateon > 0;
                returnVal = 'Y';
             when CImsgs(m1).jm_status = 'Y' and
                  CImsgs(m1).jm_dateon <= cycle_date and
                  CImsgs(m1).jm_dateoff >= cycle_date;
                 returnVal = 'F';
             other;
                clear returnVal;
             endsl;

          when variable = 'SCHNAME';
             returnVal = yschname;

          when variable = 'CRM_INFO';
             returnVal = %trim(crm_info);

          when variable = 'CRMNAME';
             returnVal = %trim(credUser.name);

          when variable = 'TEL#';
             returnVal = %trim(credUser.phone);

          when variable = 'LOCALPART';
             returnVal = %trim(credUser.emailLocal);

          when variable = 'DOMAINPART';
             idx = %scan('@':credUser.email) + 1;
             if idx > 4;
                returnVal = %trim(%subst(credUser.email:idx));
             else;
                returnVal = 'credentialssolutions.com';
             endif;

          when variable = 'HDRMSGLST';
             lidx = %lookup(hscrn_id:scrn_id);
             if lidx > 0;
                returnVal = %trim(scrn_txt(lidx));
             else;
                returnVal = *blanks;
             endif;

          when variable = 'MSG_DATEON';
             returnVal = FrmtDate(hdateon:'YYYYMMDD':'MM/DD/YYYY');

          when variable = 'MSG_DATEOFF';
             returnVal = FrmtDate(hdateoff:'YYYYMMDD':'MM/DD/YYYY');

          when variable = 'MSG_DESCRP';
             returnVal = %trim(hdescrp);

          when variable = 'EDITMODE';
             returnVal = heditmode;

          when variable = 'STATUS';
             if ei = 0;
                returnVal = '200';
             else;
                clear returnVal;
             endif;

          when variable = 'DECODEDTXT';
             returnVal = %trim(decodeHTML(htext));

          when variable = 'MODE';
             returnVal = %trim(heditMode);

          when variable = 'MSG_SEQ';
             returnVal = %trim(hmsg_seq);

          when variable = 'MSG_SCRNID';
             returnVal = %trim(hscrn_id);

          when variable = 'MSG_SCRSECT';
             returnVal = %trim(hscrn_sect);

          when variable = 'MSG_LANG';
             returnVal = %trim(hlanguage);

          when %len(variable) > 2;
             select;
             when %subst(variable:1:1) = '|' and
                  %subst(variable:%len(variable):1) = '|';
               returnVal = %subst(variable:2:%len(variable)-2);
             when %subst(variable:1:1) = '"' and
                  %subst(variable:%len(variable):1) = '"';
               returnVal = %subst(variable:2:%len(variable)-2);
             endsl;

          // Other just remove the substitution value
          other;
             clear returnVal;

          endsl;

       endif;

       return returnVal;

       end-proc;

       // *********************************************************************
       // processIf: process a html 'IF' variable
       // *********************************************************************
       dcl-proc processIf;
       dcl-pi *n IND;
          criteria like(HTML_PARM) const;
       end-pi;

       select;
       when criteria = 'HTML_CHANGE_HISTORY';
         return FALSE;

       when criteria = 'TEST_INTERNAL';
          return (pgm_name = TEST_NAME and internalUser);

       when criteria = 'INTERNAL';
          return internalUser;

       when criteria = 'PROD_VERSION';
          return (pgm_name = PROD_NAME);

       when criteria = 'TEST_VERSION';
          return (pgm_name = TEST_NAME);

       when criteria = 'TESTMODULEEXISTS';
          if (cau_grp_id = 'IT' or cau_grp_id = 'DOC');
             return web_objectExists(TEST_NAME);
          else;
             return FALSE;
          endif;

       when %subst(criteria:1:5) = 'ERROR';
         htmlErrPos = %subst(criteria:6:2);
         if htmlErrPos <> *blanks and %lookup(htmlErrPos:errs_sect) = 0;
            return FALSE;
         endif;

         if ei > 0;
            return TRUE;
         else;
            return FALSE;
         endif;

       when criteria = 'CONFMSG';
         if ci > 0;
            return TRUE;
         else;
            return FALSE;
         endif;

       when criteria = 'SWTCH2D2';
          if h_applid = 'TP' and accessD2;
             return TRUE;
          else;
             return FALSE;
          endif;

       when criteria = 'HASD2';
          if accessD2;
             return TRUE;
          else;
             return FALSE;
          endif;

       when criteria = 'SWTCH2TP';
          if h_applid = 'D2' and accessTp;
             return TRUE;
          else;
             return FALSE;
          endif;

       when criteria = 'ADDNEWMSG';
          if messgCnt < 10;
             return TRUE;
          else;
             return FALSE;
          endif;

       when criteria = 'APPLID-TP';
          if h_applid = 'TP';
             return TRUE;
          else;
             return FALSE;
          endif;

       when criteria = 'APPLID-D2';
          if h_applid = 'D2';
             return TRUE;
          else;
             return FALSE;
          endif;

       when criteria = 'MSGSTAT-Y';
          if hstatus = 'Y';
             return TRUE;
          else;
             return FALSE;
          endif;

       when criteria = 'MSGSTAT-N';
          if hstatus = 'N';
             return TRUE;
          else;
             return FALSE;
          endif;

       when criteria = 'JM_CHGDT';
          if CImsgs(m1).jm_chgdate > 0;
             return TRUE;
          else;
             return FALSE;
          endif;

       when criteria = 'SHOWERROR';
         if ei > 0;
            return TRUE;
         else;
            return FALSE;
         endif;

       when criteria = 'SHOWCONF';
         if ci > 0;
            return TRUE;
         else;
            return FALSE;
         endif;

       when criteria = 'MSGAUTO-OF';
          if CImsgs(m1).jm_dateon > 0 and CImsgs(m1).jm_dateoff > 0;
             return TRUE;
          else;
             return FALSE;
          endif;

       when criteria = 'MSGAUTO-F';
          if CImsgs(m1).jm_dateon = 0 and CImsgs(m1).jm_dateoff > 0;
             return TRUE;
          else;
             return FALSE;
          endif;

       when criteria = 'ADDMODE';
          if heditmode = 'ADD';
             return TRUE;
          else;
             return FALSE;
          endif;

       when criteria = 'UPDMODE';
          if heditmode = 'UPD';
             return TRUE;
          else;
             return FALSE;
          endif;

       when criteria = 'USERISIT';
          if cau_grp_id = 'IT' or cau_grp_id = 'CRM';
             return TRUE;
          else;
             return FALSE;
          endif;

       other;
         return FALSE;

       endsl;

       end-proc;

       // *********************************************************************
       // processLoop: process a html 'LOOP' variable
       // *********************************************************************
       dcl-proc processLoop;
       dcl-pi *n;
          template like(smallBuffer) const;
          criteria like(HTML_PARM) const;
       end-pi;

      * Variables
     dworkbuffer       s                   like(smallBuffer)

       select;

       when criteria = 'SECTIONS';
          hits#_sql = 0;

          exec sql
          declare cur_msglst cursor for
             select *
                from cimessag1
                where jm_fice = :hfice and jm_applid = :h_applid and
                      jm_scrn_id = :hscrn_id and jm_lang = :hlanguage
                order by jm_scrsect, jm_msg_seq;

          exec sql
          open cur_msglst;

          if sqlcod = 0;
             // Fetch will change depending on number of sections
             // Right now there are only 3 and max of 10 msgs per section
             exec sql
             fetch Cur_MsgLst for 30 rows into :CImsgs;

             hits#_sql = sqler3;
          endif;

          exec sql
          close Cur_MsgLst;

          for sectIdx=1 to sect_size;
             startIdx = %lookup(sect_id(sectIdx):CImsgs(*).jm_scrsect);

             msgAddSeq = 0;

             if startIdx > 0;
                if httpAccept = 'json' and startIdx > 1;
                   web_addToBuffer(',');
                endif;
                web_replaceVariables(template:
                                     %trim(rootFilePath):
                                     %paddr('GETVARVALUES'):
                                     %paddr('PROCESSIF'):
                                     %paddr('PROCESSLOOP'):
                                     %paddr('PROCESSSWITCH'));
             endif;
          endfor;

       when criteria = 'CIMSGS';
          for m1=StartIdx to 30;
             if CImsgs(m1).jm_scrsect <> '01';
                leave;
             endif;

             clear htmlTitle;
             if CImsgs(m1).jm_categry = 'X';
                lidx = %lookup(CImsgs(m1).jm_categry:ctgy_id);
                if lidx > 0;
                   htmlTitle = %trim(ctgy_txt(lidx)) + ' -';
                endif;
             endif;

             htmlTitle += ' ' + %trim(CImsgs(m1).jm_descrp);

             if htmlTitle = *blanks;
                htmlTitle = 'Message Info Missing - Please Edit and Update';
             endif;

             hstatus = CImsgs(m1).jm_status;

         //DVif httpAccept = 'json' and m1 - StartIdx > 1;
             if httpAccept = 'json' and m1 - StartIdx > 0;
               // insert a comma after the 1st object
               web_addToBuffer(',');
             endif;

             web_replaceVariables(template:
                                 %trim(rootFilePath):
                                 %paddr('GETVARVALUES'):
                                 %paddr('PROCESSIF'):
                                 %paddr('PROCESSLOOP'):
                                 %paddr('PROCESSSWITCH'));

              MsgAddSeq = CImsgs(m1).jm_msg_seq + 1;
          endfor;

       endsl;

       return;

       end-proc;

       // *********************************************************************
       // processSwitch: process a html 'SWITCH' variable
       // *********************************************************************
       dcl-proc processSwitch;
       dcl-pi *n like(HTML_PARM);
          criteria like(HTML_PARM) const;
       end-pi;

       select;

       // processing TITLE for the panels
       when criteria = 'TITLE';

       other;
         return '';

       endsl;

       return '';

       end-proc;

       // *********************************************************************
       // getHttpAccept - gen environment variable Accept
       // *********************************************************************
       dcl-proc getHttpAccept;
       dcl-pi *n like(httpAccept);
       end-pi;

       // Get the Environment variable, REMOTE_ADDR.
       clear EnvRec;
       EnvName    = 'HTTP_ACCEPT';
       EnvNameLen = 11;
       callp APIGetEnv(EnvRec:EnvReclen:EnvLen:EnvName:EnvNameLen:QUSEC);
       return  %subst(EnvRec:1:EnvLen);

       end-proc;

       // *********************************************************************
       // parseJson - Parse JSON body to HTMLForm
       //  parms
       //    htmlForm - the Form array of Key/value pairs. repurposed
       //    Buffer   - the input Buffer containing valid json
       //    BufferLen - Buffer length
       //  returns number of elements extracted or -1 for error
       // *********************************************************************
       dcl-proc parseJSON;
       dcl-pi *n like(maxParms);
          json  likeds(htmlForm);
          inBuffer like(Bufin);
          inBufferLen  like(bufInLn) const;
          maxParms int(10) const;
       end-pi;

       // yajl variables
      /include yajl_h

       dcl-s yajlErrMsg varchar(500) inz('');
       dcl-s docNode like(yajl_val);
       dcl-s iYajl   int(10);
       dcl-s key     varchar(50);
       dcl-s val     like(yajl_val);

       json.numInputs = 0;

       docNode = yajl_buf_load_tree(
          %addr(inBuffer) : inbufferLen : yajlErrMsg );

       if yajlErrMsg <> '';

          json.input(1).data = yajlErrMsg;  // handle error
          return -1;
       endif;

       iYajl = 0;
       dow YAJL_OBJECT_LOOP( docNode: iYajl: key: val);
         if iYajl > maxParms;
            leave;
         endif;
         // Put Key / Val in Html arrays
         json.input(iYajl).name = key;
         json.input(iYajl).data = yajl_get_string(val);
       enddo;

       json.numInputs = iYajl;

       return iYajl;

       end-proc;

       // *********************************************************************
       // setPanelToJSON - set/switch file extension to JSON
       // *********************************************************************
       dcl-proc setPanelToJSON;
       dcl-pi *n like(panelFile);
          currentPanel like(panelFile) const;
          newExtension like(httpAccept) const;
       end-pi;
       dcl-s newPanel  like(panelFile);
       dcl-s p int(5);

       // The panel File default file extension is .html
       // and will only be used to change to .json
       // This code will scan for a 'dot' in the filename and
       // replace or add the newExtension

       p = %SCANR('/' : currentPanel);
       if p = 0;
          p = 1; // filename starts at the beginning
       endif;
       p = %SCANR('.' : currentPanel : p);
       if p = 0;
          // if no dot found just append the newExtension
          newPanel = currentPanel + '.' + newExtension;
       else;
          newPanel = %SUBST(currentPanel : 1 : p) + newExtension;
       endif;
       return  newPanel;

       end-proc;

       // *********************************************************************
       // Function: parseForm
       // *********************************************************************
      /copy rpgcopy,parsinp_sp

      /eject

       // *********************************************************************
       // Compile-time array follows:
       // *********************************************************************
      /copy rpgcopy,parsetbl
** CTDATA =========== scrn_id/scrn_txt==============================
HLP Transcript Ordering Overview
ORD eduCheck Ordering Overview
** CTDATA =========== sect_id/sect_txt==============================
01 Top Section
** CTDATA =========== lang_id/lang_txt==============================
E English
** CTDATA =========== stat_id/stat_txt==============================
Y ON
N OFF
** CTDATA =========== ctgy_id/ctgy_txt==============================
  Choose Message Category
X School Closure
N Non School Closure
