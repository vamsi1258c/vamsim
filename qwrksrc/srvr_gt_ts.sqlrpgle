      *
      *  PROGRAM-SRVR_GT_TS is a program to call SERVER_GT and
      *                     save the req/res in a temporary file
      *
      *  Author      : SenecaGlobal
      *  Date        : 08/10/20
      *****************************************************************
      *  Change History:
      * -------- --- --------------------------------------------------
      ********************************************************************
     H option(*NOXREF:*NODEBUGIO:*SRCSTMT)
     H thread(*SERIALIZE)
     H DFTACTGRP(*NO) ACTGRP(*NEW)
     H BNDDIR('CIBINDDIR':'QC2LE')
      *---------------Define Files

       //  SERVER_GT Data Structures and Variables
      /copy rpgcopy,$srvrgt_ds

      /copy rpgcopy,CLLCPSDS

       dcl-ds srvr_rc extname('SRVR_GT_PF') end-ds;
       dcl-s  prv_req_num like(req_num);
       dcl-s  idx         packed(4:0)  ;

       //----------------------------------------------------------------------
       // Main Processing Logic
       //----------------------------------------------------------------------
       exec sql set option commit=*none, closqlcsr=*endmod;

       // declare cursor to fetch from SRVR_GT_PF
       exec sql
         declare sgtCsr1 scroll cursor For
          select * from SRVR_GT_PF where PRCS_FLG = 'Y' order by req_num
          for update;
       // open cursor
       exec sql
        open sgtCsr1;

       dow sqlCode = 0;

         // fetch next file
         exec sql
          fetch next from sgtCsr1 into :SRVR_RC;

         // leave on EOF
         if sqlCode = 100;
           leave;
         endif;

         if prv_req_num = req_num;
           iter;
         else;
           prv_req_num = req_num;
         endif;

         // process a request
         processRequest();

         sqlCode = 0;

       enddo;

       // Close cursor
       exec sql
         close sgtCsr1;

       // End of the program
       *InLr = *On;

       // ---------------------------------------
       // Send and Receive data from Server_GT
       // ---------------------------------------
       dcl-proc processRequest;

         #GT_func    = in_functn;
         #GT_fice    = in_fice;
         #GT_ord#    = in_ord#;
         #GT_applid  = in_applId;
         #GT_rectyp  = in_recTyp;
         #GT_ob8     = in_ob8;
         #GT_ob24    = in_ob24;
         #GT_textv   = in_textV;
         #GT_entby   = in_entBy;

         exsr $get_GT;

         if #GT_status <> 'OK';
           // --error record not written
           out_status = #GT_status;
           out_result = 'Errors occurred while processing this request';
           exec sql
             Update  SRVR_GT_PF set ROW =:SRVR_RC
             Where current of sgtCsr1;
         Else;
           PRCS_flg   = 'P';
           out_jobnme = #GT_jobn_o;
           out_jobusr = #GT_jobu_o;
           out_jobnbr = #GT_job#_o;
           out_status = #GT_status;
           out_ord#_o = #GT_ord#_o;
           out_ob_flg = #GT_ob_flg;

           for idx = 1 to 108 by 1;
             out_result = #GT_ary(idx);
             if idx = 1;
               exec sql
                 Update  SRVR_GT_PF set ROW =:SRVR_RC
                 Where current of sgtCsr1;
             elseif #GT_ary(idx) <> *Blanks;
               exec sql
                 Insert into SRVR_GT_PF Values(:SRVR_RC);
             else;
               leave;
             endif;
           endfor;

         endif;

       //  SERVER_GT Routines
      /copy rpgcopy,$srvrgt_sr

       end-proc processRequest;

