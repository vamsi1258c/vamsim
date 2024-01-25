**FREE
// ***************************************************************************************
// Program - WRTPNL2IFS
// Description - Write Messages Panels to IFS Folder
//
// ***************************************************************************************

ctl-opt option(*NOXREF:*NODEBUGIO:*SRCSTMT) dftactgrp(*NO);


// Prototype for entry Parameters...

// Prototype for QCMDEXC
dcl-pr qcmdexc extpgm('QCMDEXC');
   theCommand      char(5000);
   cmdLength       packed(15:5);
end-pr;


// Constants....
dcl-c LF x'25';
dcl-c ifsPath '/wf/p/UTCGIPNL/';
dcl-c clrFile 'CLRPFM FILE(UTPNLMSG)';

// Local Variables...
dcl-s ifsFile  char(50);
dcl-s wMbr     char(10);
dcl-s wPrvMbr  char(10);
dcl-s wMessage varchar(3000);
dcl-s wCommand      char(5000);
dcl-s wCommandLen   packed(15:5);

//****************************************************************************************
// Start Main Processing...
//****************************************************************************************

exec sql set option commit=*none, closqlcsr=*endmod;

wCommand = clrFile;
wCommandLen = %Len(wCommand);
qcmdexc(wCommand : wCommandLen);

   // declare cursor to fetch from qtemp/fileslist
   exec sql
     declare extCsr1 scroll cursor For
       select t1.member, coalesce(t2.message_second_level_text, ' ')
       from deven.utmsg1 t1 left outer join deven.utmsg2 t2
       on t1.messages = t2.message_id
       order by T1.member, message_id;

   // open cursor
   exec sql
     open extCsr1;

   dow sqlCode = 0;
     // fetch next file
       exec sql
         fetch next from extCsr1 into :wMbr, :wMessage ;

     // leave on EOF
     if sqlCode = 100;
       @writePanel();
       leave;
     endif;

     // New panel?
     if wPrvMbr = *Blanks;
       wPrvMbr = wMbr;
     elseif wMbr <> wPrvMbr ;
       @writePanel();
       wPrvMbr = wMbr;
     endif;


     // write message line to temporary file
     wMessage = %xlate('¦':LF:wMessage);
     wMessage = %xlate('`':'''':wMessage);

     exec sql insert into UTPNLMSG  Values (:wMessage);

   enddo;

   // Close cursor
   exec sql
     close extCsr1;

   *inlr = *on;

//****************************************************************************************
// @writePanel - Write current panel to IFS and clear the temporary file
//****************************************************************************************
dcl-proc @writePanel;
   dcl-pi *n;
   end-pi;
   dcl-c dot_html '.html';

   if wPrvMbr = *blanks;
     return;
   endif;

   ifsFile = ifsPath+ %Trim(wPrvMbr)+ dot_html;
   wCommand = 'CPYTOIMPF FROMFILE(UTPNLMSG) '+
               ' TOSTMF('''+ %Trim(IfsFile)   + ''')'+
               ' STMFCCSID(437)          '+
               ' RCDDLM(*CRLF)' +
               ' STRDLM(*NONE) MBROPT(*REPLACE)' +
               ' ORDERBY(*ARRIVAL) ' ;

   wCommandLen = %Len(wCommand);
   qcmdexc(wCommand : wCommandLen);

   wCommand = clrFile;
   wCommandLen = %Len(wCommand);
   qcmdexc(wCommand : wCommandLen);


end-proc;
