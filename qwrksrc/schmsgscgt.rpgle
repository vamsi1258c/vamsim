**FREE

ctl-opt option(*NOXREF:*NODEBUGIO:*SRCSTMT) thread(*SERIALIZE) dftactgrp(*NO) actgrp(*NEW)
        bnddir('CIBINDDIR':'QC2LE');


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dcl-f dvclientf1  keyed usropn  usage(*input);
// These constants profile the fields in parsinp_ds
dcl-c HTML_NAME_LEN 30;
dcl-c HTML_DATA_LEN 1000;
dcl-c MAX_INPUT_DIM 100;
dcl-c BUFIN_LEN 32000;
dcl-s html   char(80)        DIM(1000) CTDATA;
dcl-s idx    Zoned(4)                      ;

//CopyBooks
/copy rpgcopy,zhskpg_ds
/copy rpgcopy,parsedsbas
/copy rpgcopy,parseds999
/copy rpgcopy,parsinp_ds

/define stat
/include qsysinc/qrpglesrc,sysstat
/copy rpgcopy,ifshead


//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
//Prototypes

/copy prototypes,APIGetEnv
/copy prototypes,APIStdIn
/copy prototypes,APIStdOut
/include prototypes,web

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
// CGI specific variables

// Variables for the CGI interface API for QtmhWrStout.
dcl-s BufOut char(10240) inz;

// Define line feed that is required when writing data to std output.
dcl-c LINEFEED x'15';

// Data Structure for stat() api...
dcl-ds sts likeds(stat_t);

// Constants....
dcl-c TRUE '1';
dcl-c FALSE '0';


//****************************************************************************************
// Start Main Processing...
//****************************************************************************************

exsr zcgihskpg;
%subst(parea:2) = bufin;

bufin = %xlate(x'00':' ':bufin);                         // End of Record null
bufin = %xlate(x'25':' ':bufin);                         // Line Feed (LF)
bufin = %xlate(x'0D':' ':bufin);                         // Carriage Return (CR)

cuserid = %subst(bufin:1:10);
if cuserid = *blanks;
  cuserid  = 'F001182VM';
endif;

main();

*inlr = *on;

/copy rpgcopy,zcgihskpg

// ********************************************
// Main Processing Procedure
// ********************************************
dcl-proc main;
   dcl-pi *n;
   end-pi;

   // Local Variables...
   dcl-s filedesc int(10:0);
   dcl-s filePath char(50);
   dcl-s errorText varchar(100);
   dcl-s pStrLen int(10);
   dcl-s pString char(32000);

    if not %open(dvclientf1);
       open dvclientf1;
    endif;

    chain (cuserid) client;

    for idx=1 to 50;
      html(idx)=%scanRpl('CUSERID':  %trim(cuserid)       :html(idx));
      html(idx)=%scanRpl('CSIGNONKEY': %trim(csignonkey) :html(idx));
      html(idx)=%scanRpl('CFREEFICE': %trim(cfreefice)  :html(idx));

      if %trim(html(idx)) = '';
        pString = %trim(pString) + linefeed;
      elseif %subst(html(idx):%checkr(' ':html(idx)):1) <> '+';
        pString = %trim(pString) + %trim(html(idx)) + linefeed;
      else;
        pString = %trim(pString) + %trim(html(idx)) ;
      endif;
    endfor;

    APIStdOut(pString:%len(pString):QUSEC);

   callp close(filedesc);
   return;

end-proc main;

** CTDATA HTML===================================================
Content-type: text/html
Cache-Control: no-store
Pragma: no-cache


<html>
<head>
<title>SCHMSGSCGI-Manage Message Test</title>
<script type="text/javascript">
</script>
</head>
<body>
<h1> Parchment Menu </h1>
<form id="form1" method="post" action="SCHMSGSCGI.pgm" target="_blank">
<button type="submit" value="Submit">Message Management</button>
<input name="FICE" type="hidden" value="CFREEFICE"><br>
<input name="SK" type="hidden"  value="CSIGNONKEY"><br>
<input name="USERID" type="hidden"  value="CUSERID"><br>
<input name="APPLID" type="hidden"  value="TP"><br>
<input name="FORMID" type="hidden"  value="SML"><br>
<input name="ACTION" type="hidden"  value="SML"><br>
<input name="MSG_LANG" type="hidden"  value="E"><br>
<input name="MSG_SCRNID" type="hidden"  value="HLP"><br>
<input name="MSG_SCRSECT" type="hidden"  value="01"><br>
</form>
</body>
</html>
