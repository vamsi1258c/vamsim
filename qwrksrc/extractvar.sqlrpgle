**FREE
// ***************************************************************************************
// Program - EXTRACTVAR
// Description - Extract variables used in web files and write them into a HTMLVARF file.
//
// Parameters - This program receives following parameters -
//              1. Application Name (For e.g. DVCGICCS/DVCGIRSS)
//              2. envPath (The value can be 'p' or 't' as in Prod or Test File Path)
// ***************************************************************************************

ctl-opt option(*NOXREF:*NODEBUGIO:*SRCSTMT) dftactgrp(*NO);


// Prototype for entry Parameters...
     D
dcl-pi *n;
   application char(10);
   envPath char(1);
end-pi;

/define stat
/include qsysinc/qrpglesrc,sysstat
/copy rpgcopy,ifshead

// Prototype for list file program (getIFSFls)
dcl-pr getIFSFls  EXTPGM;
   directory char(50);
   filesType char(10);
   errorMessage char(30);
end-pr;

// Data Structure for stat() api...
dcl-ds sts likeds(stat_t);

// Constants....
dcl-c TRUE '1';
dcl-c FALSE '0';

// Local Variables...
dcl-s applPath varchar(50);
dcl-s fileName varchar(50);


//****************************************************************************************
// Start Main Processing...
//****************************************************************************************

exec sql set option commit=*none, closqlcsr=*endmod;

// Clear HTMLVARF file...
exec sql delete from htmlvarf;

if application <> *blanks and (envPath = 'p' or envPath = 't');
   applPath = '/wf/' + envPath + '/' + %trim(application) + '/';

   if retrieveFileList(applPath);
      processFileList(applPath);
   endif;
endif;

*inlr = *on;

//****************************************************************************************
// retrieveFileList: Retrieve the list of all web files available under the application
//****************************************************************************************
dcl-proc retrieveFileList;
   dcl-pi *n ind;
      applPath varchar(50);
   end-pi;

   // Parameters
   dcl-s pDirectory char(50) inz('');
   dcl-s pFilesType char(10) inz('html');
   dcl-s pErrorMessage char(30) inz(' ');

   pDirectory = applPath;

   // Retrieve html files list into QTEMP/FILESLIST by calling getIFSFls program
   getIFSFls(pDirectory:pFilesType:pErrorMessage);

   // blank error message indicates 'success'
   if pErrorMessage = ' ';
     return TRUE;
   else;
     return FALSE;
   endif;

end-proc;

//****************************************************************************************
// processFileList: Process web files one at a time and extract variables
//****************************************************************************************
dcl-proc processFileList;
   dcl-pi *n;
      applPath varchar(50);
   end-pi;

   // declare cursor to fetch from qtemp/fileslist
   exec sql
     declare extCsr1 scroll cursor For
       select * from QTEMP/FILESLIST;

   // open cursor
   exec sql
     open extCsr1;

   dow sqlCode = 0;
     // fetch next file
     exec sql
       fetch next from extCsr1 into:fileName;

     // leave on EOF
     if sqlCode = 100;
       leave;
     endif;

     // extract varaibles for file
     extractVariables(applPath:fileName);

   enddo;

   // Close cursor
   exec sql
     close extCsr1;

end-proc;

//****************************************************************************************
// extractVariables: Extract all variables from the web file and write into HTMLVARF file
//****************************************************************************************
dcl-proc extractVariables;
   dcl-pi *n;
      filePath varchar(50);
      fileName varchar(50);
   end-pi;

   // Constants...
   dcl-c STARTTAG '([';
   dcl-c ENDTAG '])';
   dcl-c COMMA ',';

   // Local Variables...
   dcl-s filedesc int(10:0);
   dcl-s pStrLen int(10);
   dcl-s errorText varchar(100);
   dcl-s htmlVariable varchar(100);
   dcl-s varType varchar(50);
   dcl-s varName varchar(50);
   dcl-s pString char(32000);
   dcl-s webString varchar(32000);
   dcl-s startPos zoned(5:0) inz(1);
   dcl-s endPos zoned(5:0);
   dcl-s hasVariables ind inz(TRUE);
   dcl-s ifsPath char(100) inz;
   dcl-s exists ind inz(FALSE);

   // Start processing...
   ifsPath = %trim(filePath) + %trim(fileName);

   // Open ifs web file...
   fileDesc = open(%trimr(ifsPath):
                   O_RDWR + O_TEXTDATA :
                   S_IRUSR + S_IWUSR + S_IRGRP :
                   37);

   if filedesc < 0;
      errorText = 'Failed to open file' + %trim(fileName);

      // Insert error text in HTMLVARF file...
      exec sql insert into htmlvarf values(:application,:fileName,' ',' ',' ',:errorText);

      return;
   endif;

   // Read file from ifs
   pStrLen = read(filedesc:%addr(pString):%size(pString));
   pString = %xlate(x'00':' ':pString);                         // End of Record null
   pString = %xlate(x'25':' ':pString);                         // Line Feed (LF)
   pString = %xlate(x'0D':' ':pString);                         // Carriage Return (CR)

   webString = %trim(pString);

   // Loop through the webString and Extract Variables...
   dow hasVariables and %len(webString) > startPos ;
      endPos = 0;
      startPos = %scan(STARTTAG:webString:startPos);
      if startPos > 0;
         endPos = %scan(ENDTAG:webString:startPos);
         if endPos > 0;
            hasVariables = TRUE;
         else;
            hasVariables = FALSE;
            leave;
         endif;
      else;
         hasVariables = FALSE;
         leave;
      endif;

      htmlVariable = %subst(webString:startPos + %len(STARTTAG):
                            endPos - startPos - %len(ENDTAG));

      clear varType;
      clear varName;

      if %scan(COMMA:htmlVariable) > 0;
         varType = %subst(htmlVariable:1:%scan(COMMA:htmlVariable)-1);
         varName = %trim(%subst(htmlVariable:%scan(COMMA:htmlVariable)+1));
      else;
         varType = %trim(htmlVariable);
      endif;

      // Verify if we already extratcted this variable to prevent duplicate entries...
      exists = FALSE;

      exec sql select '1' into :exists from htmlvarf
               where htappl = :application and
                     htfileName = :fileName and
                     htvarType = :varType and
                     htvarName = :varName;

      if not exists;
         // Insert record into HTMLVARF file...
         exec sql insert into htmlvarf
                  values(:application,:fileName,:varType,:VarName,'N',' ');
      endif;

      startPos = endPos + %len(ENDTAG);

   enddo;

   // Close file...
   callp close(filedesc);
   return;

end-proc;
