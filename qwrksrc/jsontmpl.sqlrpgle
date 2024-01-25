**free
//
// JSONTMPL
// Convert the IFS webPage template html output JSON
//
// The input is the name of the path to the webPage template or a html template
// the output will be the same path with the extension replaced with '.json"
//
// The webPage extract the Variables and depending on tye type
// ([FILE,filename.html])  - Either change the extension to .json
//                           or retrieve the contents and process (my preference).
//                           Omit if extension is not .html
// ([MSG,varName])
// ([RESOURCE,varName]) = Omitted, used for resources like js and CCS
// ([SELECTED,varName]) - ?  Uses getVariable
// ([CHECKED,varName])  - ?
// ([VAR,varName]) - Write as key/value pair
//                       "varName" : "([VAR,varName])"
//
// ([WHEN,|NOT,|whenName]) - Write the when as a IF with true/false then the when block
//                       ([IF,whenName,"true","false"])([WHEN,|NOT,|whenName])
//
// ([END,whenName]) - Write as is
//                       ([END,whenName])
//
// ([IF,ifName,trueValue,falseValue]) - Need to pass the tre/false value.  Is it
//                                      possible for there to be a replaceable value
//                                      in the Value that also needs to be evaluated?
//                       "ifName" : (IF,ifName,true,false)]
// ([SWITCH,switchName]) - Write as is
//                       ([SWITCH,switchName])
// ([CASE,caseName]) - Similare to WHEN, write the condition and then caseName as true
//                       ([CASE,caseName]) "caseName" : true
//
// ([PANEL, ])  - Include as is?  Omit if parm1 is STYLE or SCRIPT?
// ([PGM ])
//
// ([LOOP,loopname]) - The loopName will be the name of the the key and an array [ ]
//                     Each row will be an object { } and each row separated by a comma.
//                     The loop will need some way to show it is JSON to the 'processLoop'
//                     procedure so a comma can be inserted between each row.
//                       "loopName": [
//                       ([LOOP,loopname]){
//                            contents of loop template
//                       }(END,loopName]) ]
//

// One idea for the processing is to create subprocedures for the GetVariable, processSwitch,
// ProcessLoop, etc. that are for JSON processing.  The jsonGetVariable could call the
// existing getVariable, then could escape the value (substitute \" for " and other special
// characters. processLoops will need to handle commas between the rows.
// The json will need a beginning delimiter of '{' and an ending delimiter of '}' with commas
// between. Another subprocedure perhaps named json_delim could be used to determine the
// starting curly brace and commas.

ctl-opt ALWNULL(*USRCTL) DFTACTGRP(*NO) ACTGRP('QILE')
   BNDDIR('CIBINDDIR':'QC2LE');

// Prototype for entry Parameters...
dcl-pi *n;
   inAppPath char(80);      // Directory or html filename
   inReplace   char(1);      // Replace existing json files?
end-pi;

// Prototype for list file program (getIFSFls)
dcl-pr getIFSFls  EXTPGM;
   directory char(50);
   filesType char(10);
   errorMessage char(30);
end-pr;

// IFS prototypes from ILE C runtime Library functions

dcl-pr access int(10) extproc('access') ;
   *n pointer value options(*string);  // file name
   *n int(10) value ;  // mode
end-pr ;

/copy rpgcopy,ifshead

// jsonVarF - Json Variables file
dcl-ds jvar_t extname('JSONVARF') qualified template end-ds;

// Constants....
dcl-c TRUE '1';
dcl-c FALSE '0';

// Local Variables..
dcl-s appPath    varchar(80);
dcl-s replace    char(1) inz('N');
dcl-s fileName   like(jvar_t.JFILENAME);
dcl-s appName    like(jvar_t.japp);
dcl-s htmlString varchar(32000);
//SGTST dcl-s jsonBuffer varchar(32000);
dcl-s jsonBuffer varchar(200000);

dcl-s appStrIdx int(5);
dcl-s appEndIdx int(5);
//****************************************************************************************
// Start Main Processing...

exec sql
   set option commit=*none, closqlcsr=*endmod;

appPath = %trimr(inAppPath);
if %parms >= 2;
   replace = inReplace;
endif;

if appPath <> *blanks and
   %subst(appPath : %len(appPath)-4 : 5) = '.html';
   // single html file provided, convert this file to json

   htmlString = getIFSFile(appPath);

   // extract variables for file
   filename = %subst(appPath:%scanr('/':appPath)+1);

   appEndIdx = %scanr('/':appPath)-1;
   if appEndIdx > 0;
      appStrIdx = %scanr('/':%subst(appPath:1:appEndIdx))+1;
      if appStrIdx > 0;
         appName = %subst(appPath:appStridx:appEndIdx-appstridx+1);
      endif;
   endif;
   if appName = *blank;
      appName = 'Unknown';
   endif;
   extractJsonDB(htmlString:appName:fileName);
   jsonBuffer = formatJSONbuffer(appName:fileName);
   // write json file to IFS
   writeJsonFile(AppPath:jsonBuffer:replace);
else;
   if %subst(appPath : %len(appPath) : 1) <> '/';
      // add a directory slash  the path
      appPath += '/';
   endif;
   appStrIdx = %scanr('/':%subst(appPath:1:%len(appPath)-1))+1;
   if appStrIdx > 1;
      appName = %subst(appPath:appStridx:%len(appPath)-appstridx   );
   endif;
   if appName = *blank;
      appName = 'Unknown';
   endif;
   // get a list of files
   if retrieveFileList(appPath);
      processFileList(appPath:appName);
   endif;
endif;

*inlr = *on;

//****************************************************************************************
// retrieveFileList: Retrieve the list of all web files available under the application
//****************************************************************************************
dcl-proc retrieveFileList;
   dcl-pi *n ind;
      applPath like(appPath);
   end-pi;

   // Parameters
   dcl-s pDirectory char(50) inz('');
   dcl-s pFilesType char(10) inz('html');
   dcl-s pErrorMessage char(30) inz(' ');

   // Clear HTMLVARF file...
   exec sql
      delete from htmlvarf;

   pDirectory = applPath;

   // Retrieve html files list into qtemp/fileslist by calling getIFSFls program
   getIFSFls(pDirectory:pFilesType:pErrorMessage);

   // blank error message indicates 'success'
   if pErrorMessage <> *blank;
      return FALSE;
   endif;

   return TRUE;

end-proc;

//****************************************************************************************
// processFileList: Process web files one at a time and extract variables
//****************************************************************************************
dcl-proc processFileList;
   dcl-pi *n;
      applPath like(appPath);
      appName like(jvar_t.japp);
   end-pi;

   dcl-s htmlString  varchar(32000);
   //SGTST dcl-s jsonBuffer  varchar(32000);
   dcl-s jsonBuffer  varchar(200000);
   dcl-s filepath    like(applPath);
   dcl-s processSqlCode like(sqlcode);

   // declare cursor to fetch from qtemp/fileslist
   exec sql
      declare ProcessCsr scroll cursor for
      select * from qtemp/fileslist;

   // open cursor
   exec sql
      open ProcessCsr;

   dow processSqlCode = 0;
      // fetch next file
      exec sql
         fetch next from processCsr into:fileName;

      processSqlCode = sqlcode;
      // leave on EOF
      if ProcesssqlCode = 100;
         leave;
      endif;

      // process only html files
      if %subst(fileName:%len(%trim(filename))-4 : 5) <> '.html';
         iter;
      endif;

      filepath = applPath + filename;
      htmlString = getIFSFile(filepath);
      extractJsonDB(htmlString:appName:fileName);

      // extract varaibles for file
      jsonBuffer = formatJSONBuffer(appName:fileName);

      // write json file to IFS
      writeJsonFile(filepath:jsonBuffer:replace);
   enddo;

   // Close cursor
   exec sql
      close ProcessCsr;

end-proc;

//-----------------------------------------------
//  web_getFileExtension:
//     Purpose: locate and return the extension for a file upload
//     Parameters:  filename => name of the file
dcl-proc web_getFileExtension;
   dcl-pi *n varchar(6);
      $filename char(512) const;
   end-pi;

   dcl-s dotIdx int(5) inz;

   dcl-s fileExtension varchar(6) inz;

   dcl-s xlc char(26) INZ('abcdefghijklmnopqrstuvwxyz');
   dcl-s XUC char(26) INZ('ABCDEFGHIJKLMNOPQRSTUVWXYZ');

   dotIdx = %scan('.':$filename);
   dow dotIdx > 0;
      fileExtension = %subst($filename:dotIdx+1:6);
      dotIdx = %scan('.':$filename:dotIdx+1);
   enddo;

   return %xlate(xuc:xlc:fileExtension);

end-proc;

//-----------------------------------------------
// extractJsonDB - Extract variables from html file and write to
// JSONVARF file.  Use the file to sort and filter out duplicates and then
// write json Template.

dcl-proc extractJsonDB;
   dcl-pi *n;
      workBuffer varchar(32000);
      App        like(jvar.japp) CONST;
      filename   like(jvar.jfilename) CONST;
   end-pi;

   dcl-ds jvar extname('JSONVARF') qualified inz end-ds;

   // Constants...
   dcl-c STARTTAG '([';
   dcl-c ENDTAG '])';
   dcl-c DQ '"';        // Double Quote

   // variables
   dcl-s endidx       int(5)  inz(1);
   dcl-s fileBuffer   like(workBuffer);
   dcl-s hasvariables ind     inz(true);
   dcl-s htmlvariable varchar(150 : 2);
   dcl-s parmlist     varchar(40) dim(6);
   dcl-s tagBlock     varchar(250);
   dcl-s startidx     int(5)  inz(1);
   dcl-s nextNest     like(jvar.jnestLevel) inz;
   dcl-s nestType     char(1);
   dcl-s delim        varchar(1) inz(''); // delimiter to use next
   dcl-s nestDelim    varchar(1);
   dcl-s nextLevelLen int(5);

   jvar.japp = App;
   jvar.Jfilename = fileName;

   exec sql delete from jsonvarf where japp = :App and jFileName = :fileName;

   dow hasvariables;
      startidx = %scan(starttag:workbuffer:startidx);
      if startidx = 0;
         hasvariables = FALSE;
         leave;
      endif;
      endidx = %scan(endtag:workbuffer:startidx);
      if endidx = 0;
         hasvariables = FALSE;
         leave;
      endif;

      htmlvariable = %subst(workbuffer:
         startidx + %len(starttag):
         endidx - startidx - %len(endtag));
      tagBlock = starttag + htmlvariable + endtag;

      parmlist = getparameters(htmlvariable);
      htmlvariable = parmlist(1);

      // set the buffer index to the end of this variable for next one.
      startidx = endidx + %len(endtag);

      select;

      when htmlvariable = 'IF';
         // ([IF,ifName,trueValue,falseValue])
         //       - Need to pass the true/false value.  Is it
         //         possible for there to be a replaceable value
         //         in the Value that also needs to be evaluated?
         // "ifName" : ([IF,ifName,true,false])
         //
         jvar.jvarseq += 1;
         jvar.jvartype = htmlvariable;
         jvar.JvarName = %trim(parmlist(2));
         jvar.jJSONName = replaceSpecialJSONChars(jvar.jvarName);
         jvar.jvarstring =  delim + DQ + jvar.jJSONName + DQ +
            ': ([IF,' + parmlist(2) + ',"true","false"])';
         jvar.jNestLevel = nextNest;

         exec sql insert into jsonvarf values(:jvar);
         delim = ',';

      when htmlvariable = 'RESOURCE';
         iter;
      when htmlvariable = 'VAR' or
         htmlvariable = 'MSG';
         // or htmlvariable = 'RESOURCE';
         // ([MSG,varName])
         // ([RESOURCE,varName])  -- This can be ignored
         // ([VAR,varName]) - Write as key/value pair
         //   "varName" : "([VAR,varName])"
         jvar.jvarseq += 1;
         jvar.jvartype = htmlvariable;
         // if 'MSG' then the next parm can be a number only.
         // if a number then prefix the key with 'MSG_"
         if htmlvariable = 'MSG' and %check('0123456789 ':parmlist(2)) = 0;
            jvar.jvarName = 'MSG_'+%trim(parmlist(2));
            jvar.jJSONName = replaceSpecialJSONChars(jvar.jVarName);
         else;
            jvar.jvarName = %trim(parmlist(2));
            jvar.jJSONName = replaceSpecialJSONChars(jvar.jVarName);
         endif;
         jvar.jvarstring = delim + DQ + jvar.jJSONName + DQ +
                           ': ' + DQ + tagBlock + DQ;
         jvar.jNestLevel = nextNest;

         exec sql insert into jsonvarf values(:jvar);
         delim = ',';

      when htmlvariable = 'WHEN';
         // ,"whenName": ([IF,whenName,"true","false"])WhenBlock
         // a WHEN condition is an IF with a block of html (maybe variables)
         // until the matching ([END ]) is reached.
         // The WHEN is written as boolean key:value and then the start of the WHEN
         // the name of the Variable with a '1' appended (so sorting keeps in the
         // proper order). The NextNEST will have a '2' appended.
         // The value of delim is appended after the nextNest Type.

         jvar.jvarseq += 1;
         jvar.jvartype = htmlvariable;
         if parmlist(2) <> 'NOT';        // if there is a ,NOT then parmlist 3 is the
            jvar.JvarName = parmlist(2); // variable name.
         else;
            jvar.JvarName = parmlist(3);
         endif;
         jvar.jJSONName = replaceSpecialJSONChars(jvar.jVarName);
         jvar.jvarstring = delim + DQ + jvar.jJSONName + DQ + ': ([IF,' +
            jvar.jVarName + ',"true","false"]) '+tagBlock;
         if nextNest <> *blank;
           nextNest += ' ';    // blank separator between nestLevels
         endif;
         jvar.jNestLevel = nextNest + jvar.jVarName + '1' + delim;

         exec sql insert into jsonvarf values(:jvar);

         nextNest += jvar.jVarName + '2' + delim;
         //delim = '';

      when htmlvariable = 'END';
         // ([END,whenName]) or ([END,loopName]) or ([END,caseName])
         // a when end is for type '2' in nextNest, a Loop is type 5, Case type 8
         // Extract the last character to determine the type,
         // +1 on Type for NestLevel,
         // Remove EndName from nextNest

         jvar.jvarseq += 1;
         jvar.jvartype = htmlvariable;
         jvar.JvarName = parmlist(2);
         // extract the 'type' as the last byte of nextNest
         nestType = %subst(nextNest:%len(nextNest):1);
         if nestType = ',';
            nestDelim = ',';
            nextLevelLen = %len(nextNest)-2;  // length without level
            nestType = %subst(nextNest:%len(nextNest)-1:1);
         elseif nestType='{';  // This is the end of the loop
            nestDelim = '}';
            nextLevelLen = %len(nextNest)-2;  // length without level
         else;
            nestDelim = '';
            nextLevelLen = %len(nextNest)-1;  // length without level
         endif;
         if nestType = '2';  // When - write as the tagBlock
            jvar.jvarstring =  tagBlock;
            jvar.jNestLevel = %subst(nextNest:1:nextLevelLen) + '3' + nestDelim;
         elseif nestType = '5';
            jvar.jVarString = '}' + tagBlock + ']';  // end Loop array
            jvar.jNestLevel = %subst(nextNest:1:nextLevelLen) + '6';
         elseif nestType = 'X';
            // Ignore the END for SELECTED AND CHECKED
         else;
            jvar.jVarString = tagBlock;  // end Switch object
            jvar.jNestLevel = %subst(nextNest:1:nextLevelLen) + '9';
         endif;

         If nestType <> 'X'; // ignore END for SELECTED and CHECKED
            exec sql insert into jsonvarf values(:jvar);
         endif;
         // if the NextNest is just this variable, clear it, else remove
         // the variable from the end of the NextNest
         if nextLevelLen = %len(%trim(parmlist(2)));
            clear nextNest;
         else;
            if %scanr(' ':nextNest) > 1;  //SGTST
            nextNest = %subst(nextNest:1:%scanr(' ':nextNest)-1);
            endif;                        //SGTST
         endif;

      when htmlvariable = 'FILE';
         // ([FILE,filename.html])  Include file and change to json.

         if web_getFileExtension(parmlist(2)) <> 'html';
            // include only if html file included
            iter;
         endif;
         fileBuffer = getIfsFile(appPath + parmlist(2));
         // The work buffer has been processed up to the
         // endidx + %len(endtag).  We can append the work buffer
         // from the last processed to the file buffer for the new
         // work buffer.
         workBuffer = filebuffer + %subst(workbuffer:endidx + %len(endtag));
         startidx = 1;
         iter;
         // code below no longer used - File is inserted in work buffer.
         // Assume that the include file will have at least one variable, so
         // insert the current delimiter in FRONT of the FILE var and
         // set the delimiter for the next to be a comma.
         jvar.jvarseq += 1;
         jvar.jvartype = htmlvariable;
         jvar.JvarName = %scanrpl('html' : 'json' : parmlist(2));
         jvar.jvarstring = delim + %scanrpl('html' : 'json' : tagBlock);
         jvar.jNestLevel = nextNest;

         exec sql insert into jsonvarf values(:jvar);
         delim = ',';

      when htmlvariable = 'PANEL';
         // ([PANEL, ])  - Include as is?  Omit if parm1 is STYLE or SCRIPT?
         if parmlist(2) = 'SCRIPT' or parmlist(2) = 'STYLE';
            iter;
         endif;
         // if a file is specified, include only if html and switch it to json
         if parmlist(2) <> *blank;
            if web_getFileExtension(parmlist(2)) <> 'html';
               // include only if html file included
               iter;
            endif;
            tagBlock = %scanrpl('html' : 'json' : tagBlock);
         endif;
         // add PANEL. similar to FILE, insert delim at front
         jvar.jvarseq += 1;
         jvar.jvartype = htmlvariable;
         jvar.JvarName = parmlist(2);
         jvar.jvarstring = delim + tagBlock;
         jvar.jNestLevel = nextNest;

         exec sql insert into jsonvarf values(:jvar);
         delim = ',';

      when htmlvariable = 'LOOP';
         // ([LOOP,loopname])
         // - The loopName will be the name of the the key and an array [ ]
         //   Each row will be an object { } and each row separated by a comma.
         //   The loop will need some way to show it is JSON to the 'jsonLoop'
         //   procedure so a comma can be inserted between each row.
         //                      , "loopName": [{
         //                       ([LOOP,loopname])
         //                       }]
         //! Should the 'jsonLoop' insert the "loopName and the beginning and ending array?
         jvar.jvarseq += 1;
         jvar.jvartype = htmlvariable;
         jvar.JvarName = parmlist(2); // variable name.
         jvar.jJSONName = replaceSpecialJSONChars(jvar.jVarName);
         jvar.jvarstring = delim + DQ+ jvar.jJSONName + DQ + ':[' + tagblock +'{' ;
         if nextNest <> *blank;
            nextNest += ' ';    // blank separator between nestLevels
         endif;
         jvar.jNestLevel = nextNest + jvar.jVarName + '4';

         exec sql insert into jsonvarf values(:jvar);

         nextNest += jvar.jVarName + '5';
         delim = '';
      when htmlvariable = 'SWITCH';
         //! Need to have an example of the SWITCH/CASE statement.
         //! Assuming that the SWITCH is a VAR and the Cas is the block of code to be
         //! executed. Write as a "switchName" : ([VAR,switchName])
         // ([SWITCH,switchName])
         jvar.jvarseq += 1;
         jvar.jvartype = htmlvariable;
         jvar.JvarName = parmlist(2); // variable name.
         jvar.jJSONName = replaceSpecialJSONChars(jvar.jVarName);
         jvar.jvarstring = DQ+ jvar.jJSONName + DQ + ':{ ' + tagblock ;
         if nextNest <> *blank;
            nextNest += ' ';    // blank separator between nestLevels
         endif;
         jvar.jNestLevel = nextNest + jvar.jVarName + '7';

         exec sql insert into jsonvarf values(:jvar);

         nextNest += jvar.jVarName + '8';

      when htmlvariable = 'CASE';
         // ([CASE,caseName])
         // - similar to WHEN except the key/value is written on the SWITCH
         jvar.jvarseq += 1;
         jvar.jvartype = htmlvariable;
         jvar.JvarName = parmlist(2);
         jvar.jJSONName = replaceSpecialJSONChars(jvar.jVarName);
         jvar.jvarstring = tagBlock;
         jvar.jNestLevel = nextNest;

         exec sql insert into jsonvarf values(:jvar);

      when htmlvariable = 'SELECTED' or htmlvariable = 'CHECKED';
         // These variables have an associated 'end' but for JSON
         // will just need a VAR.  The END will be ignored.
         // the RPG program's getVarValues will need to be reviewed and the
         // variable available for the type VAR (may be coded to be valid only
         // for if type is SELECTED). The parm after the variable name
         // is used to indicate the code needs to be reviewed.

         jvar.jvarseq += 1;
         jvar.jvartype = htmlvariable;
         jvar.JvarName = parmlist(2); // variable name.
         jvar.jJSONName = replaceSpecialJSONChars(jvar.jVarName);
         jvar.jvarstring = delim + DQ + parmlist(2) + DQ + ': ' + DQ +
                           '([VAR,' + parmlist(2) + ',checked-selected])' +DQ;
         if nextNest <> *blank;
            nextNest += ' ';    // blank separator between nestLevels
         endif;
         jvar.jNestLevel = nextNest + jvar.jVarName + 'X';

         exec sql insert into jsonvarf values(:jvar);

         nextNest += jvar.jVarName + 'X';

      other;
         // ([PGM ]) ?
         jvar.jvarseq += 1;
         jvar.jvartype = htmlvariable;
         jvar.JvarName = parmlist(2);
         jvar.jJSONName = replaceSpecialJSONChars(jvar.jVarName);
         jvar.jvarstring = tagBlock;
         jvar.jNestLevel = nextNest;

         exec sql insert into jsonvarf values(:jvar);

      endsl;
   enddo;

   return;

end-proc;

//    formatJSONBuffer
//    Returns: a buffer of the
//    Parameters: variable => the variable in the html to parse
//                            should not have start or end tags
// --------------------------------------------------
dcl-proc formatJSONBuffer;
   //SGTST dcl-pi *n     varchar(32000) rtnparm;
   dcl-pi *n     varchar(200000) rtnparm;
      appName  like(jvar_t.japp);
      fileName like(jvar_t.JFILENAME);
   end-pi;

   dcl-s jsonString like(jvar_t.JVARSTRING);
   //SGTST dcl-s jsonBuffer varchar(32000) INZ;
   dcl-s jsonBuffer varchar(200000) INZ;

   // query the jsonvarf file for the json Strings with
   // duplicates removed and orderd by level and name.
   // declare cursor to fetch from qtemp/fileslist
    exec sql
      declare csr1 scroll cursor For
        select jVarString from jsonvarf
        where japp = :appName and jfilename = :filename
        order by jvarseq;

//       where japp = :appName and jfilename = :filename
//   exec sql
//     declare csr1 scroll cursor For
//       select min(jVarString) from jsonvarf
//       where japp = :appName and jfilename = :filename
//       group by JNESTLEVEL, jvarname
//       order by jnestlevel, jvarname ;

   // open cursor
   exec sql
     open csr1;
   jsonBuffer = '{';
   dow sqlCode = 0;
     // fetch next file
     exec sql
       fetch next from csr1 into :jsonString;

     // leave on EOF
     if sqlCode = 100;
       leave;
     endif;
     jsonBuffer += jsonString + x'25';
   enddo;
   exec sql
     close csr1;
   jsonBuffer += '}';
   return jsonBuffer;

end-proc;

// --------------------------------------------------
// getParameters:
//    Purpose: parse an html variable into parameters
//             the first parameter is the controlling variable
//             see MODWEB doc for list of variable tags
//    Returns: an array containing the parameters in order
//    Parameters: variable => the variable in the html to parse
//                            should not have start or end tags
// --------------------------------------------------
dcl-proc getParameters;
   dcl-pi *n varchar(40) dim(6);
      variable  varchar(250) value;
   end-pi;

   dcl-s comma int(5);
   dcl-s parmIdx int(5)  inz(1);
   dcl-s parmList   varchar(40) dim(6) inz;

   // parameters should separated by commas
   // check for more than one parameter
   comma = %scan(',':variable);
   dow comma <> 0 and parmIdx < 6;
      if comma=1; // no data for this parm
         parmIdx +=1;
         if %len(variable)<2;
            leave;
         endif;
         variable = %subst(variable:2);
         comma = %scan(',':variable);
         iter;
      endif;

      // loop through all parameters but the last and process them
      parmList(parmIdx) = %subst(variable:1:comma-1);
      // is the parameter empty?

      // remove the parameter that was just processed
      if %len(variable) > comma;
         variable = %subst(variable:comma+1);
         // find next parameter's end
         comma = %scan(',':variable);
         parmIdx += 1;
      else;
         leave; // no more data after comma
      endif;
   enddo;

   // process last parameter
   parmList(parmIdx) = variable;

   return parmList;

end-proc;


//-----------------------------------------------
// getIFSfile

dcl-proc getifsfile;
   dcl-pi *n varchar(32000);
      ifspath varchar(200) const;
   end-pi;

   // Local Variables...
   dcl-s fileDesc int(10);
   dcl-s pStrLen int(10);
   dcl-s readPtr pointer;
   dcl-s pstring char(32001);
   dcl-s count int(10) inz(1);
   dcl-s webstring varchar(32000);

   // Start processing...

   // Open ifs web file...
   // Open ifs web file...
   fileDesc = open(%trimr(ifsPath):
      O_RDWR + O_TEXTDATA :
      S_IRUSR + S_IWUSR + S_IRGRP :
      37);

   if filedesc < 0;
      return 'Failed to open file' + ifsPath;
   endif;
   pStrLen = read(filedesc:%addr(pString):%size(pString));
   if pStrLen<= 0;
      return 'Failed to read file' + ifsPath;
   endif;

   webstring = %subst(pstring:1:pStrLen);
   return webstring;

end-proc;

//-----------------------------------------------
//  replaceSpecialJSONChars

dcl-proc replaceSpecialJSONChars;
   dcl-pi *n like(jvar_t.jJSONName);
      inVariable like(jvar_t.jJSONName);
   end-pi;
   dcl-s jsonVar like(jvar_t.jJSONName);

   // the dash, 'at', hash and dollar signs should be replaced with underscore
   //
   jsonVar =  %scanrpl('-':'_':inVariable);
   jsonVar =  %scanrpl('#':'_':jsonVar);
   jsonVar =  %scanrpl('$':'_':jsonVar);
   jsonVar =  %scanrpl('@':'_':jsonVar);

   return jsonVar;

end-proc;

//-----------------------------------------------
//  writeJsonFile

dcl-proc writeJsonFile;
   dcl-pi *n ind;
      ifspath varchar(80);
      //SGTST jsonBuffer varchar(32000);
      jsonBuffer varchar(200000);
      replace char(1);
   end-pi;

   dcl-s fd int(10); // file descriptor
   dcl-s F_OK int(10) inz(0);
   //SG dcl-C CCSID const(1208);   // 1208 is UTF-8
   dcl-C CCSID const(437);   // 1208 is UTF-8

   // change File name extension from html to json
   ifspath = %replace('json':ifspath:%len(%trim(ifspath))-3);
   ifspath = %trim(ifspath);

   // If the file should NOT be replaced then check if it exists
   if replace = 'n' or replace = 'N';
      if access(%trimr(ifspath):F_OK) = 0; // File Exists
         return FALSE;
      endif;
   endif;

   // Create the json file with the right CCSID (81(
   fd = open(ifspath
      : O_WRONLY + O_CREAT + O_TRUNC + O_CODEPAGE
      : S_IRUSR + S_IWUSR + S_IRGRP +  S_IROTH
      : CCSID );
   if fd < *zero;
      return FALSE;
   endif;

   callp close(fd);
   // Open file as text file (conversion to ASCII)
   fd = open(ifspath
        : O_RDWR + O_TEXTDATA );
   if fd < *zero;
      return FALSE;
   endif;
   callp write(fd : %addr(jsonBuffer:*data) : %len(jsonBuffer));

   callp close(fd);

   return TRUE;
end-proc;
