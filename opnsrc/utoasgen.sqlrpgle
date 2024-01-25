**FREE
// ***************************************************************************************
// Program    : UTOASGEN
// Description: Utility program to generate Open API Specifications for web programs
//              based on request/response data
// Author     : SenecaGlobal
// Date       : 08/20/2020
//
//
// Parameters - This program receives following parameters :
//              1. Application Name (eg: SCHMSGSCGI)
// ***************************************************************************************

ctl-opt option(*NOXREF:*NODEBUGIO:*SRCSTMT)  dftactgrp(*NO)
        bnddir('QC2LE':'YAJL':'HTTPAPI');

// Prototype for entry Parameters...
dcl-pi *n;
   pAppNam char(10);
end-pi;

// Prototype for to call HTTPAPI process
dcl-pr UTOASAPI extpgm('UTOASAPI');
   application_name char(10);
end-pr;

// Prototype for QCMDEXC
dcl-pr qcmdexc extpgm('QCMDEXC');
   theCommand      char(5000);
   cmdLength       packed(15:5);
end-pr;

/define stat
/include qsysinc/qrpglesrc,sysstat
/copy rpgcopy,ifshead

 // yajl variables
/include yajl/QRPGLESRC,yajl_h


// Constants....
dcl-c CR  X'0D';
dcl-c LF  X'25';
dcl-c DQT '"';
dcl-c TAB X'05';
dcl-c SQT  '''';
dcl-c COL  ':';
dcl-c COM  ',';
dcl-c OBR  '{';
dcl-c CBR  '}';
dcl-c TYPE     'type';
dcl-c DESC     'description';
dcl-c PROP     'properties';
dcl-c EXMPL    'example';
dcl-c OBJ      'object';
dcl-c ARR      'array';
dcl-c ITMS     'items';

// Local Variables...
dcl-s reqData  varchar(150000);
dcl-s resData  varchar(150000);
dcl-s pathCounter int(5) Inz(0);
dcl-s schemaCounter int(5) Inz(0);
dcl-s prvReqId char(30) Inz;
dcl-s firstRequest Ind Inz(*on);
dcl-s dataOAS   varchar(6000000) inz;

// data structures
dcl-ds logrec extname('UTOASLOG') end-ds;

//****************************************************************************************
// Start Main Processing...
//****************************************************************************************

exec sql set option commit=*none, closqlcsr=*endmod;

// call HTTPAPI program (UTOASAPI) to process any pending requests
// and get the reponses from web apllication
//-----------------------------------------------------------------
UTOASAPI(pAppNam);


// Main procedure
//---------------
generateOAS(pAppNam);

*inlr = *on;


//*****************************************************************************
// generateOAS: Generate Open API Specification for Application
//*****************************************************************************
dcl-proc generateOAS;
   dcl-pi *n;
      applPath char(10) value;
   end-pi;

   // clear request and response workfields
   clear reqData;
   clear resData;

   // declare cursor to read Request/Responses from UTOASLOG file
   exec sql
     declare oasCsr1 scroll cursor For
       select * from CIDEMO.UTOASLOG where rr_appnm = :pAppNam
                         order by rr_reqid, rr_rctyp, rr_rseq;

   // open cursor
   exec sql
     open oasCsr1;

   dow sqlCode = 0;

      // fetch next file
      exec sql
         fetch next from oasCsr1 into :logRec ;

      // leave on EOF
      if sqlCode = 100;
         // process any pending requests before leaving
         if prvReqId <> *blanks;
            processRequest();
         endif;
         leave;
      endif;

      // New request? write process and write old data to JSON file
      if prvReqId = *Blanks;
         prvReqId = rr_reqid;
      elseif prvReqId <> rr_reqid;
         processRequest();
         prvReqId = rr_reqid;
      endif;

      // sum up all request and response data
      if rr_rctyp = 'REQ';
         reqData = %trim(reqData) + ' '+ %trim(rr_data);
      elseif rr_rctyp = 'RES';
         resData = %trim(resData) + ' '+ %trim(rr_data);
      endif;

   enddo;

   // Close cursor
   exec sql
     close oasCsr1;

   // Write JSON file to IFS
   if dataOAS <> *blanks;
     writeJSON();
   endif;

end-proc;

//*****************************************************************************
// processRequest: Process a request and add to json file
//      parms: *none
//    returns: *none
//*****************************************************************************
dcl-proc processRequest;
dcl-pi *n;
end-pi;

  // convert request data to OAS format JSON format
  reqData = formatOAS(reqData:'REQ');

  // convert response data to OAS format JSON format
  resData = formatOAS(resData:'RES');

  // add request/response OAS JSON data to file
  addOASdata();

  // clear workfields before processing next request
  clear reqData;
  clear resData;

end-proc;


// *********************************************************************
// formatOAS: Read JSON and build Open API style of JSON
//     parms: inData - Input data in normal JSON format
//   returns: Output string - OAS in JSON format
// *********************************************************************
dcl-proc formatOAS;
dcl-pi *n like(reqData);
  inData like(reqData) value;
  inType char(3) value;
end-pi;

dcl-s yajlErrMsg varchar(500) inz('');
dcl-s docNode like(yajl_val);
dcl-s result  like(reqData) ;
dcl-s indent varchar(100);

  // remove any headers
  if %scan(OBR:inData:1) > 0;
    inData = %subst(inData:%scan(OBR:inData:1));
  endif;

  // load JSON tree from input string
  docNode =  yajl_string_load_tree(inData  :  yajlErrMsg );

  // no path, add dummy path
  if %trim(prvReqid) = '_';
     prvReqid = 'FRM'+ %char((pathcounter+1))+'_ACT'+ %char((pathcounter+1));
  endif;

  if yajlErrMsg <> '';
     result = yajlErrMsg;
  else;
    indent = TAB ;
    result = DQT + %trim(prvReqId) + '_' + %trim(inType) +
             DQT + COL + TAB +
             %trim(processNode(docNode:' ':indent)) + CR + LF;
  endif;

  return result;

end-proc;

// *********************************************************************
// processNode - Process JSON Node
//  parms
//    inNode - Input Node
//    inDesc - Description of node
//    inIndent - Indentation String
//  returns output string - OAS in JSON format
// *********************************************************************
dcl-proc processNode;
dcl-pi *n like(reqData) ;
  inNode like(yajl_val) value;
  inDesc varchar(50) value;
  inIndent varchar(100) value;
end-pi;

  Select;
  when (YAJL_IS_OBJECT(inNode));
    return processObject(inNode:inDesc:inIndent);
  When (YAJL_IS_ARRAY(inNode));
    return processArray(inNode:inDesc:inIndent);
  When (YAJL_IS_STRING(inNode));
    return processValue('string':inDesc:yajl_get_string(inNode):inIndent);
  When (YAJL_IS_NUMBER(inNode));
   return processValue('number':inDesc:%char(yajl_get_number(inNode)):inIndent);
  When (YAJL_IS_TRUE(inNode));
    return processValue('boolean':inDesc:'true':inIndent);
  When (YAJL_IS_FALSE(inNode));
    return processValue('boolean':inDesc:'false':inIndent);
  EndSl;

  return '';

end-proc;


// *********************************************************************
// processObject - Process JSON Object
//  parms
//    inObj - Input JSON Object
//    inDesc - Node description
//    inIndent - Indentation String
//  returns output string - OAS in JSON format
// *********************************************************************
dcl-proc processObject;
dcl-pi *n like(reqData);
  inObj like(yajl_val) value;
  inDesc varchar(50)   value;
  inIndent varchar(100)   value;
end-pi;
dcl-s objKey  varchar(50);
dcl-s objVal  like(yajl_val);
dcl-s objIdx  int(10);
dcl-s resObj  like(reqData);

  inIndent = %trim(inIndent) +  TAB;
  resObj = OBR + CR + LF + %trim(inIndent) +
    DQT + TYPE + DQT + COL + TAB + DQT + OBJ + DQT + COM +
    CR + LF + %trim(inIndent) +  DQT + PROP + DQT + COL + TAB + OBR;

  inIndent = %trim(inIndent) + TAB ;

  objIdx = 0;
  dow YAJL_OBJECT_LOOP(inObj: objIdx: objKey: objVal);
      resObj = %trim(resObj) + CR + LF + %trim(inIndent) +
               DQT + %trim(objKey) + DQT + COL + TAB +
               %trim(processNode(objVal:objKey:inIndent)) + COM ;
  enddo;

  // remove comma after last object
  %subst(resObj:%scanr(COM:resObj):1) = ' ';

  resObj = %trim(resObj) + CR + LF + %trim(inIndent) + CBR + CBR;

  return resObj;

end-proc;

// *********************************************************************
// processArray - Process JSON Array
//  parms
//    inObj - Input JSON Array
//    inDesc - Node description
//    inIndent - Indentation String
//  returns output string - OAS in JSON format
// *********************************************************************
dcl-proc processArray;
dcl-pi *n like(reqData);
  inArr like(yajl_val)  value;
  inDesc varchar(50)    value;
  inIndent varchar(100)    value;
end-pi;
dcl-s arrKey  varchar(50);
dcl-s arrVal  like(yajl_val);
dcl-s arrIdx  int(10);
dcl-s resArr  like(reqData);
dcl-s resArr2 like(reqData);

  inIndent = %trim(inIndent) + TAB;
  resArr = CR + LF +
    %trim(inIndent) + OBR +
    CR + LF + %trim(inIndent) +
    DQT + TYPE + DQT + COL + TAB + DQT + ARR  + DQT + COM +
    CR + LF + %trim(inIndent) +
    DQT + DESC + DQT + COL + TAB + DQT + inDesc + DQT + COM +
    CR + LF + %trim(inIndent) +
    DQT + ITMS + DQT + COL + TAB  ;

  arrIdx = 0;
  dow YAJL_ARRAY_LOOP(inArr: arrIdx: arrVal);
    resArr2 = %trim(resArr2) + %trim(processNode(arrVal:'':inIndent));
    leave;
  enddo;

  // handle empty array
  if resArr2 = *blanks;
    resArr2 = OBR + CBR;
  endif;

  resArr = %trim(resArr) + %trim(resArr2) +
           CR + LF + %trim(inIndent) +  CR + LF + CBR;

  return resArr;

end-proc;

// *********************************************************************
// processValue - Process JSON String, Number, Boolean
//  parms
//    inType  - Data Type
//    inDesc  - Description
//    inExmp  - Example Data
//    inIndent - Indentation String
//  returns output - OAS in JSON format
// *********************************************************************
dcl-proc processValue;
dcl-pi *n like(reqData);
  inType  char(20)     value;
  inDesc  varchar(50)  value;
  inExmp  char(1000)   value;
  inIndent varchar(100)  value;
end-pi;
dcl-s resVal  like (reqData);

  // key/value pairs
  resVal = OBR + CR + LF +
  %trim(inIndent) + TAB + DQT + TYPE + DQT + COL +
                          DQT + %trim(inType) + DQT + COM + CR + LF +
  %trim(inIndent) + TAB + DQT + DESC + DQT + COL +
                          DQT + %trim(inDesc) + DQT + COM + CR + LF +
  %trim(inIndent) + TAB + DQT + EXMPL + DQT + COL +
                          DQT + %trim(inExmp) + DQT + CR + LF +
  %trim(inIndent) + CBR ;

  return resVal;
end-proc;


// *********************************************************************
// addOASdata:  Add processed req/res OAS data to string
//      prams: *none
//    returns: *none
// *********************************************************************
dcl-proc addOASdata;
dcl-pi *n ;
end-pi;
dcl-s tmplReq    char(32000) inz;
dcl-s pString    char(32000) inz;
dcl-s pStrLen    int(10) inz;
dcl-s filedesc   int(10:0) inz;
dcl-s ifsPath    char(100) inz;
dcl-s form       char(10) inz;
dcl-s action     char(10) inz;
dcl-s toReplace  char(20) inz;
dcl-s startIdx   int(10) inz;
dcl-s endIdx     int(10) inz;

   // retrieve form id and action from request id
   form   = %subst(prvReqId:1:%scan('_':prvReqId)-1);
   action = %subst(prvReqId:%scan('_':prvReqId)+1);

   // generic information has been placed in IFS files
   // they are read here and specific content will be added
   //------------------------------------------------------

   // read header template from IFS, only for first request
   if firstRequest;
      firstRequest = *off;

      // Read header remplate
      ifsPath = '/home/VAMSIM/OPENAPI/template_header.json';

      // Open ifs web file...
      fileDesc = open(%trimr(ifsPath):
                    O_RDWR + O_TEXTDATA :
                    S_IRUSR + S_IWUSR + S_IRGRP :
                    37);

      // exit in case of errors in fetching templates
      if filedesc < 0;
         return;
      endif;

      // Read file from ifs
      pStrLen   = read(filedesc:%addr(pString):%size(pString));

      dataOAS = %trim(pString);
      clear pString;

      // Close file...
      callp close(filedesc);
   endif;


   // Read Request Template
   ifsPath = '/home/VAMSIM/OPENAPI/template_req.json';

   // Open ifs web file...
   fileDesc = open(%trimr(ifsPath):
                  O_RDWR + O_TEXTDATA :
                  S_IRUSR + S_IWUSR + S_IRGRP :
                  37);

   if filedesc < 0;
      return;
   endif;

   // Read file from ifs
   pStrLen   = read(filedesc:%addr(pString):%size(pString));

   tmplReq  = %trim(pString);
   clear pString;

   // Close file...
   callp close(filedesc);

   // Scan and replace request id
   tmplReq  = %scanRpl('&REQUESTID':%trim(prvReqId):%trim(tmplReq));
   tmplReq  = %scanRpl('&FORM':%trim(form    ):%trim(tmplReq));
   tmplReq  = %scanRpl('&ACTION':%trim(action  ):%trim(tmplReq));

   // Add current request path to file
   // It consits of request body, response, and related information
   // and the specific req/res data will be added below under components
   pathCounter = pathCounter + 1;
   toReplace = '&PATH'+%editc(pathCounter:'X');
   if pathCounter > 1;
      tmplReq = COM + CR + LF + %trim(tmplReq);
   endif;
   dataOAS = %scanRpl(%trim(toReplace):%trim(tmplReq):dataOAS);


   // Add current request data (schemas under compoents) to file
   schemaCounter = schemaCounter + 1;
   toReplace = '&SCHEMA'+%editc(schemaCounter:'X');
   if schemaCounter > 1;
      reqData = COM + CR + LF + %trim(reqData);
   endif;
   dataOAS = %scanRpl(%trim(toReplace):%trim(reqData):dataOAS);

   // Add current response data to file
   schemaCounter = schemaCounter + 1;
   toReplace = '&SCHEMA'+%editc(schemaCounter:'X');
   if schemaCounter > 1;
      resData = COM + CR + LF  + %trim(resData);
   endif;
   dataOAS = %scanRpl(%trim(toReplace):%trim(resData):dataOAS);

   // replace app name
   dataOAS = %scanRpl('&APPNAM':%trim(pAppNam):dataOAS);

end-proc;

// *********************************************************************
// writeJSON - write the final JSON string to IFS file
// *********************************************************************
dcl-proc writeJSON;
dcl-pi *n ;
end-pi;

dcl-s wCommand      char(5000);
dcl-s wCommandLen   packed(15:5);
dcl-s toReplace char(30);

  // remove any unused schemas from dataOAS string
  schemaCounter = schemaCounter + 1;
  removeString('&SCHEMA'+%editc(schemaCounter:'X'):CBR);

  // remove any unused paths from dataOAS string
  pathCounter = pathCounter + 1;
  removeString('&PATH'+%editc(pathCounter:'X'):CBR);

  // insert data into work file
  insertData();

  // copy workfile with OAS data to IFS
  wCommand  = 'CPYTOIMPF FROMFILE(UTOASDTA) TOSTMF(' + SQT +
              '/home/VAMSIM/OPENAPI/' + %trim(pAppNam)+'.json'+ SQT +
              ') MBROPT(*REPLACE) RCDDLM(*CRLF) STRDLM(*NONE)'+
               ' STMFCCSID(*PCASCII)';
  wCommandLen = %Len(wCommand);
  qcmdexc(wCommand : wCommandLen);

end-proc;

//*****************************************************************************
// removeString - Remove string between 2 words/characters in source string
//           Input parameters : inData - data to be scanned and replaced in
//                              inStrWord - starting word
//                              inEndWord - ending word
//           Returns: *none
//*****************************************************************************
dcl-proc removeString;
   dcl-pi *n ;
      inStrWord     varchar(100) value;
      inEndWord     varchar(100) value;
   end-pi;
   dcl-s idxStr int(10) inz;
   dcl-s idxEnd int(10) inz;

   idxStr = %scan(%trim(inStrWord):dataOAS);
   if idxStr > 0;
     idxEnd = %scan(%trim(inEndWord):dataOAS:idxStr);
     if idxEnd > idxStr;
        dataOAS = %trim(%subst(dataOAS:1:idxStr-1))
             + %trim(%subst(dataOAS:idxEnd));
     endif;
   endif;
end-proc;

//*****************************************************************************
// insertData - Insert Open API data to work file to write to IFS file
//           Input parameters : *none
//           Returns: *none
//*****************************************************************************
dcl-proc insertData;
   dcl-pi *n ;
   end-pi;
   dcl-c recLen 32000;
   dcl-c clrFile 'CLRPFM FILE(UTOASDTA)';
   dcl-s wCommand      char(5000);
   dcl-s wCommandLen   packed(15:5);
   dcl-s idx int(10);
   dcl-s string varchar(32000);
   dcl-s strLen int(10) inz;
   dcl-s inLen int(10)  inz;

      // clear the work file
      wCommand = clrFile;
      wCommandLen = %Len(wCommand);
      qcmdexc(wCommand : wCommandLen);

      // calculate the length of response data
      inLen = %len(%trim(dataOAS));

      // response length can be greater than record length, so split and save
      for idx = 1 to inLen by recLen;
         if (idx + recLen -1) > inLen;
            strLen = inLen - idx + 1;
         else;
            strLen = recLen;
         endif;

         string = %subst(dataOAS:idx:strLen);

         // write data to workfile
         exec sql insert into UTOASDTA values(:string);

      endfor;

   end-proc;

