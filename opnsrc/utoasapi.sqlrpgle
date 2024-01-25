**FREE
// ***************************************************************************************
// Program    : UTHTTPAPI
// Description: OAS - Use HTTPAPI to call web apps and receive response
//              - Read UTOASLOG file for request data
//              - Call HTTP API with request data
//              - Save the response in UTOASLOG file
//
// Author     : SenecaGlobal
// Date       : 08/20/2020
//
// Parameters - This program receives following parameters :
//              1. Application Name (eg: SCHMSGSCGI)
// ***************************************************************************************
ctl-opt dftactgrp(*no) bnddir('HTTPAPI');

// Prototype for entry Parameters...
dcl-pi *n;
   pAppNam char(10);
end-pi;

/copy libhttp/qrpglesrc,httpapi_h

// Constants....

// Variables...
dcl-s returnCode int(10:0);
dcl-s respData varchar(200000);
dcl-s formData varchar(32767);

// data structures
dcl-ds logrec extname('UTOASLOG') end-ds;


//****************************************************************************************
// Start Main Processing...
//****************************************************************************************
// set SQL options
exec sql set option commit=*none, closqlcsr=*endmod;


// Read through all requests of application in UTOASLOG and process
procApp(pAppNam);

// end of program
*inlr = *on;
//*****************************************************************************

//*****************************************************************************
// procApp - Read through all requests of application in UTOASLOG, call API
//           save the response back in UTOASLOG file
//           Input parameters : inAppNam - Application Name
//           Returns - *none
//*****************************************************************************
dcl-proc procApp;
   dcl-pi *n;
      inAppNam like(pAppNam) value;
   end-pi;
   // local variables
   dcl-s appURL varchar(200);
   dcl-s result varchar(200000);

   // constants
   dcl-c URL 'https://www.credentials-inc.com/CGI-BIN/';
   dcl-c PGM '.pgm';

   // build application URL for API call, based on input application name
   appURL = URL + %trim(inAppNam) + PGM;

   // declare cursor to read request/responses from UTOASLOG file
   exec sql
     declare apiCsr1 scroll cursor For
       select * from CIDEMO.UTOASLOG
                where rr_appnm = :inAppNam and rr_rctyp = 'REQ'
                      and rr_prcs <> 'P'
                order by rr_reqid, rr_rctyp, rr_rseq;

   // open cursor
   exec sql
     open apiCsr1;

   dow sqlCode = 0;

      // fetch next file
      exec sql
         fetch next from apiCsr1 into :logRec ;

      // leave on EOF
      if sqlCode = 100;
         leave;
      endif;

      // process one request at a time
      result = procReq(appURL:rr_data);

      // save response in UTOASLOG file
      savResp(rr_appnm:rr_reqid:result);

   enddo;

   // Close cursor
   exec sql
     close apiCsr1;

end-proc;

//*****************************************************************************
// procReq - Process one request at time by calling HTTPAPI and return response
//           Input parameters : inURL  - Application URL
//                              inData - Input request data
//           Returns: Response Data
//*****************************************************************************
dcl-proc procReq;
   dcl-pi *n varchar(200000);
      inURL  varchar(200);
      inData like(rr_data);
   end-pi;
   dcl-c GET  'GET';
   dcl-c POST 'POST';
   dcl-s returnCode int(10:0);
   dcl-s respData varchar(200000);

   // enable API debug
   http_debug(*on);

   // set HTTP options
   http_setOption('Content-Type':'text/json');

   // send a HTTP POST request to application
   returnCode = http_req(POST
                        :%trim(inURL)
                         :*omit
                         :respData
                         :*omit
                         :inData);

  // on success, return response
  // and on failure, send the failure message
  if returnCode <> 1;
     return 'Error occurred while calling HTTPAPI';
  else;
     return respData;
  endif;
end-proc;

//*****************************************************************************
// savResp - Save HTTPAPI response in UTOASLOG file
//           Input parameters : inAppNam - Application name
//                              inReqId  - Request Id
//                              inData   - Response data to be saved
//           Returns: *none
//*****************************************************************************
dcl-proc SavResp;
   dcl-pi *n ;
      inAppNam  like(rr_Appnm);
      inReqId   like(rr_ReqId);
      inData    varchar(200000);
   end-pi;
   dcl-c recLen 32000;
   dcl-s idx     int(10);
   dcl-s string  varchar(32000);
   dcl-s strLen  int(10) inz;
   dcl-s inLen   int(10)  inz;
   dcl-s counter int(5) inz;
   dcl-s status  char(1) inz;

      // calculate the length of response data
      inLen = %len(%trim(inData));

      // check if this is data or error message
      if %subst(%trim(inData):1:5) = 'Error';
        status = 'E';
      else;
        status = 'P';
      endif;

      // delete any existing records
      exec sql
         delete from CIDEMO.UTOASLOG
                where rr_appnm  = :inAppNam and
                      rr_reqid  = :inReqId  and
                      rr_rctyp  = 'RES';

      // update request as processed
      exec sql
         update CIDEMO.UTOASLOG
                set rr_prcs = :status
                where rr_appnm  = :inAppNam and
                      rr_reqid  = :inReqId  and
                      rr_rctyp  = 'REQ';


      // response length can be greater than record length, so split and save
      for idx = 1 to inLen by recLen;
        if (idx + recLen -1) > inLen;
          strLen = inLen;
        else;
          strLen = recLen;
        endif;

        string = %subst(inData:idx:strLen);

        counter += 1;

        // save the response in log file
        exec sql
          insert into CIDEMO.UTOASLOG
                values(:status, :inAppNam, :inReqId,'RES', 1, :string);

      endfor;

   end-proc;

