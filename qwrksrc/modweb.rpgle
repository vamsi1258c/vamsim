      **************************************************************************
      *  (C) Copyright 2016, Credentials Solutions, LLC
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
      *  Description : Web module common procedures
      *  Document location: ZCredntl/Secure/TechDocs/Docs/Pgms/MODWEB.html
      *
      *  Written by  : SAS
      *  Date Written: 04/12/2016
      **************************************************************************
      *  Module Name          : MODWEB
      *
      *  Main Subprocedures
      *  web_processFile         : retrieves, parses, and sends a file
      *  web_getIFSfile          : retrieves a file's content from server_wp
      *  web_replacevariables    : parses a buffer looking for tags to replace
      *
      *  Helper Subprocedures
      *  web_addToBuffer         : add content to a buffer
      *  web_clean               : convert HTML special chars to encoded chars
      *  web_getFileExtension    : retrieve a the file extension for uploaded files
      *  web_getMessage          : retrieve a message from a message file
      *  web_getMessagesList     : retrieve a group of messages from a message file
      *  web_getUploadBuffers    : retrieve a number of buffers for an upload
      *  web_identifyBrowser     : identify what browser a request is from
      *  web_objectExists        : check to see if an object exists
      *  web_sanitize            : clean input based on a white list
      *  web_sendBuffer          : send a buffer out
      *  web_urlDecode           : translate a url with special characters
      *  web_urlDecodeUtf8       : translate a utf8 url with special characters
      *  web_validProtocol       : check a passed web protocol is valid
      *  web_zGetMessage         : retrieve a message from a message file
      *
      *  Internal Subprocedures
      *  doError                 : send admin e-mail when an error occurs
      *  getContent              : get the content between a start and end variable
      *  parseSwith              : get the valid content between SWITCH block
      *  getParameters           : parse an html variable into parameters
      *  sanitizeBrowser         : convert HTML special chars to blanks for browsers
      **************************************************************************
      *  Change History:
      * -------- --- -----------------------------------------------------------
      * 07/12/19 TDR Addeds support for 'RESOURCE' in replaceVariables
      * 05/03/19 JWG Changed web_getifsfile to handle getting the file direct when
      *              server_wp is down or doesnt respond in time
      * 03/04/19 SAS Updated to not call server_wpc anymore, just log that we got
      *               got the webpages on our own.
      * 11/29/18 JWG Added @;: to ADDRESS constant in web_clean
      * 10/02/18 JWG Added call to MODML when template language folder found in
      *              the web page path
      *              Added web_setRootPath to standardize how the root path is set
      *              Added web_setPanelFile for resetting panel file to .js/.css and
      *              applying test text to the file names
      * 05/22/18 MRB Updated to only use SERVER_WP when file is production
      * 03/29/18 SAS Add & to list of valid email chars
      * 03/08/18 TDR Increasing the length of getContent return value
      * 02/20/18 TDR Updated 'FILE' tag to call web_processFile
      *              added a check to make sure processSwitch exists before calling
      * 02/08/18 SAS Removed CLLCWWWPGM hard coding
      * 12/22/17 SAS Added check for WF files in web_getIFSfile, only use server_WP
      *               to get webfiles not any other file (such as TPIN)
      * 08/22/17 TDR Added support for PGM replacement variable
      * 07/24/17 TDR Added support for SERVER_EP DTAQs
      *              Added web_objectExists procedure to check if object exists
      * 05/05/17 SG  Code Delivery #17
      * 03/14/17 SG  Updated web_getIFSFIle to request web and email pages seperately
      * 06/29/17 JWG Updated clean_address in web_clean to have three more chars
      * 02/01/17 MRB Updated error handling if server WP is ever down or not responding
      * 01/25/17 MRB Added clean constant for text areas
      * 12/27/16 JWG Added single quote to web clean address
      * 11/28/16 JWG Added new procedures for file upload processing, getting the
      *              file extension and the number of upload buffers
      *              organized global variables
      * 11/02/16 JWG Modified web_zGetMessage PI to make ovrmsg,rplcdata,rplclen
      *              *nopass and removed ovrfile, added new clean constant
      *              change shift loop in replacevariables to use %subarr
      * 10/28/16 SG  Added capability to handle SWITCH variable in html files
      * 10/21/16 JWG fixed second parm not setting validchars if not a constant and
      *              not blank in web_clean
      * 10/21/16 JWG Added web_zgetmessage to retrieve a msg from a msgfile
      *              and allow for var substitution, changed web_clean procedure
      *              interface, added new clean constant checks for phone, name,
      *              and base, default web_clean to alphnumeric with space
      *              removed unused clean constants select/when
      * 08/25/16 JWG Added MS EDGE browser scan to web_identifyBrowser
      * 08/23/16 DDZ Added check of workBuffer length during sendPrevious and addPrevious
      * 08/08/16 MRB Updated web_clean, added , to address and ., to full name
      *              Resorted procedures
      * 07/07/16 DDZ add web_clean
      * 06/07/16 DDZ changed web_sanitize to be OWASP suggestions
      *              change name of htmlLine
      * 05/13/16 SG  1) Added capability to restart SERVER_WP process inside of
      *                 web_getIFSFile
      *              2) Added below new procedures to retrieve messages
      *                 web_getIFSFile
      *                 web_getmessageslist
      * 05/08/16 SG  Added new procedure web_addToBuffer which builds up buffer
      *              output before sending it
      * 05/05/16 SG  Added web_processFile which calls below procedures
      *                web_getIfsFile
      *                web_replaceVariables
      *                web_sendBuffer
      * 04/26/16 SG  Added following procedures
      *                web_getIfsFile
      *                web_sendBuffer
      *                web_replaceVariables
      * 04/12/16 SAS New module for web programs
      **************************************************************************
     h nomain
     h option(*SRCSTMT)

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Prototypes
      /copy prototypes,WEB
      /copy prototypes,ML
      /copy prototypes,zsystemcmd

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Copy Books(Declaration)
      /copy rpgcopy,parsedsbas
      /copy rpgcopy,parseds70
      /copy rpgcopy,cllcpsds
      /copy rpgcopy,statusDs
      /copy rpgcopy,zhskpg_ds

       // Global Variable Declaration - will go in PROTOTYPE and be brought in
       //  this is an exception to the rule of no d-specs in prototypes

       // Global boolean constants
       dcl-c FALSE '0';
       dcl-c TRUE '1';

       dcl-c ENDTAG '])';
       dcl-c STARTTAG '([';

       dcl-c LINEFEED x'15';
       dcl-c NEWLINE x'25';
       dcl-c HTTPUSER 'QTMHHTTP  ';

       dcl-c DFT_DISTRO 'ONC';

       dcl-s urlDecodeParm like(urlDecode);
       dcl-s idx int(10);

       // variables for the CGI interface API for QtmhWrStout
       dcl-s buffer like(fileBuffer) inz;
       dcl-s bufferTrip int(10) inz(26000);

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Main Subprocedues
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

       // --------------------------------------------------
       // web_processFile: EXTERNAL
       //    Purpose: read a file from Ifs, replace variables, and send the buffer
       //    Parameters: rootPath => the root Ifs folder that holds the file to read
       //                fileName => the file in the rootPath to read
       //                ptrGetVariable => procedure that specifies what to replace
       //                                  html tags with
       //                ptrProcessIf => procedure to handle ([IF]) tags
       //                ptrProcessLoop => procedure to handle ([LOOP]) tags
       //                ptrProcessSwitch => procedure to handle ([SWITCH]) tags
       //                ptrOvrSndBuffer => procedure to override how the buffer is sent out
       // --------------------------------------------------
       dcl-proc web_processFile export;
       dcl-pi *n;
          rootPath             like(#wp_in.webPage) const;
          fileName             like(#wp_in.webPage) const;
          ptrGetVariable       pointer(*proc) const;
          ptrProcessIf         pointer(*proc) const;
          ptrProcessLoop       pointer(*proc) const;
          ptrProcessSwitch     pointer(*proc) const options(*nopass:*omit);
          ptrOvrSndBuffer      pointer(*proc) const options(*nopass);
       end-pi;

     d fileHtml        s                   like(#wp_out.result)
     dstatus           ds                  likeds(statusDs)

       if rootPath = *blanks or fileName = *blanks;
          doError(DFT_DISTRO:
                  'Attempted to read a blank file ' +
                  %trim(rootPath) + %trim(fileName) +
                  ' to buffer');

SG     //? What should be the process in case of errors? do we need to
       //  process Fatalerror.html (as in CGISHELL2?)

       endif;

       // read html file from IFS into buffer
       fileHtml = web_getIfsFile(%trim(rootPath) + %trim(fileName):
                                 status);

       if status.code = SUCCESS;
          // replace the variables and send it to buffer
          if %parms() >= %parmNum(ptrOvrSndBuffer);
             web_replaceVariables(fileHtml:
                                  %trim(rootPath):
                                  ptrGetVariable:
                                  ptrProcessIf:
                                  ptrProcessLoop:
                                  ptrProcessSwitch:
                                  ptrOvrSndBuffer);
          else;
             if %parms() >= %parmNum(ptrProcessSwitch);
                web_replaceVariables(fileHtml:
                                     %trim(rootPath):
                                     ptrGetVariable:
                                     ptrProcessIf:
                                     ptrProcessLoop:
                                     ptrProcessSwitch);
             else;
                web_replaceVariables(fileHtml:
                                     %trim(rootPath):
                                     ptrGetVariable:
                                     ptrProcessIf:
                                     ptrProcessLoop);
             endif;
          endif;
       endif;

       // send html data in buffer
       if %parms() >= %parmNum(ptrOvrSndBuffer);
          web_sendBuffer(*omit:ptrOvrSndBuffer);
       else;
          web_sendBuffer();
       endif;

       clear buffer;

       end-proc;

       // --------------------------------------------------
       // web_getIfsFile: EXTERNAL
       //    Purpose: to retrieve an Ifs file using server_wp
       //    Returns: a buffer that contains the file data
       //    Parameters: webPage => the Ifs path to the file to read
       //                status => data structure to communicate success
       // --------------------------------------------------
       dcl-proc web_getIfsFile export;
       dcl-pi *n               like(#wp_out.result) rtnparm;
          webPage              like(#wp_in.webPage) const;
          @status              likeds(statusDs);
       end-pi;

      /copy prototypes,dataqueues

      * Variables
       dcl-c PROD_DIR '/WF/P/';

       dcl-s dtaqInputName char(10);
       dcl-s dtaqOutputName char(10);
       dcl-s errorText char(800);

       dcl-s useWp ind inz(FALSE);
       dcl-s useEp ind inz(FALSE);
       dcl-s getFileDirect ind inz(FALSE);
     drqseq#           s                   like(#wp_in.rqseq#) static
     d#wp_out_len      s              5p 0
     dwpIsUp           s               n   inz(TRUE) static
     depIsUp           s               n   inz(TRUE) static

       // Get HTML message from the IFS file
       clear #wp_in;
       clear #wp_out;
       #wp_in.jobn = pgm_jobname;
       #wp_in.jobu = pgm_userid;
       #wp_in.job# = pgm_jobnbr;

       #wp_in.rqdate = %dec(%date():*iso);
       #wp_in.rqtime = %dec(%time():*iso);
       #wp_in.rqseq# = rqseq# + 1;

       #wp_in.func = 'GP';
       #wp_in.webPage = %trim(webPage);

       reset useWp;
       reset useEp;
       reset getFileDirect;
       if %scan(PROD_DIR:%xlate(xlc:xuc:#wp_in.webPage)) = 0;
          getFileDirect = TRUE;

       elseif (pgm_userid = HTTPUSER);
          useWp = TRUE;
          dtaqInputName = #wp_name_i;
          dtaqOutputName = #wp_name_o;

       else;
          useEp = TRUE;
          dtaqInputName = #ep_name_i;
          dtaqOutputName = #ep_name_o;
       endif;

       // Send request to retrieve HTML data for the file from IFS
       if (useWp and wpIsUp) or (useEp and epIsUp);
          qsnddtaq(dtaqInputName:
                   #WP_LIB:
                   %len(#wp_in):
                   #wp_in);
       endif;

       if wpIsUp = FALSE or epIsUp = FALSE;
          getFileDirect = TRUE;
       endif;

       // In case there was an error before this call, keep reading
       // until the correct file is returned
       dow #wp_out.webPage <> #wp_in.webPage;
          clear #wp_out_len;
          clear #wp_out;

          if getFileDirect;
             #wp_out.result = readFile(#wp_in.webPage);
             #wp_out_len = %len(#wp_out.result);
             #wp_out.webPage = #wp_in.webPage;

             // If HTML file does not exist in specific language folder,
             // then respecitive file from Template (TM) folder should
             // be loaded.
             if #wp_out_len = 0;
                #wp_in.webPage = %ScanRpl('/EN/':'/TM/':#wp_in.webPage);
                #wp_in.webPage = %ScanRpl('/FR/':'/TM/':#wp_in.webPage);
                #wp_in.webPage = %ScanRpl('/SP/':'/TM/':#wp_in.webPage);

                #wp_out.result = readFile(#wp_in.webPage);
                #wp_out_len = %len(#wp_out.result);
                #wp_out.webPage = #wp_in.webPage;
             endif;

             iter;
          endif;

          // Receive web message from the IFS file
          if (useWp and wpIsUp) or (useEp and epIsUp);
             qrcvdtaq(dtaqOutputName:
                      #WP_LIB:
                      #wp_out_len:
                      #wp_out:
                      #wp_wait:
                      #Wp_keyord:
                      %len(#wp_in.dqkey):
                      #wp_in.dqkey:
                      0:
                      ' ');
          endif;

          // If returned info length = 0, then SERVER_WP might not running
          // or the request that we sent was not sent back to us correctly.
          // so just go get the web page on your own

          if #wp_out_len = 0 or
             wpIsUp = FALSE or epIsUp = FALSE;

             if useWp;
                wpIsUp = FALSE;
             else;
                epIsUp = FALSE;
             endif;

             // something else is wrong- retreive the file directly
             #wp_out.result = readFile(#wp_in.webPage);
             #wp_out_len = %len(#wp_out.result);
             #wp_out.webPage = #wp_in.webPage;

             //write a record in cierrors1 that we retreived the file without WP
             errorText = 'MODWEB read file directly, not through WP. ' +
                         'File: ' + %trim(#wp_in.webPage);
             doError('LOG':errorText:'LE');
          endif;
       enddo;

       if #wp_out.errFlg = TRUE;
          @status.code = ERROR;
       else;
          @status.code = SUCCESS;
       endif;

       // Multi Language change to check if file is being picked from
       // Template folder.If yes,then execute procedure to clean up the
       // tags and keys from html file.
       if (@status.code = SUCCESS) and
          (%scan('/TM/': #wp_in.webPage) <> 0 or
           %scan('/tm/': #wp_in.webPage) <> 0) and
          (%scan('.HTML': #wp_in.webPage) <> 0 or
           %scan('.html': #wp_in.webPage) <> 0);
          #wp_out.result = ml_cleanHtml(%trim(#wp_out.result):@status);
       endif;

       if @status.code = ERROR or #wp_out_len = 0;
          clear #wp_out.result; // page is in error
       endif;

       return #wp_out.result;
      /copy rpgcopy,$systemerr

       end-proc;

       // --------------------------------------------------
       // web_replaceVariables: EXTERNAL
       //    Purpose: Parse a buffer for html variables and replace
       //             with the corresponding values
       //             Does not send the data in the buffer out at end
       //             see MODWEB doc for list of variable tags
       //    Parameters: htmlPage => a buffer containing the html to parse
       //                rootPath => the path to use when reading a file
       //                ptrGetVariable => procedure that specifies what to replace
       //                                  html tags with
       //                ptrProcessIf => procedure to handle ([IF]) tags
       //                ptrProcessLoop => procedure to handle ([LOOP]) tags
       //                ptrProcessSwitch => procedure to handle ([SWITCH]) tags
       //                ptrOvrSndBuffer => procedure to override how the buffer is sent out
       // --------------------------------------------------
       dcl-proc web_replaceVariables export;
          dcl-pi *n;
             htmlPage             like(#wp_out.result) const;
             rootPath             like(#wp_in.webPage) const;
             ptrGetVariable       pointer(*proc) const;
             ptrProcessIf         pointer(*proc) const;
             ptrProcessLoop       pointer(*proc) const;
             ptrProcessSwitch     pointer(*proc) const options(*nopass:*omit);
             ptrOvrSndBuffer      pointer(*proc) const options(*nopass);
          end-pi;

          dcl-pr  getVariable     like(htmlPage) extproc(ptrGetVariable);
            varType               like(HTML_VAR) const;
            varName               like(HTML_PARM) dim(HTML_PARM_DIM) const
                                  options(*nopass);
          end-pr;

          dcl-pr  processIf       IND extproc(ptrProcessIf);
            criteria              like(HTML_PARM) const;
          end-pr;

          dcl-pr  processLoop     extproc(ptrProcessLoop);
             template             like(smallBuffer) const;
             criteria             like(HTML_PARM) const;
          end-pr;

      * variables
     dstatus           ds                   likeds(statusds)
      *
     dendIdx           s                    like(idx) inz(1)
     dstartIdx         s                    like(idx) inz(1)
     dendIdx2          s                    like(idx) inz(1)
     dstartIdx2        s                    like(idx) inz(1)
     dworkBuffer       s                    like(fileBuffer)
     dhtmlVariable     s                    like(HTML_VAR) inz
     dparmList         s                    like(HTML_PARM)
     d                                      dim(HTML_PARM_DIM) inz
     dtoReplace        s            250a    varying
     dreplaceValue     s                    like(htmlPage)
     dselectMatch      s             10a    varying inz
     dsubParmList      s                    like(HTML_PARM)
     d                                      dim(HTML_PARM_DIM) inz
     dvalueString      s             50a    varying inz
     dtemplate         s                    like(smallBuffer)
      *
     dhasVariables     s               n    inz(TRUE)

          // Start Procressing
          workBuffer = htmlPage;

          dow hasVariables;
             startIdx = %scan(STARTTAG:workBuffer:startIdx);
             if startIdx > 0;
                endIdx = %scan(ENDTAG:workBuffer:startIdx);
                if endIdx > 0;
                   hasVariables = TRUE;
                else;
                   hasVariables = FALSE;
                   leave;
                endif;
             else;
                hasVariables = FALSE;
                leave;
             endif;

             htmlVariable = %subst(workBuffer:
                             startIdx + %len(STARTTAG):
                             endIdx - startIdx - %len(ENDTAG));
             toReplace = STARTTAG + htmlVariable + ENDTAG;

             parmList = getParameters(htmlVariable);
             htmlVariable = parmList(1);
             //shift list left one position
             %subarr(subParmList:1) = %subarr(parmList:2);
             %subarr(parmList:1) = %subarr(subParmList:1);

             endIdx += %len(ENDTAG);

             select;

             //conditional output variable
             when htmlVariable = 'IF';

               // check the CGI specific condition
               htmlVariable = 'VAR';
               if processIf(parmList(1));
                  replaceValue = getVariable(htmlVariable:parmList(2));
               else;
                  replaceValue = getVariable(htmlVariable:parmList(3));
               endif;

             //conditional section of html
             when htmlVariable = 'WHEN';
               startIdx2 = startIdx + %len(toReplace);

               if parmList(1) = 'NOT';
                  replaceValue = getContent(parmlist(2):workBuffer:startIdx2:
                                         endIdx2:status);
               else;
                  replaceValue = getContent(parmlist(1):workBuffer:startIdx2:
                                         endIdx2:status);
               endif;

               // this will put the tag out to the screen on a failure
               if status.code <> SUCCESS;
                  // move startIdx to be after the start tag so it stays in the buffer
                  startIdx = endIdx;
                  iter;
               endif;

               endIdx = endIdx2;

               if parmlist(1) = 'NOT';
                  if processIf(parmList(2));
            //SGTST         replaceValue = '';
                  endif;
               else;
                  if not processIf(parmList(1));
            //SGTST         replaceValue = '';
                  endif;
               endif;

             //switch case execution of HTML
             when htmlVariable = 'SWITCH';
                if %parms >= %parmnum(ptrProcessSwitch) and
                   %addr(ptrProcessSwitch) <> *null;

                   startIdx2 = startIdx + %len(toReplace);

                   replaceValue = getContent(parmlist(1):workBuffer:startIdx2:
                                         endIdx2:status);

                   // this will put the tag out to the screen on a failure
                   if status.code <> SUCCESS;
                      startIdx = endIdx;
                      iter;
                   endif;

                   endIdx = endIdx2;

                   exsr sendPrevious;
                   replaceValue = parseSwitch(parmlist(1):replaceValue:
                                              ptrProcessSwitch:status);

                   // this will put the tag out to the screen on a failure
                   if status.code <> SUCCESS;
                      startIdx = endIdx;
                      iter;
                   endif;
                else;
                   clear replaceValue;
                endif;

             // Selected Input Substitutions
             when htmlVariable = 'SELECTED';
               startIdx2 = startIdx + %len(toReplace);

               replaceValue = getContent(parmlist(1):workBuffer:startIdx2:
                                      endIdx2:status);

               if status.code <> SUCCESS;
                  startIdx = endIdx;
                  iter;
               endif;

               endIdx = endIdx2;

               // parameter 2 contains value to match on
               selectMatch = getVariable(htmlVariable:parmList);
               valueString = 'value="' + selectMatch + '"';

               replaceValue = %scanrpl(valueString:valueString + ' selected':
                                       replaceValue);

             // Checked Input Substitutions
             when htmlVariable = 'CHECKED';
               startIdx2 = startIdx + %len(toReplace);

               replaceValue = getContent(parmlist(1):workBuffer:startIdx2:
                                      endIdx2:status);

               if status.code <> SUCCESS;
                  startIdx = endIdx;
                  iter;
               endif;

               endIdx = endIdx2;

               // parameter 2 contains value to match on
               selectMatch = getVariable(htmlVariable:parmList);
               valueString = 'value="' + selectMatch + '"';

               replaceValue = %scanrpl(valueString:valueString + ' checked':
                                       replaceValue);

             // Loop Substitutions
             when htmlVariable = 'LOOP';
               startIdx2 = startIdx + %len(toReplace);
               template = getContent(parmlist(1):workBuffer:startIdx2:
                                     endIdx2:status);

               if status.code <> SUCCESS;
                  startIdx = endIdx;
                  iter;
               endif;

               endIdx = endIdx2;

               exsr sendPrevious;
//sg           processLoop(template:parmList(1));
               replaceValue = '';

             // panel substitutions, can also call a different output program
             when htmlVariable = 'PANEL' or htmlVariable = 'PGM';
                exsr sendPrevious;
                replaceValue = getVariable(htmlVariable:parmList);

             // Regular file substitution
             when htmlVariable = 'FILE';
                exsr sendPrevious;
                if parmList(1) <> *blanks;
                   if %parms() >= %parmNum(ptrOvrSndBuffer);
                      web_processFile(%trim(rootPath):
                                      %trim(parmList(1)):
                                      ptrGetVariable:
                                      ptrProcessIf:
                                      ptrProcessLoop:
                                      ptrProcessSwitch:
                                      ptrOvrSndBuffer);
                   else;
                      if %parms() >= %parmNum(ptrProcessSwitch);
                         web_processFile(%trim(rootPath):
                                         %trim(parmList(1)):
                                         ptrGetVariable:
                                         ptrProcessIf:
                                         ptrProcessLoop:
                                         ptrProcessSwitch);
                      else;
                         web_processFile(%trim(rootPath):
                                         %trim(parmList(1)):
                                         ptrGetVariable:
                                         ptrProcessIf:
                                         ptrProcessLoop);
                      endif;
                   endif;
                endif;

                clear replaceValue;

             // Regular variable substitution
             when (htmlVariable = 'VAR' or htmlVariable = 'MSG' or
                   htmlVariable = 'RESOURCE');
               if parmList(1) <> *blanks;
                  replaceValue = getVariable(htmlVariable:parmList);
               else;
                  replaceValue = '';
               endif;

             other;
                 startIdx = endIdx;
                 iter;
             endsl;

             workBuffer = %replace(replaceValue:workBuffer:startIdx:
                                     endIdx - startIdx);

             // build up local buffer with processed html
             exsr addPrevious;
          enddo;

          // first add any left over html data in work buffer
          if %len(workBuffer) > 0;
             if %parms() >= %parmNum(ptrOvrSndBuffer);
                web_addToBuffer(workBuffer:ptrOvrSndBuffer);
             else;
                web_addToBuffer(workBuffer);
             endif;
          endif;

          return;

          //********************************************************************
          // sendPrevious: html in workbuffer prior to loop tag must be added to
          //               buffer before processLoop, otherwise the page will be
          //               out of order.
          //********************************************************************
          begsr sendPrevious;

          if startIdx = 1;  //If we don't have anything to send, don't
             leavesr;
          endif;

          if %parms() >= %parmNum(ptrOvrSndBuffer);
             web_addToBuffer(%subst(workBuffer:1:startIdx-1):
                             ptrOvrSndBuffer);
             web_sendBuffer(*omit:ptrOvrSndBuffer);
          else;
             web_addToBuffer(%subst(workBuffer:1:startIdx-1));
             web_sendBuffer();
          endif;

          if %len(workBuffer) >= startIdx;
             workBuffer = %subst(workBuffer:startIdx);
             endIdx = endIdx - startIdx + 1;
          else;
             // create same effect as above, but at end of buffer
             clear workBuffer;
             endIdx = 1;
          endif;
          startIdx = 1;

          endsr;

          //********************************************************************
          // addPrevious: build local buffer with the replaced html
          //********************************************************************
          begsr addPrevious;

          if startIdx = 1;  // If we don't have anything to send, don't
             leavesr;
          endif;

          if %parms() >= %parmNum(ptrOvrSndBuffer);
             web_addToBuffer(%subst(workBuffer:1:startIdx-1):
                             ptrOvrSndBuffer);
          else;
             web_addToBuffer(%subst(workBuffer:1:startIdx-1));
          endif;

          if %len(workBuffer) >= startIdx;
             workBuffer = %subst(workBuffer:startIdx);
          else;
             // create same effect as above, but at end of buffer
             clear workBuffer;
          endif;
          startIdx = 1;

          endsr;

       end-proc;

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Helper Subprocedures
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

       // --------------------------------------------------
       // web_addToBuffer: EXTERNAL
       //    Purpose: add data to MODWEB's default buffer
       //             THIS DOES NOT CLEAR addBuffer!!!
       //    Parameters: addBuffer => buffer containing the data to add
       //                ptrOvrSndBuffer => procedure to override how the buffer is sent out
       // --------------------------------------------------
       dcl-proc web_addToBuffer export;
       dcl-pi *n;
          addBuffer             like(fileBuffer) const;
          ptrOvrSndBuffer       pointer(*proc) const options(*nopass);
       end-pi;

       // Check if remaining buffer can accommdate received tempBuffer value
       if (%size(buffer) - %len(buffer) - 2) >= %len(addBuffer);
          buffer += addBuffer;
       else;
          if %parms() >= %parmNum(ptrOvrSndBuffer);
             web_sendBuffer(*omit:ptrOvrSndBuffer);
          else;
             web_sendBuffer();
          endif;

          buffer += addBuffer;
       endif;

       // Check if buffer is reached maximum threshold value of bufferTrip
       if %len(buffer) >= bufferTrip;
          if %parms() >= %parmNum(ptrOvrSndBuffer);
             web_sendBuffer(*omit:ptrOvrSndBuffer);
          else;
             web_sendBuffer();
          endif;
       endif;

       return;

       end-proc;

       //*********************************************************************
       // Function: web_clean - clean input based on a white list already set
       //                       or passed. if you want a custom white list,
       //                       it can either be an add on to a constant in the
       //                       third parameter or standalone as the second
       //                       parameter
       //
       //   Base White List Checks
       //------------------------------------------
       //
       // CLEAN_ADDRESS = Address Line
       //      alpha + numbers + '''- .,&/\#'
       //
       // CLEAN_BASE = alpha + space + numbers   (default)
       //
       // CLEAN_EMAIL = alpha + numbers + '.-_@&'
       //
       // CLEAN_NAME = alpha + space + quote + dash + period_comma
       //
       // CLEAN_NUMBER = numbers + space + dash + decimal
       //
       // CLEAN_PHONE = numbers + '()+*-ext.EXT'
       //
       // CLEAN_TEXT = Text areas / free form text
       //      alpha + space + numbers + quote + dash + period_comma + special
       //*********************************************************************
       dcl-proc web_clean export;
       dcl-pi *n varchar(1000) rtnparm;
         inputString varchar(1000) const;
         haveCleanWhat varchar(50) options(*nopass) const;
         haveAdd2whiteList varchar(50) options(*nopass) const;
       end-pi;

       dcl-c ADDRESS '- .,;:&/\#@';
       dcl-c ALPHA 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
       dcl-c ALPHA_SPACE 'ABCDEFGHIJKLMNO+
                          PQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz';
       dcl-c DASH '-';
       dcl-c DECIMAL '.';
       dcl-c DOUBLE_DASH '--';
       dcl-c DOUBLE_QUOTE '''''';
       dcl-c EMAIL '.-_@&';
       dcl-c NUMBERS '0123456789';
       dcl-c PERIOD_COMMA '.,';
       dcl-c PHONE '()+*-ext.EXT';
       dcl-c QUOTE '''';
       dcl-c SPACE ' ';
       dcl-c SPECIAL '!@#$%^&*()/\";:?';      // Everything except <>

       dcl-s add2whiteList like(haveAdd2whiteList) inz;
       dcl-s cleanWhat like(haveCleanWhat) inz;
       dcl-s validChars varchar(102) inz;

       if inputString = *blanks;
          return '';
       endif;

       if %parms >= %parmnum(haveCleanWhat);
          cleanWhat = haveCleanWhat;
       endif;

       if %parms >= %parmnum(haveAdd2whiteList);
          add2whiteList = haveAdd2whiteList;
       endif;


       select;
       // Address field
       when cleanWhat = CLEAN_ADDRESS;
          validChars = ALPHA + NUMBERS + QUOTE + ADDRESS;

       // Base clean option
       when cleanWhat = CLEAN_BASE;
          validChars = ALPHA + SPACE + NUMBERS;

       // Email field
       when cleanWhat = CLEAN_EMAIL;
          validChars = ALPHA + NUMBERS + EMAIL;

       // Name field
       when cleanWhat = CLEAN_NAME;
          validChars = ALPHA + SPACE + QUOTE + DASH + PERIOD_COMMA;

       // Numeric field
       when cleanWhat = CLEAN_NUMBER;
          validChars = NUMBERS + SPACE + DASH + DECIMAL;

       // Phone Number Field
       when cleanWhat = CLEAN_PHONE;
          validChars = NUMBERS + PHONE;

       // Text area field
       when cleanWhat = CLEAN_TEXT;
          validChars = ALPHA + SPACE + NUMBERS + QUOTE + DASH + PERIOD_COMMA +
                       SPECIAL;

       // RMV: Character - alpha only
       when cleanWhat = CLEAN_ALPHA;
          validChars = ALPHA;

       // RMV: Character - alpha only
       when cleanWhat = CLEAN_ALPHA_S;
          validChars = ALPHA_SPACE;

       // RMV: Alphanumeric no space
       when cleanWhat = CLEAN_ALPHNUM;
          validChars = ALPHA + NUMBERS;

       // RMV: Alphanumeric with space
       when cleanWhat = CLEAN_ALPHNUM_S;
          validChars = ALPHA + SPACE + NUMBERS;

       // RMV: Integer - positive and negative
       when cleanWhat = CLEAN_INT_NEG;
          validChars = NUMBERS + DASH;

       // RMV: Integer - positive only (default)
       when cleanWhat = CLEAN_INT_POS;
          validChars = NUMBERS;

       // RMV: full name field
       when cleanWhat = CLEAN_NAME_FULL;
          validChars = ALPHA + SPACE + QUOTE + DASH + PERIOD_COMMA;

       // RMV: single name field
       when cleanWhat = CLEAN_NAME_SINGLE;
          validChars = ALPHA + QUOTE + DASH;

       // was passed a string in the second parm
       when cleanWhat <> *blanks;
          validChars = cleanWhat;

       other;    // Default
          validChars = ALPHA + SPACE + NUMBERS;

       endsl;

       validChars += add2whiteList;
       if validChars = *blanks;
          validChars = x'00';
       endif;

       if %check(validChars:%trim(inputString)) = 0;
          if %scan(DOUBLE_QUOTE:inputString) = 0 and
             %scan(DOUBLE_DASH:inputString) = 0;
             return inputString;
          endif;
       endif;

       return web_sanitize(%scanrpl('--':'':inputString));

       end-proc;

       //***********************************************************************
       //  web_getFileExtension:    EXTERNAL
       //     Purpose: locate and return the extension for a file upload
       //     Parameters:  filename => name of the file
       //***********************************************************************
       dcl-proc web_getFileExtension export;
       dcl-pi *n like(fileExtension_t);
          $filename char(512) const;
       end-pi;

       dcl-s dotIdx like(idx) inz;
       dcl-s fileExtension like(fileExtension_t) inz;

       dotIdx = %scan('.':$filename);
       dow dotIdx > 0;
          fileExtension = %subst($filename:dotIdx:6);
          dotIdx = %scan('.':$filename:dotIdx+1);
       enddo;

       return %xlate(xuc:xlc:fileExtension);

       end-proc;

       // --------------------------------------------------
       // web_getMessage: EXTERNAL   - AVOID USING
       //    Purpose: retrieve a message text only
       //    Returns: the retrieved message text
       //    Parameters: prefix => the prefix for the message numbers in the list
       //                number => the message number to retrieve
       //                applType => used to set msg_global
       //                msgFile => the file that the messages are contained in
       // --------------------------------------------------
       dcl-proc web_getMessage export;
       dcl-pi *n varchar(1024);
          prefix     char(3) const;
          number     char(4) const;
          applType   char(2) const;
          msgFile    like(msgFile_t) const options(*nopass);
       end-pi;

      /copy rpgcopy,messageds

     dreturnText       s           1024a   varying inz

     dmsg              ds                  likeds(msgDs_t) inz

       if %parms() >= %parmNum(msgFile);
          clear msg_global;
          msg_file = msgFile;
       else;
          msg_global = applType;
       endif;

       clear msg;
       msg_id = prefix + number;

       select;
       when msg_global = 'GA';
          msg_file = 'GAMSGS_ENG*LIBL   ';
       when msg_global = 'TP';
          msg_file = 'TPMESSAGES*LIBL   ';
       when msg_global = 'CS';
          msg_file = 'CSMESSAGES*LIBL   ';
       when msg_global = 'RS';
          msg_file = 'DVMESSAGES*LIBL   ';
       when msg_global = 'D2';
          msg_file = 'D2MESSAGES*LIBL   ';
       endsl;

       msg = web_zGetMessage(msg_id:msg_file);

       returnText = msg.text;

       return returnText;

       end-proc;

       // --------------------------------------------------
       // web_getMessagesList: EXTERNAL
       //    Purpose: retrieve a list of messages
       //    Returns: a buffer containing the text contained in the messages
       //    Parameters: prefix => the prefix for the message numbers in the list
       //                numberToWrite => the number of messages to retrieve
       //                messageList => a list of the messages to write w/o prefixes
       //                applType => used to set msg_global
       //                msgFile => the file that the messages are contained in
       // --------------------------------------------------
       dcl-proc web_getMessagesList export;
       dcl-pi *n like(smallBuffer) rtnparm;
          prefix        char(3) const;
          numberToWrite packed(3:0) const;
          messageList   char(100) dim(40) const;
          applType      char(2) const;
          msgFile       like(msgFile_t) const options(*nopass);
       end-pi;

      /copy rpgcopy,messageds
     dmsgidx           s                   like(idx)
     dreturnText       s                   like(smallBuffer)

     derrline          ds            90
     d errtext                 1     90
     d err_indic               1      1
     d err_msg                 2      5
     d err_vf                  6     11

       if numberToWrite > 0;
          for msgidx = 1 to numberToWrite;

            errtext = messagelist(msgidx);

            // if error text contains an error message number only or an
            // common error message
            if err_indic = '#' or err_indic = '@';
               if %parms() >= %parmNum(msgFile);
                  returnText += web_getMessage(prefix:err_msg:applType:msgFile);
               else;
                  returnText += web_getMessage(prefix:err_msg:applType);
               endif;
            elseif %len(%trim(errtext)) = 4;
               if %parms() >= %parmNum(msgFile);
                  returnText += web_getMessage(prefix:errText:applType:msgFile);
               else;
                  returnText += web_getMessage(prefix:errText:applType);
               endif;
            else;
               // error message contains full text of error message
               returnText += %trim(errline);
            endif;

          endfor;
       endif;

       return returnText;

       end-proc;


       //**********************************************************************
       //  web_getUploadBuffers EXTERNAL
       //     Purpose: calculate the number of buffers for a file upload
       //     Parameters: contentLn => from zcgihskpg, length of all buffers
       //                 maxdataln => max data length allowed for each buffer
       //**********************************************************************
       dcl-proc web_getUploadBuffers export;
       dcl-pi *n packed(5);
          $contentLength like(contentLn) const;
          $maxDataLength like(contentLn) const;
       end-pi;

       dcl-s numBuffers packed(5);

       numBuffers = ($contentLength/$maxDataLength);

       if ($maxDataLength * numBuffers) < $contentLength;
          numBuffers += 1;
       endif;

       return numBuffers;

       end-proc;


       //***********************************************************************
       // Function: web_identifyBrowser - get users browser information
       //***********************************************************************
       dcl-proc web_identifyBrowser export;
       dcl-pi web_identifyBrowser likeds(browserInfo);
         userAgent char(300) const;
       end-pi;

       dcl-s browser like(browserInfo.browser);
       dcl-s compMode char(20) inz;
       dcl-s device_os like(browserInfo.deviceOS);
       dcl-s devc_os_v like(browserInfo.deviceOS_ver);
       dcl-s len packed(5:0);
       dcl-s mobile char(10) inz;
       dcl-s pos packed(3:0);
       dcl-s pos2 packed(3:0);

       // Start Calc Specs

       // Attempt to identify the device_os
       select;
       when %scan('(MACINTOSH':userAgent) > 0;
          device_os = 'MAC';
       when %scan('(IPAD':userAgent) > 0;
          device_os = 'IPAD';
       when %scan('(IPOD':userAgent) > 0;
          device_os = 'IPOD';
       when %scan('(IPHONE':userAgent) > 0;
          device_os = 'IPHONE';
       when %scan('(LINUX; U; ANDROID':userAgent) > 0 or
            %scan('(LINUX; ANDROID':userAgent) > 0 or
            %scan('(ANDROID':userAgent) > 0;
          device_os = 'ANDROID';
       when %scan('(X11':userAgent) > 0;
          device_os = 'X11';
       when %scan('BLACKBERRY9300':userAgent) > 0;
          device_os = 'BB9300';
       when %scan('BLACKBERRY9700':userAgent) > 0;
          device_os = 'BB9700';
       when %scan('BLACKBERRY 9650':userAgent) > 0;
          device_os = 'BB9650-SAF';
       when %scan('BLACKBERRY 9780':userAgent) > 0;
          device_os = 'BB9780-SAF';
       when %scan('BLACKBERRY':userAgent) > 0;
          device_os = 'BB';
       when %scan('WINDOWS PHONE':userAgent) > 0;
          device_os = 'WINPHN';
       when %scan('WINDOWS NT ':userAgent) > 0;
          device_os = 'WIN';
       other;
          device_os = 'UNKNOWN';
       endsl;

       // Attempt to locate the browser
       select;
       when %scan('GOOGLEBOT':userAgent) > 0;
          browser = 'GOOGLEBOT';
       when %scan('GOOGLE WEB PREVIEW':userAgent) > 0;
          browser = 'GOOGLEWEBPREVIEW-BOT';
       when %scan('YAHOO LINK PREVIEW':userAgent) > 0;
          browser = 'YAHOOLINKPREVIEW-BOT';
       when %scan('TRIDENT/7.0':userAgent) > 0 or
            %scan('IEMOBILE/11.0':userAgent) > 0;
          browser = 'IE11';
       when %scan('MSIE 10.0':userAgent) > 0;
          browser = 'IE10';
       when %scan('MSIE 9.0':userAgent) > 0;
          browser = 'IE9';
       when %scan('MSIE 8.0':userAgent) > 0;
          browser = 'IE8';
       when %scan('MSIE 7.0':userAgent) > 0;
          browser = 'IE7';
       when %scan('MSIE 6.0':userAgent) > 0;
          browser = 'IE6';
       when %scan('EDGE/':userAgent) > 0 and
            %scan('CHROME/':userAgent) > 0 and
            %scan('SAFARI/':userAgent) > 0;
          browser = 'MS EDGE';
       when %scan('OPERA':userAgent) > 0 or
            %scan(' OPR/':userAgent) > 0;
          browser = 'OPERA';
       when %scan('FIREFOX':userAgent) > 0;
          browser = 'FIREFX';
       when %scan('AOLAPP/':userAgent) > 0;
          browser = 'AOL';
       when %scan('CHROMIUM':userAgent) > 0;
          browser = 'CHROMIUM';
       when %scan('CHROME':userAgent) > 0 or
            %scan('CRIOS/':userAgent) > 0;
          browser = 'CHROME';
       when %scan('SAFARI':userAgent) > 0 and
            device_os <> 'ANDROID';
          browser = 'SAFARI';
       when device_os = 'UNKNOWN';
          browser = %subst(userAgent:13);
          browser = SanitizeBrowser(browser);
       when device_os <> *blanks;
          browser = 'UNKNOWN';
       endsl;

       // See if IE is in compatibility mode
       select;
       when %scan('COMPATIBLE; MSIE 10.0;':userAgent) > 0;
          compMode = ' COMP(IE 10)';
       when %scan('COMPATIBLE; MSIE 9.0;':userAgent) > 0;
          compMode = ' COMP(IE 9)';
       when %scan('COMPATIBLE; MSIE 8.0;':userAgent) > 0;
          compMode = ' COMP(IE 8)';
       when %scan('COMPATIBLE; MSIE 7.0;':userAgent) > 0;
          compMode = ' COMP(IE 7)';
       when %scan('COMPATIBLE; MSIE 5.0;':userAgent) > 0;
          compMode = ' COMP(MSIE 5)';
       endsl;

       // Attempt to locate the OS version
       select;
       when device_os = 'MAC';
          pos = %scan('MAC OS X':userAgent);
          if pos > 0;
             pos += 9;
             pos2 = %check('0123456789_. ':userAgent:pos);
             if pos2 > 0;
                len = pos2 - pos;
                devc_os_v = %trim(%subst(userAgent:pos:len));
             else;
                pos2 = %scan(')':userAgent:pos);
                if pos2 > 0;
                   len = pos2 - pos;
                   devc_os_v = %trim(%subst(userAgent:pos:len));
                else;
                   pos2 = %scan(';':userAgent:pos);
                   if pos2 > 0;
                      len = pos2 - pos;
                      devc_os_v = %trim(%subst(userAgent:pos:len));
                   endif;
                endif;
             endif;
          endif;
          devc_os_v = %xlate('_E':'. ':devc_os_v);

       when device_os = 'WIN';
          select;
          when %scan('NT 10.0':userAgent) > 0;
             devc_os_v = '10';
          when %scan('NT 6.3':userAgent) > 0 or
               %scan('WINDOWS PHONE 8.1':userAgent) > 0;
             devc_os_v = '8.1';
          when %scan('NT 6.2':userAgent) > 0 or
               %scan('WINDOWS PHONE 8':userAgent) > 0;
             devc_os_v = '8';
          when %scan('NT 6.1':userAgent) > 0;
             devc_os_v = '7';
          when %scan('NT 6.0':userAgent) > 0;
             devc_os_v = 'Vista';
          when %scan('NT 5.1':userAgent) > 0 or
               %scan('NT 5.2':userAgent) > 0;
             devc_os_v = 'XP';
          when %scan('NT 5.':userAgent) > 0;
             devc_os_v = '2K';
          endsl;
          if device_os = 'WINPHN';
             mobile = ' MOBILE';
          endif;

       when device_os = 'X11';
          select;
          when %scan('I686 ON X86_64':userAgent) > 0;
             devc_os_v = 'I686-X86_64';
          when %scan('X86_64':userAgent) > 0;
             devc_os_v = 'X86_64';
          when %scan('I686':userAgent) > 0;
             devc_os_v = 'I686';
          endsl;

          select;
          when %scan('X11; CROS':userAgent) > 0;
             device_os = 'X11-CHROME';
          when %scan('UBUNTU':userAgent) > 0;
             device_os = 'X11-UBUNTU';
          endsl;

       when device_os = 'IPAD';
          pos = %scan('IPAD; CPU':userAgent);
          if pos > 0;
             pos += 10;
             pos2 = %scan(' LIKE':userAgent:pos);
             if pos2 > 0;
                len = pos2 - pos;
                devc_os_v = %trim(%subst(userAgent:pos:len));
                devc_os_v = %xlate('_':'.':devc_os_v);
             endif;
          endif;
          mobile = ' MOBILE';

       when device_os = 'IPOD';
          pos = %scan('CPU IPOD':userAgent);
          if pos > 0;
             pos += 11;
             pos2 = %scan(' LIKE':userAgent:pos);
             if pos2 > 0;
                len = pos2 - pos;
                devc_os_v = %trim(%subst(userAgent:pos:len));
                devc_os_v = %xlate('_':'.':devc_os_v);
             endif;
          endif;
          mobile = ' MOBILE';

       when device_os = 'IPHONE';
          pos = %scan('CPU IPHONE':userAgent);
          if pos > 0;
             pos += 11;
             pos2 = %scan(' LIKE':userAgent:pos);
             if pos2 > 0;
                len = pos2 - pos;
                devc_os_v = %trim(%subst(userAgent:pos:len));
                devc_os_v = %xlate('_':'.':devc_os_v);
             endif;
          endif;
          mobile = ' MOBILE';

       when device_os = 'ANDROID';
          pos = %scan('(ANDROID':userAgent);
          if pos > 0;
             pos += 10;
             pos2 = %scan(';':userAgent:pos);
             if pos2 > 0;
                len = pos2 - pos;
                devc_os_v = %trim(%subst(userAgent:pos:len));
             endif;
          else;
             pos = %scan('ANDROID':userAgent);
             if pos > 0;
                pos += 8;
                pos2 = %scan(';':userAgent:pos);
                if pos2 > 0;
                   len = pos2 - pos;
                   devc_os_v = %trim(%subst(userAgent:pos:len));
                endif;
             endif;
          endif;
          mobile = ' MOBILE';

       when device_os = 'UNKNOWN';

          devc_os_v = '(SPOOF?)';

       endsl;
       devc_os_v = %scanrpl('RV:':'':devc_os_v);
       devc_os_v = %xlate(';':' ':devc_os_v);

       browserInfo.browser = %trim(browser)+%trimr(compMode)+%trimr(mobile);
       browserInfo.deviceOS = device_os;
       browserInfo.deviceOS_ver = devc_os_v;

       return browserInfo;

       end-proc;

       //*********************************************************************
       // web_objectExists - check to see if the test object exists
       //*********************************************************************
       dcl-proc web_objectExists export;
          dcl-pi web_objectExists ind;
             $member  char(10) const;
             $library char(10) const options(*nopass);
          end-pi;

          dcl-s library char(10);

          if %parms >= %parmNum($library);
             library = $library;
          else;
             library = '*LIBL';
          endif;

          sysCmd = 'CHKOBJ OBJ(' + %trim(library) + '/' + %trim($member) +
                   ') OBJTYPE(*PGM)';

          if system(sysCmd) <> 0;
             return FALSE;
          endif;

          return TRUE;

       end-proc;

       //***********************************************************************
       // web_sanitize: convert HTML special chars to encoded chars
       //***********************************************************************
       dcl-proc web_sanitize export;
       dcl-pi *n varchar(1000) rtnparm;
          theHTML varchar(1000) value;
       end-pi;

       theHTML = %scanrpl('&':'&amp;':theHTML);
       theHTML = %scanrpl('''':'&#x27;':theHTML);
       theHTML = %scanrpl('>':'&gt;':theHTML);
       theHTML = %scanrpl('<':'&lt;':theHTML);
       theHTML = %scanrpl('"':'&quot;':theHTML);
       theHTML = %scanrpl('/':'&#x2F;':theHTML);

       return theHTML;

       end-proc;

       // --------------------------------------------------
       // web_sendBuffer: EXTERNAL
       //    Purpose: send a buffer out to a web page
       //    Parameters: bufferDataIn => buffer containing the data to send out
       //                ptrOvrSndBuffer => procedure to override how the buffer is sent out
       //    To send the default buffer that is built in MODWEB,
       //       pass no parameters or pass *omit
       // --------------------------------------------------
       dcl-proc web_sendBuffer export;
       dcl-pi *n;
          bufferDataIn         like(fileBuffer) const options(*omit:*nopass);
          ptrOvrSndBuffer      pointer(*proc) const options(*nopass);
       end-pi;

       dcl-pr ovrSendBuffer    extproc(ptrOvrSndBuffer);
          ovrBufrData          like(fileBuffer) const;
       end-pr;

     dbufferDataOut    s                   like(fileBuffer)

      /copy prototypes,APIStdOUT

       // set which buffer's data to use
       if %parms() >= %parmNum(bufferDataIn);
          if %addr(bufferDataIn) = *null;
             bufferDataOut = buffer;
             clear buffer;
          else;
             bufferDataOut = bufferDataIn;
          endif;
       else;
          bufferDataOut = buffer;
          clear buffer;
       endif;

       // send buffer
       if %len(bufferDataOut) > 0;
          if %parms() >= %parmNum(ptrOvrSndBuffer);
             ovrSendBuffer(bufferDataOut);
          else;
             APIStdOut(bufferDataOut:%len(bufferDataOut):QUSEC);
          endif;
       endif;

       return;

       end-proc;

       //********************************************************************
       // web_setPanelFile - Standardize setting of Panel Files
       //
       //    Main Purpose: use to set the extension with ([PANEL,SCRIPT])
       //                  and/or ([PANEL,STYLE]) and see if it exists
       //
       //    Secondary Purpose: Set a testing path text prior to the extension
       //                      i.e. DVCGITP uses inf_studentInfo_test.html to
       //                           allow for a compile to production without
       //                           pushing test wf files live
       //
       //    Parameters:
       //      REQUIRED
       //         $rootFilePath => root WF path used by CGI program
       //         $panelFile => panel file set by CGI program
       //         $extension => extension
       //      OPTIONAL (NO PASS) - parms can only be used if test
       //         $isTest => self explanatory boolean
       //         $addToFileName => text to be added to the file name for
       //                           testing purposes
       //
       // NOTE: .html or no extension are only allowed extension types
       //       for panelFile
       //********************************************************************
       dcl-proc web_setPanelFile export;
       dcl-pi *n like(#wp_in.webPage) rtnparm;
          $rootFilePath like(#wp_in.webPage) const;
          $panelFile like(#wp_in.webPage) const;
          $extension varchar(4) const;
          $isTest ind const options(*nopass);
          $addToFileName varchar(15) const options(*nopass);
       end-pi;

       dcl-c HTML '.html';

       dcl-s addToFileName like($addToFileName) inz;
       dcl-s existFile char(512);
       dcl-s fileExtension like(fileExtension_t) inz;
       dcl-s idx packed(3) inz;
       dcl-s isTest ind inz(FALSE);

       dcl-s @panelFile like(#wp_in.webPage) inz;

       // .html or no extension are only allowed for panelFile
       fileExtension = web_getFileExtension($panelFile);
       if fileExtension <> *blanks and fileExtension <> HTML;
          return '';
       endif;

       if %parms >= %parmnum($isTest);
          isTest = $isTest;
          if %parms >= %parmnum($addToFileName);
             addToFileName = $addToFileName;
          endif;
       endif;

       select;
       when $extension <> HTML and $extension <> *blanks;
          @panelFile = %scanrpl(HTML:$extension:$panelFile);

       when isTest and addToFileName <> *blanks;
          idx = %scan(HTML:$panelFile);
          if idx = 0;
             @panelFile = %trim($panelFile) + addToFileName + HTML;
          else;
             @panelFile = %trim(%subst($panelFile:1:idx-1)) + addToFileName +
                          %trim(%subst($panelFile:idx));
          endif;

       other;
          if %scan(HTML:$panelFile) = 0;
             @panelFile = %trim($panelFile) + HTML;
          else;
             @panelFile = %trim($panelFile);
          endif;

       endsl;

       existFile = %trim($rootFilePath) + %trim(@panelFile);
       if not existsIFS(existFile);
          if isTest and addToFileName <> *blanks;
             @panelFile =  %scanrpl(addToFileName:'':@panelFile);
             existFile = %trim($rootFilePath) + %trim(@panelFile);
             if not existsIFS(existFile);
                clear @panelFile;
             endif;
          else;
             clear @panelFile;
          endif;
       endif;

       return @panelFile;

       end-proc;

       //********************************************************************
       // web_setRootPath
       //    Purpose: Standardize the way our root file path is set
       //    Parameters:
       //      REQUIRED
       //         $programName => name of the program from PSDS
       //         $prodName => program's prod name, constant in CGI program
       //         $staging => staging indicator set in CGI program
       //      OPTIONAL (NO PASS/OMIT)
       //         $language => 2 character language folder
       //      OPTIONAL (NO PASS)
       //         $rootProgram => root program name
       //********************************************************************
       dcl-proc web_setRootPath export;
       dcl-pi *n like(#wp_in.webPage) rtnParm;
          $programName varchar(10) const;
          $prodName varchar(10) const;
          $staging ind const;
          $language varchar(2) const options(*nopass:*omit);
          $rootProgram varchar(10) const options(*nopass);
       end-pi;

       dcl-c WF_FILEPATH '/wf/';
       dcl-c DIR_PROD 'p/' ;
       dcl-c DIR_STAGE 's/';
       dcl-c DIR_TEST 't/';
       dcl-c SLASH '/';

       dcl-s language varchar(3);
       dcl-s directory varchar(2);
       dcl-s rootProgram varchar(10);
       dcl-s @rootFilePath like(#wp_in.webPage);


       clear language;
       if %parms >= %parmnum($language);
          if %addr($language) <> *null;
             if $language <> *blanks;
                language = $language + SLASH;
             endif;
          endif;
       endif;

       clear rootProgram;
       if %parms >= %parmnum($rootProgram);
          if $rootProgram <> *blanks;
             rootProgram = $rootProgram;
          endif;
       endif;


       select;
       when $staging;
         directory = DIR_STAGE;
       when $programName = $prodName;
         directory = DIR_PROD;
       other;
         directory = DIR_TEST;
       endsl;

       if rootProgram <> *blanks;
          @rootFilePath = WF_FILEPATH + directory + rootProgram + SLASH +
                          $prodName + SLASH + language;
       else;

          @rootFilePath = WF_FILEPATH + directory + $prodName + SLASH +
                          language;

       endif;

       return @rootFilePath;

       end-proc;

       //********************************************************************
       // Function: web_urlDecode
       //********************************************************************
       dcl-proc web_urlDecode export;
       dcl-pi *n like(urlDecodeParm) rtnParm;
          decodeField like(urlDecodeParm) const;
       end-pi;

       dcl-ds *n;
          parmValue like(urlDecodeParm);
          parmChar char(1) pos(1) dim(%len(parmValue));
       end-ds;
       dcl-s pi int(5);

       parmValue = decodeField;
       // now table translate all % values
       pi = %scan('%':parmValue:1);
       dow pi > 0;
          if %tlookup(%subst(parmValue:pi:3):tabpct:tabchr) = *on;
             parmChar(pi) = tabchr;
          endif;
          parmValue = %subst(parmValue:1:pi) + %subst(parmValue:pi+3);
          pi = %scan('%':parmValue:pi+1);
       enddo;
       return parmValue;

       end-proc;

       //********************************************************************
       // Function: web_urlDecodeUtf8
       //********************************************************************
       dcl-proc web_urlDecodeUtf8 export;
       dcl-pi *n like(urlDecodeParm) rtnParm;
          decodeField like(urlDecodeParm) const;
       end-pi;

       dcl-ds *n;
          parmValue like(urlDecodeParm);
          parmChar char(1) pos(1) dim(%len(parmValue));
       end-ds;
       dcl-s pi int(5);
       dcl-s plen zoned(2:0);

       parmValue = decodeField;
       // now table translate all % values
       pi = %scan('%':parmValue:1);
       dow pi > 0;
          if %tlookup(%subst(parmValue:pi:6):tabUTF:tabchrUTF) = *on;
             parmChar(pi) = tabchrUTF;
             plen = 6;
          elseif %tlookup(%subst(parmValue:pi:3):tabpct:tabchr) = *on;
             parmChar(pi) = tabchr;
             plen = 3;
          endif;
          parmValue = %subst(parmValue:1:pi) + %subst(parmValue:pi+plen);
          pi = %scan('%':parmValue:pi+1);
       enddo;
       return parmValue;

       end-proc;

       //***********************************************************************
       // Function: web_validProtocol - Check to see the web protocols are OK
       //       webProtocol  - String from zcghskpg or robo instit field
       //       webProtoType - Allowed protocol name/type (TLS, etc.)
       //       webProtoVrsn - Allowed protocol version associated type
       //***********************************************************************
       dcl-proc web_validProtocol export;
       dcl-pi *n ind;
          webProtocol char(20) const;
          webProtoType char(10) const;
          webProtoVrsn packed(5:3) const;
       end-pi;

       dcl-s curVersion char(10);
       dcl-s idx packed(3:0);
       dcl-s numeric char(11) inz('0123456789.');
       dcl-s wrkType char(10);

       wrkType = webProtoType;
       if wrkType = *blanks;
          wrkType = '!@#$';
       endif;

       idx = %scan(%trim(wrkType):webProtocol);
       if idx > 0;
          idx += %len(%trim(wrkType));
          curVersion = ' ' + %subst(webProtocol:idx:1) + '.' +
                       %subst(webProtocol:idx+1);

          curVersion = %xlate(' ':'0':curVersion);

          if %check(numeric:curVersion) = 0 and
             %dec(curVersion:5:3) >= webProtoVrsn;
             return TRUE;
          endif;

       endif;

       return FALSE;

       end-proc;

       //***********************************************************************
       // Function: web_zGetMessage - retrieve a message
       //    Purpose: retrieve a message from a message file - RECOMMENDED!!!
       //    Parameters:
       //     Required:  msgId ds => contains prefix and msg number
       //                msgFile => qualified name of the message file
       //
       //     Optional:  msgOvrPrefix => school override prefix to try first
       //                msgReplace => replacement data in the message, &1,etc.
       //                msgReplaceLen => length of replacement data
       //***********************************************************************
       dcl-proc web_zGetMessage export;
       dcl-pi web_zGetMessage likeds(msgDs_t) rtnparm;
         $msgId likeds(msgId_t) const;
         $msgFile like(msgFile_t) const;
         $msgOvrPrefix like(msgOvrPrefix_t) options(*nopass:*omit) const;
         $msgReplace like(msgReplaceData_t) options(*nopass) const;
         $msgReplaceLen like(msgReplaceLen_t) options(*nopass) const;
       end-pi;

       // Prototype for QMHRTVM API
       dcl-pr qmhrtvm extpgm('QMHRTVM');
          msgInfo char(3000) options(*varsize);
          msgInfoLen int(10) const;
          formatName char(8) const;
          msgId char(7) const;
          msgF char(20) const;
          replacement char(512) options(*varsize);
          replacementLen int(10) const;
          replaceSubVals char(10) const;
          returnFCC char(10) const;
          usec char(216) options(*varsize);
          retrOpt char(10) const;
          ccsidCnvtTo int(10) const;
          ccsidRplcDta int(10) const;
       end-pr;

       // Parm 1 - msgInfo
     dQMHRTVMDS        ds
     d QMHBR                   1      4i 0
     d QMHBAVL                 5      8i 0
     d QMHMSEV                 9     12i 0
     d QMHALTIDX              13     16i 0
     d QMHALTO                17     25
     d QMHLOGI                26     26
     d QMHMSGID               27     33
     d QMHRESRVD              34     36
     d QMHNUMVARS             37     40i 0
     d QMHSIDCSI              41     44i 0
     d QMHSIDCSIR             45     48i 0
     d QMHSIDCCST             49     52i 0
     d QMHORT                 53     56i 0
     d QMHOFFDRPY             57     60i 0
     d QMHLENDRPY             61     64i 0
     d QMHOFFMSG              65     68i 0
     d QMHLENMSG              69     72i 0
     d QMHLENMSGA             73     76i 0
     d QMHOFFMSGH             77     80i 0
     d QMHLENMSGH             81     84i 0
     d QMHLENMSHA             85     88i 0
     d QMHOFFSUB              89     92i 0
     d QMHLENSUBR             93     96i 0
     d QMHLENSUBA             97    100i 0
     d QMHLENSUBE            101    104i 0
     dmsgData                105   3004a

       // parm 2 - msgInfoLen
       dcl-s msgLength int(10) inz(1024);

       // parm 3 - formatName
       dcl-s msgFormat char(8) inz('RTVM0300');

       // parm 4 - msgId
       dcl-ds msgId likeds(msgId_t);

       // parm 5 - msgF
       dcl-s msgFile like(msgFile_t) inz;

       // parm 6 - replacement
       dcl-s msgRepData like(msgReplaceData_t) inz;

       // parm 7 - replacementLen
       dcl-s msgRepLen like(msgReplaceLen_t) inz(0);

       // parm 8 - replacementSubVals
       dcl-s msgRepParm  char(10) inz('*NO       ');

       // parm 9 - returnFCC
       dcl-s msgRetrnF char(10) inz('*NO       ');

       // parm 10 - usec (error)
     dmsgErr           ds
     d msgErrBP                1      4i 0
     d msgErrBA                5      8i 0
     d msgErrEID               9     15a
     d msgErrRsv              16     16a
     d msgErrDta              17    216a

       // parm 11 - retrOpt ( Optional )
       dcl-s msgRetr char(10);

       // parm 12 - ccsidCnvtTo ( Optional )
       dcl-s msgCCSIDcvtTo int(10) inz(65535);

       // parm 13 - ccsidRplcDta ( Optional )
       dcl-s msgCCSIDrplcDta int(10) inz(65535);

       dcl-c LF x'15';

       dcl-s attempts zoned(1) inz;
       dcl-s defaultPrefix like(msgId_t.prefix) inz;
       dcl-s numAttempts zoned(1) inz(1);

       dcl-ds rtrnMsg likeds(msgDs_t) inz;

       msgFile = $msgFile;
       msgId = $msgId;

       if %parms() >= %parmnum($msgOvrPrefix);
          if %addr($msgOvrPrefix) <> *null;
             if $msgOvrPrefix <> *blanks;
                defaultPrefix = msgId.Prefix;
                msgId.prefix = $msgOvrPrefix;
                numAttempts = 2;
             endif;
          endif;
       endif;

       if %parms() >= %parmnum($msgReplace);
          msgRepData = $msgReplace;
       endif;

       if %parms() >= %parmnum($msgReplaceLen);
          msgRepLen = $msgReplaceLen;
       endif;

       clear QMHRTVMDS;
       clear msgerr;

       msgerrbp = 216;
       msgRetr = '*MSGID';
       msgRetrnF = '*NO';
       msgRepParm = '*YES';

       rtrnMsg.found = FALSE;
       rtrnMsg.haveText = FALSE;

       for attempts = 1 to numAttempts;

          callp qmhrtvm(QMHRTVMDS:msgLength:msgFormat:msgId.id:msgFile:
                        msgRepData:msgRepLen:msgRepParm:msgRetrnF:msgErr:
                        msgRetr:msgCCSIDcvtTo:msgCCSIDrplcDta);
          select;
          when QMHBAVL > 0;
             rtrnMsg.level1 = %subst(QMHRTVMDS:QMHOFFMSG+1:QMHLENMSGA);
             rtrnMsg.text = %subst(QMHRTVMDS:QMHOFFMSGH+1:QMHLENMSHA);
             rtrnMsg.found = TRUE;
             if QMHLENMSHA > 0;
                rtrnMsg.haveText = TRUE;
                rtrnMsg.text = %xlate('¦':LF:rtrnMsg.text);
                rtrnMsg.text = %xlate('`':'''':rtrnMsg.text);
             else;
                clear rtrnMsg.text;
             endif;
             leave;

          when attempts < numAttempts;
             if defaultPrefix <> *blanks;
                msgId.prefix = defaultPrefix;
             endif;

          endsl;
       endfor;

       rtrnMsg.id = msgId.id;

       return rtrnMsg;

       end-proc;

      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      * Internal Subprocedures
      * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

       //********************************************************************
       // INTERNAL
       // doError: send admin e-mail when an error occurs
       //********************************************************************
       dcl-proc doError;
       dcl-pi *n;
          distro             char(3) const;
          errText            varchar(800) const;
          function           char(2) options(*nopass) const;
       end-pi;

       /copy rpgcopy,$srvrem_ds

       if %parms() >= %parmNum(function);
          #em_func = function;
       else;
          #em_func = 'EM';
       endif;

       #em_dist = distro;
       #em_subjct = %trim(pgm_name) + ' Error';
       #em_short  = *blanks;
       #em_long = %trim(pgm_name) + ':' + linefeed +
                  'TEXT=' + %trim(errText);
       #em_fice = *blanks;
       #em_order# = *blanks;
       #em_seqnce = '';

       exsr $put_em;

       return;

       /copy rpgcopy,$srvrem_sp

       end-proc;

       // --------------------------------------------------
       // getContent: INTERNAL
       //    Purpose: get the content between a start variable and
       //             an end variable
       //             See MODWEB document or web_replaceVariables for a
       //             list of tags that use this procedure
       //    Returns: Buffer containing the content
       //    Parameters: criteria => Used to build the end tag
       //                buffer => the buffer to retrieve content from
       //                startIdx => index in buffer to start search at
       //                @endIdx => index to hold the position after the end
       //                           variable
       // --------------------------------------------------
       dcl-proc getContent;
       dcl-pi *n like(fileBuffer) rtnparm;
          criteria  like(HTML_PARM) const;
          buffer    like(fileBuffer) const;
          startIdx  like(idx) const;
          @endIdx   like(idx);
          @status   likeds(statusDs);
       end-pi;

     dendVarStart      s                   like(idx)
     dendVariable      s            100a   varying inz

       @status.code = SUCCESS;
       @status.message = '';
       endVariable = STARTTAG + 'END,' + criteria + ENDTAG;

       endVarStart = %scan(endVariable:buffer:startIdx);

       if endVarStart > 0 ;
          @endIdx = endVarStart + %len(endVariable);
          return %subst(buffer:startIdx:endVarStart - startIdx);
       else;
          @status.code = 'NF';
          @status.message = 'End variable not found';
       endif;

       return '';

       end-proc;

       //*********************************************************************
       // Function: existsIFS
       //*********************************************************************
       dcl-proc existsIFS;
          dcl-pi *n ind;
             $existFile char(512) const;
          end-pi;

          /copy prototypes,exists_ifs

          dcl-s existRc char(1);

          callp exists_ifs($existFile:existRc);

          return existRc;

       end-proc;

       // --------------------------------------------------
       // parseSwitch: INTERNAL
       //    Purpose: get the valid content between SWITCH block
       //    Returns: Buffer containing the content
       //    Parameters: buffer => the buffer to retrieve content from
       //                ptrProcessSwitch => procedure to handle ([CASE]) tags
       // --------------------------------------------------
       dcl-proc parseSwitch;
       dcl-pi *n like(smallBuffer) rtnparm;
          switchcriteria       like(HTML_PARM) const;
          buffer               like(fileBuffer) const;
          ptrProcessSwitch     pointer(*proc) const;
          @status              likeds(statusDs);
       end-pi;

       dcl-pr  processSwitch   like(HTML_PARM) extproc(ptrProcessSwitch);
         criteria              like(HTML_PARM) const;
       end-pr;

      * variables
      *
     dcaseVarStart     s                    like(idx) inz(1)
     dendVarStart      s                    like(idx) inz(1)
     dcaseCriteria     s                    like(HTML_PARM)
     dcaseStrVariable  s            100a   varying inz
     dcaseEndVariable  s            100a   varying inz

       // Start Procressing
       @status.code = SUCCESS;
       @status.message = '';

       if switchCriteria = *blanks;
          @status.code = 'NC';
          @status.message = 'Not a valid Criteria for SWITCH variable';
          return '';
       endif;

       caseCriteria = processSwitch(switchCriteria);

       if caseCriteria = *blanks;
          return '';
       else;
          caseStrVariable = STARTTAG + 'CASE,' + caseCriteria + ENDTAG;
          caseendVariable = STARTTAG + 'END,' + caseCriteria + ENDTAG;

          caseVarStart = %scan(caseStrVariable:buffer:1);
          endVarStart = %scan(caseendVariable:buffer:
                             caseVarStart + %len(caseStrVariable));
       endif;

       select;
       // case or its corresponding end variable not found
       when (caseVarStart > 0 and endVarStart = 0) or
            (caseVarStart = 0 and endVarStart > 0);
          @status.code = 'NF';
          @status.message = 'Case or its corresponding end variable not found';
          return '';

       // case Criteria found
       when caseVarStart > 0 and endVarStart > 0;
          return %subst(buffer:
                        caseVarStart + %len(caseStrVariable):
                        endVarStart - caseVarStart - %len(caseStrVariable));

       // case Criteria not found
       other;
          return '';
       endsl;

       end-proc;

       // --------------------------------------------------
       // getParameters: INTERNAL
       //    Purpose: parse an html variable into parameters
       //             the first parameter is the controlling variable
       //             see MODWEB doc for list of variable tags
       //    Returns: an array containing the parameters in order
       //    Parameters: variable => the variable in the html to parse
       //                            should not have start or end tags
       // --------------------------------------------------
       dcl-proc getParameters;
       dcl-pi *n like(HTML_PARM) dim(HTML_PARM_DIM);
          variable  like(tmpl_htmlLine) value;
       end-pi;

     d comma           s                   like(idx)
     d parmIdx         s                   like(idx) inz(1)
     d parmList        s                   like(HTML_PARM) dim(HTML_PARM_DIM)
     d                                     inz

       // parameters should separated by commas
       // check for more than one parameter
       comma = %scan(',':variable);
       dow comma <> 0 and parmIdx < HTML_PARM_DIM;

          // loop through all parameters but the last and process them
          parmList(parmIdx) = %subst(variable:1:comma-1);

          // remove the parameter that was just processed
          variable = %subst(variable:comma+1);

          // find next parameter's end
          comma = %scan(',':variable);
          parmIdx += 1;
       enddo;

       // process last parameter
       parmList(parmIdx) = variable;

       return parmList;

       end-proc;

       // --------------------------------------------------
       // readFile: INTERNAL
       //    Purpose: get the data from an IFS file
       //    Returns: Data from the file
       //    Parameters: IFS path to read the data from
       // --------------------------------------------------
       dcl-proc readFile;
          dcl-pi *n like(#wp_out.result) rtnparm;
             ifsFullName varchar(100) value;
          end-pi;

      /copy rpgcopy,ifshead

     dpString          s          32000a   inz
     dpStrLen          s             10i 0
     dfileDesc         s             10i 0
     dfileData         s                   like(#wp_out.result) inz

          // open ifs file
          fileDesc = open(%trimr(ifsFullName):
                          O_RDONLY + O_TEXTDATA :
                          S_IRGRP :
                          37);
          if filedesc < 0;
             doError('ONC':'Failed to open file: ' + ifsFullName +
                              linefeed);
             return fileData;
          endif;

          // read file from ifs
          pStrLen = read(filedesc:%addr(pString):%size(pString));
          pString =  %xlate(x'00':' ':pString);
          fileData = %trim(pString);

          // close file
          callp close(fileDesc);

          return fileData;

       end-proc;

       //***********************************************************************
       // INTERNAL
       // sanitizeBrowser: convert HTML special chars to blanks for browsers
       //***********************************************************************
       dcl-proc sanitizeBrowser;
       dcl-pi *n char(20);
          browserVal char(20) value;
       end-pi;

       browserVal = %scanrpl('&':'':browserVal);
       browserVal = %scanrpl(';':'':browserVal);
       browserVal = %scanrpl('/*':'':browserVal);
       browserVal = %scanrpl('*/':'':browserVal);
       browserVal = %scanrpl('-':'':browserVal);
       browserVal = %scanrpl('''':'':browserVal);
       browserVal = %scanrpl('>':'':browserVal);
       browserVal = %scanrpl('<':'':browserVal);
       browserVal = %scanrpl('"':'':browserVal);
       browserVal = %scanrpl('/':'':browserVal);
       browserVal = %scanrpl('#':'':browserVal);
       browserVal = %scanrpl(':':'':browserVal);

       return browserVal;

       end-proc;

      /copy rpgcopy,parsetbl
