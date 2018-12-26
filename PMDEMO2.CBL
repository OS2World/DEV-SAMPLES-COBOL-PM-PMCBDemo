      $set ans85 mf noosvs defaultbyte"00" callfh"extfh"
      *--------------------------------------------------------------*
      * PMDEMO2.CBL
      *
      * Sample program using entry fields defined in a resource file
      * and graphic display of entered values. These values are
      * written to an Indexed file
      * Copyright 1989, Micro Focus Ltd.
      * Author B J Edwards
      *--------------------------------------------------------------*
       environment division.
       special-names.
	    call-convention 3 is OS2API.
      *---------------------------------------------------------*
       input-output section.
       file-control.
           select PmFile assign "demofile.ism"
           organization is indexed
           record key is file-record-key
           access is dynamic.

       file section.
       fd  PmFile.
       01  PmFileRecord.
            03 file-record-key     pic x(5).
            03 file-field-contents pic x(48).

       working-storage section.

      *---------------------------------------------------------*
      *  COBOL Copy libraries containing relevant constants
      *---------------------------------------------------------*
           copy "HWND.78".
           copy "SPTR.78".
           copy "FCF.78".
           copy "WM.78".
           copy "WC.78".
           copy "WS.78".
           copy "CS.78".
           copy "MB.78".
           copy "MBID.78".
           copy "DID.78".
           copy "BM.78".
           copy "EM.78".
           copy "EN.78".
           copy "DT.78".
           copy "CLR.78".
           copy "DRO.78".
           copy "SWP.78".
           copy "PATSYM.78".
      *---------------------------------------------------------*
       78  WM-MYMESSAGE            value H"1001".
      *---------------------------------------------------------*
           copy "pmdemo2.cpy".

       78 ID-WINDOW                         VALUE 2.
       01  work-data.
           03  hab                 pic 9(9) comp-5.
           03  hmq                 pic 9(9) comp-5.
           03  hwndClient          pic 9(9) comp-5.
           03  hwndGraphClient     pic 9(9) comp-5.
           03  hwndFrame           pic 9(9) comp-5.
           03  hwndGraphFrame      pic 9(9) comp-5.
           03  hwndDialog          pic 9(9) comp-5.
           03  temp-hwnd           pic 9(9) comp-5.
           03  flFrameFlags        pic 9(9) comp-5.
           03  szClientClass       pic x(20)  value 'pmdemo2'.
           03                      pic x      value x"00".
           03  szGraphClass        pic x(20)  value 'graph'.
           03                      pic x      value x"00".
           03  GraphTitle          pic x(20)  value 'Graphs Display'.
           03                      pic x      value x"00".
           03  Header-text.
               05                  pic x(11)  value "Sales for: ".
               05  title-key       pic x(6).
               05                  pic x      value x"00".
           03  nullText            pic x      value x"00".
           03  loop-flag           pic x      value 'C'.
               88  loop-end                   value 'E'.
           03  ModHandle           pic s9(4) comp-5.
           03  ModName             pic x(7) value "PMDEMO2".
           03                      pic x value x"00".
           03  bool                pic 9(4) comp-5.
               88  boolTRUE                   value 1.
               88  boolFALSE                  value 0.
           copy "qmsg.cpy".

           03  strlen              pic s9(9) comp-5.
           03  ClientWndProc       procedure-pointer.
           03  AboutDlgProc        procedure-pointer.
           03  KeyDlgProc          procedure-pointer.
           03  DataDlgProc         procedure-pointer.
           03  GraphsWndProc       procedure-pointer.
        01 numeric-message.
           03  pic x(30) value "WARNING: Non numeric value in ".
           03  month-msg pic x(10).
           03                      pic x      value x"00".
        01 save-message.
           03  pic x(30) value "Do you wish to save the data".
           03                      pic x      value x"00".
        01 month-names.
           03       pic x(10) value "January".
           03       pic x(10) value "February".
           03       pic x(10) value "March".
           03       pic x(10) value "April".
           03       pic x(10) value "May".
           03       pic x(10) value "June".
           03       pic x(10) value "July".
           03       pic x(10) value "August".
           03       pic x(10) value "September".
           03       pic x(10) value "October".
           03       pic x(10) value "November".
           03       pic x(10) value "December".

        01 redefines month-names.
           03 month-name           pic x(10) occurs 12.

        01  MY-MB                  pic 9(4) comp-5.

        01  entry-field-contents.
            03 Jan-entry-data        pic x(3).
            03                       pic x value x"00".
            03 Feb-entry-data        pic x(3).
            03                       pic x value x"00".
            03 Mar-entry-data        pic x(3).
            03                       pic x value x"00".
            03 Apr-entry-data        pic x(3).
            03                       pic x value x"00".
            03 May-entry-data        pic x(3).
            03                       pic x value x"00".
            03 Jun-entry-data        pic x(3).
            03                       pic x value x"00".
            03 Jul-entry-data        pic x(3).
            03                       pic x value x"00".
            03 Aug-entry-data        pic x(3).
            03                       pic x value x"00".
            03 Sep-entry-data        pic x(3).
            03                       pic x value x"00".
            03 Oct-entry-data        pic x(3).
            03                       pic x value x"00".
            03 Nov-entry-data        pic x(3).
            03                       pic x value x"00".
            03 Dec-entry-data        pic x(3).
            03                       pic x value x"00".
       01  redefines entry-field-contents.
            03 entry-field-data      pic x(4) occurs 12.
       01  input-key-field.
            03  key-data             pic x(6).
            03                       pic x value x"00".

       01  real-numeric-values.
           03  real-num-val          pic 9(4) comp-5 occurs 12.

       01  saved-contents            pic x(60).

       01  test-char                 pic x.
       01  test-num redefines test-char pic 99 comp.
       01  i                       pic 99 comp-5.

       01 entry-field-handles.
           03  hwndEntryField      pic 9(9) comp-5 occurs 12.
       01  hwndKeyField            pic 9(9) comp-5.
       01  hwndKeyDisplay          pic 9(9) comp-5.

       01  temp-mp1                pic xxxx comp-5.
       01  redefines temp-mp1.
           03  temp-mp1w1          pic xx   comp-5.
           03  temp-mp1w2          pic xx   comp-5.

       01  temp-short              pic 9(4) comp-5.
       01  temp-bytes1             pic x(3).
       01  temp-bytes2             pic x(3).
       01  swp-param               pic 9(4) comp-5.
       01  text-draw               pic 9(4) comp-5.

       01  short-vars.
           03  cxChar      pic s9(4) comp-5.
           03  cxCaps      pic s9(4) comp-5.
           03  cyChar      pic s9(4) comp-5.
           03  cyDesc      pic s9(4) comp-5.
           03  cxClient    pic s9(4) comp-5.
           03  cyClient    pic s9(4) comp-5.

       01  entry-field-index       pic 9(4) comp-5.

       01  n                       pic 99 comp-5.
       01  loop-flag               pic 99 comp-5.
           88  loop-not-terminated value 0.
           88  loop-terminated     value 1.
       01  numeric-flag            pic 99 comp-5.
           88  numerics-fail       value 0.
           88  numerics-pass       value 1.
       01  key-flag                pic 99 comp-5.
           88  key-empty           value 0.
           88  key-not-empty       value 1.

       01  fail-index              pic 9(4) comp-5.

       01  workarea.
            03  temp-word       pic xx   comp-5.
            03  REDEFINES temp-word.
                05 temp-ls      pic x   comp-5.
                05 temp-ms      pic x   comp-5.
            03  temp-snum1          pic s9(4) comp-5.
            03  temp-snum2          pic s9(4) comp-5.
            03  temp-snum3          pic s9(4) comp-5.
            03  temp-snum4          pic s9(4) comp-5.

       01  temp-line.
           03  temp-char           pic x occurs 20.
       01  temp-ind                pic 9(4) comp-5.
       01  temp-ind1               pic 9(4) comp-5.
       01  temp-ind2               pic 9(4) comp-5.

      *---------------------------------------------------------*
       local-storage section.
       01  ptl.
           03  x        pic s9(9) comp-5.
           03  y        pic s9(9) comp-5.

       01  hps                     pic 9(9) comp-5.

       01  mresult                 pic s9(9) comp-5.
       01  rcl.
           copy "rectl.cpy".

       01 others.
           78 fm-start value NEXT.
           copy "FONTMETR.CPY".
           78 size-of-fm value NEXT - fm-start.

      *---------------------------------------------------------*
       linkage section.

       01  hwnd                    pic xxxx comp-5.
       01  msg                     pic xx   comp-5.

       01  mp1                     pic xxxx comp-5.
       01  redefines mp1.
           03  mp1w1               pic xx   comp-5.
           03  mp1w2               pic xx   comp-5.

       01  mp2                     pic xxxx comp-5.
       01  redefines mp2.
           03  mp2w1               pic xx   comp-5.
           03  mp2w2               pic xx   comp-5.

      *---------------------------------------------------------*
       procedure division OS2API.
       main section.
           perform start-up
           perform register-classes
           if boolTRUE
               perform create-client-window
               if hwndFrame not = 0
                    perform message-loop until loop-end
               end-if
           end-if
           perform shut-down
           stop run.
      *---------------------------------------------------------*
       start-up section.
           open i-o PmFile
           perform find-resource-handle
           perform set-procedure-entry-point
           call OS2API 'WinInitialize'
                       using   by value 0 size 2
                       returning hab

           call OS2API 'WinCreateMsgQueue'
                       using by value hab
                             by value 0 size 2
                       returning hmq.
      *---------------------------------------------------------*
       set-procedure-entry-point section.
           set ClientWndProc  to ENTRY 'ClientWndProc'
           set AboutDlgProc   to ENTRY 'AboutDlgProc'
           set KeyDlgProc     to ENTRY 'KeyDlgProc'.
           set DataDlgProc    to ENTRY 'DataDlgProc'.
           set GraphsWndProc  to ENTRY 'GraphsWndProc'.
      *---------------------------------------------------------*
       register-classes section.
           call OS2API 'WinRegisterClass'
                       using by value     hab
                             by reference szClientClass
                             by value     ClientWndProc
                             by value     CS-SIZEREDRAW size 4
                             by value     0        size 2
                       returning bool.
           call OS2API 'WinRegisterClass'
                       using by value     hab
                             by reference szGraphClass
                             by value     GraphsWndProc
                             by value     CS-SIZEREDRAW size 4
                             by value     0        size 2
                       returning bool.
      *---------------------------------------------------------*
       message-loop section.
           call OS2API 'WinGetMsg'
                         using   by value hab
                                 by reference qmsg
                                 by value 0            size 4
                                 by value 0            size 2
                                 by value 0            size 2
                         returning bool

           if boolFALSE
                set loop-end to true
           else
                call OS2API 'WinDispatchMsg'
                         using by value hab
                               by reference qmsg
           end-if.
      *---------------------------------------------------------*
       find-resource-handle section.
           call OS2API 'DosGetModHandle'
                           using by reference ModName
                                 by reference ModHandle
                           returning bool
           if not boolFALSE
               move 0 to ModHandle
           end-if.
      *---------------------------------------------------------*
       shut-down section.
           call OS2API 'WinDestroyWindow'   using by value
                                                   hwndGraphClient
           call OS2API 'WinDestroyWindow'   using by value hwndFrame
           call OS2API 'WinDestroyMsgQueue' using by value hmq
           call OS2API 'WinTerminate'       using by value hab
           close PmFile.
      *---------------------------------------------------------*
       create-client-window section.
           compute flFrameFlags = FCF-TITLEBAR      + FCF-SYSMENU
                                + FCF-SIZEBORDER    + FCF-MINMAX
                                + FCF-SHELLPOSITION + FCF-TASKLIST
                                + FCF-MENU          + FCF-ICON

           call OS2API 'WinCreateStdWindow'
                            using by value     HWND-DESKTOP size 4
                                  by value     WS-VISIBLE   size 4
                                  by reference flFrameFlags
                                  by reference szClientClass
                                  by reference nulltext
                                  by value     0            size 4
                                  by value     ModHandle
                                  by value     ID-RESOURCE  size 2
                                  by reference hwndClient
                            returning hwndFrame

           compute flFrameFlags = FCF-TITLEBAR
                                + FCF-SHELLPOSITION + FCF-BORDER

           call OS2API 'WinCreateStdWindow'
                            using by value     hwndClient
                                  by value     WS-VISIBLE   size 4
                                  by reference flFrameFlags
                                  by reference szGraphClass
                                  by reference GraphTitle
                                  by value     0            size 4
                                  by value     0            size 2
                                  by value     0            size 2
                                  by reference hwndGraphClient
                            returning hwndGraphFrame.
      *---------------------------------------------------------*
       MyClientWndProc section.

       entry 'ClientWndProc' using
                              by value hwnd
			      by value msg
			      by value mp1
			      by value mp2.

           evaluate msg
               when WM-CREATE
                   perform WM-CREATE-routine

               when WM-COMMAND
                   perform WM-COMMAND-routine

               when WM-PAINT
                   perform WM-PAINT-routine

               when OTHER
                   perform WM-Default-Window-routine

           end-evaluate

           exit program returning mresult.

       WM-COMMAND-routine section.
           evaluate mp1w1

               when IDM-ENTRIES

                   call OS2API 'WinDlgBox'
                      using by value HWND-DESKTOP size 4
                            by value hwnd
                            by value KeyDlgProc
                            by value modHandle
                            by value IDD-KEY size 2
                            by value 0 size 4

                  perform check-for-empty-key

                  if key-not-empty
                       call OS2API 'WinDlgBox'
                          using by value HWND-DESKTOP size 4
                                by value hwnd
                                by value DataDlgProc
                                by value modHandle
                                by value IDD-DATA size 2
                                by value 0 size 4
                  end-if

               when IDM-ABOUT
                   call OS2API 'WinDlgBox'
                      using by value HWND-DESKTOP size 4
                            by value hwnd
                            by value AboutDlgProc
                            by value modHandle
                            by value IDD-ABOUT size 2
                            by value 0 size 4

           end-evaluate

           move 0 to mresult.

       WM-CREATE-routine section.

           move low-values to entry-field-contents
           move 0 to mresult.

       WM-PAINT-routine section.
           call OS2API 'WinBeginPaint' using
                 by value hwnd
                 by value 0 size 4
                 by value 0 size 4
              returning hps

           call OS2API 'GpiErase' using by value hps

           call OS2API 'WinEndPaint' using
                 by value hps

           move 0 to mresult.

       WM-Default-Window-routine section.
           call OS2API 'WinDefWindowProc'
                 using by value hwnd
                       by value msg
                       by value mp1
                       by value mp2
                 returning mresult.

      *---------------------------------------------------------*
       MyAboutDialogProc section.

       entry 'AboutDlgProc' using
                              by value hwnd
			      by value msg
			      by value mp1
			      by value mp2.
          evaluate msg
              when WM-COMMAND
                  evaluate mp1w1
                      when DID-OK
                      when DID-CANCEL
                          call OS2API 'WinDismissDlg' using
                            by value hwnd
                            by value 1 size 2
                          move 0 to mresult
                  end-evaluate
              when other
                  call OS2API 'WinDefDlgProc'
                   using by value hwnd
                         by value msg
                         by value mp1
                         by value mp2
                   returning mresult
          end-evaluate
          exit program returning mresult.

      *---------------------------------------------------------*
       MyDataDialogProc section.

       entry 'DataDlgProc' using
                              by value hwnd
			      by value msg
			      by value mp1
			      by value mp2.
          evaluate msg
              when WM-INITDLG
                  move hwnd to hwndDialog
                  call OS2API 'WinPostMsg' using
                        by value hwnd
                        by value WM-MYMESSAGE size 2
                        by value 0 size 4
                        by value 0 size 4

                  move 1 to mresult

              when WM-MYMESSAGE
                  move saved-contents to entry-field-contents
                  perform find-entry-handles
                  perform find-display-handle
                  perform set-entry-field-sizes
                  perform fill-entry-fields
                  perform fill-display-field

                  call OS2API 'WinSetFocus' using
                        by value HWND-DESKTOP size 4
                        by value hwndEntryField(1)

                  move 0 to mresult

              when WM-COMMAND
                  evaluate mp1w1
                      when DID-OK
                          perform get-entry-field-values
                          perform extract-numerics
                          perform show-graph
                          if numerics-fail
                              move month-name(fail-index) to month-msg
                              compute MY-MB = MB-ICONEXCLAMATION +
                                              MB-ABORTRETRYIGNORE +
                                              MB-DEFBUTTON2 +
                                              MB-MOVEABLE
                              call OS2API 'WinMessageBox'
                                  using by value HWND-DESKTOP size 4
                                        by value HWND-DESKTOP size 4
                                        by reference numeric-message
                                        by reference szClientClass
                                        by value 0 size 2
                                        by value MY-MB
                                  returning temp-short
                              evaluate temp-short
                               when MBID-IGNORE
                                 move entry-field-contents to
                                                       saved-contents
                                 perform save-record-to-disk
                                 call OS2API 'WinDismissDlg' using
                                       by value hwnd
                                       by value 0 size 2
                               when MBID-RETRY
                                 call OS2API 'WinSetFocus' using
                                     by value HWND-DESKTOP size 4
                                     by value hwndEntryField(fail-index)
                               when MBID-ABORT
                                 call OS2API 'WinDismissDlg' using
                                       by value hwnd
                                       by value 0 size 2
                                 move saved-contents to
                                                 entry-field-contents
                                 perform extract-numerics
                                 perform show-graph
                              end-evaluate
                          else
                              compute MY-MB = MB-ICONQUESTION +
                                              MB-YESNO +
                                              MB-MOVEABLE
                              call OS2API 'WinMessageBox'
                                  using by value HWND-DESKTOP size 4
                                        by value HWND-DESKTOP size 4
                                        by reference save-message
                                        by reference szClientClass
                                        by value 0 size 2
                                        by value MY-MB
                                  returning temp-short
                              if temp-short = MBID-YES
                                  move entry-field-contents to
                                                       saved-contents
                                  perform save-record-to-disk
                              else
                                  move saved-contents to
                                                 entry-field-contents
                                  perform extract-numerics
                                  perform show-graph
                              end-if
                              call OS2API 'WinDismissDlg' using
                                       by value hwnd
                                       by value 0 size 2
                          end-if
                  end-evaluate

                  move 0 to mresult

              when other

                  call OS2API 'WinDefDlgProc' using
                         by value hwnd
                         by value msg
                         by value mp1
                         by value mp2
                   returning mresult
          end-evaluate
          exit program returning mresult.

       MyKeyDialogProc section.

       entry 'KeyDlgProc' using
                              by value hwnd
			      by value msg
			      by value mp1
			      by value mp2.
          evaluate msg
              when WM-INITDLG
                  move hwnd to hwndDialog
                  call OS2API 'WinPostMsg' using
                        by value hwnd
                        by value WM-MYMESSAGE size 2
                        by value 0 size 4
                        by value 0 size 4

                  move 1 to mresult

              when WM-MYMESSAGE
                  perform find-key-handle
                  perform set-key-field-size
                  perform fill-key-field

                  call OS2API 'WinSetFocus' using
                        by value HWND-DESKTOP size 4
                        by value hwndKeyField

                  move 0 to mresult

              when WM-COMMAND
                  evaluate mp1w1
                      when DID-OK
                          perform get-key-field-value
                          perform load-record-from-disk
                          call OS2API 'WinDismissDlg' using
                                       by value hwnd
                                       by value 0 size 2
                  end-evaluate

                  move 0 to mresult

              when other

                  call OS2API 'WinDefDlgProc' using
                         by value hwnd
                         by value msg
                         by value mp1
                         by value mp2
                   returning mresult
          end-evaluate
          exit program returning mresult.

       find-entry-handles section.
           move IDD-ENTRYFLDJAN to temp-short
           move 1 to entry-field-index
           perform 12 times
               call OS2API 'WinWindowFromID' using
                        by value hwnd
                        by value temp-short
                        returning temp-hwnd
               move temp-hwnd to hwndEntryField(entry-field-index)
               add 1 to temp-short
               add 1 to entry-field-index
           end-perform.

       find-display-handle section.
           move IDD-KEYDISPLAY to temp-short
               call OS2API 'WinWindowFromID' using
                        by value hwnd
                        by value temp-short
                        returning temp-hwnd
               move temp-hwnd to hwndKeyDisplay.

       find-key-handle section.
           move IDD-ENTRYFLDKEY to temp-short
           call OS2API 'WinWindowFromID' using
                        by value hwnd
                        by value temp-short
                        returning temp-hwnd
           move temp-hwnd to hwndKeyField.

       set-entry-field-sizes section.
           move 3 to temp-mp1w1
           move 0 to temp-mp1w2
           move 1 to entry-field-index
           perform 12 times
               call OS2API 'WinSendMsg'using
                   by value hwndEntryField(entry-field-index)
                   by value EM-SETTEXTLIMIT   size 2
                   by value temp-mp1
                   by value 0            size 4
               add 1 to entry-field-index
           end-perform.

       set-key-field-size section.
           move 6 to temp-mp1w1
           move 0 to temp-mp1w2
           call OS2API 'WinSendMsg'using
                   by value hwndKeyField
                   by value EM-SETTEXTLIMIT   size 2
                   by value temp-mp1
                   by value 0            size 4.

       fill-entry-fields section.
           move 1 to entry-field-index
           perform 12 times
               call OS2API 'WinSetWindowText' using
                  by value hwndEntryField(entry-field-index)
                  by reference entry-field-data(entry-field-index)
               add 1 to entry-field-index
           end-perform.

       fill-display-field section.
           call OS2API 'WinSetWindowText' using
                  by value hwndKeyDisplay
                  by reference input-key-field
                  returning bool.

       fill-key-field section.
           call OS2API 'WinSetWindowText' using
                  by value hwndKeyField
                  by reference input-key-field.

       get-entry-field-values section.
           move low-values to entry-field-contents
           move 1 to entry-field-index
           perform 12 times
               call OS2API 'WinQueryWindowText' using
                   by value hwndEntryField(entry-field-index)
                   by value 5 size 2
                   by reference entry-field-data(entry-field-index)
               returning bool
               add 1 to entry-field-index
           end-perform.

       get-key-field-value section.
           move low-values to input-key-field
           call OS2API 'WinQueryWindowText' using
                   by value        hwndKeyField
                   by value        7               size 2
                   by reference    input-key-field
               returning bool.

       extract-numerics section.
           set numerics-pass to true
           perform varying entry-field-index from 1 by 1 until
                   entry-field-index > 12
               move entry-field-data(entry-field-index) to temp-bytes1
               move spaces to temp-bytes2
               move 3 to n
               if temp-bytes1(3:1) = low-values or spaces
                   if temp-bytes1(2:1) = low-values or spaces
                       if temp-bytes1(1:1) = low-values or spaces
                           move temp-bytes1 to temp-bytes2
                       else
                           move temp-bytes1(1:1) to temp-bytes2(3:1)
                       end-if
                   else
                       move temp-bytes1(1:2) to temp-bytes2(2:2)
                   end-if
               else
                   move temp-bytes1 to temp-bytes2
               end-if
               inspect temp-bytes2 replacing leading low-values by "0"
               inspect temp-bytes2 replacing leading spaces by "0"
               if temp-bytes2 not numeric
                   if numerics-pass
                       set numerics-fail to true
                       move entry-field-index to fail-index
                   end-if
                   move 0 to real-num-val(entry-field-index)
               else
                   move temp-bytes2 to real-num-val(entry-field-index)
               end-if
           end-perform.
      *---------------------------------------------------------*
       MyGraphsWndProc section.

       entry 'GraphsWndProc' using
                              by value hwnd
                              by value msg
                              by value mp1
                              by value mp2.

          evaluate msg
               when WM-CREATE
                   call OS2API 'WinGetPS'
                                using by value hwnd
                                returning hps

                   call OS2API 'GpiQueryFontMetrics'
                                using by value hps
                                      by value size-of-fm size 4
                                      by reference FONTMETRICS
                                returning bool

                   move FONTMETRICS-lEmInc          to cxCaps
                   move FONTMETRICS-lMaxBaselineExt to cyChar
                   move FONTMETRICS-lMaxDescender   to cyDesc

                   call OS2API 'WinReleasePS'
                                using by value hps

                   call OS2API 'WinPostMsg' using
                        by value hwnd
                        by value WM-MYMESSAGE size 2
                        by value 0 size 4
                        by value 0 size 4

                   move 0 to mresult

              when WM-MYMESSAGE
                call OS2API 'WinQueryWindowRect'
                           using   by value hwndClient
                                   by reference rcl

                compute temp-snum1 = (RECTL-xRight / 4)
                compute temp-snum2 = 0
                compute temp-snum3 = (RECTL-xRight / 4 * 3)
                compute temp-snum4 = RECTL-yTop
                compute swp-param = SWP-DEACTIVATE + SWP-MOVE
                                  + SWP-SHOW     + SWP-SIZE
                call OS2API 'WinSetWindowPos' using
                        by value hwndGraphFrame
                        by value HWND-TOP size 4
                        by value temp-snum1
                        by value temp-snum2
                        by value temp-snum3
                        by value temp-snum4
                        by value swp-param
                  move 0 to mresult

               when WM-SIZE
                   move mp2w1 to cxClient
                   move mp2w2 to cyClient
                   move 0 to mresult

               when WM-PAINT
                   call OS2API 'WinBeginPaint' using
                         by value hwnd
                         by value 0 size 4
                         by value 0 size 4
                      returning hps

                   call OS2API 'GpiErase' using by value hps
                   perform draw-graph
                   call OS2API 'WinEndPaint' using
                        by value hps
                   move 0 to mresult

               when WM-CLOSE
                   move 0 to mresult

               when OTHER
                   perform WM-Default-Window-routine
          end-evaluate
          exit program returning mresult.

       find-string-len section.
           if temp-line = spaces
               move 0 to temp-ind
           else
               perform varying temp-ind from 20 by -1 until
                                        temp-char(temp-ind) not = spaces
               end-perform
           end-if
           move temp-ind to strlen.

       draw-graph section.
             call OS2API 'GpiSetPattern'
                            using by value hps
                                  by value PATSYM-HALFTONE size 4

             call OS2API 'GpiSetColor'
                                using by value hps
                                      by value CLR-BLACK size 4
             compute x = cxCaps + 50
             compute y = cyClient - 2 * cyChar + cyDesc + 15
             if key-data = spaces or low-values
                 move spaces to temp-line
             else
                 move key-data to title-key
                 move header-text to temp-line
             end-if
             perform find-string-len
             call OS2API 'GpiCharStringAt'
                                using by value hps
                                      by reference ptl
                                      by value strlen
                                      by reference temp-line

             perform varying entry-field-index
              from 1 by 1 until entry-field-index > 12
               move entry-field-index to temp-ind1
               divide 2 into temp-ind1
               multiply 2 by temp-ind1
               if temp-ind1 = entry-field-index
                   call OS2API 'GpiSetColor'
                                using by value hps
                                      by value CLR-BLUE size 4
               else
                   call OS2API 'GpiSetColor'
                                using by value hps
                                      by value CLR-RED size 4
               end-if
               move cxCaps to x
               move entry-field-index to temp-ind
               subtract 1 from temp-ind
               divide temp-ind by 12 giving temp-ind1
                                  remainder temp-ind2
               compute y = cyClient - (temp-ind2 * 2.25 + 4)
                                           * cyChar / 2 + cyDesc
               move month-name(entry-field-index) to temp-line
               perform find-string-len
               call OS2API 'GpiCharStringAt'
                                using by value hps
                                      by reference ptl
                                      by value strlen
                                      by reference temp-line

               compute x = cxCaps * 10
               compute y = y - (cyDesc + cyChar / 2) + 5

               call OS2API 'GpiMove'
                                using by value hps
                                      by reference ptl
               compute x = x + real-num-val(entry-field-index) / 5
               compute y = y + cyChar - 1
               if real-num-val(entry-field-index) not = 0
                   call OS2API 'GpiBox'
                                using by value hps
                                      by value DRO-FILL size 4
                                      by reference ptl
                                      by value 0 size 4
                                      by value 0 size 4
               end-if
           end-perform.

       show-graph section.
           call OS2API 'WinUpdateWindow'
                    using by value hwndGraphClient
           call OS2API 'WinInvalidateRect'
                    using by value hwndGraphClient
                          by value 0 size 4
                          by value 0 size 2.

       load-record-from-disk section.
           perform capitalize-key
           move input-key-field to file-record-key
           read PmFile
               invalid key
                   move low-values to saved-contents
               not invalid key
                   move file-field-contents to saved-contents
           end-read
           move saved-contents to entry-field-contents
           perform extract-numerics
           perform show-graph.

        save-record-to-disk section.
           perform capitalize-key
           move input-key-field to file-record-key
           move saved-contents to file-field-contents
           write PmFileRecord
               invalid key
                   rewrite PmFileRecord
                   end-rewrite
           end-write.

       capitalize-key section.
           perform varying i from 1 by 1 until i > 6
               move input-key-field(i:1) to test-char
               if test-num > 96 and < 123
                   subtract 32 from test-num
                   move test-char to input-key-field(i:1)
               end-if
           end-perform.

       check-for-empty-key section.
           set key-empty to true
           perform varying i from 1 by 1 until i > 6 or key-not-empty
               if input-key-field(i:1) = spaces or low-values
                   next sentence
               else
                   set key-not-empty to true
               end-if
           end-perform.
