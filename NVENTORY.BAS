'$DYNAMIC

DEFINT A-Z
DECLARE SUB CalcMax ()
DECLARE SUB AboutMenu ()
DECLARE SUB AddRecord ()
DECLARE SUB BarMenu ()
DECLARE SUB ChangeDir ()
DECLARE SUB ChangeRecord ()
DECLARE SUB ChangeSortField ()
DECLARE SUB ClearRandom ()
DECLARE SUB ClearTemp ()
DECLARE SUB DeleteRecord ()
DECLARE SUB EditMenu ()
DECLARE SUB Ending ()
DECLARE SUB FileMenu ()
DECLARE SUB GenerateRandomFile ()
DECLARE SUB GetDirectory ()
DECLARE SUB Intro ()
DECLARE SUB Main ()
DECLARE SUB MoveFile ()
DECLARE SUB Optimize ()
DECLARE SUB OptionsMenu ()
DECLARE SUB PLock (RecordNum%)
DECLARE SUB PrintAll ()
DECLARE SUB PrintMenu ()
DECLARE SUB PUnlock (RecordNum%)
DECLARE SUB Relog ()
DECLARE SUB ShowAll ()
DECLARE SUB ShowMessage (Message$)
DECLARE SUB SelectBrowse ()
DECLARE SUB SendToPrinter (RecordNum%)
DECLARE SUB Sort (NumOfFields%, SortField%, NumOfItems%)

DECLARE FUNCTION Browse% (currItem%, X%, Y%, L%, H%, Shadow%, ShadChar$, NumOfFields%, Locs%(), Title$(), NumOfInfo%, WindowTitle$)
DECLARE SUB ClearKey ()
DECLARE SUB CloseAll ()
DECLARE SUB CloseWindow (WinNumber)
DECLARE SUB DrawFrame (WinNumber)
DECLARE SUB DrawShadow (WinNumber%)
DECLARE SUB DrawTitle (WinNumber%)
DECLARE FUNCTION ExplodeWindow% (X%, Y%, L%, H%, Fore%, Back%, Frame$, Title$, Shadow%, ShadChar$)
DECLARE SUB GetKey (Char$)
DECLARE FUNCTION GetString$ (WinNumber, row, col, Start$, end$, Vis, MAX)
DECLARE FUNCTION MakeWindow% (X%, Y%, L%, H%, Fore%, Back%, Frame$, Title$, Shadow%, ShadChar$)
DECLARE FUNCTION Menu% (currChoice%, X%, Y%, Length%, Fg%, Bg%, Item$(), Hot$, NumOfItems%)
DECLARE SUB RedrawAll ()
DECLARE SUB WinPrintCenter (WinNumber%, X%, text$)
DECLARE SUB WindowReset ()
DECLARE SUB WindowInitialize ()
DECLARE SUB WinRedraw (WinNumber)
DECLARE SUB WinPrint (WinNumber, Xt, Yt, text$)

CONST MAXWINDOW = 5
CONST TRUE = -1
CONST FALSE = NOT TRUE

TYPE WindowStruct
    Frame AS STRING * 6
    XPos AS INTEGER
    YPos AS INTEGER
    Foreground AS INTEGER
    Background AS INTEGER
    Length AS INTEGER
    Height AS INTEGER
    Title AS STRING * 65
    Buffer AS STRING * 2000
    Shadow AS INTEGER
    ShadChar AS STRING * 1
END TYPE

DIM SHARED Handle(0 TO MAXWINDOW + 1) AS WindowStruct
DIM SHARED WinCount
DIM SHARED redraw
DIM SHARED BackChar$
DIM SHARED Fore
DIM SHARED Back
DIM SHARED Move$
DIM SHARED Dur
DIM SHARED choice
DIM SHARED subchoice
DIM SHARED DoubleFrame AS STRING * 6
DIM SHARED SingleFrame AS STRING * 6
DoubleFrame = "É»È¼ºÍ"
SingleFrame = "Ú¿ÀÙ³Ä"

'Start of NVENTORY
CONST STARTMAX = 1

DIM SHARED MAXRECS
MAXRECS = STARTMAX

TYPE RecordStruct
    SerialNumber AS STRING * 15
    Item AS STRING * 20
    Model AS STRING * 10
    Department AS STRING * 10
    Location AS STRING * 10
    Note1 AS STRING * 10
    Note2 AS STRING * 10
END TYPE
DIM SHARED Equip AS RecordStruct
DIM SHARED EquipLength
EquipLength = LEN(Equip)

DIM SHARED PrintErr
DIM SHARED IDNum$
DIM SHARED MenuC1
DIM SHARED MenuC2
DIM SHARED ManipulateC1
DIM SHARED ManipulateC2
DIM SHARED InfoC1
DIM SHARED InfoC2
DIM SHARED PercentageChar$
DIM SHARED Model$(19)
DIM SHARED SortField
DIM SHARED Directory$
DIM SHARED RanFile$
DIM SHARED PrintedLines

DIM SHARED Info$(7, 2)
DIM SHARED Title$(5)
DIM SHARED Locs(5)

Title$(1) = "Serial Number"
Locs(1) = 1
Title$(2) = "Item Description"
Locs(2) = 16
Title$(3) = "Model Num"
Locs(3) = 37
Title$(4) = "Department"
Locs(4) = 48
Title$(5) = "Location"
Locs(5) = 59

ON ERROR GOTO ErrorHandler


LOCATE , , 1, 11, 12
PRINT
DO
    PRINT "Are you using a (C)olor or (M)onochrome monitor?";
    GetKey Kbd$
    PRINT Kbd$;
    Kbd$ = UCASE$(Kbd$)

    SELECT CASE Kbd$
        CASE "M"
            MenuC1 = 0
            MenuC2 = 7
            ManipulateC1 = 0
            ManipulateC2 = 7
            InfoC1 = 0
            InfoC2 = 7
            EXIT DO

        CASE "C"
            MenuC1 = 15
            MenuC2 = 4
            ManipulateC1 = 1
            ManipulateC2 = 3
            InfoC1 = 7
            InfoC2 = 1
            EXIT DO

        CASE ELSE
    END SELECT

LOOP
PRINT

BarMenu

ErrorHandler:
SELECT CASE ERR
    CASE 24, 25, 27
        PrintErr = TRUE
        BadPrint = ExplodeWindow(10, 13, 53, 1, 15, 4, SingleFrame, "ERROR!", 1, "")
        WinPrintCenter BadPrint, 1, "Printer not responding ... Any key to continue"
        SOUND 1900, .2
        GetKey ""
        CloseWindow BadPrint
        RESUME NEXT
    CASE 7, 14
        LOCATE 2, 1, 1
        COLOR 7, 0
        PRINT "The program has run out of memory.  TERMINATING PROGRAM!";
        SLEEP 5
        END
    CASE 70
        SLEEP 2
        RESUME
    CASE ELSE
END SELECT
RESUME NEXT

REM $STATIC
SUB AboutMenu
    DIM MenuItem$(1)

    MenuItem$(1) = " About..."
    Hot$ = "A"

    DO UNTIL NewChoice < 0
        COLOR MenuC2, MenuC1
        LOCATE 1, 2
        PRINT " ð "

        NewChoice = Menu(NewChoice, 2, 1, 22, MenuC1, MenuC2, MenuItem$(), Hot$, 1)

        SELECT CASE NewChoice
            CASE 1
                GOSUB ShowAboutStuff
            CASE -2
                subchoice = -2
            CASE -3
                subchoice = -3
            CASE ELSE
        END SELECT
    LOOP
    EXIT SUB

    ShowAboutStuff:
    AboutWin = ExplodeWindow(7, 17, 45, 8, ManipulateC1, ManipulateC2, SingleFrame, "", 1, "")
    WinPrintCenter AboutWin, 2, "INVENTORY MANAGER"
    WinPrintCenter AboutWin, 3, "Version 4.8 Release 3"
    WinPrintCenter AboutWin, 4, "Copyright (C) Nathan Thomas, 1993."
    WinPrintCenter AboutWin, 5, "Produced by Independent Distributors"
    WinPrintCenter AboutWin, 7, STRING$(45, 196)
    WinPrintCenter AboutWin, 8, "<  OK  >"
       
    Kbd$ = ""
    DO UNTIL Kbd$ = CHR$(13)
        Kbd$ = GetString$(AboutWin, 8, 22, "", "", 0, 0)
    LOOP

    CloseWindow AboutWin
    RETURN

END SUB

SUB AddRecord
    Equip.SerialNumber = CHR$(255)
    Equip.Item = CHR$(255)
    Equip.Model = CHR$(255)
    Equip.Department = CHR$(255)
    Equip.Location = CHR$(255)
    Equip.Note1 = CHR$(255)
    Equip.Note2 = CHR$(255)
   
    AddWin = ExplodeWindow(3, 2, 74, 9, ManipulateC1, ManipulateC2, DoubleFrame, "Add an Item", 1, "")
    WinPrint AddWin, 2, 1, "Serial Number:_______________     Item Description:____________________"
    WinPrint AddWin, 3, 1, STRING$(74, 196)
    WinPrint AddWin, 4, 1, "Model:__________    Department:__________    Location:__________"
    WinPrint AddWin, 5, 1, STRING$(74, 196)
    WinPrint AddWin, 6, 1, "TAB        to move forward in fields       <ESC>   to abort input"
    WinPrint AddWin, 7, 1, "BACKTAB    to move backward in fields      <ENTER> to accept information"
    WinPrint AddWin, 8, 1, STRING$(74, 196)
    WinPrintCenter AddWin, 9, "Type a '?' in the Department field for a list of Departments"

    place = 1

    DO UNTIL finished = TRUE
        IF place = 1 THEN
            Kbd$ = GetString$(AddWin, 2, 15, Serial$, Serial$, 15, 15)
            IF Move$ = "TAB" THEN place = 2
            IF Move$ = "BKTAB" THEN place = 5
            IF Move$ = "ESC" THEN CloseWindow AddWin: EXIT SUB
            IF Move$ = "RET" THEN finished = TRUE
        END IF
        IF place = 2 THEN
            Kbd$ = GetString$(AddWin, 2, 52, Item$, Item$, 20, 20)
            IF Move$ = "TAB" THEN place = 3
            IF Move$ = "BKTAB" THEN place = 1
            IF Move$ = "ESC" THEN CloseWindow AddWin: EXIT SUB
            IF Move$ = "RET" THEN finished = TRUE
        END IF
        IF place = 3 THEN
            Kbd$ = GetString$(AddWin, 4, 7, Model$, Model$, 10, 10)
            IF Move$ = "TAB" THEN place = 4
            IF Move$ = "BKTAB" THEN place = 2
            IF Move$ = "ESC" THEN CloseWindow AddWin: EXIT SUB
            IF Move$ = "RET" THEN finished = TRUE
        END IF
        IF place = 4 THEN
            Kbd$ = GetString$(AddWin, 4, 32, Department$, Department$, 10, 10)
            IF RTRIM$(Department$) = "?" THEN
                GOSUB ShowChoices
            END IF
            IF Move$ = "TAB" THEN place = 5
            IF Move$ = "BKTAB" THEN place = 3
            IF Move$ = "ESC" THEN CloseWindow AddWin: EXIT SUB
            IF Move$ = "RET" THEN finished = TRUE
        END IF
        IF place = 5 THEN
            Kbd$ = GetString$(AddWin, 4, 55, Location$, Location$, 10, 10)
            IF Move$ = "TAB" THEN place = 1
            IF Move$ = "BKTAB" THEN place = 4
            IF Move$ = "ESC" THEN CloseWindow AddWin: EXIT SUB
            IF Move$ = "RET" THEN finished = TRUE
        END IF

        IF finished = TRUE THEN
            flag = 0
            IF RTRIM$(Serial$) = "" THEN
                ErrorWin = ExplodeWindow(6, 25, 40, 1, 15, 4, SingleFrame, "ERROR!", 1, "")
                WinPrintCenter ErrorWin, 1, "Can't save without Serial Number!"
                SOUND 1900, .2
                GetKey ""
                CloseWindow ErrorWin
                finished = FALSE
                flag = -1
            ELSE
                ShowMessage "Searching..."

                CalcMax
                OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                FOR count = 1 TO MAXRECS

                    GET #1, count, Equip

                    IF RTRIM$(UCASE$(Equip.SerialNumber)) = RTRIM$(UCASE$(Serial$)) THEN
                        flag = -1
                        EXIT FOR
                    END IF

                    IF ASC(Equip.SerialNumber) = 255 THEN
                        flag = count
                        EXIT FOR
                    END IF
                NEXT
                CLOSE #1

                IF flag = 0 THEN
                    flag = count
                    finished = TRUE
                END IF
                IF flag = -1 THEN
                    ErrorWin = ExplodeWindow(6, 25, 41, 1, 15, 4, SingleFrame, "ERROR!", 1, "")
                    WinPrintCenter ErrorWin, 1, "There's already an item by that number!"
                    SOUND 1900, .2
                    GetKey ""
                    CloseWindow ErrorWin
                    finished = FALSE
                END IF
                IF flag >= 0 THEN
                    finished = TRUE
                    ShowMessage "Writing Record..."
                                         
                    OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                                  
                    Serial$ = UCASE$(Serial$)
                    Department$ = UCASE$(Department$)
                                  
                    IF RTRIM$(Item$) = "" THEN Item$ = CHR$(255)
                    IF RTRIM$(Model$) = "" THEN Model$ = CHR$(255)
                    IF RTRIM$(Department$) = "" THEN Department$ = CHR$(255)
                    IF RTRIM$(Location$) = "" THEN Location$ = CHR$(255)
                    Equip.SerialNumber = Serial$
                    Equip.Item = Item$
                    Equip.Model = Model$
                    Equip.Department = Department$
                    Equip.Location = Location$
                    Equip.Note1 = IDNum$
                    Equip.Note2 = ""
                    PUT #1, flag, Equip
                    CLOSE #1
                    ShowMessage "Inventory Manager  (c)Copyright 1993 by Nathan Thomas"
                END IF
            END IF
        END IF
    LOOP
    CloseWindow AddWin
    EXIT SUB

    ShowChoices:
    FOR t = 1 TO MAXRECS
        Model$(t) = ""
    NEXT
    ShowFlag = 0

    NumberOfChoices = 0
    OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
    num.recs = LOF(1) / EquipLength
    FOR count = 1 TO num.recs
        GET #1, count, Equip
        IF ASC(Equip.Department) <> 255 THEN
            CurrentInfo$ = UCASE$(RTRIM$(Equip.Department))
            FOR t = 1 TO NumberOfChoices
                IF CurrentInfo$ = Model$(t) THEN
                    ShowFlag = 1
                    EXIT FOR
                END IF
            NEXT
            IF ShowFlag = 0 THEN
                ShowFlag = 0
                Model$(NumberOfChoices + 1) = CurrentInfo$
                NumberOfChoices = NumberOfChoices + 1
            ELSE
                ShowFlag = 0
            END IF
        END IF
    NEXT
    CLOSE #1

    Equip.SerialNumber = Serial$
    Equip.Item = Item$
    Equip.Model = Model$
    Equip.Location = Location$
    Equip.Note1 = Create$

    IF NumberOfChoices > 18 THEN NumberOfChoices = 18

    Model$(NumberOfChoices + 1) = "None!"

    NewChoice = Menu(NewChoice, 4, 4, 10, MenuC1, MenuC2, Model$(), Hot$, NumberOfChoices + 1)

    Move$ = ""

    IF NewChoice = NumberOfChoices + 1 OR NewChoice < 1 THEN Department$ = "": RETURN

    Department$ = RTRIM$(Model$(NewChoice))

    RETURN
END SUB

SUB BarMenu
    BackChar$ = "±"
    PercentageChar$ = "²"
    Fore = 7
    Back = 0
    Dur = 3
    GetDirectory
    GenerateRandomFile
    WindowInitialize
    Intro

    choice = 2

    DO
        SELECT CASE choice
            CASE 1
                AboutMenu
            CASE 2
                FileMenu
            CASE 3
                EditMenu
            CASE 4
                PrintMenu
            CASE 5
                OptionsMenu
        END SELECT
      
        SELECT CASE subchoice
            CASE -2
                choice = choice - 1
                IF choice = 0 THEN choice = 5
            CASE -3
                choice = choice + 1
                IF choice = 6 THEN choice = 1
        END SELECT
    LOOP
END SUB

FUNCTION Browse% (currItem, X, Y, L, H, Shadow, ShadChar$, NumOfFields, Locs(), Title$(), NumOfInfo, WindowTitle$)
    BrowseC1 = ManipulateC1
    BrowseC2 = ManipulateC2

    DIM BInfo$(6, 20)

    DT = Dur
    Dur = 3

    BrowseWin = ExplodeWindow(X, Y, L, H, BrowseC1, BrowseC2, DoubleFrame, WindowTitle$, Shadow, ShadChar$)
    redraw = 0
    WinPrint BrowseWin, 1, Locs(1), Title$(1)
    FOR t = 2 TO NumOfFields
        WinPrint BrowseWin, 1, Locs(t), "³" + Title$(t)
    NEXT
    WinPrint BrowseWin, 2, 1, STRING$(L, 196)
    FOR t = 2 TO NumOfFields
        WinPrint BrowseWin, 2, Locs(t), "Å"
    NEXT
    WinRedraw BrowseWin

    FirstItem = 1
    IF currItem > NumOfInfo OR currItem < 1 THEN currItem = 1
    IF currItem > H - 2 THEN
        FirstItem = currItem
        currItem = 1
    END IF

    BrowseWin1 = MakeWindow(X + 2, Y, L, H - 2, BrowseC1, BrowseC2, "", "", 0, "")

    redraw = 0
    BrowseWin2 = MakeWindow(X + 1 + currItem, Y, L, 1, BrowseC2, BrowseC1, "", "", 0, "")
    redraw = 1

    GOSUB ShowItems

    finished = FALSE
    DO UNTIL finished = TRUE
        GOSUB ShowBar
        GetKey Kbd$

        SELECT CASE Kbd$
            CASE CHR$(0) + "H"
                currItem = currItem - 1
                IF currItem = 0 THEN
                    FirstItem = FirstItem - 1
                    IF FirstItem = 0 THEN FirstItem = 1
                    currItem = 1
                    GOSUB ShowItems
                END IF

            CASE CHR$(0) + "P"
                currItem = currItem + 1
                IF ((currItem + FirstItem) - 1) > NumOfInfo THEN currItem = currItem - 1
                IF currItem = H - 1 THEN
                    FirstItem = FirstItem + 1
                    currItem = H - 2
                    GOSUB ShowItems
                END IF

            CASE CHR$(0) + "I"
                FirstItem = FirstItem - (H - 2)
                IF FirstItem < 1 THEN FirstItem = 1: currItem = 1
                GOSUB ShowItems

            CASE CHR$(0) + "Q"
                FirstItem = FirstItem + (H - 2)
                IF FirstItem + currItem - 1 > NumOfInfo THEN FirstItem = NumOfInfo: currItem = 1
                GOSUB ShowItems

            CASE CHR$(27)
                Browse% = -1
                finished = TRUE

            CASE CHR$(13)
                Browse% = ((currItem + FirstItem) - 1)
                finished = TRUE

            CASE "P", "p"
                Browse% = ((currItem + FirstItem) - 1)
                finished = TRUE

            CASE "A", "a"
                Browse% = ((currItem + FirstItem) - 1)
                finished = TRUE

            CASE ELSE
                SOUND 1900, .2
        END SELECT
    LOOP

    redraw = 0
    CloseWindow BrowseWin2
    CloseWindow BrowseWin1
    CloseWindow BrowseWin
    redraw = 1
    RedrawAll

    Dur = DT
    Move$ = UCASE$(Kbd$)
    ShowMessage "Inventory Manager  (c)Copyright 1993 by Nathan Thomas"
    ERASE BInfo$
    EXIT FUNCTION

    ShowItems:
    redraw = 0
    Handle(BrowseWin1).Buffer = ""
   
    OPEN Directory$ + RanFile$ FOR RANDOM AS #1 LEN = EquipLength
    FOR count = 1 TO H - 2
        GET #1, FirstItem + count - 1, Equip
        BInfo$(1, count) = Equip.SerialNumber
        BInfo$(2, count) = Equip.Item
        BInfo$(3, count) = Equip.Model
        BInfo$(4, count) = Equip.Department
        BInfo$(5, count) = Equip.Location
        BInfo$(6, count) = Equip.Note1
           
        FOR count1 = 1 TO NumOfFields
            IF count1 <> 1 THEN
                WinPrint BrowseWin1, count, Locs(count1), "³" + BInfo$(count1, count)
            ELSE
                WinPrint BrowseWin1, count, Locs(count1), BInfo$(count1, count)
            END IF
        NEXT
             
        IF FirstItem + count - 1 >= NumOfInfo THEN EXIT FOR
    NEXT
    CLOSE #1
    redraw = 1
    WinRedraw BrowseWin1
    RETURN

    ShowBar:
    Handle(BrowseWin2).Buffer = ""
    Handle(BrowseWin2).XPos = X + 1 + currItem
    WinRedraw BrowseWin1
    FOR count1 = 1 TO NumOfFields
        IF count1 <> 1 THEN
            WinPrint BrowseWin2, 1, Locs(count1), "³" + BInfo$(count1, currItem)
        ELSE
            WinPrint BrowseWin2, 1, Locs(count1), BInfo$(count1, currItem)
        END IF
    NEXT
    ShowMessage "(" + LTRIM$(STR$(FirstItem + currItem - 1)) + " of" + STR$(NumOfInfo) + " R#" + LTRIM$(RTRIM$(BInfo$(6, currItem))) + ")"
    RETURN
END FUNCTION

SUB CalcMax
    OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
    MAXRECS = LOF(1) / EquipLength
    CLOSE #1
END SUB

SUB ChangeDir
    DirWin = ExplodeWindow(8, 20, 46, 2, ManipulateC1, ManipulateC2, DoubleFrame, "", 1, "")
      
    WinPrint DirWin, 1, 1, "Enter the new directory location of the files"
    WinPrint DirWin, 2, 1, "Directory:"

    KILL Directory$ + RanFile$

    IF Directory$ <> "" THEN
        Directory$ = LEFT$(Directory$, LEN(Directory$) - 1)
    END IF
   
    Kbd$ = GetString$(DirWin, 2, 11, Directory$, Directory$, 35, 64)

    Directory$ = UCASE$(RTRIM$(Directory$))

    IF RIGHT$(Directory$, 1) <> "\" AND Directory$ <> "" THEN
        Directory$ = Directory$ + "\"
    END IF

    ClearTemp
    CloseWindow DirWin
END SUB

SUB ChangeRecord

    finished = FALSE
    DO UNTIL finished = TRUE
        MoveFile

        Sort 6, SortField, MAXRECS

        flag1 = Browse(flag1, 3, 5, 69, 15, 1, " ", 5, Locs(), Title$(), MAXRECS, "Change a Record - <ENTER> to change")

        IF flag1 = -1 THEN
            finished = TRUE
        END IF
         
        IF finished = FALSE THEN
            OPEN Directory$ + RanFile$ FOR RANDOM AS #1 LEN = EquipLength
            GET #1, flag1, Equip
            CLOSE #1
            flag = VAL(Equip.Note1)


            IF ASC(Equip.SerialNumber) <> 255 THEN
                OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                GET #1, flag, Equip
                CLOSE #1

                IF RTRIM$(Equip.Note2) <> "LOCKED" THEN
                    OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                    PLock flag
                    CLOSE #1
                    Create$ = RTRIM$(UCASE$(Equip.Note1))

                    IF IDNum$ = Create$ OR IDNum$ = "MANAGER" THEN
                        Serial$ = Equip.SerialNumber
                        Item$ = RTRIM$(Equip.Item)
                        IF ASC(Item$) = 255 THEN Item$ = ""
                        Model$ = RTRIM$(Equip.Model)
                        IF ASC(Model$) = 255 THEN Model$ = ""
                        Department$ = RTRIM$(Equip.Department)
                        IF ASC(Department$) = 255 THEN Department$ = ""
                        Location$ = RTRIM$(Equip.Location)
                        IF ASC(Location$) = 255 THEN Location$ = ""
                        Create$ = RTRIM$(UCASE$(Equip.Note1))
          
                        ChangeWin = ExplodeWindow(3, 2, 74, 9, ManipulateC1, ManipulateC2, DoubleFrame, "Change an Item", 1, "")
                        WinPrint ChangeWin, 2, 1, "Serial Number:                    Item Description:____________________"
                        WinPrint ChangeWin, 3, 1, STRING$(74, 196)
                        WinPrint ChangeWin, 4, 1, "Model:__________    Department:__________    Location:__________"
                        WinPrint ChangeWin, 5, 1, STRING$(74, 196)
                        WinPrint ChangeWin, 6, 1, "TAB        to move forward in fields       <ESC>   to abort input"
                        WinPrint ChangeWin, 7, 1, "BACKTAB    to move backward in fields      <ENTER> to accept information"
                        WinPrint ChangeWin, 8, 1, STRING$(74, 196)
                        WinPrintCenter ChangeWin, 9, "Type a '?' in the Department field for a list of Departments"

                        WinPrint ChangeWin, 2, 15, UCASE$(Serial$)
                        WinPrint ChangeWin, 2, 52, Item$
                        WinPrint ChangeWin, 4, 7, Model$
                        WinPrint ChangeWin, 4, 32, Department$
                        WinPrint ChangeWin, 4, 55, Location$
          
                        place = 1
           
                        DO
                            IF place = 1 THEN
                                Kbd$ = GetString$(ChangeWin, 2, 52, Item$, Item$, 20, 20)
                                Equip.Item = Item$
                                IF Move$ = "TAB" THEN place = 2
                                IF Move$ = "BKTAB" THEN place = 4
                                IF Move$ = "ESC" THEN EXIT DO
                                IF Move$ = "RET" THEN EXIT DO
                            END IF
                            IF place = 2 THEN
                                Kbd$ = GetString$(ChangeWin, 4, 7, Model$, Model$, 10, 10)
                                IF Move$ = "TAB" THEN place = 3
                                IF Move$ = "BKTAB" THEN place = 1
                                IF Move$ = "ESC" THEN EXIT DO
                                IF Move$ = "RET" THEN EXIT DO
                            END IF
                            IF place = 3 THEN
                                Kbd$ = GetString$(ChangeWin, 4, 32, Department$, Department$, 10, 10)
                                IF RTRIM$(Department$) = "?" THEN
                                    GOSUB ShowChangeChoices
                                END IF
                                IF Move$ = "TAB" THEN place = 4
                                IF Move$ = "BKTAB" THEN place = 2
                                IF Move$ = "ESC" THEN EXIT DO
                                IF Move$ = "RET" THEN EXIT DO
                            END IF
                            IF place = 4 THEN
                                Kbd$ = GetString$(ChangeWin, 4, 55, Location$, Location$, 10, 10)
                                IF Move$ = "TAB" THEN place = 1
                                IF Move$ = "BKTAB" THEN place = 3
                                IF Move$ = "ESC" THEN EXIT DO
                                IF Move$ = "RET" THEN EXIT DO
                            END IF
                        LOOP
               
                        OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                        FOR t = 1 TO LOF(1) / EquipLength
                            GET #1, t, Equip
                            IF RTRIM$(Equip.SerialNumber) = RTRIM$(Serial$) THEN
                                flag = t
                                EXIT FOR
                            END IF
                        NEXT
              
                        IF Move$ = "RET" THEN
                            Equip.SerialNumber = Serial$
                            Equip.Item = Item$
                            Equip.Model = Model$
                            Equip.Department = Department$
                            Equip.Location = Location$
                            Equip.Department = UCASE$(Department$)
                            PUT #1, flag, Equip
                        END IF
                   
                        PUnlock flag
                        CLOSE #1
              
                        CloseWindow ChangeWin
                    ELSE
                        ErrWin = ExplodeWindow(8, 5, 41, 5, 15, 4, DoubleFrame, "ERROR!", 1, "")
                        WinPrintCenter ErrWin, 1, "Your ID number does not match up with"
                        WinPrintCenter ErrWin, 2, "the ID number that created this item."
                        WinPrintCenter ErrWin, 3, "Therefore, YOU CANNOT CHANGE THIS ITEM!"
                        WinPrintCenter ErrWin, 5, "<Press any key to continue>"
                        OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                        PUnlock flag
                        CLOSE #1
                        GetKey ""
                        CloseWindow ErrWin
                    END IF
                ELSEIF RTRIM$(Equip.Note2) = "LOCKED" THEN
                    ErrWin = ExplodeWindow(7, 15, 50, 4, 15, 4, DoubleFrame, "ERROR!", 1, "")
                    WinPrintCenter ErrWin, 1, "You cannot change this record at this time."
                    WinPrintCenter ErrWin, 2, "Another user is accessing it."
                    WinPrintCenter ErrWin, 4, "<Press any key to continue>"
                    GetKey ""
                    CloseWindow ErrWin
                    CLOSE #1
                END IF
            ELSE
                ErrWin = ExplodeWindow(7, 15, 50, 3, 15, 4, DoubleFrame, "ERROR!", 1, "")
                WinPrintCenter ErrWin, 1, "You cannot change a nonexistent item!"
                WinPrintCenter ErrWin, 3, "<Press any key to continue>"
                OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                PUnlock flag
                CLOSE #1
                GetKey ""
                CloseWindow ErrWin
            END IF
        END IF
    LOOP
    EXIT SUB

    ShowChangeChoices:
    FOR t = 1 TO MAXRECS
        Model$(t) = ""
    NEXT
    ShowFlag = 0

    Serial$ = RTRIM$(Equip.SerialNumber)
    Item$ = RTRIM$(Equip.Item)
    Model$ = RTRIM$(Equip.Model)
    Department$ = RTRIM$(Equip.Department)
    Location$ = RTRIM$(Equip.Location)
    Create$ = RTRIM$(UCASE$(Equip.Note1))
             

    NumberOfChoices = 0
    OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
    num.recs = LOF(1) / EquipLength
    FOR count = 1 TO num.recs
        GET #1, count, Equip
        IF ASC(Equip.Department) <> 255 THEN
            CurrentInfo$ = UCASE$(RTRIM$(Equip.Department))
            FOR t = 1 TO NumberOfChoices
                IF CurrentInfo$ = Model$(t) THEN
                    ShowFlag = 1
                    EXIT FOR
                END IF
            NEXT
            IF ShowFlag = 0 THEN
                ShowFlag = 0
                Model$(NumberOfChoices + 1) = CurrentInfo$
                NumberOfChoices = NumberOfChoices + 1
            ELSE
                ShowFlag = 0
            END IF
        END IF
    NEXT
    CLOSE #1

    Equip.SerialNumber = Serial$
    Equip.Item = Item$
    Equip.Model = Model$
    Equip.Location = Location$
    Equip.Note1 = Create$

    IF NumberOfChoices > 18 THEN NumberOfChoices = 18

    Model$(NumberOfChoices + 1) = "None!"

    NewChoice = Menu(NewChoice, 4, 4, 10, MenuC1, MenuC2, Model$(), Hot$, NumberOfChoices + 1)

    Move$ = ""

    IF NewChoice = NumberOfChoices + 1 OR NewChoice < 1 THEN Department$ = "": RETURN

    Department$ = RTRIM$(Model$(NewChoice))

    RETURN
END SUB

SUB ChangeSortField
    DIM Item$(6)

    Item$(1) = " Serial Number"
    Item$(2) = " Item Description"
    Item$(3) = " Model Number"
    Item$(4) = " Department"
    Item$(5) = " Location"
    Item$(6) = " Quit!"
    Hot$ = "SIMDLQ"

    NewChoice = SortField

    NewChoice = Menu(NewChoice, 7, 10, 19, MenuC1, MenuC2, Item$(), Hot$, 6)

    IF NewChoice > 5 OR NewChoice < 1 THEN
        EXIT SUB
    ELSEIF NewChoice < 6 THEN
        SortField = NewChoice
    END IF

    OPEN Directory$ + LEFT$(IDNum$, 8) + ".INI" FOR OUTPUT AS #1
    PRINT #1, SortField
    CLOSE #1

END SUB

SUB ClearKey
    DO UNTIL INKEY$ = "": LOOP
END SUB

SUB ClearRandom
    KILL Directory$ + "INVENRAN.DAT"
   
    OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
    FOR count = 1 TO STARTMAX
        GET #1, count, Equip
        Equip.SerialNumber = CHR$(255)
        Equip.Item = CHR$(255)
        Equip.Model = CHR$(255)
        Equip.Department = CHR$(255)
        Equip.Location = CHR$(255)
        Equip.Note1 = CHR$(255)
        Equip.Note2 = CHR$(255)
        PUT #1, count, Equip
    NEXT
    CLOSE #1
END SUB

SUB ClearTemp
    OPEN Directory$ + RanFile$ FOR RANDOM AS #1 LEN = EquipLength
    FOR count = 1 TO MAXRECS
        GET #1, count, Equip
        IF ASC(Equip.SerialNumber) <> 255 THEN
            Equip.SerialNumber = CHR$(255)
            Equip.Item = CHR$(255)
            Equip.Model = CHR$(255)
            Equip.Department = CHR$(255)
            Equip.Location = CHR$(255)
            Equip.Note1 = CHR$(255)
            Equip.Note2 = CHR$(255)
            PUT #1, count, Equip
        END IF
    NEXT
    CLOSE #1
END SUB

SUB CloseAll
    FOR i = 1 TO WinCount
        CloseWindow WinCount
    NEXT
END SUB

SUB CloseWindow (WinNumber)
    IF WinNumber = 0 THEN EXIT SUB
    IF WinNumber > WinCount THEN EXIT SUB
    FOR count = WinNumber TO WinCount
        Handle(count) = Handle(count + 1)
    NEXT
    WinCount = WinCount - 1
    IF redraw = 1 THEN RedrawAll
END SUB

SUB DeleteRecord
    DO UNTIL finished = TRUE
        MoveFile

        Sort 6, SortField, MAXRECS

        flag1 = Browse(flag1, 3, 5, 69, 15, 1, " ", 5, Locs(), Title$(), MAXRECS, "Delete a Record - <ENTER> to Delete")
  
        IF flag1 = -1 THEN
            finished = TRUE
        END IF

        IF finished = FALSE THEN
            OPEN Directory$ + RanFile$ FOR RANDOM AS #1 LEN = EquipLength
            GET #1, flag1, Equip
            CLOSE #1
            flag = VAL(Equip.Note1)

            IF ASC(Equip.SerialNumber) <> 255 AND Move$ = CHR$(13) THEN
                OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                GET #1, flag, Equip
                CLOSE #1
                Create$ = RTRIM$(UCASE$(Equip.Note1))

                IF RTRIM$(Equip.Note2) <> "LOCKED" THEN
                    OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                    PLock flag
                    GET #1, flag, Equip
                    CLOSE #1
                    IF IDNum$ = Create$ OR IDNum$ = "MANAGER" THEN
                  
                        AskWin = ExplodeWindow(6, 7, 20, 1, InfoC1, InfoC2, DoubleFrame, "Delete", 1, "")
                        WinPrintCenter AskWin, 1, "Are you sure? "

                        Ask$ = ""
                        DO WHILE UCASE$(Ask$) <> "Y" AND UCASE$(Ask$) <> "N"
                            Kbd$ = GetString$(AskWin, 1, 17, "", Ask$, 1, 1)
                            Ask$ = UCASE$(Ask$)
                            OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                            FOR t = 1 TO LOF(1) / EquipLength
                                GET #1, t, Equip
                                IF RTRIM$(Equip.SerialNumber) = RTRIM$(Serial$) THEN
                                    flag = t
                                    EXIT FOR
                                END IF
                            NEXT
                                   
                            IF Ask$ = "Y" THEN
                                Equip.SerialNumber = CHR$(255)
                                Equip.Item = CHR$(255)
                                Equip.Model = CHR$(255)
                                Equip.Department = CHR$(255)
                                Equip.Location = CHR$(255)
                                Equip.Note1 = CHR$(255)
                                Equip.Note2 = ""
                                PUT #1, flag, Equip
                            ELSEIF Ask$ = "N" THEN
                                PUnlock flag
                            END IF
                               
                            CLOSE #1
                        LOOP
                           
                        CloseWindow AskWin
                    ELSE
                        ErrWin = ExplodeWindow(8, 5, 41, 5, 15, 4, DoubleFrame, "ERROR!", 1, "")
                        WinPrintCenter ErrWin, 1, "Your ID number does not match up with"
                        WinPrintCenter ErrWin, 2, "the ID number that created this item."
                        WinPrintCenter ErrWin, 3, "Therefore, YOU CANNOT DELETE THIS ITEM!"
                        WinPrintCenter ErrWin, 5, "<Press any key to continue>"
                        OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                        PUnlock flag
                        CLOSE #1
                        GetKey ""
                        CloseWindow ErrWin
                    END IF
                ELSEIF RTRIM$(Equip.Note2) = "LOCKED" THEN
                    ErrWin = ExplodeWindow(7, 15, 50, 4, 15, 4, DoubleFrame, "ERROR!", 1, "")
                    WinPrintCenter ErrWin, 1, "You cannot change this record at this time."
                    WinPrintCenter ErrWin, 2, "Another user is accessing it."
                    WinPrintCenter ErrWin, 4, "<Press any key to continue>"
                    GetKey ""
                    CloseWindow ErrWin
                END IF
            ELSEIF ASC(Equip.SerialNumber) = 255 THEN
                ErrWin = ExplodeWindow(7, 15, 50, 3, 15, 4, DoubleFrame, "ERROR!", 1, "")
                WinPrintCenter ErrWin, 1, "You cannot delete a nonexistent item!"
                WinPrintCenter ErrWin, 3, "<Press any key to continue>"
                OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                PUnlock flag
                CLOSE #1
                GetKey ""
                CloseWindow ErrWin
            END IF
        END IF
    LOOP
END SUB

SUB DrawFrame (WinNumber)
    X = Handle(WinNumber).XPos
    Y = Handle(WinNumber).YPos
    L = Handle(WinNumber).Length
    H = Handle(WinNumber).Height
    F = Handle(WinNumber).Foreground
    B = Handle(WinNumber).Background
    Frame$ = Handle(WinNumber).Frame

    IF X = 0 OR Y = 0 OR L = 0 OR H = 0 OR RTRIM$(Frame$) = "" THEN EXIT SUB

    COLOR F, B
    LOCATE X, Y
    PRINT LEFT$(Frame$, 1); STRING$(L, RIGHT$(Frame$, 1)); MID$(Frame$, 2, 1);
    FOR count = 1 TO H
        LOCATE X + count, Y
        PRINT MID$(Frame$, 5, 1);
        LOCATE X + count, Y + L + 1
        PRINT MID$(Frame$, 5, 1);
    NEXT
    LOCATE X + count, Y
    PRINT MID$(Frame$, 3, 1); STRING$(L, RIGHT$(Frame$, 1)); MID$(Frame$, 4, 1);
END SUB

SUB DrawShadow (WinNumber)
    X = Handle(WinNumber).XPos
    Y = Handle(WinNumber).YPos
    F = Handle(WinNumber).Foreground
    B = Handle(WinNumber).Background
    L = Handle(WinNumber).Length
    H = Handle(WinNumber).Height
    Shadow = Handle(WinNumber).Shadow
    Shad$ = Handle(WinNumber).ShadChar
       
    IF Shadow = 1 THEN
        COLOR 7, 0
        YY = Y + L + 2
        XX = X
        FOR cnt = 1 TO H + 1
            LOCATE XX + cnt, YY
            PRINT STRING$(2, Shad$);
        NEXT
        IF X + H + 2 <= 25 THEN LOCATE X + H + 2, Y + 2: PRINT STRING$(L + 2, Shad$);
        COLOR F, B
    END IF
END SUB

SUB DrawTitle (WinNumber)
    X = Handle(WinNumber).XPos
    Y = Handle(WinNumber).YPos
    L = Handle(WinNumber).Length
    F = Handle(WinNumber).Foreground
    B = Handle(WinNumber).Background
    Title$ = Handle(WinNumber).Title
    IF RTRIM$(Title$) = "" THEN EXIT SUB

    COLOR F, B

    IF RTRIM$(Title$) <> "" AND LEN(RTRIM$(Title$)) < (L - 2) THEN
        YY = (INT(L / 2)) - (INT(LEN(RTRIM$(Title$)) / 2))
        LOCATE X, Y + YY: PRINT " "; RTRIM$(Title$); " ";
    END IF
END SUB

SUB EditMenu
    DIM MenuItem$(6)
  
    MenuItem$(1) = " Enter new Item..."
    MenuItem$(2) = " Delete Item(s)..."
    MenuItem$(3) = " Change Item(s)..."
    MenuItem$(4) = "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
    MenuItem$(5) = " Browse through specified Items..."
    MenuItem$(6) = " Browse through all Items"
    Hot$ = "EDC SA"

    DO UNTIL NewChoice < 0
        COLOR MenuC2, MenuC1
        LOCATE 1, 11
        PRINT " Edit "

        NewChoice = Menu(NewChoice, 2, 10, 36, MenuC1, MenuC2, MenuItem$(), Hot$, 6)

        SELECT CASE NewChoice
            CASE 1
                AddRecord
            CASE 2
                DeleteRecord
            CASE 3
                ChangeRecord
            CASE 5
                SelectBrowse
            CASE 6
                ShowAll
            CASE -2
                subchoice = -2
            CASE -3
                subchoice = -3
            CASE ELSE
        END SELECT
    LOOP
END SUB

SUB Ending
    WinRedraw 0
    FinalWindow = ExplodeWindow(5, 20, 40, 9, MenuC1, MenuC2, DoubleFrame, "Goodbye!", 1, "")
    WinPrintCenter FinalWindow, 1, "Thank you for using the"
    WinPrintCenter FinalWindow, 2, "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
    WinPrintCenter FinalWindow, 3, "³                                   ³"
    WinPrintCenter FinalWindow, 4, "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
    WinPrintCenter FinalWindow, 5, "Your records are safe with us!"
    WinPrintCenter FinalWindow, 8, "(c)Copyright 1993 by Nathan Thomas"
    WinPrintCenter FinalWindow, 9, "All Rights Reserved"
    FinalWindow1 = MakeWindow(7, 23, 35, 1, ManipulateC2, ManipulateC1, "", "", 0, "")
    WinPrintCenter FinalWindow1, 1, "I N V E N T O R Y   M A N A G E R"
    SLEEP 4
    redraw = 0
    CloseAll
    ClearKey
    COLOR 7, 0
    CLS
    KILL Directory$ + RanFile$
    END
END SUB

FUNCTION ExplodeWindow% (X, Y, L, H, Fore, Back, Frame$, Title$, Shadow, ShadChar$)
    Dur = INT(Dur)
    IF Dur < 1 THEN Dur = 1
    WinCount = WinCount + 1
    Handle(WinCount).XPos = X
    Handle(WinCount).YPos = Y
    Handle(WinCount).Length = L
    Handle(WinCount).Height = H
    Handle(WinCount).Foreground = Fore
    Handle(WinCount).Background = Back
    Handle(WinCount).Frame = Frame$
    Handle(WinCount).Title = Title$
    Handle(WinCount).Shadow = Shadow
    Handle(WinCount).ShadChar = ShadChar$
    Handle(WinCount).Buffer = ""
 
    COLOR Fore, Back
    L1 = 0
    H1 = 0
    X1 = X + INT(H / 2) + 1
    Y1 = Y + INT(L / 2)
    DO
        LOCATE X1, Y1
        PRINT LEFT$(Frame$, 1); STRING$(L1, RIGHT$(Frame$, 1)); MID$(Frame$, 2, 1);
        FOR count = 1 TO H1
            LOCATE X1 + count, Y1
            PRINT MID$(Frame$, 5, 1); SPACE$(L1); MID$(Frame$, 5, 1);
        NEXT
        LOCATE X1 + count, Y1
        PRINT MID$(Frame$, 3, 1); STRING$(L1, RIGHT$(Frame$, 1)); MID$(Frame$, 4, 1);

        IF L1 = L AND H1 = H AND X1 = X AND Y1 = Y THEN EXIT DO

        IF L1 + (Dur * 2) > L THEN
            L1 = L
        ELSE
            L1 = L1 + (Dur * 2)
        END IF

        IF Y1 - Dur < Y THEN
            Y1 = Y
        ELSE
            Y1 = Y1 - Dur
        END IF

        IF H1 + Dur > H THEN
            H1 = H
        ELSE
            H1 = H1 + Dur
        END IF

        IF X1 - Dur < X THEN
            X1 = X
        ELSE
            X1 = X1 - Dur
        END IF
        FOR t = 1 TO 500: NEXT
    LOOP
    ExplodeWindow% = WinCount
    DrawTitle WinCount
    DrawShadow WinCount
END FUNCTION

SUB FileMenu
    DIM MenuItem$(6)

    MenuItem$(1) = " Clear Data File"
    MenuItem$(2) = "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
    MenuItem$(4) = "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
    MenuItem$(5) = " DOS Shell"
    MenuItem$(6) = " Exit"
    Hot$ = "C D SX"

    DO UNTIL NewChoice < 0
        DD$ = LEFT$(Directory$ + "                    ", 20)
        IF LEN(DD$) < LEN(Directory$) THEN
            DD$ = LEFT$(DD$, 19) + CHR$(16)
        END IF

        MenuItem$(3) = " Directory      " + DD$

        COLOR MenuC2, MenuC1
        LOCATE 1, 5
        PRINT " File "

        NewChoice = Menu(NewChoice, 2, 4, 38, MenuC1, MenuC2, MenuItem$(), Hot$, 6)

        SELECT CASE NewChoice
            CASE 1
                ClearRandom
            CASE 3
                ChangeDir
            CASE 5
                COLOR 7, 0
                CLS
                COLOR MenuC1, MenuC2
                LOCATE 8, 1: PRINT "Type EXIT to return to the Inventory Manager"
                SHELL
                LOCATE 25, 30: PRINT "Press any key to return to the INVENTORY MANAGER...";
                GetKey ""
                RedrawAll
            CASE 6
                CloseAll
                Ending
            CASE -2
                subchoice = -2
            CASE -3
                subchoice = -3
            CASE ELSE
        END SELECT
    LOOP
END SUB

SUB GenerateRandomFile
    RANDOMIZE TIMER
    FOR t = 1 TO 8
        cc = INT(RND(1) * 25) + 65
        RanFile$ = RanFile$ + CHR$(cc)
        PRINT CHR$(cc);
    NEXT
    RanFile$ = RanFile$ + ".TMP"
END SUB

SUB GetDirectory
    SHELL "cd > CURRDIR"
    OPEN "CURRDIR" FOR INPUT AS #1
    LINE INPUT #1, Directory$
    CLOSE #1
    KILL "CURRDIR"

    IF RIGHT$(Directory$, 1) <> "\" AND Directory$ <> "" THEN
        Directory$ = UCASE$(Directory$) + "\"
    END IF
END SUB

SUB GetKey (Char$)
    ClearKey

    Char$ = ""
    DO UNTIL Char$ <> ""
        Char$ = INKEY$
    LOOP
END SUB

FUNCTION GetString$ (WinNumber, row, col, Start$, end$, Vis, MAX)
    Move$ = ""
    IF WinNumber > WinCount THEN EXIT FUNCTION
    IF WinNumber = 0 THEN EXIT FUNCTION
           
    X = Handle(WinNumber).XPos
    Y = Handle(WinNumber).YPos
    F = Handle(WinNumber).Foreground
    B = Handle(WinNumber).Background
    row = row + X
    col = col + Y
   
    curr$ = LEFT$(Start$, MAX)
    IF curr$ = CHR$(8) THEN curr$ = ""

    LOCATE , , 1
    COLOR F, B

    finished = FALSE
    DO
        GOSUB GetStringShowText
        Kbd$ = ""
        GetKey Kbd$
        LOCATE , , 1

        SELECT CASE Kbd$
            CASE CHR$(0) + CHR$(15)
                Move$ = "BKTAB"
                finished = TRUE

            CASE CHR$(9)
                Move$ = "TAB"
                finished = TRUE

            CASE CHR$(27)
                Move$ = "ESC"
                finished = TRUE

            CASE CHR$(13)
                Move$ = "RET"
                finished = TRUE
                GetString$ = Kbd$

            CASE CHR$(27)
                finished = TRUE
                GetString$ = Kbd$
                Move$ = Kbd$
             
            CASE CHR$(8)
                IF curr$ <> "" THEN
                    curr$ = LEFT$(curr$, LEN(curr$) - 1)
                ELSE
                    SOUND 1900, .2
                END IF

            CASE " " TO CHR$(255)
                IF LEN(curr$) < MAX THEN
                    curr$ = curr$ + Kbd$
                ELSE
                    SOUND 1900, .2
                END IF
             
            CASE ELSE
                SOUND 1900, .2
        END SELECT
   
    LOOP UNTIL finished

    end$ = curr$

    LOCATE , , 0
    COLOR F, B
    EXIT FUNCTION

    GetStringShowText:
    LOCATE row, col, 0
    COLOR F, B
    IF LEN(curr$) > Vis THEN
        WinPrint WinNumber, row - X, col - Y, RIGHT$(curr$, Vis)
    ELSE
        WinPrint WinNumber, row - X, col - Y, curr$ + STRING$(Vis - LEN(curr$), "_")
        LOCATE row, col + LEN(curr$)
    END IF
    LOCATE , , 1
    RETURN
END FUNCTION

SUB Intro
    FirstWin = ExplodeWindow(4, 18, 47, 9, 0, 7, SingleFrame, "Welcome!", 1, "")
    WinPrintCenter FirstWin, 2, "Welcome to Inventory Manager"
    WinPrintCenter FirstWin, 4, "Copyright (C) Nathan Thomas, 1993."
    WinPrintCenter FirstWin, 5, "All Rights Reserved."
    WinPrintCenter FirstWin, 7, STRING$(47, 196)
    WinPrintCenter FirstWin, 8, "Developed for NorthWest High School"
    WinPrintCenter FirstWin, 9, "Programmed by Nathan Thomas"


    InputWin = ExplodeWindow(16, 35, 34, 1, ManipulateC1, ManipulateC2, DoubleFrame, "", 1, "")
    WinPrint InputWin, 1, 3, "Enter your login ID:__________"

    DO UNTIL IDNum$ <> "" AND Pass$ = "INVENTORY"
        IDNum$ = UCASE$(ENVIRON$("ID"))
        Kbd$ = GetString$(InputWin, 1, 23, IDNum$, IDNum$, 10, 10)
        IDNum$ = UCASE$(RTRIM$(IDNum$))

        IF IDNum$ = "MANAGER" THEN
            PassWin = ExplodeWindow(17, 38, 34, 1, MenuC1, MenuC2, DoubleFrame, "", 1, "")
            WinPrint PassWin, 1, 3, "Enter the password:_________"
            Kbd$ = GetString$(PassWin, 1, 22, "", Pass$, 10, 10)
            Pass$ = UCASE$(RTRIM$(Pass$))
            IF Pass$ <> "INVENTORY" THEN CloseWindow PassWin
        ELSE
            Pass$ = "INVENTORY"
        END IF
    LOOP
    
    redraw = 0
    CloseAll
    redraw = 1
    RedrawAll

    WinInfo = MakeWindow(24, 0, 80, 1, InfoC1, InfoC2, "", "", 0, "")
    ShowMessage "Inventory Manager  (c)Copyright 1993 by Nathan Thomas"

    BarWin = MakeWindow(0, 0, 80, 1, MenuC1, MenuC2, "", "", 0, "")
    WinPrint BarWin, 1, 1, "  ð  File  Edit  Print  Options"
    WinPrint BarWin, 1, 61, "³Temp: " + RanFile$
    WinPrint BarWin, 1, (60 - LEN(IDNum$)), IDNum$
    SortField = 1

    OPEN Directory$ + LEFT$(IDNum$, 8) + ".INI" FOR INPUT AS #1
    INPUT #1, SortField
    CLOSE #1
END SUB

FUNCTION MakeWindow% (X, Y, L, H, Fore, Back, Frame$, Title$, Shadow, ShadChar$)
    WinCount = WinCount + 1
    Handle(WinCount).XPos = X
    Handle(WinCount).YPos = Y
    Handle(WinCount).Length = L
    Handle(WinCount).Height = H
    Handle(WinCount).Foreground = Fore
    Handle(WinCount).Background = Back
    Handle(WinCount).Frame = Frame$
    Handle(WinCount).Title = Title$
    Handle(WinCount).Shadow = Shadow
    Handle(WinCount).ShadChar = ShadChar$
    Handle(WinCount).Buffer = ""
 
    COLOR Fore, Back
    IF redraw = 1 THEN
        DrawFrame WinCount
        DrawTitle WinCount
        DrawShadow WinCount
        FOR t = 1 TO H
            LOCATE X + t, Y + 1
            PRINT SPACE$(L);
        NEXT
    END IF
    MakeWindow% = WinCount
END FUNCTION

FUNCTION Menu% (currChoice, X, Y, Length, Fg, Bg, Item$(), Hot$, NumOfItems)

    Hot$ = UCASE$(Hot$)
    IF RTRIM$(Hot$) <> "" THEN Length = Length + 1

    Height = NumOfItems

    MenuWin = MakeWindow(X, Y, Length, Height, Fg, Bg, DoubleFrame, "", 1, "")
    FOR t = 1 TO NumOfItems
        Item$(t) = LEFT$(Item$(t) + SPACE$(Length), Length)
        IF RTRIM$(Hot$) <> "" AND MID$(Hot$, t, 1) <> " " THEN
            Item$(t) = LEFT$(Item$(t), Length - 1) + MID$(Hot$, t, 1) + " "
        ELSEIF LEFT$(Item$(t), 1) = "Ä" THEN
            Item$(t) = LEFT$(Item$(t), Length - 1) + "Ä"
        END IF
        WinPrint MenuWin, t, 1, Item$(t)
    NEXT

    IF currChoice = 0 THEN currChoice = 1

    redraw = 0
    MenuWin1 = MakeWindow(X + currChoice - 1, Y, Length, 1, Bg, Fg, "", "", 0, "")
    redraw = 1

    finished = FALSE
    DO UNTIL finished = TRUE
        WinRedraw MenuWin
        WinPrint MenuWin1, 1, 1, Item$(currChoice)

        GetKey Kbd$
        Kbd$ = UCASE$(Kbd$)

        SELECT CASE Kbd$
            CASE CHR$(0) + "H"
                currChoice = currChoice - 1
                IF currChoice = 0 THEN currChoice = NumOfItems
                IF LEFT$(Item$(currChoice), 1) = "Ä" OR LEFT$(Item$(currChoice), 1) = "Í" THEN
                    currChoice = currChoice - 1
                END IF

            CASE CHR$(0) + "P"
                currChoice = currChoice + 1
                IF currChoice = NumOfItems + 1 THEN currChoice = 1
                IF LEFT$(Item$(currChoice), 1) = "Ä" OR LEFT$(Item$(currChoice), 1) = "Í" THEN
                    currChoice = currChoice + 1
                END IF

            CASE CHR$(0) + "K"
                currChoice = -2
                finished = TRUE

            CASE CHR$(0) + "M"
                currChoice = -3
                finished = TRUE

            CASE CHR$(13)
                finished = TRUE

            CASE "A" TO "z"
                GOSUB GetHotKey

            CASE ELSE
                SOUND 1900, .2
        END SELECT
       
        Handle(MenuWin1).XPos = X + currChoice - 1
        Handle(MenuWin1).YPos = Y
    LOOP
    redraw = 0
    CloseWindow MenuWin1
    CloseWindow MenuWin
    redraw = 1
    RedrawAll
    Menu% = currChoice
    EXIT FUNCTION

    GetHotKey:
    FOR MB = 1 TO NumOfItems
        IF UCASE$(MID$(Hot$, MB, 1)) = UCASE$(Kbd$) THEN
            finished = TRUE

            IF MB <> currChoice THEN
                Handle(MenuWin1).XPos = X + MB - 1
                Handle(MenuWin1).YPos = Y
                WinRedraw MenuWin
                WinPrint MenuWin1, 1, 1, Item$(MB)
                FOR t = 1 TO 12000: NEXT
            END IF

            currChoice = MB
        END IF
    NEXT
    RETURN

END FUNCTION

SUB MoveFile
    CalcMax
    HoldOn = ExplodeWindow(10, 26, 27, 2, 0, 7, SingleFrame, "", 1, "")
    WinPrintCenter HoldOn, 1, "Updating..."
    WinPrint HoldOn, 2, 3, "[úúúúúúúúúúúúúúú]  0%  "

    KILL Directory$ + RanFile$
  
    st$ = "copy " + Directory$ + "INVENRAN.DAT " + Directory$ + RanFile$ + " > NUL"
  
    LOCATE , , , 16, 16
    SHELL st$
    LOCATE , , , 11, 12

    Interval = INT(MAXRECS / 15)

    OPEN Directory$ + RanFile$ FOR RANDOM AS #1 LEN = EquipLength
    FOR count = 1 TO MAXRECS
        GET #1, count, Equip
        Equip.Note1 = STR$(count)
        Equip.Note2 = ""
        PUT #1, count, Equip
      
        IF count = count1 + Interval THEN
            WinPrint HoldOn, 2, 22, LTRIM$(RTRIM$(STR$(INT(100 * (count / MAXRECS))))) + "%"
            IF NumOfCounts < 15 THEN WinPrint HoldOn, 2, 4 + NumOfCounts, PercentageChar$
            count1 = count
            NumOfCounts = NumOfCounts + 1
        END IF
    NEXT
    WinPrint HoldOn, 2, 3, "[" + STRING$(15, PercentageChar$) + "]  100%"
    CLOSE #1

    CloseWindow HoldOn
END SUB

SUB Optimize
    CalcMax
    KILL Directory$ + RanFile$
  
    st$ = "copy " + Directory$ + "INVENRAN.DAT " + Directory$ + RanFile$ + " > NUL"
  
    LOCATE , , , 16, 16
    SHELL st$
    LOCATE , , , 11, 12

    Sort 7, SortField, MAXRECS

    KILL Directory$ + "INVENRAN.DAT"

    OPEN Directory$ + RanFile$ FOR RANDOM AS #1 LEN = EquipLength
    num.recs = LOF(1) / EquipLength
    OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #2 LEN = EquipLength
    FOR count = 1 TO num.recs
        GET #1, count, Equip
        IF ASC(Equip.SerialNumber) <> 255 THEN
            PUT #2, count, Equip
        END IF
    NEXT
    CLOSE #2
    CLOSE #1
END SUB

SUB OptionsMenu
    DIM MenuItem$(3)

    DO UNTIL NewChoice < 0

        MenuItem$(1) = " Change Sort Field..."
        IF MenuC1 = 15 THEN
            CSys$ = "Color"
        ELSE
            CSys$ = "Monochrome"
        END IF
        MenuItem$(2) = " Toggle Colors    " + RIGHT$("          " + CSys$, 10)
        MenuItem$(3) = " Optimize"
        Hot$ = "CTO"

        IF IDNum$ = "MANAGER" THEN ccs = 3 ELSE ccs = 2

        COLOR MenuC2, MenuC1
        LOCATE 1, 24
        PRINT " Options "

        NewChoice = Menu(NewChoice, 2, 23, 30, MenuC1, MenuC2, MenuItem$(), Hot$, ccs)

        SELECT CASE NewChoice
            CASE 1
                ChangeSortField
            CASE 2
                IF CSys$ = "Color" THEN
                    MenuC1 = 0
                    Handle(2).Foreground = 0
                    MenuC2 = 7
                    Handle(2).Background = 7
                    ManipulateC1 = 0
                    ManipulateC2 = 7
                    InfoC1 = 0
                    Handle(1).Foreground = 0
                    InfoC2 = 7
                    Handle(1).Background = 7
                ELSE
                    MenuC1 = 15
                    Handle(2).Foreground = 15
                    MenuC2 = 4
                    Handle(2).Background = 4
                    ManipulateC1 = 1
                    ManipulateC2 = 3
                    InfoC1 = 7
                    Handle(1).Foreground = 7
                    InfoC2 = 1
                    Handle(1).Background = 1
                END IF
                RedrawAll
            CASE 3
                Optimize
            CASE -2
                subchoice = -2
            CASE -3
                subchoice = -3
            CASE ELSE
        END SELECT
    LOOP
END SUB

SUB PLock (RecordNum)
    GET #1, RecordNum, Equip
    Equip.Note2 = "LOCKED"
    PUT #1, RecordNum, Equip
END SUB

SUB PrintAll
    PrintErr = FALSE
    LPRINT
    IF PrintErr = TRUE THEN EXIT SUB

    ShowMessage "Printing all Records..."
    OPEN Directory$ + RanFile$ FOR RANDOM AS #1 LEN = EquipLength
    FOR count = 1 TO MAXRECS
        GET #1, count, Equip
        flag = VAL(Equip.Note1)
        IF ASC(Equip.SerialNumber) <> 255 THEN
            SendToPrinter flag
        END IF
    NEXT
    CLOSE #1
    ShowMessage "Inventory Manager  (c)Copyright 1993 by Nathan Thomas"
END SUB

SUB PrintMenu
    DIM MenuItem$(2)
  
    MenuItem$(1) = " Print All Records"
    MenuItem$(2) = " Form Feed Printer"
    Hot$ = "PF"

    DO UNTIL NewChoice < 0
        COLOR MenuC2, MenuC1
        LOCATE 1, 17
        PRINT " Print "

        NewChoice = Menu(NewChoice, 2, 16, 20, MenuC1, MenuC2, MenuItem$(), Hot$, 2)

        SELECT CASE NewChoice
            CASE 1
                MoveFile
                PrintAll
            CASE 2
                LPRINT CHR$(12)
                PrintedLines = 0
            CASE -2
                subchoice = -2
            CASE -3
                subchoice = -3
            CASE ELSE
        END SELECT
    LOOP
END SUB

SUB PUnlock (RecordNum)
    GET #1, RecordNum, Equip
    Equip.Note2 = CHR$(255)
    PUT #1, RecordNum, Equip
END SUB

SUB RedrawAll
    FOR WCount = 0 TO WinCount
        WinRedraw WCount
    NEXT
    COLOR 7, 0
END SUB

SUB SelectBrowse
    DIM MenuItem$(4)

    MenuItem$(1) = " Show by Model"
    MenuItem$(2) = " Show by Location"
    MenuItem$(3) = " Show by Department"
    MenuItem$(4) = " Quit!"
    Hot$ = "MLDQ"

    DO UNTIL NewChoice = 4
        ShowFlag = 0
        NewChoice = Menu(NewChoice, 7, 10, 21, MenuC1, MenuC2, MenuItem$(), Hot$, 4)

        ClearTemp

        SELECT CASE NewChoice

            CASE 1
                InputWin = ExplodeWindow(6, 10, 41, 1, InfoC1, InfoC2, DoubleFrame, "Browse Model Type", 1, "")
                WinPrintCenter InputWin, 1, "Enter the model number:          "
                Kbd$ = GetString$(InputWin, 1, 28, "", Model$, 10, 10)
                Model$ = RTRIM$(UCASE$(Model$))
                CloseWindow InputWin
           
                IF Model$ <> "" THEN
                    ShowFlag = 1
                    count = 1
                    ShowMessage "Searching..."
                    CalcMax
                    OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                    OPEN Directory$ + RanFile$ FOR RANDOM AS #2 LEN = EquipLength
                    FOR count1 = 1 TO MAXRECS
                        GET #1, count1, Equip
                        IF RTRIM$(UCASE$(Equip.Model)) = Model$ THEN
                            Equip.Note1 = STR$(count1)
                            PUT #2, count, Equip
                            count = count + 1
                        END IF
                    NEXT
                    CLOSE #2
                    CLOSE #1
                END IF

            CASE 2
                InputWin = ExplodeWindow(6, 10, 41, 1, InfoC1, InfoC2, DoubleFrame, "Browse Location", 1, "")
                WinPrintCenter InputWin, 1, "Enter the Location:          "
                Kbd$ = GetString$(InputWin, 1, 26, "", Location$, 10, 10)
                Location$ = RTRIM$(UCASE$(Location$))
                IF Location$ = "" THEN CloseWindow InputWin: RETURN
                CloseWindow InputWin
       
                IF Location$ <> "" THEN
                    ShowFlag = 1
                    count = 1
                    ShowMessage "Searching..."
                    CalcMax
                    OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                    OPEN Directory$ + RanFile$ FOR RANDOM AS #2 LEN = EquipLength
                    FOR count1 = 1 TO MAXRECS
                        GET #1, count1, Equip
                        IF LEFT$(RTRIM$(UCASE$(Equip.Location)), LEN(Location$)) = Location$ THEN
                            Equip.Note1 = STR$(count1)
                            PUT #2, count, Equip
                            count = count + 1
                        END IF
                    NEXT
                    CLOSE #2
                    CLOSE #1
                END IF
          
            CASE 3
                InputWin = ExplodeWindow(6, 10, 41, 1, InfoC1, InfoC2, DoubleFrame, "Browse Department", 1, "")
                WinPrintCenter InputWin, 1, "Enter the Department:          "
                Kbd$ = GetString$(InputWin, 1, 27, "", Department$, 10, 10)
                Department$ = RTRIM$(UCASE$(Department$))
                IF Department$ = "" THEN CloseWindow InputWin: RETURN
                CloseWindow InputWin
       
                IF Department$ <> "" THEN
                    ShowFlag = 1
                    count = 1
                    ShowMessage "Searching..."
                    CalcMax
                    OPEN Directory$ + "INVENRAN.DAT" FOR RANDOM AS #1 LEN = EquipLength
                    OPEN Directory$ + RanFile$ FOR RANDOM AS #2 LEN = EquipLength
                    FOR count1 = 1 TO MAXRECS
                        GET #1, count1, Equip
                        IF LEFT$(RTRIM$(UCASE$(Equip.Department)), LEN(Department$)) = Department$ THEN
                            Equip.Note1 = STR$(count1)
                            PUT #2, count, Equip
                            count = count + 1
                        END IF
                    NEXT
                    CLOSE #2
                    CLOSE #1
                END IF
       
            CASE 4
                EXIT SUB
            CASE ELSE
                NewChoice = 1

        END SELECT

        ShowMessage "Inventory Manager  (c)Copyright 1993 by Nathan Thomas"
        IF ShowFlag = 1 THEN
            IF choice < 4 AND (count - 1 > 0) THEN
                Sort 6, SortField, count - 1

                DO
                    flag1 = Browse(flag1, 3, 5, 69, 15, 1, "", 5, Locs(), Title$(), count - 1, "Browse through Items - <P> to print record / <A> to print all")
                    IF flag1 = -1 THEN EXIT DO
       
                    IF Move$ = "P" THEN
                        OPEN Directory$ + RanFile$ FOR RANDOM AS #1 LEN = EquipLength
                        GET #1, flag1, Equip
                        CLOSE #1
          
                        IF VAL(Equip.Note1) <> 0 THEN
                            flag = VAL(Equip.Note1)
                            SendToPrinter flag
                        END IF
                    ELSEIF Move$ = "A" THEN
                        PrintAll
                    END IF
                LOOP
            END IF
        END IF
    LOOP
END SUB

SUB SendToPrinter (RecordNum)
    PrintErr = FALSE
   
    OPEN Directory$ + RanFile$ FOR RANDOM AS #1 LEN = EquipLength
    GET #1, RecordNum, Equip
    CLOSE #1

    LPRINT
    IF PrintErr = FALSE THEN
        PrintWin = ExplodeWindow(9, 27, 25, 3, ManipulateC1, ManipulateC2, DoubleFrame, "Print", 1, "")
        WinPrintCenter PrintWin, 2, "Printing Record:" + STR$(RecordNum)

        LPRINT "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿";
        LPRINT "³ Serial Number:"; Equip.SerialNumber; "     Item Description:"; Equip.Item; "     ³";
        LPRINT "ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´";
        LPRINT "³ Model:"; Equip.Model; "    Department:"; Equip.Department; "    Location:"; Equip.Location; "            ³";
        LPRINT "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ";
        CloseWindow PrintWin
    END IF
END SUB

SUB ShowAll

    MoveFile

    Sort 6, SortField, MAXRECS

    DO
        flag1 = Browse(flag1, 3, 5, 69, 15, 1, "", 5, Locs(), Title$(), MAXRECS, "Browse through Items - <P> to print / <A> to print all")
        IF flag1 = -1 THEN EXIT DO
        IF Move$ = "P" THEN
            OPEN Directory$ + RanFile$ FOR RANDOM AS #1 LEN = EquipLength
            GET #1, flag1, Equip
            CLOSE #1
          
            IF VAL(Equip.Note1) <> 0 THEN
                flag = VAL(Equip.Note1)
                SendToPrinter flag
            END IF
        ELSEIF Move$ = "A" THEN
            PrintAll
        END IF
    LOOP
END SUB

SUB ShowMessage (Message$)
    Message$ = RTRIM$(LTRIM$(Message$))
   
    WinPrint 1, 1, 1, SPACE$(80)
    WinPrintCenter 1, 1, Message$
    WinRedraw 1
END SUB

SUB Sort (NumOfFields, SortField, NumOfItems)
    IF SortField < 1 THEN EXIT SUB

    ShowMessage "Sorting..."

    OPEN Directory$ + RanFile$ FOR RANDOM AS #1 LEN = EquipLength
    num.recs = LOF(1) / EquipLength
  
    Offset = INT(num.recs / 2)

    DO WHILE Offset > 0
        Limit = num.recs - Offset
        DO
            Switch = FALSE
        
            FOR X = 1 TO Limit
                GET #1, X, Equip
                Info$(1, 1) = Equip.SerialNumber
                Info$(2, 1) = Equip.Item
                Info$(3, 1) = Equip.Model
                Info$(4, 1) = Equip.Department
                Info$(5, 1) = Equip.Location
                Info$(6, 1) = Equip.Note1
                Info$(7, 1) = Equip.Note2
                GET #1, X + Offset, Equip
                Info$(1, 2) = Equip.SerialNumber
                Info$(2, 2) = Equip.Item
                Info$(3, 2) = Equip.Model
                Info$(4, 2) = Equip.Department
                Info$(5, 2) = Equip.Location
                Info$(6, 2) = Equip.Note1
                Info$(7, 2) = Equip.Note2
           
                IF UCASE$(Info$(SortField, 1)) > UCASE$(Info$(SortField, 2)) THEN
                    FOR cnt = 1 TO NumOfFields
                        SWAP Info$(cnt, 1), Info$(cnt, 2)
                    NEXT
                    Equip.SerialNumber = Info$(1, 1)
                    Equip.Item = Info$(2, 1)
                    Equip.Model = Info$(3, 1)
                    Equip.Department = Info$(4, 1)
                    Equip.Location = Info$(5, 1)
                    Equip.Note1 = Info$(6, 1)
                    Equip.Note2 = Info$(7, 1)
                    PUT #1, X, Equip
                    Equip.SerialNumber = Info$(1, 2)
                    Equip.Item = Info$(2, 2)
                    Equip.Model = Info$(3, 2)
                    Equip.Department = Info$(4, 2)
                    Equip.Location = Info$(5, 2)
                    Equip.Note1 = Info$(6, 2)
                    Equip.Note2 = Info$(7, 2)
                    PUT #1, X + Offset, Equip
               
                    Switch = X
                END IF
            NEXT

            Limit = Switch - Offset
        LOOP WHILE Switch

        Offset = Offset \ 2
    LOOP
    CLOSE #1

    ShowMessage "Inventory Manager  (c)Copyright 1993 by Nathan Thomas"
END SUB

SUB WindowInitialize
    CLS
    LOCATE 1, 1, 0, 11, 12
    VIEW PRINT 1 TO 25
    IF BackChar$ = "" THEN BackChar$ = " "
    WinCount = -1
    redraw = 0
    FirstWin = MakeWindow(1, 0, 80, 23, Fore, Back, "", "", 0, "")
    Handle(0).Buffer = STRING$(2000, BackChar$)
    redraw = 1
    WinRedraw 0
    RedrawAll
END SUB

SUB WinPrint (WinNumber, row, col, text$)
    IF WinNumber > WinCount THEN EXIT SUB
    IF WinNumber < 1 THEN EXIT SUB
    X = Handle(WinNumber).XPos
    Y = Handle(WinNumber).YPos
    L = Handle(WinNumber).Length
    H = Handle(WinNumber).Height
    Buffer$ = Handle(WinNumber).Buffer
  
    FOR t = 1 TO row - 1
        place = place + L
    NEXT
    place = place + col
  
    MID$(Buffer$, place, LEN(text$)) = text$
    Handle(WinNumber).Buffer = Buffer$

    IF redraw = 1 THEN
        IF WinNumber = WinCount THEN
            X = Handle(WinNumber).XPos
            Y = Handle(WinNumber).YPos
            L = Handle(WinNumber).Length
            H = Handle(WinNumber).Height
            F = Handle(WinNumber).Foreground
            B = Handle(WinNumber).Background
            Buffer$ = Handle(WinNumber).Buffer
                
            COLOR F, B
              
            pl = 1
            FOR t = 1 TO H
                LOCATE X + t, Y + 1
                PRINT MID$(Buffer$, pl, L);
                pl = pl + L
            NEXT
        ELSE
            WinRedraw WinNumber
        END IF
    END IF
END SUB

SUB WinPrintCenter (WinNumber, X, text$)
    L = Handle(WinNumber).Length
    IF LEN(text$) > L THEN text$ = LEFT$(text$, L)
    Y = (INT(L / 2)) - (INT(LEN(text$) / 2)) + 1
    WinPrint WinNumber, X, Y, text$
END SUB

SUB WinRedraw (WinNumber)
    FOR i = WinNumber TO WinCount
        X = Handle(WinNumber).XPos
        Y = Handle(WinNumber).YPos
        L = Handle(WinNumber).Length
        H = Handle(WinNumber).Height
        F = Handle(WinNumber).Foreground
        B = Handle(WinNumber).Background
        Buffer$ = Handle(WinNumber).Buffer
        
        COLOR F, B
        DrawFrame WinNumber
        DrawTitle WinNumber
        DrawShadow WinNumber
            
        pl = 1
        FOR t = 1 TO H
            LOCATE X + t, Y + 1
            PRINT MID$(Buffer$, pl, L);
            pl = pl + L
        NEXT
    NEXT
END SUB

