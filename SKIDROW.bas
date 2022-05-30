'
'                         Skid Row Cracktro Conversion
'                       --============================--
'
'-------------------------------------------------------------------------
   
    OPTION STATIC
    OPTION EXPLICIT

'-------------------------------------------------------------------------
' Customisable Variables;
'-------------------------------------------------------------------------

    CONST   XRES    =   640:'           X Res
    CONST   YRES    =   480:'           Y Res
    DIM SHARED AS INTEGER BHEIGHT =12:' Border Size
    
    DIM SHARED AS STRING  PAGES(500)

    #include "nfoz.txt"


    DIM SHARED AS INTEGER PAGE_OFFSET = 0
    
'-------------------------------------------------------------------------

'   Includes and Defs;

    #INCLUDE "TINYPTC_ext.BI"
    #include "windows.bi"    

    ' Large font;
    #INCLUDE "skidpal.bas"
    #INCLUDE "skidraw.bas"
    
    #include "ufmod.bi"
    #include "skidmod.bas"
    Dim hWave As HWAVEOUT   
'-------------------------------------------------------------------------------
' Initialise the Font;
'-------------------------------------------------------------------------------
           
    Const LfimgX = 480:' Strip width
    Const LfimgY = 19: ' Strip Height
    

    Declare Sub LfDrawImage(byval imxpos as integer,byval imypos as integer,byval SX as integer,byval SY as integer)
    Declare Sub LFLoadDataImage()    
    'Picture buffer
    Dim Shared LFimg_buffer( lfimgx * lfimgy ) as integer    
    'RGB color palette buffer
    Dim Shared LFimg_r(256), LFimg_g(256), LFimg_b(256) as short    
    LFLoadDataImage()

'-------------------------------------------------------------------------------
' Other Sub Defs;
'-------------------------------------------------------------------------------

    DECLARE SUB LARGETEXT(BYVAL LTX AS INTEGER , BYVAL LTY AS INTEGER ,BYVAL LTS AS STRING)
    DECLARE SUB BLANK_SCREEN()
    DECLARE SUB TRIANGLE(BYVAL X1 AS INTEGER , BYVAL Y1 AS INTEGER, BYVAL X2 AS INTEGER , BYVAL Y2 AS INTEGER , BYVAL X3 AS INTEGER, BYVAL Y3 AS INTEGER , BYVAL TC AS INTEGER)
    DECLARE SUB POLY_ERASE ()
    DECLARE SUB POLY_REDRAW()
    DECLARE SUB TEXT()
'-------------------------------------------------------------------------------
' Global Variables;
'-------------------------------------------------------------------------------
    
    DIM SHARED AS UINTEGER BUFFER   (XRES * YRES)


    DIM SHARED AS INTEGER EX1,EY1,EX2,EY2,EX3,EY3,EX4,EY4,EX5,EY5,EX6,EY6
    
        EX6=XRES/2
        EY6=YRES/2        
        EX1=0
        EY1=0        
        EX2=0
        EY2=YRES        
        EX3=XRES
        EY3=YRES        
        EX4=XRES
        EY4=0        
        EX5=0
        EY5=0        

    DIM SHARED AS DOUBLE ELAPSED
    
'-------------------------------------------------------------------------------
' Open Screen;
'-------------------------------------------------------------------------------

    ptc_allowclose(0)
    ptc_setdialog(0,"",0,1)
    
    IF (PTC_OPEN("Skid Row 2007",XRES,YRES)=0) THEN
    END-1
    END IF  
    hWave = uFMOD_PlaySong(@skid.xm(0),57967,XM_MEMORY)

'-------------------------------------------------------------------------------
' Main ();
'-------------------------------------------------------------------------------
ELAPSED=TIMER+2

WHILE(GetAsyncKeyState(VK_ESCAPE)<>-32767 AND ptc_getleftbutton() = 0)    
    
IF TIMER-ELAPSED>=0 THEN
    TEXT()
    IF TIMER-ELAPSED<4 THEN POLY_ERASE()
    IF TIMER-ELAPSED>10 THEN POLY_REDRAW()    
    IF TIMER-ELAPSED>13 THEN 
        ELAPSED=TIMER
        PAGE_OFFSET=PAGE_OFFSET+10
        IF PAGE_OFFSET>= (NUM_PAGES*10) THEN PAGE_OFFSET=0
    END IF
END IF

    PTC_UPDATE@BUFFER(0)
    BLANK_SCREEN()

    
WEND
    uFMOD_StopSong()
END

'--------------------------------------------------------------------------
'SUBROUTINE: Blit 10 Lines Of Text Down The Screen.
'--------------------------------------------------------------------------

SUB TEXT()
    DIM YL AS INTEGER
    DIM CENT AS INTEGER
    FOR YL=1 TO 10
        CENT=(XRES-(((LEN(PAGES(YL+PAGE_OFFSET)))* 20)))/2
        LARGETEXT(CENT,(YL*44)-20 , PAGES(YL+PAGE_OFFSET))
    NEXT
END SUB


'--------------------------------------------------------------------------
'SUBROUTINE: Clear Polygons To Reveal Text.
'--------------------------------------------------------------------------

SUB POLY_ERASE()
    
    DIM AS INTEGER ERASE_COL = &H000042
    IF EY1< YRES THEN TRIANGLE(EX1,EY1,EX2,EY2,EX6,EY6,ERASE_COL)
    IF EX2<XRES  THEN TRIANGLE(EX2,EY2,XRES,YRES,EX6,EY6,ERASE_COL)
    IF EY3>0     THEN TRIANGLE(EX3,EY3,EX4,EY4,EX6,EY6,ERASE_COL)
    TRIANGLE(EX4,EY4,EX5,EY5,EX6,EY6,ERASE_COL)
    
    IF EY1<YRES THEN 
        IF EY1< YRES THEN EY1=EY1+10
        IF EY3>0 THEN EY3=EY3-10
    END IF
    IF EY1>=YRES AND EX2<XRES THEN 
        IF EX2<XRES THEN EX2=EX2+10
        IF EX4>0 THEN EX4=EX4-10
    END IF
    
END SUB

'--------------------------------------------------------------------------
'SUBROUTINE: Restore Polygons To Obscure Text.
'--------------------------------------------------------------------------

SUB POLY_REDRAW()
    DIM AS INTEGER ERASE_COL = &H000042
    IF EY1< YRES THEN TRIANGLE(EX1,EY1,EX2,EY2,EX6,EY6,ERASE_COL)
    IF EX2<XRES  THEN TRIANGLE(EX2,EY2,XRES,YRES,EX6,EY6,ERASE_COL)
    IF EY3>0     THEN TRIANGLE(EX3,EY3,EX4,EY4,EX6,EY6,ERASE_COL)
    TRIANGLE(EX4,EY4,EX5,EY5,EX6,EY6,ERASE_COL)
    
    IF EY1>0 THEN 
        IF EX2>0 THEN EX2=EX2-10
        IF EX4<XRES THEN EX4=EX4+10
    END IF
    IF EY1>0  AND EX4>=XRES THEN 
        IF EY1>0 THEN EY1=EY1-10
        IF EY3<YRES THEN EY3=EY3+10
    END IF
    
END SUB
'-------------------------------------------------------------------------------
' SUBROUTINE : Erase Buffer, Draw Borders;
'-------------------------------------------------------------------------------

SUB BLANK_SCREEN()
    
    DIM LENG AS INTEGER
    DIM AS INTEGER Y,TC,Y2
    DIM PP AS UINTEGER PTR
    
    LENG = xres
    
    FOR Y=0 TO YRES-1
        
        SELECT CASE Y
        
            CASE 0 TO BHEIGHT
            TC=&H000000
            CASE  YRES-BHEIGHT TO YRES
            TC=&H000000
            CASE BHEIGHT TO BHEIGHT+2
            TC=&HFFFFFF
            CASE  YRES-BHEIGHT-2 TO YRES-BHEIGHT
            TC=&HFFFFFF        
            CASE ELSE
            TC=&H000042
            
        END SELECT
    
    PP = @BUFFER(Y*XRES)     
    
    ASM
        
        MOV EAX,DWORD PTR[TC]
        MOV ECX, [LENG]
        MOV EDI, [PP]
        REP STOSD
        
    END ASM
    
    NEXT

  
END SUB


'-------------------------------------------------------------------------------
' SUBROUTINE : Control Sub to print a string of text.
'-------------------------------------------------------------------------------

SUB LARGETEXT(BYVAL LTX AS INTEGER , BYVAL LTY AS INTEGER ,BYVAL LTS AS STRING)
    
    DIM AS INTEGER A,MMM,NNN
    lts=UCASE(LTS)
    FOR A=1 TO LEN(LTS)
    NNN=(ASC(MID(LTS,A,1))-31)
    
    
    IF NNN>63 THEN NNN=-1
    'if nnn=0 then nnn=1
    MMM=NNN*8

    if nnn>0 then LFDRAWIMAGE( LTX,LTY, MMM , 0 )
    
    LTX=LTX+20

    NEXT

END SUB


'-------------------------------------------------------------------------------
' SUBROUTINE : (used once) Load the font.
'-------------------------------------------------------------------------------

Sub LFLoadDataImage() 
    dim i as integer
    'Loads Color palette
    for i = 0 to 255
         LFimg_r( i ) = skid.bmp.pal (i*3)'Red color
         LFimg_g( i ) = skid.bmp.pal (i*3+1)'Green color
         LFimg_b( i ) = skid.bmp.pal (i*3+2)'Blue color
         
         LFimg_r( i ) =(LFimg_r(i) Shl 16) Or (LFimg_g(i) Shl 8 )  Or LFimg_b(i)
         
    Next    
    
    for i = 1 to (LFimgx*LFimgy) - 1
         LFimg_buffer(i) = skid.bmp.raw (i)
    next  
        
End Sub



'-------------------------------------------------------------------------------
' SUBROUTINE : Chop out the right letter and draw it.
'-------------------------------------------------------------------------------

Sub LFDrawImage(byval xpos as integer,byval ypos as integer,byval SX as integer,byval SY as integer)
    dim as integer x,y,pixel,mong,intx,inty,xxx,yyy
    xxx=xpos
    yyy=0
    
    for Y =0 to 18

        for X = SX+1 to SX+8
            
            pixel = LFimg_buffer(x+(y*lfimgx))            
            mong = (LFimg_r(pixel) )            
                
                                
                inty = yyy+ypos                
                intx = xxx
                
                if intX > 0  AND intX<XRES AND MONG<>&H040204 then 
                    Buffer( intX  +((intY) * XRES  )) = mong
                    Buffer( intX  +1+((intY) * XRES  )) = mong
                    Buffer( intX  +((intY+1) * XRES  )) = mong
                    Buffer( intX  +1+((intY+1) * XRES  )) = mong
                    
                
                END IF
            
            xxx=xxx+2

        next
        
            yyy=yyy+2
            xxx=xpos
    next
    
End Sub


'-------------------------------------------------------------------------------
' SUBROUTINE : Draw Flat Shaded Triangle;
'-------------------------------------------------------------------------------

SUB TRIANGLE(BYVAL X1 AS INTEGER , BYVAL Y1 AS INTEGER, BYVAL X2 AS INTEGER , BYVAL Y2 AS INTEGER , BYVAL X3 AS INTEGER, BYVAL Y3 AS INTEGER , BYVAL TC AS INTEGER)
'-------------------------------------------------------------------------
' FLAT TRIANGLE RENDERER WITH ASSEMBLY LANGUAGE RASTERISING BY SHOCKWAVE ^ DBF ^ S!P 2006.
'-------------------------------------------------------------------------
'-------------------------------------------------------------------------
' WE NEED TO SORT THESE POINTS INTO ORDER FROM TOP TO BOTTOM, AN EXCHANGE SORT IS OK.
' AS WE ONLY HAVE GOT 3 POINTS TO ARRANGE.
'-------------------------------------------------------------------------
DIM AS INTEGER TEMPX,TEMPY,LO,LI 
                DIM AS INTEGER PX(3)
                DIM AS INTEGER PY(3)
                DIM TFLAG AS INTEGER
                dim pp as uinteger PTR
                DIM AS INTEGER IL1,IL2,SLICE
                TFLAG=0
        PX(1)= X1
        PX(2)= X2
        PX(3)= X3
        
        PY(1)= Y1
        PY(2)= Y2
        PY(3)= Y3

FOR LO = 1 TO 2
    FOR LI =1 TO 2      
        IF PY(LI+1) <= PY(LI) THEN
        TEMPX = PX(LI) : TEMPY = PY(LI)
        PX(LI) = PX(LI+1)
        PY(LI) = PY(LI+1)
        PX(LI+1) = TEMPX
        PY(LI+1) = TEMPY
        END IF    
    NEXT LI
NEXT LO
 
'   BOOT OUT INVISIBLE TRIANGLES!

    IF PX(1)<0 AND PX(2)<0  AND PX(3)< 0 THEN TFLAG=1
    IF PX(1)>XRES AND PX(2)>XRES  AND PX(3)>XRES THEN TFLAG=1
    IF PY(1)>YRES AND PY(2)>YRES  AND PY(3)>YRES THEN TFLAG=1
    
        DIM AS DOUBLE XP1,XP2:' SCREEN POSITIONS.
        DIM AS DOUBLE XI1,XI2:' INTERPOLATIONS.
        
'***
'*** REGULAR TRIANGLE (Y1<Y2 Y2<Y3)
'***

IF PY(1)<PY(2) AND PY(2)<PY(3) or (PY(2) = PY(3)) THEN
    TFLAG=1
XP1 = PX(1)
XP2 = PX(1)
XI1 = (PX(1)-PX(2)) / (PY(2) - PY(1))
XI2 = (PX(1)-PX(3)) / (PY(3) - PY(1))

FOR LO = PY(1) TO PY(2)-1
    
IF LO>=BHEIGHT+4 AND LO<YRES-(BHEIGHT+4) THEN 

    IF XP1<=XP2 THEN
        IL1=XP1
        IL2=XP2
    ELSE
        IL1=XP2
        IL2=XP1
    END IF
    
    IF IL2>XRES THEN IL2=XRES
    IF IL1<0 THEN IL1=0

    SLICE = IL2-IL1
    IF SLICE>0 THEN
    PP = @BUFFER(IL1+(LO*XRES))   
    asm
        mov eax,dword ptr[TC]
        mov ecx, [slice]
        mov edi, [PP]
        rep stosd
    end asm   
    END IF
    

END IF

XP1=XP1-XI1
XP2=XP2-XI2
NEXT

XI1 = (PX(2)-PX(3)) / (PY(3) - PY(2))
XP1 = PX(2)

FOR LO = PY(2) TO PY(3)
IF LO>=BHEIGHT+4 AND LO<YRES-(BHEIGHT+4) THEN 
    IF XP1<=XP2 THEN
        IL1=XP1
        IL2=XP2
    ELSE
        IL1=XP2
        IL2=XP1
    END IF

    IF IL2>XRES THEN IL2=XRES
    IF IL1<0 THEN IL1=0

    SLICE = IL2-IL1
    IF SLICE>0 THEN
    PP = @BUFFER(IL1+(LO*XRES))   
    asm
        mov eax,dword ptr[TC]
        mov ecx, [slice]
        mov edi, [PP]
        rep stosd
    end asm   
    END IF
END IF
XP1=XP1-XI1
XP2=XP2-XI2
NEXT

END IF


'***
'*** FLAT TOPPED TRIANGLE Y1=Y2
'***

IF TFLAG=0 AND PY(1) = PY(2) THEN
    
        TFLAG=1
        XP1 = PX(1)
        XP2 = PX(2)
        XI1 = (PX(1)-PX(3)) / (PY(3) - PY(1))
        XI2 = (PX(2)-PX(3)) / (PY(3) - PY(2))
FOR LO = PY(1) TO PY(3)
IF LO>=BHEIGHT+4 AND LO<YRES-(BHEIGHT+4) THEN 
    IF XP1<=XP2 THEN
        IL1=XP1
        IL2=XP2
    ELSE
        IL1=XP2
        IL2=XP1
    END IF
    
    IF IL2>XRES THEN IL2=XRES
    IF IL1<0 THEN IL1=0
    
    SLICE = IL2-IL1
    IF SLICE>0 THEN
    PP = @BUFFER(IL1+(LO*XRES))   
    asm
        mov eax,dword ptr[TC]
        mov ecx, [slice]
        mov edi, [PP]
        rep stosd
    end asm   
    END IF
END IF
    XP1=XP1-XI1
    XP2=XP2-XI2

NEXT
END IF
END SUB
