VERSION 5.00
Begin VB.Form Form1 
   BorderStyle     =   0  'None
   Caption         =   "0"
   ClientHeight    =   8955
   ClientLeft      =   0
   ClientTop       =   -105
   ClientWidth     =   12000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   597
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   800
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
' Programmed By [ Zaid Markabi ]
' ___________________________________________________________________________________________________
'|                                                                                                   |\_______________________
'|  ###############        ###         #####   ######                ######    #####                 |                        |\0 1 1 1 0 0 1 1 0 0 0 1 0 0 1 0 0 1 0 0 1 1 1 1 0 0 1 1 0 0 0 1 0 0 1 0 0 1 0 0 1
'| ##############         #####         ###     ##   ##               ######  #####                  |      Zaid Markabi      |=\ 1 0 0 1 0 0 0 0 0 1 1 0 1 0 0 0 1 1 1 0 1 0 0 1 0 0 0 0 0 1 1 0 1 0 0 0 1 1 1 0
'|         ####          ### ###        ###     ##    ##              ##  ## ##  ##                  |                        |==\0 0 1 1 1 0 1 0 0 1 0 0 1 1 0 0 1 0 1 1 0 0 1 1 1 0 1 0 0 1 0 0 1 1 0 0 1 0 1 1
'|       ###            ###   ###       ###     ##     ##    #####    ##   ###   ##                  | zaidmarkabi@yahoo.com  |===\ 1 __________________________________  0 1 0 0 0 1 1 1 0 1 0 0 1 0 0 1 0 0 0 1
'|     ###             ###########      ###     ##     ##   ####      ##    #    ##                  |                        |====|>| Development For Our Digital Life | 1 1 0 0 1 1 1 0 1 0 0 1 0 0 0 1 1 0 1 0
'|   ###              #############     ###     ##    ##              ##         ##      A R K A B I | VisualBasic Programmer |===/ 1|__________________________________| 0 1 1 0 1 0 0 0 1 1 1 0 1 0 1 1 0 1 0 0
'| ##############    ###         ###    ###     ##   ##               ##         ##     ############ |                        |==/0 0 1 1 1 0 1 0 0 1 0 0 1 1 0 0 1 0 1 1 0 0 1 1 1 0 1 0 0 1 0 0 1 1 0 0 1 0 1 1
'| ###############   ###         ###   #####   ######                ####       ####   ### 2008 ###  |Syria(Arab Area)-Tartuse|=/ 1 0 0 1 0 0 0 0 0 1 1 0 1 0 0 0 1 1 1 0 1 0 0 1 0 0 0 0 0 1 1 0 1 0 0 0 1 1 1 0
'|                                                                                    ############   | _______________________|/0 1 1 1 0 0 1 1 0 0 0 1 0 0 1 0 0 1 0 0 1 1 1 1 0 0 1 1 0 0 0 1 0 0 1 0 0 1 0 0 1
'|___________________________________________________________________________________________________|/

' Email me at
' zaidmarkabi@yahoo.com
' I hope to hear from u soon,

' About Me
' --------
' Name:  Zaid Markabi
' Language:  Arabic
' Nationality:  Arabic -Syrian
' Live in : Syria - Tartuse
' My Twin 's Website : YazanMarkabi.Jeeran.com
' My Motto : Development For Our Digital Life
' Email:  ZaidMarkabi@ yahoo.com

Option Explicit

Private TV3D As New TVEngine

Private LastPost As D3DVECTOR

Private TankActor As TVMesh
Private TankGun As TVMesh
Private TankPosition As D3DVECTOR
Private TankGunPosition As D3DVECTOR
Private TankDestination As D3DVECTOR
Private TankDirection As D3DVECTOR
Private GunON As Boolean
Private CollisionResult As TVCollisionResult
Private TankHealth As Integer

Private TankComActor() As TVMesh
Private TankComGun() As TVMesh
Private TankComGunPosition() As D3DVECTOR
Private TankComDestination() As D3DVECTOR
Private TankComDirection() As D3DVECTOR
Private TankComKDestination() As D3DVECTOR
Private TankComKDirection() As D3DVECTOR
Private GunComON() As Boolean
Private TankComPosition() As D3DVECTOR
Private TankComHealth() As Integer
Private TankComKill() As Integer
Private AllCom As Integer

Private TankFActor() As TVMesh
Private TankFGun() As TVMesh
Private TankFGunPosition() As D3DVECTOR
Private TankFDestination() As D3DVECTOR
Private TankFDirection() As D3DVECTOR
Private TankFKDestination() As D3DVECTOR
Private TankFKDirection() As D3DVECTOR
Private GunFON() As Boolean
Private TankFPosition() As D3DVECTOR
Private TankFHealth() As Integer
Private TankFKill() As Integer
Private TankWin() As Boolean

Private GameOver As Boolean
Private GameWin As Boolean
Private GameMid As Boolean
Private GameOverWait As Integer

Private ZoomGun As D3DVECTOR2

Private Land As New TVLandscape

Private TextureFactory As TVTextureFactory

Private Scene As TVScene

Private ScreenText As TVScreen2DText
Private ScreenText2 As TVScreen2DImmediate

Private InputEngine As TVInputEngine

Private Atmos As TVAtmosphere

Private DoLoop As Boolean

Private sngPositionX As Single
Private sngPositionY As Single
Private sngPositionZ As Single
Private sngAngleX As Single
Private sngAngleY As Single

Private tmpMouseX As Long, tmpMouseY As Long
Private tmpLookAtX As Single, tmpLookAtZ As Single, tmpLookAtY As Single
Private I As Integer
Private LastPos As D3DVECTOR
Private I3 As Integer
Private I4 As Integer

Private sngWalk As Single
Private sngStrafe As Single

Private sngBrake As Single

Private Sub Form_Load()
On Error Resume Next
    
    TV3D.SetSearchDirectory App.Path
    TV3D.Init3DWindowedMode Me.hWnd
    TV3D.DisplayFPS = False
    Set InputEngine = New TVInputEngine

    Set Scene = New TVScene

    Scene.SetSceneBackGround 0.4, 0.3, 0.3

Scene.SetCollisionPrecision 50

    Set TextureFactory = New TVTextureFactory

    Land.GenerateHugeTerrain App.Path + "\Data Files\Mission1\Track.jpg", TV_PRECISION_LOW, 8, 8, -700, -1024, True
    
    Land.SetTerrainScale 4, 1, 4
    
    TextureFactory.LoadTexture App.Path + "\Data Files\Mission1\Earth.jpg", "LandTexture"
    TextureFactory.LoadTexture App.Path + "\Data Files\Gfx\tank.bmp", "TankTexture", , , TV_COLORKEY_NO
    TextureFactory.LoadTexture App.Path + "\Data Files\Gfx\Gun.bmp", "TankGunTexture", , , TV_COLORKEY_NO
    TextureFactory.LoadTexture App.Path + "\Data Files\Gfx\Zoom.bmp", "Zoom", , , TV_COLORKEY_BLACK

    Land.SetTexture GetTex("LandTexture")
    
    TV3D.SetAngleSystem TV_ANGLE_DEGREE
    
    Set TankActor = New TVMesh
    
    Set TankGun = New TVMesh
    
    Set TankActor = Scene.CreateMeshBuilder
    Set TankGun = Scene.CreateMeshBuilder
    
    ' Init the sky
    Set Atmos = New TVAtmosphere

Atmos.Fog_SetParameters
Atmos.Fog_Enable True
Atmos.Fog_SetColor 0.4, 0.3, 0.3, 1
Atmos.Fog_SetType TV_FOG_LINEAR, TV_FOGTYPE_VERTEX

ZoomGun.x = 400
ZoomGun.y = 150

    TV3D.SetSearchDirectory App.Path + "\Data Files\"
    
    TankActor.Load3DSMesh App.Path + "\Data Files\Models\Tank.3ds", False
    TankActor.SetTexture GetTex("TankTexture")
    
    TankGun.Load3DSMesh App.Path + "\Data Files\Models\Gun.3ds", False
    TankGun.SetTexture GetTex("TankGunTexture")
    TankGun.SetColor RGBA(1, 0, 0, 1)
    TankGun.ScaleMesh 0.05, 0.05, 0.05
    
    TankPosition.x = 2000
    TankPosition.z = 2000
    TankPosition.y = Land.GetHeight(TankPosition.x, TankPosition.z)
    
    TankGunPosition.x = 2000
    TankGunPosition.z = 2000
    TankGunPosition.y = Land.GetHeight(TankPosition.x, TankPosition.z) + 10
    
    TankActor.SetPosition TankPosition.x, TankPosition.y, TankPosition.z
    
    TankGun.SetPosition TankGunPosition.x, TankGunPosition.y, TankGunPosition.z
        
TankHealth = 100

AllCom = 5
ReDim TankComPosition(0 To AllCom)
ReDim TankComActor(0 To AllCom)
ReDim TankComGun(0 To AllCom)
ReDim TankComGunPosition(0 To AllCom)
ReDim TankComDestination(0 To AllCom)
ReDim TankComDirection(0 To AllCom)
ReDim GunComON(0 To AllCom)
ReDim TankComPosition(0 To AllCom)
ReDim TankComHealth(0 To AllCom)
ReDim TankComKDestination(0 To AllCom)
ReDim TankComKDirection(0 To AllCom)
ReDim TankComKill(0 To AllCom)

ReDim TankFPosition(0 To AllCom)
ReDim TankFActor(0 To AllCom)
ReDim TankFGun(0 To AllCom)
ReDim TankFGunPosition(0 To AllCom)
ReDim TankFDestination(0 To AllCom)
ReDim TankFDirection(0 To AllCom)
ReDim GunFON(0 To AllCom)
ReDim TankFPosition(0 To AllCom)
ReDim TankFHealth(0 To AllCom)
ReDim TankFKDestination(0 To AllCom)
ReDim TankFKDirection(0 To AllCom)
ReDim TankFKill(0 To AllCom)
ReDim TankWin(0 To AllCom)

For I = 0 To AllCom
Set TankComActor(I) = Scene.CreateMeshBuilder
TankComActor(I).Load3DSMesh App.Path + "\Data Files\Models\Tank.3ds", False
TankComActor(I).SetTexture GetTex("TankTexture")
TankComActor(I).SetColor RGBA(0, 0, 1, 1)
TankComPosition(I).x = (250 * I) + 2000
TankComPosition(I).z = (250 * I) + 2000
TankComPosition(I).y = Land.GetHeight(TankComPosition(I).x, TankComPosition(I).z) + 10
TankComActor(I).SetPosition TankComPosition(I).x, TankComPosition(I).y, TankComPosition(I).z
Set TankComGun(I) = Scene.CreateMeshBuilder
TankComGun(I).Load3DSMesh App.Path + "\Data Files\Models\Gun.3ds", False
TankComGun(I).SetTexture GetTex("TankGunTexture")
TankComGun(I).SetColor RGBA(0, 0, 1, 1)
TankComGun(I).ScaleMesh 0.05, 0.05, 0.05
TankComGunPosition(I) = TankComActor(I).GetPosition
TankComHealth(I) = 40
TankComKill(I) = I

Set TankFActor(I) = Scene.CreateMeshBuilder
TankFActor(I).Load3DSMesh App.Path + "\Data Files\Models\Tank.3ds", False
TankFActor(I).SetTexture GetTex("TankTexture")
TankFPosition(I).x = (300 * (I + 1)) + 2000
TankFPosition(I).z = (300 * I) + 2000
TankFPosition(I).y = Land.GetHeight(TankFPosition(I).x, TankFPosition(I).z) + 10
TankFActor(I).SetPosition TankFPosition(I).x, TankFPosition(I).y, TankFPosition(I).z
Set TankFGun(I) = Scene.CreateMeshBuilder
TankFGun(I).Load3DSMesh App.Path + "\Data Files\Models\Gun.3ds", False
TankFGun(I).SetTexture GetTex("TankGunTexture")
TankFGun(I).SetColor RGBA(1, 0, 0, 1)
TankFGun(I).ScaleMesh 0.05, 0.05, 0.05
TankFGunPosition(I) = TankComActor(I).GetPosition
TankFHealth(I) = 20
TankFKill(I) = I
TankFKDestination(I).x = 6000
TankFKDestination(I).z = 3000
Next
    
    ' We set the camera vectors (position and look at) and angles.
    sngPositionX = 0
    sngPositionY = 20
    sngPositionZ = 0
    sngAngleX = 0
    sngAngleY = 0
    
    ' We set the initial values of movement
    sngWalk = 0
    sngStrafe = 0
    
    Form1.Show
        
    DoLoop = True
    Main_Loop

End Sub

Private Sub Form_Unload(Cancel As Integer)
    DoLoop = False
End Sub

Private Sub Main_Loop()
On Error Resume Next
TV3D.SetGeneralSpeed 100
     
     Do
        DoEvents
If GameOver = False Then
LastPost = TankActor.GetPosition

Check_Input
Check_Coms
Check_Movement

For I = 0 To AllCom
If TankComHealth(I) > 0 Then
If GetDistance3D(TankActor.GetPosition.x, 0, TankActor.GetPosition.z, TankComPosition(I).x, 0, TankComPosition(I).z) < 35 Then
TankActor.SetPosition LastPost.x, LastPost.y, LastPost.z
TankPosition = LastPost
End If
End If
If TankFHealth(I) > 0 Then
If GetDistance3D(TankActor.GetPosition.x, 0, TankActor.GetPosition.z, TankFActor(I).GetPosition.x, 0, TankFActor(I).GetPosition.z) < 35 Then
TankActor.SetPosition LastPost.x, LastPost.y, LastPost.z
TankPosition = LastPost
End If
End If
Next

TV3D.Clear

Land.Render False, False

Scene.RenderAllMeshes

Render_Screen_Text

Atmos.Atmosphere_Render

TV3D.RenderToScreen

Else

If GameOverWait < 300 Then
Render_Screen_Text
TV3D.RenderToScreen
GameOverWait = GameOverWait + 1
Else
DoLoop = False
End If
End If

Loop Until DoLoop = False

    Set TextureFactory = Nothing
    Set TankActor = Nothing
    Set Land = Nothing
    Set InputEngine = Nothing
    Set Scene = Nothing
    Set TV3D = Nothing
End
End Sub

Private Sub Check_Input()
On Error Resume Next
If InputEngine.IsKeyPressed(TV_KEY_ESCAPE) = True Then
DoLoop = False
End If

If InputEngine.IsKeyPressed(TV_KEY_UP) = True Then
sngWalk = sngWalk + 0.05
If sngWalk > 1 Then sngWalk = 1
End If
        
If InputEngine.IsKeyPressed(TV_KEY_DOWN) = True Then
sngWalk = sngWalk - 0.05
If sngWalk < -1 Then sngWalk = -1
End If

If InputEngine.IsKeyPressed(TV_KEY_NUMPAD8) = True Then
ZoomGun.y = ZoomGun.y - 2
End If
If InputEngine.IsKeyPressed(TV_KEY_NUMPAD2) = True Then
ZoomGun.y = ZoomGun.y + 2
End If
If InputEngine.IsKeyPressed(TV_KEY_NUMPAD4) = True Then
ZoomGun.x = ZoomGun.x - 2
End If
If InputEngine.IsKeyPressed(TV_KEY_NUMPAD6) = True Then
ZoomGun.x = ZoomGun.x + 2
End If
If InputEngine.IsKeyPressed(TV_KEY_NUMPAD5) = True Then
ZoomGun.x = 400
ZoomGun.y = 150
End If

If sngWalk > 0 Then
sngWalk = sngWalk - 0.01
End If
If sngWalk < 0 Then
sngWalk = sngWalk + 0.01
End If

If InputEngine.IsKeyPressed(TV_KEY_LEFT) = True Then
sngStrafe = sngStrafe + 0.05
If sngStrafe > 1 Then sngStrafe = 1
ElseIf InputEngine.IsKeyPressed(TV_KEY_RIGHT) = True Then
sngStrafe = sngStrafe - 0.05
If sngStrafe < -1 Then sngStrafe = -1
End If

InputEngine.GetMouseState tmpMouseX, tmpMouseY
sngAngleY = sngAngleY - (tmpMouseX / 100)

If InputEngine.IsKeyPressed(TV_KEY_SPACE) = True Then
If GunON = False Then
TankDirection = TankPosition
TankGunPosition = TankPosition
Set CollisionResult = Scene.MousePicking(ZoomGun.x, ZoomGun.y, TV_COLLIDE_LANDSCAPE, TV_TESTTYPE_ACCURATETESTING)
        If CollisionResult.IsCollision Then
            TankDestination = CollisionResult.GetImpactPoint
        End If
GunON = True
End If
End If
End Sub

Private Sub Check_Movement()
On Error Resume Next

        Select Case sngWalk
        Case Is > 0
            sngWalk = sngWalk - sngBrake * TV3D.TimeElapsed
            If sngWalk < 0 Then sngWalk = 0
        End Select

        Select Case sngStrafe
        Case Is > 0
            sngStrafe = sngStrafe - 0.001 * TV3D.TimeElapsed
            If sngStrafe < 0 Then sngStrafe = 0
        Case Is < 0
            sngStrafe = sngStrafe + 0.001 * TV3D.TimeElapsed
            If sngStrafe > 0 Then sngStrafe = 0
        End Select

        TankPosition.x = TankPosition.x + (Cos(sngAngleY) * sngWalk / 5 * TV3D.TimeElapsed) + (Cos(sngAngleY + 3.141596 / 2) * sngStrafe / 5 * TV3D.TimeElapsed)
        TankPosition.z = TankPosition.z + (Sin(sngAngleY) * sngWalk / 5 * TV3D.TimeElapsed) + (Sin(sngAngleY + 3.141596 / 2) * sngStrafe / 5 * TV3D.TimeElapsed)
        TankPosition.y = Land.GetHeight(TankPosition.x, TankPosition.z) + 15

        TankActor.SetPosition TankPosition.x, TankPosition.y, TankPosition.z

        TankActor.SetRotation (sngWalk * 10), 90 + (sngAngleY * -57.295), (sngStrafe * 10)

        tmpLookAtX = TankPosition.x - (Cos(sngAngleY) * 40)
        tmpLookAtZ = TankPosition.z - (Sin(sngAngleY) * 40)
        tmpLookAtY = Land.GetHeight(tmpLookAtX, tmpLookAtZ) + 31

        Scene.SetCamera tmpLookAtX, tmpLookAtY, tmpLookAtZ, TankPosition.x, TankPosition.y, TankPosition.z

' MOVE GUN FRIENDS
For I = 0 To AllCom
If TankFHealth(I) > 0 Then

If GunFON(I) = True Then
If GetDistance3D(TankFGunPosition(I).x, 0, TankFGunPosition(I).z, TankFDestination(I).x, 0, TankFDestination(I).z) > 10 Then
TankFDirection(I) = VNormalize(VSubtract(TankFDestination(I), TankFGunPosition(I)))
TankFGunPosition(I) = VAdd(TankFGunPosition(I), VScale(TankFDirection(I), 20))
If TankFGunPosition(I).y < Land.GetHeight(TankFGunPosition(I).x, TankFGunPosition(I).z) Then
GunFON(I) = False
End If
TankFGun(I).SetPosition TankFGunPosition(I).x, TankFGunPosition(I).y, TankFGunPosition(I).z
Else
GunFON(I) = False
TankFGun(I).SetPosition TankFPosition(I).x, TankFPosition(I).y, TankFPosition(I).z
End If
Else
TankFGun(I).SetPosition TankFPosition(I).x, TankFPosition(I).y, TankFPosition(I).z
End If

' MOVE FRIEND
If GetDistance3D(TankFPosition(I).x, 0, TankFPosition(I).z, TankFKDestination(I).x, 0, TankFKDestination(I).z) > 10 Then
TankFKDirection(I) = VNormalize(VSubtract(TankFKDestination(I), TankFPosition(I)))
TankFPosition(I) = VAdd(TankFPosition(I), VScale(TankFKDirection(I), 1))
TankFPosition(I).y = Land.GetHeight(TankFPosition(I).x, TankFPosition(I).z) + 15
TankFActor(I).SetPosition TankFPosition(I).x, TankFPosition(I).y, TankFPosition(I).z
If TankComHealth(TankFKill(I)) > 0 Then
TankFActor(I).LookAtPoint TankComActor(TankFKill(I)).GetPosition
Else
TankFActor(I).LookAtPoint TankFKDestination(I)
End If
Else
TankWin(I) = True
TankFHealth(I) = 0
TankFActor(I).SetPosition TankFPosition(I).x, TankFPosition(I).y, TankFPosition(I).z
End If
Else
TankFPosition(I).y = Land.GetHeight(TankFPosition(I).x, TankFPosition(I).z) + 15
TankFActor(I).SetPosition TankFPosition(I).x, TankFPosition(I).y, TankFPosition(I).z
End If

' WIN FRIEND
If GetDistance3D(TankFPosition(I).x, 0, TankFPosition(I).z, TankFKDestination(I).x, 0, TankFKDestination(I).z) = 0 Then
If TankFHealth(I) > 0 Then
TankWin(I) = True
TankFHealth(I) = 0
TankFActor(I).SetPosition TankFPosition(I).x, TankFPosition(I).y, TankFPosition(I).z
End If
End If
Next

' GUN PLAYER GO
If GunON = True Then
If GetDistance3D(TankGunPosition.x, 0, TankGunPosition.z, TankDestination.x, 0, TankDestination.z) > 10 Then
TankDirection = VNormalize(VSubtract(TankDestination, TankGunPosition))
TankGunPosition = VAdd(TankGunPosition, VScale(TankDirection, 20))
If TankGunPosition.y < Land.GetHeight(TankGunPosition.x, TankGunPosition.z) Then
GunON = False
End If
TankGun.SetPosition TankGunPosition.x, TankGunPosition.y + 5, TankGunPosition.z
Else
GunON = False
TankGun.SetPosition TankPosition.x, TankPosition.y, TankPosition.z
End If
Else
TankGun.SetPosition TankPosition.x, TankPosition.y, TankPosition.z
End If

If GetDistance3D(TankGunPosition.x, 0, TankGunPosition.z, TankDestination.x, 0, TankDestination.z) > 1000 Then
GunON = False
TankGun.SetPosition TankPosition.x, TankPosition.y, TankPosition.z
End If

' COMs GUN GO
For I = 0 To AllCom
If TankComHealth(I) > 0 Then
If GunComON(I) = True Then
If GetDistance3D(TankComGunPosition(I).x, 0, TankComGunPosition(I).z, TankComDestination(I).x, 0, TankComDestination(I).z) > 10 Then
TankComDirection(I) = VNormalize(VSubtract(TankComDestination(I), TankComGunPosition(I)))
TankComGunPosition(I) = VAdd(TankComGunPosition(I), VScale(TankComDirection(I), 20))
If TankComGunPosition(I).y < Land.GetHeight(TankComGunPosition(I).x, TankComGunPosition(I).z) Then
GunComON(I) = False
End If
TankComGun(I).SetPosition TankComGunPosition(I).x, TankComGunPosition(I).y, TankComGunPosition(I).z
Else
GunComON(I) = False
TankComGun(I).SetPosition TankComPosition(I).x, TankComPosition(I).y, TankComPosition(I).z
End If
Else
TankComGun(I).SetPosition TankComPosition(I).x, TankComPosition(I).y, TankComPosition(I).z
End If

' GUN COM KILL YOU
If GunComON(I) = True Then
If TankComGunPosition(I).y - TankActor.GetPosition.y > -5 And TankComGunPosition(I).y - TankActor.GetPosition.y < 5 Then
If TankComGunPosition(I).x - TankActor.GetPosition.x > -5 And TankComGunPosition(I).x - TankActor.GetPosition.x < 5 Then
If TankComGunPosition(I).z - TankActor.GetPosition.z > -5 And TankComGunPosition(I).z - TankActor.GetPosition.z < 5 Then
TankHealth = TankHealth - 1
GunComON(I) = False
TankComGun(I).SetPosition TankComPosition(I).x, TankComPosition(I).y, TankComPosition(I).z
End If
End If
End If
End If

' GUN COMS KILL FRIENDS
If TankFHealth(TankComKill(I)) > 0 Then
If GunComON(I) = True Then
If TankComGunPosition(I).y - TankFActor(TankComKill(I)).GetPosition.y > -5 And TankComGunPosition(I).y - TankFActor(I).GetPosition.y < 5 Then
If TankComGunPosition(I).x - TankFActor(TankComKill(I)).GetPosition.x > -5 And TankComGunPosition(I).x - TankFActor(I).GetPosition.x < 5 Then
If TankComGunPosition(I).z - TankFActor(TankComKill(I)).GetPosition.z > -5 And TankComGunPosition(I).z - TankFActor(I).GetPosition.z < 5 Then
TankFHealth(I) = TankFHealth(I) - 1
GunComON(I) = False
TankComGun(I).SetPosition TankComPosition(I).x, TankComPosition(I).y, TankComPosition(I).z
End If
End If
End If
End If
End If

' CHECK PLAYER GUN IF KILL COMs
If GunON = True Then
If TankGunPosition.x - TankComActor(I).GetPosition.x > -10 And TankGunPosition.x - TankComActor(I).GetPosition.x < 10 Then
If TankGunPosition.z - TankComActor(I).GetPosition.z > -10 And TankGunPosition.z - TankComActor(I).GetPosition.z < 10 Then
If TankGunPosition.y - TankComActor(I).GetPosition.y > -50 And TankGunPosition.y - TankComActor(I).GetPosition.y < 50 Then
GunON = False
TankGun.SetPosition TankPosition.x, TankPosition.y, TankPosition.z
TankComHealth(I) = TankComHealth(I) - 1
End If
End If
End If
End If

TankComPosition(I).y = Land.GetHeight(TankComPosition(I).x, TankComPosition(I).z) + 10
End If
Next
End Sub

Private Sub Check_Coms()
On Error Resume Next
Dim I2 As Integer

If TankHealth < 1 Then
GameOver = True
End If

For I = 0 To AllCom

' IF FRIEND DIE
If TankFHealth(I) < 1 Then
TankFPosition(I).x = -50000
TankFPosition(I).y = -50000
TankFPosition(I).z = -50000
TankFHealth(I) = 0
Scene.DestroyMesh TankFActor(I)
Scene.DestroyMesh TankFGun(I)
TankComKill(I) = TankComKill(I) + 1
End If

If TankComKill(I) > AllCom Then
TankComKill(I) = 0
End If

I3 = 0
I4 = 0
For I2 = 0 To AllCom
If TankFHealth(I2) < 1 Then
If TankWin(I2) = False Then
I3 = I3 + 1
Else
I4 = I4 + 1
End If
End If
Next
If I3 = AllCom Then
GameOver = True
Exit Sub
End If
If I4 = AllCom Then
GameWin = True
GameOver = True
Exit Sub
End If

I3 = 0
For I2 = 0 To AllCom
If TankFHealth(I2) < 1 Then
I3 = I3 + 1
End If
Next
If I3 = AllCom Then
GameMid = True
GameOver = True
Exit Sub
End If

' FRIEND ATTACK COMS
If TankFHealth(I) > 0 Then
If GunFON(I) = False Then
TankFDirection(I) = TankFPosition(I)
TankFGunPosition(I) = TankFPosition(I)
TankFDestination(I) = TankComActor(TankFKill(I)).GetPosition
GunFON(I) = True
End If
End If

' CHECK COMS
If TankComHealth(I) > 0 Then

' COM KILL YOU OR FRIEND
If GunComON(I) = False Then
If Int(Right(Rnd, 1)) > 5 Then
TankComDirection(I) = TankComPosition(I)
TankComGunPosition(I) = TankComPosition(I)
If TankFHealth(I) > 0 Then
TankComDestination(I) = TankFActor(TankComKill(I)).GetPosition
GunComON(I) = True
End If
Else
TankComDirection(I) = TankComPosition(I)
TankComGunPosition(I) = TankComPosition(I)
TankComDestination(I) = TankActor.GetPosition
GunComON(I) = True
End If
End If

LastPos = TankComPosition(I)
TankComKDestination(I) = TankFPosition(TankComKill(I))
TankComActor(I).LookAtPoint TankComDestination(I)
If GetDistance3D(TankComPosition(I).x, 0, TankComPosition(I).z, TankFPosition(TankComKill(I)).x, 0, TankFPosition(TankComKill(I)).z) > 200 Then
For I2 = 0 To AllCom
If Not I2 = I Then
If GetDistance3D(TankComPosition(I).x, 0, TankComPosition(I).z, TankComPosition(I2).x, 0, TankComPosition(I2).z) > 100 Then
TankComKDirection(I) = VNormalize(VSubtract(TankComKDestination(I), TankComPosition(I)))
TankComPosition(I) = VAdd(TankComPosition(I), VScale(TankComKDirection(I), 1))
TankComActor(I).SetPosition TankComPosition(I).x, TankComPosition(I).y, TankComPosition(I).z
Else
If Not TankComPosition(I).x = LastPos.x And TankComPosition(I).z = LastPos.z Then
TankComActor(I).SetPosition TankComPosition(I).x, TankComPosition(I).y, TankComPosition(I).z
Else
TankComKDirection(I) = VNormalize(VSubtract(TankComKDestination(I), TankComPosition(I)))
TankComPosition(I) = VAdd(TankComPosition(I), VScale(TankComKDirection(I), -1))
TankComActor(I).SetPosition TankComPosition(I).x, TankComPosition(I).y, TankComPosition(I).z
End If
End If
End If
If GetDistance3D(TankComPosition(I).x, 0, TankComPosition(I).z, TankFPosition(TankComKill(I)).x, 0, TankFPosition(TankComKill(I)).z) > 100 Then
TankComKDirection(I) = VNormalize(VSubtract(TankComKDestination(I), TankComPosition(I)))
TankComPosition(I) = VAdd(TankComPosition(I), VScale(TankComKDirection(I), 1))
TankComActor(I).SetPosition TankComPosition(I).x, TankComPosition(I).y, TankComPosition(I).z
Else
If Not TankComPosition(I).x = LastPos.x And TankComPosition(I).z = LastPos.z Then
TankComActor(I).SetPosition TankComPosition(I).x, TankComPosition(I).y, TankComPosition(I).z
Else
TankComKDirection(I) = VNormalize(VSubtract(TankComKDestination(I), TankComPosition(I)))
TankComPosition(I) = VAdd(TankComPosition(I), VScale(TankComKDirection(I), -1))
TankComActor(I).SetPosition TankComPosition(I).x, TankComPosition(I).y, TankComPosition(I).z
End If
End If
Next
End If

Else ' HE IS DIE
Scene.DestroyMesh TankComActor(I)
Scene.DestroyMesh TankComGun(I)
TankComPosition(I).x = -50000
TankComPosition(I).y = -50000
TankComPosition(I).z = -50000
End If
Next
End Sub

Private Sub Render_Screen_Text()
On Error Resume Next
Dim TopRender As Integer
If GameOver = False Then
If GameWin = False Then
Set ScreenText = New TVScreen2DText
ScreenText.ACTION_BeginText
ScreenText.NormalFont_Create "Health", "Algerian", 20, True, False, False
ScreenText.NormalFont_DrawText "Health : " + CStr(TankHealth) + " ", 20, 20, RGBA(0, 1, 0, 1), "Health"
ScreenText.NormalFont_Create "Informations", "Algerian", 12, True, False, False
ScreenText.NormalFont_DrawText "Coms : ", 20, 50, RGBA(0, 0, 1, 1), "Informations"
TopRender = 60
For I = 0 To AllCom
TopRender = TopRender + 20
ScreenText.NormalFont_DrawText "Coms " + CStr(I + 1) + " : " + CStr(TankComHealth(I)) + " ", 20, TopRender, RGBA(0, 1, 1, 1), "Informations"
Next
TopRender = TopRender + 20
ScreenText.NormalFont_DrawText "Friends : ", 20, TopRender, RGBA(1, 0, 0, 1), "Informations"
For I = 0 To AllCom
TopRender = TopRender + 20
If TankWin(I) = False Then
ScreenText.NormalFont_DrawText "Friends " + CStr(I + 1) + " : " + CStr(TankFHealth(I)) + " ", 20, TopRender, RGBA(1, 1, 0, 1), "Informations"
Else
ScreenText.NormalFont_DrawText "Friends " + CStr(I + 1) + " : Win ", 20, TopRender, RGBA(1, 1, 0, 1), "Informations"
End If
Next
ScreenText.NormalFont_DrawText "Zaid Markabi", 20, 530, RGBA(1, 0, 0, 1), "Health"
ScreenText.NormalFont_DrawText "zaidmarkabi@yahoo.com", 20, 560, RGBA(1, 0, 0, 1), "Health"
ScreenText.ACTION_EndText
Set ScreenText2 = New TVScreen2DImmediate
ScreenText2.DRAW_Texture GetTex("Zoom"), ZoomGun.x - 20, ZoomGun.y - 20, ZoomGun.x + 20, ZoomGun.y + 20
End If
End If

If GameOver = True Then
If GameWin = False Then
If GameMid = False Then
Set ScreenText = New TVScreen2DText
ScreenText.ACTION_BeginText
ScreenText.NormalFont_Create "GameOver", "Algerian", 36, True, False, False
ScreenText.NormalFont_DrawText "You Lose ! ", 300, 280, RGBA(1, 0, 1, 1), "GameOver"
ScreenText.ACTION_EndText
Else
If GameWin = True Then
Set ScreenText = New TVScreen2DText
ScreenText.ACTION_BeginText
ScreenText.NormalFont_Create "GameWin", "Algerian", 36, True, False, False
ScreenText.NormalFont_DrawText "You Win ! ", 300, 280, RGBA(1, 0, 1, 1), "GameWin"
ScreenText.ACTION_EndText
End If
If GameMid = True Then
Set ScreenText = New TVScreen2DText
ScreenText.ACTION_BeginText
ScreenText.NormalFont_Create "GameMid", "Algerian", 36, True, False, False
ScreenText.NormalFont_DrawText "Breaking Even ! ", 300, 280, RGBA(1, 0, 1, 1), "GameMid"
ScreenText.ACTION_EndText
End If
End If
End If
End If
End Sub

