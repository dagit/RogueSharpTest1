module Camera

open Microsoft.Xna.Framework

open RogueSharp

open Global
open InputUtil

type Camera () =
  member val Zoom = 1.0f with get, set
  member val Position : Vector2 = Vector2.Zero with get,set
  member val Rotation = 0.0f with get

  member val ViewportWidth  = 0 with get, set
  member val ViewportHeight = 0 with get, set

  member this.ViewportCenter =
    new Vector2( float32 this.ViewportWidth * 0.5f, float32 this.ViewportHeight * 0.5f )

  member this.TranslationMatrix : Matrix =
    (* The int cast is for truncating the floats *)
    Matrix.CreateTranslation( float32 (- int this.Position.X)
                            , float32 (- int this.Position.Y)
                            , 0.0f ) *
      Matrix.CreateRotationZ ( this.Rotation ) *
      Matrix.CreateScale ( new Vector3( this.Zoom, this.Zoom, 1.0f ) ) *
      Matrix.CreateTranslation ( new Vector3( this.ViewportCenter, 0.0f ) )

  member this.AdjustZoom (amount : float32) : unit =
    this.Zoom <- this.Zoom + amount
    if this.Zoom < 0.25f
    then
      this.Zoom <- 0.25f

  member this.MoveCamera (cameraMovement : Vector2, ?clampToMap : bool) : unit =
    let clampToMap = defaultArg clampToMap false
    let newPosition = this.Position + cameraMovement
    if clampToMap
    then
      this.Position <- this.MapClampedPosition newPosition
    else
      this.Position <- newPosition

  member this.ViewportWorldBoundry : Rectangle =
    let viewportCorner = this.ScreenToWorld Vector2.Zero
    let viewportBottomCorner = this.ScreenToWorld (new Vector2( float32 this.ViewportWidth
                                                              , float32 this.ViewportHeight ) )
    new Rectangle( int viewportCorner.X
                 , int viewportCorner.Y
                 , int <| viewportBottomCorner.X - viewportCorner.X
                 , int <| viewportBottomCorner.Y - viewportCorner.Y )
  
  member this.CenterOn (position : Vector2) : unit =
    this.Position <- position

  member this.CenterOn (cell : Cell) : unit =
    this.Position <- this.CenteredPosition(cell, true)

  member private this.CenteredPosition (cell : Cell, ?clampToMap : bool) : Vector2 =
    let clampToMap = defaultArg clampToMap false
    let cameraPosition = new Vector2( float32 <| cell.X * Global.SpriteWidth
                                    , float32 <| cell.Y * Global.SpriteHeight )
    let cameraCenteredOnTilePosition =
      new Vector2( cameraPosition.X + float32 (Global.SpriteWidth / 2)
                 , cameraPosition.Y + float32 (Global.SpriteHeight / 2) )
    if clampToMap
    then
      this.MapClampedPosition cameraCenteredOnTilePosition
    else
      cameraCenteredOnTilePosition

  member private this.MapClampedPosition (position : Vector2) : Vector2 =
    let cameraMax = new Vector2( (float32 <| Global.MapWidth * Global.SpriteWidth)
                                  - ((float32 this.ViewportWidth) / this.Zoom / 2.0f)
                               , (float32 <| Global.MapHeight * Global.SpriteHeight)
                                  - ((float32 this.ViewportHeight) / this.Zoom / 2.0f) )
    Vector2.Clamp( position
                 , new Vector2( (float32 this.ViewportWidth) / this.Zoom / 2.0f
                              , (float32 this.ViewportHeight) / this.Zoom / 2.0f )
                 , cameraMax )

  member this.WorldToScreen (worldPosition : Vector2) : Vector2 =
    Vector2.Transform( worldPosition, this.TranslationMatrix )

  member this.ScreenToWorld (screenPosition : Vector2) : Vector2 =
    Vector2.Transform( screenPosition, Matrix.Invert( this.TranslationMatrix ) )

  member this.HandleInput (inputState : InputState) (controllingPlayer : PlayerIndex) : unit =
    let mutable cameraMovement = Vector2.Zero
    if inputState.IsScrollLeft ( System.Nullable(controllingPlayer) )
    then
      cameraMovement.X <- - 1.0f
    else if inputState.IsScrollRight ( System.Nullable(controllingPlayer) )
    then
      cameraMovement.X <-   1.0f
    else if inputState.IsScrollUp ( System.Nullable(controllingPlayer) )
    then
      cameraMovement.Y <- - 1.0f
    else if inputState.IsScrollDown ( System.Nullable(controllingPlayer) )
    then
      cameraMovement.Y <-   1.0f
    else if inputState.IsZoomIn ( System.Nullable(controllingPlayer) )
    then
      this.AdjustZoom  0.25f
    else if inputState.IsZoomOut ( System.Nullable(controllingPlayer) )
    then
      this.AdjustZoom -0.25f

    if cameraMovement <> Vector2.Zero
    then
      cameraMovement.Normalize()

    cameraMovement <- cameraMovement * 25.0f

    this.MoveCamera (cameraMovement, true)

let Camera = new Camera()