module RogueSharpGame

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

open RogueSharp
open RogueSharp.MapCreation
open RogueSharp.Random

open Player
open PathToPlayer
open AggressiveEnemy
open InputUtil
open Global
open LayerDepth
open Camera

type Game1 () as this =
    inherit Game()
 
    do this.Content.RootDirectory <- "."
    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch = Unchecked.defaultof<SpriteBatch>

    let mutable floor = Unchecked.defaultof<Texture2D>
    let mutable wall  = Unchecked.defaultof<Texture2D>

    let mutable map    = Unchecked.defaultof<IMap>
    let mutable player = Unchecked.defaultof<Player>
    let mutable enemy  = Unchecked.defaultof<AggressiveEnemy>

    let mutable inputState = new InputState()
 
    override this.Initialize() =
        do spriteBatch <- new SpriteBatch(this.GraphicsDevice)
           let mapCreationStrategy =
              new RandomRoomsMapCreationStrategy<Map>( Global.MapWidth, Global.MapHeight, 100, 7, 3 )
           map <- Map.Create( mapCreationStrategy )
           base.Initialize()
           Camera.ViewportWidth  <- graphics.GraphicsDevice.Viewport.Width
           Camera.ViewportHeight <- graphics.GraphicsDevice.Viewport.Height
           Camera.Zoom           <- 0.25f

    override this.LoadContent() =
        floor  <- this.Content.Load("floor")
        wall   <- this.Content.Load("wall")
        let mutable empty = this.GetRandomEmptyCell ()
        player <- new Player( empty.X, empty.Y
                            , this.Content.Load<Texture2D>( "player" ) )
        Camera.CenterOn empty
        Global.GameState <- PlayerTurn
        empty <- this.GetRandomEmptyCell ()
        let path = new PathToPlayer(player, map, this.Content.Load( "white" ))
        path.CreateFrom empty.X empty.Y
        enemy <- new AggressiveEnemy( empty.X, empty.Y
                                    , this.Content.Load<Texture2D>( "hound" ), path)
        this.UpdatePlayerFieldOfView
 
    override this.Update (gameTime) =
        inputState.Update()
        if inputState.IsExitGame( System.Nullable(PlayerIndex.One))
        then
            exit 0
        if inputState.IsSpace ( System.Nullable( PlayerIndex.One ) )
        then
          match Global.GameState with
          | PlayerTurn -> Global.GameState <- Debugging
          | Debugging  -> Global.GameState <- PlayerTurn
          | _          -> ()
          printf "%A\n" Global.GameState
        Camera.HandleInput inputState PlayerIndex.One
        if Global.GameState = PlayerTurn
           && player.HandleInput inputState map
        then
          Camera.CenterOn (map.GetCell(player.X, player.Y))
          this.UpdatePlayerFieldOfView
          Global.GameState <- EnemyTurn
        if Global.GameState = EnemyTurn
        then
          enemy.Update
          Global.GameState <- PlayerTurn
        base.Update gameTime

    override this.Draw (gameTime) =
        do this.GraphicsDevice.Clear Color.Black
           spriteBatch.Begin( SpriteSortMode.BackToFront, BlendState.AlphaBlend
                            , null, null, null, null
                            , System.Nullable(Camera.TranslationMatrix) )

           let filter = fun (c : Cell) -> c.IsExplored || Global.GameState = Debugging
           let cells = Seq.filter filter (map.GetAllCells())
           for cell in cells do
             let position   = Vector2( float32 <| cell.X * Global.SpriteWidth
                                     , float32 <| cell.Y * Global.SpriteHeight )
             let mutable tint = Color.White
             if (not cell.IsInFov) && not (Global.GameState = Debugging)
             then
              tint <- Color.Gray
             if cell.IsWalkable
             then
               spriteBatch.Draw( floor
                               , System.Nullable(position)
                               , System.Nullable()
                               , System.Nullable()
                               , System.Nullable()
                               , 0.0f
                               , System.Nullable(Vector2.One)
                               , System.Nullable(tint)
                               , SpriteEffects.None
                               , LayerDepth.Cells )
             else
               spriteBatch.Draw( wall
                               , System.Nullable(position)
                               , System.Nullable()
                               , System.Nullable()
                               , System.Nullable()
                               , 0.0f
                               , System.Nullable(Vector2.One)
                               , System.Nullable(tint)
                               , SpriteEffects.None
                               , LayerDepth.Cells )
           player.Draw( spriteBatch )
           if Global.GameState = Debugging
              || map.IsInFov( enemy.X, enemy.Y )
           then
            enemy.Draw( spriteBatch )

           spriteBatch.End()

           base.Draw( gameTime )

    (* Private methods below here *) 
    member private this.GetRandomEmptyCell () : Cell =
      let random = Global.Random
      let rec findOpenSpot () =
        let x = random.Next 49
        let y = random.Next 29
        if map.IsWalkable(x, y)
        then
          map.GetCell(x, y)
        else
          findOpenSpot ()
      findOpenSpot ()

    member private this.UpdatePlayerFieldOfView =
      map.ComputeFov( player.X, player.Y, 30, true )
      let cells = Seq.filter (fun (c:Cell) -> c.IsInFov) (map.GetAllCells())
      for cell in cells do
        map.SetCellProperties( cell.X, cell.Y, cell.IsTransparent, cell.IsWalkable, true )