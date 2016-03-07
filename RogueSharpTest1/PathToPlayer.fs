module PathToPlayer

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

open RogueSharp

open Player
open Global
open LayerDepth

type PathToPlayer (p : Player, m : IMap, s : Texture2D) =
  let player             = p
  let map                = m
  let sprite             = s
  let mutable pathFinder = new PathFinder(m)
  let mutable cells      = Seq.empty<Cell>

  member this.FirstCell : Cell option =
    if Seq.isEmpty cells
    then
      None
    else
      Some (Seq.head cells)

  member this.CreateFrom (x:int) (y:int) =
    if x = player.X && y = player.Y
    then
      cells <- Seq.empty<Cell>
    else
      cells <- pathFinder.ShortestPath( map.GetCell( x,y ), map.GetCell( player.X, player.Y ) ).Steps
            |> Seq.cast<Cell>

  member this.Draw (spriteBatch : SpriteBatch ) =
    if (not (cells |> Seq.isEmpty)) && Global.GameState = Debugging
    then
      let filter = fun (c:Cell) -> c <> null
      let cs     = Seq.filter filter cells
      for cell in cs do
        spriteBatch.Draw ( sprite
                         , System.Nullable( new Vector2( float32 <| cell.X * sprite.Width
                                                       , float32 <| cell.Y * sprite.Width ) )
                         , System.Nullable()
                         , System.Nullable()
                         , System.Nullable()
                         , 0.0f
                         , System.Nullable( Vector2.One )
                         , System.Nullable( Color.Blue * 0.2f )
                         , SpriteEffects.None
                         , LayerDepth.Paths )