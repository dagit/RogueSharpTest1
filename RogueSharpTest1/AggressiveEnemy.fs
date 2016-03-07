module AggressiveEnemy

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

open RogueSharp

open InputUtil
open PathToPlayer
open LayerDepth

type AggressiveEnemy (x, y, sprite, p : PathToPlayer) =
  let        path   : PathToPlayer = p
  member val X      : int          = x      with get,set
  member val Y      : int          = y      with get,set
  member val Sprite : Texture2D    = sprite with get,set

  member this.Draw (spriteBatch : SpriteBatch) : unit =
    spriteBatch.Draw( this.Sprite
                    , System.Nullable(new Vector2( float32 <| this.X * this.Sprite.Width
                                                 , float32 <| this.Y * this.Sprite.Width ) )
                    , System.Nullable()
                    , System.Nullable()
                    , System.Nullable()
                    , 0.0f
                    , System.Nullable(Vector2.One)
                    , System.Nullable(Color.White)
                    , SpriteEffects.None
                    , LayerDepth.Figures )
    path.Draw spriteBatch

  member this.Update : unit =
    path.CreateFrom this.X this.Y
    match path.FirstCell with
    | Some(cell) -> this.X <- cell.X
                    this.Y <- cell.Y
    | None       -> ()