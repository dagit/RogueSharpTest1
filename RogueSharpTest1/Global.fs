module Global

open RogueSharp.Random

type GameStates =
  | Nothing
  | PlayerTurn
  | EnemyTurn
  | Debugging

type Global () =
  static member val GameState : GameStates = Nothing with get, set
  static member val Random       = new DotNetRandom() with get
  static member val MapWidth     = 50 with get
  static member val MapHeight    = 30 with get
  static member val SpriteWidth  = 64 with get
  static member val SpriteHeight = 64 with get