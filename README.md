# Saving Santa's (basement) with Sutil

## What's next
- refactoring Sound management with useful types instead of strings and tuples
- adding "resources", "components" and "systems" (think ECS-based architecture)
- defining a level (Map, row-first encoding in an array?, striding)
- rendering a level (divs in rows, "background" and "entity", 🧱 as walls, simple color als floor?)
- rendering the player 🧝 
- moving the player
- "collision detection"
- collecting things
- ogres and ogre movement
- player health and attacks
- ogre attacks 
- losing the game
- leaving the level ⏫
- winning the game
- ghosts and ghost movement and ghost attacks
- adding more levels?

## Requirements
I tested it with:
- Windows 10
- node 16.10.0
- dotnet 5.0.201 
- Chrome 95

## Running it
- The first time: `dotnet tool restore && npm install`
- Afterwards just: `npm run start`
