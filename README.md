# Snake Game
This is the final project for the 3rd year subject (Micro Processors) written in assembly language, made using Visual Studio and MASM 32-bit


it is a game of a snake moving on the screen with the goal to eat as much food as possible.
you can change the level to another harder/easier one and/or the speed in which snake moves.
---



---
# Game instructions 
If you didnot choose any level the game will start automatically in the level with no obstacles and there is no walls on the screen, to change the level from the main menu choose select level. Here you can choose from 3 levels the first is none that there is no obstacles, the second is the box level where the screen will form a yellow walls at its borders like a box, and the third level is the camera frame level in which a frame like camera lens frame will be printed on screen.  
You can also change the speed which snake moves by choosing from main menu select speed and adjust speed as you wish, we have four speeds you can choose from them arranged in ascending order, ranged from one to four. You can adjust speed from this menu. 

You lose if the snake collide with the walls painted with yellow or if snake ate itself.  
To increase your score (which will be displayed at the bottom left corner of the screen) you have to collect the food printed on the screen in purple color.  
To close the game while playing press ESC key on keyboard and then choose from main menu the last choice (Exit)

# Game logic
a snake is built of segments that increase by eating food and his live ends when he hits a wall or eats himself.

;--------------------------------Snake and Snake Movment--------------------------------
for the snake we built it by constucting and array of segments which each segment holding it's X & Y position in the screen buffer.
when the snake moves the procedure MoveSnake calculates the position of the new head and checks if the new head will make the snake
eat a food so it doesn't delete the old tail(default),hit a wall so it runds the 'end the game' procedures or
hit nothing so it just adds the new head to the segmant array and changes the head Index to be able to locate the new head in the next move.

# contributers 
1. mohamed sayed
2. ahmed sayed
3. abdelrahman ahmed
4. mohamed atef
5. mohamed ezzat
