-- Name: Andrew McGuiness
-- Date: December 7, 2017
-- Course: ITEC 320 Procedural Analysis and Design
--
-- EXTRA CREDIT ATTEMPTED: I Used Pointers to Players inside of the collections.
--
--
-- Purpose: This program takes in a list of Players from stdin, then has them
--   play in a competition.  A Player is eliminated when he has two losses, and
--   a single winner will be determined.  Input is read in in the following
--   format:  Characters 1..20 = Name, 21..End_of_Line will be parsed for skill
--   The order that Players are entered will be stored with the Player as their
--   "Order" which can be used to break ties.  Players are compared based on
--   skill, wins, losses and arrival order.
--
--  Sample input:
--
--  Person890123456789     500
--  Person2                400
--  Person3                300
--  Person4                200
--  Person5                100
--
--  Expected output:
--
--  Name                  Number  Skill  Wins  Losses
--  Person890123456789       1     500     4     0
--  Person2                  2     400     2     2
--  Person3                  3     300     2     2
--  Person5                  5     100     0     2
--  Person4                  4     200     0     2

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with stack_pkg;
with queue_pkg;
with Ada.Unchecked_Deallocation;
procedure Competition is

   -- Number of cols for the name
   LENGTH : Constant := 20;

   -- When matches are played, a Stat is used to determine the winner.
   type Stat is (SKILL, WINS, LOSSES, ORDER);


   type Player is record
      name                    : String(1..LENGTH);
      order, win, loss, skill : Integer;
   end record;

   type Player_Ptr is access Player;
   procedure Free is new Ada.Unchecked_Deallocation(Player, Player_Ptr);

   package Player_Ptr_Stack is new stack_pkg(Player_Ptr);
   use Player_Ptr_Stack;

   package Player_Ptr_Queue is new queue_pkg(Player_Ptr);
   use Player_Ptr_Queue;

   ----------------------------------------------------------
   -- Purpose: Print a Player to stdout.
   -- Parameters: p: Player to print
   ----------------------------------------------------------
   procedure Put( p : in Player) is
   begin
      Put( p.name );
      Put( p.order );
      Put( p.skill );
      Put( p.win );
      Put( p.loss );
      New_Line;
   end Put;

   ----------------------------------------------------------
   -- Purpose: Read input from stdin and write it into a Player object
   -- Parameters: p: Player to read input into
   ----------------------------------------------------------
   procedure Get( p : out Player) is
      s    : String(1..80);
      last : Integer;
   begin
      Get_Line( s, last);

      p.name  := s(1..20);
      p.skill := Integer'Value( s(21..last) );
      p.win := 0;
      p.loss := 0;
   end Get;

   ----------------------------------------------------------
   -- Purpose: Get the given stat for a Player
   -- Parameters: a: Player to lookin
   --             s: stat to get
   -- Returns: The stat for the player
   ----------------------------------------------------------
   function GetStat( a : in Player_Ptr; s : in Stat ) return Integer
   is
   begin
      case s is
         when SKILL =>
            return a.skill;
         when WINS =>
            return a.win;
         when LOSSES =>
            return a.loss;
         when ORDER =>
            return a.order;
      end case;
   end GetStat;


   ----------------------------------------------------------
   -- Purpose: Compare two players based on a stat value.
   -- Parameters: a, b: Players to compare
   --                s: stat to use for comparison
   -- Postcondition: a will contain the winner, b will contain the loser
   -- Returns: True if the comparison produced a winner
   ----------------------------------------------------------
   function compare( a, b : in out Player_Ptr; s : Stat) return Boolean
   is
      t       : Player_Ptr;
      a_value : Integer := GetStat(a, s);       -- Cache the Player's stats
      b_value : Integer := GetStat(b, s);
   begin
      if a_value = b_value then
         -- There was a tie, need to try another stat
         return False;
      elsif
        (s in SKILL | WINS and then a_value < b_value)  or else
        (s in LOSSES | ORDER and then a_value > b_value) then

         -- b currently holds the greater value, so we need to swap them
         t := a;
         a := b;
         b := t;
      end if;


      return True;
   end compare;

   ----------------------------------------------------------
   -- Purpose: Play a single match between two players.
   -- Parameters: a, b: Players to compare
   -- Postcondition: a will contain the winner, b will contain the loser
   ----------------------------------------------------------
   procedure PlayMatch( a, b : in out Player_Ptr) is
   begin
      -- Continue trying comparisons until one returns True
      if not compare( a, b, SKILL) then
         if not compare( a, b, WINS) then
            if not compare( a, b, LOSSES) then
               if not compare( a, b, ORDER) then
                  null;
               end if;
            end if;
         end if;
      end if;

      -- Adjust stats
      a.win  := a.win + 1;
      b.loss := b.loss + 1;
   end PlayMatch;

   ----------------------------------------------------------
   -- Purpose: Zero loss and one loss Queue each contain a single player.  Play
   --    matches between them until one is eliminated.
   -- Parameters: zero, one: Queues that contain the remaining players
   --                  done: Stack to place players on once finished
   -- Precondition: zero and one each contain a single player
   -- Postcondition: zero and one will be empty
   ----------------------------------------------------------
   procedure PlayFinalMatches( zero, one : in out Queue; done : in out Stack)
   is
      a, b : Player_Ptr;
   begin
      a := front( zero );
      dequeue( zero );

      b := front( one );
      dequeue( one );

      while a.loss < 2 and then b.loss < 2 loop
         PlayMatch( a, b );
      end loop;

      push( b, done );
      push( a, done );
   end PlayFinalMatches;

   ----------------------------------------------------------
   -- Purpose: Players that have zero losses will play matches until there is
   --   only a single remaining player with zero losses.
   -- Parameters: zero, one: Queues for Players
   --                 count: Number of Players
   -- Precondition: zero contains at least two players
   -- Postcondition: zero contains exactly one player
   ----------------------------------------------------------
   procedure PlayZeroLoss(zero, one : in out Queue; count : Integer)
   is
      remaining : Integer := count;
      a, b      : Player_Ptr;
   begin
      while remaining > 1 loop
         a := front( zero );
         dequeue( zero );

         b := front( zero );
         dequeue( zero );

         PlayMatch( a, b);

         enqueue( a, zero );
         enqueue( b, one );

         remaining := remaining - 1;
      end loop;
   end PlayZeroLoss;

   ----------------------------------------------------------
   -- Purpose: Players that have one loss will play matches until there is
   --   only a single remaining player with one loss.
   -- Parameters: one: Queue for remaining Players
   --            done: Stack to place eliminated Players
   --           count: Number of Players
   -- Precondition: one contains at least one player
   -- Postcondition: one contains exactly one player
   ----------------------------------------------------------
   procedure PlayOneLoss( one : in out Queue; done : in out Stack; count : Integer)
   is
      remaining : Integer := count;
      a, b      : Player_Ptr;
   begin
      while remaining > 1 loop
         a := front( one );
         dequeue( one );

         b := front( one );
         dequeue( one );

         PlayMatch( a, b );

         enqueue( a, one );
         push(    b, done );

         remaining := remaining - 1;
      end loop;
   end PlayOneLoss;

   ----------------------------------------------------------
   -- Purpose: Continuously read Players in from stdin and place them
   --     into the Zero loss Queue.
   -- Parameters: zero: Container for the Players
   --            count: Number of Players that showed up
   ----------------------------------------------------------
   procedure GetPlayers( zero : out Queue; count : out Integer )
   is
      p : Player_Ptr;
   begin
      count := 0;

      while not End_Of_File loop
         p := new Player;
         Get( p.all );
         count :=  count + 1;
         p.order := count;
         enqueue( p, zero );
      end loop;
   end GetPlayers;


   ----------------------------------------------------------
   -- Purpose: Pop and Print all values from a Stack
   -- Parameters: s: Stack to print and empty
   -- Postcondition: s will be empty
   ----------------------------------------------------------
   procedure PrintAndEmpty( s : in out Stack )
   is
      p : Player_Ptr;
   begin
      while not is_Empty( s ) loop
         p := top(s);
         pop( s );
         Put( p.all );
         Free( p );
      end loop;
   end PrintAndEmpty;

   ----------------------------------------------------------
   -- Purpose: Play matches between players until only one player remains with
   --    less than two losses.
   -- Parameters: zero, one: Queues for players that are not done
   --                  done: Stack for players that are done
   --                 count: Number of players that showed up
   -- Precondition: zero has at least two players in it
   -- Postcondition: zero and one will be empty, all players in done
   ----------------------------------------------------------
   procedure PlayMatches( zero, one : in out Queue;
                          done      : in out Stack; count : in Integer)
   is
   begin
      PlayZeroLoss( zero, one, count);
      PlayOneLoss(   one, done, count-1);
      PlayFinalMatches( zero, one, done);
   end PlayMatches;


   zeroLoss  : Queue;
   oneLoss   : Queue;
   doneStack : Stack;
   count     : Integer;
begin
   GetPlayers( zeroLoss, count);

   if count > 1 then
      PlayMatches( zeroLoss, oneLoss, doneStack, count );

      Put_Line( "Name                       Number     Skill       Wins      Losses" );
      PrintAndEmpty( doneStack );

   elsif count = 1 then
      declare
         p : Player_Ptr := front(zeroLoss);
      begin
         Put_Line("Only one player showed up.");
         Put_Line(p.name & " takes the prize!");
         Free(p);
      end;

   elsif count = 0 then
      Put_Line("No one showed up to play!");
      Put_Line("Take the prize money home!");

   end if;

end Competition;
