with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with stack_pkg;
with queue_pkg;
with Ada.Unchecked_Deallocation;
procedure Competition is
   LENGTH : Constant := 20;

   type Player is record
      name                    : String(1..LENGTH);
      order, win, loss, skill : Integer;
   end record;

   type Player_Ptr is access Player;
   procedure Free is new Ada.Unchecked_Deallocation(Player, Player_Ptr);

   procedure Put( p : in Player) is
   begin
      Put( p.name );
      Put( p.order );
      Put( p.skill );
      Put( p.win );
      Put( p.loss );
      New_Line;
   end Put;

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

   procedure PlayMatch( a, b : in Player_Ptr; winner, loser : out Player_Ptr) is
   begin
      if a.skill > b.skill then
         a.win  := a.win + 1;
         b.loss := b.loss + 1;

         winner := a;
         loser  := b;
      elsif a.skill < b.skill then
         a.loss := a.loss + 1;
         b.win  := b.win + 1;

         winner := b;
         loser  := a;
      else
         if a.win > b.win then
            a.win  := a.win + 1;
            b.loss := b.loss + 1;

            winner := a;
            loser  := b;
         elsif a.win < b.win then
            a.loss := a.loss + 1;
            b.win := b.win + 1;

            winner := b;
            loser  := a;
         else
            if a.loss < b.loss then
               a.win  := a.win + 1;
               b.loss := b.loss + 1;

               winner := a;
               loser  := b;
            elsif a.loss > b.loss then
               a.loss := a.loss + 1;
               b.win := b.win   + 1;

               winner := b;
               loser  := a;
            else
               if a.order < b.order then
                  a.win  := a.win  + 1;
                  b.loss := b.loss + 1;

                  winner := a;
                  loser  := b;
               else
                  a.loss := a.loss + 1;
                  b.win  := b.win  + 1;

                  winner := b;
                  loser  := a;
               end if;

            end if;
         end if;

      end if;
   end PlayMatch;

   package Player_Ptr_Stack is new stack_pkg(Player_Ptr);
   use Player_Ptr_Stack;

   package Player_Ptr_Queue is new queue_pkg(Player_Ptr);
   use Player_Ptr_Queue;

   zero_loss : Player_Ptr_Queue.Queue;
   one_loss  : Player_Ptr_Queue.Queue;
   two_loss  : Player_Ptr_Stack.Stack;

   p  : Player_Ptr;
   count : Integer := 0;

begin
   -- Load up the 0 loss queue
   while not End_Of_File loop
      p := new Player;
      Get( p.all );
      count :=  count + 1;
      p.order := count;
      enqueue( p, zero_loss );
   end loop;

   declare
      remain : Integer := count;
      a, b   : Player_Ptr;
   begin
      while remain > 1 loop
         a := front( zero_loss );
         dequeue( zero_loss );

         b := front( zero_loss );
         dequeue( zero_loss );

         PlayMatch( a, b, a, b );

         enqueue( a, zero_loss );
         enqueue( b, one_loss );

         remain := remain - 1;
      end loop;
   end;

   Put( "Count: "); Put( count );
   New_Line;

   declare
      remain : Integer := count - 1;
      a, b   : Player_Ptr;
   begin
      while remain > 1 loop
         a := front( one_loss );
         dequeue( one_loss );

         b := front( one_loss );
         dequeue( one_loss );

         PlayMatch( a, b, a, b );

         enqueue( a, one_loss );
         push(b, two_loss );

         remain := remain - 1;
      end loop;
   end;


   declare
      a, b   : Player_Ptr;
   begin

      a := front( zero_loss );
      dequeue( zero_loss );

      b := front( one_loss );
      dequeue( one_loss );

      while a.loss < 2 and then b.loss < 2 loop
         PlayMatch( a, b, a, b );
      end loop;

      push( b, two_loss );
      push( a, two_loss );
   end;



   Put( "Name                       Number     Skill       Wins      Losses" );
   New_Line;


   while not is_Empty(zero_loss) loop
      p := front(zero_loss);
      dequeue( zero_loss );
      Put( p.all );
      Free( p );
   end loop;

   while not is_Empty(one_loss) loop
      p := front(one_loss);
      dequeue( one_loss );
      Put( p.all );
      Free( p );
   end loop;

   while not is_Empty( two_loss ) loop
      p := top(two_loss);
      pop( two_loss );
      Put( p.all );
      Free( p );
   end loop;
end Competition;
