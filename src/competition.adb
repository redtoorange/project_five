with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with stack_pkg;
with Ada.Unchecked_Deallocation;
procedure Competition is
   type Player is record
      name : String(1..20);
      order, win, loss, skill : Integer;
   end record;

   type Player_Ptr is access Player;
   procedure Free is new Ada.Unchecked_Deallocation(Player, Player_Ptr);


   procedure Put( p : in Player) is
   begin
      Put_Line( "Name:" & p.name );
--        Put_Line( "Order:" & p.order'Image );
--        Put_Line( "Skill:" & p.skill'Image );
--        Put_Line( "Wins: " & p.win'Image );
--        Put_Line( "Loss: " & p.loss'Image );
      New_Line;
   end Put;

   procedure Get( p : out Player) is
   begin
      Get( p.name );
      --Get( p.order );
      --Get( p.skill );
      p.win := 0;
      p.loss := 0;
   end Get;


   package IntStack is new stack_pkg(Integer);
   use IntStack;

   package Player_Ptr_Stack is new stack_pkg(Player_Ptr);
   use Player_Ptr_Stack;

   i : IntStack.Stack;
   o : Integer;

   ps : Player_Ptr_Stack.Stack;
   p  : Player_Ptr;

begin
   for i in 0..2 loop
      p := new Player;
      Get( p.all );
      push( p, ps);
   end loop;

   New_Line;

   while not is_Empty(ps) loop
      p := top(ps);
      pop(ps);
      Put( p.all);
   end loop;
end Competition;
