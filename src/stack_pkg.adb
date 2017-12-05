with Ada.Unchecked_Deallocation;
package body stack_pkg is
   procedure Free is new Ada.Unchecked_Deallocation(StackNode, Stack);
   
   function is_Empty(S: Stack) return Boolean is
     (S = null);
   
   function is_Full(S: Stack) return Boolean
   is
      t : Stack;
   begin
      t := new StackNode;
      Free(t);
      return False;
   exception
      when others =>
         return True;
   end is_Full;
   

   procedure push(Item: ItemType; S : in out Stack)
   is
   begin
      S := new StackNode'(Item, S); 
   end push;
   
   procedure pop(S : in out Stack)
   is
      t : Stack := S;
   begin
      S := S.Next;
      Free(t);
   end pop;
   

   function top(S: Stack) return ItemType is
     ( S.Item);
   
end stack_pkg;
